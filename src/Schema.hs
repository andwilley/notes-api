{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Schema
  ( Query(..)
  , Mutation(..)
  , getNote
  , getNotes
  , createNote
  , changeNote
  , removeNotes
  )
where

import           Data.Morpheus.Types            ( GQLType(..)
                                                , IORes
                                                , IOMutRes
                                                , liftEitherM
                                                )
import           Data.Morpheus.Kind             ( OBJECT )
import           GHC.Generics                   ( Generic )
import qualified Database.MongoDB              as DB
import           Control.Monad.Identity
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import qualified Data.Text                     as T
                                                ( length )
import           Data.Bson                      ( Value
                                                , ObjectId
                                                , Value(String, ObjId)
                                                , valueAt
                                                , typed
                                                )
import           Note
import           Lens.Simple

-- infix is awkward with a qualifier
(=:) :: DB.Val v => DB.Label -> v -> DB.Field
(=:) = (DB.=:)

-- TODO: consider having func for each query param (noteById, noteByTitle, noteDateRange)
data Query m = Query { note :: NoteArgs -> m Note
                     , notes :: () -> m [Note]
                     } deriving (Generic, GQLType)

data Mutation m = Mutation { addNote :: NewNote -> m Note
                           , updateNote :: UpdateNote -> m Note
                           , deleteNotes :: NoteIdList -> m [Note]
                           } deriving (Generic, GQLType)

runQuery
  :: (Monad m) => DB.Action IO (Either String (m a)) -> IO (Either String (m a))
runQuery act = do
  pipe <- DB.connect (DB.host "127.0.0.1")
  e    <- DB.access pipe DB.master "notes" act
  DB.close pipe
  return e

-- read

getNote :: NoteArgs -> IORes e Note
getNote NoteArgs { title, noteid } = liftEitherM $ do
  eitherNote <- runQuery $ getNoteByProp title noteid
  return $ runIdentity <$> eitherNote

getNoteByProp
  :: Maybe Text -> Maybe Text -> DB.Action IO (Either String (Identity Note))
getNoteByProp title noteId = do -- inside the Action monad
  maybeNoteDoc <- DB.findOne (DB.select (makeSelector title noteId) "notes")
  return $ do -- inside the Either monad
    idDoc <- eitherDocFromMaybe maybeNoteDoc
    return $ docToNote <$> idDoc
 where
  makeSelector (Just title') (Just id') =
    ["title" =: title', "_id" =: readIdOrEmpty id']
  makeSelector Nothing       (Just id') = ["_id" =: readIdOrEmpty id']
  makeSelector (Just title') Nothing    = ["title" =: title']
  makeSelector Nothing       Nothing    = []

getNotes :: () -> IORes () [Note]
getNotes () = liftEitherM $ runQuery getAllNotes

getAllNotes :: DB.Action IO (Either String [Note])
getAllNotes = do
  noteDocList <- DB.rest =<< DB.find (DB.select [] "notes")
  return $ Right $ map docToNote noteDocList

-- create

createNote :: NewNote -> IOMutRes e Note
createNote note' = liftEitherM $ do
  n <- runQuery $ insertNote note'
  return $ runIdentity <$> n

insertNote :: NewNote -> DB.Action IO (Either String (Identity Note))
insertNote newNote = do
  doc <- liftIO $ updateNoteCreateDate =<< updateNoteModifyDate
    (noteToDoc madeNote)
  id' <- DB.insert "notes" doc
  return $ Right $ return $ setNoteId (pack $ show id') (docToNote doc)
  where madeNote = makeNote newNote

-- update

changeNote :: UpdateNote -> IOMutRes e Note
changeNote note = liftEitherM $ do
  n <- runQuery $ saveNote note
  return $ runIdentity <$> n

saveNote :: UpdateNote -> DB.Action IO (Either String (Identity Note))
saveNote note'@UpdateNote { updateId = nId' } = do
  maybeNoteDoc <- DB.findOne (DB.select ["_id" =: readIdOrEmpty nId'] "notes")
  case maybeNoteDoc of
    Just noteDoc -> do
      let updatedNote = mergeNote (docToNote noteDoc) note'
      note' <- liftIO $ updateNoteModifyDate $ noteToDoc updatedNote
      DB.save "notes" note'
      return $ Right $ return (docToNote note')
    Nothing -> return $ Left "record not found"

-- delete

removeNotes :: NoteIdList -> IOMutRes e [Note]
removeNotes ids = liftEitherM $ runQuery $ delNotes $ noteIdList ids

delNotes :: [Text] -> DB.Action IO (Either String [Note])
delNotes ids = do
  delNotes <- DB.rest =<< DB.find sels
  s        <- DB.delete selq
  return $ Right $ map docToNote delNotes
 where
  orIds = ["$or" =: map (\id' -> ["_id" =: readIdOrEmpty id']) ids]
  selq  = DB.select orIds "notes"
  sels  = DB.select orIds "notes"

-- utilities

eitherDocFromMaybe :: Maybe DB.Document -> Either String (Identity DB.Document)
eitherDocFromMaybe (Just doc) = Right $ return doc
eitherDocFromMaybe Nothing    = Left "No result found"

readIdOrEmpty :: Text -> Value
readIdOrEmpty id' | T.length id' == 24 = ObjId (read $ unpack id' :: ObjectId)
                  | otherwise          = String ""
