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
import           Data.Morpheus                  ( interpreter )
import           Data.Morpheus.Document         ( importGQLDocumentWithNamespace
                                                )
import           Data.Morpheus.Kind             ( OBJECT )
import           GHC.Generics                   ( Generic )
import qualified Database.MongoDB              as DB
import           Data.Maybe                     ( fromJust )
import           Data.Data                      ( Constr
                                                , constrFields
                                                )
import           Control.Monad.Identity
import           Control.Monad.IO.Class
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
                                                , (!?)
                                                )
import           Note
import           Control.Exception

-- infix is awkward with a qualifier
(=:) :: DB.Val v => DB.Label -> v -> DB.Field
(=:) = (DB.=:)

-- TODO: consider having func for each query param (noteById, noteByTitle)
data Query m = Query { note :: NoteArgs -> m Note
                     , notes :: () -> m [Note]
                     } deriving (Generic, GQLType)

data Mutation m = Mutation { addNote :: NewNote -> m Note
                           , updateNote :: UpdateNote -> m Note
                           , deleteNotes :: NoteIdList -> m [Note]
                           } deriving (Generic, GQLType)

getNote :: NoteArgs -> IORes e Note
getNote NoteArgs { title, noteid } = liftEitherM $ do
  eitherNote <- runQuery $ getNoteByProp title noteid
  return $ runIdentity <$> eitherNote

getNotes :: () -> IORes () [Note]
getNotes () = liftEitherM $ runQuery getAllNotes

createNote :: NewNote -> IOMutRes e Note
createNote note' = liftEitherM $ do
  n <- runQuery $ insertNote note'
  return $ runIdentity <$> n

changeNote :: UpdateNote -> IOMutRes e Note
changeNote note = liftEitherM $ do
  n <- runQuery $ saveNote note
  return $ runIdentity <$> n

removeNotes :: NoteIdList -> IOMutRes e [Note]
removeNotes ids = liftEitherM $ runQuery $ delNotes $ noteIdList ids

runQuery
  :: (Monad m) => DB.Action IO (Either String (m a)) -> IO (Either String (m a))
runQuery act = do
  pipe <- DB.connect (DB.host "127.0.0.1")
  e    <- DB.access pipe DB.master "notes" act
  DB.close pipe
  return e

getAllNotes :: DB.Action IO (Either String [Note])
getAllNotes = do
  noteDocList <- DB.rest =<< DB.find (DB.select [] "notes")
  return $ Right $ map docToNote noteDocList

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

insertNote :: NewNote -> DB.Action IO (Either String (Identity Note))
insertNote newNote = do
  doc       <- liftIO $ noteToDoc <$> madeNote
  id'       <- DB.insert "notes" doc
  madeNote' <- liftIO $ updateNoteId (pack $ show id') <$> madeNote
  return $ Right $ return madeNote'
  where madeNote = makeNote newNote

saveNote :: UpdateNote -> DB.Action IO (Either String (Identity Note))
saveNote note'@UpdateNote { updateId = nId' } = do
  maybeNoteDoc <- DB.findOne (DB.select ["_id" =: readIdOrEmpty nId'] "notes")
  case maybeNoteDoc of
    Just noteDoc -> do
      updatedNote <- liftIO $ mergeNote (docToNote noteDoc) note'
      DB.save "notes" $ noteToDoc updatedNote
      return $ Right $ return updatedNote
    Nothing -> return $ Left "record not found"

delNotes :: [Text] -> DB.Action IO (Either String [Note])
delNotes ids = do
  delNotes <- DB.rest =<< DB.find sels
  s        <- DB.delete selq
  return $ Right $ map docToNote delNotes
 where
  orIds = ["$or" =: map (\id' -> ["_id" =: readIdOrEmpty id']) ids]
  selq  = DB.select orIds "notes"
  sels  = DB.select orIds "notes"

eitherDocFromMaybe :: Maybe DB.Document -> Either String (Identity DB.Document)
eitherDocFromMaybe (Just doc) = Right $ return doc
eitherDocFromMaybe Nothing    = Left "No result found"

-- docTo :: Constr -> DB.Document -> Note
-- docTo constr doc = docTo' constrList doc constr
--     where constrList = constrFields constr

-- docTo' :: [String] -> DB.Document -> Constr -> Note
-- docTo' [field         ] doc constr = constr $ valueAt field doc
-- docTo' (field : fields) doc constr = "field"


    -- Note (Just "title1") "cDate1" "mDate1" "content1"

readIdOrEmpty :: Text -> Value
readIdOrEmpty id' | T.length id' == 24 = ObjId (read $ unpack id' :: ObjectId)
                  | otherwise          = String ""
