{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Schema
  ( Query(..)
  , Mutation(..)
  , getNote
  , getNotes
  , createNote
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
import           Data.Bson                      ( Value
                                                , ObjectId
                                                , Value(String)
                                                , valueAt
                                                , typed
                                                , cast'
                                                , (!?)
                                                )
import           Note

-- infix is awkward with a qualifier
(=:) :: DB.Val v => DB.Label -> v -> DB.Field
(=:) = (DB.=:)

-- TODO: consider having func for each query param (noteById, noteByTitle)
data Query m = Query { note :: NoteArgs -> m Note
                     , notes :: () -> m [Note]
                     } deriving (Generic, GQLType)

data Mutation m = Mutation { addNote :: NewNote -> m Note
                           , updateNote :: UpdateNote -> m Note
                          --  , deleteNote :: Text -> m ()
                           } deriving (Generic, GQLType)

getNote :: NoteArgs -> IORes e Note
getNote NoteArgs { nTitle, nId } = liftEitherM $ do
  eitherNote <- runQuery $ getNoteByProp nTitle nId
  return $ runIdentity <$> eitherNote

getNotes :: () -> IORes () [Note]
getNotes () = liftEitherM $ runQuery getAllNotes

createNote :: NewNote -> IOMutRes e Note
createNote note' = liftEitherM $ do
  n <- runQuery $ insertNote note'
  return $ runIdentity <$> n

changeNote :: UpdateNote -> IOMutRes e Note
changeNote note = undefined

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
    ["title" =: title', "_id" =: (read $ unpack id' :: ObjectId)]
  makeSelector Nothing (Just id') = ["_id" =: (read $ unpack id' :: ObjectId)]
  makeSelector (Just title') Nothing = ["title" =: title']
  makeSelector Nothing Nothing = []

insertNote :: NewNote -> DB.Action IO (Either String (Identity Note))
insertNote newNote = do
  doc       <- liftIO $ noteToDoc <$> madeNote
  id'       <- DB.insert "notes" doc
  madeNote' <- liftIO $ updateNoteId (pack $ show id') <$> madeNote
  return $ Right $ return madeNote'
  where madeNote = makeNote newNote

liftNoteIO
  :: DB.Action IO (Either String (Identity (IO Note)))
  -> DB.Action IO (Either String (Identity Note))
liftNoteIO = undefined

saveNote :: Note -> DB.Action IO (Either String (Identity Note))
saveNote note'@Note { noteId = Just nId' } = do
  maybeNoteDoc <- DB.findOne (DB.select ["_id" =: nId'] "notes")
  case maybeNoteDoc of
    Just noteDoc -> do
      DB.save "notes" $ noteToDoc note' -- this is not what I want, note' is a partial record, need to merge these
      return $ Right $ return note'
    Nothing -> return $ Left "record not found"
saveNote Note { noteId = Nothing } =
  return $ Left "id field needed for update operation"

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
