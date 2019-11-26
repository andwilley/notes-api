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

-- infix is awkward with a qualifier
(=:) :: DB.Val v => DB.Label -> v -> DB.Field
(=:) = (DB.=:)

-- TODO: consider having func for each query param (noteById, noteByTitle)
data Query m = Query { note :: NoteArgs -> m Note
                     , notes :: () -> m [Note]
                     } deriving (Generic, GQLType)

data Mutation m = Mutation { addNote :: Note -> m Text
                          --  , updateNote :: Note -> m ()
                          --  , deleteNote :: Text -> m ()
                           } deriving (Generic, GQLType)

data Note = Note { noteId :: Maybe Text
                 , title   :: Text
                 , createDate  :: Maybe Text
                 , modifyDate :: Maybe Text
                 , content :: Text
                 } deriving (Generic)

instance GQLType Note where
  type KIND Note = OBJECT

data NoteArgs = NoteArgs { nTitle :: Maybe Text
                         , nId :: Maybe Text
                         } deriving (Show, Generic)

getNote :: NoteArgs -> IORes e Note
getNote NoteArgs { nTitle, nId } = liftEitherM $ do
  eitherNote <- runQuery $ getNoteByProp nTitle nId
  return $ runIdentity <$> eitherNote

getNotes :: () -> IORes () [Note]
getNotes () = liftEitherM $ runQuery getAllNotes

createNote :: Note -> IOMutRes e Text
createNote note' = liftEitherM $ do
  n <- runQuery $ insertNote note'
  return $ runIdentity <$> n

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
  makeSelector Nothing (Just id') =
    ["_id" =: (read $ unpack id' :: ObjectId)]
  makeSelector (Just title') Nothing = ["title" =: title']
  makeSelector Nothing       Nothing = []

insertNote :: Note -> DB.Action IO (Either String (Identity Text))
insertNote note' = do
  id' <- DB.insert "notes" doc
  return $ Right $ return (pack $ show id' :: Text)
 where
  doc =
    noteToDoc note' { createDate = Just "today", modifyDate = Just "today" }

eitherDocFromMaybe :: Maybe DB.Document -> Either String (Identity DB.Document)
eitherDocFromMaybe (Just doc) = Right $ return doc
eitherDocFromMaybe Nothing    = Left "No result found"

docToNote :: DB.Document -> Note
docToNote doc = Note { noteId     = docId
                     , title      = docTitle
                     , createDate = docCreDate
                     , modifyDate = docModDate
                     , content    = docContent
                     }
 where
  docId      = fmap (pack . show) (doc !? "_id" :: Maybe DB.Value) :: Maybe Text
  docTitle   = typed $ valueAt "title" doc
  docCreDate = typed <$> doc !? "createDate"
  docModDate = typed <$> doc !? "modifyDate"
  docContent = typed $ valueAt "content" doc

noteToDoc :: Note -> DB.Document
noteToDoc Note { noteId = nId, title = nTitle, createDate = nCDate, modifyDate = nMDate, content = nContent }
  = idDoc ++ cDateDoc ++ mDateDoc ++ ["title" =: nTitle, "content" =: nContent]
 where
  idDoc = case nId of
    Just id' -> ["_id" =: (read (unpack id') :: ObjectId)]
    Nothing  -> []
  cDateDoc = case nCDate of
    Just nCDate' -> ["createDate" =: nCDate']
    Nothing      -> []
  mDateDoc = case nMDate of
    Just nMDate' -> ["modifyDate" =: nMDate']
    Nothing      -> []

-- docTo :: Constr -> DB.Document -> Note
-- docTo constr doc = docTo' constrList doc constr
--     where constrList = constrFields constr

-- docTo' :: [String] -> DB.Document -> Constr -> Note
-- docTo' [field         ] doc constr = constr $ valueAt field doc
-- docTo' (field : fields) doc constr = "field"


    -- Note (Just "title1") "cDate1" "mDate1" "content1"
