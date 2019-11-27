{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE OverloadedStrings     #-}

module Note
  ( Note(..)
  , NewNote(..)
  , UpdateNote(..)
  , NoteArgs(..)
  , updateNote
  , makeNote
  , noteToDoc
  , docToNote
  , updateNoteId
  )
where

import           Data.Morpheus.Types            ( GQLType(..) )
import           Data.Morpheus.Kind             ( OBJECT )
import           Database.MongoDB              as DB
import           GHC.Generics                   ( Generic )
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import           Data.Maybe                     ( fromMaybe
                                                , isJust
                                                , fromJust
                                                )
import           Data.Bson                      ( valueAt )
import           Data.Time.ISO8601              ( parseISO8601 )
import           Data.Time

data Note = Note { noteId :: Maybe Text
                 , noteTitle   :: Text
                 , noteCreateDate  :: UTCTime
                 , noteModifyDate :: UTCTime
                 , noteContent :: Text
                 } deriving (Show, Generic)

data NewNote = NewNote { newTitle :: Text
                       , newContent :: Text
                       } deriving (Show, Generic)

data UpdateNote = UpdateNote { updateId :: Text
                             , maybeNoteTitle :: Maybe Text
                             , maybeNoteContent :: Maybe Text
                             } deriving (Show, Generic)

instance GQLType Note where
  type KIND Note = OBJECT

data NoteArgs = NoteArgs { nTitle :: Maybe Text
                         , nId :: Maybe Text
                         } deriving (Show, Generic)

makeNote :: NewNote -> IO Note
makeNote NewNote { newTitle = nTitle, newContent = nContent } = do
  todaysDate <- getCurrentTime
  return Note { noteId         = Nothing
              , noteTitle      = nTitle
              , noteCreateDate = todaysDate
              , noteModifyDate = todaysDate
              , noteContent    = nContent
              }

updateNoteId :: Text -> Note -> Note
updateNoteId newId note = note { noteId = Just newId }

updateNote :: Note -> UpdateNote -> IO Note
updateNote note1@Note { noteId = nId1, noteTitle = nTitle1, noteCreateDate = nCDate1, noteModifyDate = nMDate1, noteContent = nContent1 } UpdateNote { updateId = nId2, maybeNoteTitle = nTitle2, maybeNoteContent = nContent2 }
  | maybe False (nId2 ==) nId1
  = do
    todaysDate <- getCurrentTime
    return Note { noteId         = nId1
                , noteTitle      = fromMaybe nTitle1 nTitle2
                , noteCreateDate = nCDate1
                , noteModifyDate = todaysDate
                , noteContent    = fromMaybe nContent1 nContent2
                }
  | otherwise
  = return note1

-- this is pretty not typesafe (string literals and valueAt)
docToNote :: DB.Document -> Note
docToNote doc = Note { noteId         = docId
                     , noteTitle      = docTitle
                     , noteCreateDate = fromJust docCreDate
                     , noteModifyDate = fromJust docModDate
                     , noteContent    = docContent
                     }
 where
  docId      = pack . show <$> (doc !? "_id" :: Maybe DB.Value) :: Maybe Text
  docTitle   = typed $ valueAt "title" doc
  docCreDate = parseISO8601 $ typed $ valueAt "createDate" doc
  docModDate = parseISO8601 $ typed $ valueAt "modifyDate" doc
  docContent = typed $ valueAt "content" doc

-- this is also not typesafe (string literals)
noteToDoc :: Note -> DB.Document
noteToDoc Note { noteId = nId, noteTitle = nTitle, noteCreateDate = nCDate, noteModifyDate = nMDate, noteContent = nContent }
  = idDoc
    ++ [ "noteTitle" =: nTitle
       , "noteCreateDate" := (String $ pack $ show nCDate)
       , "noteModifyDate" := (String $ pack $ show nMDate)
       , "noteContent" =: nContent
       ]
 where
  idDoc = case nId of
    Just id' -> ["_id" =: (read (unpack id') :: ObjectId)]
    Nothing  -> []
