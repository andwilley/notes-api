{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE OverloadedStrings     #-}

module Note
  ( Note(..)
  , NewNote(..)
  , UpdateNote(..)
  , NoteArgs(..)
  , mergeNote
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
import           Data.Time.ISO8601              ( parseISO8601
                                                , formatISO8601
                                                )
import           Data.Time

data Note = Note { noteId :: Maybe Text
                 , noteTitle   :: Text
                 , noteCreateDate  :: Text
                 , noteModifyDate :: Text
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
              , noteCreateDate = pack $ formatISO8601 todaysDate
              , noteModifyDate = pack $ formatISO8601 todaysDate
              , noteContent    = nContent
              }

updateNoteId :: Text -> Note -> Note
updateNoteId newId note = note { noteId = Just newId }

mergeNote :: Note -> UpdateNote -> IO Note
mergeNote note1@Note { noteId = nId1, noteTitle = nTitle1, noteCreateDate = nCDate1, noteModifyDate = nMDate1, noteContent = nContent1 } UpdateNote { updateId = nId2, maybeNoteTitle = nTitle2, maybeNoteContent = nContent2 }
  | maybe False (nId2 ==) nId1
  = do
    todaysDate <- getCurrentTime
    return Note { noteId         = nId1
                , noteTitle      = fromMaybe nTitle1 nTitle2
                , noteCreateDate = nCDate1
                , noteModifyDate = pack $ formatISO8601 todaysDate
                , noteContent    = fromMaybe nContent1 nContent2
                }
  | otherwise
  = return note1

-- this is pretty not typesafe (string literals and valueAt)
docToNote :: DB.Document -> Note
docToNote doc = Note { noteId         = docId
                     , noteTitle      = docTitle
                     , noteCreateDate = pack $ show $ fromMaybe "" docCreDate
                     , noteModifyDate = pack $ show $ fromMaybe "" docModDate
                     , noteContent    = docContent
                     }
 where
  docId      = pack . show <$> (doc !? "_id" :: Maybe DB.Value) :: Maybe Text
  docTitle   = typed $ valueAt "noteTitle" doc
  docCreDate = pack . show <$> parseISO8601 (typed $ valueAt "noteCreateDate" doc)
  docModDate = pack . show <$> parseISO8601 (typed $ valueAt "noteModifyDate" doc)
  docContent = typed $ valueAt "noteContent" doc

-- this is also not typesafe (string literals)
noteToDoc :: Note -> DB.Document
noteToDoc Note { noteId = nId, noteTitle = nTitle, noteCreateDate = nCDate, noteModifyDate = nMDate, noteContent = nContent }
  = idDoc
    ++ [ "noteTitle" =: nTitle
       , "noteCreateDate" := String nCDate
       , "noteModifyDate" := String nMDate
       , "noteContent" =: nContent
       ]
 where
  idDoc = case nId of
    Just id' -> ["_id" =: (read (unpack id') :: ObjectId)]
    Nothing  -> []
