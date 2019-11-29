{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Note
  ( Note(..)
  , NewNote(..)
  , UpdateNote(..)
  , NoteArgs(..)
  , NoteIdList(..)
  , noteId'
  , noteTitle'
  , noteContent'
  , noteCreateDate'
  , noteModifyDate'
  , setNoteId
  , mergeNote
  , makeNote
  , noteToDoc
  , docToNote
  , updateNoteCreateDate
  , updateNoteModifyDate
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
import           Data.Maybe                     ( fromMaybe )
import           Data.Bson                      ( valueAt )
import           Data.Time.ISO8601              ( parseISO8601
                                                , formatISO8601
                                                )
import           Data.Time
import           Data.Time.Clock.POSIX          ( posixSecondsToUTCTime )
import           Data.Data                      ( Constr
                                                , constrFields
                                                )
import           Lens.Simple

-- entities

data Note = Note { noteId :: Maybe Text
                 , noteTitle   :: Text
                 , noteCreateDate  :: Text
                 , noteModifyDate :: Text
                 , noteContent :: Text
                 } deriving (Show, Generic)

instance GQLType Note where
  type KIND Note = OBJECT

-- lenses

noteId' :: Lens' Note (Maybe Text)
noteId' = lens noteId (\note newId -> note { noteId = newId })

noteTitle' :: Lens' Note Text
noteTitle' = lens noteTitle (\note newTitle -> note { noteTitle = newTitle })

noteCreateDate' :: Lens' Note Text
noteCreateDate' = lens
  noteCreateDate
  (\note newCreateDate -> note { noteCreateDate = newCreateDate })

noteModifyDate' :: Lens' Note Text
noteModifyDate' = lens
  noteModifyDate
  (\note newModifyDate -> note { noteModifyDate = newModifyDate })

noteContent' :: Lens' Note Text
noteContent' =
  lens noteContent (\note newContent -> note { noteContent = newContent })

-- getters and setters

setNoteId :: Text -> Note -> Note
setNoteId newId = over noteId' (const $ Just newId)

-- query params

data NewNote = NewNote { newTitle :: Text
                       , newContent :: Text
                       } deriving (Show, Generic)

data UpdateNote = UpdateNote { updateId :: Text
                             , maybeNoteTitle :: Maybe Text
                             , maybeNoteContent :: Maybe Text
                             } deriving (Show, Generic)

data NoteArgs = NoteArgs { title :: Maybe Text
                         , noteid :: Maybe Text
                         } deriving (Show, Generic)

newtype NoteIdList = NoteIdList {noteIdList :: [Text]} deriving (Show, Generic)

-- utilities

makeNote :: NewNote -> Note
makeNote NewNote { newTitle = nTitle, newContent = nContent } = Note
  { noteId         = Nothing
  , noteTitle      = nTitle
  , noteCreateDate = ""
  , noteModifyDate = ""
  , noteContent    = nContent
  }

updateNoteCreateDate :: DB.Document -> IO DB.Document
updateNoteCreateDate doc = do
  timeNow <- getCurrentTime
  return $ merge ["noteCreateDate" =: timeNow] doc

updateNoteModifyDate :: DB.Document -> IO DB.Document
updateNoteModifyDate doc = do
  timeNow <- getCurrentTime
  return $ merge ["noteModifyDate" =: timeNow] doc

mergeNote :: Note -> UpdateNote -> Note
mergeNote note1@Note { noteId = nId1, noteTitle = nTitle1, noteCreateDate = nCDate1, noteModifyDate = nMDate1, noteContent = nContent1 } UpdateNote { updateId = nId2, maybeNoteTitle = nTitle2, maybeNoteContent = nContent2 }
  | maybe False (nId2 ==) nId1
  = Note { noteId         = nId1
         , noteTitle      = fromMaybe nTitle1 nTitle2
         , noteCreateDate = nCDate1
         , noteModifyDate = nMDate1
         , noteContent    = fromMaybe nContent1 nContent2
         }
  | otherwise
  = note1

-- this is pretty not typesafe (string literals and valueAt)
docToNote :: DB.Document -> Note
docToNote doc = Note { noteId         = docId
                     , noteTitle      = docTitle
                     , noteCreateDate = pack $ formatISO8601 docCreDate
                     , noteModifyDate = pack $ formatISO8601 docModDate
                     , noteContent    = docContent
                     }
 where
  docId      = pack . show <$> (doc !? "_id" :: Maybe DB.Value) :: Maybe Text
  docTitle   = typed $ valueAt "noteTitle" doc
  docCreDate = typed $ valueAt "noteCreateDate" doc :: UTCTime
  docModDate = typed $ valueAt "noteModifyDate" doc :: UTCTime
  docContent = typed $ valueAt "noteContent" doc

-- this is also not typesafe (string literals)
noteToDoc :: Note -> DB.Document
noteToDoc Note { noteId = nId, noteTitle = nTitle, noteCreateDate = nCDate, noteModifyDate = nMDate, noteContent = nContent }
  = idDoc
    ++ [ "noteTitle" =: nTitle
       , "noteCreateDate" := UTC
         (fromMaybe (posixSecondsToUTCTime 0) (parseISO8601 $ unpack nCDate))
       , "noteModifyDate" := UTC
         (fromMaybe (posixSecondsToUTCTime 0) (parseISO8601 $ unpack nMDate))
       , "noteContent" =: nContent
       ]
 where
  idDoc = case nId of
    Just id' -> ["_id" =: (read (unpack id') :: ObjectId)]
    Nothing  -> []


-- docTo :: Constr -> DB.Document -> Note
-- docTo constr doc = docTo' constrList doc constr
--     where constrList = constrFields constr

-- docTo' :: [String] -> DB.Document -> Constr -> Note
-- docTo' [field         ] doc constr = constr $ valueAt field doc
-- docTo' (field : fields) doc constr = "field"


    -- Note (Just "title1") "cDate1" "mDate1" "content1"
