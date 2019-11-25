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
  )
where

import           Data.Morpheus.Types            ( GQLType(..)
                                                , IORes
                                                , liftEitherM
                                                )
import           Data.Morpheus.Kind             ( OBJECT )
import           GHC.Generics                   ( Generic )
import qualified Database.MongoDB              as DB
import           Data.Data                      ( Constr
                                                , constrFields
                                                )
import           Control.Monad.Identity
import           Data.Text                      ( Text )
import           Data.Bson                      ( Value
                                                , valueAt
                                                , typed
                                                )

-- infix is awkward with a qualifier
(=:) :: DB.Val v => DB.Label -> v -> DB.Field
(=:) = (DB.=:)

data Query m = Query { note :: NoteArgs -> m Note
                     , notes :: () -> m [Note]
                     } deriving (Generic, GQLType)

data Mutation m = Mutation { addNote :: Note -> m Note
                           , updateNote :: Note -> m Note
                           , deleteNote :: Text -> m Note
                           } deriving (Generic, GQLType)

data Note = Note { noteId :: Text
                 , title   :: Text
                 , createDate  :: Text
                 , modifyDate :: Text
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
getNoteByProp title noteId = do -- inside the Action IO monad
  maybeNoteDoc <- DB.findOne (DB.select (makeSelector title noteId) "notes")
  return $ do -- inside the Either monad
    idDoc <- eitherDocFromMaybe maybeNoteDoc
    return $ docToNote <$> idDoc
 where
  makeSelector (Just title') (Just id') = ["title" =: title', "noteId" =: id']
  makeSelector Nothing       (Just id') = ["noteId" =: id']
  makeSelector (Just title') Nothing    = ["title" =: title']
  makeSelector Nothing       Nothing    = []

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
  docId      = typed $ valueAt "noteId" doc
  docTitle   = typed $ valueAt "title" doc
  docCreDate = typed $ valueAt "createDate" doc
  docModDate = typed $ valueAt "modifyDate" doc
  docContent = typed $ valueAt "content" doc

-- docTo :: Constr -> DB.Document -> Note
-- docTo constr doc = docTo' constrList doc constr
--     where constrList = constrFields constr

-- docTo' :: [String] -> DB.Document -> Constr -> Note
-- docTo' [field         ] doc constr = constr $ valueAt field doc
-- docTo' (field : fields) doc constr = "field"


    -- Note (Just "title1") "cDate1" "mDate1" "content1"
