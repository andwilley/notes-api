{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Schema
    ( Query(..)
    , resolveNote
    , resolveNotes
    )
where

import           Data.Typeable
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
import           Data.Text                      ( Text )
import           Data.Bson                      ( Value
                                                , valueAt
                                                , typed
                                                )

data Query m = Query { note :: NoteArgs -> m [Note]
                     , notes :: () -> m [Note]
                     } deriving (Generic, GQLType)

data Note = Note { noteId :: Text
                 , title   :: Text
                 , createDate  :: Text
                 , modifyDate :: Text
                 , content :: Text
                 } deriving (Generic)

instance GQLType Note where
    type KIND Note = OBJECT

data NoteArgs = NoteArgs { noteTitle :: Maybe Text
                         , noteCreateDate :: Maybe Text
                         } deriving (Show, Generic)

resolveNote :: NoteArgs -> IORes e [Note]
resolveNote NoteArgs { noteTitle, noteCreateDate } =
    liftEitherM $ runQuery $ getNoteByProp noteTitle noteCreateDate

resolveNotes :: () -> IORes () [Note]
resolveNotes () = liftEitherM $ runQuery getAllNotes

-- what if this used Applicative / Monad m in stead of [a],
-- we could potentially just <$> the results
runQuery :: DB.Action IO [a] -> IO (Either String [a])
runQuery act = do
    pipe <- DB.connect (DB.host "127.0.0.1")
    e    <- DB.access pipe DB.master "notes" act
    DB.close pipe
    return $ Right e

getAllNotes :: DB.Action IO [Note]
getAllNotes = fmap (map docToNote) (DB.rest =<< DB.find (DB.select [] "notes"))

getNoteByProp :: Maybe Text -> Maybe Text -> DB.Action IO [Note]
getNoteByProp title cDate =
    map docToNote . dbResultFromMaybe <$> DB.findOne (DB.select [] "notes")

dbResultFromMaybe :: Maybe DB.Document -> [DB.Document]
dbResultFromMaybe (Just doc) = [doc]
dbResultFromMaybe Nothing    = []

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
