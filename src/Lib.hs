{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Lib
  ( runApi
  )
where

import qualified Data.ByteString.Lazy.Char8    as B

import           Data.Morpheus                  ( interpreter )
import           Data.Morpheus.Document         ( importGQLDocumentWithNamespace
                                                )
import           Data.Morpheus.Types            ( GQLRootResolver(..)
                                                , IORes
                                                , QUERY
                                                , Resolver
                                                , Undefined
                                                )
import           Data.Text                      ( Text )
import           Data.Bson                      ( valueAt )
import qualified Database.MongoDB              as DB
import           Data.Maybe
import           Control.Monad.Reader           ( runReader )
import           Schema                         ( Query(..)
                                                , Mutation(..)
                                                , getNote
                                                , getNotes
                                                , createNote
                                                , changeNote
                                                )

rootResolver :: GQLRootResolver IO () Query Mutation Undefined
rootResolver = GQLRootResolver
  { queryResolver        = Query { note = getNote, notes = getNotes }
  , mutationResolver     = Mutation { addNote    = createNote
                                    , updateNote = changeNote
                                    }
  , subscriptionResolver = undefined
  }

runApi :: B.ByteString -> IO B.ByteString
runApi = interpreter rootResolver
