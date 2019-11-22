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
                                                , resolveNote
                                                , resolveNotes
                                                )

rootResolver :: GQLRootResolver IO () Query Undefined Undefined
rootResolver = GQLRootResolver
  { queryResolver        = Query { note = resolveNote, notes = resolveNotes }
  , mutationResolver     = undefined
  , subscriptionResolver = undefined
  }

runApi :: B.ByteString -> IO B.ByteString
runApi = interpreter rootResolver
