{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
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
                                                , Undefined
                                                )
import           Data.Text                      ( Text )

importGQLDocumentWithNamespace "src/schema.gql"

rootResolver :: GQLRootResolver IO () Query Undefined Undefined
rootResolver = GQLRootResolver
    { queryResolver        = Query { queryNote, queryNotes }
    , mutationResolver     = undefined
    , subscriptionResolver = undefined
    }
  where
    queryNote QueryNoteArgs { queryNoteArgsId } = pure Note { noteId
                                                            , noteTitle
                                                            , noteCreateDate
                                                            , noteModifyDate
                                                            , noteContent
                                                            }
      where
        noteId _ = pure "testId"
        noteTitle _ = pure (Just "test title")
        noteCreateDate _ = pure "create date"
        noteModifyDate _ = pure "modify date"
        noteContent _ = pure (Just "this is test content")
    queryNotes () = pure
        [ Note { noteId
               , noteTitle
               , noteCreateDate
               , noteModifyDate
               , noteContent
               }
        ]
      where
        noteId _ = pure "testId"
        noteTitle _ = pure (Just "test title")
        noteCreateDate _ = pure "create date"
        noteModifyDate _ = pure "modify date"
        noteContent _ = pure (Just "this is test content")

runApi :: B.ByteString -> IO B.ByteString
runApi = interpreter rootResolver

