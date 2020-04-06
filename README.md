# notes-api

## Schema

### Query

```graphql
note(
  title: String
  noteid: String
): Note!

notes: [Note!]!
```

### Mutation

```graphql
addNote(
  newTitle: String!
  newContent: String!
): Note!

updateNote(
  updateId: String!
  maybeNoteTitle: String
  maybeNoteContent: String
): Note!

deleteNotes(
  noteIdList: [String!]!
): [Note!]!
```

## Usage

Requires definition of src/Apikeys.hs

```haskell
{-# LANGUAGE OverloadedStrings     #-}

module Apikeys (apikeys) where

import qualified Data.Set as Set
import qualified Data.Text.Lazy as TL

apikeys :: Set.Set TL.Text
apikeys = Set.fromList [ "test"
                       , "test2"
                       ]
```
