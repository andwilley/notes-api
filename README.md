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

## Mutation

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
