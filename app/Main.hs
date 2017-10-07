module Main where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Bifunctor (first)
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Database.Esqueleto ( LeftOuterJoin(..)
                          , InnerJoin(..)
                          , select, from, in_, just
                          , on, where_, val, valList
                          , (^.), (?.))
import qualified Database.Esqueleto as E
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite
import Database.Persist.TH
import Text.Show.Pretty

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User sql=users
  name Text
  email Text
  UniqueUserEmail email
  deriving Eq Show

Post
  userSlackId Text
  userName Text
  someText Text
  someOtherText Text
  deriving Show

Comment
  comment Text
  post PostId
  deriving Show
  
Tag
  tagName Text
  deriving Show

PostTag
  tag TagId
  post PostId
  deriving Show
|]

-- type ControlIO m = (MonadIO m, MonadBaseControl IO m)

type DBM m a =
  (MonadIO m, MonadThrow m, Monad m) => SqlPersistT m a

type DB a = forall m. DBM m a

type DBVal val =
  ( PersistEntity val
  , PersistEntityBackend val ~ SqlBackend
  , PersistStore (PersistEntityBackend val))

dumpMigration :: DB ()
dumpMigration = printMigration migrateAll

runMigrations :: DB ()
runMigrations = runMigration migrateAll

runDB :: DB a -> IO a
runDB = runSqlite ":memory:"

main :: IO ()
main = runDB dumpMigration

-- https://www.dailydrip.com/blog/ecto-vs-activerecord.html

--- Get all records

-- ActiveRecord
-- Model.all

-- Ecto
-- Repo.all(App.Model)

getAllUsers :: DB [Entity User]
getAllUsers = selectList [] []

-- Alternately, with TypeApplications
-- (please don't do this)
getAllUsers' :: IO ()
getAllUsers' = do
  users <- runDB $ selectList @_ @_ @User [] []
  mapM_ print users

--- Search by name

-- ActiveRecord
-- Model.find_by(name: name)

-- Ecto
-- Repo.one(from t in App.Model, where: t.name == ^name, limit: 1)

getFirstUserByEmail :: Text -> DB (Maybe (Entity User))
getFirstUserByEmail email = selectFirst [UserEmail ==. email] []

-- Fetch a single record based on id=1

-- ActiveRecord
-- Model.find(1)

-- Ecto
-- Model |> Repo.get!(1)

getIdOneUser :: DB (Maybe (Entity User))
getIdOneUser = getEntity (toSqlKey 1)

-- Using ActiveRecord, we can do:

-- artist = Artist.get(1)
-- artist.name = "Name"
-- artist.save

updateFirstUserName :: Text -> DB ()
updateFirstUserName newName = do
  update (toSqlKey 1) [UserName =. newName]

-- parameterizing it
updateFirstUserName' :: Key User -> Text -> DB ()
updateFirstUserName' userKey newName = do
  update userKey [UserName =. newName]

-- updateFirstUserName :: Text -> DB ()
-- updateFirstUserName = updateFirstUserName' (toSqlKey 1)

-- As a functional language we don't have data with state, nor do we have behavior. We only have functions.

-- In general, if you want to talk with the database, you need to talk with the Repository first.

-- artist = Repo.get(Artist, 1)
-- changeset = Artist.changeset(artist, name: "Changed name")
-- Repo.update(changeset)
-- If we check side-by-side what Active Record and repository does, we cannot see when Active Record touches the Database. We just do a save and it hits the database implicitly. In Ecto, you always interact with the database explicitly.

-- Ecto will not talk to the database without you asking it to. Everything is totally explicit. Any interaction with the database should pass through the Repository.

-- I mean, yes, you only talk to the database when you use 'runDB' or similar.

migrateCreateUserThenChangeName :: IO ()
migrateCreateUserThenChangeName = do
  runDB $ do
    runMigrations
    newUserKey <- insert $ User { userName = "Chris", userEmail = "chris@lol.com" }
    updateFirstUserName' newUserKey "New Name"
    userWithNewName <- get newUserKey
    liftIO $ print userWithNewName

-- Prelude> migrateCreateUserThenChangeName 
-- Migrating: CREATE TABLE "users"("id" INTEGER PRIMARY KEY,"name" VARCHAR NOT NULL,"email" VARCHAR NOT NULL,CONSTRAINT "unique_user_email" UNIQUE ("email"))
-- Just (User {userName = "New Name", userEmail = "chris@lol.com"})
-- Prelude> 

-- Migrations
-- Ecto also has migrations. This is not really different from what ActiveRecord offers to us.

-- defmodule SlackPosting.Repo.Migrations.CreatePosts do
--   use Ecto.Migration

--   def change do
--     create table(:posts) do
--       add :text, :text
--       add :user_slack_id, :string
--       add :user_name, :string

--       timestamps()
--     end

--   end
-- end

-- Persistent does too, but approaches it differently by focusing on
-- generating fresh and differential migrations against the database
-- rather than creating a migration DSL.
-- cf. dumpMigrations, runMigrations

-- Schemas
-- Schema is normally a map between your types and your database. But not necessarily.

-- If we check the documentation:

--  An Ecto schema is used to map any data source into an Elixir struct. One of such use cases is to map data coming from a repository, usually a table, into Elixir structs. 
-- An interesting thing to mention is that we don't need a schema for using Ecto. We can bypass the use of Schemas by using the table name as a string. Schemas are very flexible.

-- Here is an example of Schema definition in Ecto.

-- defmodule SlackPosting.Journals.Post do
--   use Ecto.Schema
--   import Ecto.Changeset
--   alias SlackPosting.Journals.Post

--   schema "posts" do
--     field :user_slack_id, :string
--     field :user_name, :string
--     field :text, :string
--     many_to_many :tags, SlackPosting.Journals.Tag, join_through: SlackPosting.Journals.PostTag
--     has_many :comments, SlackPosting.Journals.Comment

--     timestamps()
--   end

--   @doc false
--   def changeset(%Post{} = post, attrs) do
--     post
--     |> cast(attrs, [:text, :user_slack_id, :user_name])
--     |> validate_required([:text, :user_slack_id])
--   end
-- end

-- Changeset
-- A changeset handles the entire lifecycle of database updates. Filtering, casting, validations, handling errors, etc.

-- For our Post, we have validations for text and user_slack_id. We are using the cast to only get the correct attributes from our post.

--   def changeset(%Post{} = post, attrs) do
--     post
--     |> cast(attrs, [:text, :user_slack_id, :user_name])
--     |> validate_required([:text, :user_slack_id])
--   end
-- Any validation will be here.

-- Associations
-- Ecto also offers us associations. In this example, we are doing a has_many association and a many_to_many association.

-- schema "posts" do
--     field :user_slack_id, :string
--     field :user_name, :string
--     field :text, :string
--     many_to_many :tags, SlackPosting.Journals.Tag, join_through: SlackPosting.Journals.PostTag
--     has_many :comments, SlackPosting.Journals.Comment
-- This is pretty similar to what ActiveRecord does.


-- def list_posts do
--     Repo.all(Post)
--     |> Repo.preload([:comments, :tags])
-- end

migrateFixturesTest :: IO ()
migrateFixturesTest = do
  runDB $ do
    runMigrations
    pk1 <- insert $ Post "slack1" "name1" "" ""
    pk2 <- insert $ Post "slack2" "name2" "" ""
    pk3 <- insert $ Post "slack2" "name3" "" ""
    _ <- insert $ Comment "pk1 c1" pk1
    _ <- insert $ Comment "pk1 c2" pk1
    _ <- insert $ Comment "pk2 c3" pk2
    tg1 <- insert $ Tag "tag1"
    ptg1 <- insert $ PostTag tg1 pk1
    pwcat <- postsWithCommentsAndTags
    liftIO $ pPrint pwcat

tagsForPosts :: [Key Post] -> DB [(Key Post, Entity Tag)]
tagsForPosts postKeys =
  unValueThePostKeys $
  select $
  from $ \ ( postTag `InnerJoin` tag ) -> do
    on (tag ^. TagId
        E.==. postTag ^. PostTagTag)
    where_ (postTag ^. PostTagPost
            `in_` valList postKeys)
    return (postTag ^. PostTagPost, tag)
  where unValueThePostKeys :: DB [(E.Value (Key Post), Entity Tag)]
                           -> DB [(Key Post, Entity Tag)]
        unValueThePostKeys = (fmap . fmap) (first E.unValue)

postsWithCommentsAndTags :: DB (Map
                                (Key Post)
                                (Entity Post, [Entity Comment], [Entity Tag]))
postsWithCommentsAndTags = do
  postsAndComments <- posts
  let postKeys = fmap (entityKey . fst) postsAndComments
  postKeysWithTags <- tagsForPosts postKeys
  let initialMap = postsInitialMap postsAndComments
      postsWithTags = addTagsToMap postKeysWithTags initialMap
  return postsWithTags
  where
    posts =
      select $
      from $ \ ( post
                 `InnerJoin`
                 comment ) -> do
        on (post ^. PostId
            E.==. (comment ^. CommentPost))
        return (post, comment)

    postsInitialMap :: [(Entity Post, Entity Comment)]
                    -> Map (Key Post) (Entity Post, [Entity Comment], [Entity Tag])
    postsInitialMap postsAndComments =
      foldl' insertPostCom M.empty postsAndComments
      where insertPostCom m (post, comment) =
              M.insertWith
              (\ _ (post, comments, tags) -> (post, comment : comments, tags))
              (entityKey post) (post, [comment], []) m

    addTagsToMap :: [(Key Post, Entity Tag)]
                 -> Map (Key Post) (Entity Post, [Entity Comment], [Entity Tag])
                 -> Map (Key Post) (Entity Post, [Entity Comment], [Entity Tag])
    addTagsToMap postKeysTags initialMap =
      foldl' insertPostKeyTag initialMap postKeysTags
      where insertPostKeyTag :: Map (Key Post)
                                (Entity Post, [Entity Comment], [Entity Tag])
                             -> (Key Post, Entity Tag)
                             -> Map (Key Post)
                                (Entity Post, [Entity Comment], [Entity Tag])
            insertPostKeyTag m (postKey, tagEntity) =
              M.adjust
              (\(post, comment, tags) ->
                  (post, comment, tagEntity : tags))
              postKey
              m
