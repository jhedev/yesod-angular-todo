{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import           Yesod
import           Yesod.Auth
import           Yesod.Auth.GoogleEmail
import           Yesod.Static
import           Text.Hamlet (hamletFile)
import           Text.Julius
import           Data.Aeson
import           Data.Default (def)
import           Data.Typeable (Typeable)
import qualified Data.Text as T
import           Data.Maybe (fromJust)
import           Network.HTTP.Conduit (Manager, newManager, conduitManagerSettings)
import           Database.Persist.Sqlite
    ( ConnectionPool, SqlPersistT, runSqlPool, runMigration
    , createSqlitePool, runSqlPersistMPool
    )
import           Database.Persist.TH 
import           Data.Typeable
import           Control.Applicative 
import           Control.Monad (mzero)

import           System.Environment


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
Task
    task String
    done Bool
    deriving Show
|]


instance ToJSON (Entity Task) where
    toJSON (Entity tid (Task task done)) = object
        [ "id" .= tid
        , "task" .= task
        , "done" .= done
        ]

instance FromJSON Task where
    parseJSON (Object o)  = Task
        <$> o  .: "task"
        <*> o  .: "done"
    parseJSON _ = mzero

-- The application
data Todo = Todo 
    { getStatic :: Static 
    , connPool :: ConnectionPool
    , httpManager :: Manager
    }

-- Routes
mkYesod "Todo" [parseRoutes|
/                       HomeR GET
/static                 StaticR Static getStatic 
/api/todo               TodosR GET PUT
/api/todo/#TaskId       TodoR  GET PUT DELETE
|]


-- Instances of the application
instance Yesod Todo where
    approot = ApprootStatic "http://localhost:3000"

instance YesodPersist Todo where
    type YesodPersistBackend Todo = SqlPersistT
    runDB f = do
        master <- getYesod
        let pool = connPool master
        runSqlPool f pool

instance RenderMessage Todo FormMessage where
    renderMessage _ _ = defaultFormMessage



-- Handler for /
getHomeR :: Handler Html
getHomeR = defaultLayout $(whamletFile "static/index.hamlet")

-- Handler returning all todos as Json
getTodosR :: Handler Value
getTodosR = runDB (selectList [] []) >>= returnJson . asTodoEntities
    where asTodoEntities :: [Entity Task] -> [Entity Task]
          asTodoEntities = id

-- Handler accepting put requests to store new todos
putTodosR :: Handler Value
putTodosR = do
            todo <- parseJsonBody_
            tid <- runDB $ insert (todo :: Task)
            (runDB (get404 tid)) >>= returnJson . Entity tid

-- Handler returning todos by given id
getTodoR :: TaskId -> Handler Value
getTodoR tid = runDB (get404 tid) >>= returnJson . Entity tid


-- Handler for updating certain todos by id
putTodoR :: TaskId -> Handler Value
putTodoR tid = do
            todo <- parseJsonBody_
            runDB $ update tid [TaskTask =. (taskTask (todo :: Task)), TaskDone =. (taskDone (todo :: Task))]
            sendResponseCreated $ TodoR tid


-- Handler for deleting certain todos by id
deleteTodoR :: TaskId -> Handler Value
deleteTodoR tid = do
                runDB $ delete tid
                redirect HomeR


-- Main application
main :: IO ()
main = do
    port <- getEnv "PORT"
    s <- static "static"
    pool <- createSqlitePool "dev.sqlite3" 10
    runSqlPersistMPool (runMigration migrateAll) pool
    manager <- newManager conduitManagerSettings 
    warp (read port :: Int) $ Todo s pool manager
