{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell #-}
{-# LANGUAGE GADTs,OverloadedStrings,FlexibleContexts, FlexibleInstances #-}
module LearnBasicYesod where

import Yesod as Y
import Data.Text (pack, Text)
import Network.HTTP.Conduit (simpleHttp)
import Network.HTTP.Types (status200)
import Data.Digest.Pure.SHA (showDigest, sha1)
import Database.Persist.Sqlite
import Data.Maybe
import Control.Exception.Lifted hiding (Handler)
import Data.ByteString.Lazy.Internal (ByteString)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Stuff
    value Text
    deriving Show
|]

data Challenge = Challenge ConnectionPool

mkYesod "Challenge" [parseRoutes|
/fib/#Int FibR
/google-body GoogR GET
/store StoreR POST GET
|]

instance Yesod Challenge

instance RenderMessage Challenge FormMessage where
renderMessage _ _ = defaultFormMessage

instance YesodPersist Challenge where
type YesodPersistBackend Challenge = SqlPersist

runDB action = do
  Challenge pool <- getYesod
  runSqlPool action pool

handleFibR :: Int -> Handler RepJson
handleFibR num = jsonToRepJson $ object ["response" .= show_fib]
    where
    show_fib = show $ fib num
    fib :: Int -> Int
    fib 0 = 0
    fib 1 = 1
    fib n = fib (n - 1) + fib (n - 2)

getGoogR :: Handler RepJson
getGoogR = do
    body <- try (simpleHttp "http://www.google.com")
    case body of
    Left (SomeException ex) -> jsonToRepJson $ object ["response" .= ("ERROR: " ++ (show ex))]
    Right val -> jsonToRepJson $ object ["response" .= (showDigest $ sha1 val)]

postStoreR :: Handler ()
postStoreR = do
    mvalue <- runInputPost $ ireq textField "value"
    runDB $ Y.insert $ Stuff mvalue
    sendResponseStatus status200 ()

getStoreR :: Handler RepJson
getStoreR = do
    mvalue <- runDB $ Y.selectFirst [] [Y.Desc StuffValue, Y.LimitTo 1]
    case mvalue of
    Nothing -> jsonToRepJson $ object ["response" .= (show "NO DATA IN DATABASE")]
    Just mvalue' -> jsonToRepJson $ object ["response" .= (show . stuffValue $ Y.entityVal mvalue')]

    main = withSqlitePool ":memory:" 10 $ \pool -> do
    runSqlPool (runMigration migrateAll) pool
    warpDebug 3000 $ Challenge pool
