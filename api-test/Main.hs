{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Main (main) where

import Control.Applicative           (pure)
import Control.Monad.Unicode         ((=≪))
import Control.Category.Unicode      ((∘))
import Data.ByteString.Char8         (readFile)
import Data.Either                   (either)
import Data.Function                 (($), id)
import Data.Functor                  ((<$>))
import Data.Maybe                    (listToMaybe, maybe)
import Data.Pool                     (Pool)
import Data.String                   (fromString)
import Data.Text                     (Text)
import Data.Yaml                     (decodeEither)
import Database.Groundhog.Core       (withConn)
import Database.Groundhog.Postgresql (Postgresql(Postgresql), createPostgresqlPool)
import Database.PostgreSQL.Simple    (ConnectInfo(ConnectInfo, connectDatabase, connectHost, connectPassword, connectPort, connectUser), postgreSQLConnectionString)
import Database.Squealer.Types       (Database(Database, dbname), unIdentifier)
import Prelude                       (error)
import System.Environment            (getArgs)
import System.IO                     (IO)
import Yesod.Core                    (Approot(ApprootMaster), Yesod(approot, yesodMiddleware), mkYesod, renderRoute)
import Yesod.Core.Dispatch           (warp)
import Yesod.Core.Handler            (addHeader, getYesod)
import Yesod.Routes.Parse            (parseRoutes)
import Yesod.Squealer                (Squealer(Squealer, database), YesodSquealer(withConnection))

import qualified Data.ByteString.Char8 as B8 (unpack)
import qualified Data.Text             as T  (unpack)



data App
  = App
    { squealer ∷ Squealer
    , connPool ∷ Pool Postgresql
    , appRoot  ∷ Text
    }

instance Yesod App where
  approot
    = ApprootMaster appRoot

  yesodMiddleware handler
    = do
      addHeader "Access-Control-Allow-Origin" "*"
      addHeader "Access-Control-Allow-Methods" "GET, PUT, POST, DELETE, OPTIONS"
      addHeader "Access-Control-Allow-Headers" "Accept, Origin, X-Requested-With"
      handler

instance YesodSquealer App where
  withConnection f
    = withConn (f ∘ getConnection) ∘ connPool =≪ getYesod
    where
      getConnection (Postgresql c) = c

mkYesod "App" [parseRoutes|
/v1/database SquealerR Squealer squealer
|]

main ∷ IO ()
main
  = do
    -- This is only to silence unimportant warnings.
    _ ← pure ([] ∷ [Handler ()])
    _ ← pure ([] ∷ [Widget])
    _ ← pure resourcesApp

    (filename : maybeAppRoot)
      ← getArgs

    let
      appRoot
        = maybe "" fromString
        $ listToMaybe maybeAppRoot

    database
      ← either error id
      ∘ decodeEither
      <$> readFile filename

    connPool
      ← createPostgresqlPool
        (connectionString database)
        poolSize

    warp
      port
      App { squealer = Squealer {..}, ..}
  where
    port = 9000
    poolSize = 10

    connectionString
      Database {..}
      = B8.unpack
      ∘ postgreSQLConnectionString
      $ connectInfo
      where
        connectInfo
          = ConnectInfo
            { connectHost     = ""
            , connectPort     = -1
            , connectUser     = ""
            , connectPassword = ""
            , connectDatabase = T.unpack $ unIdentifier dbname
            }
