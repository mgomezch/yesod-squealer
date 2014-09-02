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
import Data.Maybe                    (maybe)
import Data.Pool                     (Pool)
import Data.String                   (fromString)
import Data.Text                     (Text)
import Data.Yaml                     (decodeEither)
import Database.Groundhog.Core       (withConn)
import Database.Groundhog.Postgresql (Postgresql(Postgresql), createPostgresqlPool)
import Database.PostgreSQL.Simple    (ConnectInfo(ConnectInfo, connectDatabase, connectHost, connectPassword, connectPort, connectUser), postgreSQLConnectionString)
import Database.Squealer.Types       (Database(Database, dbname), unIdentifier)
import Prelude                       (error)
import System.Environment            (getArgs, lookupEnv)
import System.IO                     (IO)
import Text.Read                     (read)
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

    [filename]
      ← getArgs

    database @ Database {..}
      ← either error id
      ∘ decodeEither
      <$> readFile filename

    let
      defaultDB
        = T.unpack
        $ unIdentifier dbname

    appRoot         ← maybe ""        fromString <$> lookupEnv "APPROOT"
    port            ← maybe 9000      read       <$> lookupEnv "PORT"
    poolSize        ← maybe 10        read       <$> lookupEnv "POOLSIZE"
    connectHost     ← maybe ""        fromString <$> lookupEnv "PGHOST"
    connectPort     ← maybe 5432      read       <$> lookupEnv "PGPORT"
    connectUser     ← maybe ""        fromString <$> lookupEnv "PGUSER"
    connectPassword ← maybe ""        fromString <$> lookupEnv "PGPASSWORD"
    connectDatabase ← maybe defaultDB fromString <$> lookupEnv "DATABASE"

    connPool
      ← createPostgresqlPool
        (connectionString ConnectInfo {..})
        poolSize

    warp
      port
      App { squealer = Squealer {..}, ..}
  where
    connectionString
      = B8.unpack
      ∘ postgreSQLConnectionString
