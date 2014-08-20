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
import Data.ByteString.Char8         (readFile, unpack)
import Data.Either                   (either)
import Data.Function                 (($), id)
import Data.Functor                  ((<$>))
import Data.Pool                     (Pool)
import Data.Text                     (Text)
import Data.Yaml                     (decodeEither)
import Database.Groundhog.Core       (withConn)
import Database.Groundhog.Postgresql (Postgresql(Postgresql), createPostgresqlPool)
import Database.PostgreSQL.Simple    (ConnectInfo(ConnectInfo, connectDatabase, connectHost, connectPassword, connectPort, connectUser), postgreSQLConnectionString)
import Prelude                       (error)
import System.Environment            (getArgs)
import System.IO                     (IO)
import Yesod.Core                    (Approot(ApprootStatic), Yesod(approot, yesodMiddleware), mkYesod, renderRoute)
import Yesod.Core.Dispatch           (warp)
import Yesod.Core.Handler            (addHeader, getYesod)
import Yesod.Routes.Parse            (parseRoutes)
import Yesod.Squealer                (Squealer(Squealer, database), YesodSquealer(withConnection))



data App
  = App
    { squealer ∷ Squealer
    , connPool ∷ Pool Postgresql
    }

instance Yesod App where
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
/9000             HomeR     OPTIONS GET
/9000/v1/database SquealerR Squealer squealer
|]

optionsHomeR ∷ Handler ()
optionsHomeR = pure ()

getHomeR ∷ Handler Text
getHomeR = pure "ok"

main ∷ IO ()
main
  = do
    [filename]
      ← getArgs

    database
      ← either error id
      ∘ decodeEither
      <$> readFile filename

    connPool
      ← createPostgresqlPool
        connectionString
        poolSize

    warp
      port
      App { squealer = Squealer {..}, ..}
  where
    port = 9000
    poolSize = 10
    connectInfo
      = ConnectInfo
        { connectHost     = "localhost"
        , connectPort     = 5432
        , connectUser     = "username"
        , connectPassword = "xyzzy"
        , connectDatabase = "database"
        }

    connectionString
      = unpack
      ∘ postgreSQLConnectionString
      $ connectInfo
