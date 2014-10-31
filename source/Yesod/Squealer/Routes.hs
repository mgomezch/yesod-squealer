{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE UnicodeSyntax       #-}
{-# LANGUAGE ViewPatterns        #-}

module Yesod.Squealer.Routes where

import Data.Text                  (Text)
import Database.Squealer          (Database)
import Yesod.Core                 (Yesod, renderRoute)
import Yesod.Core.Dispatch        (mkYesodSubData, parseRoutes)
import Yesod.Core.Types           (HandlerT)
import Database.PostgreSQL.Simple (Connection)

data Squealer
  = Squealer
    { database ∷ Database
    }

mkYesodSubData "Squealer" [parseRoutes|
/                                DatabaseR    OPTIONS GET
/#Text                           TableR       OPTIONS GET
/#Text/column/#Text              ColumnR      OPTIONS GET
/#Text/rows                      RowsR        OPTIONS GET POST
/#Text/version/#Text             VersionR     OPTIONS GET PUT DELETE
/#Text/version/#Text/predecessor PredecessorR OPTIONS GET
/#Text/version/#Text/successor   SuccessorR   OPTIONS GET
|]

class Yesod master ⇒ YesodSquealer master where
  withConnection
    ∷ (Connection → HandlerT master IO a)
    → HandlerT master IO a

type SquealerHandler a
  = ∀ master. YesodSquealer master
  ⇒ HandlerT Squealer (HandlerT master IO) a
