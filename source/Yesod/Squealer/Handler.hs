{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ImplicitParams            #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonoLocalBinds          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE UnicodeSyntax             #-}
{-# LANGUAGE ViewPatterns              #-}

module Yesod.Squealer.Handler
  ( escape, escapeFieldName
  , handleParameters
  , runSQL, runSQLDebug
  )
where

import Control.Applicative              (pure)
import Control.Arrow                    (second)
import Control.Category.Unicode         ((∘))
import Control.Lens.Lens                ((&))
import Control.Monad                    (when)
import Control.Monad.IO.Class           (liftIO)
import Control.Monad.Trans              (lift)
import Control.Monad.Unicode            ((=≪))
import Data.Eq.Unicode                  ((≠))
import Data.Function                    (($), const, id, flip)
import Data.Functor                     ((<$>))
import Data.Maybe                       (Maybe(Just), maybe)
import Data.Map                         (Map, delete, fromList, lookup, toList)
import Data.Siren                       (RenderLink, parseEmbedding)
import Data.String                      (String, fromString)
import Data.Text                        (Text, unpack)
import Data.Tuple                       (uncurry)
import Database.HsSqlPpp.Ast            (Statement)
import Database.HsSqlPpp.Pretty         (printStatements)
import Database.PostgreSQL.Simple       (Connection)
import Database.PostgreSQL.Simple.Types (Query)
import Database.Squealer.Types          (Identifier(Identifier), escapeIdentifier)
import Network.URI                      (escapeURIString, isUnreserved)
import System.IO                        (IO, putStr)
import Yesod.Core                       (Route, getRouteToParent)
import Yesod.Core.Handler               (getCurrentRoute, getRequest, getUrlRenderParams, getYesod, redirect)
import Yesod.Core.Types                 (reqGetParams)

import Yesod.Squealer.Routes



handleParameters
  ∷ (Map Text Text → Map Text Text)
  → (RenderLink (Route Squealer) ⇒ Squealer → Map Text Text → SquealerHandler a)
  → SquealerHandler a

handleParameters
  adjustParameters
  responseHandler
  = do
    oldParameters
      ← fromList
      ∘ reqGetParams
      <$> getRequest

    let
      embedding
        = parseEmbedding
        =≪ lookup "embedding" oldParameters

      newParameters
        = oldParameters
        & maybe (delete "embedding") (const id) embedding
        & adjustParameters

    when (oldParameters ≠ newParameters)
      $ do
        Just currentRoute ← getCurrentRoute
        redirect (currentRoute, toList newParameters)

    renderParent ← lift getUrlRenderParams
    routeToParent ← getRouteToParent
    squealer ← getYesod

    let
      ?embedding = embedding

      ?render
        = (∘ second toList)
        ∘ uncurry
        $ renderParent
        ∘ routeToParent

    responseHandler
      squealer
      newParameters



runSQL
  ∷ (Connection → Query → IO a)
  → Statement
  → SquealerHandler a

runSQL q
  = lift ∘ withConnection
  ∘ (liftIO ∘) ∘ flip q
  ∘ fromString ∘ printStatements ∘ pure


runSQLDebug
  ∷ (Connection → Query → IO a)
  → Statement
  → SquealerHandler a

runSQLDebug q s
  = do
    liftIO ∘ putStr ∘ printStatements $ pure s
    runSQL q s



escape ∷ Text → String
escape
  = unpack
  ∘ escapeIdentifier
  ∘ Identifier



escapeFieldName ∷ String → String
escapeFieldName = escapeURIString isUnreserved
