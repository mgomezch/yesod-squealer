{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ImplicitParams            #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonoLocalBinds          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE UnicodeSyntax             #-}

module Yesod.Squealer.Handler.Database
  ( getDatabaseR
  , sirenDatabase
  )
where

import Control.Applicative      (pure)
import Control.Category.Unicode ((∘))
import Data.Aeson.Types         ((.=))
import Data.Function            (($), id)
import Data.Functor             ((<$>))
import Data.Monoid              (mempty)
import Data.Siren               ((⤠), Entity(entityActions, entityProperties, entitySubEntities, entityTitle), RenderLink, embedEntity, embedLink, embeddingActions, maybeEmbedded, mkEntity)
import Database.Squealer.Types  (Database(Database, dbname, tables), unIdentifier, Table(Table, columns, key, tablename))
import Prelude                  ((+), (-), Integer)
import Yesod.Core.Content       (TypedContent)
import Yesod.Core.Handler       (provideRep, selectRep)
import Yesod.Routes.Class       (Route)

import Yesod.Squealer.Handler       (handleParameters)
import Yesod.Squealer.Handler.Table (sirenTable)

import Yesod.Squealer.Routes



getDatabaseR ∷ SquealerHandler TypedContent
getDatabaseR
  = handleParameters adjustParameters respond
  where
    adjustParameters
      = id -- FIXME: this should remove all parameters, no?

    respond squealer _parameters
      = selectRep
      $ do
        provideRep ∘ pure $ sirenDatabase database
      where
        Squealer {..} = squealer



sirenDatabase
  ∷ RenderLink (Route Squealer)
  ⇒ Database
  → Entity

sirenDatabase
  Database {..}
  = (mkEntity $ ?render self) -- TODO: use lenses
    { entityTitle
    , entityProperties
    , entitySubEntities
    , entityActions
    }
  where
    self = (DatabaseR, mempty)

    entityTitle
      = pure
      $ unIdentifier dbname

    entityProperties
      = [ "database" .= dbname
--      , "comment"  .= comment -- TODO
        ]

    entitySubEntities
      = maybeEmbedded linked embedded
      <$> tables
      where
        linked Table {..}
          = embedLink []
          $ ["item"] ⤠ TableR (unIdentifier tablename)

        embedded table
          = embedEntity ["item"]
          $ sirenTable table

    entityActions
      = embeddingActions self
