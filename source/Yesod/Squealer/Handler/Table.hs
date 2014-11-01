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
{-# LANGUAGE ViewPatterns              #-}

module Yesod.Squealer.Handler.Table
  ( getTableR
  , sirenTable
  )
where

import Control.Applicative      (pure)
import Control.Category.Unicode ((∘))
import Data.Aeson.Types         ((.=))
import Data.Eq.Unicode          ((≡))
import Data.Foldable            (find)
import Data.Function            (($), const, id)
import Data.Functor             ((<$>))
import Data.Maybe               (maybe)
import Data.Monoid              (mempty)
import Data.Monoid.Unicode      ((⊕))
import Data.Siren               ((⤠), Entity(entityActions, entityLinks, entityProperties, entitySubEntities, entityTitle), RenderLink, embedEntity, embedLink, embeddingActions, mkEntity)
import Data.Text                (Text)
import Database.Squealer.Types  (Database(Database, dbname, tables), Table(Table, columns, key, tablename), colname, unIdentifier)
import Yesod.Core               (Route)
import Yesod.Core.Content       (TypedContent)
import Yesod.Core.Handler       (notFound, provideRep, selectRep)

import Yesod.Squealer.Handler        (handleParameters)
import Yesod.Squealer.Handler.Column (sirenColumn)

import Yesod.Squealer.Routes



getTableR ∷ Text → SquealerHandler TypedContent
getTableR tablename'
  = handleParameters adjustParameters respond
  where
    adjustParameters
      = id

    respond squealer _parameters
      = maybe notFound found
      $ find my tables
      where
        Squealer {..} = squealer
        Database {..} = database
        my = (tablename' ≡) ∘ unIdentifier ∘ tablename

        found table
          = selectRep
          $ do
            provideRep ∘ pure $ sirenTable table



sirenTable
  ∷ RenderLink (Route Squealer)
  ⇒ Table
  → Entity

sirenTable
  Table {..}
  = (mkEntity $ ?render self) -- TODO: use lenses
    { entityTitle
    , entityProperties
    , entitySubEntities
    , entityLinks
    , entityActions
    }
  where
    self = (TableR tablename', mempty)

    tablename' = unIdentifier tablename

    entityTitle
      = pure tablename'

    entityProperties
      = [ "table name" .= tablename
--      , "comment"    .= comment -- TODO
        ]

    entitySubEntities
      = maybe linked (const embedded) ?embedding
      <$> key ⊕ columns
      where
        linked (unIdentifier ∘ colname → colname') -- TODO: refactor Squealer
          = embedLink []
          $ ["item"] ⤠ ColumnR tablename' colname'

        embedded column
          = embedEntity ["item"]
          $ sirenColumn tablename' column

    entityLinks
      = [ ["describes" ] ⤠ RowsR tablename'
        , ["collection"] ⤠ DatabaseR
        ]

    entityActions
      = embeddingActions self
