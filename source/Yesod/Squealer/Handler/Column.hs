{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ImplicitParams            #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonoLocalBinds          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE UnicodeSyntax             #-}

module Yesod.Squealer.Handler.Column
  ( getColumnR
  , sirenColumn
  )
where

import Control.Applicative      (pure)
import Control.Category.Unicode ((∘))
import Data.Aeson.Types         ((.=))
import Data.Eq.Unicode          ((≡))
import Data.Foldable            (find)
import Data.Function            (($))
import Data.Maybe               (maybe)
import Data.Map                 (delete)
import Data.Monoid              (mempty)
import Data.Monoid.Unicode      ((⊕))
import Data.Siren               ((⤠), Entity(entityLinks, entityProperties, entityTitle), RenderLink, mkEntity)
import Data.Text                (Text)
import Database.Squealer.Types  (Column(Attribute, Reference, colname, coltype, target), Database(Database, dbname, tables), Table(Table, columns, key, tablename), colname, unIdentifier)
import Prelude                  ((+), (-), Integer)
import Yesod.Core.Content       (TypedContent)
import Yesod.Core.Handler       (notFound, provideRep, selectRep)
import Yesod.Routes.Class       (Route)

import Yesod.Squealer.Handler (handleParameters)

import Yesod.Squealer.Routes



getColumnR ∷ Text → Text → SquealerHandler TypedContent
getColumnR tablename' colname'
  = handleParameters adjustParameters respond
  where
    adjustParameters
      = delete "embedding"

    respond squealer _parameters
      = maybe notFound found
      $ do
        Table {..} ← find my tables
        find my' $ key ⊕ columns
      where
        Squealer {..} = squealer
        Database {..} = database
        my  = (tablename' ≡) ∘ unIdentifier ∘ tablename
        my' = (colname'   ≡) ∘ unIdentifier ∘ colname

        found column
          = selectRep
          $ do
            provideRep ∘ pure $ sirenColumn tablename' column



sirenColumn
  ∷ RenderLink (Route Squealer)
  ⇒ Text → Column → Entity

sirenColumn
  tablename'
  column
  = (mkEntity $ ?render self) -- TODO: use lenses
    { entityTitle
    , entityProperties
    , entityLinks
    }
  where
    self = (ColumnR tablename' colname', mempty)

    colname' = unIdentifier $ colname column

    entityTitle
      = pure colname'

    entityProperties
      = [ "column name" .= colname'
--      , "comment"     .= comment -- TODO
        , "type"
          .= case column of
            Attribute {..} → coltype -- TODO: better type description, especially for ENUMs
            Reference {..} → "foreign key"
        ]

    entityLinks
      = [ ["collection"] ⤠ TableR tablename'
        ]
      ⊕ case column of
        Attribute {..} → mempty
        Reference {..} → [["type"] ⤠ TableR (unIdentifier target)]
