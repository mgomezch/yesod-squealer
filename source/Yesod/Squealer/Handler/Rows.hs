{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ImplicitParams            #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonoLocalBinds          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE UnicodeSyntax             #-}

module Yesod.Squealer.Handler.Rows
  ( getRowsR
  , postRowsR
  )
where

import Control.Applicative          (pure)
import Control.Category.Unicode     ((∘))
import Control.Lens.At              (at)
import Control.Lens.Combinators     ((&))
import Control.Lens.Setter          ((%~), (.~), mapped)
import Control.Lens.Prism           (Prism', isn't)
import Control.Lens.Tuple           (_2)
import Control.Monad                (guard)
import Control.Monad.Unicode        ((=≪), (≫=))
import Data.Aeson.Types             ((.=))
import Data.Eq.Unicode              ((≡))
import Data.Foldable                (find)
import Data.Function                (($))
import Data.Functor                 ((<$), (<$>), fmap)
import Data.Map                     (empty, lookup)
import Data.Maybe                   (Maybe(Just), maybe)
import Data.Monoid                  (mempty)
import Data.Monoid.Unicode          ((⊕))
import Data.Ord                     ((<), max, min)
import Data.Siren                   ((↦), (⤠), RenderLink, RouteParameters, actionFields, actionMethod, actionTitle, embedLink, embeddingActions, Entity(entityActions, entityLinks, entityProperties, entitySubEntities), fieldTitle, fieldValue, maybeEmbedded, mkAction, mkEntity, mkField)
import Data.String                  (String)
import Data.Text                    (Text, pack, unpack)
import Data.Text.Lens               (unpacked)
import Database.HsSqlPpp.Annotation (emptyAnnotation)
import Database.HsSqlPpp.Ast        (ScalarExpr(NumberLit))
import Database.HsSqlPpp.Quote      (pgsqlStmt)
import Database.PostgreSQL.Simple   (fromOnly, query_)
import Database.Squealer.Types      (Database(Database, dbname, tables), Table(Table, columns, key, tablename), unIdentifier)
import Numeric.Lens                 (decimal)
import Prelude                      ((+), (-), Integer)
import Text.Read                    (readMaybe)
import Text.Show                    (show)
import Yesod.Core.Content           (TypedContent)
import Yesod.Core.Handler           (notFound, provideRep, selectRep)
import Yesod.Routes.Class           (Route)

import Yesod.Squealer.Handler (escape, handleParameters, runSQL)

import Yesod.Squealer.Routes

import Prelude (error) -- FIXME: remove this once everything is implemented



minLimit, maxLimit, minOffset ∷ Integer
minLimit      = 1
maxLimit      = 50
minOffset     = 0

defaultLimit, defaultOffset ∷ Text
defaultLimit  = "10"
defaultOffset = "0"



getRowsR ∷ Text → SquealerHandler TypedContent
getRowsR tablename'
  = handleParameters adjustParameters respond
  where
    between l u = atLeast l ∘ atMost u
    atLeast = max
    atMost = min

    adjustParameters
      = adjust "limit"  defaultLimit  (between minLimit maxLimit)
      ∘ adjust "offset" defaultOffset (atLeast minOffset)
      where
        adjust parameter defaultValue setNumber
          = at parameter
          %~ maybe (pure defaultValue) pure
          ∘ (mapped ∘ unpacked %~ defaultIfInvalid ∘ modifyIfValid)
          where
            modifyIfValid = number %~ setNumber
            defaultIfInvalid = isn't number ≫= (? unpack defaultValue)
            (?) c t f = if c then t else f

            number ∷ Prism' String Integer
            number = decimal

    respond squealer parameters
      = maybe notFound found
      $ find my tables
      where
        Squealer {..} = squealer
        Database {..} = database
        my = (tablename' ≡) ∘ unIdentifier ∘ tablename

        found
          table @ Table {..}
          = do
            [count ∷ Integer] ← fmap fromOnly <$> runSQL query_ countSQL
            entries           ← fmap fromOnly <$> runSQL query_ entriesSQL
            toView
              count
              entries
          where
            limit, offset ∷ Integer
            Just limit  = readParameter "limit"
            Just offset = readParameter "offset"

            readParameter parameter
              = readMaybe ∘ unpack
              =≪ lookup parameter parameters

            table_sql = escape tablename'

            -- Note: This performs poorly in PostgreSQL because of MVCC.
            -- Consider removing the count from the response to improve
            -- scalability.  Currently, this implies a full seq scan on the
            -- active rows table.
            countSQL
              = [pgsqlStmt|
                  select count(*)
                  from   $i(table_sql)."active"
                  ;
                |]

            entriesSQL
              = [pgsqlStmt|
                  select     "entry" :: text
                  from       $i(table_sql)."active"
                  inner join $i(table_sql)."version" using ("entry")
                  order by   "journal timestamp" desc, "entry" asc
                  limit      $(limit_sql)
                  offset     $(offset_sql)
                  ;
                |]
              where
                limit_sql  = number limit
                offset_sql = number offset
                number = NumberLit emptyAnnotation ∘ show
                -- TODO: filtering

            toView
              count
              entries
              = selectRep
              $ do
                provideRep ∘ pure $ sirenRows TablePage {..} parameters



data TablePage
  = TablePage
    { table                ∷ Table
    , count, limit, offset ∷ Integer
    , entries              ∷ [Text]
    }

sirenRows
  ∷ RenderLink (Route Squealer)
  ⇒ TablePage → RouteParameters → Entity

sirenRows
  TablePage {..}
  parameters
  = (mkEntity $ ?render self) -- TODO: use lenses
    { entityProperties
    , entitySubEntities
    , entityLinks
    , entityActions
    }
  where
    Table {..} = table
    tablename' = unIdentifier tablename

    self = (RowsR tablename', parameters)

    entityProperties
      = [ "count"  .= count
        , "limit"  .= limit
        , "offset" .= offset
        ]

    entitySubEntities
      = maybeEmbedded linked embedded
      <$> entries
      where
        linked entry
          = embedLink []
          $ ["item"] ⤠ VersionR tablename' entry

        embedded entry
          = error "unimplemented: embedded version data" -- FIXME

    entityLinks
      = [ ["describedby"] ⤠ TableR tablename'
        ]
      ⊕ setOffset "previous" previousBound previousOffset
      ⊕ setOffset     "next"     nextBound     nextOffset
      where
        previousOffset = max minOffset $ offset - limit
        nextOffset     = offset + limit

        previousBound = minOffset      < offset
        nextBound     = offset + limit < count

        setOffset relation showIf newOffset
          = [relation] ↦ self'
          <$ guard showIf
          where
            self'
              = self
              & _2
              ∘ at "offset"
              ∘ mapped
              ∘ unpacked
              ∘ decimal
              .~ newOffset

    entityActions
      = embeddingActions self
      ⊕ [ paginate
        , insert
        , search
        ]
      where
        paginate
          = (mkAction "paginate" href)
            { actionTitle = pure "Paginate"
            , actionFields
              = [ field "limit"  "Limit"  limit
                , field "offset" "Offset" offset
                ]
            }
          where
            href
              = ?render
              $ self
              & _2
              %~ (at "limit"  .~ mempty)
              ∘  (at "offset" .~ mempty)

            field name title value
              = (mkField name "number")
                { fieldTitle = pure title
                , fieldValue = pure ∘ pack ∘ show $ value
                }

        insert
          = (mkAction "insert" selfNoParameters)
            { actionTitle = pure "Insert"
            , actionMethod = "POST"
            , actionFields = [] -- FIXME: table columns
            }

        search
          = (mkAction "search" selfNoParameters)
            { actionTitle = pure "Search"
            , actionFields = [] -- FIXME: table columns
            }

        selfNoParameters
          = ?render $ self & _2 .~ empty




postRowsR ∷ Text → SquealerHandler ()
postRowsR _ = error "unimplemented" -- TODO
