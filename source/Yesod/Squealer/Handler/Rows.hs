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
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE UnicodeSyntax             #-}

module Yesod.Squealer.Handler.Rows
  ( getRowsR
  , postRowsR
  )
where

import Control.Applicative          (pure)
import Control.Arrow.Unicode        ((⁂))
import Control.Category             (id)
import Control.Category.Unicode     ((∘))
import Control.Exception            (throwIO)
import Control.Lens.At              (at)
import Control.Lens.Lens            ((&), (<&>), Lens')
import Control.Lens.Fold            (hasn't)
import Control.Lens.Prism           (Prism', isn't)
import Control.Lens.Setter          ((%~), (.~), (<>~), mapped)
import Control.Lens.Traversal       (Traversal')
import Control.Lens.Tuple           (_2)
import Control.Monad                (guard, mzero, void)
import Control.Monad.IO.Class       (liftIO)
import Control.Monad.Logger         (logError)
import Control.Monad.Unicode        ((=≪), (≫=))
import Data.Aeson.Types             ((.=))
import Data.Either                  (Either(Left, Right), either)
import Data.Eq.Unicode              ((≡))
import Data.Foldable                (find)
import Data.Function                (($))
import Data.Functor                 ((<$), (<$>), fmap)
import Data.List                    (length, partition, splitAt, unzip, zip)
import Data.Map                     (empty, fromList, lookup)
import Data.Maybe                   (Maybe(Just), catMaybes, maybe)
import Data.Monoid.Unicode          ((⊕))
import Data.Ord                     ((<), max, min)
import Data.Siren                   ((↦), (⤠), Entity(entityActions, entityLinks, entityProperties, entitySubEntities), Field(Field, fieldName, fieldTitle, fieldType, fieldValue), FieldType(FieldType), RenderLink, RouteParameters, actionFields, actionMethod, actionTitle, embedEntity, embedLink, embeddingActions, fieldTitle, fieldValue, maybeEmbedded, mkAction, mkEntity, mkField)
import Data.String                  (String)
import Data.Text                    (Text, pack, unpack)
import Data.Text.Lens               (unpacked)
import Data.Traversable             (traverse)
import Data.Tuple                   (fst)
import Database.HsSqlPpp.Annotation (emptyAnnotation)
import Database.HsSqlPpp.Ast        (Name(Name), NameComponent(Nmc), QueryExpr(Select, Values), ScalarExpr(NumberLit, StringLit), SelectItem(SelExp), SelectItemList, SelectList(SelectList), Statement(Insert, QueryStatement))
import Database.HsSqlPpp.Quote      (pgsqlStmt, sqlExpr)
import Database.PostgreSQL.Simple   (execute_, fromOnly, query_)
import Database.Squealer.Types      (Database(Database, dbname, tables), Table(Table, columns, key, tablename), _Reference, colname, escapeIdentifier, unIdentifier)
import Numeric.Lens                 (decimal)
import Prelude                      ((+), (-), Integer)
import Text.Read                    (readMaybe)
import Text.Show                    (show)
import Yesod.Core.Content           (TypedContent)
import Yesod.Core.Handler           (getYesod, notFound, provideRep, runRequestBody, selectRep)
import Yesod.Core.Types             (ErrorResponse(InternalError), HandlerContents(HCError))
import Yesod.Routes.Class           (Route)

import Yesod.Squealer.Handler         (escape, escapeFieldName, handleParameters, runSQL)
import Yesod.Squealer.Handler.Version (VersionData(VersionData, attributes, references, revocation, table, timestamp, version), sirenVersion)

import Yesod.Squealer.Routes



data TablePage
  = TablePage
    { pagedTable           ∷ Table
    , count, limit, offset ∷ Integer
    , entries              ∷ [Either Text VersionData]
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
    Table {..} = pagedTable
    tablename' = unIdentifier tablename

    self = (RowsR tablename', parameters)

    entityProperties
      = [ "count"  .= count
        , "limit"  .= limit
        , "offset" .= offset
        ]

    entitySubEntities
      = either linked embedded
      <$> entries
      where
        linked entry
          = embedLink []
          $ ["item"] ⤠ VersionR tablename' entry

        embedded entry
          = embedEntity ["item"]
          $ sirenVersion entry

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
              %~ (at "limit"  .~ mzero)
              ∘  (at "offset" .~ mzero)

            field name title value
              = (mkField name "number")
                { fieldTitle = pure title
                , fieldValue = pure ∘ pack ∘ show $ value
                }

        tableFields
          = toField <$> key ⊕ columns
          where
            toField column
              = Field {..}
              where
                colname'   = unIdentifier $ colname column
                fieldName  = colname' & unpacked %~ escapeFieldName
                fieldType  = FieldType $ ?render (ColumnR tablename' colname', empty) -- TODO: maybe this should use predefined types in some cases?
                fieldValue = mzero
                fieldTitle = pure colname'

        insert
          = (mkAction "insert" selfNoParameters)
            { actionTitle = pure "Insert"
            , actionMethod = "POST"
            , actionFields = tableFields
            }

        search
          = (mkAction "search" selfNoParameters)
            { actionTitle = pure "Search"
            , actionFields = tableFields
            }

        selfNoParameters
          = ?render $ self & _2 .~ empty



_QueryExpr ∷ Traversal' Statement QueryExpr
_QueryExpr
  f
  ( QueryStatement
    annotation
    select
  )
  = mk <$> f select
  where
    mk select'
      = QueryStatement
        annotation
        select'

_QueryExpr _f s = pure s


_SelectList ∷ Traversal' QueryExpr SelectList
_SelectList
  f
  ( Select
    annotation
    distinct
    project
    from
    where_
    group
    having
    order
    limit
    offset
  )
  = mk <$> f project
  where
    mk project'
      = Select
        annotation
        distinct
        project'
        from
        where_
        group
        having
        order
        limit
        offset

_SelectList _f s = pure s


_SelectItemList ∷ Lens' SelectList SelectItemList
_SelectItemList
  f
  ( SelectList
    annotation
    items
  )
  = mk <$> f items
  where
    mk items'
      = SelectList
        annotation
        items'



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
          pagedTable @ Table {..}
          = do
            [count ∷ Integer]
              ← fmap fromOnly
              <$> runSQL query_ countSQL

            entries
              ← traverse toEntry
              =≪ queryEntries

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

            table_sql
              = escape tablename'

            allColumns
              = key
              ⊕ columns

            (attributeColumns, referenceColumns)
              = partition
                (hasn't _Reference)
                allColumns

            extraColumnNames
              = [ "journal timestamp"
                , "end timestamp"
                ]
              ⊕ (attributeColumns <&> colname)
              ⊕ (referenceColumns <&> (⊕ " version") ∘ colname)

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

            queryEntries
              = runSQL query_ versionsSQL
              where
                versionsSQL
                  = maybeEmbedded id addColumns
                    [pgsqlStmt|
                      select     "version"."entry" :: text
                      from       $i(table_sql)."active"
                      inner join $i(table_sql)."version" using ("entry")
                      order by   "version"."journal timestamp" desc
                      ,          "version"."entry"             asc
                      limit      $(limit_sql)
                      offset     $(offset_sql)
                      ;
                    |]
                  where
                    limit_sql  = number limit
                    offset_sql = number offset
                    number = NumberLit emptyAnnotation ∘ show
                    -- TODO: filtering

                    addColumns
                      = _QueryExpr
                      ∘ _SelectList
                      ∘ _SelectItemList
                      <>~ (nameToSQL <$> extraColumnNames)
                      where
                        nameToSQL name
                          = SelExp
                            emptyAnnotation
                            [sqlExpr|"version".$i(name_sql) :: text|]
                          where
                            name_sql
                              = escape
                              $ unIdentifier name

            toEntry row
              = maybeEmbedded
                (Left  <$> asVersion    )
                (Right <$> asVersionData)
              where
                asVersion ∷ SquealerHandler Text
                asVersion
                  = case row of
                    [Just version]
                      → pure version

                    _
                      → do
                        $logError ∘ pack $ "Database returned unexpected query results; expected a single column with the version identifier, but got " ⊕ show (length row) ⊕ " columns."
                        liftIO ∘ throwIO ∘ HCError $ InternalError "Bad query results"

                asVersionData ∷ SquealerHandler VersionData
                asVersionData
                  = case row of
                    (Just version : Just timestamp : revocation : versionData)
                      | countColumns ≡ length versionData
                      → do
                        let
                          (attributes, references)
                            = zip attributeColumns
                            ⁂ zip referenceColumns
                            $ splitAt (length attributeColumns) versionData

                        pure VersionData
                          { table = pagedTable
                          , ..
                          }

                    _
                      → do
                        $logError ∘ pack $ "Database returned unexpected query results; expected " ⊕ show (countColumns + 3) ⊕ " columns with the full version data, but got " ⊕ show (length row) ⊕ " columns."
                        liftIO ∘ throwIO ∘ HCError $ InternalError "Bad query results"
                  where
                    countColumns
                      = length allColumns

            toView
              count
              entries
              = selectRep
              $ do
                provideRep ∘ pure $ sirenRows TablePage {..} parameters



postRowsR ∷ Text → SquealerHandler ()
postRowsR tablename'
  = do
    squealer ← getYesod

    bodyParameters
      ← fromList ∘ fst
      <$> runRequestBody

    respond squealer bodyParameters
  where
    respond squealer bodyParameters
      = maybe notFound found
      $ find my tables
      where
        Squealer {..} = squealer
        Database {..} = database
        my = (tablename' ≡) ∘ unIdentifier ∘ tablename

        found
          Table {..}
          = void $ runSQL execute_ sql
            -- TODO: catch exceptions and respond accordingly.
            -- if a column was specified but the table doesn’t have it,
            -- respond invalidArgs.
          where
            sql
              = Insert emptyAnnotation
                targetTable insertColumns values
                returning
              where
                targetTable
                  = Name emptyAnnotation
                    [Nmc $ escape tablename']

                insertColumns
                  = Nmc ∘ unpack ∘ escapeIdentifier
                  <$> columnIdentifiers

                values
                  = Values emptyAnnotation
                    [StringLit emptyAnnotation ∘ unpack <$> values']

                returning
                  = mzero -- FIXME: Squealer should always include the entry in the main view, and this should use RETURNING with it to respond Created with the version URI to the newly inserted version!

                (columnIdentifiers, values' ∷ [Text])
                  = unzip ∘ catMaybes
                  $ getColumnValue <$> key ⊕ columns
                  where
                    getColumnValue column
                      = (columnName, )
                      <$> lookup fieldName bodyParameters
                      where
                        columnName
                          = colname column

                        fieldName
                          = (unpacked %~ escapeFieldName)
                          ∘ unIdentifier
                          $ columnName
