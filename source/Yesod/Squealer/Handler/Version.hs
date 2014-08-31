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

module Yesod.Squealer.Handler.Version
  ( getVersionR
  , putVersionR
  , deleteVersionR
  , getPredecessorR
  , getSuccessorR

  , sirenVersion
  , VersionData
    ( VersionData
    , attributes
    , references
    , revocation
    , table
    , timestamp
    , version
    )
  )
where

import Control.Applicative          (pure)
import Control.Arrow.Unicode        ((⁂))
import Control.Category.Unicode     ((∘))
import Control.Lens.Fold            (hasn't)
import Control.Lens.Lens            ((&), (<&>))
import Control.Lens.Setter          ((%~))
import Control.Monad                (mzero, void)
import Control.Monad.Unicode        ((=≪))
import Data.Aeson.Types             ((.=), object)
import Data.Eq.Unicode              ((≡))
import Data.Foldable                (find, foldl')
import Data.Function                (($))
import Data.Functor                 ((<$), (<$>), fmap)
import Data.List                    (length, null, partition, splitAt, zip)
import Data.Map                     (delete, empty, fromList, lookup)
import Data.Maybe                   (Maybe(Just), catMaybes, fromMaybe, maybe)
import Data.Monoid.Unicode          ((⊕))
import Data.Siren                   ((⤠), RenderLink, Entity(entityActions, entityLinks, entityProperties, entitySubEntities), Field(Field, fieldName, fieldTitle, fieldType, fieldValue), FieldType(FieldType), actionFields, actionMethod, actionTitle, embedLink, mkAction, mkEntity)
import Data.Text                    (Text, unpack)
import Data.Text.Lens               (unpacked)
import Data.Traversable             (sequence)
import Data.Tuple                   (fst)
import Database.HsSqlPpp.Annotation (emptyAnnotation)
import Database.HsSqlPpp.Ast        (Distinct(Dupes), Name(Name), NameComponent(Nmc), QueryExpr(Select), SelectItem(SelExp), SelectList(SelectList), SetClause(SetClause), Statement(Delete, QueryStatement, Update), TableAlias(NoAlias), TableRef(Tref))
import Database.HsSqlPpp.Quote      (sqlExpr)
import Database.PostgreSQL.Simple   (execute_, query_)
import Database.Squealer.Types      (Column(Reference, colname, target), Database(Database, dbname, tables), Table(Table, columns, key, tablename), _Reference, colname, escapeIdentifier, unIdentifier)
import Network.HTTP.Types.Status    (seeOther303)
import Yesod.Core.Content           (TypedContent)
import Yesod.Core.Handler           (getYesod, notFound, provideRep, redirectWith, runRequestBody, selectRep)
import Yesod.Routes.Class           (Route)

import Yesod.Squealer.Handler (escape, escapeFieldName, handleParameters, runSQL, runSQLDebug)

import Yesod.Squealer.Routes

import Prelude (error) -- FIXME: remove this once everything is implemented



data VersionData
  = VersionData
    { table      ∷ Table
    , version    ∷ Text
    , timestamp  ∷ Text
    , revocation ∷ Maybe Text
    , attributes ∷ [(Column, Maybe Text)]
    , references ∷ [(Column, Maybe Text)]
    }

sirenVersion
  ∷ RenderLink (Route Squealer)
  ⇒ VersionData → Entity

sirenVersion
  VersionData {..}
  = (mkEntity $ ?render self) -- TODO: use lenses
    { entityProperties
    , entitySubEntities
    , entityLinks
    , entityActions
    }
  where
    Table {..} = table
    tablename' = unIdentifier tablename

    self = (VersionR tablename' version, empty)

    entityProperties
      = [ "attributes" .= object (attributes <&> \ (l, v) → unIdentifier (colname l) .= v)
        , "metadata"   .= metadata
        ]
      where
        metadata
          = object
            [ "timestamp"  .= timestamp
            , "revocation" .= revocation
            ]

    entitySubEntities
      = referenceLink <$> (catMaybes $ sequence <$> references)
      where
        referenceLink (column, targetVersion)
          = embedLink ["reference"]
          $ [?render (ColumnR tablename' $ unIdentifier colname, empty)]
          ⤠ VersionR (unIdentifier target) targetVersion
          where
            Reference {..} = column

    entityLinks
      = [ ["collection"         ] ⤠ RowsR        tablename'
        , ["profile"            ] ⤠ TableR       tablename'
        , ["predecessor-version"] ⤠ PredecessorR tablename' version
        , ["successor-version"  ] ⤠ SuccessorR   tablename' version
        ]

    entityActions
      = fromMaybe actions
      $ [] <$ revocation
      where
        actions
          = [ deleteAction
            , updateAction
            ]
          where
            updateAction
              = (mkAction "update" href)
                { actionTitle = pure "Update"
                , actionMethod = "PUT"
                , actionFields
                }
              where
                href = ?render self

                actionFields
                  = toField <$> attributes ⊕ references
                  where
                    toField (column, value)
                      = Field {..}
                      where
                        colname'   = unIdentifier $ colname column
                        fieldName  = colname' & unpacked %~ escapeFieldName
                        fieldType  = FieldType $ ?render (ColumnR tablename' colname', empty) -- TODO: maybe this should use predefined types in some cases?
                        fieldValue = value
                        fieldTitle = pure colname'

            deleteAction
              = (mkAction "delete" href)
                { actionTitle = pure "Delete"
                , actionMethod = "DELETE"
                }
              where
                href = ?render self



getVersionR ∷ Text → Text → SquealerHandler TypedContent
getVersionR tablename' version
  = handleParameters adjustParameters respond
  where
    adjustParameters
      = delete "embedding"

    respond squealer _parameters
      = maybe notFound found
      $ find my tables
      where
        Squealer {..} = squealer
        Database {..} = database
        my = (tablename' ≡) ∘ unIdentifier ∘ tablename

        found
          table @ Table {..}
          = do
            [Just timestamp : revocation : versionData] ← runSQL query_ versionSQL
            let
              (attributes, references)
                = zip attributeColumns
                ⁂ zip referenceColumns
                $ splitAt (length attributeColumns) versionData

            toView VersionData {..}
          where
            (attributeColumns, referenceColumns)
              = partition (hasn't _Reference)
              $ key ⊕ columns

            versionSQL
              = QueryStatement emptyAnnotation
              $ Select emptyAnnotation
                distinct projection relation condition
                grouping groupCondition order limit offset
              where
                distinct = Dupes

                projection
                  = SelectList emptyAnnotation
                  ∘ fmap qname
                  $ [ "journal timestamp"
                    , "end timestamp"
                    ]
                  ⊕ (attributeColumns <&> unIdentifier                  ∘ colname)
                  ⊕ (referenceColumns <&> unIdentifier ∘ (⊕ " version") ∘ colname)
                  where
                    qname column
                      = SelExp emptyAnnotation
                        [sqlExpr|$i(column_sql) :: text|]
                      where
                        column_sql
                          = table_sql
                          ⊕ "." ⊕ escape "version"
                          ⊕ "." ⊕ escape column

                relation
                  = pure
                  $ Tref emptyAnnotation
                    versionTable
                    alias
                  where
                    versionTable
                      = Name emptyAnnotation
                      $ Nmc <$> [table_sql, escape "version"]

                    alias
                      = NoAlias emptyAnnotation

                condition
                  = pure
                    [sqlExpr|$i(entry_column_sql) = $s(entry_sql)|]
                  where
                    entry_column_sql
                      = version_view_sql
                      ⊕ "."
                      ⊕ escape "entry"

                    entry_sql
                      = unpack version

                grouping       = mzero
                groupCondition = mzero
                order          = mzero
                limit          = mzero
                offset         = mzero

                table_sql = escape tablename'
                version_view_sql = table_sql ⊕ "." ⊕ escape "version"

            toView versionData
              = selectRep
              $ do
                provideRep ∘ pure $ sirenVersion versionData



putVersionR ∷ Text → Text → SquealerHandler ()
putVersionR tablename' version
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
          = do
            void $ runSQLDebug execute_ sql
            redirectWith seeOther303 $ VersionR tablename' version
            -- TODO: catch exceptions and respond accordingly.
            -- if a column was specified but the table doesn’t have it,
            -- respond invalidArgs.
            -- FIXME: perhaps this should redirect at the new version
            -- instead of the now revoked current version.
          where
            sql
              = Update emptyAnnotation
                targetTable
                setClauses
                extraTables
                condition
                returning
              where
                targetTable
                  = Name emptyAnnotation
                  ∘ pure
                  $ Nmc table_sql

                setClauses
                  = toSetClause
                  <$> key ⊕ columns
                  where
                    toSetClause column
                      = SetClause emptyAnnotation name
                      ∘ maybe [sqlExpr|NULL|] toExpression
                      $ lookup fieldName bodyParameters
                      where
                        name
                          = Nmc
                          ∘ unpack
                          ∘ escapeIdentifier
                          $ columnName

                        toExpression value
                          = [sqlExpr|$s(value_sql)|]
                          where
                            value_sql
                              = unpack value

                        fieldName
                          = (unpacked %~ escapeFieldName)
                          ∘ unIdentifier
                          $ columnName

                        columnName
                          = colname column

                extraTables
                  = pure
                  $ Tref emptyAnnotation
                    name
                    alias
                  where
                    name
                      = Name emptyAnnotation
                        [Nmc version_view_sql]

                    alias
                      = NoAlias emptyAnnotation

                condition
                  = pure
                  ∘ foldl' (∧) versionEntry
                  $ if null key
                    then pure "identity"
                    else keyLabels
                  where
                    keyLabels
                      = unIdentifier ∘ colname
                      <$> key

                    versionEntry
                      = [sqlExpr|$i(entry_column_sql) = $s(entry_sql)|]
                      where
                        entry_column_sql
                          = version_view_sql
                          ⊕ "."
                          ⊕ escape "entry"

                        entry_sql
                          = unpack version

                    acc ∧ column
                      = [sqlExpr|$(acc) and $i(table_column_sql) = $i(version_column_sql)|]
                      where
                        table_column_sql   = table_sql        ⊕ "." ⊕ column_sql
                        version_column_sql = version_view_sql ⊕ "." ⊕ column_sql

                        column_sql
                          = escape column

                returning = mzero

                table_sql = escape tablename'
                version_view_sql = table_sql ⊕ "." ⊕ escape "version"



deleteVersionR ∷ Text → Text → SquealerHandler ()
deleteVersionR tablename' version
  = respond =≪ getYesod
  where
    respond squealer
      = maybe notFound found
      $ find my tables
      where
        Squealer {..} = squealer
        Database {..} = database
        my = (tablename' ≡) ∘ unIdentifier ∘ tablename

        found
          Table {..}
          = do
            void $ runSQLDebug execute_ sql -- FIXME: handle errors.  Breaking a foreign key constraint with ON DELETE RESTRICT should not cause a 500 Internal Server Error.  also, think about 404s.
            redirectWith seeOther303 $ VersionR tablename' version
          where
            sql
              = Delete emptyAnnotation
                targetTable relation condition
                returning
              where
                targetTable
                  = Name emptyAnnotation
                  ∘ pure
                  $ Nmc table_sql

                relation
                  = pure
                  ∘ Tref emptyAnnotation versionTable
                  $ NoAlias emptyAnnotation
                  where
                    versionTable
                      = Name emptyAnnotation
                      $ Nmc <$> [table_sql, escape "version"]

                condition
                  = pure
                  ∘ foldl' (∧) versionEntry
                  $ if null key
                    then pure "identity"
                    else keyLabels
                  where
                    keyLabels
                      = unIdentifier ∘ colname
                      <$> key

                    versionEntry
                      = [sqlExpr|$i(entry_column_sql) = $s(entry_sql)|]
                      where
                        entry_column_sql
                          = version_view_sql
                          ⊕ "."
                          ⊕ escape "entry"

                        entry_sql
                          = unpack version

                    acc ∧ column
                      = [sqlExpr|$(acc) and $i(table_column_sql) = $i(version_column_sql)|]
                      where
                        table_column_sql   = table_sql        ⊕ "." ⊕ column_sql
                        version_column_sql = version_view_sql ⊕ "." ⊕ column_sql

                        column_sql
                          = escape column

                returning = mzero

                table_sql = escape tablename'
                version_view_sql = table_sql ⊕ "." ⊕ escape "version"



getPredecessorR ∷ Text → Text → SquealerHandler TypedContent
getPredecessorR = error "unimplemented" -- TODO



getSuccessorR ∷ Text → Text → SquealerHandler TypedContent
getSuccessorR _ _ = error "unimplemented" -- TODO
