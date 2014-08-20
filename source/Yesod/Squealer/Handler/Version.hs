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
  )
where

import Control.Applicative          (pure)
import Control.Category.Unicode     ((∘))
import Control.Lens.Combinators     ((<&>))
import Control.Lens.Fold            (hasn't)
import Control.Lens.Setter          ((%~))
import Control.Lens.Traversal       (both)
import Control.Monad                (mzero, void)
import Data.Aeson.Types             ((.=), object)
import Data.Eq.Unicode              ((≡))
import Data.Foldable                (find, foldl')
import Data.Function                (($), const)
import Data.Functor                 ((<$>), fmap)
import Data.List                    (length, null, partition, splitAt, zip)
import Data.Map                     (delete, empty)
import Data.Maybe                   (Maybe(Just), catMaybes, maybe)
import Data.Monoid.Unicode          ((⊕))
import Data.Siren                   ((⤠), RenderLink, Entity(entityActions, entityLinks, entityProperties, entitySubEntities), actionFields, actionMethod, actionTitle, embedLink, mkAction, mkEntity)
import Data.Text                    (Text, unpack)
import Data.Traversable             (sequence)
import Data.Tuple                   (uncurry)
import Database.HsSqlPpp.Annotation (emptyAnnotation)
import Database.HsSqlPpp.Ast        (Distinct(Dupes), Name(Name), NameComponent(Nmc), QueryExpr(Select), SelectItem(SelExp), SelectList(SelectList), Statement(Delete, QueryStatement), TableAlias(NoAlias), TableRef(Tref))
import Database.HsSqlPpp.Quote      (sqlExpr)
import Database.PostgreSQL.Simple   (execute_, query_)
import Database.Squealer.Types      (Column(Reference, colname, target), Database(Database, dbname, tables), Table(Table, columns, key, tablename), _Reference, colname, unIdentifier)
import Network.HTTP.Types.Status    (seeOther303)
import Prelude                      ((+), (-), Integer)
import Yesod.Core.Content           (TypedContent)
import Yesod.Core.Handler           (notFound, provideRep, redirectWith, selectRep)
import Yesod.Routes.Class           (Route)

import Yesod.Squealer.Handler (escape, handleParameters, runSQL)

import Yesod.Squealer.Routes

import Prelude (error) -- FIXME: remove this once everything is implemented



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
            [Just timestamp : revocation : row] ← runSQL query_ versionSQL
            let
              (attributeValues, referenceValues)
                = splitAt (length attributeColumns) row

              (attributes, references)
                = both %~ catMaybes ∘ fmap sequence ∘ uncurry zip
                $ ( (attributeColumns, attributeValues)
                  , (referenceColumns, referenceValues)
                  )

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
                  ∘ Tref emptyAnnotation versionTable
                  $ NoAlias emptyAnnotation
                  where
                    versionTable
                      = Name emptyAnnotation
                      $ Nmc <$> [table_sql, escape "version"]

                condition
                  = pure
                    [sqlExpr|$i(entry_column_sql) = $s(version_sql)|]
                  where
                    entry_column_sql
                      = table_sql
                      ⊕ "." ⊕ escape "version"
                      ⊕ "." ⊕ escape "entry"

                grouping       = mzero
                groupCondition = mzero
                order          = mzero
                limit          = mzero
                offset         = mzero

                table_sql = escape tablename'
                version_sql = unpack version

            toView versionData
              = selectRep
              $ do
                provideRep ∘ pure $ sirenVersion versionData



data VersionData
  = VersionData
    { table      ∷ Table
    , version    ∷ Text
    , timestamp  ∷ Text
    , revocation ∷ Maybe Text
    , attributes ∷ [(Column, Text)]
    , references ∷ [(Column, Text)]
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
      = referenceLink <$> references
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
      = maybe [updateAction, deleteAction] (const []) revocation
      where
        updateAction
          = (mkAction "update" href)
            { actionTitle = pure "Update"
            , actionMethod = "PUT"
            , actionFields = [] -- FIXME: table columns
            }
          where
            href = ?render self

        deleteAction
          = (mkAction "delete" href)
            { actionTitle = pure "Delete"
            , actionMethod = "DELETE"
            }
          where
            href = ?render self



putVersionR ∷ Text → Text → SquealerHandler ()
putVersionR = error "unimplemented" -- TODO



deleteVersionR ∷ Text → Text → SquealerHandler ()
deleteVersionR tablename' version
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
          Table {..}
          = do
            void $ runSQL execute_ deleteSQL -- FIXME: handle errors.  Breaking a foreign key constraint with ON DELETE RESTRICT should not cause a 500 Internal Server Error.  also, think about 404s.
            redirectWith seeOther303 $ VersionR tablename' version
          where
            deleteSQL
              = Delete emptyAnnotation
                whence relation condition
                returning
              where
                whence
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
                  = pure ∘ foldl' (∧) versionEntry
                  $ if null key
                    then pure "identity"
                    else keyLabels
                  where
                    keyLabels
                      = unIdentifier ∘ colname
                      <$> key

                    versionEntry
                      = [sqlExpr|"entry" = $s(version_sql)|]

                    acc ∧ column
                      = [sqlExpr|$(acc) and $i(table_column_sql) = $i(version_column_sql)|]
                      where
                        table_column_sql
                          = table_sql
                          ⊕ "." ⊕ column_sql

                        version_column_sql
                          = table_sql
                          ⊕ "." ⊕ escape "version"
                          ⊕ "." ⊕ column_sql

                        column_sql
                          = escape column

                returning = mzero

                table_sql = escape tablename'
                version_sql = unpack version



getPredecessorR ∷ Text → Text → SquealerHandler TypedContent
getPredecessorR = error "unimplemented" -- TODO



getSuccessorR ∷ Text → Text → SquealerHandler TypedContent
getSuccessorR _ _ = error "unimplemented" -- TODO
