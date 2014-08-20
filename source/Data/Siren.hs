{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams             #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NoMonoLocalBinds           #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE UnicodeSyntax              #-}
{-# LANGUAGE ViewPatterns               #-}

module Data.Siren
  ( Embedding(Depth, Full), contractEmbedding, expandEmbedding, renderEmbedding, parseEmbedding
  , RouteParameters, Reference, RenderLink, setEmbedding
  , getEmbedded

  , sirenContentType
  , mkEntity, Entity(Entity, entityURI, entityClasses, entityProperties, entitySubEntities, entityLinks, entityActions, entityTitle)
  , mkAction, Action(Action, actionName, actionClasses, actionMethod, actionURI, actionTitle, actionType, actionFields)
  , mkField , Field (Field , fieldName, fieldType, fieldValue, fieldTitle)
  , embeddingActions

  , EmbeddingType(EmbeddedLink, EmbeddedRepresentation)
  , SubEntity(SubEntity, subEntityRelations, subEntityContents)
  , SubEntityContents
      ( Linked, embeddedLinkURI, embeddedLinkClasses
      , Nested, embeddedRepresentation
      )

  , Link(Link, linkRelations, linkURI), (↦), (⤠), link, linkPlain
  , embedLink, embedEntity, maybeEmbedded

  , Method    (Method    , unMethod    )
  , ActionType(ActionType, unActionType)
  , FieldType (FieldType , unFieldType )
  )
where

import Control.Applicative      ((<*), (<|>), empty, pure)
import Control.Category.Unicode ((∘))
import Control.Lens.At          (at)
import Control.Lens.Combinators ((&))
import Control.Lens.Fold        (preview)
import Control.Lens.Setter      ((.~), (?~))
import Control.Lens.Tuple       (_2)
import Control.Lens.Prism       (Prism', prism')
import Control.Monad            (guard)
import Control.Monad.Unicode    ((=≪))
import Data.Aeson.Types         ((.=), FromJSON, Pair, ToJSON(toJSON), Value(Object), object)
import Data.Attoparsec.Text     (decimal, endOfInput, parseOnly, string)
import Data.Bool                (Bool(False))
import Data.Bool.Unicode        ((∧))
import Data.Default             (Default(def))
import Data.Eq                  (Eq((==)))
import Data.Eq.Unicode          ((≠), (≡))
import Data.Function            (($))
import Data.Functor             ((<$), (<$>))
import Data.HashMap.Lazy        (HashMap)
import Data.Map                 (Map)
import Data.Maybe               (Maybe, maybe)
import Data.Monoid              (mempty)
import Data.Monoid.Unicode      ((⊕))
import Data.Ord                 ((>), Ord)
import Data.String              (IsString)
import Data.Text                (Text, pack)
import Data.Traversable         (traverse)
import Prelude                  (Integer, pred, succ)
import Text.Read                (Read)
import Text.Show                (Show, show)
import Yesod.Core.Content       (HasContentType(getContentType), ToContent(toContent), ToTypedContent(toTypedContent))
import Yesod.Core.Types         (ContentType)



data Embedding
  = Depth Integer
  | Full
  deriving (Eq, Ord, Read, Show)

renderEmbedding ∷ Embedding → Text
renderEmbedding
  = \ case
    Depth n → pack $ show n
    Full → "full"

parseEmbedding ∷ Text → Maybe Embedding -- FIXME: distinguish a parse error from a valid parse of the default value.
parseEmbedding
  = (preview traverse ∘)
  ∘ parseOnly
  $ embedding <* endOfInput
  where
    embedding
      = depth <|> full
      where
        depth
          = do
            d ← decimal
            guard $ d > 0
            pure $ Depth d

        full
          = Full
          <$ string "full"

expandEmbedding ∷ Embedding → Embedding
expandEmbedding
  = \ case
    Full → Full
    Depth n → Depth (succ n)

contractEmbedding ∷ Embedding → Maybe Embedding -- FIXME: use Depth 0 as the minimum value, not Nothing.
contractEmbedding
  = \ case
    Full → pure Full
    Depth n
      → Depth (pred n)
      <$ guard (n > 1)



type RouteParameters
  = Map Text Text

type Reference target
  = (target, RouteParameters)

type RenderLink target
  = ( ?render    ∷ Reference target → Text
    , ?embedding ∷ Maybe Embedding -- FIXME: use Depth 0 as the Nothing value, remove Maybe
    -- TODO: the route parameters should really be implicit here as such: ?parameters ∷ RouteParameters
    )

setEmbedding
  ∷ Maybe Embedding
  → (target, RouteParameters)
  → (target, RouteParameters)
setEmbedding embedding
  = _2 ∘ at "embedding"
  .~ (renderEmbedding <$> embedding)



getEmbedded
  ∷ (RenderLink target ⇒ Entity)
  → (RenderLink target ⇒ Entity)

getEmbedded a
  = let ?embedding = newEmbedding in a
  where
    newEmbedding
      = contractEmbedding
      =≪ ?embedding



---



data Entity
  = Entity
    { entityURI         ∷ Text
    , entityClasses     ∷ [Text]
    , entityProperties  ∷ [Pair]
    , entitySubEntities ∷ [SubEntity]
    , entityLinks       ∷ [Link]
    , entityActions     ∷ [Action]
    , entityTitle       ∷ Maybe Text
    }
  deriving (Eq, Show)

mkEntity ∷ Text → Entity
mkEntity entityURI
  = Entity {..}
  where
    entityClasses     = def
    entityProperties  = def
    entitySubEntities = def
    entityLinks       = def
    entityActions     = def
    entityTitle       = def

instance ToJSON Entity where
  toJSON Entity {..}
    = object
      [ "class"      .= entityClasses
      , "properties" .= object entityProperties
      , "entities"   .= entitySubEntities
      , "links"      .= (selfLink : entityLinks)
      , "actions"    .= entityActions
      , "title"      .= entityTitle
      ]
    where
      selfLink
        = Link
          { linkRelations = ["self"]
          , linkURI       = entityURI
          }

instance ToContent Entity where
  toContent
    = toContent
    ∘ toJSON

instance ToTypedContent Entity where
  toTypedContent
    = toTypedContent
    ∘ (,) sirenContentType
    ∘ toContent

instance HasContentType Entity where
  getContentType _ = sirenContentType

sirenContentType ∷ ContentType
sirenContentType = "application/vnd.siren+json; charset=utf-8"



data SubEntity
  = ∀ embedding
  . SubEntity
    { subEntityRelations ∷ [Text] -- TODO: This should use NonEmpty, not [].
    , subEntityContents  ∷ SubEntityContents embedding
    }

instance Eq SubEntity where
  l == r
    = subEntityRelations l ≡ subEntityRelations r
    ∧ case (l, r) of
      { ( SubEntity { subEntityContents = l' @ Linked {} }
        , SubEntity { subEntityContents = r' @ Linked {} }
        )
        → l' ≡ r'
        ;

        ( SubEntity { subEntityContents = l' @ Nested {} }
        , SubEntity { subEntityContents = r' @ Nested {} }
        )
        → l' ≡ r'
        ;

        _ → False;
      }

deriving instance Show SubEntity

-- This should be imported from Control.Lens.Aeson — however, the
-- lens-aeson package is currently incompatible with the latest aeson and
-- fixing that seems nontrivial due to how number representations changed.
-- Depending on lens-aeson brings numerous dependency issues at the moment,
-- and as nice as it is, it’s a nicer solution at the moment to eliminate
-- the dependency given it’s only for this one very simple prism.
_Object ∷ Prism' Value (HashMap Text Value)
_Object
  = prism' Object
  $ \ case
    Object o → pure o
    _        → empty

instance ToJSON SubEntity where
  toJSON SubEntity {..}
    = toJSON subEntityContents
    & _Object ∘ at "rel"
    ?~ toJSON subEntityRelations



data EmbeddingType
  = EmbeddedLink
  | EmbeddedRepresentation
  deriving (Eq, Read, Show)

data SubEntityContents (embedding ∷ EmbeddingType) where
  Linked
    ∷ { embeddedLinkURI     ∷ Text
      , embeddedLinkClasses ∷ [Text]
      }
    → SubEntityContents EmbeddedLink

  Nested
    ∷ { embeddedRepresentation ∷ Entity
      }
    → SubEntityContents EmbeddedRepresentation

deriving instance Eq   (SubEntityContents embedding)
deriving instance Show (SubEntityContents embedding)

instance ToJSON (SubEntityContents embedding) where
  toJSON
    = \ case
      Linked {..}
        → object
          [ "href"  .= embeddedLinkURI
          , "class" .= embeddedLinkClasses
          ]

      Nested {..}
        → toJSON embeddedRepresentation

embedLink ∷ [Text] → Link → SubEntity
embedLink
  embeddedLinkClasses
  Link {..}
  = SubEntity
    { subEntityRelations = linkRelations
    , subEntityContents
      = Linked
        { embeddedLinkURI = linkURI
        , ..
        }
    }

embedEntity
  ∷ [Text]
  → (RenderLink target ⇒ Entity   )
  → (RenderLink target ⇒ SubEntity)
embedEntity
  subEntityRelations
  entity
  = SubEntity
    { subEntityContents
      = Nested
        { embeddedRepresentation
          =
            let
              ?embedding = contractEmbedding =≪ ?embedding
              in
                entity
        }
    , ..
    }

maybeEmbedded ∷ (?embedding ∷ Maybe Embedding) ⇒ a → a → a
maybeEmbedded noEmbedding withEmbedding
  = maybe noEmbedding checkEmbedding ?embedding
  where
    checkEmbedding
      = \ case
        Full → withEmbedding
        Depth n
          → if n > 0
            then withEmbedding
            else noEmbedding



data Link
  = Link
    { linkRelations ∷ [Text] -- TODO: This should use NonEmpty, not [].
    , linkURI       ∷ Text
    }
  deriving (Eq, Read, Show)

instance ToJSON Link where
  toJSON Link {..}
    = object
      [ "rel"  .= linkRelations
      , "href" .= linkURI
      ]

infix 6 ↦
(↦) ∷ RenderLink target ⇒ [Text] → Reference target → Link
linkRelations ↦ reference
  = Link {..}
  where
    linkURI
      = ?render
        (setEmbedding ?embedding reference)

link ∷ RenderLink target ⇒ [Text] → Reference target → Link
link = (↦)

infix 6 ⤠
(⤠) ∷ RenderLink target ⇒ [Text] → target → Link
(⤠)
  = (∘ (, mempty))
  ∘ (↦)
  where
    ?embedding = empty

linkPlain ∷ RenderLink target ⇒ [Text] → target → Link
linkPlain = (⤠)



data Action
  = Action
    { actionName    ∷ Text
    , actionClasses ∷ [Text]
    , actionMethod  ∷ Method
    , actionURI     ∷ Text
    , actionTitle   ∷ Maybe Text
    , actionType    ∷ ActionType
    , actionFields  ∷ [Field]
    }
  deriving (Eq, Read, Show)

mkAction ∷ Text → Text → Action
mkAction
  actionName
  actionURI
  = Action {..}
  where
    actionClasses = def
    actionMethod  = def
    actionTitle   = def
    actionType    = def
    actionFields  = def

instance ToJSON Action where
  toJSON Action {..}
    = object
      [ "name"   .= actionName
      , "class"  .= actionClasses
      , "method" .= actionMethod
      , "href"   .= actionURI
      , "title"  .= actionTitle
      , "type"   .= actionType
      , "fields" .= actionFields
      ]



newtype Method
  = Method
    { unMethod ∷ Text
    }
  deriving (Eq, FromJSON, IsString, Ord, Read, Show, ToJSON)

instance Default Method where
  def = "GET"



newtype ActionType
  = ActionType
    { unActionType ∷ Text
    }
  deriving (Eq, FromJSON, IsString, Ord, Read, Show, ToJSON)

instance Default ActionType where
  def = "application/x-www-form-urlencoded"



data Field
  = Field
    { fieldName  ∷ Text
    , fieldType  ∷ FieldType
    , fieldValue ∷ Maybe Text
    , fieldTitle ∷ Maybe Text
    }
  deriving (Eq, Read, Show)

instance ToJSON Field where
  toJSON Field {..}
    = object
      [ "name"  .= fieldName
      , "type"  .= fieldType
      , "value" .= fieldValue
      , "title" .= fieldTitle
      ]

mkField ∷ Text → FieldType → Field
mkField fieldName fieldType
  = Field {..}
  where
    fieldValue = def
    fieldTitle = def



newtype FieldType
  = FieldType
    { unFieldType ∷ Text
    }
  deriving (Eq, FromJSON, IsString, Ord, Read, Show, ToJSON)

instance Default FieldType where
  def = "text"



embeddingActions
  ∷ RenderLink target
  ⇒ Reference target → [Action]

embeddingActions self
  = [embed]
  ⊕ (embedNothing <$) (guard $ ?embedding ≠ empty    )
  ⊕ (embedFull    <$) (guard $ ?embedding ≠ pure Full)
  where
    embed
      = (mkAction "embed" href)
        { actionTitle  = pure "Embed sub-entities"
        , actionFields = [depth]
        }
      where
        href = ?render $ setEmbedding empty self
        depth
          = (mkField "embedding" "number")
            { fieldTitle = pure "Depth"
            }

    embedNothing
      = (mkAction "embedNothing" href)
        { actionTitle = pure "Don’t embed sub-entities"
        }
      where
        href = ?render $ setEmbedding empty self

    embedFull
      = (mkAction "embedFull" href)
        { actionTitle = pure "Fully embed sub-entities"
        }
      where
        href = ?render $ setEmbedding (pure Full) self
