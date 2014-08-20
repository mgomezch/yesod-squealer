{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UnicodeSyntax         #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Yesod.Squealer
  ( module Yesod.Squealer
  , module Yesod.Squealer.Routes
  )
where

import Control.Applicative (pure)
import Data.Text           (Text)
import System.IO           (IO)
import Yesod.Core          (YesodSubDispatch(yesodSubDispatch))
import Yesod.Core.Dispatch (mkYesodSubDispatch)
import Yesod.Core.Types    (HandlerT)

import Yesod.Squealer.Handler.Column   (getColumnR)
import Yesod.Squealer.Handler.Database (getDatabaseR)
import Yesod.Squealer.Handler.Rows     (getRowsR, postRowsR)
import Yesod.Squealer.Handler.Table    (getTableR)
import Yesod.Squealer.Handler.Version  (getVersionR, putVersionR, deleteVersionR, getPredecessorR, getSuccessorR)

import Yesod.Squealer.Routes



instance YesodSquealer master ⇒ YesodSubDispatch Squealer (HandlerT master IO) where
  yesodSubDispatch = $(mkYesodSubDispatch resourcesSquealer)



-- TODO: CORS headers, and Allow from the resource stuff.
-- TODO: authorization, access restrictions…

optionsDatabaseR ∷ SquealerHandler ()
optionsDatabaseR = pure ()

optionsTableR ∷ Text → SquealerHandler ()
optionsTableR _ = pure ()

optionsRowsR ∷ Text → SquealerHandler ()
optionsRowsR _ = pure ()

optionsColumnR ∷ Text → Text → SquealerHandler ()
optionsColumnR _ _ = pure ()

optionsVersionR ∷ Text → Text → SquealerHandler ()
optionsVersionR _ _ = pure ()

optionsPredecessorR ∷ Text → Text → SquealerHandler ()
optionsPredecessorR _ _ = pure ()

optionsSuccessorR ∷ Text → Text → SquealerHandler ()
optionsSuccessorR _ _ = pure ()
