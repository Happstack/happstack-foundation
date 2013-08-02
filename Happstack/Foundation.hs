{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies, TypeSynonymInstances, RankNTypes, RecordWildCards, GADTs #-}
{-|
@happstack-foundation@ provides a type-safe environment for Haskell web development. It builds on top of:

 - happstack-server - an HTTP server

 - HSP - HTML Templating

 - web-routes - type-safe URL routing

 - reform - type-safe form composition and validation

 - acid-state - native Haskell persistent database

An example application can be found here:

<http://patch-tag.com/r/mae/happstack/snapshot/current/content/pretty/happstack-foundation/examples/ControlV/Main.hs>

A screencast can be found here:

<http://www.youtube.com/watch?v=7Wmszk4wZxQ>

@happstack-foundation@ itself is not yet documented in the Happstack Crash Course. However, all of the components that it uses are:

<http://www.happstack.com/docs/crashcourse/index.html>

-}
module Happstack.Foundation
    ( -- * Configuration
      AcidConfig(..)
    , FoundationConf(..)
    , defaultConf
      -- * Type Aliases
    , FoundationT
    , FoundationT'
    , FoundationForm
    -- * FoundationT functions
    , whereami
    , getRequestState
    , setRequestState
    , modifyRequestState
      -- * HTML Template
    , defaultTemplate
      -- * acid-state
    , HasAcidState(..)
    , query
    , update
    , getAcidSt
      -- * running
    , simpleApp
    -- * re-exports
    , Data(..)
    , Typeable(..)
    , module Control.Applicative
    , module Control.Monad.Reader
    , module Control.Monad.State
    , module Data.SafeCopy
    , module Data.Acid
    , module Happstack.Server
    , module HSP
    , module Web.Routes
    , module Web.Routes.Happstack
    , module Web.Routes.TH
    , module Text.Reform
    , module Text.Reform.Happstack
    , module Text.Reform.HSP.Text
    )
    where

import Control.Applicative
import Control.Concurrent
import Control.Exception.Lifted    (bracket)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Acid hiding (query, update)
import Data.Acid.Advanced
import Data.Acid.Local
import Data.Data
import Data.SafeCopy
import Data.Text as Text
import qualified Data.Text.Lazy as Lazy
import Data.String (IsString(..))
import Control.Monad.Reader
import Control.Monad.State
import Happstack.Server.XMLGenT () -- instances for (Happstack (XMLGenT m))
import HSP  (XMLGenerator(..), XMLGen(..), EmbedAsChild(..), EmbedAsAttr(..), XML, XMLGenT(..), unXMLGenT, XMLType, Attr(..), fromStringLit)
import Text.Reform
import Text.Reform.Happstack
import Text.Reform.HSP.Text
import Happstack.Server
import Happstack.Server.SimpleHTTP
import qualified Happstack.Server.HSP.HTML as HTML
import Web.Routes
import Web.Routes.TH
import Web.Routes.Happstack
import Web.Routes.XMLGenT

-- | 'HasAcidState' provides a single method 'getAcidState' which can be used to retrieve an 'AcidState' handle from the current monad.
class HasAcidState m st where
    getAcidState :: m (AcidState st)


instance (HasAcidState (FoundationT' url acid reqSt m) acidSt) => HasAcidState (XMLGenT (FoundationT' url acid reqSt m)) acidSt where
    getAcidState = XMLGenT getAcidState


getAcidSt :: (Functor m, MonadState (AppState url acidState requestState) m) => m acidState
getAcidSt = acid <$> get

-- | wrapper around query from acid-state
--
-- This variant automatically gets the 'AcidState' handle from the monad
query :: forall event m.
         ( Functor m
         , MonadIO m
         , QueryEvent event
         , HasAcidState m (EventState event)
         ) =>
         event
      -> m (EventResult event)
query event =
    do as <- getAcidState
       query' (as :: AcidState (EventState event)) event

-- | wrapper around update from acid-state
--
-- This variant automatically gets the 'AcidState' handle from the monad
update :: forall event m.
          ( Functor m
          , MonadIO m
          , UpdateEvent event
          , HasAcidState m (EventState event)
          ) =>
          event
       -> m (EventResult event)
update event =
    do as <- getAcidState
       update' (as :: AcidState (EventState event)) event

-- | bracket the opening and close of the `AcidState` handle.
--
-- automatically creates a checkpoint on close
--
-- unfortunately, when nesting multiple calls if some migrations
-- succeed and some fail it leaves the system in an state where it is
-- hard to roll back to the old version of the app because some of the
-- checkpoints have been upgrade. We should replace this with a
-- version that only does the checkpoint if *all* the acid states
-- could be openned successfully.
withLocalState :: (MonadBaseControl IO m, MonadIO m, IsAcidic st, Typeable st) =>
                  Maybe FilePath        -- ^ path to state directory
               -> st                    -- ^ initial state value
               -> (AcidState st -> m a) -- ^ function which uses the `AcidState` handle
               -> m a
withLocalState mPath initialState =
    bracket (liftIO $ (maybe openLocalState openLocalStateFrom mPath) initialState)
            (\acid -> liftIO $ (createArchive acid >> createCheckpointAndClose acid))

-- | simple record that holds some state information that we want available in the 'FoundationT' monad
data AppState url acidState requestState = AppState
    { here  :: url
    , acid  :: acidState
    , reqSt :: requestState
    }

-- | similar to the 'FoundationT'' type alias, but without the 'XMLGenT' wrapper. This variant is most often used in class constraints.
type FoundationT' url acidState requestState m = RouteT url (StateT (AppState url acidState requestState) (ServerPartT m))

-- | the 'FoundationT' monad
--
-- - @url@ - the type-safe URL route type
--
-- - @acidState@ - the type of the state value stored in acid-state
--
-- - @requestState@ - a per-request state value that the developer can get/set/modify
--
-- - @m@ - inner monad
--
-- see also: 'whereami', 'getRequestState', 'setRequestState', 'modifyRequestState', 'simpleApp'
type FoundationT  url acidState requestState m = XMLGenT (FoundationT' url acidState requestState m)

-- | returns the decoded 'url' from the 'Request'
whereami :: (Functor m, Monad m) => FoundationT url acidState requestState m url
whereami = here <$> get

-- | get the 'requestState' value
getRequestState :: (Functor m, MonadState (AppState url acidState requestState) m) => m requestState
getRequestState = reqSt <$> get

-- | set the 'requestState' value
setRequestState :: (Functor m, MonadState (AppState url acidState requestState) m) =>
                   requestState
                -> m ()
setRequestState st = modify $ \appState -> appState { reqSt = st }

-- | set the 'requestState' value
modifyRequestState :: MonadState (AppState url acidState requestState) m =>
                      (requestState -> requestState)
                   -> m ()

modifyRequestState f = modify $ \appState -> appState { reqSt = f (reqSt appState) }

instance (Functor m, Monad m) => HasAcidState (FoundationT url (AcidState acidState) requestState m) acidState where
    getAcidState = acid <$> get

-- | an error type used with reform forms
data AppError
    = AppCFE (CommonFormError [Input])
    | TextError Text

instance IsString AppError where
    fromString = TextError . fromString

instance FormError AppError where
    type ErrorInputType AppError = [Input]
    commonFormError = AppCFE

instance (Functor m, Monad m) => EmbedAsChild (FoundationT' url acidState requestState m) AppError where
    asChild (AppCFE cfe)    = asChild (commonFormErrorStr show cfe)
    asChild (TextError txt) = asChild txt

-- | 'FoundationForm' is an alias for working with reform based Forms
type FoundationForm url acidState requestState m = Form (FoundationT url acidState requestState m) [Input] AppError [FoundationT url acidState requestState m XML] ()

-- | configuration information for our acid-state database
data AcidConfig st where
    AcidLocal :: (IsAcidic st, Typeable st) => Maybe FilePath -> st -> AcidConfig (AcidState st)
    AcidUsing :: st -> AcidConfig st

withAcidConfig :: (MonadIO m, MonadBaseControl IO m) => AcidConfig st -> (st -> m a) -> m a
withAcidConfig (AcidLocal acidPath initialState) f =
    withLocalState acidPath initialState f
withAcidConfig (AcidUsing st) f = f st

-- | default page template
defaultTemplate :: ( Functor m, Monad m
                   , XMLGenerator (FoundationT' url acidState requestState m)
                   , EmbedAsChild (FoundationT' url acidState requestState m) body
                   , EmbedAsChild (FoundationT' url acidState requestState m) headers
                   , XMLType (FoundationT' url acidState requestState m) ~ XML
                   ) =>
                   Lazy.Text
                -> headers
                -> body
                -> FoundationT url acidState requestState m XML
defaultTemplate title headers body =
    XMLGenT $ HTML.defaultTemplate title headers body

-- | configuration for server
data FoundationConf = FoundationConf
    { httpConf   :: Conf
    , bodyPolicy ::  BodyPolicy
    }

-- | configuration
defaultConf :: FoundationConf
defaultConf = FoundationConf
    { httpConf = nullConf
    , bodyPolicy = defaultBodyPolicy "/tmp" 10000000 100000 100000
    }

-- | run the application
--
-- starts the database, listens for requests, etc.
simpleApp :: (ToMessage a, PathInfo url, Monad m) =>
             (forall r. m r -> IO r)             -- ^ function to flatten inner monad
          -> FoundationConf                      -- ^ 'Conf' to pass onto 'simpleHTTP'
          -> AcidConfig acidState                -- ^ 'AcidState' configuration
          -> requestState                        -- ^ initial @requestState@ value
          -> url                                 -- ^ default URL (ie, what does / map to)
          -> Text                                -- ^ the base URL for the site as seen by the outside world (or, at least, by your openid provider) (e.g. "http://example.org:8000", no trailing slash)
          -> (url -> FoundationT url acidState requestState m a) -- ^ handler
          -> IO ()
simpleApp flattener FoundationConf{..} acidConfig initialReqSt defRoute baseURI route =
    withAcidConfig acidConfig $ \acid ->
        do tid <- forkIO $ simpleHTTP httpConf $ do decodeBody bodyPolicy
                                                    implSite baseURI Text.empty (site acid)
           waitForTermination
           killThread tid
    where
      site acid =
          setDefault defRoute $ mkSitePI (\showFn url ->
                                        mapServerPartT flattener (evalStateT (unRouteT (unXMLGenT (route url)) showFn) (AppState url acid initialReqSt)))
