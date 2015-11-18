{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings, RecordWildCards, QuasiQuotes, TemplateHaskell, TypeFamilies, TypeSynonymInstances, OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmFhsx2hs #-}
module Main where

import Control.Exception             (bracket, finally)
import Control.Lens                  ((^.))
import Data.Acid.Local               (createCheckpointAndClose)
import qualified Data.IxSet          as IxSet
import Data.IxSet                    (IxSet, Indexable, Proxy(..), (@=), getEQ, getOne, ixSet, ixFun)
import Data.Maybe
import Data.Text                     (Text)
import Data.UserId                   (UserId(..))
import qualified Data.Text.Lazy      as Lazy
import qualified Data.Text           as Text
import Data.Time.Clock               (UTCTime, getCurrentTime)
import Happstack.Authenticate.Core           (AuthenticateState, AuthenticateConfig(..), AuthenticateURL(Controllers), Username, getToken, tokenUser, unUsername, username, usernamePolicy)
import Happstack.Authenticate.Route          (initAuthentication)
import Happstack.Authenticate.Password.Core  (PasswordConfig(..))
import Happstack.Authenticate.Password.Route (initPassword)
-- import Happstack.Authenticate.Password.URL(PasswordURL(..))
-- import Happstack.Authenticate.OpenId.Core  (OpenIdState)
import Happstack.Authenticate.OpenId.Route   (initOpenId)
-- import Happstack.Authenticate.OpenId.URL   (OpenIdURL(..))

import qualified Happstack.Server.HSP.HTML as HTML
import Happstack.Server.JMacro       ()
import Happstack.Foundation
import HSP                           (toName)
import HSP.XML                       as HTML
import Language.Javascript.JMacro
import System.Environment
import System.FilePath               ((</>))

------------------------------------------------------------------------------
-- Model
------------------------------------------------------------------------------

-- | an id which uniquely identifies a paste
--
-- NOTE: 'PasteId 0' indicates that a 'Paste' has not been assigned an
-- id yet. Though.. I am not thrilled about 0 having special meaning
-- that is not enforced by the type system.
newtype PasteId = PasteId { unPasteId :: Integer }
    deriving (Eq, Ord, Read, Show, Enum, Data, Typeable)
$(deriveSafeCopy 0 'base ''PasteId)
$(derivePathInfo ''PasteId)

-- | The format of the paste. Currently we only support plain-text,
-- but later we might add support for Haskell syntax hightlighting,
-- etc.
data Format
    = PlainText
      deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable)
$(deriveSafeCopy 0 'base ''Format)

-- | the meta-data for a 'Paste'
--
-- We break this out separately from the paste, because we often want
-- only the meta-data. For example, when generating a list of recent pastes.
data PasteMeta = PasteMeta
    { pasteId  :: PasteId
    , replyTo  :: Maybe PasteId
    , title    :: Text
    , nickname :: Username
    , format   :: Format
    , pasted   :: UTCTime
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''PasteMeta)

-- | a 'Paste'
data Paste = Paste
    { pasteMeta :: PasteMeta
    , paste     :: Text
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''Paste)

-- | The 'Indexable Paste' instance will allow us to create an 'IxSet Paste'
--
-- We index on the 'PasteId' and the time it was pasted.
instance Indexable Paste where
    empty =
        ixSet [ ixFun $ (:[]) . pasteId  . pasteMeta
              , ixFun $ (:[]) . pasted   . pasteMeta
              ]

-- | record to store in acid-state
data CtrlVState = CtrlVState
    { pastes      :: IxSet Paste
    , nextPasteId :: PasteId
    }
    deriving (Data, Typeable)
$(deriveSafeCopy 0 'base ''CtrlVState)

-- | initial value to use with acid-state when no prior state is found
initialCtrlVState :: CtrlVState
initialCtrlVState =
    CtrlVState { pastes      = IxSet.empty
               , nextPasteId = PasteId 1
               }

------------------------------------------------------------------------------
-- Acid-State events
------------------------------------------------------------------------------

-- | add or update a paste
--
-- If the PasteId is '0', then update the paste to use the next unused PasteId and insert it into the IxSet.
--
-- Otherwise, we update the existing paste.
insertPaste :: Paste
            -> Update CtrlVState PasteId
insertPaste p@Paste{..}
    | pasteId pasteMeta == PasteId 0 =
        do cvs@CtrlVState{..} <- get
           put $ cvs { pastes = IxSet.insert (p { pasteMeta = pasteMeta { pasteId = nextPasteId }}) pastes
                     , nextPasteId = succ nextPasteId
                     }
           return nextPasteId
    | otherwise =
        do cvs@CtrlVState{..} <- get
           put $ cvs { pastes = IxSet.updateIx (pasteId pasteMeta) p pastes }
           return (pasteId pasteMeta)

-- | get a paste by it's 'PasteId'
getPasteById :: PasteId -> Query CtrlVState (Maybe Paste)
getPasteById pid = getOne . getEQ pid . pastes <$> ask

type Limit  = Int
type Offset = Int

-- | get recent pastes
getRecentPastes :: Limit  -- ^ maximum number of recent pastes to return
                -> Offset -- ^ number of pastes skip (useful for pagination)
                -> Query CtrlVState [PasteMeta]
getRecentPastes limit offset =
    do CtrlVState{..} <- ask
       return $ map pasteMeta $ take limit $ drop offset $ IxSet.toDescList (Proxy :: Proxy UTCTime) pastes

-- | get a list of all paste ids
getPasteIds :: Query CtrlVState [PasteId]
getPasteIds =
    do CtrlVState{..} <- ask
       return $ map (pasteId . pasteMeta) $ IxSet.toAscList (Proxy :: Proxy PasteId) pastes

-- | now we need to tell acid-state which functions should be turn into
-- acid-state events.
$(makeAcidic ''CtrlVState
   [ 'getPasteById
   , 'getRecentPastes
   , 'getPasteIds
   , 'insertPaste
   ])

------------------------------------------------------------------------------
-- Route
------------------------------------------------------------------------------

-- | All the routes for our web application
data Route
    = ViewRecent
    | ViewPaste PasteId
    | NewPaste
    | CSS
    | Authenticate AuthenticateURL
    | CtrlVAppJs
      deriving (Eq, Ord, Read, Show, Data, Typeable)

-- | we will just use template haskell to derive the route mapping
$(derivePathInfo ''Route)

------------------------------------------------------------------------------
-- CtrlV type-aliases
------------------------------------------------------------------------------

-- | The foundation types are heavily parameterized -- but for our app
-- we can pin all the type parameters down.

type CtrlV'    = FoundationT' Route Acid () IO
type CtrlV     = XMLGenT CtrlV'
type CtrlVForm = FoundationForm Route Acid () IO

------------------------------------------------------------------------------
-- From demo-hsp Acid.hs
------------------------------------------------------------------------------

-- | 'Acid' holds all the 'AcidState' handles for this site.
data Acid = Acid
    { acidAuthenticate :: AcidState AuthenticateState
    , acidPaste        :: AcidState CtrlVState
    }

instance (Functor m, Monad m) => HasAcidState (FoundationT' url Acid reqSt m) AuthenticateState where
    getAcidState = acidAuthenticate <$> getAcidSt

instance (Functor m, Monad m) => HasAcidState (FoundationT' url Acid reqSt m) CtrlVState where
    getAcidState = acidPaste <$> getAcidSt

-- | run an action which takes 'Acid'.
--
-- Uses 'bracket' to open / initialize / close all the 'AcidState' handles.
--
-- WARNING: The database files should only be opened by one thread in
-- one application at a time. If you want to access the database from
-- multiple threads (which you almost certainly do), then simply pass
-- the 'Acid' handle to each thread.
withAcid :: AcidState AuthenticateState
         -> Maybe FilePath -- ^ state directory
         -> (Acid -> IO a) -- ^ action
         -> IO a
withAcid authenticateState mBasePath f =
    let basePath = fromMaybe "_state" mBasePath in
    bracket (openLocalStateFrom (basePath </> "paste")   initialCtrlVState)   (createCheckpointAndClose) $ \paste ->
        f (Acid authenticateState paste)


------------------------------------------------------------------------------
-- angular App Controler
------------------------------------------------------------------------------

ctrlVAppJs :: JStat
ctrlVAppJs = [jmacro|
  {
    var ctrlVApp = angular.module('ctrlVApp', [
      'happstackAuthentication',
      'usernamePassword',
      'openId',
      'ngRoute'
    ]);

    ctrlVApp.config(['$routeProvider',
      function($routeProvider) {
        $routeProvider.when('/resetPassword',
                             { templateUrl: '/authenticate/authentication-methods/password/partial/reset-password-form',
                               controller: 'UsernamePasswordCtrl'
                             });
      }]);
  }
  |]

------------------------------------------------------------------------------
-- appTemplate
------------------------------------------------------------------------------

-- | page template function
appTemplate :: ( EmbedAsChild CtrlV' headers
               , EmbedAsChild CtrlV' body
               ) =>
               Lazy.Text   -- ^ page title
            -> headers  -- ^ extra headers to add to \<head\> tag
            -> body     -- ^ contents of \<body\> tag
            -> CtrlV Response
appTemplate ttl moreHdrs bdy =
    do routeFn <- askRouteFn
       html <- <html>
                <head>
                 <meta name="viewport" content="width=device-width, initial-scale=1" />
                 <title><% ttl %></title>
                 <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js"></script>
                 <script src="//ajax.googleapis.com/ajax/libs/angularjs/1.2.24/angular.min.js"></script>
                 <script src="//ajax.googleapis.com/ajax/libs/angularjs/1.2.24/angular-route.min.js"></script>
                 <script src=(routeFn CtrlVAppJs [])></script>
                 <script src=(routeFn (Authenticate Controllers) [])></script>
                 <link rel="stylesheet" href=CSS type="text/css" media="screen" />
                 <% moreHdrs %>
                </head>
                <body ng-app="ctrlVApp" ng-controller="AuthenticationCtrl">
                 <div ng-controller="UsernamePasswordCtrl">
                  <div id="logo">^V</div>
                  <ul class="menu">
                   <li><a href=NewPaste>new paste</a></li>
                   <li><a href=ViewRecent>recent pastes</a></li>
                  </ul>
--                 <up-login-inline />
                  <% bdy %>
                 </div>
                </body>
               </html>

{-
                   <% do mUserId <- join $ liftM2 getUserId getAcidState getAcidState

                         -- Debugging
                         -- authState <- query' acidAuth AskAuthState
                         -- let authDump = traceMsg "authState: " $ ppDoc authState

                         case mUserId of
                            Nothing -> do
                              <ul class="auth">
                                <li>You are not logged in</li>
                                <li><a href=(U_AuthProfile $ AuthURL A_Login)>login</a></li>
                                <li><a href=(U_AuthProfile $ AuthURL A_CreateAccount)>create account</a></li>
                              </ul>
                            (Just uid) -> do
                              <ul class="auth">
                                <li>You are logged in with profile <% Lazy.pack $ show $ unUserId uid %></li>
                                -- Debugging
                                -- <li><% show authDump %></li>
                                <li><a href=(U_AuthProfile $ AuthURL A_Logout)>logout</a></li>
                                <li><a href=(U_AuthProfile $ AuthURL A_AddAuth)>add another auth mode to your profile</a></li>
                              </ul>
                   %>
-}
       return (toResponse html)

-- | This makes it easy to embed a PasteId in an HSP template
instance EmbedAsChild CtrlV' PasteId where
    asChild (PasteId i) = asChild ('#' : show i)

-- | This makes it easy to embed a timestamp into an HSP
-- template. 'show' provides way too much precision, so something
-- using formatTime would be better.
instance EmbedAsChild CtrlV' UTCTime where
    asChild time = asChild (show time)

instance EmbedAsChild CtrlV' Username where
   asChild u = asChild (u ^. unUsername)
------------------------------------------------------------------------------
-- Pages
------------------------------------------------------------------------------

-- | page handler for 'ViewRecent'
viewRecentPage :: CtrlV Response
viewRecentPage =
    do method GET
       recent <- query (GetRecentPastes 20 0)
       case recent of
         [] -> appTemplate "Recent Pastes" () <p>There are no pastes yet.</p>
         _  -> appTemplate "Recent Pastes" () $
                <%>
                 <h1>Recent Pastes</h1>
                 <table>
                  <thead>
                   <tr>
                    <th>id</th>
                    <th>replyto</th>
                    <th>title</th>
                    <th>author</th>
                    <th>date</th>
                    <th>format</th>
                   </tr>
                  </thead>
                  <tbody>
                   <% mapM mkTableRow recent %>
                  </tbody>
                 </table>
                </%>
    where
      mkTableRow PasteMeta{..} =
          <tr>
           <td><a href=(ViewPaste pasteId)><% show $ unPasteId pasteId %></a></td>
           <td><% case replyTo of
                    Nothing -> <span/>
                    (Just pid) -> <a href=(ViewPaste pid)><% pid %></a> %></td>
           <td><a href=(ViewPaste pasteId)><% title       %></a></td>
           <td><% nickname %></td>
           <td><% pasted      %></td>
           <td><% show format %></td>
          </tr>

-- | page handler for 'ViewPaste'
viewPastePage :: PasteId -> CtrlV Response
viewPastePage pid =
    do method GET
       mPaste <- query (GetPasteById pid)
       case mPaste of
         Nothing ->
             do notFound ()
                appTemplate "Paste not found." () $
                    <p>Paste <% pid %> could not be found.</p>
         (Just (Paste (PasteMeta{..}) paste)) ->
             do ok ()
                appTemplate (Lazy.pack $ "Paste " ++ (show $ unPasteId pid)) () $
                    <div class="paste">
                     <dl class="paste-header">
                      <dt>Paste:</dt><dd><a href=(ViewPaste pid)><% pid %></a></dd>
                      <% case replyTo of
                           Nothing -> <%><span /></%>
                           (Just rid) ->
                               <%><dt>Reply To:</dt><dd><a href=(ViewPaste rid)><% rid %></a></dd></%>
                        %>
                      <dt>Title:</dt><dd><% title %></dd>
                      <dt>Author:</dt><dd><% nickname %></dd>
                     </dl>
                     <div class="paste-body">
                      <% formatPaste format paste %>
                     </div>
                    </div>

-- | convert the paste to HTML. We currently only support 'PlainText',
-- but eventually it might do syntax hightlighting, markdown, etc.
--
-- Note that we do not have to worry about escaping the txt
-- value.. that is done automatically by HSP.
formatPaste :: Format -> Text -> CtrlV XML
formatPaste PlainText txt =
    <pre><% txt %></pre>

-- | page handler for 'NewPaste'
newPastePage :: CtrlV Response
newPastePage =
    do here <- whereami
       appTemplate "Add a Paste" () $
              <%>
                <div up-authenticated=False>
                 <h1>You Are Not Logged In</h1>
                 <up-login-inline />
                 <p>If you don't have an account already you can signup:</p>
                 <up-signup-password />

                 <p>If you have forgotten your password you can request it to be sent to your email address:</p>
                 <up-request-reset-password />

                 <div ng-controller="OpenIdCtrl">
                  <p>You could also sign in using your Google OpenId:</p>
                  <openid-google />
                  <openid-yahoo />
                 </div>
                </div>

                <div up-authenticated=True>
                 <h1>Add a paste</h1>
                 <% theForm here %>
                 <p>You are logged in. You can <a ng-click="logout()" href="">Click Here To Logout</a>. Or you can change your password here:</p>
                 <up-change-password />

                 <div ng-controller="OpenIdCtrl" ng-hide="!claims.authAdmin">
                  <p>If you are an admin you can edit the realm:</p>
                  <openid-realm />
                 </div>

                </div>
              </%>
    where
      theForm :: Route -> CtrlV [CtrlV XML]
      theForm here =
          do pasteIds <- query GetPasteIds
             reform (form here) "add" success Nothing (pasteForm pasteIds)

      success :: Paste -> CtrlV Response
      success paste =
          do pid <- update (InsertPaste paste)
             seeOtherURL (ViewPaste pid)

-- | the 'Form' used for entering a new paste
pasteForm :: [PasteId] -> CtrlVForm Paste
pasteForm pasteIds =
    (fieldset $
       ul $
          (,,,) <$> (li $ label <span>reply to</span> ++> (select ((PasteId 0,"none") : [(pid, show (unPasteId pid)) | pid <- pasteIds]) (== PasteId 0)))
                <*> (li $ label <span>title</span>  ++> (inputText "" `transformEither` required) <++ errorList)
                <*> (li $ label <span>format</span> ++> formatForm)
                <*> (li $ label <div>paste</div>    ++> errorList ++> (textarea 80 25 "" `transformEither` required))
                <* inputSubmit "paste!"
    )  `transformEitherM` toPaste
    where
      formatForm =
          select [(a, show a) | a <- [minBound .. maxBound]] (== PlainText)
      toPaste (rid, ttl, fmt, bdy) =
          do now     <- liftIO getCurrentTime
             as      <- getAcidState
             mToken <- lift $ getToken as
             case mToken of
               Nothing ->
                 return $ Left $ "You must login to post."
               (Just (token, _)) ->
                 return $ Right $
                        (Paste { pasteMeta = PasteMeta { pasteId  = PasteId 0
                                                       , replyTo  = case rid of (PasteId 0) -> Nothing ; _ -> Just rid
                                                       , title    = ttl
                                                       , nickname = token ^. tokenUser ^. username
                                                       , format   = fmt
                                                       , pasted   = now
                                                       }
                               , paste = bdy
                               })
      required txt
          | Text.null txt = Left "Required"
          | otherwise     = Right txt

------------------------------------------------------------------------------
-- route
------------------------------------------------------------------------------

-- | the route mapping function
route :: (AuthenticateURL -> RouteT AuthenticateURL (ServerPartT IO) Response)
      -> Text
      -> Route
      -> CtrlV Response
route routeAuthenticate _baseURL url =
    case url of
      -- FIXME: replace the ViewRecent thing here with "go back to
      -- the last page we were on". - rlpowell
      Authenticate authenticateURL ->
         lift $ nestURL Authenticate $
            mapRouteT lift $ routeAuthenticate authenticateURL
      CtrlVAppJs   ->
        do ok $ toResponse $ ctrlVAppJs

      ViewRecent      -> viewRecentPage
      (ViewPaste pid) -> viewPastePage pid
      NewPaste        -> newPastePage
      CSS             -> serveFile (asContentType "text/css") "style.css"

------------------------------------------------------------------------------
-- main
------------------------------------------------------------------------------

-- | start the app. listens on port 8000.
main :: IO ()
main = do
  (cleanup, routeAuthenticate, authenticateState) <-
         let authenticateConfig = AuthenticateConfig
               { _isAuthAdmin        = const $ return True
               , _usernameAcceptable = usernamePolicy
               , _requireEmail       = True
               }
             passwordConfig = PasswordConfig
               { _resetLink = "http://localhost:8000/#resetPassword"
               , _domain    =  "example.org"
               , _passwordAcceptable = \t ->
                   if Text.length t >= 5
                   then Nothing
                   else Just "Must be at least 5 characters."
               }
         in
           initAuthentication Nothing authenticateConfig
            [ initPassword passwordConfig
            , initOpenId
            ]
  withAcid authenticateState Nothing $ \acid -> do
           args <- getArgs
           let appPort = if (length args > 0) then (read (args !! 0) :: Int) else 8000
           let hosturl = if (length args > 1) then (Text.pack ((args !! 1) :: String))   else (Text.pack "http://localhost:8000")
           (simpleApp id
             defaultConf { httpConf = nullConf {
                                        port      = appPort
                                      }
                         }
               (AcidUsing acid)
               ()
               ViewRecent
               hosturl
               (route routeAuthenticate hosturl)) `finally` cleanup
