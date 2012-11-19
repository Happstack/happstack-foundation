{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings, RecordWildCards, TemplateHaskell, TypeFamilies, TypeSynonymInstances, OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Main where

import Happstack.Foundation
import qualified Data.IxSet as IxSet
import Data.IxSet (IxSet, Indexable, Proxy(..), (@=), getEQ, getOne, ixSet, ixFun)
import Data.Text  (Text)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime, getCurrentTime)
-- Added for auth
import Happstack.Auth
import Happstack.Auth.Core.Auth
import Happstack.Auth.Core.Profile
import Control.Exception           (bracket)
import Data.Acid.Local             (createCheckpointAndClose)
import System.FilePath             ((</>))
import Data.Maybe
import System.Environment

-- Added for Blaze under Auth
import qualified Happstack.Server.HSP.HTML as HTML
import Text.Blaze.Html                        (Html)
import Text.Blaze.Html.Renderer.String         (renderHtml)
import qualified HSX.XMLGenerator as HSX

------------------------------------------------------------------------------
-- Model
------------------------------------------------------------------------------

-- | an id which uniquely identifies a paste
--
-- NOTE: 'PasteId 0' indicates that a 'Paste' has not been assigned an
-- id yet. Though.. I am not thrilled about 0 having special meaning
-- that is not enforced by the type system.
newtype PasteId = PasteId { unPasteId :: Integer }
    deriving (Eq, Ord, Read, Show, Enum, Data, Typeable, SafeCopy)
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
    , title    :: Text
    , nickname :: Text
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
        ixSet [ ixFun $ (:[]) . pasteId . pasteMeta
              , ixFun $ (:[]) . pasted  . pasteMeta
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

-- | now we need to tell acid-state which functions should be turn into
-- acid-state events.
$(makeAcidic ''CtrlVState
   [ 'getPasteById
   , 'getRecentPastes
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
    | U_AuthProfile AuthProfileURL
      deriving (Eq, Ord, Read, Show, Data, Typeable)

-- | we will just use template haskell to derive the route mapping
$(derivePathInfo ''Route)

------------------------------------------------------------------------------
-- CtrlV type-aliases
------------------------------------------------------------------------------

-- | The foundation types are heavily parameterized -- but for our app
-- we can pin all the type parameters down.
type CtrlV'    = FoundationT' Route CtrlVState () IO
type CtrlV     = XMLGenT CtrlV'
type CtrlVForm = FoundationForm Route CtrlVState () IO

------------------------------------------------------------------------------
-- From demo-hsp Acid.hs
------------------------------------------------------------------------------

-- | 'Acid' holds all the 'AcidState' handles for this site.
data Acid = Acid
    { acidAuth        :: AcidState AuthState
    , acidProfile     :: AcidState ProfileState
    }

-- | run an action which takes 'Acid'.
--
-- Uses 'bracket' to open / initialize / close all the 'AcidState' handles.
--
-- WARNING: The database files should only be opened by one thread in
-- one application at a time. If you want to access the database from
-- multiple threads (which you almost certainly do), then simply pass
-- the 'Acid' handle to each thread.
withAcid :: Maybe FilePath -- ^ state directory
         -> (Acid -> IO a) -- ^ action
         -> IO a
withAcid mBasePath f =
    let basePath = fromMaybe "_state" mBasePath in
    bracket (openLocalStateFrom (basePath </> "auth")        initialAuthState)        (createCheckpointAndClose) $ \auth ->
    bracket (openLocalStateFrom (basePath </> "profile")     initialProfileState)     (createCheckpointAndClose) $ \profile ->
        f (Acid auth profile)

------------------------------------------------------------------------------
-- appTemplate
------------------------------------------------------------------------------

-- | page template function
--
-- There are two forms here because we need to make it work for both
-- our usual happstack-foundation/HSP stuff, and for happstack-auth,
-- which is Blaze rather than HSP based.  So we make the base
-- template, and then make two versions that give it each of the two
-- contexts it needs.
baseAppTemplate :: ( XMLGenerator m
               , Happstack m
               , EmbedAsAttr m (Attr String Route)
               , EmbedAsChild m headers
               , EmbedAsChild m body
               ) =>
               Acid
            -> String   -- ^ page title
            -> headers  -- ^ extra headers to add to \<head\> tag
            -> body     -- ^ contents of \<body\> tag
            -> m (HSX.XMLType m)
baseAppTemplate acid@Acid{..} ttl moreHdrs bdy = HTML.defaultTemplate ttl <%><link rel="stylesheet" href=CSS type="text/css" media="screen" /><% moreHdrs %></%> $
                <%>
                 <div id="logo">^V</div>
                 <ul class="menu">
                  <li><a href=NewPaste>new paste</a></li>
                  <li><a href=ViewRecent>recent pastes</a></li>
                 </ul>
                   <% do mUserId <- getUserId acidAuth acidProfile

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
                                <li>You are logged in with profile <% show $ unUserId uid %></li>
                                -- Debugging
                                -- <li><% show authDump %></li>
                                <li><a href=(U_AuthProfile $ AuthURL A_Logout)>logout</a></li>
                                <li><a href=(U_AuthProfile $ AuthURL A_AddAuth)>add another auth mode to your profile</a></li>
                              </ul>
                   %>
                 <% bdy %>
                </%>

-- This is the baseAppTemplate wrapped so that it can be used in the
-- normal happstack-foundation stuff.
appTemplate :: ( EmbedAsChild CtrlV' headers
               , EmbedAsChild CtrlV' body
               ) =>
               Acid
            -> String   -- ^ page title
            -> headers  -- ^ extra headers to add to \<head\> tag
            -> body     -- ^ contents of \<body\> tag
            -> CtrlV Response
appTemplate acid ttl moreHdrs bdy = liftM toResponse (XMLGenT $ baseAppTemplate acid ttl moreHdrs bdy)

-- | This makes it easy to embed a PasteId in an HSP template
instance EmbedAsChild CtrlV' PasteId where
    asChild (PasteId i) = asChild ('#' : show i)

-- | This makes it easy to embed a timestamp into an HSP
-- template. 'show' provides way too much precision, so something
-- using formatTime would be better.
instance EmbedAsChild CtrlV' UTCTime where
    asChild time = asChild (show time)

------------------------------------------------------------------------------
-- Pages
------------------------------------------------------------------------------

-- | page handler for 'ViewRecent'
viewRecentPage :: Acid -> CtrlV Response
viewRecentPage acid =
    do method GET
       recent <- query (GetRecentPastes 20 0)
       case recent of
         [] -> appTemplate acid "Recent Pastes" () <p>There are no pastes yet.</p>
         _  -> appTemplate acid "Recent Pastes" () $
                <%>
                 <h1>Recent Pastes</h1>
                 <table>
                  <thead>
                   <tr>
                    <th>id</th>
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
           <td><a href=(ViewPaste pasteId)><% title       %></a></td>
           <td><% nickname    %></td>
           <td><% pasted      %></td>
           <td><% show format %></td>
          </tr>

-- | page handler for 'ViewPaste'
viewPastePage :: Acid -> PasteId -> CtrlV Response
viewPastePage acid pid =
    do method GET
       mPaste <- query (GetPasteById pid)
       case mPaste of
         Nothing ->
             do notFound ()
                appTemplate acid "Paste not found." () $
                    <p>Paste <% pid %> could not be found.</p>
         (Just (Paste (PasteMeta{..}) paste)) ->
             do ok ()
                appTemplate acid ("Paste " ++ (show $ unPasteId pid)) () $
                    <div class="paste">
                     <dl class="paste-header">
                      <dt>Paste:</dt><dd><a href=(ViewPaste pid)><% pid %></a></dd>
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
newPastePage :: Acid -> CtrlV Response
newPastePage acid@Acid{..} =
    do here <- whereami
       mUserId <- getUserId acidAuth acidProfile
       case mUserId of
          Nothing ->
            appTemplate acid "Add a Paste" () $
              <%>
                <h1>You Are Not Logged In</h1>
              </%>
          (Just uid) ->
            appTemplate acid "Add a Paste" () $
              <%>
                <h1>Add a paste</h1>
                <% reform (form here) "add" success Nothing pasteForm %>
              </%>
    where
      success :: Paste -> CtrlV Response
      success paste =
          do pid <- update (InsertPaste paste)
             seeOtherURL (ViewPaste pid)

-- | the 'Form' used for entering a new paste
pasteForm :: CtrlVForm Paste
pasteForm =
    (fieldset $
       ul $
          (,,,) <$> (li $ label <span>title</span>  ++> (inputText "" `transformEither` required) <++ errorList)
                <*> (li $ label <span>nick</span>   ++> (inputText "" `transformEither` required) <++ errorList)
                <*> (li $ label <span>format</span> ++> formatForm)
                <*> (li $ label <div>paste</div>    ++> errorList ++> (textarea 80 25 "" `transformEither` required))
                <* inputSubmit "paste!"
    )  `transformEitherM` toPaste
    where
      formatForm =
          select [(a, show a) | a <- [minBound .. maxBound]] (== PlainText)
      toPaste (ttl, nick, fmt, bdy) =
          do now <- liftIO getCurrentTime
             return $ Right $
                        (Paste { pasteMeta = PasteMeta { pasteId  = PasteId 0
                                                       , title    = ttl
                                                       , nickname = nick
                                                       , format   = fmt
                                                       , pasted   = now
                                                       }
                               , paste = bdy
                               })
      required txt
          | Text.null txt = Left "Required"
          | otherwise     = Right txt

------------------------------------------------------------------------------
-- Auth Support Functions
------------------------------------------------------------------------------

-- the key to using happstack-authenticate with HSP is simple. First
-- you need to be able to embed Html in your HSP monad like this:
--
-- I (Robin Lee Powell) don't know how to fix this warning:
--
--    Warning: orphan instance:
--      instance (Functor m, Monad m) => EmbedAsChild (RouteT url m) Html
--
instance (Functor m, Monad m) => EmbedAsChild (RouteT url m) Html where
    asChild html = asChild (HTML.CDATA False (renderHtml html))

-- If you want to use your usual app template, then the auth stuff,
-- which is in RouteT AuthProfileURL, needs to have a way to render
-- your routes inside that context.
-- 
instance (Functor m, Monad m) => EmbedAsAttr (RouteT AuthProfileURL m) (Attr String Route) where
    asAttr (n := u) = do
           asAttr $ HTML.MkAttr (HTML.toName n, HTML.pAttrVal (concatMap (((++) "/") . Text.unpack) (toPathSegments u)))

-- Stick our usual app template into a state where the auth
-- functions, which are Blaze based, are OK with it.
appTemplate' :: (Happstack m, EmbedAsAttr m (Attr String Route), XMLGenerator m, EmbedAsChild m Html, HSX.XMLType m ~ XML) => Acid -> String -> Html -> Html -> m Response
appTemplate' a t h b = liftM toResponse (baseAppTemplate a t h b)

------------------------------------------------------------------------------
-- route
------------------------------------------------------------------------------

-- | the route mapping function
route :: Acid -> Text -> Route -> CtrlV Response
route acid@Acid{..} baseURL url =
    case url of
      -- FIXME: replace the ViewRecent thing here with "go back to
      -- the last page we were on". - rlpowell
      (U_AuthProfile authProfileURL) ->
          do vr <- showURL ViewRecent
             XMLGenT $ nestURL U_AuthProfile $ handleAuthProfile acidAuth acidProfile (appTemplate' acid) Nothing (Just baseURL) vr authProfileURL
      ViewRecent      -> viewRecentPage acid
      (ViewPaste pid) -> viewPastePage acid pid
      NewPaste        -> newPastePage acid
      CSS             -> serveFile (asContentType "text/css") "style.css"

------------------------------------------------------------------------------
-- main
------------------------------------------------------------------------------

-- | start the app. listens on port 8000.
main :: IO ()
main = withAcid Nothing $ \acid -> do
           args <- getArgs
           let appPort = if (length args > 0) then (read (args !! 0) :: Int) else 8000
           let hosturl = if (length args > 1) then (Text.pack ((args !! 1) :: String))   else (Text.pack "http://localhost:8000")
           simpleApp id
            defaultConf { httpConf = nullConf {
                       port      = appPort
                   }
                 }
              (AcidLocal Nothing initialCtrlVState)
              ()
              ViewRecent
              hosturl
              (route acid hosturl)
