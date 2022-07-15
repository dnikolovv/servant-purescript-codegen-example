module GenTypesDemo.AppM where

import Prelude

import Affjax.RequestBody (RequestBody(..))
import Control.Monad.Reader (ReaderT, ask, asks, runReaderT)
import Data.Argonaut (Json, JsonDecodeError)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Lens ((.~), (^.))
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty.Internal (NonEmptyString(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Effect.Now as Now
import GenTypesDemo.API.Error (APIError(..))
import GenTypesDemo.Capability.Global (GlobalEvent(..), GlobalIO, emitGlobalAction, emitGlobalEvent)
import GenTypesDemo.Capability.Global (class MonadGlobal)
import GenTypesDemo.Capability.Halo (class MonadHalo)
import GenTypesDemo.Capability.Log (class LogMessages)
import GenTypesDemo.Capability.Now (class Now)
import GenTypesDemo.Capability.Routing (class MonadRouting)
import GenTypesDemo.Capability.Users (class MonadUsers)
import GenTypesDemo.Component.Routing (RoutingIO)
import GenTypesDemo.Config as Config
import GenTypesDemo.Data.Log as Log
import Halogen.Subscription as HS
import Network.RemoteData (RemoteData)
import Network.RemoteData as RemoteData
import Pipes.Core (Proxy)
import React.Basic.Hooks as React
import React.Halo (component, hoist) as Halo
import React.Halo (component, hoist, useHalo) as Halo
import Servant.PureScript (class MonadAjax, AjaxError(..), Request, request)
import Servant.PureScript (class MonadAjax, Request, request)
import ServerAPI (Api)
import ServerAPI (Api)
import ServerAPI as Server
import ServerAPI as ServerAPI
import Type.Proxy (Proxy(..))
import URI (Authority(..), Host(..), RelativePart(..))
import URI.Host.RegName as RegName
import URI.Path (Path(..))
import URI.Path.Segment (segmentFromString)
import URI.RelativePart (_path, _relPath)
import URI.RelativeRef (_relPart)

data LogLevel
  = Dev
  | Prod

instance Show LogLevel where
  show Dev = "Dev"
  show Prod = "Prod"

derive instance eqLogLevel :: Eq LogLevel

derive instance ordLogLevel :: Ord LogLevel

type Env
  = { routing :: RoutingIO
    , logLevel :: LogLevel
    , globalIO :: GlobalIO
    }

newtype AppM a
  = AppM (ReaderT Env Aff a)

runAppM :: forall a. Env -> AppM a -> Aff a
runAppM env (AppM act) = runReaderT act env

-- | Halo
instance monadHaloAppM :: MonadHalo AppM where
  component name spec =
    AppM do
      env <- ask
      liftEffect
        $ Halo.component name
            spec { eval = Halo.hoist (runAppM env) <<< spec.eval }

derive newtype instance functorAppM :: Functor AppM

derive newtype instance applyAppM :: Apply AppM

derive newtype instance applicativeAppM :: Applicative AppM

derive newtype instance bindAppM :: Bind AppM

derive newtype instance monadAppM :: Monad AppM

derive newtype instance monadEffectAppM :: MonadEffect AppM

derive newtype instance monadAffAppM :: MonadAff AppM

instance nowAppM :: Now AppM where
  now = liftEffect Now.now
  nowDate = liftEffect Now.nowDate
  nowTime = liftEffect Now.nowTime
  nowDateTime = liftEffect Now.nowDateTime

instance logMessagesAppM :: LogMessages AppM where
  logMessage log = do
    logLevel <- AppM $ asks _.logLevel
    liftEffect case logLevel, Log.reason log of
      Prod, Log.Debug -> pure unit
      _, _ -> Console.log $ Log.message log

instance monadAjaxAppM :: MonadAjax Api AppM where
  request api = AppM <<< request api <<< setAuthority Config.apiUrl

instance monadGlobalAppM :: MonadGlobal AppM where
  emitGlobalAction act = AppM $ do
    listener <- asks (\s -> s.globalIO.actionsListener)
    liftEffect $ HS.notify listener act
  emitGlobalEvent ev = AppM $ do
    listener <- asks (\s -> s.globalIO.eventsListener)
    liftEffect $ HS.notify listener ev
  getGlobalEventsEmitter = AppM $ asks (\s -> s.globalIO.eventsEmitter)
  getGlobalActionsEmitter = AppM $ asks (\s -> s.globalIO.actionsEmitter)

instance monadRoutingAppM :: MonadRouting AppM where
  read = liftEffect =<< (AppM $ asks _.routing.read)
  reload = liftEffect =<< (AppM $ asks _.routing.reload)
  getEmitter = AppM $ asks _.routing.emitter
  navigate route = do
    f <- AppM $ asks _.routing.navigate
    liftEffect $ f route
  redirect route = do
    f <- AppM $ asks _.routing.redirect
    liftEffect $ f route

instance monadUsersAppM :: MonadUsers AppM where
  listUsers = callApi ServerAPI.getUsers
  newUser = callApi <<< ServerAPI.postUser
  deleteUser = callApi <<< ServerAPI.deleteUserByUserId

callApi ::
  forall m result.
  Monad m =>
  MonadGlobal m =>
  m (Either (AjaxError JsonDecodeError Json) result) ->
  m (Either APIError result)
callApi apiCall = do
  apiCall >>= \result ->
    case result of
      Left err -> do
        emitGlobalEvent $ APIErrorOccurred (APIError err)
        pure $ Left (APIError err)
      Right validResponse -> pure $ Right validResponse


setAuthority ::
  forall reqContent resContent decodeError req res.
  String ->
  Request reqContent resContent decodeError req res ->
  Request reqContent resContent decodeError req res
setAuthority apiUrl req =
  let
    rel = req ^. prop (Proxy :: _ "uri")

    auth =
      ( Authority
          Nothing
          (NameAddress (RegName.unsafeFromString $ NonEmptyString apiUrl))
      )

    (currentRelPath :: Array String) =
      fromMaybe []
        $ rel
        ^. _relPart
        <<< _relPath

    newPart =
      RelativePartAuth auth
        $ Path (map segmentFromString currentRelPath)

    updated = rel # _relPart .~ newPart
  in
    req # prop (Proxy :: _ "uri") .~ updated