module SoundStore where


import React.Flux
import Control.Concurrent
import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import GHCJS.DOM.JSFFI.AudioContext
import Sound
import Timed
import Expr
import ExprParser
import Sound1
import Sound2


dispatchSound :: SoundAction -> [SomeStoreAction]
dispatchSound a = [action @SoundState a]


handleSound :: SoundAction -> ([SomeStoreAction], [EventModification])
handleSound = simpleHandler . dispatchSound


newtype WrappedAudioContext = WrappedAudioContext (AudioContext, OpenThreads)


instance Show WrappedAudioContext where
  showsPrec _ _ = showString "<audiocontext>"


instance Eq WrappedAudioContext where
  _ == _ = True


data SoundState
  = SoundState
  { currentInputText :: String
  , currentProgram :: Maybe (Either Program (Either Program1 Expr))
  , currentAudioContext :: Maybe WrappedAudioContext
  }
  deriving (Eq, Show, Typeable)


initialSoundState :: IO SoundState
initialSoundState = do
  return SoundState
    { currentInputText = ""
    , currentProgram = Nothing
    , currentAudioContext = Nothing
    }


data SoundAction
  = PlayProgram
  | StopProgram
  | ReadProgram String
  deriving (Show, Typeable, Generic)


instance StoreData SoundState where
    type StoreAction SoundState = SoundAction
    transform = runSoundAction


runSoundAction :: SoundAction -> SoundState -> IO SoundState
runSoundAction a oldState =
  case a of

    PlayProgram -> do
      case currentAudioContext oldState of
        Nothing -> return ()
        Just (WrappedAudioContext (ac, tid)) -> do
          _ <- forkIO $ do
            killOpenThreads tid
            threadDelay 100000
            close ac
            return ()
          return ()
      case currentProgram oldState of
        Nothing ->
          return oldState { currentAudioContext = Nothing }
        Just p -> do
          case p of
            Left p0 -> do
              (ac, tid) <- runProgram p0 []
              return oldState { currentAudioContext = Just $ WrappedAudioContext (ac, tid) }
            Right (Left p1) -> do
              (ac, tid) <- runProgram1 p1
              return oldState { currentAudioContext = Just $ WrappedAudioContext (ac, tid) }

            Right (Right p1) -> do
              (ac, tid) <- runExpr p1
              return oldState { currentAudioContext = Just $ WrappedAudioContext (ac, tid) }

    StopProgram -> do
      case currentAudioContext oldState of
        Nothing -> return ()
        Just (WrappedAudioContext (ac, tid)) -> do
          _ <- forkIO $ do
            killOpenThreads tid
            threadDelay 100000
            close ac
            return ()
          return ()

      return oldState { currentAudioContext = Nothing }

    ReadProgram s ->
      case reads s of
        [(p, "")] ->
          return oldState { currentProgram = Just (Left p), currentInputText = s }
        _ -> case reads s of
          [(p1, "")] ->
            return oldState { currentProgram = Just (Right (Left p1)), currentInputText = s }
          _ -> case parseExpr s of
            Just prog ->
              return oldState { currentProgram = Just (Right (Right prog)), currentInputText = s }
            _ ->
              return oldState { currentProgram = Nothing, currentInputText = s }

