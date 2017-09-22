module Sound1 where


import React.Flux ()
import GHCJS.DOM.JSFFI.AudioContext
import GHCJS.DOM.JSFFI.Generated.AudioNode
import GHCJS.DOM.JSFFI.Generated.OscillatorNode
import GHCJS.DOM.JSFFI.Generated.GainNode
import GHCJS.DOM.JSFFI.Generated.AudioParam
import GHCJS.DOM.JSFFI.Generated.Enums
import qualified GHCJS.DOM.JSFFI.Generated.BiquadFilterNode as Filt
import Control.Applicative
import Control.Concurrent
import Data.Char as Char
import System.Random as Rand


import Synth
import Timed


beep :: IO ()
beep = do
  ac <- newAudioContext
  osc <- createOscillator ac
  getDestination ac >>= \d -> connect osc d Nothing Nothing
  start osc Nothing
  t <- getCurrentTime ac
  stop osc (Just (t + 1.0))
  return ()


data ADSR = ADSR
  { adsrAttack :: Double
  , adsrDecay :: Double
  , adsrSustain :: Double
  , adsrRelease :: Double
  }
  deriving (Eq, Show)


data Sound1 = Sound1
  { sound1Freq :: Float
  , sound1Amp :: Float
  , sound1ADSR :: ADSR
  }
  deriving (Eq, Show)


defaultSound1a :: Sound1
defaultSound1a = Sound1 300 0.5 (ADSR 0.01 0.1 0.5 1.0)


defaultSound1b :: Sound1
defaultSound1b = Sound1 400 0.5 $ ADSR 0.01 0.1 0.5 1.0


playSound1 :: AudioContext -> Sound1 -> Double -> IO ()
playSound1 audioCtx sound1 t = do
  osc <- createOscillator audioCtx
  gain <- createGain audioCtx
  connect osc gain Nothing Nothing
  getDestination audioCtx >>= \dest -> connect gain dest Nothing Nothing
  freq_param <- getFrequency osc
  setValue freq_param (sound1Freq sound1)
  gain_param <- getGain gain
  let
    amp = sound1Amp sound1
    attack = adsrAttack (sound1ADSR sound1)
    decay = attack + adsrDecay (sound1ADSR sound1)
    sustain = adsrSustain (sound1ADSR sound1)
    release = decay + adsrRelease (sound1ADSR sound1)
  setValueAtTime gain_param 0.0 (realToFrac t)
  linearRampToValueAtTime gain_param amp (realToFrac (t + attack))
  linearRampToValueAtTime gain_param (amp * (realToFrac sustain)) (realToFrac (t + decay))
  linearRampToValueAtTime gain_param 0.0 (realToFrac (t + release))
  start osc (Just t)
  stop osc (Just (t + release))


playS1 :: Float -> Timed ()
playS1 freq = Timed $ \ac time _ots _env sf -> do
  playSound1 ac (defaultSound1a { sound1Freq = freq }) (time + commandLatency)
  return ((), time)

-------------------------------------------------------------------------------
type Program = [Command]


data Command
  = Wait Double
  | Play Float
  | Synth Synth
  | Fork Program
  | Loop Int Program
  deriving (Eq, Show, Read)


runCommand :: Command -> Timed ()
runCommand c =
  case c of
    Wait t -> wait t
    Play freq -> playS1 freq
    Fork cs -> Timed $ \ac t ots env sf -> do
      tid <- forkIO $ do
        _ <- unTimed (mapM_ runCommand cs) ac t ots env sf
        myThreadId >>= removeOpenThread ots
        return ()
      addOpenThread ots tid
      return ((), t)
    Loop i cs ->
      if i < 1 then
        return ()
      else do
        mapM_ runCommand cs
        runCommand (Loop (i-1) cs)
    Synth s ->
      playSynth s


--runProgram :: Program -> [(Name, Synth)] -> IO (AudioContext, OpenThreads)
--runProgram cs synthDefs = do
runProgram :: Program -> [(String, Either Synth AudioNode)] -> IO (AudioContext, OpenThreads)
runProgram cs synthDefs = do
  ac <- newAudioContext
  t <- getCurrentTime ac
  ots <- newMVar []
  sf <- newMVar []
  tid <- forkIO $ do
    (_, _) <- unTimed (mapM_ runCommand cs) ac t ots synthDefs sf
    myThreadId >>= removeOpenThread ots
    return ()
  addOpenThread ots tid
  return (ac, ots)


prog1 :: Program
prog1 =
  [ Loop 2
    [ Loop 4
      [ Play 300
      , Wait 0.5
      , Play 400
      , Wait 0.5
      , Play 500
      , Wait 0.5
      , Play 600
      , Wait 0.5
      ]
    , Loop 4
      [ Play 300
      , Wait 0.25
      ]
    ]
  ]


example1 :: String
example1 = "[ Loop 4\n  [ Loop 2\n    [ Fork\n      [ Play 200\n      , Wait 1.0\n      , Play 300\n      , Wait 0.5\n      , Play 300\n      , Wait 0.5\n      , Play 200\n      , Wait 2.0\n      ]\n    , Fork\n      [ Loop 2\n        [ Play 400\n        , Wait 0.5\n        , Play 600\n        , Wait 0.25\n        , Play 600\n        , Wait 0.5\n        , Play 600\n        , Wait 0.25\n        ]\n      ]\n    , Play 800\n    , Wait 1.0\n    , Play 1200\n    , Wait 1.0\n    , Play 800\n    , Wait 0.5\n    , Play 1200\n    , Wait 1.0\n    , Play 1200\n    , Wait 0.25\n    , Play 1200\n    , Wait 0.25\n    ]\n  , Loop 2\n    [ Fork\n      [ Play 250\n      , Wait 0.5\n      , Play 250\n      , Wait 1.0\n      , Play 250\n      , Wait 0.5\n      , Play 250\n      , Wait 1.0\n      , Play 250\n      , Wait 1.0\n      ]\n    , Play 125\n    , Wait 4.0\n    ]\n  ]\n]"


example2 :: String
example2 = "[ Fork\n  [ Loop 8\n    [ Synth (Gain [Filter Lowpass [Osc Saw 0.4 (ConstParam 300) (Just 1) ] (EnvParam 300 [(1000, 0.25), (500, 0.5)]) (ConstParam 10)] (EnvParam 0.0 [(1.0, 0.01), (0.5, 0.5), (0.0, 0.2)])  )\n    , Wait 1\n    ]\n  ]\n, Loop 4\n  [ Synth (Gain [Filter Lowpass [Osc Saw 0.4 (ConstParam 200) (Just 2) ] (EnvParam 300 [(1000, 0.25), (500, 0.5)]) (ConstParam 10)] (EnvParam 0.0 [(1.0, 0.01), (0.0, 2.0)])  )\n  , Wait 2\n  ]\n]"
