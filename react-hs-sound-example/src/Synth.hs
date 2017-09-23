module Synth where

import GHCJS.DOM.Types
import GHCJS.DOM.JSFFI.AudioContext
import GHCJS.DOM.JSFFI.Generated.AudioNode
import GHCJS.DOM.JSFFI.Generated.OscillatorNode
import GHCJS.DOM.JSFFI.Generated.GainNode
import GHCJS.DOM.JSFFI.Generated.AudioParam
import GHCJS.DOM.JSFFI.Generated.Enums
import qualified GHCJS.DOM.JSFFI.Generated.BiquadFilterNode as Filt

import GHCJS.DOM.EventTargetClosures
import GHCJS.DOM.JSFFI.Generated.AudioBuffer hiding (getGain)
import GHCJS.DOM.EventM
import GHCJS.DOM.JSFFI.Generated.ScriptProcessorNode
import GHCJS.DOM.JSFFI.Generated.AudioProcessingEvent
import Control.Monad.Trans

data Param
  = ConstParam Double
  | EnvParam Double [(Double, Double)]
  | VarParam String
  | NodeParam Synth
  deriving (Eq, Show, Read)


data Synth
  = Osc WaveForm Double Param (Maybe Double)
  | Gain [Synth] Param
  | Filter FilterType [Synth] Param Param
  | Let String Synth Synth
  | Var String
  | Dc Double
  | Mult Synth Synth
  | Add Synth Synth
  deriving (Eq, Show, Read)


data WaveForm = Sine | Square | Saw | Tri
  deriving (Eq, Show, Read)


data FilterType
  = Lowpass
  | Highpass
  | Bandpass
  | Lowshelf
  | Highshelf
  | Peaking
  | Notch
  | Allpass
  deriving (Eq, Show, Read)


getWaveFormType :: WaveForm -> OscillatorType
getWaveFormType Sine = OscillatorTypeSine
getWaveFormType Square = OscillatorTypeSquare
getWaveFormType Saw = OscillatorTypeSawtooth
getWaveFormType Tri = OscillatorTypeTriangle


getFilterType :: FilterType -> BiquadFilterType
getFilterType Lowpass = BiquadFilterTypeLowpass
getFilterType Highpass = BiquadFilterTypeHighpass
getFilterType Bandpass = BiquadFilterTypeBandpass
getFilterType Lowshelf = BiquadFilterTypeLowshelf
getFilterType Highshelf = BiquadFilterTypeHighshelf
getFilterType Peaking = BiquadFilterTypePeaking
getFilterType Notch = BiquadFilterTypeNotch
getFilterType Allpass = BiquadFilterTypeAllpass


mkParam :: AudioContext -> [(String, Either Synth AudioNode)] -> Double -> AudioParam -> Param -> IO (Double, [AudioNode])
mkParam ac environment t0 param p =
  case p of
    ConstParam d -> do
      setValue param (realToFrac d)
      return (t0, [])
    EnvParam d ds -> do
      setValueAtTime param (realToFrac d) (realToFrac t0)
      let
        loop t1 [] = return (t1, [])
        loop t1 ((v,t):ds') = do
          linearRampToValueAtTime param (realToFrac v) (realToFrac (t1 + t))
          loop (t1 + t) ds'
      loop t0 ds
    NodeParam synth -> do
      setValue param 0.0
      (node, t1, finalizers) <- mkNode ac environment t0 synth
      connectParam node param Nothing
      return (t1, finalizers)

    VarParam name ->
      case lookup name environment of
        Just (Left synth) -> do
          setValue param 0.0
          (node, t1, finalizers) <- mkNode ac environment t0 synth
          connectParam node param Nothing
          return (t1, finalizers)

        Just (Right node) -> do
          connectParam node param Nothing
          return (t0, [])

        Nothing -> do
          print (name ++ " not found")
          return (t0, [])


mkNode :: AudioContext -> [(String, Either Synth AudioNode)] -> Double -> Synth -> IO (AudioNode, Double, [AudioNode])
mkNode ac environment t0 synth =
  case synth of
    Osc wf amp p mb_dur -> do
      osc <- createOscillator ac
      setType osc (getWaveFormType wf)
      freq_param <- getFrequency osc
      (t1a, fs1a) <- mkParam ac environment t0 freq_param p
      gain <- createGain ac
      gain_param <- getGain gain
      setValue gain_param (realToFrac amp)
      connect osc gain Nothing Nothing
      start osc (Just t0)
      let t1 = t0 + maybe 0 id mb_dur
      case mb_dur of
        Nothing -> return ()
        Just _dur ->
          stop osc (Just t1)
      let node = toAudioNode gain
      return (node, max t1 t1a, node : toAudioNode osc : fs1a)

    Gain inputs p -> do
      nodes <- mapM (mkNode ac environment t0) inputs
      let t1 = maximum $ map (\(_,t,_) -> t) nodes
      gain <- createGain ac
      gain_param <- getGain gain
      (t1a, fs1a) <- mkParam ac environment t0 gain_param p
      mapM_ (\(n,_,_) -> connect n gain Nothing Nothing) nodes
      let node = toAudioNode gain
      return (toAudioNode gain, max t1 t1a, node : (fs1a ++ (nodes >>= (\(_,_,n) -> n))))

    Filter ft inputs freq q -> do
      nodes <- mapM (mkNode ac environment t0) inputs
      let t1 = maximum (map (\(_,t,_)->t) nodes)
      filterNode <- createBiquadFilter ac
      Filt.setType filterNode (getFilterType ft)
      freq_param <- Filt.getFrequency filterNode
      q_param <- Filt.getQ filterNode
      (t1a, fs1a) <- mkParam ac environment t0 freq_param freq
      (t1b, fs1b) <- mkParam ac environment t0 q_param q
      mapM_ (\(n,_,_) -> connect n filterNode Nothing Nothing) nodes
      return (toAudioNode filterNode, max t1 (max t1a t1b), toAudioNode filterNode : (nodes >>= (\(_,_,n)->n)) ++ fs1a ++ fs1b)

    Var name -> do
      case lookup name environment of
        Just (Left synth') ->
          mkNode ac environment t0 synth'

        Just (Right node) ->
          return (node, t0, [])

        Nothing ->
          error (name ++ " undefined")

    Let name synth1 synth2 -> do
      (node1, t1a, fs1a) <- mkNode ac environment t0 synth1
      (node2, t1b, fs1b) <- mkNode ac ((name, Right node1) : environment) t0 synth2
      return (node2, max t1a t1b, node1 : node2 : (fs1a ++ fs1b))

    Dc v -> do
      sp <- createScriptProcessor ac 1024 Nothing Nothing
      on sp audioProcess $ do
        e <- event -- AudioProcessingEvent
        outputBuffer <- getOutputBuffer e -- AudioBuffer
        oarr  <- getChannelData outputBuffer 0
        oarr2  <- getChannelData outputBuffer 1
        --liftIO $ multFloat32Array iarr1 iarr2 oarr
        liftIO $ dcFloat32Array2 v oarr oarr2

      return (toAudioNode sp, t0, [toAudioNode sp])

    Add synth1 synth2 -> do
      synthOp2 ac environment t0 synth1 synth2 addFloat32Array2

    Mult synth1 synth2 -> do
      synthOp2 ac environment t0 synth1 synth2 multFloat32Array2


synthOp2 :: AudioContext -> [(String, Either Synth AudioNode)] -> Double -> Synth -> Synth -> (Float32Array -> Float32Array -> Float32Array -> Float32Array -> IO ()) -> IO (AudioNode, Double, [AudioNode])
synthOp2 ac environment t0 synth1 synth2 func = do
  (node1, t1a, fs1a) <- mkNode ac environment t0 synth1
  (node2, t1b, fs1b) <- mkNode ac environment t0 synth2
  cm <- createChannelMerger ac (Just 2)
  connect node1 cm Nothing (Just 0)
  connect node2 cm Nothing (Just 1)
  sp <- createScriptProcessor ac 1024 Nothing Nothing
  connect cm sp Nothing Nothing
  on sp audioProcess $ do
    e <- event -- AudioProcessingEvent
    inputBuffer <- getInputBuffer e -- AudioBuffer
    outputBuffer <- getOutputBuffer e -- AudioBuffer
    iarr1 <- getChannelData inputBuffer 0
    iarr2 <- getChannelData inputBuffer 1
    oarr  <- getChannelData outputBuffer 0
    oarr2  <- getChannelData outputBuffer 1
    liftIO $ func iarr1 iarr2 oarr oarr2

  return (toAudioNode sp, max t1a t1b, toAudioNode sp : toAudioNode cm : (fs1a ++ fs1b))


expandSynth :: [(String, Synth)] -> Synth -> Synth
expandSynth env synth =
  case synth of
    Osc wf amp freq dur ->
      Osc wf amp (expandParam env freq) dur

    Gain inputs gain ->
      Gain (map (expandSynth env) inputs) (expandParam env gain)

    Filter ft inputs freq q ->
      Filter ft (map (expandSynth env) inputs) (expandParam env freq) (expandParam env q)

    Let name s1 s2 ->
      if elem name (map fst env) then
        Let name (expandSynth env s1) s2
      else
        Let name (expandSynth env s1) (expandSynth env s2)

    Var name ->
      case lookup name env of
        Just s ->
          s
        Nothing ->
          error (name ++ " undefined")

    Mult s1 s2 ->
      Mult (expandSynth env s1) (expandSynth env s2)


expandParam :: [(String, Synth)] -> Param -> Param
expandParam env synth =
  case synth of
    ConstParam d -> ConstParam d
    EnvParam d ds -> EnvParam d ds
    VarParam name ->
      case lookup name env of
        Just s ->
          NodeParam s
        Nothing ->
          error (name ++ " undefined")
    NodeParam s ->
      NodeParam (expandSynth env s)


multFloat32Array :: Float32Array -> Float32Array -> Float32Array -> IO ()
multFloat32Array i1 i2 o =
  js_multFloat32Array (unFloat32Array i1) (unFloat32Array i2) (unFloat32Array o)


multFloat32Array2 :: Float32Array -> Float32Array -> Float32Array -> Float32Array -> IO ()
multFloat32Array2 i1 i2 o1 o2 =
  js_multFloat32Array2 (unFloat32Array i1) (unFloat32Array i2) (unFloat32Array o1) (unFloat32Array o2)


addFloat32Array2 :: Float32Array -> Float32Array -> Float32Array -> Float32Array -> IO ()
addFloat32Array2 i1 i2 o1 o2 =
  js_addFloat32Array2 (unFloat32Array i1) (unFloat32Array i2) (unFloat32Array o1) (unFloat32Array o2)


dcFloat32Array2 :: Double -> Float32Array -> Float32Array -> IO ()
dcFloat32Array2 d o1 o2 =
  js_dcFloat32Array2 d (unFloat32Array o1) (unFloat32Array o2)


foreign import javascript unsafe
  "$1.map(function(i1, ix){ $3[ix] = i1*$2[ix]})"
  js_multFloat32Array :: JSVal -> JSVal -> JSVal -> IO ()


foreign import javascript unsafe
  "$1.map(function(i1, ix){ $3[ix] = i1*$2[ix]; $4[ix] = $3[ix];})"
  js_multFloat32Array2 :: JSVal -> JSVal -> JSVal -> JSVal -> IO ()


foreign import javascript unsafe
  "$1.map(function(i1, ix){ $3[ix] = i1+$2[ix]; $4[ix] = $3[ix];})"
  js_addFloat32Array2 :: JSVal -> JSVal -> JSVal -> JSVal -> IO ()


foreign import javascript unsafe
  "$2.map(function(i1, ix){ $2[ix] = $1; $3[ix] = $2[ix];})"
  js_dcFloat32Array2 :: Double -> JSVal -> JSVal -> IO ()
