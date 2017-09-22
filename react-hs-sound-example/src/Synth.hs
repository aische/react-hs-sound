module Synth where

import GHCJS.DOM.Types
import GHCJS.DOM.JSFFI.AudioContext
import GHCJS.DOM.JSFFI.Generated.AudioNode
import GHCJS.DOM.JSFFI.Generated.OscillatorNode
import GHCJS.DOM.JSFFI.Generated.GainNode
import GHCJS.DOM.JSFFI.Generated.AudioParam
import GHCJS.DOM.JSFFI.Generated.Enums
import qualified GHCJS.DOM.JSFFI.Generated.BiquadFilterNode as Filt
import GHCJS.DOM.JSFFI.Generated.ScriptProcessorNode
import GHCJS.DOM.JSFFI.Generated.EventTarget
import GHCJS.DOM.JSFFI.Generated.AudioProcessingEvent
import GHCJS.DOM.JSFFI.Generated.Event

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


mkParam :: AudioContext -> [(String, Either Synth AudioNode)] -> Double -> AudioParam -> Param -> IO Double
mkParam ac environment t0 param p =
  case p of
    ConstParam d -> do
      setValue param (realToFrac d)
      return t0
    EnvParam d ds -> do
      setValueAtTime param (realToFrac d) (realToFrac t0)
      let
        loop t1 [] = return t1
        loop t1 ((v,t):ds') = do
          linearRampToValueAtTime param (realToFrac v) (realToFrac (t1 + t))
          loop (t1 + t) ds'
      loop t0 ds
    NodeParam synth -> do
      setValue param 0.0
      (node, t1) <- mkNode ac environment t0 synth
      connectParam node param Nothing
      return t1

    VarParam name ->
      case lookup name environment of
        Just (Left synth) -> do
          setValue param 0.0
          (node, t1) <- mkNode ac environment t0 synth
          connectParam node param Nothing
          return t1

        Just (Right node) -> do
          connectParam node param Nothing
          return t0

        Nothing -> do
          print (name ++ " not found")
          return t0


mkNode :: AudioContext -> [(String, Either Synth AudioNode)] -> Double -> Synth -> IO (AudioNode, Double)
mkNode ac environment t0 synth =
  case synth of
    Osc wf amp p mb_dur -> do
      osc <- createOscillator ac
      setType osc (getWaveFormType wf)
      freq_param <- getFrequency osc
      t1a <- mkParam ac environment t0 freq_param p
      gain <- createGain ac
      gain_param <- getGain gain
      setValue gain_param (realToFrac amp)
      connect osc gain Nothing Nothing
      start osc (Just t0)
      let t1 = t0 + maybe 0 id mb_dur
      case mb_dur of
        Nothing -> return ()
        Just dur ->
          stop osc (Just t1)
      return (toAudioNode gain, max t1 t1a)

    Gain inputs p -> do
      nodes <- mapM (mkNode ac environment t0) inputs
      let t1 = maximum $ map snd nodes
      gain <- createGain ac
      gain_param <- getGain gain
      t1a <- mkParam ac environment t0 gain_param p
      mapM_ (\(n,_) -> connect n gain Nothing Nothing) nodes
      return (toAudioNode gain, max t1 t1a)

    Filter ft inputs freq q -> do
      nodes <- mapM (mkNode ac environment t0) inputs
      let t1 = maximum (map snd nodes)
      filterNode <- createBiquadFilter ac
      Filt.setType filterNode (getFilterType ft)
      freq_param <- Filt.getFrequency filterNode
      q_param <- Filt.getQ filterNode
      t1a <- mkParam ac environment t0 freq_param freq
      t1b <- mkParam ac environment t0 q_param q
      mapM_ (\(n,_) -> connect n filterNode Nothing Nothing) nodes
      return (toAudioNode filterNode, max t1 (max t1a t1b))

    Var name -> do
      case lookup name environment of
        Just (Left synth') ->
          mkNode ac environment t0 synth'

        Just (Right node) ->
          return (node, t0)

        Nothing ->
          error (name ++ " undefined")

    Let name synth1 synth2 -> do
      (node1, t1a) <- mkNode ac environment t0 synth1
      (node2, t1b) <- mkNode ac ((name, Right node1) : environment) t0 synth2
      return (node2, max t1a t1b)


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

createFloat32Array :: Float -> IO Float32Array
createFloat32Array f = js_createFloat32Array f

foreign import javascript unsafe
  "Float32Array.from([$1])"
  js_createFloat32Array      :: Float -> IO Float32Array

--  "((function(x){ var arr = new Float32Array(1); arr[0] = x; return arr; })($1))"
