module Sound2 where


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
import Sound1


type Program1 = ([(String, Synth)], Program)


runProgram1 :: Program1 -> IO (AudioContext, OpenThreads)
runProgram1 (defs, program) =
  let
    loop [] synthDefs = runProgram program (map (\(n,s) -> (n, Left s)) synthDefs)
    loop ((name, synth):defs') synthDefs = loop defs' ((name, expandSynth synthDefs synth) : synthDefs)
  in
    loop defs []

