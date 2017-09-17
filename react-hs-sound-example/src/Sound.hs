module Sound where


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
import Expr
import ExprParser ()
import Parser ()
import Sound1
import Sound2


example3 :: String
example3 = "let k = 300\nlet sy = \\freq. \\dur. Gain [Osc Sine freq dur] Env 0.0 (1.0 0.01 0.5 * dur 0.1 0.0 * dur 0.8)\nLoop 100 \\x.\n Fork\n  Loop 16 \\i.\n   Play @@sy k 0.12\n   Wait 0.125\n   End\n  End\n Fork\n  Loop 8 \\i.\n   Play @@sy * 2 k 0.5\n   Wait 0.25\n   End\n  End\n Fork \n  Wait 0.1\n  Loop 8 \\i. \n   Set k Random 200 1000 \n   Wait 0.25 \n   End\n  End\n Wait 2 \n End\nEnd"


example4 :: String
example4 = "let k = 300\nlet env = \\dur. Env 0.0 (1.0 0.01 0.5 * dur 0.1 0.0 * dur 0.8)\nlet sy1 = \\freq. \\dur. \n  Gain [Osc Sine freq dur] Env 0.0 (1.0 0.01 0.5 * dur 0.1 0.0 * dur 0.8)\nlet sy2 = \\freq. \\dur. \n  Gain [Filter Lowpass [Osc Saw freq dur] Env freq (* freq 4 * dur 0.2 freq * dur 0.4) 10] @env dur\nLoop 100 \\x.\n Fork\n  Loop 16 \\i.\n   Play @@sy1 k 0.2\n   Wait 0.125\n   End\n  End\n Fork\n  Loop 8 \\i.\n   Play @@sy2 * 2 k 0.5\n   Wait 0.25\n   End\n  End\n Fork \n  Wait 0.1\n  Loop 8 \\i. \n   Set k Random 200 1000 \n   Wait 0.25 \n   End\n  End\n Wait 2 \n End\nEnd"


example5 :: String
example5 = "let env = \\dur. Env 0.0 (1.0 0.01 0.5 * dur 0.1 0.0 * dur 0.8)\nlet env2 = \\freq.\\dur. Env freq (* 4 freq * 0.2 dur freq * 0.8 dur)\nlet sy1 = \\freq. \\dur. \n  Gain [Filter Lowpass [Osc Saw freq dur] @@env2 freq dur 10] @env dur\nlet pat1 = \\freq1. \\freq2. \\dur.\n  Loop 6 \\i.\n    Play @@sy1 freq1 dur\n    Wait dur\n    End\n  Loop 2 \\i.\n    Play @@sy1 freq2 dur\n    Wait dur\n    End\n  End \nlet pat2 = \\freq.\n  Fork @@@pat1 * 2 freq * 3 freq 0.5\n  Loop 2 \\i. \n    @@@pat1 * 4 freq * 6 freq 0.25\n  End\nFork \n  Loop 4 \\z.\n    Loop 2 \\x.\n      @pat2 100\n    Loop 1 \\x.\n      @pat2 80\n    @pat2 75\n  End\nWait 16\nPlay @@sy1 200 12\nWait 16\nPlay @@sy1 200 8\nWait 8\nPlay @@sy1 160 4\nWait 4\nPlay @@sy1 150 4\nWait 4\nPlay @@sy1 200 4\nWait 4\nPlay @@sy1 400 4\nWait 4\nPlay @@sy1 160 4\nWait 4\nPlay @@sy1 150 4\nWait 4\nEnd\n"


example6 :: String
example6 = "let env = \\dur. Env 0.0 (1.0 0.01 0.5 * dur 0.1 0.0 * dur 0.8)\nlet env2 = \\freq.\\dur. Env freq (* 4 freq * 0.2 dur freq * 0.8 dur)\nlet sy1 = \\freq. \\dur. \n  Gain [Filter Lowpass [Osc Saw freq dur] @@env2 freq dur 10] @env dur\nlet pat1 = \\freq1. \\freq2. \\dur.\n  Loop 6 \\i.\n    Play @@sy1 freq1 dur\n    Wait dur\n    End\n  Loop 2 \\i.\n    Play @@sy1 freq2 dur\n    Wait dur\n    End\n  End \nlet pat2 = \\freq.\n  Fork @@@pat1 * 2 freq * 3 freq 0.5\n  Loop 2 \\i. \n    @@@pat1 * 4 freq * 6 freq 0.25\n  End\nLoopInf \\x. # infinite loop \n  Fork \n    Loop 4 \\z.\n      Loop 2 \\x.\n        @pat2 100\n      Loop 1 \\x.\n        @pat2 80\n      @pat2 75\n    End\n  Wait 16\n  Play @@sy1 200 12\n  Wait 16\n  Play @@sy1 200 8\n  Wait 8\n  Play @@sy1 160 4\n  Wait 4\n  Play @@sy1 150 4\n  Wait 4\n  Play @@sy1 200 4\n  Wait 4\n  Play @@sy1 400 4\n  Wait 4\n  Play @@sy1 160 4\n  Wait 4\n  Play @@sy1 150 4\n  Wait 4\n  End\n"