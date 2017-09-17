# react-hs-sound-example

compile: 

    cd react-hs-sound-example/
    npm install
    stack build
    make
    
open html/sound.html

[live demo](https://dvde.biz/ghcjs-audio/)

#### about


This project is about using Web Audio API with GHCJS. 
It is inspired by SuperCollider and based on the idea of interpreting a simple programming language in the browser that creates audio nodes and makes sound.

Like most of my projects, this is a proof of concept and an experiment to explore some ideas. It is far away from being usable for real music production: The language is very simple and un-typed, and there are only a few basic "unit generators" to build synthesizers (just oscillators, filters, and envelopes - no addition or multiplication of audio signals, no sample buffers, no constant audio signal (DC) etc).

The interesting part of this project was to get the timing of the audio events correct. With GHCJS it is possible to use multi threading in the browser which makes it easy to fork threads and to use threadDelay to implement a "wait" command. For playing the sounds, the internal clock of the Web Audio API is used, but there is a "Timed" monad that keeps track of the current time, so the sound events are written to the audio context "just in time". 

	data Timed a = Timed { unTimed :: AudioContext 
	                               -> Double                             -- current time
	                               -> OpenThreads 
	                               -> [(String, Either Synth AudioNode)] 
	                               -> IO (a, Double)                     -- result and new current time
	                     }

	wait :: Double -> Timed ()
	wait t = Timed $ \ac time _ots _env -> do
	  let time' = time + t                                      -- compute new time
	  currentTime <- getCurrentTime ac                          -- get audio-clock time
	  let delayTime = round ((time' - currentTime) * 1000000)   -- compute thread delay time
	  if delayTime <= 10000 then
	    print delayTime
	  else
	    threadDelay delayTime
	  return ((), time')

	commandLatency :: Double
	commandLatency = 0.2

	playSynth :: Synth -> Timed ()
	playSynth synth = Timed $ \ac time _ots env -> do
	  node <- mkNode ac env (time + commandLatency) synth
	  getDestination ac >>= \dest -> connect node dest Nothing Nothing
	  return ((), time)



The "Fork" command is interpreted like this:

	applyStatement :: Environment -> Statement -> Timed Value
	applyStatement environment value =
	  case value of

	  	...

	    ForkStatement e enext -> do
	      Timed $ \ac t ots env -> do
	        tid <- forkIO $ do
	          _ <- unTimed (eval environment e >>= evalStatement) ac t ots env
	          myThreadId >>= removeOpenThread ots
	          return ()
	        addOpenThread ots tid
	        return (ActionValue, t)
	      eval environment enext >>= evalStatement

	    ...



The data structure and interpreter for synthesizers looks like this:

	data Param
	  = ConstParam Double
	  | EnvParam Double [(Double, Double)]
	  | VarParam String
	  | NodeParam Synth
	  deriving (Eq, Show, Read)


	data Synth
	  = Osc WaveForm Double Param (Maybe Double)
	  | Gain [Synth] Param
	  | Dc Param (Maybe Double)
	  | Filter FilterType [Synth] Param Param
	  | Let String Synth Synth
	  | Var String
	  deriving (Eq, Show, Read)


	mkNode :: AudioContext -> [(String, Either Synth AudioNode)] -> Double -> Synth -> IO AudioNode
	mkNode ac environment t0 synth =
	  case synth of

	  	...

	    Osc wf amp p mb_dur -> do
	      osc <- createOscillator ac
	      setType osc (getWaveFormType wf)
	      freq_param <- getFrequency osc
	      mkParam ac environment t0 freq_param p
	      gain <- createGain ac
	      gain_param <- getGain gain
	      setValue gain_param (realToFrac amp)
	      connect osc gain Nothing Nothing
	      start osc (Just t0)
	      case mb_dur of
	        Nothing -> return ()
	        Just dur ->
	          stop osc (Just (t0 + dur))
	      return $ toAudioNode gain

	    ...

To implement arbitrary "BinOps" like addition or multiplication it would be necessary to use the ScriptProcessorNode (which seems to be deprecated). So far I did not figure out how to do that in Haskell. 




#### example 6:

This example uses an infinite loop to play the same patterns over and over. That would not be possible if all the audio events were written to the audio context at once when the program starts. 

    let env = \dur. Env 0.0 (1.0 0.01 0.5 * dur 0.1 0.0 * dur 0.8)
    let env2 = \freq.\dur. Env freq (* 4 freq * 0.2 dur freq * 0.8 dur)
    let sy1 = \freq. \dur. 
      Gain [Filter Lowpass [Osc Saw freq dur] @@env2 freq dur 10] @env dur
    let pat1 = \freq1. \freq2. \dur.
      Loop 6 \i.
        Play @@sy1 freq1 dur
        Wait dur
        End
      Loop 2 \i.
        Play @@sy1 freq2 dur
        Wait dur
        End
      End 
    let pat2 = \freq.
      Fork @@@pat1 * 2 freq * 3 freq 0.5
      Loop 2 \i. 
        @@@pat1 * 4 freq * 6 freq 0.25
      End
    LoopInf \x. # infinite loop 
      Fork 
        Loop 4 \z.
          Loop 2 \x.
            @pat2 100
          Loop 1 \x.
            @pat2 80
          @pat2 75
        End
      Wait 16
      Play @@sy1 200 12
      Wait 16
      Play @@sy1 200 8
      Wait 8
      Play @@sy1 160 4
      Wait 4
      Play @@sy1 150 4
      Wait 4
      Play @@sy1 200 4
      Wait 4
      Play @@sy1 400 4
      Wait 4
      Play @@sy1 160 4
      Wait 4
      Play @@sy1 150 4
      Wait 4
      End

I played this example in Firefox 55.0.3 (OSX) for longer than one hour and there were no glitches. In Chrome I got distortions from time to time. 
