module Timed where

import GHCJS.DOM.JSFFI.AudioContext
import GHCJS.DOM.JSFFI.Generated.AudioNode
import Control.Concurrent


import Synth


commandLatency :: Double
commandLatency = 0.2


data Timed a = Timed { unTimed :: AudioContext -> Double -> OpenThreads -> [(String, Either Synth AudioNode)] -> IO (a, Double) }


instance Functor Timed where
  fmap f (Timed g) = Timed $ \ac t ots env -> do
    (a, t') <- g ac t ots env
    return (f a, t')


instance Applicative Timed where
  pure a = Timed $ \_ac t _ots _env -> return (a, t)
  Timed mf <*> Timed ma = Timed $ \ac t ots env -> do
    (f, t') <- mf ac t ots env
    (a, t'') <- ma ac t' ots env
    return (f a, t'')


instance Monad Timed where
  return a = Timed $ \_ac t _ots _env -> return (a, t)
  Timed m >>= f = Timed $ \ac t ots env -> do
    (a, t') <- m ac t ots env
    unTimed (f a) ac t' ots env


liftTimed :: IO a -> Timed a
liftTimed ioa = Timed $ \_ac time _ots _env -> do
    a <- ioa
    return (a, time)


wait :: Double -> Timed ()
wait t = Timed $ \ac time _ots _env -> do
  let time' = time + t
  currentTime <- getCurrentTime ac
  let delayTime = round ((time' - currentTime) * 1000000)
  if delayTime <= 10000 then
    print delayTime
  else
    threadDelay delayTime
  return ((), time')


playSynth :: Synth -> Timed ()
playSynth synth = Timed $ \ac time _ots env -> do
  node <- mkNode ac env (time + commandLatency) synth
  getDestination ac >>= \dest -> connect node dest Nothing Nothing
  return ((), time)


type OpenThreads = MVar [ThreadId]


killOpenThreads :: OpenThreads -> IO ()
killOpenThreads ots = do
  tids <- readMVar ots
  mapM_ killThread tids


addOpenThread :: OpenThreads -> ThreadId -> IO ()
addOpenThread ots tid =
  modifyMVar ots (\xs -> return (tid : xs, ()))


removeOpenThread :: OpenThreads -> ThreadId -> IO ()
removeOpenThread ots tid =
  modifyMVar ots (\xs -> return (filter ((/=)tid) xs, ()))


type ScheduledFinalizers = MVar [(Double, IO ())]


runScheduledFinalizers :: ScheduledFinalizers -> Double -> IO ()
runScheduledFinalizers mvar time =
  let
    loop ((t, io) : xs) =
      if t >= time then do
        io
        loop xs
      else do
        xs' <- loop xs
        return ((t, io) : xs')
    loop [] =
      return []
  in do
    xs <- takeMVar mvar
    xs' <- loop xs
    putMVar mvar xs'


addScheduledFinalizer :: ScheduledFinalizers -> (Double, IO ()) -> IO ()
addScheduledFinalizer mvar x = do
  xs <- takeMVar mvar
  putMVar mvar (x:xs)

