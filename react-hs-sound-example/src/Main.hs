module Main where

import React.Flux
import React.Flux.Ajax
import SoundViews
import SoundStore
import CommitHash
--import Control.Concurrent


soundAction :: SoundAction -> SomeStoreAction
soundAction a = action @SoundState a

main :: IO ()
main = do
  putStrLn commitHash
  initAjax
  ls <- initialSoundState
  registerInitialStore ls
  reactRenderView "soundapp" soundApp
  return ()
