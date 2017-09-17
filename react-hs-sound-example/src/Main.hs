module Main where

import React.Flux
import React.Flux.Ajax
import SoundViews
import SoundStore
--import Control.Concurrent


soundAction :: SoundAction -> SomeStoreAction
soundAction a = action @SoundState a

main :: IO ()
main = do
  initAjax
  ls <- initialSoundState
  registerInitialStore ls
  reactRenderView "soundapp" soundApp
  return ()
