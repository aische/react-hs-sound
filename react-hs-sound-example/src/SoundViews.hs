{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | The views for the TODO app
module SoundViews where

import React.Flux
--import qualified Data.Set as Set
--import qualified Data.Text as T
--import qualified Data.List as List

import SoundStore
import Sound1
import Sound2
import Sound


-- | The controller view and also the top level of the LAMBDA app.  This controller view registers
-- with the store and will be re-rendered whenever the store changes.
soundApp :: View '[]
soundApp = mkControllerView @'[StoreArg SoundState] "sound app" $ \soundState ->
    div_ $ do
        soundView soundState


soundView :: SoundState -> ReactElementM 'EventHandlerCode ()
soundView ss = section_ ["id" $= "sound"] $ do
    h2_ [ "key" &= ("h1" :: String)] "sound"
    div_ $ do
      button_ [ "key" &= ("bex1" :: String), onClick $ \_ _ -> handleSound (ReadProgram example1) ] $ elemString "example 1"
      button_ [ "key" &= ("bex2" :: String), onClick $ \_ _ -> handleSound (ReadProgram example2) ] $ elemString "example 2"
      button_ [ "key" &= ("bex3" :: String), onClick $ \_ _ -> handleSound (ReadProgram example3) ] $ elemString "example 3"
      button_ [ "key" &= ("bex4" :: String), onClick $ \_ _ -> handleSound (ReadProgram example4) ] $ elemString "example 4"
      button_ [ "key" &= ("bex5" :: String), onClick $ \_ _ -> handleSound (ReadProgram example5) ] $ elemString "example 5"
      button_ [ "key" &= ("bex6" :: String), onClick $ \_ _ -> handleSound (ReadProgram example6) ] $ elemString "example 6"
    div_ $ textarea_
      [ onChange $ \evt -> handleSound (ReadProgram $ target evt "value")
      , style [("width", "100%"), ("height", "200px")]
      , "value" &= currentInputText ss
      ] mempty
    div_ [ "key" &= ("d4" :: String)] $ do
      -- button_ [ "key" &= ("b10" :: String), onClick $ \_ _ -> handleSound (PlaySound1Now defaultSound1a) ] $ elemString "sound1a"
      -- button_ [ "key" &= ("b11" :: String), onClick $ \_ _ -> handleSound (PlaySound1Now defaultSound1b) ] $ elemString "sound1b"
      -- button_ [ "key" &= ("b12" :: String), onClick $ \_ _ -> handleSound (StartSound $ not $ ssIsPlaying ss) ] $ elemString $ if ssIsPlaying ss then "stop" else "start"
      case currentProgram ss of
        Nothing ->
          return ()
        Just _ ->
          button_ [ "key" &= ("b13" :: String), onClick $ \_ _ -> handleSound (PlayProgram) ] $ elemString "play program"
      case currentAudioContext ss of
        Nothing ->
          return ()
        Just _ ->
          button_ [ "key" &= ("b14" :: String), onClick $ \_ _ -> handleSound (StopProgram) ] $ elemString "stop program"
    --button_ [ "key" &= ("b15" :: String), onClick $ \_ _ -> handleSound (Yippie) ] $ elemString "yippie"
