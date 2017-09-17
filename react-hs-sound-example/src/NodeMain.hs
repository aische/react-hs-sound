import React.Flux
import SoundViews
import SoundStore
import qualified Data.Text.IO as T

main :: IO ()
main = do
  initialSoundState >>= registerInitialStore
  reactRenderViewToString True soundApp >>= T.putStrLn
