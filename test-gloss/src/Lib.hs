module Lib
    ( someFunc
    ) where
import Graphics.Gloss.Data.Picture

testp :: Picture
testp = Bitmap width height bitmap False
  where width = 200
        height = 100
        bitmap = error "To Be Done!"

-- Perhaps later: check out https://hackage.haskell.org/package/gloss-raster-accelerate-1.9.0.0/docs/src/Graphics-Gloss-Accelerate-Raster-Array.html



someFunc :: IO ()
someFunc = putStrLn "someFunc"
