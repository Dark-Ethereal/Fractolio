module Lib
    ( someFunc
    ) where

import           Codec.Picture
import           Codec.Picture.Saving (imageToTiff)
import           Codec.Picture.Types
import qualified Data.ByteString.Lazy as B
import           Data.Complex

--import           Graphics.Gloss
--import           Graphics.Gloss.Juicy

width = 3840
whRel = (`div` 16) . (*10)
height = whRel width


mandel :: Complex Double -> Int
mandel = length . takeWhile ((2 >=) . magnitude) . take maxIter . z
  where
    z c = iterate (\x -> x^2 + c) 0
    maxIter = 20

image :: Image PixelRGB8
image = generateImage imfunc (width) (height)
    where
      imfunc x y = mandelbrotColor $ (scale*x') :+ (scale*y')
        where
          x' = fromIntegral $ x - (width `div` 2)
          y' = fromIntegral $ y
      scale = 2 / (fromIntegral $ width)

mandelbrotColor c = PixelRGB8 m m m
  where m = fromIntegral $ ((mandel c) `mod` 2) * (255 - (mandel c * 13))


someFunc = do
  B.writeFile "Fractolio.tiff" $ imageToTiff (ImageRGB8 image)

-- display (InWindow "Mandelbrot" (width,height) (0,0)) black (fromImageRGB8 image)
