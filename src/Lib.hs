module Lib
    ( someFunc
    ) where

import           Codec.Picture
import           Codec.Picture.Saving (imageToPng)
import           Codec.Picture.Types
import qualified Data.ByteString.Lazy as B
import           Data.Complex
import           Data.Semigroup       ((<>))
import           Options.Applicative

data View = View
  { coords     :: (Double, Double)
  , zoom       :: Double
  , resolution :: (Int, Int)
  , iters      :: Int
  }

viewParser :: Parser View
viewParser = View
  <$> option auto
      ( short 'c'
      <> metavar "COORDS"
      <> help "Set the center coordinates"
      <> value (0,0)
      <> showDefault
      )
  <*> option auto
      ( short 'z'
      <> metavar "FACTOR"
      <> help "Set the zoom factor"
      <> value 0.5
      <> showDefault
      )
  <*> argument auto (metavar "RESOLUTION")
  <*> option auto
      ( short 'i'
      <> metavar "ITERATIONS"
      <> help "Set the number of iterations"
      <> value 64
      )

mandel :: Int -> Complex Double -> Int
mandel maxIter = length . takeWhile ((2 >=) . magnitude) . take maxIter . z
  where
    z c = iterate (\x -> x^2 + c) 0

mkImage :: View -> Image PixelRGB8
mkImage v = uncurry (generateImage (imfunc v)) $ resolution v

imfunc :: View -> Int -> Int -> PixelRGB8
imfunc v x y = mandelbrotColor (iters v) (pixToComplex v x y)

pixToComplex :: View -> Int -> Int -> Complex Double
pixToComplex (View (cx, cy) z (rx,ry) _) x y = nx :+ ny
  where
    nx = subtract cx . (* scaler) . subtract ((fromIntegral rx)/2) . fromIntegral $ x
    ny = subtract cy . (* scaler) . subtract ((fromIntegral ry)/2) . fromIntegral $ y
    scaler = (2/(fromIntegral ry))*2.5*z

mandelbrotColor ::Int -> Complex Double -> PixelRGB8
mandelbrotColor i c = PixelRGB8 m m m
  where m = if mandel i c < i then fromIntegral $ (mandel i c) * 255 `div` i else 0

opts = info (viewParser <**> helper)
  ( fullDesc
  <> header "Fractolio: a fractal drawing program" )

someFunc :: IO ()
someFunc = writeImage =<< execParser opts

writeImage :: View -> IO ()
writeImage = B.writeFile "Fractolio.png" . imageToPng . ImageRGB8 . mkImage
