module Lib
    ( someFunc
    ) where

import           Codec.Picture
import           Codec.Picture.Saving (imageToPng)
import           Codec.Picture.Types
import           Control.Monad        (liftM2)
import qualified Data.ByteString.Lazy as B
import           Data.Complex
import           Data.Fixed           (mod')
import           Data.Semigroup       ((<>))
import           Options.Applicative


data View = View
  { coords     :: (Double, Double)
  , zoom       :: Double
  , resolution :: (Int, Int)
  , iters      :: Int
  }

type Iters = Int

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
      <> value 1
      <> showDefault
      )
  <*> argument auto (metavar "RESOLUTION")
  <*> option auto
      ( short 'i'
      <> metavar "ITERATIONS"
      <> help "Set the number of iterations"
      <> value 64
      )

mandel :: RealFloat a =>
     Iters -- maximum iters
  -> Complex a -- c value
  -> Iters -- iterations taken to escape
mandel maxIter = length . cullEscaped . take maxIter . mPath
  where
    mandelFunc :: RealFloat a => Complex a -> Complex a -> Complex a
    mandelFunc c = (+c) . (^2)

    mPath :: RealFloat a => Complex a -> [Complex a]
    mPath = (`iterate` 0) . mandelFunc

    cullEscaped = takeWhile ((2 >=) . magnitude)

mkImage :: View -> Image PixelRGB8
mkImage v = uncurry (generateImage (imfunc v)) $ resolution v

imfunc :: View -> Int -> Int -> PixelRGB8
imfunc v x y = mandelbrotColor (iters v) (pixelToComplex v x y)

pixelToComplex :: View -> Int -> Int -> Complex Double
pixelToComplex (View (cx, cy) z (rx,ry) _) x y = real :+ complex
  where
    real = subtract cx . scale . subtract ((fromIntegral rx)/2) . fromIntegral $ x
    complex = subtract cy . scale . subtract ((fromIntegral ry)/2) . fromIntegral $ y
    scale = (/ (fromIntegral ry * z)) . (2.5 *)

mandelbrotColor :: RealFloat a =>
     Iters -- maximum iterations
  -> Complex a -- initial c value
  -> PixelRGB8 -- output color
mandelbrotColor = liftM2 (.) iterColor mandel

iterColor ::
     Iters -- maximum iterations
  -> Iters -- iterations taken
  -> PixelRGB8 -- output color
iterColor m i
  | m <= i    = PixelRGB8 0 0 0
  | otherwise = PixelRGB8 r g b
  where
    a :: Double
    a = fromIntegral i / fromIntegral m
    r = (h . f) a
    g = h . f $ a + 1/3
    b = h . f $ a + 2/3

    f :: Double -> Double
    f i
      | mi >= 5/6 = 1
      | mi >= 2/3 = (i - 2/3) * 6
      | mi >= 2/3 = 0
      | mi >= 1/6 = 1 - ((i - 1/6) * 6)
      | otherwise = 1
      where mi = mod' i 1

    h = round . (*255)




opts = info (viewParser <**> helper)
  ( fullDesc
  <> header "Fractolio: a fractal drawing program" )

someFunc :: IO ()
someFunc = writeImage =<< execParser opts

writeImage :: View -> IO ()
writeImage = B.writeFile "Fractolio.png" . imageToPng . ImageRGB8 . mkImage
