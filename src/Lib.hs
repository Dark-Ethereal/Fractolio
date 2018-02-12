module Lib
    ( someFunc
    ) where

import           Codec.Picture
import           Codec.Picture.Saving (imageToPng)
import           Codec.Picture.Types
import qualified Data.ByteString.Lazy as B
import           Data.Complex
import           Data.Monoid          hiding ((<>))
import           Data.Ratio
import           Data.Semigroup       ((<>))
import           Options.Applicative


data View = View
  { coords     :: (Double, Double)
  , zoom       :: Double
  , resolution :: (Int, Int)
  , iters      :: Int
  }

data MandelResult = MandelResult
  { iterCount :: Ratio Int
  , lastNum   :: Complex Double
  , interSum  :: Double}

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

mandel :: Int -> Complex Double -> MandelResult
mandel maxIter comp = MandelResult
  { iterCount = ic % maxIter
  , lastNum = if ic == maxIter then last zl else zl !! ic
  , interSum = if ic == maxIter then sumLengths zl else sumLengths (f zl)
  }
  where
    iterCount = ic % maxIter
    ic = (length . f) zl
    zl :: [Complex Double]
    zl = z comp

    z :: Complex Double -> [Complex Double]
    z c = take maxIter . iterate (\x -> x^2 + c) $ 0
    f = takeWhile ((2>=) . magnitude)
    sumLengths = getSum . foldMap Sum . fmap magnitude

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
mandelbrotColor i c = PixelRGB8 r g b
  where
    MandelResult ic ln is  = mandel i c
    r = if ic < 1 then fromIntegral . floor $ ic * 255 else 0
    g = floor . (*255) . (/2) . magnitude $ ln
    b = floor . (*255) . (/is) $ (magnitude ln)

opts = info (viewParser <**> helper)
  ( fullDesc
  <> header "Fractolio: a fractal drawing program" )

someFunc :: IO ()
someFunc = writeImage =<< execParser opts

writeImage :: View -> IO ()
writeImage = B.writeFile "Fractolio.png" . imageToPng . ImageRGB8 . mkImage
