{-# LANGUAGE RecordWildCards #-}
module Stamp where

import System.Random
import Graphics.Image
import Graphics.Image.Interface

data StampSpec = StampSpec
  { spec_stampIndex :: Int
  , spec_tint       :: Pixel RGB Double 
  , spec_alpha      :: Double
  , spec_pos        :: (Double,Double) -- (row %,col %)
  , spec_angle      :: Double -- [0,2pi]
  , spec_scale      :: Double
  }

genStampSpec :: IO StampSpec
genStampSpec = StampSpec <$>
  randomRIO (1,1816) <*>
  genRGB <*>
  randomIO <*>
  randomIO <*>
  randomRIO (0, 2*pi) <*>
  randomRIO (0, 2)
  where
    genRGB :: IO (Pixel RGB Double)
    genRGB = PixelRGB <$> randomIO <*> randomIO <*> randomIO

data Stamp = Image {stampImage :: Image VU YA Double, stampHeight, stampWidth :: Double}

loadStamp :: Int -> IO Stamp
loadStamp idx = do
  image <- readImageYA VU $ "res/stamps/" ++ show idx ++ ".png"
  pure $ Image image (fromIntegral $ rows image) (fromIntegral $ cols image)

-- Place stamp modified by stamp spec on image
placeStamp :: (Stamp, StampSpec) -> Image VU RGB Double -> Image VU RGB Double
placeStamp (Image{..}, StampSpec{..}) bkgImage = imap placePixel bkgImage
  where
    (bkgHeight, bkgWidth) = dims bkgImage
    sinTheta = sin spec_angle
    cosTheta = cos spec_angle

    -- convert from image space to stamp space
    -- undo position, scale, rotation, center offset
    toStampSpace :: (Int,Int) -> (Double,Double)
    toStampSpace (r,c) = (sc * sinTheta + sr * cosTheta + stampHeight / 2.0, sc * cosTheta - sr * sinTheta + stampWidth / 2.0)
      where
        sr = (fromIntegral r - fst spec_pos * fromIntegral bkgHeight) / spec_scale
        sc = (fromIntegral c - snd spec_pos * fromIntegral bkgWidth) / spec_scale

    -- apply stamp spec tint and alpha to stamp pixel, premuliplying alpha
    applyColor :: Pixel YA Double -> Pixel RGBA Double
    applyColor (PixelYA pLuma pAlpha) = addAlpha alpha' $ liftPx (* (pLuma * alpha')) spec_tint
      where
        alpha' = spec_alpha * pAlpha

    -- calculate final pixel, mixing stamp with background if needed
    placePixel :: (Int,Int) -> Pixel RGB Double -> Pixel RGB Double
    placePixel pos bkgP = liftPx (* (1 - getAlpha stampP)) bkgP + dropAlpha stampP
      where
        (sr,sc) = toStampSpace pos
        stampP 
          | sr < 0 || sr >= stampHeight || sc < 0 || sc >= stampWidth = 0
          | otherwise = interpolate Bilinear (Fill 0) (floor stampHeight,floor stampWidth) (applyColor . index stampImage) (sr,sc)
