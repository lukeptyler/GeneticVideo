module MakeStamps
    ( makeStamps
    ) where

import           Graphics.Image
import qualified Graphics.Image as I
import           Graphics.Image.Interface hiding (mapM_)

makeStamps :: String -> Int -> IO ()
makeStamps folder count = mapM_ convertToStamp [1..count]
  where
    convertToStamp :: Int -> IO ()
    convertToStamp idx = do
      image <- readImageYA VU $ "res/" ++ folder ++ "/" ++ show idx ++ ".png"
      writeImage ("res/stamps/" ++ show idx ++ ".png") $ normalizeStamp image

normalizeStamp :: Image VU YA Double -> Image VU YA Double
normalizeStamp img = I.map normalizePixel img
  where
    m = getPxC (I.maximum $ I.map dropAlpha img) LumaY

    normalizePixel :: Pixel YA Double -> Pixel YA Double
    normalizePixel pix = setPxC pix LumaYA (getPxC pix LumaYA / m)