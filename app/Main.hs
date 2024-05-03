module Main (main) where

import Stamp

import           Graphics.Image
import qualified Graphics.Image as I

main :: IO ()
main = do
  let grad_color = makeImageR VU (200, 200) (\(i, j) -> PixelRGB (fromIntegral i) (fromIntegral j) 0) / 200 :: Image VU RGB Double

  spec1 <- genStampSpec
  spec2 <- genStampSpec
  spec3 <- genStampSpec

  stamp1 <- loadStamp $ spec_stampIndex spec1
  stamp2 <- loadStamp $ spec_stampIndex spec2
  stamp3 <- loadStamp $ spec_stampIndex spec3

  let placed = placeStamp (stamp3,spec3) $ placeStamp (stamp2,spec2) $ placeStamp (stamp1,spec1) grad_color

  displayImage placed
  print $ imageDifference grad_color placed

imageDifference :: Image VU RGB Double -> Image VU RGB Double -> Int
imageDifference img1 img2 = getX $ I.sum $ squashWith2 (\dif a b -> dif + abs (round (a * 255) - round (b * 255))) 0 img1 img2


