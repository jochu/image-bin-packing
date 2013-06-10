{-# LANGUAGE PatternGuards   #-}
{-# LANGUAGE TemplateHaskell #-}
----------------------------------------------------------------------------------------------------
import           Control.Monad              (forM_)
import           Data.List                  (sortBy)
import           Data.Ord                   (comparing)
import           System.Environment         (getArgs)
import           Text.Printf                (printf)
----------------------------------------------------------------------------------------------------
import           Control.Lens               (view, _1)
----------------------------------------------------------------------------------------------------
import           RectBinPacker.BinRotatable (Orientation (Rotated))
import qualified RectBinPacker.BinRotatable as Bin
import           RectBinPacker.Geometry     (Pos (Pos), dimHeight, dimWidth, posX, posY)
import           RectBinPacker.Image        (Image (Image), imageFile, imageOrientation, readImages)


----------------------------------------------------------------------------------------------------
-- | Given a list of images, output the bash commands required to generate the image map
main :: IO ()
main = do
    args <- getArgs
    case args of
      [dir, output] -> do
        content <- getContents

        let images = readImages content
            packed = foldr Bin.insertRotate Bin.empty images
            packedOrdered = sortBy (comparing (view _1)) (Bin.positions packed)

        printf "# SHEET: %d %d\n" (view dimWidth packed) (view dimHeight packed)
        forM_ packedOrdered $ \(pos, img) -> do
          printf "# %s %d %d %s\n"
                 (view imageFile img)
                 (view posX pos)
                 (view posY pos)
                 (show (view imageOrientation img))
          if pos == Pos 0 0
            then printf "convert -gravity NorthWest \
                        \-background transparent \
                        \-extent %dx%d \\( %s %s/%s.png \\) %s\n"
                        (view dimWidth packed)
                        (view dimHeight packed)
                        (rotateCommand img)
                        dir
                        (view imageFile img)
                        output
            else printf "composite -geometry +%d+%d \
                        \ \\( %s %s/%s.png \\) %s %s\n"
                        (view posX pos)
                        (view posY pos)
                        (rotateCommand img)
                        dir
                        (view imageFile img)
                        output
                        output
      _ -> putStrLn "Usage: ./image-bin-packing input-images-dir output-image"
  where
    rotateCommand (Image _ _ Rotated) = "-rotate 90"
    rotateCommand _ = ""
