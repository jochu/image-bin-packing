{-# LANGUAGE TemplateHaskell #-}
----------------------------------------------------------------------------------------------------
-- | This module provides a representation of Images that is avaiable for use within the rotatable
-- rectangle bin packing
module RectBinPacker.Image where
----------------------------------------------------------------------------------------------------
import           Data.Char                  (isDigit)
----------------------------------------------------------------------------------------------------
import           Control.Lens               (view)
import           Control.Lens.TH            (makeLenses)
----------------------------------------------------------------------------------------------------
import           RectBinPacker.BinRotatable (Orientation (..), Rotatable (..))
import           RectBinPacker.Geometry     (Dim (Dim), HasDim (..))

-- * Image Type

----------------------------------------------------------------------------------------------------
data Image = Image
  { _imageFile        :: FilePath    -- ^ File path of the image
  , _imageDim         :: Dim         -- ^ Dimensions of the file
  , _imageOrientation :: Orientation -- ^ Current orientation of the image
  } deriving (Eq, Show)


-- ** Lenses

makeLenses ''Image


----------------------------------------------------------------------------------------------------
instance HasDim Image where
  dim = imageDim


----------------------------------------------------------------------------------------------------
instance Rotatable Image where
  rotate (Image fp d o) = Image fp (rotate d) (rotate o)


-- * Query

-- ** Orientation


----------------------------------------------------------------------------------------------------
-- | Rotates an image into portrait view. It does nothing if it already is.
orientTall :: Image -> Image
orientTall img
    | view dimWidth img > view dimHeight img = rotate img
    | otherwise = img


----------------------------------------------------------------------------------------------------
-- | Rotates an image into landscape view. It does nothing if it already is.
orientWide :: Image -> Image
orientWide img
    | view dimWidth img < view dimHeight img = rotate img
    | otherwise = img


----------------------------------------------------------------------------------------------------
-- | Rotates an image into it's original rotation
upright :: Image -> Image
upright img
    | view imageOrientation img /= Upright = rotate img
    | otherwise = img


-- * Read


----------------------------------------------------------------------------------------------------
-- | Reads images from a string in the following format:
--
-- @
-- image-name width height
-- image-name width height
-- ...
-- @
--
-- Reading will stop at the first invalid entry
readImages :: String -> [Image]
readImages = toImages . words
  where
    toImages (name:width:height:rest)
      | all isDigit width && all isDigit height =
          Image name (Dim (read width) (read height)) Upright : toImages rest
    toImages _ = []
