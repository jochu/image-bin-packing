-- | This module is an extension of the "RectBinPacker.Bin" module and provides an interface that
-- allows you to insert a rotatable object into a 'Bin'. The orientation of the object will be
-- automatically determined to be the best fit.
----------------------------------------------------------------------------------------------------
module RectBinPacker.BinRotatable
  ( module RectBinPacker.Bin
  -- * Orientation Type
  , Orientation(..)
  , Rotatable(..)
  -- * Insertion
  , insertRotate

  ) where
----------------------------------------------------------------------------------------------------
import Control.Lens (view)
----------------------------------------------------------------------------------------------------
import RectBinPacker.Bin
import RectBinPacker.Geometry

-- * Orientation Type

----------------------------------------------------------------------------------------------------
data Orientation = Upright | Rotated
  deriving (Eq, Show)


----------------------------------------------------------------------------------------------------
-- | Represents a type that has an 'Orientation' that can rotated.
class Rotatable a where
  rotate :: a -> a


----------------------------------------------------------------------------------------------------
instance Rotatable Orientation where
  rotate Upright = Rotated
  rotate Rotated = Upright


----------------------------------------------------------------------------------------------------
instance Rotatable Dim where
  rotate (Dim w h) = Dim h w


-- * Insertion

----------------------------------------------------------------------------------------------------
-- | Insert an object into a tree. It is inserted in both the upright and rotated position. The
-- smaller tree is kept.
insertRotate :: (HasDim a, Rotatable a) => a -> Bin a -> Bin a
insertRotate val tree
    | view maxExtent normal > view maxExtent rotated = rotated
    | otherwise = normal
  where
    normal = insert val tree
    rotated = insert (rotate val) tree
