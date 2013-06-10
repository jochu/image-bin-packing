{-# LANGUAGE TemplateHaskell #-}

----------------------------------------------------------------------------------------------------
-- | The Geometry module provides types and utilities for basic rectangle geometry.
module RectBinPacker.Geometry where
----------------------------------------------------------------------------------------------------
import           Control.Lens    (Getter, to, view)
import           Control.Lens.TH (makeClassy)
----------------------------------------------------------------------------------------------------


-- * Types


----------------------------------------------------------------------------------------------------
-- | Dimensions on two integer axes
data Dim = Dim
  { _dimWidth  :: !Int -- ^ Width of the dimension
  , _dimHeight :: !Int -- ^ Height of the dimension
  } deriving (Eq, Show)

makeClassy ''Dim


----------------------------------------------------------------------------------------------------
-- | Positions on two integer axes
data Pos = Pos
  { _posX :: !Int -- ^ X position
  , _posY :: !Int -- ^ Y position
  } deriving (Eq, Show, Ord)

makeClassy ''Pos


----------------------------------------------------------------------------------------------------
-- | Rectangle built from a position and dimension
data Rect = Rect
  { _rectPos :: Pos -- ^ Position
  , _rectDim :: Dim -- ^ Dimension
  } deriving (Eq, Show)

makeClassy ''Rect


----------------------------------------------------------------------------------------------------
instance HasPos Rect where
  pos = rectPos


----------------------------------------------------------------------------------------------------
instance HasDim Rect where
  dim = rectDim


-- * Dimension Utlities

----------------------------------------------------------------------------------------------------
-- | Whether the first dimension fits within the second dimension
--
-- >>> Dim 1 2 `fits` Dim 2 4
-- True
--
-- >>> Dim 1 4 `fits` Dim 2 2
-- False
fits :: (HasDim a, HasDim b) => a -> b -> Bool
fits smaller bigger =
    view dimWidth smaller <= view dimWidth bigger &&
    view dimHeight smaller <= view dimHeight bigger


----------------------------------------------------------------------------------------------------
-- | Return the area for a given dimension
dimArea :: HasDim a => a -> Int
dimArea dim = view dimWidth dim * view dimHeight dim


----------------------------------------------------------------------------------------------------
-- | A lens that finds the area of a dimension
area :: HasDim a => Getter a Int
area = to dimArea


----------------------------------------------------------------------------------------------------
-- | Finds the length of the longest side
dimMaxExtent :: HasDim a => a -> Int
dimMaxExtent dim = max (view dimWidth dim) (view dimHeight dim)


----------------------------------------------------------------------------------------------------
-- | A lens for the longest side of a dimension
maxExtent :: HasDim a => Getter a Int
maxExtent = to dimMaxExtent
