{-# LANGUAGE PatternGuards   #-}
{-# LANGUAGE TemplateHaskell #-}
----------------------------------------------------------------------------------------------------
-- | This module provides a data structure which implements a rectangular bin packing algorithm.
module RectBinPacker.Bin where
import           Control.Applicative    (pure, (<$>), (<*>), (<|>))
import           Control.Lens           (over, set, view, _Just)
import           Control.Lens.TH        (makeLenses)
import           Data.Maybe             (isJust)
----------------------------------------------------------------------------------------------------
import           RectBinPacker.Geometry


-- * Split and Bin Type

----------------------------------------------------------------------------------------------------
-- | A 'Split' represents a split in a rectangle. Each split contains a value in a corner and two
-- 'Bin' children representing the remaining area within the rectangle.
--
-- An example of a split looks like:
--
-- @
-- ,-------.------.
-- | Value | Left |
-- |-------+------|
-- |     Right    |
-- `--------------'
-- @
data Split a = Split
  { _splitVal   :: a       -- ^ Value for the split which is occupying a corner in the split
  , _splitLeft  :: Bin a  -- ^ Bin representing the one side of the split
  , _splitRight :: Bin a  -- ^ Bin representing the the remaining side of the split
  } deriving (Eq, Show)


----------------------------------------------------------------------------------------------------
-- | A 'Bin' represents a rectangle which may be empty or splitted into a 'Split'.
data Bin a = Bin
  { _treeRect  :: Rect
  , _treeSplit :: Maybe (Split a)
  } deriving (Eq, Show)


-- ** Lenses

makeLenses ''Split
makeLenses ''Bin


----------------------------------------------------------------------------------------------------
instance HasRect (Bin a) where
  rect = treeRect


----------------------------------------------------------------------------------------------------
instance HasPos (Bin a) where
  pos = treeRect . pos


----------------------------------------------------------------------------------------------------
instance HasDim (Bin a) where
  dim = treeRect . dim

-- * Construction

----------------------------------------------------------------------------------------------------
-- | An empty tree
empty :: Bin a
empty = Bin (Rect (Pos 0 0) (Dim 0 0)) Nothing

-- ** Update

----------------------------------------------------------------------------------------------------
-- | Given a function `f` and 'Bin', map over all 'Bin's and sub-'Bin's and apply the function f
-- to the 'Rect'
mapRect :: (Rect -> Rect) -> Bin a -> Bin a
mapRect f = over rect f
            . over (treeSplit . _Just . splitLeft) (mapRect f)
            . over (treeSplit . _Just . splitRight) (mapRect f)

-- ** Insertion

----------------------------------------------------------------------------------------------------
-- | Attempts to insert an object into a tree. If the object does not fit, it returns 'Nothing'.
insertWithoutGrowth :: HasDim a => a -> Bin a -> Maybe (Bin a)
insertWithoutGrowth val tree
    | Just split <- view treeSplit tree = do
        let mkSplit = Split (view splitVal split)
        newSplit <- mkSplit <$> insertWithoutGrowth val (view splitLeft split)
                          <*> pure (view splitRight split)
                   <|> mkSplit <$> pure (view splitLeft split)
                              <*> insertWithoutGrowth val (view splitRight split)
        return $ set treeSplit (Just newSplit) tree
    | val `fits` tree = Just $ split val (view rect tree)
    | otherwise = Nothing


----------------------------------------------------------------------------------------------------
-- | Inserts an object into the tree. When the object does not fit, the tree is grown to fit the
-- object.
insert :: HasDim a => a -> Bin a -> Bin a
insert val tree =
    case insertWithoutGrowth val tree of
      Nothing -> grow val tree
      Just t -> t


-- ** Splitting

----------------------------------------------------------------------------------------------------
-- | Split a rectangle horizontally making the largest rectangle downward.
--
-- @
-- ,----.    ,-.--.
-- |    | -> |v|  |
-- |    |    |----|
-- |    |    |    |
-- `----'    `----'
-- @
splitDownward :: HasDim a => a -> Rect -> Bin a
splitDownward val rect | not (val `fits` rect) =
    error "splitDownward: value does not fit in the given rectangle"
splitDownward val rect =
    Bin rect
         (Just (Split val
                     -- Top right corner
                     (Bin (Rect (Pos (view posX rect + view dimWidth val)
                                      (view posY rect))
                                 (Dim (view dimWidth rect - view dimWidth val)
                                      (view dimHeight val)))
                           Nothing)
                     -- Bottom half
                     (Bin (Rect (Pos (view posX rect)
                                      (view posY rect + view dimHeight val))
                                 (Dim (view dimWidth rect)
                                      (view dimHeight rect - view dimHeight val)))
                           Nothing)))


----------------------------------------------------------------------------------------------------
-- | Split a rectangle veritically making the largest rectangle rightward
--
-- @
-- ,----.    ,----.
-- |    | -> |v|  |
-- |    |    |-|  |
-- |    |    | |  |
-- `----'    `----'
-- @
splitRightward :: HasDim a => a -> Rect -> Bin a
splitRightward val rect | not (val `fits` rect) =
    error "splitRightward: value does not fit in the given rectangle"
splitRightward val rect =
    Bin rect
         (Just (Split val
                     -- Bottom left corner
                     (Bin (Rect (Pos (view posX rect)
                                      (view posY rect + view dimHeight val))
                                 (Dim (view dimWidth val)
                                      (view dimHeight rect - view dimHeight val)))
                           Nothing)
                     -- Right half
                     (Bin (Rect (Pos (view posX rect + view dimWidth val)
                                      (view posY rect))
                                 (Dim (view dimWidth rect - view dimWidth val)
                                      (view dimHeight rect)))
                           Nothing)))


----------------------------------------------------------------------------------------------------
-- | Splits a rectangle into two. It splits along the longest side.
split :: HasDim a => a -> Rect -> Bin a
split val rect
    | dw > dh = splitDownward val rect
    | otherwise = splitRightward val rect
  where
    dw = view dimWidth rect - view dimWidth val
    dh = view dimHeight rect - view dimHeight val


-- ** Growing

----------------------------------------------------------------------------------------------------
-- | Grow a tree rightward. Assumes that item inserted to the right is not taller than the tree
-- itself
--
-- @
-- ,---.    ,---.-.
-- | t |    | t |v|
-- |   | -> |   |-|
-- |   |    |   | |
-- `---'    `---'-'
-- @
--
-- New root split contains:
--
--   * value: v
--
--   * left: t
--
--   * right: new whitespace
growRightward :: HasDim a => a -> Bin a -> Either String (Bin a)
growRightward val tree
  | view dimHeight val > view dimHeight tree = Left "growRightward: value is taller than tree"
  | isParent tree = Right $
      Bin (Rect (Pos (view dimWidth tree) 0)
                 (Dim (view dimWidth tree + view dimWidth val)
                      (view dimHeight tree)))
            (Just (Split val
                        -- Root split
                        tree
                        -- New whitespace
                        (Bin (Rect (Pos (view dimWidth tree)
                                         (view dimHeight val))
                                    (Dim (view dimWidth val)
                                         (view dimHeight tree - view dimHeight val)))
                              Nothing)))
  | otherwise = Right $ split val (Rect (Pos 0 0) (view dim val))


----------------------------------------------------------------------------------------------------
-- | Grow a tree downward. Assumes that the item inserted below is not wider than the tree itself.
--
-- @
-- ,---.    ,---.
-- | t | -> | t |
-- `---'    |---|
--          |v| |
--          `---'
-- @
--
-- New root split contains:
--
--   * value: v
--
--   * left: t
--
--   * right: new whitespace
growDownward :: HasDim a => a -> Bin a -> Either String (Bin a)
growDownward val tree
  | view dimWidth val > view dimWidth tree = Left "growDown: value is wider than tree"
  | isParent tree = Right $
      Bin (Rect (Pos 0 (view dimHeight tree))
                 (Dim (view dimWidth tree)
                      (view dimHeight tree + view dimHeight val)))
           (Just (Split val
                       -- Root split
                       tree
                       -- New whitespace
                       (Bin (Rect (Pos (view dimWidth val)
                                        (view dimHeight tree))
                                   (Dim (view dimWidth tree - view dimWidth val)
                                        (view dimHeight val)))
                             Nothing)))
  | otherwise = Right $ split val (Rect (Pos 0 0) (view dim val))


----------------------------------------------------------------------------------------------------
-- | Grow a tree in downward using value as the primary height dimension.
--
-- @
-- ,---.    ,---.-.
-- | t | -> | t |v|
-- `---'    |---| |
--          |   | |
--          `-----'
-- @
--
-- New root split contains:
--
--   * value: v
--
--   * left: t
--
--   * right: new whitespace
growDownwardVal :: HasDim a => a -> Bin a -> Either String (Bin a)
growDownwardVal val tree
  | view dimHeight val < view dimHeight tree = Left "growDownWaste: tree is taller than value"
  | isParent tree = Right $
        Bin (Rect (Pos (view dimWidth tree) 0)
                   (Dim (view dimWidth val + view dimWidth tree)
                        (view dimHeight val)))
             (Just (Split val
                         tree
                         -- New whitespace
                         (Bin (Rect (Pos 0 (view dimHeight tree))
                                     (Dim (view dimWidth tree)
                                          (view dimHeight val - view dimHeight tree)))
                               Nothing)))
  | otherwise = Right $ split val (Rect (Pos 0 0) (view dim val))


----------------------------------------------------------------------------------------------------
-- | Grow a tree
grow :: HasDim a => a -> Bin a -> Bin a
grow val tree =
    case growth of
      Left err -> error ("grow: The impossible has happened, we cannot grow the tree - " ++ err)
      Right t -> t
  where
    growth
      | view dimWidth tree > view dimHeight tree =
            growDownward val tree <|> growRightward val tree <|> growDownwardVal val tree
      | otherwise =
            growRightward val tree <|> growDownward val tree <|> growDownwardVal val tree


-- * Query

----------------------------------------------------------------------------------------------------
-- | Whether a given tree has any children or not
isParent :: Bin a -> Bool
isParent tree = isJust $ view treeSplit tree


----------------------------------------------------------------------------------------------------
-- | Lists all objects and their position in a Bin in a pre-order traversal
positions :: Bin a -> [(Pos, a)]
positions tree =
    case view treeSplit tree of
      Nothing -> []
      Just split ->
        (view pos tree, view splitVal split)
        : positions (view splitLeft split)
        ++ positions (view splitRight split)

