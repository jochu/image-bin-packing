-- Tests
import Test.QuickCheck
import Control.Lens
import RectBinPacker.Bin
import RectBinPacker.Geometry

prop_splitRightwardDoesNotChangeWidth (Positive w) (Positive w') =
    case view treeSplit (splitRightward (Dim w 0) (Rect (Pos 0 0) (Dim (w + w') 0))) of
      Nothing -> False
      Just node ->
        let nw = view (splitLeft . dimWidth) node + view (splitRight . dimWidth) node
        in nw == w + w' -- (nw, nh) == (w', h')

prop_splitDownwardDoesNotChangeHeight (Positive h) (Positive h') =
    case view treeSplit (splitDownward (Dim 0 h) (Rect (Pos 0 0) (Dim 0 (h + h')))) of
      Nothing -> False
      Just node ->
        let nh = view (splitLeft . dimHeight) node + view (splitRight . dimHeight) node
        in if nh == h + h'
           then True
           else error (show (nh, h + h'))

prop_splitRightwardDoesNotChangeArea (Positive w) (Positive w') (Positive h) (Positive h') x y =
    case view treeSplit (splitRightward (Dim w h) (Rect (Pos x y) (Dim (w + w') (h + h')))) of
      Nothing -> False
      Just node ->
        let a = view (splitLeft . area) node + view (splitRight . area) node + view area (Dim w h)
        in a == view area (Dim (w + w') (h + h'))

prop_splitDownwardDoesNotChangeArea (Positive w) (Positive w') (Positive h) (Positive h') x y =
    case view treeSplit (splitDownward (Dim w h) (Rect (Pos x y) (Dim (w + w') (h + h')))) of
      Nothing -> False
      Just node ->
        let a = view (splitLeft . area) node + view (splitRight . area) node + view area (Dim w h)
        in a == view area (Dim (w + w') (h + h'))
