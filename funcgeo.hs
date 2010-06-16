import Prelude hiding (div)
import Data.List (union)

type Vec = (Double, Double)
type Pair = (Vec, Vec)
type Picture = Vec -> Vec -> Vec -> [Pair]

mul :: Vec -> Double -> Vec
mul (x, y) m = (x * m, y * m)

div :: Vec -> Double -> Vec
div (x, y) d = (x / d, y / d)

add :: Vec -> Vec -> Vec
add (x0, y0) (x1, y1) = (x0 + x1, y0 + y1)

adds :: [Vec] -> Vec
adds vs = foldl add (0, 0) vs

sub :: Vec -> Vec -> Vec
sub (x0, y0) (x1, y1) = (x0 - x1, y0 - y1)

subs :: [Vec] -> Vec
subs vs = foldl sub (0, 0) vs

grid :: Double -> Double -> [Pair] -> Picture
grid m n vs = f
  where
    f :: Picture
    f a b c =
      map g vs where
        g :: Pair -> Pair
        g ((x0, y0), (x1, y1)) =
          ((adds [(div (mul b x0) m), a, (div (mul c y0) n)]),
           (adds [(div (mul b x1) m), a, (div (mul c y1) n)]))

polygon :: [Vec] -> [Pair]
polygon vs = zip (last vs : init vs) vs

blank :: Picture
blank = \a b c -> []

beside :: Picture -> Picture -> Picture
beside p q = f
  where
    f :: Picture
    f a b c = union (p a bHalf c) (q (add a bHalf) bHalf c)
      where
        bHalf = c `div` 2

above :: Picture -> Picture -> Picture
above p q = f
  where
    f :: Picture
    f a b c = union (p (add a cHalf) b cHalf) (q a b cHalf)
      where
        cHalf = c `div` 2

rot :: Picture -> Picture
rot p = f
  where
    f :: Picture
    f a b c = p (add a b) c (mul b (-1))

quartet :: Picture -> Picture -> Picture -> Picture -> Picture
quartet p1 p2 p3 p4 = above (beside p1 p2) (beside p3 p4)

cycle :: Picture -> Picture
cycle p = quartet p (rot (rot (rot p))) (rot p) (rot (rot p))




man = grid 14 20 (polygon [(6, 10), (0, 10), (0, 12), (6, 12), (6, 14),
                           (4, 16), (4, 18), (6, 20), (8, 20), (10, 18),
                           (10, 16), (8, 14), (8, 12), (10, 12), (10, 14),
                           (12, 14), (12, 10), (8, 10), (8, 8), (10, 0),
                           (8, 0), (7, 4), (6, 0), (4, 0), (6, 8)])




-- def grid(m, n, s):
--     """Defines a picture function from lines in a grid, s, bounded by vectors
--     m and n."""
--     def _(a, b, c):
--         return tuple(
--             (reduce(vadd, (vdiv(vmul(b, x0), m), a, vdiv(vmul(c, y0), n))),
--              reduce(vadd, (vdiv(vmul(b, x1), m), a, vdiv(vmul(c, y1), n))))
--             for (x0, y0), (x1, y1) in s)
--     return _

-- (defun grid (m n s)
--   "defines a picture from lines in a grid"
--   (lambda (a b c)
--     (loop for line in s collect
--           (destructuring-bind ((x0 y0) (x1 y1)) line
--             (list (p+ (p/ (p* b x0) m) a (p/ (p* c y0) n))
--                   (p+ (p/ (p* b x1) m) a (p/ (p* c y1) n)))))))
