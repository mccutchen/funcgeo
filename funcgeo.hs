import Prelude hiding (div)

type Vec = (Double, Double)
type Pair = (Vec, Vec)

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

grid :: Double -> Double -> [Pair] -> (Vec -> Vec -> Vec -> [Pair])
grid m n vs = f
  where
    f :: Vec -> Vec -> Vec -> [Pair]
    f a b c =
      map g vs where
        g :: Pair -> Pair
        g ((x0, y0), (x1, y1)) =
          ((adds [(div (mul b x0) m), a, (div (mul c y0) n)]),
           (adds [(div (mul b x1) m), a, (div (mul c y1) n)]))






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
