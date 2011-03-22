{- Functional Geometry

Original idea by Peter Henderson, see
http://www.ecs.soton.ac.uk/~ph/funcgeo.pdf and
http://www.ecs.soton.ac.uk/~ph/papers/funcgeo2.pdf

Implemented in Lisp by Frank Buß, see
http://www.frank-buss.de/lisp/functional.html

Ported to Haskell by Will McCutchen <mccutchen@gmail.com>

Note: This is my very first Haskell program, so it's most likely horrible.
Please don't hold that against me. -}

import Prelude hiding (div, cycle)
import Data.List (union, intercalate)
import Text.Printf (printf)


-- A vector is a single point
type Vec = (Double, Double)

-- A pair of vectors is a line
type Pair = (Vec, Vec)

-- A picture function takes three vectors: "a" for the offset from bottom
-- left, and "b" and "c" for the bounding box of the picture.  It returns a
-- list of vector pairs representing coordinates that can be plotted as
-- PostScript.
type Picture = Vec -> Vec -> Vec -> [Pair]


-- ---------------------------------------------------------------------------
-- Basic vector arithmetic
-- ---------------------------------------------------------------------------
mul :: Vec -> Double -> Vec
mul (x, y) m = (x * m, y * m)

div :: Vec -> Double -> Vec
div (x, y) d = (x / d, y / d)

add :: Vec -> Vec -> Vec
add (x0, y0) (x1, y1) = (x0 + x1, y0 + y1)

sub :: Vec -> Vec -> Vec
sub (x0, y0) (x1, y1) = (x0 - x1, y0 - y1)

-- Add and subtract multiple vectors at once. Note to self: As suggested by
-- `hlint`, these definitions take advantage of (what I think of as) implicit
-- currying. They each take an implied [Vec] argument, which is automagically
-- used as the missing final argument to foldl.
adds :: [Vec] -> Vec
adds = foldl add (0, 0)

subs :: [Vec] -> Vec
subs = foldl sub (0, 0)


-- ---------------------------------------------------------------------------
-- Image function building blocks
-- ---------------------------------------------------------------------------

-- Defines a picture function from the line segments (Pairs) in the list "vs",
-- bounded by vectors "m" and "n".
grid :: Double -> Double -> [Pair] -> Picture
grid m n vs = f
  where
    f a b c =
      map g vs where
        g ((x0, y0), (x1, y1)) =
          (adds [div (mul b x0) m, a, div (mul c y0) n],
           adds [div (mul b x1) m, a, div (mul c y1) n])

-- Converts a list of vectors specifying the points in a polygon into a list
-- of line segments that can be used to draw the polygon, by pairing up the
-- individual points.
polygon :: [Vec] -> [Pair]
polygon vs = zip (last vs : init vs) vs

blank :: Picture
blank a b c = []

beside :: Picture -> Picture -> Picture
beside p q = f
  where
    f a b c = p a bHalf c `union` q (add a bHalf) bHalf c
      where
        bHalf = b `div` 2

above :: Picture -> Picture -> Picture
above p q = f
  where
    f a b c = p (add a cHalf) b cHalf `union` q a b cHalf
      where
        cHalf = c `div` 2

rot :: Picture -> Picture
rot p = f
  where
    f a b c = p (add a b) c (mul b (-1))

quartet :: Picture -> Picture -> Picture -> Picture -> Picture
quartet p1 p2 p3 p4 = above (beside p1 p2) (beside p3 p4)

cycle :: Picture -> Picture
cycle p = quartet p (rot (rot (rot p))) (rot p) (rot (rot p))


-- ---------------------------------------------------------------------------
-- Rendering/output functions
-- ---------------------------------------------------------------------------

-- Returns a string containing the PostScript needed to render the picture "p"
plot :: Picture -> String
plot p = header ++ "\n" ++ picture ++ "\n" ++ footer
  where
    header = intercalate "\n" [
      "500 500 scale", ".1 .1 translate", "0 setlinewidth",
      "0 0 moveto 1 0 lineto 1 1 lineto 0 1 lineto 0 0 lineto"]
    footer = "stroke\nshowpage\n"
    picture = intercalate "\n" (map pfunc (p (0,0) (1,0) (0,1)))
      where
        pfunc ((x0,y0), (x1, y1)) =
          printf "%.6f %.6f moveto %.6f %.6f lineto" x0 y0 x1 y1

write p filename = writeFile filename (plot p)

main = write fishes "fishes.ps"


-- ---------------------------------------------------------------------------
-- Premade picture functions
-- ---------------------------------------------------------------------------

-- a man
man = grid 14 20 (polygon [(6, 10), (0, 10), (0, 12), (6, 12), (6, 14),
                           (4, 16), (4, 18), (6, 20), (8, 20), (10, 18),
                           (10, 16), (8, 14), (8, 12), (10, 12), (10, 14),
                           (12, 14), (12, 10), (8, 10), (8, 8), (10, 0),
                           (8, 0), (7, 4), (6, 0), (4, 0), (6, 8)])

manBesideMan = beside man man
manAboveMan = above man man
manRotated = rot man
manQuartet = quartet man man man man
manCycle = cycle man


-- the building blocks of the Escher fish
p = grid 16 16
    [((4, 4), (6, 0)), ((0, 3), (3, 4)), ((3, 4), (0, 8)),
     ((0, 8), (0, 3)), ((4, 5), (7, 6)), ((7, 6), (4, 10)),
     ((4, 10), (4, 5)), ((11, 0), (10, 4)), ((10, 4), (8, 8)),
     ((8, 8), (4, 13)), ((4, 13), (0, 16)), ((11, 0), (14, 2)),
     ((14, 2), (16, 2)), ((10, 4), (13, 5)), ((13, 5), (16, 4)),
     ((9, 6), (12, 7)), ((12, 7), (16, 6)), ((8, 8), (12, 9)),
     ((12, 9), (16, 8)), ((8, 12), (16, 10)), ((0, 16), (6, 15)),
     ((6, 15), (8, 16)), ((8, 16), (12, 12)), ((12, 12), (16, 12)),
     ((10, 16), (12, 14)), ((12, 14), (16, 13)), ((12, 16), (13, 15)),
     ((13, 15), (16, 14)), ((14, 16), (16, 15))]

q = grid 16 16
    [((2, 0), (4, 5)), ((4, 5), (4, 7)), ((4, 0), (6, 5)),
     ((6, 5), (6, 7)), ((6, 0), (8, 5)), ((8, 5), (8, 8)),
     ((8, 0), (10, 6)), ((10, 6), (10, 9)), ((10, 0), (14, 11)),
     ((12, 0), (13, 4)), ((13, 4), (16, 8)), ((16, 8), (15, 10)),
     ((15, 10), (16, 16)), ((16, 16), (12, 10)), ((12, 10), (6, 7)),
     ((6, 7), (4, 7)), ((4, 7), (0, 8)), ((13, 0), (16, 6)),
     ((14, 0), (16, 4)), ((15, 0), (16, 2)), ((0, 10), (7, 11)),
     ((9, 12), (10, 10)), ((10, 10), (12, 12)), ((12, 12), (9, 12)),
     ((8, 15), (9, 13)), ((9, 13), (11, 15)), ((11, 15), (8, 15)),
     ((0, 12), (3, 13)), ((3, 13), (7, 15)), ((7, 15), (8, 16)),
     ((2, 16), (3, 13)), ((4, 16), (5, 14)), ((6, 16), (7, 15))]

r = grid 16 16
    [((0, 12), (1, 14)), ((0, 8), (2, 12)), ((0, 4), (5, 10)),
     ((0, 0), (8, 8)), ((1, 1), (4, 0)), ((2, 2), (8, 0)),
     ((3, 3), (8, 2)), ((8, 2), (12, 0)), ((5, 5), (12, 3)),
     ((12, 3), (16, 0)), ((0, 16), (2, 12)), ((2, 12), (8, 8)),
     ((8, 8), (14, 6)), ((14, 6), (16, 4)), ((6, 16), (11, 10)),
     ((11, 10), (16, 6)), ((11, 16), (12, 12)), ((12, 12), (16, 8)),
     ((12, 12), (16, 16)), ((13, 13), (16, 10)), ((14, 14), (16, 12)),
     ((15, 15), (16, 14))]

s = grid 16 16
    [((0, 0), (4, 2)), ((4, 2), (8, 2)), ((8, 2), (16, 0)),
     ((0, 4), (2, 1)), ((0, 6), (7, 4)), ((0, 8), (8, 6)),
     ((0, 10), (7, 8)), ((0, 12), (7, 10)), ((0, 14), (7, 13)),
     ((8, 16), (7, 13)), ((7, 13), (7, 8)), ((7, 8), (8, 6)),
     ((8, 6), (10, 4)), ((10, 4), (16, 0)), ((10, 16), (11, 10)),
     ((10, 6), (12, 4)), ((12, 4), (12, 7)), ((12, 7), (10, 6)),
     ((13, 7), (15, 5)), ((15, 5), (15, 8)), ((15, 8), (13, 7)),
     ((12, 16), (13, 13)), ((13, 13), (15, 9)), ((15, 9), (16, 8)),
     ((13, 13), (16, 14)), ((14, 11), (16, 12)), ((15, 9), (16, 10))]

-- Build the drawing of the fish out of the parts defined above
t = quartet p q r s
u = cycle (rot q)
side1 = quartet blank blank (rot t) t
side2 = quartet side1 side1 (rot t) t
corner1 = quartet blank blank blank u
corner2 = quartet corner1 side1 (rot side1) u
pseudocorner = quartet corner2 side2 (rot side2) (rot t)
fishes = cycle pseudocorner
