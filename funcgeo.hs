type Vector = (Double, Double)
type Vectors = [Vector]

mul :: Vector -> Double -> Vector
mul (vx, vy) m = (vx * m, vy * m)

div :: Vector -> Double -> Vector
div (vx, vy) d = (vx / d, vy / d)

add :: Vector -> Vector -> Vector
add (vx1, vy1) (vx2, vy2) = (vx1 + vx2, vy1 + vy2)

sub :: Vector -> Vector -> Vector
sub (vx1, vy1) (vx2, vy2) = (vx1 - vx2, vy1 - vy2)

grid :: Vector -> Vector -> Vectors -> (Vector -> Vector -> Vector -> Vectors)
grid (x0, y0) (x1, y1) (v:vs)
