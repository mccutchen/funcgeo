#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
Functional Geometry

Original idea by Peter Henderson, see
http://www.ecs.soton.ac.uk/~ph/funcgeo.pdf and
http://www.ecs.soton.ac.uk/~ph/papers/funcgeo2.pdf

Implemented in Lisp by Frank Buß, see
http://www.frank-buss.de/lisp/functional.html

Ported to Python by Will McCutchen <mccutchen@gmail.com>

Usage:

    plot(p, [f])

where `p` is a picture function and `f` is a file-like object which defaults
to sys.stdout.
"""

import sys
from operator import add, sub


##############################################################################
# The framework.
#
# Notes:
#
# Vectors are represented as 2-tuples of (x, y).  The only negative about this
# is the need to ensure that all of the operations explicitly return tuples
# (because, e.g., you can't make a set of lists because lists are unhashable).
#
# Explicit conversion of operands to floats is required in the multiplcation
# and division functions.
##############################################################################

def vmul(vector, m):
    """Vector scalar multiplication."""
    return tuple(x * float(m) for x in vector)

def vdiv(vector, d):
    """Vector scalar division."""
    return tuple(x / float(d) for x in vector)

def vadd(v1, v2):
    """Vector addition."""
    return tuple(map(add, v1, v2))

def vsub(v1, v2):
    """Vector subtraction."""
    return tuple(map(sub, v1, v2))

def grid(m, n, s):
    """Defines a picture function from lines in a grid, s, bounded by vectors
    m and n."""
    def _(a, b, c):
        return tuple(
            (reduce(vadd, (vdiv(vmul(b, x0), m), a, vdiv(vmul(c, y0), n))),
             reduce(vadd, (vdiv(vmul(b, x1), m), a, vdiv(vmul(c, y1), n))))
            for (x0, y0), (x1, y1) in s)
    return _

def polygon(points):
    """Converts the given points, which specify a polygon, into a list of
    lines suitable for input into the grid() function."""
    return tuple((points[i-1], point) for i, point in enumerate(points))

def blank():
    """A blank picture function."""
    return lambda a, b, c: ()

def beside(p, q):
    """Places picture p beside picture q."""
    def _(a, b, c):
        b_half = vdiv(b, 2)
        return tuple(set(p(a, b_half, c) +
                         q(vadd(a, b_half), b_half, c)))
    return _

def above(p, q):
    """Places picture p above picture q."""
    def _(a, b, c):
        c_half = vdiv(c, 2)
        return tuple(set(p(vadd(a, c_half), b, c_half) +
                         q(a, b, c_half)))
    return _

def rot(p):
    """Rotates picture p by 90 degrees."""
    return lambda a, b, c: p(vadd(a, b), c, vmul(b, -1))

def quartet(p1, p2, p3, p4):
    """Returns the given pictures laid out in a square."""
    return above(beside(p1, p2), beside(p3, p4))

def cycle(p):
    """Returns the given picture duplicated and rotated in a square."""
    return quartet(p, rot(rot(rot(p))), rot(p), rot(rot(p)))

def flip(p):
   """Flips picture horizontally"""
   def _(a, b, c):
       return p(vadd(a, b), vmul(b, -1), c)
   return _

def rot45(p):
   """Rotates picture p by 45 degrees."""
   def _(a, b, c):
       return p(vadd(a, vdiv(vadd(b, c),2)),
                vdiv(vadd(b, c),2),
                vdiv(vadd(c, vmul(b, -1)), 2))

def besidequad(m, n, p, q):
   """Places picture p beside picture q scaled by m & n."""
   def _(a, b, c):
       mnscale = float(m) / (m + n)
       nmscale = float(n) / (m + n)
       pv = p(a, vmul(b, mnscale), c)
       qv = q(vadd(a, vmul(b, mnscale)), vmul(b, nmscale), c)
       return tuple(set(pv + qv))
   return _

def abovequad(m, n, p, q):
   """Places picture p beside picture q scaled by m & n."""
   def _(a, b, c):
       mnscale = float(m) / (m + n)
       nmscale = float(n) / (m + n)
       pv = p(vadd(a, vmul(c, nmscale)), b, vmul(c, mnscale))
       qv = q(a, b, vmul(c, nmscale))
       return tuple(set(pv + qv))
   return _

def nonet(p, q, r,
          s, t, u,
          v, w, x):
    return abovequad(
        1, 2,
        besidequad(
            1, 2,
            p, besidequad(1, 1, q, r)),
        abovequad(
            1, 1,
            besidequad(
                1, 2,
                s, besidequad(1, 1, t, u)),
            besidequad(
                1, 2,
                v, besidequad(1, 1, w, x))))

def plot(p, f=sys.stdout):
    """Writes the given picture function to the given file as PostScript."""
    def w(*s): print >> f, '\n'.join(s)

    w('500 500 scale', '.1 .1 translate', '0 setlinewidth',
      '0 0 moveto 1 0 lineto 1 1 lineto 0 1 lineto 0 0 lineto')

    for (x0, y0), (x1, y1) in p((0,0), (1,0), (0,1)):
        w('%f %f moveto %f %f lineto' % (x0, y0, x1, y1))

    w('stroke', 'showpage')


if __name__ == '__main__':

    ##########################################################################
    # A simple test.  Let's build a bunch of predefined picture functions.
    ##########################################################################

    # defines a man
    man = grid(
        14, 20,
        polygon(((6, 10), (0, 10), (0, 12), (6, 12), (6, 14),
                 (4, 16), (4, 18), (6, 20), (8, 20), (10, 18),
                 (10, 16), (8, 14), (8, 12), (10, 12), (10, 14),
                 (12, 14), (12, 10), (8, 10), (8, 8), (10, 0),
                 (8, 0), (7, 4), (6, 0), (4, 0), (6, 8))))

    man_beside_man = beside(man, man)
    man_above_man = above(man, man)
    man_rotated = rot(man)
    man_quartet = quartet(man, man, man, man)
    man_cycle = cycle(man)


    ##########################################################################
    # the fish
    ##########################################################################
    p = grid(
        16, 16,
        (((4, 4), (6, 0)), ((0, 3), (3, 4)), ((3, 4), (0, 8)),
         ((0, 8), (0, 3)), ((4, 5), (7, 6)), ((7, 6), (4, 10)),
         ((4, 10), (4, 5)), ((11, 0), (10, 4)), ((10, 4), (8, 8)),
         ((8, 8), (4, 13)), ((4, 13), (0, 16)), ((11, 0), (14, 2)),
         ((14, 2), (16, 2)), ((10, 4), (13, 5)), ((13, 5), (16, 4)),
         ((9, 6), (12, 7)), ((12, 7), (16, 6)), ((8, 8), (12, 9)),
         ((12, 9), (16, 8)), ((8, 12), (16, 10)), ((0, 16), (6, 15)),
         ((6, 15), (8, 16)), ((8, 16), (12, 12)), ((12, 12), (16, 12)),
         ((10, 16), (12, 14)), ((12, 14), (16, 13)), ((12, 16), (13, 15)),
         ((13, 15), (16, 14)), ((14, 16), (16, 15))))

    q = grid(
        16, 16,
        (((2, 0), (4, 5)), ((4, 5), (4, 7)), ((4, 0), (6, 5)),
         ((6, 5), (6, 7)), ((6, 0), (8, 5)), ((8, 5), (8, 8)),
         ((8, 0), (10, 6)), ((10, 6), (10, 9)), ((10, 0), (14, 11)),
         ((12, 0), (13, 4)), ((13, 4), (16, 8)), ((16, 8), (15, 10)),
         ((15, 10), (16, 16)), ((16, 16), (12, 10)), ((12, 10), (6, 7)),
         ((6, 7), (4, 7)), ((4, 7), (0, 8)), ((13, 0), (16, 6)),
         ((14, 0), (16, 4)), ((15, 0), (16, 2)), ((0, 10), (7, 11)),
         ((9, 12), (10, 10)), ((10, 10), (12, 12)), ((12, 12), (9, 12)),
         ((8, 15), (9, 13)), ((9, 13), (11, 15)), ((11, 15), (8, 15)),
         ((0, 12), (3, 13)), ((3, 13), (7, 15)), ((7, 15), (8, 16)),
         ((2, 16), (3, 13)), ((4, 16), (5, 14)), ((6, 16), (7, 15))))

    r = grid(
        16, 16,
        (((0, 12), (1, 14)), ((0, 8), (2, 12)), ((0, 4), (5, 10)),
         ((0, 0), (8, 8)), ((1, 1), (4, 0)), ((2, 2), (8, 0)),
         ((3, 3), (8, 2)), ((8, 2), (12, 0)), ((5, 5), (12, 3)),
         ((12, 3), (16, 0)), ((0, 16), (2, 12)), ((2, 12), (8, 8)),
         ((8, 8), (14, 6)), ((14, 6), (16, 4)), ((6, 16), (11, 10)),
         ((11, 10), (16, 6)), ((11, 16), (12, 12)), ((12, 12), (16, 8)),
         ((12, 12), (16, 16)), ((13, 13), (16, 10)), ((14, 14), (16, 12)),
         ((15, 15), (16, 14))))

    s = grid(
        16, 16,
        (((0, 0), (4, 2)), ((4, 2), (8, 2)), ((8, 2), (16, 0)),
         ((0, 4), (2, 1)), ((0, 6), (7, 4)), ((0, 8), (8, 6)),
         ((0, 10), (7, 8)), ((0, 12), (7, 10)), ((0, 14), (7, 13)),
         ((8, 16), (7, 13)), ((7, 13), (7, 8)), ((7, 8), (8, 6)),
         ((8, 6), (10, 4)), ((10, 4), (16, 0)), ((10, 16), (11, 10)),
         ((10, 6), (12, 4)), ((12, 4), (12, 7)), ((12, 7), (10, 6)),
         ((13, 7), (15, 5)), ((15, 5), (15, 8)), ((15, 8), (13, 7)),
         ((12, 16), (13, 13)), ((13, 13), (15, 9)), ((15, 9), (16, 8)),
         ((13, 13), (16, 14)), ((14, 11), (16, 12)), ((15, 9), (16, 10))))

    # Build the drawing of the fish out of the parts defined above
    t = quartet(p, q, r, s)
    u = cycle(rot(q))
    side1 = quartet(blank(), blank(), rot(t), t)
    side2 = quartet(side1, side1, rot(t), t)
    corner1 = quartet(blank(), blank(), blank(), u)
    corner2 = quartet(corner1, side1, rot(side1), u)
    pseudocorner = quartet(corner2, side2, rot(side2), rot(t))
    fishes = cycle(pseudocorner)

    # Draw the fishes
    plot(fishes)
