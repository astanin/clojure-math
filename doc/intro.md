# Introduction to clojure-math

## Vector arithmetics

`clojure.math.geometry` defines some protocols and their
implementations for basic vector arithmetics. They are defined for
standard clojure sequences (vectors preferred), as well as for Apache
Commons Math vector classes.

Operations:

   - `plus`
   - `minus`
   - `scale`
   - `dot` (dot product)
   - `cross` (cross product, defined only for 3D vectors, returns a
     scalar for 2D vectors)
   - `norm` (Euclidean $L_2$ norm)
   - `normalize`

Examples:

    user=> (use 'clojure.math.geometry)
    nil
    user=> (plus [1 2 3] [1 2 3])
    [2 4 6]
    user=> (dot [1 2 3] [1 2 3])
    14
    user=> (cross [1 0 0] [0 0 1])
    [0 -1 0]
    user=> (cross [0 1] [1 0])
    -1
    user=> (cross [1] [2])
    IllegalArgumentException dimension must be 2 or 3  clojure.math.geometry/eval1188/fn--1189 (geometry.clj:38)


To use vector types from Commons Math library (`Vector2D`, `Vector3D`), one may
use factory functions:

  - `make-vector`, which is a smart constructor; it chooses the right
    vector class based on its arity
  - `to-vector`, which is idempotent, and is safe to apply many times

For example:

    user=> (to-vector [1 2 3])
    #<Vector3D$Seqable$d90500e3 {1; 2; 3}>
    user=> (make-vector 1 2)
    #<Vector2D$Seqable$d90500e3 {1; 2}>


## Performance of vector arithmetics

Vector operations don't mutate their arguments, and this implies higher
allocation and GC costs. The library derives new classes from `Vector2D` and
`Vector3D` to implement `clojure.lang.Seqable`, `proxy` is expensive.
Neither this library nor the underlying Commons Math library
are designed for the extreme performance.

That said, the library is reasonably fast on small vectors, using
`to-vector` is an order of magnitude faster than doing naive vector
arithmetics over the Clojure-vectors. However, Clojure wrappers are
slower than direct method calls on Commons Math objects.

| Vector                                    | plus | scale |  dot | cross | normalize |
|-------------------------------------------+------+-------+------+-------+-----------|
| (Vector3D. 1 2 3) / direct method calls   |   16 |  2760 |   35 |    53 |        18 |
| (Vector3D. 1 2 3) / EuclideanVector funcs |   25 |    24 |   44 |    61 |        29 |
| (to-vector [1 2 3])                       |   93 |   101 |  110 |   126 |       244 |
| (double-array [1 2 3])                    |   40 |    41 |  217 |  1431 |       841 |
| [1.0 2.0 3.0]                             |  836 |  1234 | 1126 |   985 |      2566 |

Table: relative times for different operations across supported implementations.

Consider creating vectors directly with `(Vector3D. x y z)`,
`(Vector2D. x y)`, if the performance if more important than a convenience
of having a `clojure.lang.Seqable` instance.
