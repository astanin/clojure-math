# Introduction to clojure-math

## Using vectors

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

```clojure
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
```

To use vector types from Commons Math library (`Vector2D`, `Vector3D`), one may
use factory functions:

  - `to-vector`, which is idempotent, and is safe to apply many times
  - `make-vector`, which is a smart constructor; it chooses the right
    vector class based on its arity

For example:

```clojure
user=> (to-vector [1 2 3])
#<Vector3D$Seqable$d90500e3 {1; 2; 3}>
user=> (make-vector 1 2)
#<Vector2D$Seqable$d90500e3 {1; 2}>
```


### Performance of vector arithmetics

Vector operations don't mutate their arguments, and this implies more
allocation costs. The library derives new classes from `Vector2D` and
`Vector3D` to implement `clojure.lang.Seqable`, `proxy` call comes at
a cost.

That said, the library is reasonably fast on small vectors. Vector
addition for Clojure vectors and Common Math's vectors:

```
user=> (use 'criterium.core)
nil
user=> (let [u [1.0 2.0 3.0] v [1.0 2.0 3.0]] (bench (plus u v)))
Evaluation count : 66745920 in 60 samples of 1112432 calls.
             Execution time mean : 918.076489 ns
...
user=> (let [u (to-vector [1 2 3]) v (to-vector [1 2 3])] (bench (plus u v)))
Evaluation count : 19435560 in 60 samples of 323926 calls.
             Execution time mean : 3.160163 us
...
user=> (let [u (Vector3D. 1 2 3) v (Vector3D. 1 2 3)] (bench (.add u v)))
Evaluation count : 5844652440 in 60 samples of 97410874 calls.
             Execution time mean : 10.256131 ns
...
```

FIXME: remove reflection; target at least commons-math performance.
