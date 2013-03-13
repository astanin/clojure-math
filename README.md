# clojure-math

Incomplete bindings to Commons Math, and other math-related bits of
code, to make up for the lack of NumPy and SciPy in Clojure.


## Usage

### Vectors

Use `to-vector` to create a `Vector2D` or `Vector3D` object, which
implements also `clojure.lang.Seqable`. Supported vector operations
are: `plus`, `minus`, `scale`, `norm`, `normalize`, `dist`, `dot`, and
`cross`.

    (use 'clojure.math.geometry)

    (let [x (to-vector [1 2 3])
          y (to-vector [4 5 6])]
      (cross x y))

Currently supported data types: any `clojure.lang.Seqable`, double
arrays, `Vector2D`, `Vector3D`.

Using Commons Math's vector constructors directly results in faster
operations, but you don't get a `Seqable` instance:

    (import 'org.apache.commons.math3.geometry.euclidean.threed.Vector3D)

    (let [x (Vector3D. 1 2 3)
          y (Vector3D. 4 5 6)]
      (cross x y))


### Convolution and filtering

For discrete convolution of one-dimensonal sequences:

    user=> (use 'clojure.math.signal)
    nil
    user=> (conv [1 2 3 4 5] [0.25 0.5 0.25] :valid)
    [2.0 3.0 4.0]

The third parameter to `conv` is optional; acceptable values are:
`:full` (default), `:same`, and `:valid`. The semantics is the same as
for `mode` parameter of [`numpy.convolve`][numpy-convolve].

Currently supported data types are: double arrays and vectors.

[numpy-convolve]: http://docs.scipy.org/doc/numpy/reference/generated/numpy.convolve.html


## License

Copyright © 2013 Sergey Astanin

Distributed under the Eclipse Public License, the same as Clojure.
