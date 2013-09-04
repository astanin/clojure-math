# clojure-math

Incomplete bindings to Commons Math, EJML, and other math-related bits
of code, to make up for the lack of NumPy and SciPy in Clojure.

[API documentation](http://astanin.github.io/clojure-math/)


## Usage

### Vector algebra

Supported vector operations are: `plus`, `minus`, `scale`, `norm`,
`normalize`, `dist`, `dot`, and `cross`.

Currently supported vector representations: any
`clojure.lang.Seqable`, double arrays, `Vector2D` from `Vector3D` from
Commons Math 3, `DenseMatrix64F` from EJML.

To create Commons Math's vectors from Clojure sequences, use `to-vector`:

    (use 'clojure.math.geometry)
    (use '[clojure.math.internal.cmath3 :only [to-vector]])

    (let [x (to-vector [1 2 3])
          y (to-vector [4 5 6])]
      (cross x y))

Using Commons Math's vector constructors directly results in faster
operations, but you don't get a `Seqable` instance.


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

For median filter use `medfilt`:

    user=> (use 'clojure.math.signal)
    nil
    user=> (medfilt [0 0 1 0 0 0 2 0 3 0 0] 5)
    (0 0 0 0 0 0 0 0 0 0 0)


### Geometric fitting

Fitting lines using total least squares

    user=> (use 'clojure.math.geometry.fit)
    nil
    user=> (fit-line [[0 1] [1 2] [2 3.1] [4 5]])
    {:point [1.75 2.775], :direction [-0.7059474712429025 -0.7082641935363889]}



## License

Copyright © 2013 Sergey Astanin

Distributed under the Eclipse Public License, the same as Clojure.
