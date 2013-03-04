# clojure-math

Incomplete bindings to Commons Math to make up for the lack of NumPy in Clojure.


## Usage

Use `to-vector` to create a `Vector2D` or `Vector3D` object, which
implements also `clojure.lang.Seqable`. Supported vector operations
are: `plus`, `minus`, `scale`, `norm`, `normalize`, `dot`, and
`cross`.

    (use 'clojure.math.geometry)

    (let [x (to-vector [1 2 3])
          y (to-vector [4 5 6])]
      (cross x y))

Using Commons Math's vector constructors directly is much faster, but
you don't get a `Seqable` instance:

    (import 'org.apache.commons.math3.geometry.euclidean.threed.Vector3D)

    (let [x (Vector3D. 1 2 3)
          y (Vector3D. 4 5 6)]
      (cross x y))


## License

Copyright © 2013 Sergey Astanin

Distributed under the Eclipse Public License, the same as Clojure.
