(ns figdemo.projector)


;;let's figure out how to project
;;all the things!

;;note: most of the math side can
;;be had here:
;;http://www.math.cornell.edu/~froh/231f08e1a.pdf

;;We have a simple* task:

;;There is a set of points in R3 (3d data),
;;called XYZ, that define a surface.  We are
;;guaranteed to have at least 3 points,
;;possibly more.

;;Let's call the function that defines
;;this surface F...

;;Our task is:
;;  Given any arbitrary 2D point, P0 = [x y],
;;  what is the z value of [x y] when
;;  projected onto F, such that
;;  (f [x y]) => [x y z], where
;;  [x y z] is a point that intersects
;;  the plane, ABC, where
;;  ABC is defined by three points P1, P2, P3
;;  such that:
;;  P1, P2, P3 are within XYZ,
;;  For the 2D Euclidean distance, defined by
;;  (e2d u v),
;;  (e2d P0 P1) <= (e2d P0 P2) <= (e2d P0 P3), <=
;;  (e2d P0 x) forall x within (XYZ - {P1 P2 P3})

;;  That is, ABC is a plane defined by the points
;;  that are neareast to P0 in the 2 dimensional plane
;;  defined by the first two dimensions.
(def xyz #{[0 0 0]
           [1 1 0.5]
           [2 0.5 0.5]
           [10 8 4]})

;;util
(defn square [x] (* x x))

;;euclidean distance func
(defn distance-2d [[x1 y1 ]
                   [x2 y2 ]]  
  (Math/sqrt (+ (square (- x2 x1))
                (square (- y2 y1)))))

;;find the 3 nearest points to define
;;an interpolating plane
(defn nearest [x pts]
  (->> pts 
       (sort-by (fn [y]
                  (distance-2d x y)))
       (take 3)))

;;define the plane from three points
;;If we want to construct a plane,
;;we can derive two vectors that
;;span the plane, and use the vector
;;cross product to define a vector
;;[a b c] normal to the plane.
;;the cross product is handily defined
;;as
(defn cross-3d [u v]
  (let [[u1 u2 u3] u
        [v1 v2 v3] v]
    [(- (* u2 v3) (* u3 v2))
     (- (* u3 v1) (* u1 v3))
     (- (* u1 v2) (* u2 v1))]))

;;utils for naive vector math
(defn v- [ls rs] (mapv - ls rs))
(defn v+ [ls rs] (mapv + ls rs))
(defn v* [ls rs] (mapv * ls rs))
(defn dot [ls rs] (reduce + (v* ls rs)))

;;Since we can compute the normal
;;vector [a b c] that defines the
;;plane via cross product, we
;;need two vectors that span the
;;plane to provide to our
;;cross-product.  We can construct
;;these vectors by vector subtraction,
;;assuming the first "point" is our
;;common origin, we compute two new
;;vectors and use the resulting
;;normal to compute the scalar value
;;d for the plane definition:
(defn ->plane-vec
  ([p1 p2 p3]
   (let [v1      (v- p2 p1)
         v2      (v- p3 p1)
         [a b c] (cross-3d v1 v2)
         d       (dot p1 [a b c])
         _       (when (and (zero? a) (zero? c))
                   (throw (js/Error. (str [:bad-plane
                                           [p1 p2 p3]
                                           [v1 v2]]))))                                          
         ]
     [[a b c] d]))
  ([[p1 p2 p3]] (->plane-vec p1 p2 p3)))

;;this is a bad plane apparently.
(comment
  (def ps [[13,12, 0.82974507] [16 ,12, 0.530213494] [10, 12, 0.298692411]])
  (def v1  [3 0 -0.299531576])
  (def v2 [-3 0 -0.531052659])
  )


;;If we're smart, we can solve for
;;the value of z in our planar
;;definition:
;;[ax by cz] = d
;;ax + by + cz = d
;;cz = d - (ax + by)
;;z =  d - (ax + by) / c
;;If we have a definition of the plane,
;;a normal vector, and the constant d,
;;then we can solve for our 2d projection:
(defn onto-plane [[x y] [[a b c] d]]
  (let [z (/ (- d
                (+ (* a x)
                   (* b y)))
             c)]
    [x y z]))

;;given a 2d point, and some xyz-coords
;;in data, return a  point where the
;;z-coordinate is computed either by
;;finding an existing coordinate
;;[x y z] in the data, or interpolated
;;by projecting the point [x y] onto
;;the interpolating plane defined by
;;the three points nearest to [x y] in
;;data, defined by euclidean distance.
(defn interpolate [[x y :as p] data]
  (if (contains? data p) p
      (onto-plane p
        (->plane-vec
               (nearest p data)))))

(comment ;testing 
(let [p [1.5 0.5]]
  (onto-plane p (apply ->plane-vec (nearest p xyz))))
;;=>[1.5 0.5 0.4166666666666667]

(interpolate [2 0.5] xyz)
;;=> [2 0.5 0.5]

;;Note: nearest neighbors requires
;;all-pairs comparisons...
;;we can probably do better than this,
;;rather than ed, we can probably keep the
;;items sorted by x and y,
;;and find data nearest x y....
;;we could also build a mst, find the nearest point
;;and walk outwards until we have three...
;;There are a slew of optimizations we can apply,
;;including spatial partitioning.  For now, we'll
;;anticipate much smaller datasets, and stick
;;with our naive interpolation algorithms.
;;This dataset has 1600 points, and interpolates
;;very quickly.  at 40K, we start hitting 0.17 s
;;runtime per interpolation.  Our algorithm is
;;mainly limited by nearest-neighbors computation.
(def test-data  
  (set 
   (for [x (range -100 100 5)
         y (range -100 100 5)]
     [x y (+ x y)])))


(interpolate [20 33] test-data)
;;[20 33 53]
(interpolate [21 33] test-data)
;;[21 33 54]
(interpolate [80 80 ] test-data)
;;[80 80 160]

(def test-data2  
  (set 
   (for [x (range -100 100 )
         y (range -100 100 )]
     [x y (* x y)])))
)

