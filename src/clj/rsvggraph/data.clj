(ns rsvggraph.data
  "Normalise data for use in generating radial graphs."
  (:require [clojure2d.color :refer [gradient]]))


(def ;; ^:dynamic 
  *gradient*
  "The gradient to use to automatically assign pleasing colours to sectors, if
   no colours are defined in the data. Suitable gradients are defined 
   [here](https://clojure2d.github.io/clojure2d/docs/static/gradients/)."
  :rainbow2)

(def children-fn
  "Basic (overridable) children function; assumes `data` is a map, and returns 
   the value of the `:children` key within that map."
  (memoize (fn [data]
             (:children data))))

(def quantity-fn
  "Basic (overridable) children function; assumes `data` is a map. If the value
   of the `:children` key within that map is a sequence, sums the result of 
   mapping itself over that sequence. Otherwise, returns the value of the 
   `:quantity` key, if present and a number, or `1` as a final default."
  (memoize (fn [data]
             (let [c (children-fn data)
                   q (:quantity data)]
               (cond (coll? c) (reduce + 0 (map quantity-fn c))
                     (number? q) q
                     :else 1)))))