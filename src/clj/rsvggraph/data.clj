(ns rsvggraph.data
  "Normalise data for use in generating radial graphs. A lot of the things
   I want to do here, especially with colour, are not readily portable between
   clojure and clojurescript, so this is Clojure only for now."
  (:require [clojure2d.color :refer [format-hex gradient]]
            [fastmath.core :refer [pow]]))


(def ^:dynamic *gradient*
  "The gradient to use to automatically assign pleasing colours to sectors, if
   no colours are defined in the data. Suitable gradients are defined 
   [here](https://clojure2d.github.io/clojure2d/docs/static/gradients/)."
  :rainbow2)

(def ^:dynamic *children*
  "Basic (overridable) children function; assumes `data` is a map, and returns 
   the value of the `:children` key within that map."
  (memoize (fn [data]
             (:children data))))

(def ^:dynamic *quantity*
  "Basic (overridable) children function; assumes `data` is a map. If the value
   of the `:children` key within that map is a sequence, sums the result of 
   mapping itself over that sequence. Otherwise, returns the value of the 
   `:quantity` key, if present and a number, or `1` as a final default."
  (memoize (fn [data]
             (let [c (*children* data)
                   q (:quantity data)]
               (cond (number? q) q
                     (coll? c) (reduce + 0 (map *quantity* c)) 
                     :else 1)))))

(def ^:dynamic *colour*
  "Return, as a hex string acceptable to HTML and CSS, the appropriate colour
   for this `datum` having this `central-quantity-fraction` as the centre of
   its position in the arc."
  (fn [data central-quantity-fraction]
    (let [col (:colour data)
          grad (gradient *gradient*)]
      (cond
        col col
        (number? data) (format-hex (grad data))
        (number? central-quantity-fraction) (format-hex (grad central-quantity-fraction))
        :else "gray"))))

(def proportion
  (memoize
   (fn [data total]
     (double (/ (*quantity* data) total)))))

(defn normalise
  ([data]
   (normalise data 1 (*quantity* data) 0))
  ([data ring total left]
   (let [id (or (:id data)
                (keyword (gensym (or (:title data) "datum"))))
         q (*quantity* data)
         p (proportion data total)
         color (*colour* data (+ left (/ p 2)))]
     (assoc data
            :children
            (when (:children data)
              (loop [c (first (:children data))
                     l left
                     y (rest (:children data))
                     v nil]
                (let [r (+ l (proportion c total))
                      c' (assoc
                                (normalise c (inc ring) total l)
                                :left l
                                :right r)
                      v' (cons c' v)]
                  ;; (println (format "Label: %s: ring: %d; left: %f; right: %f; colour %s." 
                                  ;;  (:label c') ring (double l) r color))
                  (cond (empty? y) (vec v')
                        :else
                        (recur (first y)
                               r
                               (rest y)
                               v')))))
            :colour color
            :id id
            :inner-diameter (/ 1 (pow 3 ring))
            :label (or (:label data) id)
            :left 0
            :proportion p
            :quantity q
            :right 1
            :ring ring))))