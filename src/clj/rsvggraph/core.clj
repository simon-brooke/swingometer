(ns rsvggraph.core
  (:require [clojure.math :refer [cos PI sin]]
            [clojure.string :refer [join replace]]
            [clojure.xml :refer [emit]]
            [fastmath.core :refer [pow]]
            [hiccup2.core :refer [html]]
            [rsvggraph.data :refer [normalise]]))



(defn polar-to-cartesian
  "Return, as a map with keys :x. :y, the cartesian coordinates at the point
   `radius` distance at `theta` (degrees) angle from a point at
   cartesian coordinates `cx`, `cy`."
  [cx cy radius theta]
  (let
   [in-radians (/ (* (- theta 90) PI) 180.0)]
    {:x (+ cx (* radius (cos in-radians)))
     :y (+ cy (* radius (sin in-radians)))}))

(defn describe-arc
  "Return as a string an SVG path definition describing an arc centred
   at `cx`, cy` starting at `start-angle` and ending at `end-angle` (both
   angles in degrees)."
  [cx cy radius start-angle end-angle]
  (let
   [start (polar-to-cartesian cx cy radius start-angle)
    end (polar-to-cartesian cx cy radius end-angle)
    large-arc? (if (<= (- end-angle start-angle) 180) 0 1)
    sweep (if (> end-angle start-angle) 1 0)]
    (join " " ["M" (:x start) (:y start) "A" radius radius 0 large-arc? sweep (:x end) (:y end)])))

(defn draw-segment
  [datum diameter]
  (let [r' (/ diameter 2)
        thickness (/ r' (:ring datum));; (/ 1 (pow 3 (:ring datum)))
        radius (- r' (/ thickness 2))
        start-angle (* (:left datum) 360)
        end-angle (* (:right datum) 360)
        id (str (:id datum) "-segment")
        path-data (describe-arc r' r' radius start-angle end-angle)]
;;    (println (format "Id: %s; radius: %s; start: %s; end: %s; thickness %s" id radius start-angle end-angle thickness))
    [:g {:id (str id "group")}
     [:path {:class "rsvggraph-segment"
             :id id
             :style {:fill "none"
                     :stroke (:colour datum)
                     :stroke-width thickness}
             :d path-data}]
     [:text [:textPath {:href (str "#" id)
                        :path path-data} (:label datum)]]]))

(defn flatten-data
  [data]
  (cond (empty? (:children data)) data
        :else (flatten (cons (dissoc data :children) (map flatten-data (:children data))))))

(def ^:dynamic *background* "white")

(def ^:dynamic *foreground* "black")

(defn data->svg
  [data diameter]
  (let [data' (normalise data)]
    [:svg {:xmlSpace "preserve"
           :overflow "visible"
           :viewBox (join " " [0 0 diameter diameter])
           :width (str diameter "px")
           :height (str diameter "px")
           :y "0px"
           :x "0px"
           :version "1.1"
           :id (:id data')
           :class (str "rsvggraph-graph")
           :xmlns "http://www.w3.org/2000/svg"}
     [:circle {:id (str (:id data') "-background") :cx (/ diameter 2) :cy (/ diameter 2) :r (/ diameter 2) :style {:fill "white"}}]
     [:text
      {:text-anchor "middle"
       :x (/ diameter 2)
       :y (/ diameter 2)
       :width (/ diameter 4)
       :id (str (:id data') "-title")
       :class "rsvggraph-value"} [:tspan (:label data) ": " (:quantity data)]]
     (map #(draw-segment % diameter) (flatten-data data))]))

(defn data->svg-file
  [data diameter path]
  (emit (data->svg data diameter)))

(defn data->html-file
  [data diameter path]
  (spit path (join "\n" ["<?xml version='1.0' encoding='UTF-8'?>" (replace (html (data->svg data diameter)) #"\> *\<" ">\n<")])))