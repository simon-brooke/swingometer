(ns rsvggraph.core
  (:require [clojure.math :refer [cos floor PI sin]]
            [clojure.string :refer [join replace]]
            [clojure.xml :refer [emit]]
            [hiccup2.core :refer [html]]
            [rsvggraph.data :refer [normalise]]))


(def ^:dynamic *background* "white")

(def ^:dynamic *foreground* "black")


(defn polar-to-cartesian
  "Return, as a map with keys :x. :y, the cartesian coordinates at the point
   `radius` distance at `theta` (degrees) angle from a point at
   cartesian coordinates `cx`, `cy`."
  [geometry]
  (let
   [in-radians (/ (* (- (:angle geometry) 90) PI) 180.0)]
    {:x (+ (:cx geometry) (* (:radius geometry) (cos in-radians)))
     :y (+ (:cy geometry) (* (:radius geometry) (sin in-radians)))}))

(defn describe-arc
  "Return as a string an SVG path definition describing an arc centred
   at `cx`, cy` starting at `start-angle` and ending at `end-angle` (both
   angles in degrees)."
  [geometry start-angle end-angle]
  (let
   [start (polar-to-cartesian (assoc geometry :angle start-angle))
    end (polar-to-cartesian (assoc geometry :angle end-angle))
    large-arc? (if (<= (- end-angle start-angle) 180) 0 1)
    radius (:radius geometry)
    sweep (if (> end-angle start-angle) 1 0)]
    (join " "
          ["M" (:x start) (:y start) "A" radius radius 0 large-arc? sweep
           (:x end) (:y end)])))

(def ^:dynamic *minor-segment-threshold* 0.07)

(defn- minor-segment? [datum]
  (< (- (:right datum) (:left datum)) *minor-segment-threshold*))

(defn- font-size [thickness]
  (int (* 0.15 thickness)))

(defn- text-path [datum tp-id geometry thickness start-angle end-angle]
  [:path {:class "rsvggraph-text-path"
          :id tp-id
          :style {:fill "none"
                  :stroke "none"}
          :d (let [angle (if (> (+ start-angle end-angle) 360) start-angle end-angle)
                   radius (:radius geometry)
                   end (polar-to-cartesian (assoc geometry :radius (* 1.2 (:radius geometry)) :angle angle))
                   height (int (:y end))]
               (if (minor-segment? datum)
                 (if (> angle 180)
                   (format "M %d %d L %d %d" (- (int (:x end)) (* (count (:label datum)) (font-size thickness))) height
                         (int (:x end)) height)
                   (format "M %d %d L %d %d" (int (:x end)) height (:width geometry) height))
                 (describe-arc (assoc geometry :radius (- radius (* 0.9 thickness)))
                               start-angle end-angle)))}])

(defn- label-indicator-path [geometry angle]
  (let [start (polar-to-cartesian (assoc geometry :angle angle))
        end (polar-to-cartesian 
             (assoc geometry :radius (* 1.2 (:radius geometry)) :angle angle))]
    (format "M %d %d L %d %d" (int (:x start)) (int (:y start))
            (int (:x end)) (int (:y end)))))

(defn- label-indicator [datum geometry start-angle end-angle]
  (when (minor-segment? datum)[:path {:class "rsvggraph-minor-label-indicator" :style {:fill "none" :stroke *foreground* :stroke-width "thin"}
             :d (label-indicator-path geometry (if (> (+ start-angle end-angle) 360) start-angle end-angle))}]))

(defn draw-segment
  [datum geometry]
  (println (format "Radius: %s; ring: %s." (:radius geometry) (:ring datum)))
  (let [thickness (/ (:radius geometry) (:ring datum));; (/ 1 (pow 3 (:ring datum)))
        radius (- (:radius geometry) (/ thickness 2))
        start-angle (* (:left datum) 360)
        end-angle (* (:right datum) 360)
        id (str (:id datum) "-segment")
        tp-id (str "tp-" id)
        path-data (describe-arc (assoc geometry :radius radius)
                                start-angle end-angle)]
    [:g {:id (str id "group")}
     [:path {:class "rsvggraph-segment"
             :id id
             :style {:fill "none"
                     :stroke (:colour datum)
                     :stroke-width thickness}
             :d path-data}]
     (text-path datum tp-id geometry thickness start-angle end-angle)
     (label-indicator datum geometry start-angle end-angle)
     [:text {:style {:fill *foreground*
                     :font-family "sans-serif"
                     :font-weight "bold"
                     :font-size (str (font-size thickness))}}
      [:textPath {:xlink:href (str "#" tp-id)
                  :startOffset "2%"} [:tspan (:label datum)]]]]))

;; <text
;;      xml:space="preserve"
;;      style="font-style:normal;font-variant:normal;font-weight:bold;font-stretch:normal;font-size:32px;font-family:sans-serif;-inkscape-font-specification:'sans-serif Bold';fill:#ffcc00;stroke-width:4.9852"
;;      id="text562"><textPath
;;        xlink:href="#no-show-segment"
;;        id="textPath770"><tspan
;;          id="tspan560">Did not vote</tspan></textPath></text>

(defn flatten-data
  [data]
  (cond (empty? (:children data)) data
        :else (flatten (cons (dissoc data :children) (map flatten-data (:children data))))))

(defn- circle
  [data geometry]
  [:circle {:id (str (:id data) "-background")
            :cx (:cx geometry)
            :cy (:cy geometry)
            :r (:radius geometry)
            :style {:fill "white"}}])

(defn- cantre-label [data geometry]
  [:text
   {:text-anchor "middle"
    :x (:cx geometry)
    :y (:cy geometry)
    :width (/ (:diameter geometry) 4)
    :id (str (:id data) "-title")
    :style {:font-family "sans-serif"
            :font-weight "bold"}
    :class "rsvggraph-value"} [:tspan (:label data) ": " (:quantity data)]])

(defn base-geometry
  [diameter]
  (let [height (* 1.5 diameter)
        width (* 2 diameter)]
    {:cx (/ width 2)
     :cy (/ height 2)
     :diameter diameter
     :height height
     :radius (/ diameter 2)
     :width width}))

(defn data->svg
  [data diameter]
  (let [data' (normalise data)
        height (* 2 diameter)
        width (* 3 diameter)
        geometry (base-geometry diameter)]
    [:svg {:xmlSpace "preserve"
           :overflow "visible"
           :viewBox (join " " [0 0 height height])
           :width (str width "px")
           :height (str height "px")
           :y "0px"
           :x "0px"
           :version "1.1"
           :id (:id data')
           :class (str "rsvggraph-graph")
           :xmlns "http://www.w3.org/2000/svg"
           :xmlns:xlink "http://www.w3.org/1999/xlink"}
     (circle data' geometry)
     (cantre-label data' geometry)
     (map #(draw-segment % geometry) (flatten-data data'))]))

(defn data->svg-file
  [data diameter path]
  (emit (data->svg data diameter)))

(defn data->html-file
  [data diameter path]
  (spit path (join "\n"
                   ["<?xml version='1.0' encoding='UTF-8'?>"
                    (replace (html (data->svg data diameter)) #"\> *\<" ">\n<")])))