(ns swingometer.swingometer
  (:require [clojure.string :as string]
            [re-com.core     :refer [h-box v-box box gap line label title slider checkbox p]]
            [re-com.box      :refer [flex-child-style]]
            [re-com.util     :refer [deref-or-value]]
            [re-com.validate :refer [number-or-string? css-style? html-attr? validate-args-macro]]
            [reagent.core    :as    reagent]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; swingometer: an experiment in animating SVG from re-frame.
;;;; Draws heavily on re-com..
;;;;
;;;; This program is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU General Public License
;;;; as published by the Free Software Foundation; either version 2
;;;; of the License, or (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
;;;; USA.
;;;;
;;;; Copyright (C) 2014 Simon Brooke
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ------------------------------------------------------------------------------------
;;  Component: swingometer
;; ------------------------------------------------------------------------------------

;;; It seems the defaults given here are just documentation; the defaults
;;; that are actually used are those given in the :or clause of the argument map.
(def swingometer-args-desc
  [{:name :model         :required true  :type "map | atom"
    :validate-fn map? :description "A map mapping keys to maps of the following structure: {:id :snp :name \"Scottish National Party\" :colour \"yellow\" :votes 1234}"}
   {:name :width         :required false :type "integer"                 :default "300"
    :validate-fn integer?          :description "a CSS width"}
   {:name :height        :required false :type "integer"                 :default "200"
    :validate-fn integer?          :description "a CSS height"}
   {:name :class         :required false :type "string"
    :validate-fn string?           :description "CSS class names, space separated, for the top-level SVG element"}
   {:name :frame-class   :required false :type "string"                 :default "snm-frame"
    :validate-fn string?           :description "CSS class names, space separated, for the frame"}
   {:name :hub-class     :required false :type "string"                 :default "snm-hub"
    :validate-fn string?           :description "CSS class names, space separated, for the hub"}
   {:name :scale-class   :required false :type "string"                 :default "snm-scale"
    :validate-fn string?           :description "CSS class names, space separated, for the scale"}
   {:name :id            :required false :type "string"                 :default "meter"
    :validate-fn string?           :description "Element id for this instance of the meter"}
   {:name :gradations    :reduired false :type "integer"                :default 5
    :validate-fn integer?          :description "Number of gradations to show on the scale, not counting the point."}
   {:name :style         :required false :type "CSS style map"
    :validate-fn css-style?        :description "CSS styles to add or override"}
   {:name :attr          :required false :type "HTML attr map"
    :validate-fn html-attr?        :description [:span "HTML attributes, like " [:code ":on-mouse-move"] [:br] "No " [:code ":class"] " or " [:code ":style"] "allowed"]}])


;; (defn abs
;;   "Return the absolute value of the (numeric) argument."
;;   [n] (max n (- n)))


;; the constant 140 represents the full sweep of the needle
;; from the left end of the scale to right end, in degrees.
(def full-scale-deflection 140)


(defn deflection
  "Return the linear deflection of a needle given this `value` on the
  range `min-value`...`total-votes`."
  [value min-value max-value]
  (let [range (- max-value min-value)
        zero-offset (/ (- 0 min-value) range)
        limited (min (max (+ zero-offset (/ value range)) 0) 1)]
    (* (- limited 0.5) full-scale-deflection)))


(defn polar-to-cartesian
  "Return, as a map with keys :x. :y, the cartesian coordinates at the point
   `radius` distance at `theta` (degrees) angle from a point at
   cartesian coordinates `cx`, `cy`."
  [cx cy radius theta]
  (let
    [in-radians (/ (* (- theta 90) (aget js/Math "PI")) 180.0)]
    {:x (+ cx (* radius (.cos js/Math in-radians)))
     :y (+ cy (* radius (.sin js/Math in-radians)))}))


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
    (string/join " " ["M" (:x start) (:y start) "A" radius radius 0 large-arc? sweep (:x end) (:y end)])))


(defn as-label
  "If this arg is a floating point number, format it to a reasonable width; else return it."
  [arg]
  (if
    (and (number? arg) (not (integer? arg)))
    (.toFixed arg 1)
    arg))


(defn gradation
  "Return as a string an SVG path definition describing a radial stroke from a center
  at `cx`, cy` starting at `min-radius` and extending to `max-radius`."
  [cx cy min-radius max-radius angle label]
  [:g {:class "snm-gradation"
       :transform (string/join " " ["rotate(" angle cx cy ")"])}
   [:path {:d (string/join
                " "
                ["M"
                 cx
                 (- cy
                    (+ min-radius
                       (* (- max-radius min-radius) 0.333)))
                 "L"
                 cx
                 (- cy max-radius)])}]
   [:text {:text-anchor "start"
           :x cx
           :y (- cy min-radius)} (as-label label)]])

(def model {:snp {:id :snp :name "Scottish National Party" :colour "yellow" :votes 100}
                             :lab {:id :lab :name "Labour Party" :colour "red" :votes 90}
                             :con {:id :con :name "Conservative Party" :colour "blue" :votes 80}
                             :ld  {:id :ld :name "Liberal Democrats" :colour "GoldenRod" :votes 70}
                             :grn {:id :grn :name "Scottish Green Party" :colour "green" :votes 60}
                             :ukp {:id :ukp :name "United Kingdom Independence Party" :colour "DarkViolet" :votes 50}})

(defn biggest-to-the-middle-sort
  "Sort this list of `maps` representing parties so that those with the most votes are in
  the middle."
  [maps]
  (let [first-sort (sort-by :votes maps)
        evens (take-nth 2 first-sort)
        odds (take-nth 2 (rest first-sort))]
    (concat evens (reverse odds))))


(defn recursively-draw-segments
  "Walk down a list of parties, returning a labelled SVG arc segment for each one.
  `still-to-do` is the (remainder of the) list of parties being scanned, should
  initially be the whole list;
  `done` is the parties which have been scanned, and should initially be `nil`.
  `total-votes` is the total number of votes for all parties.
  `cx` and `cy` are the cartesian coordinates of the centre of arc.
  `radius` is the radius of the arc."
  [still-to-do done total-votes cx cy radius]
  (if
    (empty? still-to-do) nil
    (let [votes-done (reduce + (map :votes done))
          start-angle (deflection votes-done 0 total-votes)
          party (first still-to-do)
          end-angle (deflection (+ (:votes party) votes-done) 0 total-votes)
          others (recursively-draw-segments (rest still-to-do) (cons party done) total-votes cx cy radius)
          vote-share (* (/ (:votes party) total-votes) 100)]
      (if (> vote-share 1)
        (cons [:g [:path {:class "snm-scale"
                          :id (str (:id party) "-segment")
                          :style {:stroke (:colour party)}
                          :d (describe-arc cx cy radius start-angle end-angle)}]
               (gradation cx cy (* radius 0.8) (* radius 1.1) start-angle
                          (str
                            (if (> vote-share 5) (name (:id party)) "")
                            (if (> vote-share 10) (str " " (as-label vote-share) "%"))))]
              others)
        others))))


(defn swingometer
  "Render an SVG swinging needle meter"
  [& {:keys [model width height class scale-class frame-class id style attr]
      :or   {width          300
             height         200
             scale-class    "snm-scale"
             frame-class    "snm-frame"
             id "meter"}
      :as   args}]
  {:pre [(validate-args-macro swingometer-args-desc args "swingometer")]}
  (let [model (deref-or-value model)
        mid-point-deflection (/ full-scale-deflection 2)
        cx (/ width 2)
        cy (* height 0.90)
        needle-length (* height 0.75)
        scale-radius (* height 0.7)
        gradation-inner (* height 0.55)
        gradations 5
        total-votes (reduce + (map #(:votes %) (vals model)))]
    [box
     :align :start
     :child [:div
             (merge
               {:class (str "swingometer  " class)
                :style (merge (flex-child-style "none")
                              {:width width :height height}
                              style)}
               attr)
             [:svg {:xmlSpace "preserve"
                    :overflow "visible"
                    :viewBox (string/join " " [0 0 width height])
                    :width (str width "px")
                    :height (str height "px")
                    :y "0px"
                    :x "0px"
                    :version "1.1"
                    :id id
                    :class (str "snm-meter " class)}
                            [:text
               {:text-anchor "middle"
                :x (/ width 2)
                :y (/ height 2)
                :width "100"
                :id (str id "-total-votes")
                :class "snm-value"}[:tspan (reduce + (map :votes (vals model)))]]
              [:path {:class scale-class
                      :id (str id "-scale")
                      :d (describe-arc cx cy scale-radius
                                       (- 0 mid-point-deflection)
                                       mid-point-deflection)}]
              (apply vector
                     (cons :g (recursively-draw-segments (biggest-to-the-middle-sort (vals model)) nil total-votes cx cy scale-radius)))
              [:rect {:class frame-class
                      :id (str id "-frame")
                      :x (* width 0.05) :y (* height .05) :height cy :width (* width 0.9)}]]]]))
