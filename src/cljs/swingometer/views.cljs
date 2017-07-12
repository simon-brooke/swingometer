(ns swingometer.views
  (:require [re-frame.core :as    re-frame]
            [re-com.core   :refer [h-box v-box box gap line label title progress-bar slider checkbox p single-dropdown]]
            [re-com.util   :refer [deref-or-value]]
            [swingometer.swingometer :refer [swingometer  swingometer-args-desc]]
            [swingometer.utils   :refer [panel-title title2 args-table github-hyperlink status-text]]
            [reagent.core  :as    reagent]))

(defn swingometer-demo
  []
  (let [model (reagent/atom {:snp {:id :snp :name "Scottish National Party" :colour "yellow" :votes 10}
                             :lab {:id :lab :name "Labour Party" :colour "red" :votes 10}
                             :con {:id :con :name "Conservative Party" :colour "blue" :votes 10}
                             :ld  {:id :ld :name "Liberal Democrats" :colour "GoldenRod" :votes 10}
                             :grn {:id :grn :name "Scottish Green Party" :colour "green" :votes 10}
                             :ukp {:id :ukp :name "United Kingdom Independence Party" :colour "DarkViolet" :votes 10}})]
    (fn
      []
      [v-box
       :size     "auto"
       :gap      "10px"
       :children [[panel-title "Swingometer"]
                  [h-box
                   :gap      "100px"
                   :children [[v-box
                               :gap      "10px"
                               :width    "450px"
                               :children [[title2 "Notes"]
                                          [status-text "Wildly experimental"]
                                          [p "An SVG swingometer intended to be useful in elections."]

                                          [title2 "Behaviour"]


                                          [args-table swingometer-args-desc]]]
                              [v-box
                               :gap      "10px"
                               :children [[title2 "Demo"]
                                          [v-box
                                           :gap      "20px"
                                           :children [[swingometer
                                                       :model     model
                                                       :height    600
                                                       :width     1000]
                                                      [title :level :level3 :label "Parameters"]
                                                      [h-box
                                                       :gap "10px"
                                                       :children [[box :align :start :child [:label (:name (:con (deref-or-value model)))]]
                                                                  [slider
                                                                   :model     (:votes (:con (deref-or-value model)))
                                                                   :min       0
                                                                   :max       1000
                                                                   :width     "200px"
                                                                   :on-change #(reset! model
                                                                                       (merge (deref-or-value model)
                                                                                              {:con (merge (:con (deref-or-value model))
                                                                                                           {:votes %})}))]
                                                                  [label :label (:votes (:con (deref-or-value model)))]]]
                                                      [h-box
                                                       :gap "10px"
                                                       :children [[box :align :start :child [:label (:name (:grn (deref-or-value model)))]]
                                                                  [slider
                                                                   :model     (:votes (:grn (deref-or-value model)))
                                                                   :min       0
                                                                   :max       1000
                                                                   :width     "200px"
                                                                   :on-change #(reset! model
                                                                                       (merge (deref-or-value model)
                                                                                              {:grn (merge (:grn (deref-or-value model))
                                                                                                           {:votes %})}))]
                                                                  [label :label (:votes (:grn (deref-or-value model)))]]]
                                                      [h-box
                                                       :gap "10px"
                                                       :children [[box :align :start :child [:label (:name (:lab (deref-or-value model)))]]
                                                                  [slider
                                                                   :model     (:votes (:lab (deref-or-value model)))
                                                                   :min       0
                                                                   :max       1000
                                                                   :width     "200px"
                                                                   :on-change #(reset! model
                                                                                       (merge (deref-or-value model)
                                                                                              {:lab (merge (:lab (deref-or-value model))
                                                                                                           {:votes %})}))]
                                                                  [label :label (:votes (:lab (deref-or-value model)))]]]
                                                      [h-box
                                                       :gap "10px"
                                                       :children [[box :align :start :child [:label (:name (:ld (deref-or-value model)))]]
                                                                  [slider
                                                                   :model     (:votes (:ld (deref-or-value model)))
                                                                   :min       0
                                                                   :max       1000
                                                                   :width     "200px"
                                                                   :on-change #(reset! model
                                                                                       (merge (deref-or-value model)
                                                                                              {:ld (merge (:ld (deref-or-value model))
                                                                                                           {:votes %})}))]
                                                                  [label :label (:votes (:ld (deref-or-value model)))]]]
                                                      [h-box
                                                       :gap "10px"
                                                       :children [[box :align :start :child [:label (:name (:snp (deref-or-value model)))]]
                                                                  [slider
                                                                   :model     (:votes (:snp (deref-or-value model)))
                                                                   :min       0
                                                                   :max       1000
                                                                   :width     "200px"
                                                                   :on-change #(reset! model
                                                                                       (merge (deref-or-value model)
                                                                                              {:snp (merge (:snp (deref-or-value model))
                                                                                                           {:votes %})}))]
                                                                  [label :label (:votes (:snp (deref-or-value model)))]]]
                                                      [h-box
                                                       :gap "10px"
                                                       :children [[box :align :start :child [:label (:name (:ukp (deref-or-value model)))]]
                                                                  [slider
                                                                   :model     (:votes (:ukp (deref-or-value model)))
                                                                   :min       0
                                                                   :max       1000
                                                                   :width     "200px"
                                                                   :on-change #(reset! model
                                                                                       (merge (deref-or-value model)
                                                                                              {:ukp (merge (:ukp (deref-or-value model))
                                                                                                           {:votes %})}))]
                                                                  [label :label (:votes (:ukp (deref-or-value model)))]]]
                                                      ]]]]]]]])))


;; core holds a reference to panel, so need one level of indirection to get figwheel updates
(defn panel
  []
  [swingometer-demo])


(defn main-panel []
  (fn []
    [v-box
     :height "100%"
     :children [[swingometer-demo]]]))
