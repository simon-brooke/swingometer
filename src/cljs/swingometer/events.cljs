(ns swingometer.events
  (:require [re-frame.core :as re-frame]
            [swingometer.db :as db]))

(re-frame/reg-event-db
 :initialize-db
 (fn  [_ _]
   db/default-db))
