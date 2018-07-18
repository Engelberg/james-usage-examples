(ns lens.core
  (:refer-clojure :exclude [cond if-let when-let])
  (:require [better-cond.core :refer [cond if-let when-let defnc]]))

(defnc f [x y & {:keys [c d] :or {c 10 d 20}}]
  [x y c d])

