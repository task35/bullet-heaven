(ns task35.bullet-heaven.core
  (:use arcadia.core
        arcadia.linear)
  (:require [magic.api :as m])
  (:import [UnityEngine GameObject Vector2 Rigidbody2D]))

;; throws exception when we try to make m/defn
(defn move-2d [^GameObject go ^Vector2 v]
  (let [rb (cmpt go Rigidbody2D)
        position (.position rb)]
    (.. rb
      (MovePosition
        (v2+ position v)))))
