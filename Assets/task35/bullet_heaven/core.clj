(ns task35.bullet-heaven.core
  (:use arcadia.core
        arcadia.linear)
  (:import [UnityEngine Camera GameObject Vector2 Rigidbody2D]))

(defn move-2d [^GameObject go ^Vector2 v]
  (let [rb (cmpt go Rigidbody2D)
        position (.position rb)]
    (.. rb
        (MovePosition
          (v2+ position v)))))

(defn visible? [^GameObject go]
  (let [vp (.WorldToViewportPoint Camera/main 
                                  (.. go transform position))]
    (and (< 0 (.x vp) 1)
         (< 0 (.y vp) 1))))