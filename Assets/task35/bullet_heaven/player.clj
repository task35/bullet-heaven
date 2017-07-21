(ns task35.bullet-heaven.player
  (:use arcadia.core
        arcadia.linear)
  (:import [UnityEngine GameObject Input Rigidbody2D]))

(def max-velocity 1)
(def acceleration 0.001)

(defn mover [^GameObject go k]
  (let [horizontal (Input/GetAxis "Horizontal")
        vertical (Input/GetAxis "Vertical")
        rb (cmpt go Rigidbody2D)
        position (.. rb position)
        v (state go k)]
    (.. rb (MovePosition
             (v2+ position
                  (v2* (v2 horizontal vertical) v))))
    (when (< v max-velocity)
      (set-state!
        go k (if Input/anyKey
               (+ v acceleration)
               0.1)))))

(def movement-role
  {:state 0.1
   :fixed-update #'mover})

;; (role+ (object-named "Player") ::movement movement-role)