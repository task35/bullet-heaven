(ns task35.bullet-heaven.enemy
  (:use arcadia.core
        arcadia.linear
        task35.bullet-heaven.core)
  (:require [arcadia.introspection :as int]
            [magic.api :as m]
            [timsg.scene-var :as sv])
  (:import [UnityEngine Rigidbody2D Transform CircleCollider2D
            Resources
            Gizmos
            Color
            GameObject Component
            Time Mathf
            Rigidbody
            Rigidbody2D ForceMode2D
            Quaternion Vector2]))

;; gonna box some
;; (defn qv2* [q v]
;;   ())

;; (UnityEngine.Quaternion/op_Multiply (aa 1 0 1 0) (v2 1))

;; this is throwing an error and it really shouldn't be
(defn bename [x name]
  (set! (.name (gobj x)) name)
  x)

(defmacro set-with! [obj [sym & props] & body]
  `(let [obj# ~obj
         ~sym (.. obj# ~@props)]
     (set! (.. obj# ~@props) (do ~@body))))

(defmacro sets! [obj & field-vals]
  (let [objsym (gensym "obj_")]
    `(let [~objsym ~obj]
       ~@(for [[field val] (partition 2 field-vals)]
           `(set! (. ~objsym ~field) ~val))
       ~objsym)))

;; (def player (object-named "Player"))

(m/defn rb-rotate-by ^Rigidbody2D [^Rigidbody2D rb ^Single deg]
  (.. rb (MoveRotation (+ deg (.rotation rb))))
  rb)

(m/defn lookahead-2d ^Vector2 [^Vector2 pt
                               ^Vector2 vel
                               ^float max-accel]
  (let [brake-time (/ (Mathf/Abs (.magnitude vel))
                      max-accel)]
    (v2+ pt (v2* vel brake-time))))

(m/defn normalized-2d [^Vector2 v]
  (.normalized v))

;; returns acceleration vector (for use with AddForce)
(m/defn approach-2d ^Vector2 [^Vector2 p1
                              ^Vector2 p2
                              ^Vector2 vel
                              ^float max-accel]
  (let [ahead (lookahead-2d p1 vel max-accel)
        lookahead-diff (v2- p2 ahead)]
    (v2* (normalized-2d lookahead-diff)
      max-accel)))

(defn attractor-motion [obj k]
  (with-cmpt obj [tr Transform]
    (m/faster
      (let [phase (float 2)
            targ (v3 (-> (Mathf/PerlinNoise (float Time/realtimeSinceStartup) 0)
                         (- 0.5)
                         (* 10))
                     (- (* 10 (/ (float
                                   (Mathf/PingPong
                                     (float Time/realtimeSinceStartup)
                                     (float phase)))
                                 phase))
                        5)
                     0)]
        (set! (.position tr) targ)))))

(sv/defgetter attractor
  ([] (-> (Resources/Load "Attractor")
          (UnityEngine.Object/Instantiate)
          (bename "Attractor")))
  ([enemy]
   (role+ enemy ::attractor-motion
     {:fixed-update #'attractor-motion})))

(defn follow-the-attractor-update [obj k]
  (with-cmpt obj [tr Transform
                  rb Rigidbody2D]
    (with-cmpt (attractor) [tr Transform]
      (m/faster
        (let [phase 5
              targ (v2 (.. tr position x) (.. tr position y))
              force (approach-2d
                      (.position rb)
                      targ
                      (.velocity rb)
                      10)]
          (.AddForce rb force ForceMode2D/Force)
          nil)
        ;; angle
        ;; (let [ang (.rb angularVelocity)
        ;;       ]
        ;;   (set! (.rotation rb)
        ;;     (Mathf)))
        ))))

(m/defn ang->v2 ^Vector2 [^float ang]
  (let [rad (* ang Mathf/Deg2Rad)]
    (v2 (Mathf/Cos rad) (Mathf/Sin rad))))

(m/defn move-forwards [^Rigidbody2D rb, ^Single force]
  (.AddForce rb
    (v2* (normalized-2d (ang->v2 (float (.rotation rb))))
      (float force))
    ForceMode2D/Force)
  nil)

(defn turn-towards-attractor-update [obj k]
  (with-cmpt obj [rb Rigidbody2D]
    (with-cmpt (attractor) [a-tr Transform]
      (m/faster
        (let [vel* (.angularVelocity rb)
              a-pos (.position a-tr)
              ang2 (Mathf/SmoothDampAngle
                     (.rotation rb)
                     (Vector2/Angle ;; for some stupid reason SignedAngle doesn't work
                       (v2 (.x a-pos) (.y a-pos))
                       Vector2/right) 
                     (by-ref vel*)
                     0.1
                     Mathf/Infinity
                     Time/deltaTime)]
          (sets! rb
            angularVelocity vel*
            rotation ang2)
          ;; (move-forwards rb 1)
          ))
      ;; (move-forwards rb 1)
      )))

(defn draw-forward-gizmo [obj k]
  (with-cmpt obj [rb Rigidbody2D]
    (let [{:keys [color]
           :or {color Color/blue}} (state obj k)]
      (m/faster
        (let [^Vector2 v (ang->v2 (.rotation rb))
              posv (.. rb position)]
          (set! Gizmos/color color)
          (Gizmos/DrawLine
            (v3 (.. posv x) (.. posv y) 0)
            (v3* (v3 (.. v x) (.. v y) 0) 2))
          nil)))))

(sv/defgetter enemy
  ([] (-> (Resources/Load "Enemy")
          (UnityEngine.Object/Instantiate)
          (bename "Enemy")))
  ([enemy]
   (with-cmpt enemy [rb Rigidbody2D
                     circ CircleCollider2D]
     (sets! circ enabled false)
     (sets! rb
       position (v2 0)
       velocity (v2 0)
       isKinematic false
       ;; simulated false
       ))
   ()
   (role+ enemy ::forward-gizmo
     {:state {:color Color/green}
      :on-draw-gizmos #'draw-forward-gizmo})
   (role+ enemy ::basic
     {:state {:rot-offset 90}
      :fixed-update #'turn-towards-attractor-update})
   enemy))

(sv/defgetter enemy2
  ([] (-> (Resources/Load "Enemy")
          (UnityEngine.Object/Instantiate)
          (bename "Enemy")))
  ([enemy]
   (role+ enemy ::basic
     {:fixed-update #'follow-the-attractor-update})))

(sv/defgetter enemies
  ([] (GameObject. "enemies"))
  ([obj]
   (doseq [x (children obj)]
     (retire x))
   ;; (let [proto (Resources/Load "Enemy")]
   ;;   (dotimes [i 10]
   ;;     (let [enemy (-> proto
   ;;                     (UnityEngine.Object/Instantiate)
   ;;                     (bename "Enemy"))]
   ;;       (with-cmpt enemy [tr Transform]
   ;;         (sets! tr
   ;;           localScale (v3 (* 3 (rand 1)))))
   ;;       (child+ obj enemy)
   ;;       (role+ enemy ::basic
   ;;         {:fixed-update #'follow-the-attractor-update})))
   ;;   obj)
   ))

