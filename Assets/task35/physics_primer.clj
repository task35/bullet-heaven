(ns task35.physics-primer
  (:use arcadia.core
        arcadia.linear
        clojure.repl
        clojure.pprint)
  (:require [arcadia.introspection :as int]
            [magic.api :as m]
            [timsg.scene-var :as sv])
  (:import [UnityEngine Rigidbody2D Transform CircleCollider2D
            Font
            Resources
            Vector3
            Gizmos
            Color
            GameObject Component
            Time Mathf
            Rigidbody
            Rigidbody2D ForceMode2D
            Quaternion Vector2]
           [UnityEngine.UI Text]))

;; ============================================================
;; utilites

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

(defn load-obj [path name]
  (-> path
      Resources/Load
      UnityEngine.Object/Instantiate
      (bename name)))

;; PUT THIS IN CORE!!!
(defn roles+ [obj m]
  (reduce-kv
    (fn [obj role-key role]
      (role+ obj role-key role))
    obj
    m))

;; OUR update-state (not core's!)
(defn update-state [obj k f & args]
  (set-state! obj k
    (apply f (state obj k) args)))

;; ============================================================
;; physics utilities

;; ============================================================
;; gizmos utilities

(m/defn normalized [^Vector2 v]
  (.normalized v))

(m/defn normalized3d [^Vector3 v]
  (.normalized v))

(m/defn gorp ^Vector3 [^Vector3 v]
  v)

(defn draw-cardinal-gizmo [obj k]
  (let [{:keys [color magnitude direction]
         :or {color Color/white
              magnitude 1
              direction :forward}} (state obj k)]
    (with-cmpt obj [tr Transform]
      (m/faster
        (let [^Vector3 v (cond
                           (= direction :forward) (.forward tr),
                           (= direction :right) (.right tr)
                           (= direction :up) (.up tr)
                           ;; MAGIC BUG: REALLY shouldn't have to do this:
                           (instance? Vector3 direction) (gorp direction)
                           ;; :else (v3 0)
                           )]
          (set! Gizmos/color color)
          (Gizmos/DrawLine
            (.position tr)
            (v3+ (.position tr)
                 (v3* v magnitude)))
          ;; nil
          )))))

(defn position-gizmos+ [obj]
  (let [f (fn [m direction color]
            (let [k (keyword "task35.physics-primer",
                      (str "position-gizmo-" (name direction)))]
              (assoc m k
                {:state {:color color :direction direction}
                 :on-draw-gizmos #'draw-cardinal-gizmo
                 :on-draw-gizmos-ks [k]})))]
    (roles+ obj
      (-> {}
          (f :forward Color/blue)
          (f :up Color/green)
          (f :right Color/red)))))

;; ============================================================
;; demos

;; for now, just comment these in/out to activate various demos

(comment)
(sv/defgetter origin-marker
  ([] (GameObject. "origin-marker"))
  ([origin-marker]
   (position-gizmos+ origin-marker)
   ;; (retire origin-marker)
   ))

;; ------------------------------------------------------------
;; 2d rotation

;; object transform "up" is forward here! ("forward" is z, into the camera)

(comment)
(defn basic-rotation [obj k]
  (let [{:keys [velocity]} (state obj k)]
    (with-cmpt obj [rb Rigidbody2D]
      (m/faster
        (set! (.rotation rb) (+ (.rotation rb) velocity))
        nil))))

(comment
  (sv/defgetter basic-rotation-demo
    ([]
     (load-obj "OrangeShip" "basic-rotation-demo"))
    ([basic-rotation-demo]
     (position-gizmos+ basic-rotation-demo)
     (role+ basic-rotation-demo ::basic-rotation
       {:state {:velocity 0.5}
        :update #'basic-rotation})
     ;; retired:
     (retire basic-rotation-demo))))

;; ------------------------------------------------------------
;; torqued rotation

(defn torqued-rotation [obj k]
  (let [{:keys [force max-vel]} (state obj k)]    
    (with-cmpt obj [rb Rigidbody2D]
      (when (< (.angularVelocity rb) max-vel)
        (.AddTorque rb force ForceMode2D/Force)))))

(sv/defgetter torqued-rotation-demo
  ([]
   (load-obj "OrangeShip" "torqued-rotation-demo"))
  ([trd]
   (position-gizmos+ trd)
   (role+ trd ::torqued-rotation
     {:state {:force 0.2
              :max-vel 90}
      :update #'torqued-rotation})
   ;; retired:
   (retire trd)))

;; ------------------------------------------------------------
;; torqued rotation to rest

;; may need to further tweak this depending on what you want to count as zero
(m/defn angle-to-unit-v2 [^float angle]
  (let [rad (* Mathf/Deg2Rad angle)]
    (v2 (Mathf/Cos rad) (Mathf/Sin rad))))

(m/defn rads-to-x-axis [^Vector2 v]
  (Mathf/Atan2 (.y v) (.x v)))

(m/defn angle-to-x-axis [^Vector2 v]
  (* (rads-to-x-axis v) Mathf/Rad2Deg))

;; isn't in slightly older versions of unity
(m/defn signed-angle [^Vector2 from, ^Vector2 to]
  (- (angle-to-x-axis to)
     (angle-to-x-axis from)))

;; MAGIC BUG: dealing with (), at least as the return value
;; (m/defn squiz-giz [obj k]
;;   (let [{:keys [v]} (obj k)]
;;     (with-cmpt obj [tr Transform]
;;       )))

(m/defn v2->v3 [^Vector2 v]
  (v3 (.x v) (.y v) 0))

;; again, this is all 2d
;; careful that force is not 0, we'll divide by a multiple of it
(defn torqued-rotation-to-rest [obj k]
  (let [{:keys [force max-vel target-angle]} (state obj k)]    
    (with-cmpt obj [rb Rigidbody2D]      
      (let [vel (.angularVelocity rb)
            ;; Discount everything and assume force is just acceleration.
            ;; Then the following holds; see acceleration.nb for derivation.
            ;; braking-distance (/ (Unity))
            ;; (/ (Mathf/Pow vel 2)
            ;;    (* 2 force))
            brake-time (/ (Mathf/Abs vel)
                          force)
            lookahead (+ (.rotation rb) (* vel brake-time))
            ;; I think we want the signed angle here
            lookahead-diff (signed-angle
                             (angle-to-unit-v2 lookahead)
                             (angle-to-unit-v2 target-angle))]
        (update-state obj ::torqued-rotation-gizmos assoc :direction
          (v2->v3 (angle-to-unit-v2 lookahead))
          ;; (v3 1 -1 0)
          ;; (v2->v3 (angle-to-unit-v2
          ;;           45
          ;;           ;;lookahead
          ;;           ))
          )
        (when true ;; (< vel max-vel)
          (.AddTorque rb
            (* (Mathf/Sign lookahead-diff)
               (Mathf/Min force (Mathf/Abs lookahead-diff)))
            ForceMode2D/Force)
          nil)))))

(defn obj-rotation [obj]
  (with-cmpt obj [rb Rigidbody2D]
    (.rotation rb)))

(comment
  (sv/defgetter torqued-rotation-to-rest-demo
    ([]
     (load-obj "OrangeShip" "torqued-rotation-demo"))
    ([trr]
     (position-gizmos+ trr)
     (roles+ trr
       {::torqued-rotation-gizmos
        {:state {:color (Color. 1 0.7 0)}
         :on-draw-gizmos #'draw-cardinal-gizmo}
        ;; ::torqued-rotation
        ;; {:state {:force 90
        ;;          :max-vel 360
        ;;          :target-angle 0}
        ;;  :fixed-update #'torqued-rotation-to-rest}
        }))))


;; ------------------------------------------------------------
;; ugh


;; this should probably be in impulse mode
(defn add-torque [obj t]
  (with-cmpt obj [rb Rigidbody2D]
    (.AddTorque rb t ForceMode2D/Force)))

;; just try some really simple torque stuff; how far does 1 torquum get us?
(sv/defgetter torque-2
  ([] (load-obj "OrangeShip", "torque-2"))
  ([t2]
   (with-cmpt t2 [rb Rigidbody2D]
     (sets! rb
       rotation 0
       angularVelocity 0)
     (position-gizmos+ t2))
   ;; retired:
   (retire t2)
   ))

;; ------------------------------------------------------------
;; so, phone it in

;; we can't even calculate the brake time reasonably. so do the stupid thing.
;; if we're going to overshoot, slow down, if we're going to undershoot, speed up.
(defn torque-shrug [obj k]
  (let [{:keys [force max-vel target-angle]} (state obj k)]    
    (with-cmpt obj [rb Rigidbody2D]
      (sets! rb
        rotation (mod (.rotation rb) 360))
      (let [vel (.angularVelocity rb)
            lookahead (+ (.rotation rb)
                         90 ; adjust for angling
                         (* 1 vel))
            lookahead-diff (signed-angle
                             (angle-to-unit-v2 lookahead)
                             (angle-to-unit-v2 target-angle))]
        (update-state obj ::lookahead-giz assoc
          :direction (v2->v3 (angle-to-unit-v2 lookahead)))
        (update-state obj ::lookahead-sum-giz assoc
          :direction (v2->v3 (angle-to-unit-v2 (+ lookahead-diff lookahead))))
        ;; sometimes it falls into a weird hell and tries to accelerate infinitely.
        ;; inflect this by framerate or whatever
        ;; also still falling into some weird holes
        (if (< 180 (Mathf/Abs vel))
          ;; in this case just slow down no matter what
          (.AddTorque rb
            (* -1
               (Mathf/Sign vel)
               (Mathf/Min force (* Mathf/Deg2Rad 180))))
          (.AddTorque rb
            (* (Mathf/Sign lookahead-diff)
               (Mathf/Min
                 force
                 (Mathf/Abs
                   (* Mathf/Deg2Rad lookahead-diff) ; or somethin
                                        ; oh shit that totally did it! Deg2Rad
                   )))
            ForceMode2D/Force))))))

;; works like a charm!

(sv/defgetter torque-3
  ([] (load-obj "OrangeShip", "torque-3"))
  ([t3]
   (with-cmpt t3 [rb Rigidbody2D]
     (sets! rb
       rotation 0
       angularVelocity 100)
     (position-gizmos+ t3)
     (roles+ t3
       {::lookahead-giz {:state {:color (Color. 1 0.7 0)
                                 :magnitude 2}
                         :on-draw-gizmos #'draw-cardinal-gizmo}
        ::lookahead-sum-giz {:state {:color (Color. 1 0 1)
                                     :magnitude 3}
                             :on-draw-gizmos #'draw-cardinal-gizmo}
        ::torque-shrug {:state {:force 10
                                :max-vel 1
                                :target-angle 90}
                        :fixed-update #'torque-shrug}}))))

;; still gets weird for extremes

;; ------------------------------------------------------------
;; text maneuvers (move up to utils)

(def arial (Resources/GetBuiltinResource Font "Arial.ttf"))

;; assume we already have the canvas (which we can't assume)
;; (sv/defgetter test-text
;;   ([]
;;    (if-let [canvas (object-named "Canvas")]
;;      (let [tobj (GameObject. "Test Text")]
;;        (child+ canvas tobj)
;;        tobj)
;;      (throw (Exception. "No canvas found"))))
;;   ([tobj]
;;    (with-cmpt tobj [txt Text]
;;      (sets! txt
;;        text "hi there buddy"
;;        font arial
;;        color Color/black)
;;      tobj)))


;; ------------------------------------------------------------
;; follow something around with text

;; (sv/defgetter text-ship
;;   ([]
;;    (load-obj "OrangeShip" "text-ship"))
;;   ([text-ship]
;;    (position-gizmos+ text-ship)))

;; (sv/defgetter follow-text
;;   ([]
;;    (if-let [canvas (object-named "Canvas")]
;;      (let [tobj (GameObject. "Test Text")]
;;        (child+ canvas tobj)
;;        tobj)
;;      (throw (Exception. "No canvas found"))))ar)




