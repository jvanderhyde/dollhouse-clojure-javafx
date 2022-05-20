(ns bleep.core
  (:gen-class)
  (:require [cljfx.api :as fx])
  (:import [javafx.application Platform]))

(def *state
  (atom {:x 100, :y 200}))

(defn circle [{:keys [x y]}]
  {:fx/type :circle
   :radius 20
   :fill javafx.scene.paint.Color/ALICEBLUE
   :translate-x x
   :translate-y y})

(defn root [{:keys [x y]}]
  {:fx/type :stage
   :showing true
   :title "Bleep"
   :width 800
   :height 600
   :scene {:fx/type :scene
           :fill javafx.scene.paint.Color/DARKSLATEGRAY
           :root {:fx/type :group
                  :children [{:fx/type circle :x x :y y}]}}})

(defn do-one-step [step]
  (case (first step)
    :move (let [[_ dir dist] step]
            (case dir
              :left (swap! *state update :x #(- % dist))
              :right (swap! *state update :x #(+ dist %))
              :up (swap! *state update :y #(- % dist))
              :down (swap! *state update :y #(+ dist %))))
    :start (let [[_ delayed-action] step]
             (deref delayed-action))
    :no-op nil
    nil))

(def *animators
  (atom {}))

(defn start-animator [steps]
  (let [a (proxy [javafx.animation.AnimationTimer] []
            (handle [now]
              (if-let [[x & next] (get @*animators this)] 
                (do
                  (do-one-step x)
                  (swap! *animators assoc this next))
                (do 
                  (.stop this)
                  (swap! *animators dissoc this))
                )))]
    (swap! *animators assoc a steps)
    (.start a)
    a))

(defn move-steps [dir dist time]
  (let [frames (max (* time 60) 1)]
    (repeat frames [:move dir (/ dist frames)])))

(defn move [dir dist time]
  (start-animator (move-steps dir dist time)))

(defn running? [action]
  (let [steps (get @*animators action)]
    (and steps (not-empty steps))))

(defn wait-for-action-steps [action]
  (lazy-seq
   (if (running? action)
     (cons [:no-op] (wait-for-action-steps action))
     (list))))

(defn wait-for-action [action]
  (start-animator (wait-for-action-steps action)))

(defn do-and-wait-steps [action delayed-action]
  (if (running? action)
    (lazy-seq
     (cons [:no-op] (do-and-wait-steps action delayed-action)))
    (wait-for-action-steps (deref delayed-action))))

(defn do-and-wait [action delayed-action]
  (start-animator (do-and-wait-steps action delayed-action)))

(defn do-nothing []
  (start-animator (list)))

(defn do-together [& actions]
  (reduce do-and-wait (do-nothing) (map atom actions)))

(defmacro do-in-order
  ([] `(do-nothing))
  ([x] x)
  ([x & next]
   `(do-and-wait ~x (delay (do-in-order ~@next)))))

(def renderer
  (fx/create-renderer
   :middleware (fx/wrap-map-desc assoc :fx/type root)))

(defn square []
  (do-in-order
   (move :down 100 0.25)
   (move :right 100 0.25)
   (move :up 100 0.25)
   (move :left 100 0.25)))

(defn squiral [n]
  (do-in-order
   (move :down n 0.25)
   (move :right (+ n 1) 0.25)
   (move :up (+ n 2) 0.25)
   (move :left (+ n 3) 0.25)
   (squiral (+ n 4))))

(defn -main [& args]
  (Platform/setImplicitExit true) ;exit JavaFX when last window is closed
  (fx/mount-renderer *state renderer) ;show the window
  (do-in-order
   (do-in-order
    (move :up 100 1)
    (move :right 200 2)
    (move :down 100 1))
   (move :left 200 2)
   (do-together
    (move :up 100 2)
    (move :right 200 3))
   (square)
   (squiral 1)))

