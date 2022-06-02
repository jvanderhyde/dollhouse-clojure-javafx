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

(defn simulate-one-step [step]
  (case (first step)
    :move (let [[_ dir dist] step]
            (case dir
              :left (swap! *state update :x #(- % dist))
              :right (swap! *state update :x #(+ dist %))
              :up (swap! *state update :y #(- % dist))
              :down (swap! *state update :y #(+ dist %))))
    :no-op @*state))

(def *animators
  (atom {}))

(defn update-one-step [action]
  (let [[step & next] (get @*animators action)]
    (if step
      (case (first step)
        :join (let [[_ delayed-action] step
                    a (deref delayed-action)]
                (swap! *animators assoc a (concat (get @*animators a) next))
                (swap! *animators assoc action (list))
                (recur a))
        :eval (let [[_ f x] step
                    a (f x)]
                (swap! *animators assoc a (concat (get @*animators a) next))
                (swap! *animators assoc action (list))
                (recur a))
        (do
          (swap! *animators assoc action next)
          (simulate-one-step step)))
      (do
        (swap! *animators dissoc action)
        (.stop action)))))

(defn start-animator [steps]
  (let [a (proxy [javafx.animation.AnimationTimer] []
            (handle [now]
              (update-one-step this)))]
    (swap! *animators assoc a steps)
    (.start a)
    a))

(defn move-steps [dir dist time]
  (let [frames (max (* time 60) 1)]
    (repeat frames [:move dir (/ dist frames)])))

(defn move [dir dist time]
  (println "move" dir dist time)
  (start-animator (move-steps dir dist time)))

(defn running? [action]
  (let [steps (get @*animators action)]
    (and steps (not-empty steps))))

(defn wait-for-action-steps [action]
  (lazy-seq
   (if (running? action)
     (cons [:no-op] (wait-for-action-steps action))
     (list))))

(defn do-and-wait-steps [action1 action2]
  (if (running? action1)
    (lazy-seq
     (cons [:no-op] (do-and-wait-steps action1 action2)))
    (wait-for-action-steps action2)))

(defn do-and-wait [action1 action2]
  (start-animator (do-and-wait-steps action1 action2)))

(defn do-nothing []
  (start-animator (list)))

(defn do-together [& actions]
  (println (str "do-together (" (count actions) " actions)"))
  (reduce do-and-wait (do-nothing) actions))

(defn do-delays-in-order-steps [delayed-actions]
    (map (fn [x] [:join x]) delayed-actions))

(defn do-delays-in-order [delayed-actions]
  (println (str "do-in-order (" (count delayed-actions) " actions)"))
  (start-animator (do-delays-in-order-steps delayed-actions)))

(defmacro add-delays [items] 
  (cons 'list 
        (loop [items items dels []] 
          (if (empty? items) 
            dels 
            (recur (next items) (conj dels (list 'delay (first items))))))))

(defmacro do-in-order
  ([] `(do-nothing))
  ([& new-actions] 
   `(do-delays-in-order (add-delays ~new-actions))))

(defn for-all-in-order-steps [action-fn coll]
  (map (fn [x] [:eval action-fn x]) coll))

(defn for-all-in-order-fn [action-fn coll]
  (println (str "for-all-in-order (" (count coll) " items)"))
  (start-animator (for-all-in-order-steps action-fn coll)))

(defmacro for-all-in-order [params coll action]
  `(for-all-in-order-fn (fn ~params ~action) ~coll))

(defn for-all-together-fn [action-fn coll]
  (println (str "for-all-together (" (count coll) " items)"))
  (reduce do-and-wait (do-nothing) (map action-fn coll)))

(defmacro for-all-together [params coll action]
  `(for-all-together-fn (fn ~params ~action) ~coll))

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

(defn move-with-bounces [vx vy]
  (do-in-order
   (do-together
    (move :right vx 0)
    (move :down vy 0))
   (cond
     (or (< (:x @*state) 20) (> (:x @*state) 780))
     (move-with-bounces (- vx) vy)
     (or (< (:y @*state) 20) (> (:y @*state) 580))
     (move-with-bounces vx (- vy))
     :else
     (move-with-bounces vx vy))))

(defn test-do-in-order []
  (do-in-order
   (do-in-order
    (move :up 100 1)
    (move :right 200 2)
    (move :down 100 1))
   (move :left 200 2)
   (do-together
    (move :up 100 2)
    (move :right 200 3))))

(defn test-for-all-in-order []
  (for-all-in-order 
   [d] [:up :right :down :right :up :right] 
   (move d 100 0.5)))

(defn test-for-all-together []
  (for-all-together
   [d] [:down :down :down :right :up :up]
   (move d 100 1)))

(defn -main [& args]
  (Platform/setImplicitExit true) ;exit JavaFX when last window is closed
  (fx/mount-renderer *state renderer) ;show the window

  (test-for-all-together))

