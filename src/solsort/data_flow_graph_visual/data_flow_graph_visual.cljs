(ns solsort.data-flow-graph-visual.data-flow-graph-visual
  (:require-macros
   [cljs.core.async.macros :refer [go go-loop alt!]]
   [reagent.ratom :as ratom :refer  [reaction]])
  (:require
   [cljs.reader]
   [solsort.toolbox.setup]
   [solsort.toolbox.appdb :refer [db db! db-async!]]
   [solsort.toolbox.ui :refer [input select]]
   [solsort.util
    :refer
    [<ajax <seq<! js-seq load-style! put!close!
     parse-json-or-nil log page-ready render dom->clj]]
   [reagent.core :as reagent :refer []]
   [clojure.string :as string :refer [replace split blank?]]
   [cljs.core.async :refer [>! <! chan put! take! timeout close! pipe]]))

(def top-margin 30)
(def size 60)
(def font-size (* .2 size))
(def height (+ 150 (rand-int 200)))

(def double-rows (js/Math.floor (/ (- (* 2 height) top-margin) size)))
(defn box-pos [n]
  (let [x2 (js/Math.floor (/ n double-rows))
        x (* x2 2)
        y (js/Math.floor (mod n double-rows))
        second (>= y (/ double-rows 2))
        y (if second (- y -0.5 (js/Math.ceil (/ double-rows 2))) y)
        x (if second (inc x) x)]
    (log 'double-row double-rows x2 y)
    [(* x size) (+ top-margin (* y size))]))

(defn hash-color-light [s]
  (str "#"
       (-> s
           (hash)
           (bit-and 0xffffff)
           (bit-or 0x1b0b0b0)
           (.toString 16)
           (.slice 1))))

(def graph-size (+ 10 (rand-int 90)))
(def ops {"+" + "-" - "*" * "/" /})
#_(defn rand-args [i]
    (for [j (range (inc (rand-int 5)))] (rand-int i)))
(defn rand-args [i]
  (for [j (range (inc (rand-int 3)))]
    (js/Math.floor (* i (- 1 (js/Math.pow (js/Math.random) 2))))))
(def graph
  (loop [g [{:val -1 :fn "Immediate" :args [] :i 0}]
         i 1]
    (if (= i graph-size)
      g
      (let [e {:fn (rand-nth ["+" "-" "*" "/"]) :args (rand-args i)
               :i i}
            e (assoc e :val (apply (ops (:fn e)) (map #(:val (g %)) (:args e))))
           ;e (assoc e :val i)
]
        (recur (conj g e) (inc i))))))

(log (js/JSON.stringify (clj->js graph)))
(defn flow-lines []
  (into
   [:svg {:height height :width (* size 2 (+ .25 (js/Math.ceil (/ graph-size double-rows))))}]
   (apply
    concat
    (for [node graph]
      (for [i (range (count (:args node)))]
        (let [src (nth (:args node) i)
              dst (:i node)
              [x0 y0] (box-pos src)
              x0 (+ x0 size)
              y0 (+ y0 (* .25 size))
              xpos (/ (inc i) (inc (count (:args node))))
              [x1 y1] (box-pos dst)
              x1 (+ x1 (* xpos size))]
          [:path
           {:d (str "M " x0 " " y0 " "
                    "C "
                    (+ x0 (* 1 size)) " " y0 " "
                    x1 " " (- y1 (* 0.25 size) (* 0.05 size (- dst src))) " "
                    x1 " " y1 " ")
            :stroke-width 1.5
            :stroke (hash-color-light (str (nth (:args node) i)))
            :fill "none"}]))))
   #_[:path {:d "M 100 350 c 100 100 150 -300 300 0"
             :stroke-width 5
             :stroke :black
             :fill "none"}]))
(defn data-boxes []
  (into
   [:div]
   (for [node graph]
     (let [[x y] (box-pos (:i node))]
       (log node x y)
       [:div
        {:style
         {:display :inline-block
          :position :absolute
          :font-size font-size
          :line-height 1
          :width size
          :height (* .5 size)
          :text-align "center"
          :border-radius (* .1 size)
          :overflow :hidden
          :left x
          :top y}}
        [:div
         {:style {:height (* .25 size)
                  :background "rgba(240,255,255,0.9)"}}
         (:fn node)]
        [:div
         {:style {:background "rgba(255,240,255,0.9)"}}
         (:val node) [:br] "\u00a0"]]))))
(render [:div
         [:h1 "Data flow graph visual"]
         [:div {:style {:margin "1ex"}} "This is an experiment with how to make a compact visual representation of a data flow graph. Below is a random data flow graph, with values and operations in nodes, and lines showing the flow. This is an experiment towards better data-calc/touch-lang."]
         [:div {:style {:margin "1ex"}} "Scroll right in the graph below to see more, and reload to see a new one."]
         [:div {:style [:height height]}]
         [:div
          {:style {:position :fixed
                   :display :inline-block
                   :width "100%"
                   :background :black
                   :overflow :auto
                   :bottom 0}}
          [data-boxes]
          [flow-lines]]])

