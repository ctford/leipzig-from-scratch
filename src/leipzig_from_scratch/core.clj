(ns leipzig-from-scratch.core
  (:require [overtone.live :refer :all]
            [leipzig.melody :refer :all]
            [leipzig.scale :as scale]
            [leipzig.live :as live]
            [leipzig.chord :as chord]
            [leipzig.temperament :as temperament]))

(definst kick [freq 110]
  (-> (line:kr freq (* freq 1/2) 0.5)
      sin-osc
      (+ (sin-osc freq))
      (* (env-gen (perc 0.01 0.3) :action FREE))
      (* 2/3)))

(definst bass [freq 110]
  (-> freq
      saw
      (rlpf (line:kr freq (* freq 2) 1))
      (* (env-gen (perc 0.1 0.4) :action FREE))))

(definst organ [freq 440 dur 1]
  (-> freq
      saw
      (rlpf (mul-add (sin-osc 3) 300 (* freq 4)))
      (rlpf (mul-add (sin-osc 2) 400 (* freq 3)))
      (rlpf (mul-add (sin-osc 2) 200 (* freq 5)))
      (* (square 1))
      (* (env-gen (adsr 0.01 0.2 0.6) (line:kr 1 0 dur) :action FREE))
      (* 1/10)))

(defmethod live/play-note :default [{hertz :pitch}] (bass hertz))
(defmethod live/play-note :beat [{hertz :pitch}] (kick hertz))
(defmethod live/play-note :accompaniment [{hertz :pitch seconds :duration}] (organ hertz seconds))

(defn bassline [root]
  (->> (phrase (cycle [1 1/2 1/2 1 1]) [0 -3 -1 0 2 0 2 3 2 0])
       (where :pitch (scale/from root))
       (where :pitch (comp scale/lower scale/lower))))

(def progression [0 0 3 0 4 0])

(def beat
  (->>
    (phrase (cycle [1/2 1/4 1/4 1/2 1/2]) (cycle [-14 -14 -7 -14 -7]))
    (take 20)
    (times 6)
    (where :part (is :beat))))

(defn accompaniment [root]
  (->> (phrase (repeat 8 1) (-> chord/seventh (chord/root root) vals cycle))
       (where :part (is :accompaniment))))

(def track
  (->>
    beat
    ;(mapthen bassline progression)
    ;(with (mapthen accompaniment progression))
    ;(with beat)
    (where :pitch (comp temperament/equal scale/A scale/minor))
    (where :time (bpm 90))
    (where :duration (bpm 90))))
