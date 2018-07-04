(ns lens.core-subset
  (:refer-clojure :exclude [cond if-let when-let])
  (:require [better-cond.core :refer [defnc cond if-let when-let]]
            [clojure-csv.core :as csv]
            [semantic-csv.core :as sc]
            [clojure.java.io :as io]
            [medley.core :as medley])
  (:import org.jamesframework.core.problems.datatypes.IntegerIdentifiedData
           org.jamesframework.core.problems.objectives.Objective
           org.jamesframework.core.problems.objectives.evaluations.SimpleEvaluation
           org.jamesframework.core.subset.SubsetSolution           
           org.jamesframework.core.subset.SubsetProblem
           org.jamesframework.core.search.Search
           org.jamesframework.core.search.algo.RandomDescent
           org.jamesframework.core.subset.neigh.SingleSwapNeighbourhood
           org.jamesframework.core.search.stopcriteria.MaxRuntime
           org.jamesframework.core.search.listeners.SearchListener))

(defrecord CoreSubsetData [names dist ids]
  IntegerIdentifiedData
  (getIDs [this] ids))
(defn get-name [{names :names} id] (nth names id))
(defn get-distance [{dist :dist} id1 id2] (nth (nth dist id1) id2))

(defn read-csv-data [filename]
  (map->CoreSubsetData
   (with-open [reader (io/reader filename)]
     (let [lines (csv/parse-csv reader)
           names (first lines)]
       {:names names,
        :dist (doall (for [line (rest lines)]
                       (mapv #(Double/parseDouble %) line)))
        :ids (set (map int (range (count names))))}))))

(def core-subset-data (read-csv-data "input/coresubset.csv"))

(defn average [s] (/ (double (apply + s)) (count s)))
(def core-subset-objective 
  (reify Objective
    (isMinimizing [this] false)
    (evaluate [this solution data]
      (SimpleEvaluation/WITH_VALUE
       (cond
         :let [selected (vec (.getSelectedIDs ^SubsetSolution solution))
               num-selected (count selected)]
         (< num-selected 2) 0.0
         :let [distances (for [i (range num-selected),
                               j (range (inc i) num-selected)]
                           (get-distance data (selected i) (selected j)))]
         (average distances))))))

(defn core-subset-problem [subset-size]
  (SubsetProblem. core-subset-data core-subset-objective (int subset-size)))

(def best-solution (atom nil))

(defnc solution-info [^Search s]
  :let [sol (.getBestSolution s),
        ids (.getSelectedIDs ^SubsetSolution sol),
        names (map #(get-name core-subset-data %) ids)]
  {:best-solution-ids ids,
   :best-solution-names names,
   :best-solution-evaluation (.getValue (.getBestSolutionEvaluation s))})

(def progress-listener
  (reify SearchListener
    (searchStarted [this search] (println " >>> Search started"))
    (searchStopped [this search]
      (println (str " >>> Search stopped ("
                    (/ (.getRuntime search) 1000)
                    " sec, "
                    (.getSteps search)
                    " steps)")))
    (newBestSolution [this search newBestSolution newBestSolutionEvaluation newBestSolutionValidation]
      (reset! best-solution (solution-info search))
      (println (str "New best solution: " newBestSolutionEvaluation)))))

(defnc search [subset-size time-limit]
  :let [problem (core-subset-problem subset-size),
        search (RandomDescent. problem (SingleSwapNeighbourhood.))]
  :do (doto search
        (.addSearchListener progress-listener)
        (.addStopCriterion (MaxRuntime. time-limit TimeUnit/SECONDS))
        (.start))
  (solution-info search))


