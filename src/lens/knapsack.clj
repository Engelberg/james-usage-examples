(ns lens.knapsack
  (:refer-clojure :exclude [cond if-let when-let])
  (:require [better-cond.core :refer [defnc cond if-let when-let]]
            [clojure-csv.core :as csv]
            [semantic-csv.core :as sc]
            [clojure.java.io :as io]
            [medley.core :as medley])
  (:import org.jamesframework.core.problems.datatypes.IntegerIdentifiedData
           org.jamesframework.core.problems.objectives.Objective
           org.jamesframework.core.problems.objectives.evaluations.SimpleEvaluation
           org.jamesframework.core.problems.objectives.evaluations.Evaluation
           org.jamesframework.core.problems.constraints.Constraint
           org.jamesframework.core.problems.constraints.validations.Validation
           org.jamesframework.core.problems.constraints.PenalizingConstraint
           org.jamesframework.core.problems.constraints.validations.PenalizingValidation
           org.jamesframework.core.problems.constraints.validations.SimplePenalizingValidation
           org.jamesframework.core.problems.sol.RandomSolutionGenerator
           org.jamesframework.core.subset.SubsetSolution           
           org.jamesframework.core.subset.SubsetProblem
           org.jamesframework.core.subset.neigh.moves.SubsetMove
           org.jamesframework.core.search.Search
           org.jamesframework.core.search.algo.RandomDescent
           org.jamesframework.core.search.algo.ParallelTempering
           org.jamesframework.core.subset.neigh.SingleSwapNeighbourhood
           org.jamesframework.core.subset.neigh.SinglePerturbationNeighbourhood
           org.jamesframework.core.search.stopcriteria.MaxRuntime
           org.jamesframework.core.search.listeners.SearchListener
           org.jamesframework.core.search.neigh.Move
           org.jamesframework.core.util.SetUtilities
           java.util.concurrent.TimeUnit))

(defrecord KnapsackData [weights profits ids]
  IntegerIdentifiedData
  (getIDs [this] ids))
(defn get-weight [{:keys [weights]} id] (nth weights id))
(defn get-profit [{:keys [profits]} id] (nth profits id))

(defn read-csv-data [filename]
  (map->KnapsackData
   (let [data (partition 2 (rest (read-string (str \[ (slurp filename) \]))))]
     {:weights (into [] (comp (map second) (map double)) data),
      :profits (into [] (comp (map first) (map double)) data),
      :ids (set (map int (range (count data))))})))

(def knapsack-data (read-csv-data "input/knapsack-1000.txt"))

(defn sum [getter data s] (transduce (map (partial getter data)) + s))

(def knapsack-objective 
  (reify Objective
    (isMinimizing [this] false)
    (evaluate [this solution data]
      (SimpleEvaluation/WITH_VALUE
       (sum get-profit data (.getSelectedIDs ^SubsetSolution solution))))
    (evaluate [this move curSolution curEvaluation data]
      (SimpleEvaluation/WITH_VALUE
       (+ (.getValue curEvaluation)
          (sum get-profit data (.getAddedIDs ^SubsetMove move))
          (- (sum get-profit data (.getDeletedIDs ^SubsetMove move))))))))

(defrecord KnapsackValidation [^double curWeight ^double maxWeight]
  Validation
  (passed [this] (<= curWeight maxWeight)))

(defn knapsack-constraint [max-weight]
  (reify Constraint
    (validate [this solution data]
      (let [weight (sum get-weight data (.getSelectedIDs ^SubsetSolution solution))]
        (->KnapsackValidation weight max-weight)))
    (validate [this move curSolution curValidation data]
      (let [weight (+ (:curWeight curValidation)
                      (sum get-weight data (.getAddedIDs ^SubsetMove move))
                      (- (sum get-weight data (.getDeletedIDs ^SubsetMove move))))]
        (->KnapsackValidation weight max-weight)))))

(defn knapsack-problem [^double capacity]
  (let [problem (SubsetProblem. knapsack-data knapsack-objective)
        default-random-solution-generator (.getRandomSolutionGenerator problem)
        random-solution-generator
        (reify RandomSolutionGenerator
          (create [this r d]
            (let [^SubsetSolution sol (.create default-random-solution-generator r d)]
              (loop [weight (sum get-weight knapsack-data (.getSelectedIDs sol))]
                (cond
                  (> weight capacity)
                  (let [id (SetUtilities/getRandomElement (.getSelectedIDs sol) r)]
                    (.deselect sol id)
                    (recur (- weight (get-weight knapsack-data id))))
                  (let [final-size (.nextInt r (inc (.getNumSelectedIDs sol)))]
                    (.deselectAll sol (SetUtilities/getRandomSubset
                                       (.getSelectedIDs sol)
                                       (- (.getNumSelectedIDs sol) final-size)
                                       r))
                    sol))))))]
    (doto problem
      (.addMandatoryConstraint (knapsack-constraint capacity))
      (.setRandomSolutionGenerator random-solution-generator))))

(def best-solution (atom nil))

(defnc solution-info [^Search s p]
  :let [sol (.getBestSolution s),
        ids (.getSelectedIDs ^SubsetSolution sol)]
  {:best-solution-ids ids,
   :best-solution-profit (sum get-profit knapsack-data ids),
   :best-solution-weight (sum get-weight knapsack-data ids)
   :constraint-satisfied (.isEmpty (.getViolatedConstraints p sol))})

(defn progress-listener [problem]
  (reify SearchListener
    (searchStarted [this search] (println " >>> Search started"))
    (searchStopped [this search]
      (println (str " >>> Search stopped ("
                    (/ (.getRuntime search) 1000)
                    " sec, "
                    (.getSteps search)
                    " steps)")))
    (newBestSolution [this search newBestSolution newBestSolutionEvaluation newBestSolutionValidation]
      (reset! best-solution (solution-info search problem))
      (println (str "New best solution: " (.getValue newBestSolutionEvaluation)
                    " (unpenalized: " (sum get-profit knapsack-data (.getSelectedIDs newBestSolution)) ")")))))

(defnc search [capacity time-limit]
  :let [problem (knapsack-problem capacity),
        search (RandomDescent. problem (SinglePerturbationNeighbourhood.))]
  :do (doto search
        (.addSearchListener (progress-listener problem))
        (.addStopCriterion (MaxRuntime. time-limit TimeUnit/SECONDS))
        (.start))
  (solution-info search problem))

(defn average [s] (/ (double (apply + s)) (count s)))
(defnc search-tempered [problem time-limit]
  :let [scale (average (:profits knapsack-data)),
        min-temp (* scale 0.001),
        max-temp (* scale 0.1),
        num-replicas 10,
        search (ParallelTempering. problem (SinglePerturbationNeighbourhood.) num-replicas min-temp max-temp)]
  :do (doto search
        (.addSearchListener (progress-listener problem))
        (.addStopCriterion (MaxRuntime. time-limit TimeUnit/SECONDS))
        (.start))
  (solution-info search problem))

(defn knapsack-penalizing-constraint [max-weight highest-profit]
  (reify PenalizingConstraint
    (validate [this solution data]
      (cond
        :let [ids (.getSelectedIDs ^SubsetSolution solution)
              weight (sum get-weight data ids),
              min-remove
              (loop [ids (seq ids) weight weight min-remove 0]
                (cond
                  (or (nil? ids) (<= weight max-weight)) min-remove
                  (recur (next ids) (- weight (get-weight data (first ids))) (inc min-remove))))]
        (> min-remove 0) (SimplePenalizingValidation/FAILED
                          (* min-remove (inc highest-profit)))
        :else SimplePenalizingValidation/PASSED))))

(defn knapsack-penalizing-problem [^double capacity]
  (let [highest-profit (apply max (:profits knapsack-data))
        problem (SubsetProblem. knapsack-data knapsack-objective
                                0 (count (.getIDs ^IntegerIdentifiedData knapsack-data))
                                (fn [x y] (> (get-weight knapsack-data x)
                                             (get-weight knapsack-data y))))]
    (doto problem
      (.addPenalizingConstraint (knapsack-penalizing-constraint capacity highest-profit)))))

