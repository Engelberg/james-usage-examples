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
           org.jamesframework.core.problems.objectives.evaluations.Evaluation
           org.jamesframework.core.subset.SubsetSolution           
           org.jamesframework.core.subset.SubsetProblem
           org.jamesframework.core.subset.neigh.moves.SubsetMove
           org.jamesframework.core.search.Search
           org.jamesframework.core.search.algo.RandomDescent
           org.jamesframework.core.search.algo.ParallelTempering
           org.jamesframework.core.subset.neigh.SingleSwapNeighbourhood
           org.jamesframework.core.search.stopcriteria.MaxRuntime
           org.jamesframework.core.search.listeners.SearchListener
           org.jamesframework.core.search.neigh.Move))

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
      (println (str "New best solution: " (.getValue newBestSolutionEvaluation))))))

(defnc search [subset-size time-limit]
  :let [problem (core-subset-problem subset-size),
        search (RandomDescent. problem (SingleSwapNeighbourhood.))]
  :do (doto search
        (.addSearchListener progress-listener)
        (.addStopCriterion (MaxRuntime. time-limit TimeUnit/SECONDS))
        (.start))
  (solution-info search))

;; CoreSubset with delta

(def core-subset-objective-with-delta
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
         (average distances))))
    (evaluate [this move curSolution curEvaluation data]
      (SimpleEvaluation/WITH_VALUE
       (cond
         :let [current-eval (.getValue curEvaluation),
               selected (vec (.getSelectedIDs ^SubsetSolution curSolution))
               num-selected (count selected),
               num-distances (/ (* num-selected (dec num-selected)) 2)
               sum-distances (* current-eval num-distances)
               added (vec (.getAddedIDs ^SubsetMove move))
               removed (set (.getDeletedIDs ^SubsetMove move))
               retained (into [] (remove removed) selected)
               dist1 (vec (for [a removed, b retained]
                            (get-distance data a b)))
               dist2 (vec (for [a removed, b removed :when (< a b)]
                            (get-distance data a b)))
               dist3 (vec (for [a added, b retained]
                            (get-distance data a b)))
               dist4 (vec (for [a added, b added :when (< a b)]
                            (get-distance data a b))),
               num-distances (- (+ num-distances (count dist3) (count dist4))
                                (count dist1) (count dist2))]
         (<= num-distances 0) 0.0
         (/ (reduce - (apply + sum-distances (concat dist3 dist4))
                    (concat dist1 dist2))
            num-distances))))))

(defn core-subset-with-delta-problem [subset-size]
  (SubsetProblem. core-subset-data core-subset-objective-with-delta (int subset-size)))

(defnc search-with-delta [subset-size time-limit]
  :let [problem (core-subset-with-delta-problem subset-size),
        search (RandomDescent. problem (SingleSwapNeighbourhood.))]
  :do (doto search
        (.addSearchListener progress-listener)
        (.addStopCriterion (MaxRuntime. time-limit TimeUnit/SECONDS))
        (.start))
  (solution-info search))

;; New objective to maximize average distance of entry to nearest entry in set

(defnc find-closest [item group data]
  :when-let [others (seq (remove #{item} group))]  
  (apply min-key #(get-distance data item %) others)) 

(defrecord ClosestItem [closest-item distance])
(defrecord EntryToNearestEntryEvaluation [closest-item-map min-dist-sum]
  Evaluation
  (getValue [this]
    (cond
      :let [num-distances (count closest-item-map)]
      (<= num-distances 0) 0.0
      :else (/ min-dist-sum num-distances))))

(defn entry-to-nearest-entry-remove [{:keys [closest-item-map min-dist-sum] :as eval} item]
  (if (contains? closest-item-map item)
    (->EntryToNearestEntryEvaluation (dissoc closest-item-map item)
                                     (- min-dist-sum (get-in closest-item-map [item :distance])))
    eval))

(defn entry-to-nearest-entry-add [{:keys [closest-item-map min-dist-sum]}
                                  item closest-other-item distance]
  (if (contains? closest-item-map item)
    (->EntryToNearestEntryEvaluation (assoc closest-item-map item
                                            (->ClosestItem closest-other-item distance))
                                     (+ min-dist-sum distance
                                        (- (get-in closest-item-map [item :distance]))))
    (->EntryToNearestEntryEvaluation (assoc closest-item-map item
                                            (->ClosestItem closest-other-item distance))
                                     (+ min-dist-sum distance))))

(def entry-to-nearest-entry-objective
  (reify Objective
    (isMinimizing [this] false)
    (evaluate [this solution data]
      (let [selected (vec (.getSelectedIDs ^SubsetSolution solution))
            closest-items (for [s selected
                                :let [closest (find-closest s selected data)]
                                :when closest]
                            (clojure.lang.MapEntry.
                             s (->ClosestItem closest (get-distance data s closest))))]
        (->EntryToNearestEntryEvaluation
         (into {} closest-items)
         (transduce (comp (map val) (map :distance)) + 0.0 closest-items))))
    (evaluate [this move curSolution curEvaluation data]
      (let [added (vec (.getAddedIDs ^SubsetMove move))
            deleted (set (.getDeletedIDs ^SubsetMove move))
            selection (vec (.getSelectedIDs ^SubsetSolution curSolution))
            new-selection (into added (remove deleted) selection)
            evaluation (reduce entry-to-nearest-entry-remove curEvaluation (seq deleted))]
        (reduce (fn [ev item]
                  (cond
                    :let [closest (get-in evaluation [item :closest-item])]
                    (or (nil? closest) (contains? deleted closest))
                    (if-let [new-closest (find-closest item new-selection data)]
                      (entry-to-nearest-entry-add
                       ev item new-closest (get-distance data item new-closest))
                      (entry-to-nearest-entry-remove ev item))
                    :let [closest-added-item (find-closest item added data)]
                    (nil? closest-added-item) ev
                    :let [closest-added-item-distance (get-distance data item closest-added-item)]
                    (< closest-added-item-distance (get-distance data item closest))
                    (entry-to-nearest-entry-add ev item closest-added-item closest-added-item-distance)
                    :else ev))
                evaluation                
                new-selection)))))

(defn entry-to-nearest-entry-problem [subset-size]
  (SubsetProblem. core-subset-data entry-to-nearest-entry-objective (int subset-size)))

(defnc search-nearest-entry [subset-size time-limit]
  :let [min-temp 1e-8,
        max-temp 1e-4,
        num-replicas 10,
        problem (entry-to-nearest-entry-problem subset-size),
        search (ParallelTempering. problem (SingleSwapNeighbourhood.) num-replicas min-temp max-temp)]
  :do (doto search
        (.addSearchListener progress-listener)
        (.addStopCriterion (MaxRuntime. time-limit TimeUnit/SECONDS))
        (.start))
  (solution-info search))
