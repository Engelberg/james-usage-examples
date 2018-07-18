(ns lens.refugee
  (:refer-clojure :exclude [cond if-let when-let])
  (:require [better-cond.core :refer [defnc cond if-let when-let]]
            [clojure.string :as str]
            [clojure-csv.core :as csv]
            [semantic-csv.core :as sc]
            [clojure.java.io :as io]
            [medley.core :as medley]
            [ubergraph.core :as u]
            [ubergraph.alg :as alg]
            [clojure.data.generators :as gen :refer [*rnd*]]
            [clojure.data.priority-map :as pm])
  (:import org.jamesframework.core.problems.datatypes.IntegerIdentifiedData
           org.jamesframework.core.problems.objectives.Objective
           org.jamesframework.core.problems.objectives.evaluations.SimpleEvaluation
           org.jamesframework.core.problems.objectives.evaluations.Evaluation
           org.jamesframework.core.problems.constraints.Constraint
           org.jamesframework.core.problems.constraints.validations.Validation
           org.jamesframework.core.problems.constraints.Constraint
           org.jamesframework.core.problems.constraints.validations.PenalizingValidation
           org.jamesframework.core.problems.constraints.validations.SimpleValidation
           org.jamesframework.core.problems.GenericProblem
           org.jamesframework.core.problems.sol.RandomSolutionGenerator
           org.jamesframework.core.problems.sol.Solution
           org.jamesframework.core.search.Search
           org.jamesframework.core.search.algo.RandomDescent
           org.jamesframework.core.search.algo.SteepestDescent           
           org.jamesframework.core.search.algo.ParallelTempering
           org.jamesframework.core.search.neigh.Neighbourhood
           org.jamesframework.core.search.neigh.Move
           org.jamesframework.core.search.stopcriteria.MaxRuntime
           org.jamesframework.core.search.listeners.SearchListener
           org.jamesframework.core.search.neigh.Move
           org.jamesframework.core.util.SetUtilities
           java.util.concurrent.TimeUnit))

(defprotocol CljSolution (getSolution [this]) (getAtom [this]))

(declare families)
(defn read-data [filename]
  (let [s (slurp filename),
        s (str "'{" (str/replace s #"[=;|]" " ") "}")
        data (eval (read-string s))
        family (vec (partition 2 (get data 'ML)))
        n (get data 'n)]
    {:n n, :k (get data 'k), :maxsize (get data 'maxsize),
     :edges (vec (partition 2 (get data 'E))), :family family,
     :families (families {:n n :family family})
     :knows (frequencies (get data 'E)),
     :dislikes (vec (partition 2 (get data 'CL)))}))

(defn irange [start stop] (range start (inc stop)))
(defn sum [s] (apply + s))

(defnc families [{:keys [n family]}]
  :let [g (apply u/graph (concat (irange 1 n) (map vec family)))]
  (mapv distinct (alg/connected-components g)))

(defn num-edges-within-camp [solution {:keys [edges]}]
  (apply merge-with +
         (for [[i j] edges
               :let [si (get solution i),
                     sj (get solution j)]
               :when (= si sj)]
           {si 1})))

(defn priority->set-of-items [^clojure.data.priority_map.PersistentPriorityMap p]
  (.priority->set-of-items p))

(def refugee-objective 
  (reify Objective
    (isMinimizing [this] false)
    (evaluate [this solution {:keys [k knows edges] :as data}]
      (SimpleEvaluation/WITH_VALUE
       (let [solution (.getSolution solution)
             e (num-edges-within-camp solution data),
             m (count edges)
             camp->refugees (priority->set-of-items solution)]
         (println solution e m camp->refugees)
         (sum (for [c (irange 1 k)]
                (- (* 4 m (get e k 0))
                   (let [s (sum (for [i (camp->refugees k)]
                                  (knows i 0)))]
                     (println "s" s)
                     (* s s))))))))))

(defn make-solution [o]
  (let [sol (atom o)]
    (proxy [Solution lens.refugee.CljSolution] []
      (copy [] (make-solution @sol))
      (equals [other] (= @sol (.getSolution other)))
      (hashCode [] (hash @sol))
      (getSolution [] @sol)
      (getAtom [] sol))))

(defnc valid-camp-assignment? [assignment {:keys [maxsize dislikes]}]
  :let [camp->refugees (priority->set-of-items assignment)]
  (and (every? (fn [[camp people]] (<= (count people) maxsize)) camp->refugees)
       (every? (fn [[p1 p2]] (or (not (contains? assignment p1))
                                 (not (contains? assignment p2))
                                 (not= (assignment p1) (assignment p2))))
               dislikes)))

(defnc camp-assignment
  ([{n :n :as data}]
   :let [initial-assignment (pm/priority-map)]
   (camp-assignment initial-assignment
                         (update data :families #(sort-by count > %))))
  ([assignment {:keys [k families dislikes] :as data}]
   (empty? families) assignment
   :let [family (first families)
         highest-pair (first (rseq assignment))
         highest-camp (if highest-pair (min k (inc (val highest-pair))) 1)]
   (loop [possible-camps (gen/shuffle (irange 1 highest-camp))]
     (cond
       :when-let [i (first possible-camps)]
       :let [new-assignment (into assignment (for [f family] [f i]))]
       (not (valid-camp-assignment? new-assignment data)) (recur (next possible-camps))
       :let [solution (camp-assignment new-assignment (assoc data :families (rest families)))]
       (nil? solution) (recur (next possible-camps))
       :else solution))))

;; (defn- unchunk [s]
;;   (lazy-seq
;;     (when (seq s)
;;       (cons (first s) (unchunk (rest s))))))

;; (defnc all-camp-assignments
;;   ([{n :n :as data}]
;;    :let [initial-assignment (pm/priority-map)]
;;    (all-camp-assignments initial-assignment
;;                          (update data :families #(sort-by count > %))))
;;   ([assignment {:keys [k families dislikes] :as data}]
;;    :do (println assignment \newline families)
;;    :do (Thread/sleep 500)
;;    (empty? families) (do (println "solution") [assignment])
;;    :let [family (first families)
;;          highest-pair (first (rseq assignment))
;;          highest-camp (if highest-pair (min k (inc (val highest-pair))) 1)]
;;    (mapcat (fn [i] (let [new-assignment (into assignment (for [f family] [f i]))]
;;                      (when (valid-camp-assignment? new-assignment data)
;;                        (all-camp-assignments new-assignment
;;                                              (assoc data :families (rest families))))))
;;            (unchunk (gen/shuffle (irange 1 highest-camp))))))

(defn initial-solution [data]
  (make-solution (camp-assignment data)))

(def random-solution-generator
  (reify RandomSolutionGenerator
    (create [this r d] (binding [*rnd* r] (initial-solution d)))))

(def dislike-constraint
  (reify Constraint
    (validate [this solution data]
      (cond
        :let [solution (.getSolution solution)]
        (valid-camp-assignment? solution data) SimpleValidation/PASSED
        :else SimpleValidation/FAILED))))

(def best-solution (atom nil))

(defnc solution-info [^Search s]
  :let [sol (.getBestSolution s),
        p (.getProblem s)
        d (.getData p)
        a (.getSolution sol)]
  {:assignment a,
   :objective (.evaluate refugee-objective sol d)})

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
      (println (str "New best solution: " 
                    (.evaluate ^GenericProblem (.getProblem search) newBestSolution))))))

(defrecord RefugeeMove [family from-camp to-camp]
  Move
  (apply [this sol]
    (let [a (.getAtom sol)]
      (reset! a (into (.getSolution sol) (for [f family] [f to-camp])))))
  (undo [this sol]
    (let [a (.getAtom sol)]
      (reset! a (into (.getSolution sol) (for [f family] [f from-camp]))))))

(def refugee-move ->RefugeeMove)

(defn refugee-neighbourhood [{:keys [k n families] :as data}]
  (reify Neighbourhood
    (getRandomMove [this sol r]
      (binding [*rnd* r]
        (let [solution (.getSolution sol),
              family (gen/rand-nth families),
              person (first family)
              current-camp (get solution person)
              highest-pair (first (rseq solution))
              highest-camp (if highest-pair (min k (inc (val highest-pair))) 1)]
          (loop []
            (let [to-camp (inc (gen/uniform 0 highest-camp))]
              (if (not= current-camp to-camp)
                (refugee-move family current-camp to-camp)
                (recur)))))))
    (getAllMoves [this sol]
      (let [solution (.getSolution sol)]
        (for [family families,
              :let [person (first family)
                    current-camp (get solution person)
                    highest-pair (first (rseq solution))
                    highest-camp (if highest-pair (min k (inc (val highest-pair))) 1)]
              to-camp (irange 1 highest-camp)
              :when (not= current-camp to-camp)]
          (refugee-move family current-camp to-camp))))))

(defnc search-tempered [^GenericProblem problem time-limit]
  :let [scale (count (:edges (.getData problem))),
        scale (* scale scale)
        min-temp (* scale 0.001),
        max-temp (* scale 0.1),
        num-replicas 10,
        data (.getData problem)
        search (ParallelTempering. problem (refugee-neighbourhood data)
                                   num-replicas min-temp max-temp)]
  :do (doto search
        (.addSearchListener progress-listener)
        (.addStopCriterion (MaxRuntime. time-limit TimeUnit/SECONDS))
        (.start))
  (solution-info search))

(defnc search [problem time-limit]
  :let [data (.getData problem)
        search (SteepestDescent. problem (refugee-neighbourhood data))]
  :do (doto search
        (.addSearchListener progress-listener)
        (.addStopCriterion (MaxRuntime. time-limit TimeUnit/SECONDS))
        (.start))
  (solution-info search))

(defn refugee-problem [data]
  (let [problem (GenericProblem. data refugee-objective random-solution-generator)]
    (doto problem
      (.addMandatoryConstraint dislike-constraint))))

(def d (read-data "data/refugee_1.dzn"))
(def p (refugee-problem d))
(def n (refugee-neighbourhood d))
(def s (initial-solution d))
(def m (.getRandomMove n s))
