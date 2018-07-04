(ns lens.core
  (:refer-clojure :exclude [cond if-let when-let])
  (:require [better-cond.core :refer [cond if-let when-let]])
  (:import java.io.FileNotFoundException
           java.util.concurrent.TimeUnit
           java.util.stream.Collectors
           org.jamesframework.core.subset.SubsetProblem
           org.jamesframework.core.subset.SubsetSolution
           org.jamesframework.core.search.algo.RandomDescent
           org.jamesframework.core.subset.neigh.SingleSwapNeighbourhood
           org.jamesframework.core.search.stopcriteria.MaxRuntime
           org.jamesframework.examples.util.ProgressSearchListener))



