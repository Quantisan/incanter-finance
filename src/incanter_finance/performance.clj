(ns ^{:doc "Collection of econometrics for performance analytics."
      :author "Paul Lam"} 
     incanter-finance.performance
  (:use
    [incanter.core :only (dataset)]
    [incanter.stats :only (mean sd)]))


;; OPEN Q: incorporate time series returns data?
(defn sharpe-ratio
" Returns the Sharpe ratio of ret, where ret is a list of returns 
  with an uniform time period, e.g. monthly return.
  
  Options:
  :rf a single risk-free rate or a set of risk free rates, 
      in same time period as returns.

  Reference:
  http://en.wikipedia.org/wiki/Sharpe_ratio
"
  [ret & {:keys [rf] :or {rf 0}}]
  {:pre  [(or (number? rf) (= (count rf) (count ret)))]}
  (let [r-rf   (if (number? rf)               ; return minus risk free rate(s)
                 (map #(- % rf) ret)
                 (map #(- %1 %2) ret rf))]
    (/ (mean r-rf) (sd r-rf))))