(ns ^{:doc "Collection of econometrics for performance analytics."
      :author "Paul Lam"} 
     incanter-finance.performance
  (:use
    [incanter.core :only (dataset sqrt)]
    [incanter.stats :only (mean sd square-devs-from-mean)]))


(defn return-excess
" Returns minus risk-free rate(s)."
  [r rf]
  {:pre  [(or (number? rf) (= (count rf) (count r)))]}
  (if (number? rf)               ; return minus risk free rate(s)
    (map #(- % rf) r)
    (map #(- %1 %2) r rf)))

(defn sharpe-ratio
" Returns the Sharpe ratio of ret, where ret is a vector of returns 
  with an uniform time period, e.g. monthly returns.
  
  Options:
  :rf a single risk-free rate or a set of risk free rates, 
      in same time period as returns.

  Reference:
  http://en.wikipedia.org/wiki/Sharpe_ratio
"
  [ret & {:keys [rf] :or {rf 0}}]
  (let [r-rf   (return-excess ret rf)]
    (/ (mean r-rf) (sd r-rf))))

(defn downside-risk
" Downside risk, or the target semi-deviation. Calculated by finding the 
  root mean squared underperformance, where the underperformance is the amount 
  by which a return is below target."
  [r mar]
  (let [sub-r   (filter #(< % mar) r)]
    (sqrt (/ (apply + (square-devs-from-mean sub-r mar)) 
             (count sub-r)))))

(defn sortino-ratio
" Returns the Sortino ratio of ret, where ret is a vector of returns 
  with an uniform time period, e.g. monthly returns.
  
  Options:
  :mar minimum acceptable return, or target rate of return. Must be
       in same time period as ret.

  Reference:
  http://en.wikipedia.org/wiki/Sortino_ratio
"
  [ret & {:keys [mar] :or {mar 0}}]
  (let [ex-r   (return-excess ret mar)]
    (/ (mean ex-r) (downside-risk ret mar))))