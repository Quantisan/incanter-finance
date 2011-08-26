(ns incanter-finance.test.performance-tests
  (:use clojure.test 
        incanter-finance.performance
        (incanter core stats)))

(def ham1 (dataset [:date :return] ; hypothetical asset manager #1
                   [["2011-01-31" 0.0084] 
                    ["2011-02-28" 0.0154]
                    ["2011-03-31" -0.0046]
                    ["2011-04-30" 0.0484]
                    ["2011-05-31" 0.0117]
                    ["2011-06-30" -0.0334]]))
(def rf1 '(0.0010 0.0015 0.0012 0.0008 0.0008 0.0010))   ; risk-free rates

(defn scale-decimal
  "Rounds x to n decimal places."
  [n x]
  (.setScale (bigdec x) n java.math.BigDecimal/ROUND_HALF_UP))

(deftest returns-test
  (is (= (map (partial scale-decimal 4) 
              (return-excess ($ :return ham1) 0.001))
         '(0.0074 0.0144 -0.0056 0.0474 0.0107 -0.0344))
      "return-ecess where rf is scalar")
  (is (= (map (partial scale-decimal 4) 
              (return-excess ($ :return ham1) rf1))
         '(0.0074 0.0139 -0.0058 0.0476 0.0109 -0.0344)) 
      "return-excess where rf is a vector"))

(deftest sharpe-ratio-test
  (is (= (sharpe-ratio ($ :return ham1)) 0.28618365071780066))
  (is (= (sharpe-ratio ($ :return ham1) :rf 0.001) 0.2487740231729901))
  (is (= (sharpe-ratio ($ :return ham1) :rf rf1) 0.24637227121155225)))

(deftest sortino-ratio-test
  (is (= (downside-risk ($ :return ham1) 0.01) 0.02645297714813968))
  (is (= (sortino-ratio ($ :return ham1) :mar 0.005) 0.0946815105474268)))
