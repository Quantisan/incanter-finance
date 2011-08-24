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

(deftest sharpe-ratio-test
  (is (= (sharpe-ratio ($ :return ham1)) 0.28618365071780066))
  (is (= (sharpe-ratio ($ :return ham1) :rf 0.001) 0.2487740231729901))
  (is (= (sharpe-ratio ($ :return ham1) :rf rf1) 0.24637227121155225)))