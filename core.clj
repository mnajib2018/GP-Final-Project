(ns finalproject.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))


(def instructions
  (list
   'order
   'inc1+
   'integer_-
   'dec1-
   '*len*
   'current_index
   'doble
   ))


;;;;;;;;
;;Examples

(def example-push-state
	{:exec '(order e1- e1+ e- *len*)
	 :input_vector [1.0, 2.0, 4.0, 5.0, 6.0]
	 :indices '(0 1)
	}
)

Steps:
1) Define all functions including doble
2) Define example states including empty state
3) Lexicase
4) Behavioral Diversity
5) weighted fitness
6) 
