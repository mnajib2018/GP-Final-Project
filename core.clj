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
   'len*
   'current_index
   'doble
   ))

(defn order
  "swap the indicex at x and y if element at x is
  larger than the one at y."
  [state x y]
  (def first (nth (get state :input_vector) x))
  (def second (nth (get state :input_vector) y))
  (if (> x  y)
    ((assoc (get state :input_vector) y first)
     (assoc (get state :input_vector) x second))))

(defn inc1+
  "returns the parameter incremented by 1"
  [n]
  (inc c))

(defn dec1-
  "returns the parameter incremented by 1"
  [n]
  (dec c))

(defn integer_-
  "Subtracts the top two integers and leaves result on the integer stack.
  Note: the second integer on the stack should be subtracted from the top integer."
  [x y]
  (- x y))



(defn current_index
  [state]
  (first (get state :indices)))

(defn exec-do-range
  "CODE.DO*RANGE: An iteration instruction that executes the top item on the CODE stack a number of times that depends on the top two integers,
   while also pushing the loop counter onto the INTEGER stack for possible access during the execution of the body of the loop. 
   The top integer is the \"destination index\" and the second integer is the \"current index.\"
   First the code and the integer arguments are saved locally and popped. Then the integers are compared. 
   If the integers are equal then the current index is pushed onto the INTEGER stack and the code (which is the \"body\" of the loop) is pushed onto the EXEC stack
   for subsequent execution. If the integers are not equal then the current index will still be pushed onto the INTEGER stack
   but two items will be pushed onto the EXEC stack -- first a recursive call to CODE.DO*RANGE (with the same code and destination index, 
   but with a current index that has been either incremented or decremented by 1 to be closer to the destination index) and then the body code. 
   Note that the range is inclusive of both endpoints; a call with integer arguments 3 and 5 will cause its body to be executed 3 times, with the loop counter 
   having the values 3, 4, and 5. Note also that one can specify a loop that \"counts down\" by providing a destination index that is less than the specified current index."
   [state]
   (let [destination (peek-stack state :integer)
        current (peek-stack (pop-stack state :integer) :integer)
        code (peek-stack state :exec)
        new-state (pop-stack (pop-stack (pop-stack state :integer) :integer) :exec)]
    (if (= destination current)
      (push-to-stack 
      	           (push-to-stack new-state :exec code) 
      	           :integer 
      	           current) ; correct till here
      (push-to-stack 
      				(push-to-stack 
      					(push-to-stack 
      						(push-to-stack 
      								(push-to-stack (push-to-stack new-state :exec code) :integer current)
                      :exec 
                      'exec-do-range)
                  :exec 
                  destination)
                :exec 
                (if (> current destination)
                  (dec current)
                  (inc current)))
              :exec 
              code))))



;;;;;;;;
;;Examples

(def example-push-state
  {:exec '(code)
   :input_vector [1.0, 2.0, 4.0, 5.0, 6.0]
   :integer '(5 2 1)
   :length '()})


