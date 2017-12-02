(ns final.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

;; Examples ------------------------------------------------

(def example-push-state
  {:exec '()
   :vector '([99 98 4 3 2 1 9 6 4 3 2 1 2 3 4 5 ])
   :integer '()})

(def exprog '(1 2 exec-do-range order))

(def exind
  {:program '(1 10 exec-do-range order) :errors [1 3 4] :total-error 0 :report '()})

(def base-individual
  {:program '() :errors [] :total-error 0 :report '()})

; Instructions are either functions that take one Push
; state and return another or constant literals. 
(def instructions
  '(
    order
    exec-do-range
    vector-length
    start-index
    ))

(def non-literal-instructions
  '( 
    order
    exec-do-range
    vector-length
    start-index
    ))

;; Utilities ----------------------------------------------

(def empty-push-state
  {:exec '()
   :vector '()
   :integer '()
   :boolean '()
   :input {}})

(defn push-to-stack
  "Pushes item onto stack in state, returning the resulting state."
  [state stack item]
  (update state stack conj item)
  )

(defn pop-stack
  "Removes top item of stack, returning the resulting state."
  [state stack]
  (update state stack rest)
  )

(defn peek-stack
  "Returns top item on a stack. If stack is empty, returns :no-stack-item"
  [state stack]
  (if (empty? (get state stack))
    :no-stack-item
    (first (get state stack))
  ))

(defn empty-stack?
  "Returns true if the stack is empty in state."
  [state stack]
  (empty? (get state stack))
  )

(defn in-list?
  "returns true if given list contains given element"
  [ins arg]
      (loop [instructions ins]
        (if (empty? instructions) ; reached end of list 
          false
          (if (= (first instructions) arg) ; found!
            true
            (recur (rest instructions))))))

(defn get-args-from-stacks
  "Takes a state and a list of stacks to take args from. If there are enough args
  on each of the desired stacks, returns a map of the form {:state :args}, where
  :state is the new state with args popped, and :args is a list of args from
  the stacks. If there aren't enough args on the stacks, returns :not-enough-args."
  [state stacks]
  (loop [state state
         stacks (reverse stacks) 
         args '()]
    (if (empty? stacks)
      {:state state :args args}
      (let [stack (first stacks)]
        (if (empty-stack? state stack)
          :not-enough-args
          (recur (pop-stack state stack)
                 (rest stacks)
                 (conj args (peek-stack state stack))))))))

(defn make-push-instruction
  "A utility function for making Push instructions. Takes a state, the function
  to apply to the args, the stacks to take the args from, and the stack to return
  the result to. Applies the function to the args (taken from the stacks) and pushes
  the return value onto return-stack in the resulting state."
  [state function arg-stacks return-stack]
  (let [saved-vector (peek-stack state :vector)
        args-pop-result (get-args-from-stacks state arg-stacks)]
    (if (= args-pop-result :not-enough-args)
      state
      (let [result (apply function (:args args-pop-result))
            new-state (:state args-pop-result)]
        ; crucial -- doesn't remove vector from vector stack unless
        ; a vector will be added back to the vector stack
        (push-to-stack
         (if (and (in-list? arg-stacks :vector)
                  (not= :vector return-stack))
           (push-to-stack new-state :vector saved-vector)
           new-state)
         return-stack result)))))

;; Intstructions ------------------------------------------

(defn in1
  "Pushes the input labeled :in1 on the inputs map onto the :exec stack.
  Can't use make-push-instruction, since :input isn't a stack, but a map."
  [state]
  :STUB
  (let [result (get (get state :input) :in1)]
    (push-to-stack state :exec result)
  ))

(defn inc1+
  "increases top integer by 1"
  [state]
  (make-push-instruction state inc' [:integer] :integer))

(defn dec1-
  "decreases top integer by 1"
  [state]
  (make-push-instruction state dec' [:integer] :integer))

(defn start-index
"adds 1 to the integer stack"
  [state]
  (push-to-stack state :integer 1))

(defn exec-do-range
  [state]
  (let [destination (peek-stack state :integer)
        current (peek-stack (pop-stack state :integer) :integer)
        code (peek-stack state :exec)       
        new-state (pop-stack (pop-stack (pop-stack state :integer) :integer) :exec)]
    (if (or (= :no-stack-item destination)
            (= :no-stack-item current)
            (= :no-stack-item code))   
      state
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
         code)))))

(defn order-help
  "Swaps an element at given index with the element at
  the previous index and returns the vector. To be used
  with make-push-instruction in the order function."
  [index vector]
  (let [prev (dec index)]
    (if (and (>= prev 0) (< index (count vector)))
      (let [element1 (nth vector prev)
            element2 (nth vector index)]
        (if (> element1 element2)
          (let [new-vector (assoc (assoc vector prev element2) index element1)]
            new-vector)
          vector))
      vector)))

(defn order
  "Swaps at top integer of thex vector.
  If integer stack has no elements, noops."
  [state]
  (make-push-instruction state order-help [:integer :vector] :vector))

(defn exec-if-help
  "Returns exec1 if bool is true and exec2 otherwise."
  [bool exec1 exec2]
  (if bool
    exec2
    exec1))

(defn exec-if
  "Removes second of exec if top boolean is true, top of exec
  if otherwise. If not enough on exec or boolean, noops."
  [state]
  (make-push-instruction state exec-if-help [:boolean :exec :exec] :exec))

(defn vector-length
  "Adds length of top of vector to integer stack."
  [state]
  (make-push-instruction state count [:vector] :integer))

;; Interpreter --------------------------------------------

(defn push-literal-to-stack
  "Pushes given element 'top' to the correct stack based on comparison
  using the type function. Only does so if int or vector, other-
  wise returns given push state unchanged."
  [new-push-state top]
  (cond 
    (= (type top) (type 1)) (push-to-stack new-push-state :integer top) 
    (= (type top) (type true)) (push-to-stack new-push-state :boolean top)
    (= (type top) (type 1.0)) (push-to-stack new-push-state :float top)
    (= (type top) (type [1])) (push-to-stack new-push-state :vector top)                 
    (= (type top) (type {:in1 1})) (push-to-stack new-push-state :input top)
    :else new-push-state))

(defn interpret-one-step
  "Helper function for interpret-push-program.
  Takes a Push state and executes the next instruction on the exec stack,
  or if the next element is a literal, pushes it onto the correct stack.
  Returns the new Push state."
  [push-state]
  (let [top (peek-stack push-state :exec)
       new-push-state (pop-stack push-state :exec)] 
    (if (in-list? non-literal-instructions top) ; check instruct or literal
      ((eval top) new-push-state) ; execute instruction
      (push-literal-to-stack new-push-state top))))

(defn load-program
  "adds a program to the exec stack"
  [program start-state]
  (update start-state :exec concat program))

(defn interpret-push-program
  "Runs the given program starting with the stacks in start-state. Continues until the exec stack is empty. Returns the state of the stacks after the program finishes executing."
  [program start-state]
  (let [loaded-state (load-program program start-state)]
    (loop [current-state loaded-state]
      (if (empty? (get current-state :exec)) 
        current-state ; return state when exec is empty
        (let [new-state (interpret-one-step current-state)]
          (recur new-state))))))

;; GP -----------------------------------------------------

(defn make-random-push-program
  "Creates and returns a new program. Takes a list of instructions and
  a maximum initial program size."
  [instructions max-len]
  :STUB
  (take max-len (repeatedly #(nth instructions (int (rand (count instructions)))))))

(defn get-random-sample
  "Creates a population sample, including each element of population
  with a 50% chance."
  [population]
  (let [sample (random-sample 0.5 population)]  
    (if (empty? sample) ; in case none are selected
      (get-random-sample population)
      sample)))

(defn tournament-selection
  "Selects an individual from the population using a tournament. Returned 
  individual will be a parent in the next generation. Can use a fixed
  tournament size."
  [population]
  (if (empty? population) 
    population
    (loop [pop (get-random-sample population) ; randomly selects population
           fittest-ind (first pop) ; saves first to be compared
           fittest-error (get (first pop) :total-error)]
      (if (= 1 (count pop)) ; when at end of pop, return latest individual
        ; keep individual with lower error
        (first pop) 
        (if (< fittest-error (get (second pop) :total-error)) 
          (recur (rest pop) 
                 fittest-ind ; fittest unchanged
                 fittest-error) 
          (recur (rest pop)
                 (second pop) ; fittest changes
                 (get (second pop) :total-error)))))))


(defn get-element
  "return one element based on 50% chance for each"
  [element1 element2]
  (let [x (+ (rand-int 10) 1)]
  (if (< x 6)
    (if (nil? element1) 
      element2
      element1)
    (if (nil? element2) 
      element1
      element2))))

(defn Keep-Half?
  "return true or false based on a 50% chance"
  [n]
  (let [x (+ (rand-int 100) 1)]
  (if (< x 50)
    false
    true)))

(defn crossover
  "Crosses over two programs (note: not individuals) using uniform crossover.
  Returns child program."
  [program1 program2]
  ;get the element randomly from one of the programs
  (let [element (get-element (first program1) (first program2))]
  (cond 
    ;if both progs empty return null
    (and (empty? program1) (empty? program2)) 
            '()
    ;filter the other list if one of them is empty
    (and (empty? (rest program1)) (not (empty? (rest program2)))) 
            (cons element (filter Keep-Half? (rest program2)))
    (and (empty? (rest program2)) (not (empty? (rest program1)))) 
            (cons element (filter Keep-Half? (rest program1)))
    ;recursive call to perform crossover for next elements
    :else 
      (cons element (crossover (rest program1) (rest program2))))))

(defn add-instructions
  "returns the list with randomly selected instructions inserted
   as indicated in the index_insert list"
  [index_insert program instructions pos]
  (let [current_index (first index_insert)
        current_program_value (first program)
        instruction (rand-nth instructions)]
  (if (and (nil? current_program_value) (nil? current_index))
    '()
   (if (= current_index pos)
    (cons instruction 
                            (add-instructions (rest index_insert) 
                                              program 
                                              instructions
                                              pos))
    (cons current_program_value
                            (add-instructions index_insert 
                                              (rest program) 
                                              instructions
                                              (inc pos)))))))
(defn Insert?
  "returns true to give a 5% probability if rand num is less than 5"
  [n]
  (let [x (+ (rand-int 100) 1)]
  (if (< x 6)
    true
    false)))

(defn uniform-addition
  "Randomly adds new instructions before every instruction (and at the end of
  the program) with some probability. Returns child program."
  [program]
  (let [x (range (inc (count program)))
        ;indices of where to insert instructions
        index_insert (filter Insert? x)]
        (add-instructions index_insert program instructions 0)))

(defn keep?
  "return true if x is 5 or less"
  [n]
  (let [x (+ (rand-int 100) 1)]
  (if (< x 6)
  false
  true)))

(defn uniform-deletion
  "Randomly deletes instructions from program at some rate. Returns child program."
  [prog]
  (filter keep? prog))

(defn target-function
  "Target function: f(x) = x^3 + x + 3
  Should literally compute this mathematical function."
  [x]
  (+ (+ x 3) (* (* x x) x)))

(defn shuffle-index-list
  "Creates a series of integers that increase by 1 that is
  the same length as the given vector. Returns this series,
  shuffled." 
  [error-vector]
  (if (= 0 (count error-vector))
    :empty-vector
    (loop [index-vector []
           index 0]
      (if (= index (count error-vector))
        (shuffle index-vector) ; returns the vector shuffled
        (recur (conj index-vector index) ;; adds index to vector
               (inc index)))))) 

(defn shuffle-errors
  "Returns each indice of a vector in an order that is det-
  ermined by a vector of shuffled indices."
  [errors indices]
  (map #(nth errors %) indices))

(defn edit-individual
  "Shuffles an individual's errors and adds a new key,
  shuffled-errors, to store the shuffled result."
  [individual indices]
  (assoc individual :shuffled-errors (shuffle-errors (get individual :errors) indices)))   
  
(defn get-best-error
  "Returns the individual with the highest value that
  occurs at the given index in the individual's
  shuffled error vector."
  [population index]
  :STUB
  (if (empty? population)
    population
    (loop [population population
           fittest-ind (first population)
           fittest-error (nth (get (first population) :shuffled-errors) index)]
      (if (= 1 (count population))
        ;if last member remaining, return the fittest
        fittest-ind
        ;compare fittest-ind with next individual and adjust fittest-ind accordingly
        (if (< fittest-error (nth (get (second population) :shuffled-errors) index))
          (recur (rest population) fittest-ind fittest-error)
          (recur (rest population) (second population) (nth (get (second population) :shuffled-errors) index)))))))

(defn lexicase-selection
  [population]
  (let [example-error (get (first population) :errors)
        indices (shuffle-index-list example-error)]
    (loop [candidates (map #(edit-individual % indices) population)
           index 0]
      (cond (= 1 (count candidates)) (dissoc (first candidates) :shuffled-errors)
            (= index (count indices))(if (< 1 (count candidates))                                      (dissoc (nth candidates (rand-int (count candidates))) :shuffled-errors)
                                       (dissoc (first candidates) :shuffled-errors))   
            :else
            (let[error-index (nth indices index)
                 best (get-best-error candidates error-index)
                 best-score (nth (get best :shuffled-errors) error-index)
                 new-candidates (filter #(>= best-score (nth (get % :shuffled-errors) error-index)) candidates)]
              (recur new-candidates
                     (inc index)))))))  

(defn select-and-vary
  "Selects parent(s) from population and varies them, returning
  a child individual (note: not program). Chooses which genetic operator
  to use probabilistically. Gives 50% chance to crossover,
  25% to uniform-addition, and 25% to uniform-deletion."
  [population]
  (let [chance (int (rand 4))
        program1 (get (lexicase-selection population) :program)
        program2 (get (lexicase-selection population) :program)
        ;pick one of the genetic operators randomly
        child-program (cond
          (< chance 2) (crossover program1 program2)
          (= chance 2) (uniform-addition program1) 
          (= chance 3) (uniform-deletion program1))]
    ;return program returned by operator
    {:program child-program}))


(defn get-best
  "Selects an individual from the population using a tournament. Returned 
  individual will be a parent in the next generation. Can use a fixed
  tournament size."
  [population]
  :STUB
  (if (empty? population)
    population
    (loop [population population
           fittest-ind (first population)
           fittest-error (get (first population) :total-error)]
      (if (= 1 (count population))
        ;if last member remaining, return the fittest
        fittest-ind
        ;compare fittest-ind with next individual and adjust fittest-ind accordingly
        (if (< fittest-error (get (second population) :total-error))
          (recur (rest population) fittest-ind fittest-error)
          (recur (rest population) (second population) (get (second population) :total-error)))))))
  
(def universal-tests
  '([1]
    [1 2] 
    [2 1]
    [1 1]
    [1 2 3]
    [2 1 3]
    [1 3 2]
    [15 14 13 12 11 10 9 8 7 6 5 4 3 2 1]
    [3 2 1]
    [3 3 2]
    [4 3 5 2 3]
    [8 0 9 5 2]
    [2 8 4 3 10 9 11 10 2 3]))

(defn report
  "Reports information on the population each generation. Should look something
  like the following (should contain all of this info; format however you think
  looks best; feel free to include other info).
;-------------------------------------------------------
;               Report for Generation 3
;-------------------------------------------------------
;Best program: (in1 integer_% integer_* integer_- 0 1 in1 1 integer_* 0 integer_* 1 in1 integer_* integer_- in1 integer_% integer_% 0 integer_+ in1 integer_* integer_- in1 in1 integer_* integer_+ integer_* in1 integer_- integer_* 1 integer_%)
;Best program size: 33
;Best total error: 727
;Best errors: (117 96 77 60 45 32 21 12 5 0 3 4 3 0 5 12 21 32 45 60 77)  "
 [population generation]
  :STUB
  (println "-------------------------------------------------------")
  (println "               Report for Generation ",generation)
  (println "-------------------------------------------------------")
  (let [best-individual (get-best population)
        best-program (get best-individual :program)]
    (println best-program)
    ;;(println "Report:" (get best-individual :report)) 
    (println "Best program size:" (count best-program))
    (println "Best total error:", (get best-individual :total-error))
    (println "Best errors:", (get best-individual :errors))))


(defn get-initial-population
  "initialize the population randomly"
  [population-size max-initial-program-size]
  (take population-size (repeatedly
                         #(assoc base-individual :program (make-random-push-program instructions
                                                            max-initial-program-size)))))
                                             
(defn is-solution?
  "checks if the total error for the ind in 0"
  [individual]
  (let [total-error (get individual :total-error)]
    (if (= 0 total-error)
      true
      false)))

(defn find-solution
  "checks if the solution has been found in the current generation"
  [population]
  (loop [population population]
        (if (empty? population)
          false
         (if (is-solution? (first population))
          true
          (recur (rest population))))))

(defn vector-compare
  [vector1 vector2]
  (if (= (count vector1) (count vector2)) 
    (loop [vec1 vector1
           vec2 vector2
           error 0]
      (if (or (empty? vec1) (empty? vec2))
        error
        (if (= (first vec1) (first vec2))
          (recur (rest vec1)
                 (rest vec2)
                 error)
          (recur (rest vec1)
                 (rest vec2)
                 (inc error)))))
    10000))

(defn get-test-result
  [test-case program]
  (let [start-state (push-to-stack empty-push-state :vector test-case)
        result-state (interpret-push-program program start-state)
        result (peek-stack result-state :vector)
        correct (into [] (sort test-case))]
    (if (or (empty? result) (not= (count result) (count test-case)))
      (list test-case result correct 10000)
      (list test-case result correct (vector-compare result correct)))))

(defn sublist
  [lst]
  (list (first lst) (second lst) (nth lst 2)))

(defn eval-individual
  "Takes an individual and evaluates it on some test cases. For each test case,
  runs program with the input set to :in1 in the :input map part of the Push state.
  Then, the output is the integer on top of the integer stack in the Push state
  returned by the interpreter. Computes each error by comparing output of
  the program to the correct output.
  Returns the individual with :errors set to the list of errors on each case,
  and :total-error set to the sum of the errors.
  Note: You must consider what to do if the program doesn't leave anything
  on the integer stack."
  [individual test-cases]
  (loop [test-cases test-cases
         program (get individual :program)
         errors '()]
        ; report '()]
         ;if all test cases applied return individual with errors
         ;and :total error included
    (if (or (empty? test-cases) (nil? test-cases))
           (-> individual
              (assoc-in  [:errors] errors)
              (assoc-in [:total-error] (reduce +' errors)))
              ;(assoc-in [:report] report))         
           (recur (rest test-cases)
                  program
                  (conj errors
                        (last (get-test-result (first test-cases) program)))))))
                  ;(conj report
                       ; (sublist (get-test-result (first test-cases) program)))))))

;; IGNORE FOLLOWING, JUST NOT READY TO FULLY DELETE YET
"
(defn eval-individual
  Takes an individual and evaluates it on some test cases. For each test case,
  runs program with the input set to :in1 in the :input map part of the Push state.
  Then, the output is the integer on top of the integer stack in the Push state
  returned by the interpreter. Computes each error by comparing output of
  the program to the correct output.
  Returns the individual with :errors set to the list of errors on each case,
  and :total-error set to the sum of the errors.
  Note: You must consider what to do if the program doesn't leave anything
  on the integer stack.
  [individual test-case]
  (let [program (get individual :program)
         error (last (get-test-result test-case program))
         report (sublist (get-test-result test-case program))]
    (-> individual
        (update :errors conj error)
        (update :total-error + error)
        (update :report conj report))))"
        
"(defn lexicase-selection
  [population]
    (loop [test-cases (shuffle universal-tests)
           candidates population]
      (cond (= 1 (count candidates)) (first candidates)
            (= 1 (count test-cases)) (nth candidates (rand-int (count candidates)))
            :else
            (let [test (first test-cases)
                  tested-candidates (map #(eval-individual % test) candidates)
                  best (get-best tested-candidates)
                  filtered-candidates (filter #(>= (get best :total-error)
                                                   (get % :total-error))
                                              tested-candidates)]
              (recur (rest test-cases)
                     filtered-candidates
              )))))"

(defn create-new-generation
  "repreatedly creates a new generation for the given pop size using select-and-vary."
  [population-with-errors population-size]
  (take population-size (repeatedly #(select-and-vary population-with-errors))))

;;;;;;;;;;
;; The main gp algorithm. 
(defn push-gp
  "Main GP loop. Initializes the population, and then repeatedly
  generates and evaluates new populations. Stops if it finds an
  individual with 0 error (and should return :SUCCESS, or if it
  exceeds the maximum generations (and should return nil). Should print
  report each generation.
  --
  The only argument should be a map containing the core parameters to
  push-gp. The format given below will decompose this map into individual
  arguments. These arguments should include:
   - population-size
   - max-generations
   - error-function
   - instructions (a list of instructions)
   - max-initial-program-size (max size of randomly generated programs)"
  [{:keys [population-size max-generations error-function instructions max-initial-program-size]}]
  ;population - this parameter contains initialized population and those returned by tournament selection
  ;population-with-errors - evaluates errors and total-error for each individual of population
  (loop [population (get-initial-population population-size max-initial-program-size)
         population-with-errors (map #(eval-individual % universal-tests) population)
         generation 0]

    (report population-with-errors generation)
       (cond 
          ;return if end-state reached
          (= generation max-generations) nil
          (find-solution population-with-errors) :SUCCESS
          :else 
          ;if end-state not reached, evolve a new generation and make the recursive call
          (let [next-generation (create-new-generation population-with-errors population-size)]
            (recur next-generation (map #(eval-individual % universal-tests) next-generation) (inc generation))))))

;;;;;;;;;;
;; The main function. Uses some problem-specific functions.
(defn -main
  "Runs push-gp, giving it a map of arguments."
  [& args]
  (push-gp {:instructions instructions
            :error-function eval-individual
            :max-generations 100
            :population-size 50
            :max-initial-program-size 50}))
  ;(interpret-push-program example-push-program)
