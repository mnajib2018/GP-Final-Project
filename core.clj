(ns finalproject.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(def instructions
  (list
   'order
   'len*
   'start_index
   'exec-do-range
   ))

(def empty-push-state
  {:exec '()
   :input_vector'()
   :integer  '()
   :length  '()})

(def state-2
  {:exec '(exec-do-range order 0 1)
   :input_vector [2.0, 1.0, 4.0, 5.0, 6.0]
   :integer '()
   :length '()})


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


(defn inc1+
  "returns the parameter incremented by 1"
  [state]
  (let [top-integer (peek-stack state :integer)]
  (inc top-integer)))

(defn dec1-
  "returns the top integer decremented by 1"
  [state]
  (let [top-integer (peek-stack state :integer)]
  (dec top-integer)))

(defn integer_-
  "Subtracts the top two integers and leaves result on the integer stack.
  Note: the second integer on the stack should be subtracted from the top integer."
  [x y]
  (- x y))

(defn start_index
  [state]
  (push-to-stack state :integer 0))

(defn calculate-length
  [state]
  (count (get state :input_vector)))

(defn len* 
  [state]
  (let [new-state (push-to-stack state :length (calculate-length state))]
  (if (empty? (get state :length))
        (push-to-stack new-state :integer (first (get new-state :length)))
      (push-to-stack new-state :integer (first (get state :length))))))

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


(defn order
  [state]
  (if (empty? (get state :integer))
    (do)
  (let [index-2 (peek-stack state :integer)
        index-1 (dec index-2)
        length  (calculate-length state)
        new-state (pop-stack (pop-stack state :exec) :exec)]
        (if (and (>= index-1 0) (< index-2 length))
          (let [element-in1 (nth (get state :input_vector) index-1)
                element-in2 (nth (get state :input_vector) index-2)]
        (if (> element-in1 element-in2)
        (let
         [swap-state (assoc-in new-state [:input_vector] (assoc (get new-state :input_vector) index-2 element-in1))]
         (assoc-in swap-state [:input_vector] (assoc (get swap-state :input_vector) index-1 element-in2)))
         (do)))
          (do))))) 

;;;;;;;;;;
;; Interpreter
;;;;;;;;;;;;;;;;;;;;;;;;

(defn instruction?
  "returns true if given list contains given element"
  [ins arg]
      (loop [instructions ins]
        (if (empty? instructions) ; reached end of list 
          false
          (if (= (first instructions) arg) ; found!
            true
            (recur (rest instructions)))))) 


(defn push-literal-to-stack
  "Pushes given element 'top' to the correct stack based on comparison
  using the type function. Only does so if int, string, or input, other-
  wise returns given push state unchanged."
  [new-push-state top]
  (cond 
    ;if integer, string or input push to its stack else return original state
    (= (type top) (type 1))  (push-to-stack new-push-state :integer top) 
    ;(= (type top) (type "abc"))  (push-to-stack new-push-state :string top)
    ;(= (type top) (type {:in1 1}))  (push-to-stack new-push-state :input top)
    :else new-push-state))


(defn interpret-one-step
  "Helper function for interpret-push-program.
  Takes a Push state and executes the next instruction on the exec stack,
  or if the next element is a literal, pushes it onto the correct stack.
  Returns the new Push state."
  [push-state]
  (let [top (peek-stack push-state :exec)
       new-push-state (pop-stack push-state :exec)] 
    (if (instruction? instructions top) ; check instruct or literal
      ((eval top) new-push-state) ; execute instruction
      (push-literal-to-stack new-push-state top))))

(defn load-program
  "adds a program to the exec stack"
  [program start-state]
  (update start-state :exec concat program))

(defn interpret-push-program
  "Runs the given program starting with the stacks in start-state. Continues
  until the exec stack is empty. Returns the state of the stacks after the
  program finishes executing."
  [program start-state]
  (let [loaded-state (load-program program start-state)]
  (loop [current-state loaded-state]
    (if (empty? (get current-state :exec)) 
      current-state ; state when exec is empty
      (let [new-state (interpret-one-step current-state)]
        (recur new-state))))))


;;;;;;;;;;
;; GP

(defn make-random-push-program
  "Creates and returns a new program. Takes a list of instructions and
  a maximum initial program size."
  [instructions max-len]
  (take max-len (repeatedly #(nth instructions (int (rand (count instructions)))))))



(defn get-new-instruction
    "get either a new instruction or make a small program"
    [instructions]
     (let [x (+ (rand-int 10) 1)]
      (if (> x 2)
        (nth instructions (int (rand (count instructions))))
        (make-random-push-program instructions 4))))


(defn make-random-program
  "Create a random push program"
  [instructions max-len]
   (loop [current-len 0
          result-program '()]
      (if (= current-len max-len)
          {:program result-program}
          (recur (inc current-len) 
                 (cons (get-new-instruction instructions) result-program)))))

(defn get-random-sample
  "creates a population sample, including each element of population
  with a 50% chance"
  [population]
  (let [sample (random-sample 0.5 population)]  
    (if (empty? sample) ;just in case none are selected
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
        fittest-ind
        ; keep individual with lower error
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

(defn keep-half?
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
            (cons element (filter keep-half? (rest program2)))
    (and (empty? (rest program2)) (not (empty? (rest program1)))) 
            (cons element (filter keep-half? (rest program1)))
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
        ;return nil program and index are nil
        (if (and (nil? current_program_value) (nil? current_index))
           '()
           ;if index matches for position pos add instruction
           ;at this index without incrementing pos so original
           ;instruction can be added on next loop
           (if (= current_index pos)
            (cons instruction 
                  (add-instructions (rest index_insert) 
                                     program 
                                     instructions
                                     pos))
            ;if current_index not equal to pos
            ;go to next index with rest of program 
            ;next position
            (cons current_program_value
                  (add-instructions index_insert 
                                    (rest program) 
                                    instructions
                                    (inc pos)))))))

(defn insert?
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
        index_insert (filter insert? x)]
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

(defn select-and-vary
  "Selects parent(s) from population and varies them, returning
  a child individual (note: not program). Chooses which genetic operator
  to use probabilistically. Gives 50% chance to crossover,
  25% to uniform-addition, and 25% to uniform-deletion."
  [population]
  (let [chance (int (rand 4))
        program1 (get (tournament-selection population) :program)
        program2 (get (tournament-selection population) :program)
        ;pick one of the genetic operators randomly
        child-program (cond
                          (< chance 2) (crossover program1 program2)
                          (= chance 2) (uniform-addition program1) 
                          (= chance 3) (uniform-deletion program1))]
                      ;return program returned by operator
                      {:program child-program}))

(defn get-best-individual
  "Selects an individual from the population using a tournament. Returned 
  individual will be a parent in the next generation. Can use a fixed
  tournament size."
  [population]
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
  (println "-------------------------------------------------------")
  (println "               Report for Generation ",generation)
  (println "-------------------------------------------------------")
  (let [best-individual (get-best-individual population)
        best-program (get best-individual :program)]
        (println best-program)
        (println "Best program size:" (count best-program))
        (println "Best total error:", (get best-individual :total-error))
        (println "Best errors:", (get best-individual :errors))))
