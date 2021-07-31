(ns twenty.main)

(def board
  [[2 0 2 0]
   [0 8 0 8]
   [0 0 0 0]
   [0 2 2 0]])

(defn rotate-ccw [board]
  "Rotate the board counter clock-wise"
  (->> board
       (apply map vector)
       (reverse)
       (vec)))

(defn pad-zeroes [n row]
  "Add zeroes to the ned of row, to achieve length n."
  (concat row (repeat (- n (count row)) 0)))

(defn squash-row [row]
  "Squash row towards index 0

  Two consecutive blocks of the same value get merged to a single block.
  "
  (->> row
       (reduce
         (fn [acc x]
           (if (zero? x)
             acc
             (if (and (:can-merge acc)
                      (= (last (:result acc)) x))
               (-> acc
                   (assoc :result (conj (pop (:result acc)) (* 2 x)))
                   (assoc :can-merge false))
               (-> acc
                   (update :result conj x)
                   (assoc :can-merge true)))))
         {:can-merge false
          :result []})
       (:result)
       (filter (comp not zero?))
       (pad-zeroes (count row))))


(defn -main [args]
  (println "hello"))
