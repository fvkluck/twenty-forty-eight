(ns twenty.main)

(def field
  [[0 0 0 0]
   [0 0 0 0]
   [0 0 0 0]
   [0 0 0 0]])

(defn has-block? [x]
  (not (zero? x)))

(defn get-block [row index]
  (row index))

(defn first-index [pred start row]
  (->> row
       (map-indexed vector)
       (drop start)
       (filter (comp pred second))
       (first)
       (first)))

(defn last-index [pred start row]
  (cond
    (empty? (drop start row)) nil
    (every? pred (drop start row)) (dec (count row))
    ((comp not pred) (first row)) nil
    :else
    (->> row
         (map-indexed (fn [i x] [(dec i) x]))
         (drop start)
         (drop-while (comp pred second))
         (first)
         (first))))

(defn first-zero-index
  ([row]
    (first-zero-index row 0))
  ([row start]
   (first-index zero? start row)))

(defn pad-zeroes [n row]
  (concat row (repeat (- n (count row)) 0)))

(defn squash-row [row]
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
