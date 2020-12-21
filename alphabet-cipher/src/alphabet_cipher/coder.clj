(ns alphabet-cipher.coder)

(def sub-chart (reduce (fn [acc c]
                         (conj acc (mapv #(char (+ (mod % 26) 97)) (range c (+ c 26)))))
                       []
                       (range 26)))

(defn encode [keyword message]
  (apply str
         (map (fn [s k]
                (let [index-s (- (int s) 97)
                      index-k (- (int k) 97)]
                  (nth (nth sub-chart index-s) index-k))) message (cycle keyword))))

(defn decode [keyword message]
  (apply str
         (map (fn [c k]
                (let [index-k (- (int k) 97)]
                  (loop [index 0
                         row (nth sub-chart index-k)]
                    (if (or (empty? row) (= c (first row)))
                      (char (+ index 97))
                      (recur (inc index) (rest row))))))
              message
              (cycle keyword))))

(defn decipher [cipher message]
  (let [key-candidates (decode message cipher)]
    (reduce (fn [code nc]
              (if (= message (decode code cipher))
                (reduced code)
                (str code nc)))
            ""
            key-candidates)))
