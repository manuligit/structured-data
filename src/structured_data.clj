(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
  (Math/pow xx xx)))

(defn spiff [v]
  (let [v1 (get v 0)
        v2 (get v 2)]
    (+ v1 v2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
   (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
   (- y2 y1)))

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
   (== (height rectangle)
       (width rectangle))))

(defn area [rectangle]
  (* (height rectangle)
     (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
       [xx yy] point]
  (and (<= x1 xx x2)(<= y1 yy y2))))


(defn contains-rectangle? [outer inner]
    (let [[[x1 y1] [x2 y2]] outer
          [inner1 inner2] inner]
  (and (contains-point? outer inner1)
       (contains-point? outer inner2))))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (<= 2 (author-count book)))

(defn add-author [book new-author]
  (let [authors (:authors book)
  new-book (conj authors new-author)]
  (assoc book :authors new-book)
  ))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second (fn [x] (get x 1))]
    (map second collection)))

(defn titles [books]
  (let [books (fn [x] (:title x))])
  (map :title books))

(defn stars [n]
  (apply str (repeat n "*")))

(defn monotonic? [a-seq]
  (or
    (apply <= a-seq)
    (apply >= a-seq)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (== (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (let [newauthor (:authors book)]
    (assoc book :authors (set newauthor))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
    (set (map :name (authors books))))

(defn author->string [author]
  :-)

(defn authors->string [authors]
  :-)

(defn book->string [book]
  :-)

(defn books->string [books]
  :-)

(defn books-by-author [author books]
  :-)

(defn author-by-name [name authors]
  :-)

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%
