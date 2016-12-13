#lang racket



(define *RECOMMENDATIONS* 
  '(
    ("Lisa Rose" . (("Lady in the Water" . 2.5) ("Snakes on a Plane" . 3.5) ("Just My Luck" . 3.0) 
                    ("Superman Returns" . 3.5) ("You, Me and Dupree" . 2.5) ("The Night Listener" . 3.0)))
    ("Gene Seymour" . (("Lady in the Water" . 3.0) ("Snakes on a Plane" . 3.5) ("Just My Luck" . 1.5) 
                       ("Superman Returns" . 5.0) ("The Night Listener" . 3.0) ("You, Me and Dupree" . 3.5)))
    ("Michael Phillips" . (("Lady in the Water" . 2.5) ("Snakes on a Plane" . 3.0) 
                           ("Superman Returns" . 3.5) ("The Night Listener" . 4.0)))
    ("Claudia Puig" . (("Snakes on a Plane" . 3.5) ("Just My Luck" . 3.0) ("The Night Listener" . 4.5) 
                       ("Superman Returns" . 4.0) ("You, Me and Dupree" . 2.5)))
    ("Mick LaSalle" . (("Lady in the Water" . 3.0) ("Snakes on a Plane" . 4.0) ("Just My Luck" . 2.0) 
                       ("Superman Returns" . 3.0) ("The Night Listener" . 3.0) ("You, Me and Dupree" . 2.0)))
    ("Jack Matthews" . (("Lady in the Water" . 3.0) ("Snakes on a Plane" . 4.0) ("The Night Listener" . 3.0) 
                        ("Superman Returns" . 5.0) ("You, Me and Dupree" . 3.5)))
    ("Toby" . (("Snakes on a Plane" . 4.5) ("You, Me and Dupree" . 1.0) 
               ("Superman Returns" . 4.0)))))


; refazer &optional, labels, primeiro arg da lista que eh parametro de labels
(define (critics reviewer &optional movie)
  (labels ((get-movie (ms m)
             (cdr (assoc m ms :test #'equalp))))
    (let ((movies (cdr (assoc reviewer *RECOMMENDATIONS* :test #'equalp))))
      (if movie (get-movie movies movie) movies))))

; verificar se tudo está ok!
(define (similar person1 person2 distance)
  (let* ((movies1 (critics person1))
         (movies2 (critics person2))
         (common-movies (mapcar #'car (intersection movies1 movies2 
                                                    :test #'(lambda (x y) (equalp (car x) (car y)))))))
    (if (null common-movies)
        nil
        (funcall distance person1 person2 common-movies))))

; verificar se tudo está ok!
(define (euclidean-distance person1 person2 common-movies)
  (let* ((sum-of-squares (reduce #'+ (mapcar 
                                      #'(lambda (cm) 
                                          (expt (- (critics person1 cm) (critics person2 cm)) 2)) 
                                      common-movies)))
         (distance (/ 1 (1+ sum-of-squares))))
    distance))

; verificar se tudo está ok!
(define (sim-distance person1 person2)
  (similar person1 person2 #'euclidean-distance))

; verificar se tudo está ok e uso de #'' no body da função
(define (pearson-distance person1 person2 common-movies)
  (let* ((n (length common-movies))
         (scores1 (mapcar #'(lambda (x) (critics person1 x)) common-movies))
         (scores2 (mapcar #'(lambda (x) (critics person2 x)) common-movies))
         (sum1 (reduce #'+ scores1))
         (sum2 (reduce #'+ scores2))
         (sum1-sq (reduce #'+ (mapcar #'(lambda (x) (* x x)) scores1)))
         (sum2-sq (reduce #'+ (mapcar #'(lambda (x) (* x x)) scores2)))
         (psum (reduce #'+ (mapcar #'* scores1 scores2)))
         (num (- psum (/ (* sum1 sum2) n)))
         (den (sqrt (* (- sum1-sq (/ (expt sum1 2) n)) (- sum2-sq (/ (expt sum2 2) n))))))
    (if (zerop den) 0 (/ num den))))

; novamente, verificar uso de #''
(define (sim-pearson person1 person2)
  (similar person1 person2 #'pearson-distance))

; verificar se está tudo ok! (qual a dif de len e length)
(define (top-matches person &optional (n 5) (similarity #'sim-pearson))
  (let* ((scores (mapcar #'(lambda (x) (cons (funcall similarity person x) x)) 
                         (remove-if #'(lambda (x) (equalp x person)) (mapcar #'car *RECOMMENDATIONS*))))
         (sorted-scores (sort scores #'> :key #'car))
         (len (length sorted-scores)))
    (if (<= len n)
        sorted-scores
        (butlast sorted-scores (- len n)))))
