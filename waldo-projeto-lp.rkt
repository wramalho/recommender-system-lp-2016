#lang racket

; pacote para parsing de arquivos csv, de autoria de Neil Van Dyke
(require csv-reading)



; arquivo teste 
(define my-file "teste.csv")



; wrapper da função make-csv-reader-maker, que lê arquivos a partir de critérios
(define make-csv-to-recommender-reader
  (make-csv-reader-maker
   '((separator-chars            #\,)
     (strip-leading-whitespace?  . #t)
     (strip-trailing-whitespace? . #t))))

; acesso linha a linha de arquivo. obs: está preparada apenas para um arquivo específico; funcionalidade de teste 
(define next-row
  (make-csv-to-recommender-reader (open-input-file my-file)))



(define (csv-to-assoc-list filename)

  (define preferences-raw (csv->list (make-csv-to-recommender-reader (open-input-file filename))))
  (define header-row (list-tail (list-ref preferences-raw 0) 1))
  
  (for/list ((i (cdr preferences-raw)))
    (cons (car i)
          (map cons header-row (cdr i)))))

(define teste (csv-to-assoc-list my-file))

(define (intersect set1 set2)
  (cond [(empty? set1) '()]
        [(empty? set2) '()]

        [(= (caar set1) (caar set2)) (cons (list (caar set1)
                                                 (+ (cadar set1)
                                                    (cadar set2)))
                                           (intersect (cdr set1) (cdr set2)))]
        [(< (caar set1) (caar set2)) (intersect (cdr set1) set2)]
        [else (intersect set1 (cdr set2))]))

(define avaliação
  (lambda (avaliador [item empty])
    (if (empty? item)
        (cdr (assoc avaliador teste))
        (cdr (assoc item (cdr (assoc avaliador teste)))))))

; o ideal é ter um mapcar. melhorar
(define (similar avaliador1 avaliador2 f-distance)
  (let* ((items1 (avaliação avaliador1))
         (items2 (avaliação avaliador2))
         ; issues possíveis: mapcar, o uso do car como proc, set-intersect
         (items-em-comum (set-intersect (map car items1) (map car items2))))
    (if (empty? items-em-comum) empty
        (f-distance avaliador1 avaliador2 items-em-comum))))

(define teste-items1 (avaliação "user1"))
(define teste-items2 (avaliação "user2"))



(define (reduce op lst)
  (match lst
    ['()             (error "no elements in list")]
    [(list a)         a]
    [(cons hd tl)    (op hd (reduce op tl))]))

(define (euclidean-distance avaliador1 avaliador2 items-em-comum)
  (let* ((sum-of-squares (reduce + (map (lambda (cm)
                                          (expt
                                           (- (string->number (avaliação avaliador1 cm)) (string->number(avaliação avaliador2 cm))) 2))
                                        items-em-comum)))
         (distance (/ 1 (add1 sum-of-squares))))
    distance))


(define (pearson-distance avaliador1 avaliador2 items-em-comum)
  (let* (
         (n (length items-em-comum))
         (scores1 (map string->number (map (lambda (x) (avaliação avaliador1 x)) items-em-comum)))
         (scores2 (map string->number (map (lambda (x) (avaliação avaliador2 x)) items-em-comum)))
         (soma1 (reduce + scores1))
         (soma2 (reduce + scores2))
         (soma1-seq (reduce + (map (lambda (x) (* x x)) scores1)))
         (soma2-seq (reduce + (map (lambda (x) (* x x)) scores2)))
         (psum (reduce + (map * scores1 scores2)))
         (num (- psum (/ (* soma1 soma2) n)))
         (den (sqrt (* (- soma1-seq (/ (expt soma1 2) n)) (- soma2-seq (/ (expt soma2 2) n))))))
    (if (zero? den) 0 (/ num den))))

(define (sim-pearson avaliador1 avaliador2)
  (similar avaliador1 avaliador2 pearson-distance))
         
         

(define (sim-distance avaliador1 avaliador2)
  (similar avaliador1 avaliador2 euclidean-distance))


(define (top-matches userx [n 5] (similarity sim-pearson))
  (let* (
         (scores (map (lambda (x) (cons (similarity userx x) x))
                      (filter-not (lambda (x) (equal? x userx)) (map car teste))))
         (scores-organizados (sort scores > #:key car))
         (len (length scores-organizados)))
    (define (butlast-matches lst [n 1])
  (if (< (length lst) n) empty
      (take lst (- len n))))
      
    (if (<= len n)
        scores-organizados
        (butlast-matches scores-organizados))))




; variável para exemplificar a estrutura de dados perseguida; funcionalidade de testes
(define ideal '( ("user1" . (("A" . 1.0) ("B" . 3.0) ("C" . 4.5)))
                 ("user2" . (("A" . 3.0) ("B" . 2.5) ("C" . 5.0)))))


; devolve sugestões pro usuário (snippet para parte final)
; (format "~a, recomendo que você visite a praia ~a! Bom passeio (:" username recommendation-output)