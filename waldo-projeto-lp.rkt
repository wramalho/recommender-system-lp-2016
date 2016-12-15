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


(define (avaliação avaliador item)
  (cdr (assoc item (cdr (assoc avaliador ideal)))))

; o ideal é ter um mapcar. melhorar
(define (similar avaliador1 avaliador2 f-distance)
  (let* ((items1 (avaliação avaliador1))
         (items2 (avaliação avaliador2))
; issues possíveis: mapcar, o uso do car como proc, set-intersect
         (items-em-comum (map car (set-intersect items1 items2))))
    (if (empty? items-em-comum) empty
        (f-distance avaliador1 avaliador2 items-em-comum))))
(define (reduce op lst)
  (match lst
    ['()             (error "no elements in list")]
    [(list a)         a]
    [(cons hd tl)    (op hd (reduce op tl))]))

(define (euclidean-distance avaliador1 avaliador2 items-em-comum)
  (let* ((sum-of-squares (reduce + (map (lambda (cm)
                                          (expt (- (avaliação avaliador1 cm) (avaliação avaliador2 cm)) 2))
                                        items-em-comum)))
         (distance (/ 1 (add1 sum-of-squares))))
    distance))


(define (sim-distance avaliador1 avaliador2)
  (similar avaliador1 avaliador2 euclidean-distance))
   


; variável para exemplificar a estrutura de dados perseguida; funcionalidade de testes
(define ideal '( ("user1" . (("A" . 1.0) ("B" . 3.0) ("C" . 4.5)))
                 ("user2" . (("A" . 3.0) ("B" . 2.5) ("C" . 5.0)))))



; devolve sugestões pro usuário (snippet para parte final)
; (format "~a, recomendo que você visite a praia ~a! Bom passeio (:" username recommendation-output)