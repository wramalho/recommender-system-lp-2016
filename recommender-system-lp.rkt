#lang racket

; pacote para parsing de arquivos csv, de autoria de Neil Van Dyke
(require csv-reading)

; arquivo a ser lido 
(define my-file "teste.csv")
(define my-beaches "praias.csv")

; wrapper da função make-csv-reader-maker, que lê arquivos .csv utilizando determinados critérios de formatação e os retorna em listas
(define make-csv-to-recommender-reader
  (make-csv-reader-maker
   '((separator-chars            #\,)
     (strip-leading-whitespace?  . #t)
     (strip-trailing-whitespace? . #t))))

; acesso linha a linha de arquivo. obs: está preparada apenas para um arquivo específico; funcionalidade de teste 
(define next-row
  (make-csv-to-recommender-reader (open-input-file my-file)))

; transforma a lista produto do arquivo .csv em uma lista associativa
(define (csv-to-assoc-list filename)
  (define preferences-raw (csv->list (make-csv-to-recommender-reader (open-input-file filename))))
  (define header-row (list-tail (list-ref preferences-raw 0) 1))
  
  (for/list ((i (cdr preferences-raw)))
    (cons (car i)
          (map cons header-row (cdr i)))))

; nomes para os leitores de .csv
(define data-teste (csv-to-assoc-list my-file))
(define data (csv-to-assoc-list my-beaches))


; procedure que captura as preferências de um usuário; é possível capturar a preferência sobre um item específico
(define (avaliação avaliador [item empty] [dataset data])
  (if (empty? item)
      (cdr (assoc avaliador dataset))
      (cdr (assoc item (cdr (assoc avaliador dataset))))))

; procedure que computa a similaridade entre dois usuários dado um procedure de similaridade (distância euclidiana, similaridade jacquartiana etc)
(define (similar avaliador1 avaliador2 f-distance)
  (let* ((items1 (avaliação avaliador1))
         (items2 (avaliação avaliador2))
         (items-em-comum (set-intersect (map car items1) (map car items2))))
    (if (empty? items-em-comum) empty
        (if (equal? cosine-distance-normalized f-distance)
            (f-distance avaliador1 avaliador2)
            (f-distance avaliador1 avaliador2 items-em-comum)))))

;implementação em racket da reduce de clisp
(define (reduce proc lst)
  (match lst
    ('()             (error "lista sem elementos"))
    ((list x)         x)
    ((cons head tail)    (proc head (reduce proc tail)))))

;procedure de critério de similaridade; distância euclidiana
(define (euclidean-distance avaliador1 avaliador2 items-em-comum)
  (let* ((sum-of-squares
          (reduce +
                  (map (lambda (cm)(expt
                                    (- (string->number (avaliação avaliador1 cm)) (string->number(avaliação avaliador2 cm))) 2)) items-em-comum)))
         (distance (/ 1 (add1 sum-of-squares))))
    distance))

;procedure de critério de similaridade; correlação de pearson
(define (pearson-correlation avaliador1 avaliador2 items-em-comum)
  (let* ((n (length items-em-comum))
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


;procedure de critério de similaridade; coseno

(define (cosine-distance-normalized avaliador1 avaliador2)

  (define (avg* lst) (/ (apply + (filter-not zero? lst)) (length (filter-not zero? lst))))
    
  (let* ((data1 (filter number? (map string->number (flatten (cdr (assoc avaliador1 data))))))
         (data2 (filter number? (map string->number (flatten (cdr (assoc avaliador2 data))))))
         (vector-1-norm (map (lambda (x)(- x (avg* data1))) (filter-not zero? data1)))
         (vector-2-norm (map (lambda (x)(- x (avg* data2))) (filter-not zero? data2))))
         
    (/ (apply + (map * vector-1-norm vector-2-norm))
       (*
        (sqrt (apply + (map (lambda (x) (expt x 2)) vector-1-norm)))
        (sqrt (apply + (map (lambda (x) (expt x 2)) vector-2-norm)))))))


; calcula a similaridade entre dois users a partir da correlação de pearson
(define (sim-pearson avaliador1 avaliador2)
  (similar avaliador1 avaliador2 pearson-correlation))
         
; calcula a similaridade entre dois users a partir da distância euclidiana
(define (sim-distance avaliador1 avaliador2)
  (similar avaliador1 avaliador2 euclidean-distance))

; calcula a similaridade entre dois users a partir da distância do cosseno entre os vetores de avaliador1 e avaliador2
(define (sim-cosine avaliador1 avaliador2)
  (similar avaliador1 avaliador2 cosine-distance-normalized))

;calcula as similaridades de todos os usuários em relação ao usuário x
(define (top-matches userx [n 25] [similarity sim-pearson] [dataset data])
  (let* ((scores (map (lambda (x) (cons (similarity userx x) x))
                      (filter-not (lambda (x) (equal? x userx)) (map car dataset))))
         (scores-organizados (sort scores > #:key car))
         (len (length scores-organizados)))
    (define (butlast-matches lst [n 1])
      (if (< (length lst) n) empty
          (take lst (- len n))))
      
    (if (<= len n)
        scores-organizados
        (butlast-matches scores-organizados))))

; lista das chaves da lista associativa sem o "userx" que não precisa ser acessado pelas demais funções 
(define keys (remove "userx" (map car data)))
; lista de valores da lista associativa
(define vals (map cdr data))
; lista de todos os items possivelmente avaliados 
(define items (map car (cdr (car data))))

#| computa os melhores items a serem sugeridos, a partir do produto da avaliação de cada usuário
e sua similaridade em relação ao usuário x. em seguida, soma essas notas para cada item e divide esse
total pelo somatório das similaridades em relacão ao usuário x de todos os demais usuários que avaliaram o respectivo item, para
 evitar que items mais avaliados sejam favorecidos nas recomendações|#
(define (sum-all-scores [dataset data])
  (define lista-valores empty)
  (define (reverse-dict dictionary)
    (map (lambda (x) (cons (cdr x) (car x))) dictionary))
  (for/list ((f items))
    (define lista-acumuladora  empty)
    (for/list ((i keys))
      (define similaridade-para-userx (dict-ref (reverse-dict (top-matches "userx")) i))                   
      (set! lista-acumuladora (cons lista-acumuladora (* similaridade-para-userx (string->number (dict-ref (dict-ref data i) f))))))
    (set! lista-valores (append lista-valores (list (cons f (apply + (flatten lista-acumuladora)))))))
  lista-valores)
        
        
#| é o procedimento que será utilizado pelo usuário. ele insere as preferências do usuário x no dataset, remove as
recomendações que o usuário x já conhece e apresenta a melhor recomendação para o usuário |#
(define (get-recommendations userxprefs [similarity sim-pearson])
  (set! data (append data (list (append '("userx") userxprefs))))
  (define (sort-dicts dict)
    (sort dict (lambda (x y) (> (cdr x) (cdr y)))))
 
  (define (recomendação-única recommendations)
    (define recomendação-final (make-hash (sum-all-scores)))
    
    (for/list ((i (map car (dict-ref data "userx"))))
      (dict-remove! recomendação-final i))
    (format "Olá! Aproveite as férias e conheça (a praia d@) ~a"(caar (sort-dicts (hash->list recomendação-final)))))
  (recomendação-única (sum-all-scores)))
   









; nomes para facilitar a apresentação do trabalho
(define teste-userx '(("A" . "4.2") ("B" . "3") ("C" . "1")))
(define praias-waldo '(("Botafogo" . "3") ("Flamengo" . "2") ("Copacabana" . "3") ("Ipanema" . "4") ("Urca" . "3") ("Praia Vermelha" . "3") ("Arpoador" . 3)))





; variável para exemplificar a estrutura de dados perseguida; funcionalidade de testes
(define ideal '(("user1" . (("A" . 1.0) ("B" . 3.0) ("C" . 4.5)))
                ("user2" . (("A" . 3.0) ("B" . 2.5) ("C" . 5.0)))))

; outra variável como acima

(define movie-critics 
  '(("Lisa Rose" . (("Lady in the Water" . 2.5) ("Snakes on a Plane" . 3.5) ("Just My Luck" . 3.0) 
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

