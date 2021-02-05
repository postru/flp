#lang eopl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Autores:  Jenny Carolina Tangarife, 1765553. | Stivens Posada Trujillo, 1556051. | Nelson Galeano, 1958956. |  Diego Toro Florez, 1859942. ;;
;; Fecha de creacion: 28/01/2021                                                                                                              ;;                                                                   ;;
;; Proposito: Procedimientos que permiten indexar una lista de registros mediante una llave, saber si un registro contiene lista de registros ;;
;;            y si no contiene.                                                                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definicion

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;  CONSTRUCTORES  ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Constructor que permite definir un registro vacio
(define r-vacio
  (lambda ()
    (lambda (signal)
      (cond
        [(= signal 0) 'r-vacio]
      ))))

;; Constructor que permite definir un registro y los datos que contiene (item, registro)  
(define r-novacio
  (lambda (item reg)
    (lambda (signal)
      (cond
        [(= signal 0) 'r-novacio]
        [(= signal 1) item ]
        [(= signal 2) reg]
        [else (eopl:error "Error en r-novacio")]))))


;; Constructor que permite definir un item y los datos que contiene (llave , dato)
(define itemElm
  (lambda (key dato)
    (lambda (signal)
      (cond
        [(= signal 0) 'itemElm]
        [(= signal 1) key ]
        [(= signal 2) dato]
        [else (eopl:error "Error en itemElm")]))))


;; Constructor que permite definir un dato y los datos que contiene (elemento) -> <numero | simbolo | lista de numeros | lista de simbolos > 
(define datoElm
  (lambda (elemento)
    (lambda (signal)
      (cond
        [(= signal 0) 'datoElm]
        [(= signal 1) elemento ]
        [else (eopl:error "Error en datoElm")]))))


;; Constructor que permite definir una lista de numeros vacia 
(define lnumvacia
  (lambda ()
    (lambda (sym) (eopl:error "no encuentro  " sym))
      ))

;; Constructor que permite definir una lista de numeros y los datos que contiene (numero, lista de numeros) 
(define lnumnovacia
  (lambda (num lst)
    (lambda (signal)
      (cond
        [(= signal 0) 'lnumnovacia]
        [(= signal 1) num ]
        [(= signal 2) lst]
        [else (eopl:error "Error en lnumnovacia")]))))


;; Constructor que permite definir una lista de simbolos vacia
(define lsymvacia
  (lambda ()
    (lambda (sym) (eopl:error "no encuentro  " sym))
      ))

;; Constructor que permite definir una lista de simbolos y los datos que contiene (numero, lista de simbolos)
(define lsymnovacia
  (lambda (sym lst)
    (lambda (signal)
      (cond
        [(= signal 0) 'lsymnovacia]
        [(= signal 1) sym ]
        [(= signal 2) lst]
        [else (eopl:error "Error en lsymnovacia")]))))




;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; Observadores  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; PREDICADOS  ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Nos permite consultar si es un registro vacio
(define r-vacio?
  (lambda (exp)
    (equal? (exp 0) 'r-vacio)))

;;Nos permite consultar si un registro no es vacio
(define r-novacio?
  (lambda (exp)
    (equal? (exp 0) 'r-novacio)))

;;Nos permite consultar si tiene un item
(define itemElm?
  (lambda (exp)
    (equal? (exp 0) 'itemElm)))


;;Nos permite consultar si tiene un dato
(define datoElm?
  (lambda (exp)
    (equal? (exp 0) 'datoElm)))

;;Nos permite consultar si es una lista
(define lnumvacia?
  (lambda (reg)
    (list? reg)))

;;Nos permite consultar si es una lista de numeros no vacia 
(define lnumnovacia?
  (lambda (exp)
    (equal? (exp 0) 'lnumnovacia)))

;;Nos permite consultar si es una lista de simbolos vacia 
(define lsymvacia?
   (lambda (n)
    (null? n)))

;;Nos permite consultar si es una lista de simbolos no vacia 
(define lsymnovacia?
   (lambda (exp)
    (equal? (exp 0) 'lsymnovacia)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; EXTRACTORES  ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Nos permite extraer el item vacio de un registro
(define r-vacio->item
  (lambda (reg)
    (car reg)))

;;Nos permite extraer el item de un registro
(define r-novacio->item
  (lambda (reg)
    (reg 1)))

;;Nos permite extraer el registro 
(define r-novacio->reg
  (lambda (reg)
    (reg 2)))

;;Nos permite extraer la llave de un item
(define itemElm->key
  (lambda (reg)
    (reg 1)))

;;Nos permite extraer el dato de un item
(define itemElm->dato
  (lambda (reg)
    (reg 2)))

;;Nos permite extraer el elemento que contiene el dato
(define datoElm->elemento
  (lambda (dat)
    (dat 1)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Area del Programador  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Creacion de los registros para su evluacion  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define registro1 (r-novacio (itemElm 'a (datoElm 1))
                             (r-novacio (itemElm 'b (datoElm 2))
                                        (r-novacio (itemElm 'c (datoElm '(lnumnovacia 1 (lnumnovacia 2 (lnumnovacia 3 (lnumvacia))))))
                                                   (r-vacio)))))

(define registro2
  (r-vacio ))


(define registro3 (r-novacio (itemElm 'a (datoElm 1))
                             (r-novacio (itemElm 'b (datoElm 2))
                                        (r-novacio (itemElm 'c (datoElm 33))
                                                   (r-vacio)))))

(define registro4 (r-novacio (itemElm 'c (datoElm '(lnumnovacia 1 (lnumnovacia 2 (lnumnovacia 3 (lnumvacia))))))
                             (r-novacio (itemElm 'r (datoElm '(lnumnovacia 8 (lnumnovacia 6 (lnumnovacia 3 (lnumvacia))))))
                                        (r-novacio (itemElm 's (datoElm 1))
                                                   (r-vacio)))))

(define registro5 (r-novacio (itemElm 'a (datoElm 4))
                             (r-novacio (itemElm 'c (datoElm 5))
                                        (r-novacio (itemElm 'r (datoElm (lsymnovacia 'a (lsymnovacia 'v (lsymnovacia 'b (lsymvacia))))))
                                                   (r-vacio)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; funcion que retorna el valor de accuerdo a una llave  ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Autores: Jenny Carolina Tangarife, 1765553. | Stivens Posada Trujillo, 1556051. | Nelson Galeano, 1958956. |  Diego Toro Florez, 1859942.
;;Fecha de creacion: 28/01/2021
;;Contrato: buscar-llave: registro, simbolo-> dato(simbolo | numero | lista de numeros | lista de simbolos)
;;Proposito: buscar el dato que contiene un registro mediante su llave para ser indexado
;;Ejemplo:
;; (display(buscar-llave registro1 'c)) -> (lnumnovacia 1 (lnumnovacia 2 (lnumnovacia 3 (lnumvacia))))

(define buscar-llave
  (lambda (registro llave) 
    (cond
      [(r-vacio? registro)
       (eopl:error "No encuentro el registro")]
      [else
       (if (equal? llave (itemElm->key(r-novacio->item registro)))
           (datoElm->elemento(itemElm->dato(r-novacio->item registro)))
           (buscar-llave (r-novacio->reg registro) llave))])))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Funcion que retorna una lista de listas que contiene todos los elementos que son listas ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Autores: Jenny Carolina Tangarife, 1765553. | Stivens Posada Trujillo, 1556051. | Nelson Galeano, 1958956. |  Diego Toro Florez, 1859942.
;;Fecha de creacion: 28/01/2021
;;Contrato: buscar-listas: registro -> Lista de registros(lista de numeros | lista de simbolos)
;;Proposito:buscar los datos de un item los cuales contienen listas de simbolos o numeros
;;Ejemplo:
;;(display(buscar-listas registro4)) -> ((lnumnovacia 1 (lnumnovacia 2 (lnumnovacia 3 (lnumvacia)))) ((lnumnovacia 8 (lnumnovacia 6 (lnumnovacia 3 (lnumvacia)))) ()))

(define buscar-listas
  (lambda (reg)
    (cond
      [(r-vacio? reg) '()]
      [(lnumvacia? (datoElm->elemento(itemElm->dato(r-novacio->item reg))))
       (list (datoElm->elemento(itemElm->dato(r-novacio->item reg)))
             (buscar-listas (r-novacio->reg reg)))]
      [else (buscar-listas (r-novacio->reg reg))])))
      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Funcion que retorna una lista que contiene todos los elementos que no son listas  ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Autores: Jenny Carolina Tangarife, 1765553. | Stivens Posada Trujillo, 1556051. | Nelson Galeano, 1958956. |  Diego Toro Florez, 1859942.
;;Fecha de creacion: 28/01/2021
;;Contrato: buscar-nolistas: registro-> (simbolo | numero )
;;Proposito: buscar los datos de un item los cuales no contienen listas de simbolos o numeros
;;Ejemplo:
;; (display(buscar-llave registro1 'c)) -> (lnumnovacia 1 (lnumnovacia 2 (lnumnovacia 3 (lnumvacia))))

(define buscar-nolistas
  (lambda (reg)
    (cond
      [(r-vacio? reg) '()]
      [(not (lnumvacia? (datoElm->elemento(itemElm->dato(r-novacio->item reg)))))(list (datoElm->elemento(itemElm->dato(r-novacio->item reg))) (buscar-nolistas (r-novacio->reg reg)))]
      [else (buscar-nolistas (r-novacio->reg reg))])))



      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              PRUEBAS               ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;(display(buscar-llave registro1 'c))
;(display(buscar-llave registro2 'c))
;(display(buscar-llave registro4 'r))
;(display(buscar-llave registro5 'r))

;(display(buscar-listas registro1))
;(display(buscar-listas registro3))
;(display(buscar-listas registro4))

;(display(buscar-nolistas registro1))
;(display(buscar-nolistas registro3))
;(display(buscar-nolistas registro4))


