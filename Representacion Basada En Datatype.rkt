#lang eopl

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Autores:  Jenny Carolina Tangarife, 1765553. | Stivens Posada Trujillo, 1556051. | Nelson Galeano, 1958956. |  Diego Toro Florez, 1859942. ;;
;; Fecha de creacion: 29/01/2021                                                                                                              ;;                                                                   ;;
;; Proposito: Construir e implementar interfaces que permitan indexar una lista de registros mediante una llave,                              ;;
;;            saber si un registro contiene lista de registros y si no contiene.                                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Esta funcion crea los datos que contiene un registro (item,registro)
(define-datatype registro registro?
  (r-vacio)
  (r-novacio (item item?)
             (reg registro?)))

;;Esta funcion crea los datos que contiene un item (llave, dato)
(define-datatype item item?
  (itemElm (key symbol?)
           (dato dato?)))

;;Esta funcion crea los datos que contiene un dato (elemento)
(define-datatype dato dato?
  (datoElm (elemento element?)))

;;Esta funcion define los datos que lleva un elemento (numero | simbolo | lista de numeros | lista de simbolos)
(define element?
  (lambda (x)
    (cond
      [(number? x) #true]
      [(symbol? x) #true]
      [(lnumero? x) #true]
      [(lsimbolo? x) #true]
      [else #false])))

;;Esta funcion crea los datos que contiene una lista de numeros (lista de numeros)
(define-datatype lnumero lnumero?
  (lnumvacia)
  (lnumnovacia (num number?)
               (lnumero lnumero?)))

;;Esta funcion crea los datos que contiene una lista de simblos
(define-datatype lsimbolo lsimbolo?
  (lsymvacia)
  (lsymnovacia (sym symbol?)
               (lsimbolo lsimbolo?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Area del Programador  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Creacion de los registros para su evluacion  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define registro1 (r-novacio (itemElm 'a (datoElm 1))
                             (r-novacio (itemElm 'b (datoElm 2))
                                        (r-novacio (itemElm 'c (datoElm (lnumnovacia 1 (lnumnovacia 2 (lnumnovacia 3 (lnumvacia))))))
                                                   (r-vacio)))))

(define registro2
  (r-vacio ))


(define registro3 (r-novacio (itemElm 'a (datoElm 1))
                             (r-novacio (itemElm 'b (datoElm 2))
                                        (r-novacio (itemElm 'c (datoElm 33))
                                                   (r-vacio)))))

(define registro4 (r-novacio (itemElm 'c (datoElm (lnumnovacia 1 (lnumnovacia 2 (lnumnovacia 3 (lnumvacia))))))
                             (r-novacio (itemElm 'r (datoElm (lnumnovacia 8 (lnumnovacia 6 (lnumnovacia 3 (lnumvacia))))))
                                        (r-novacio (itemElm 's (datoElm 1))
                                                   (r-vacio)))))

(define registro5 (r-novacio (itemElm 'a (datoElm 4))
                             (r-novacio (itemElm 'c (datoElm 5))
                                        (r-novacio (itemElm 'r (datoElm (lsymnovacia 'Jenny(lsymnovacia 'Stivens (lsymnovacia 'Nelson (lsymnovacia 'Diego (lsymvacia)))))))
                                                   (r-vacio)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; funcion que retorna el valor de accuerdo a una llave  ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Autores: Jenny Carolina Tangarife, 1765553. | Stivens Posada Trujillo, 1556051. | Nelson Galeano, 1958956. |  Diego Toro Florez, 1859942.
;;Fecha de creacion: 28/01/2021
;;Contrato: buscar-llave: simbolo, registro-> dato(simbolo | numero | lista de numeros | lista de simbolos)
;;Proposito: buscar el dato que contiene un registro mediante su llave para ser indexado
;;Ejemplo:
;; (display(buscar-llave 'c registro1)) -> (lnumnovacia 1 (lnumnovacia 2 (lnumnovacia 3 (lnumvacia))))
 
 (define buscar-llave
  (lambda (llave regis)
     (cond
      [(registro? regis)(cases registro regis
                             (r-vacio () '())
                             (r-novacio (item reg) (cond
                                                     [(eqv? (buscar-llave llave item) #false) (buscar-llave llave reg)]
                                                     [else (buscar-llave llave item)])))]
      [(item? regis) (cases item regis
                       (itemElm (key dato) (cond
                                             [(eqv? llave key) (buscar-llave llave dato)]
                                             [else #false])))]
      [(dato? regis) (cases dato regis
                       (datoElm (element) element))])))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Funcion que retorna una lista de listas que contiene todos los elementos que son listas ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Autores: Jenny Carolina Tangarife, 1765553. | Stivens Posada Trujillo, 1556051. | Nelson Galeano, 1958956. |  Diego Toro Florez, 1859942.
;;Fecha de creacion: 28/01/2021
;;Contrato: buscar-listas: registro -> Lista de registros(lista de numeros | lista de simbolos)
;;Proposito:Construir una lista que contiene todos los datos que son listas de simbolos o listas de números.
;;Ejemplo:
;;(display(buscar-listas registro4)) -> ((lnumnovacia 1 (lnumnovacia 2 (lnumnovacia 3 (lnumvacia)))) ((lnumnovacia 8 (lnumnovacia 6 (lnumnovacia 3 (lnumvacia)))) ()))

(define buscar-listas
  (lambda (regis)
     (cond
      [(registro? regis)(cases registro regis
                             (r-vacio () '())
                             (r-novacio (item reg) (cond
                                                     [(eqv? (buscar-listas item) #false) (buscar-listas reg)]
                                                     [else (list (buscar-listas item) (buscar-listas reg))])))]
      [(item? regis) (cases item regis
                       (itemElm (key dato) (buscar-listas dato)))]
                                             
      [(dato? regis) (cases dato regis
                       (datoElm (element) (cond
                                            [(lnumero? element) element]
                                            [(lsimbolo? element) element]
                                            [else #false])))])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Funcion que retorna una lista que contiene todos los elementos que no son listas  ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Autores: Jenny Carolina Tangarife, 1765553. | Stivens Posada Trujillo, 1556051. | Nelson Galeano, 1958956. |  Diego Toro Florez, 1859942.
;;Fecha de creacion: 28/01/2021
;;Contrato: buscar-nolistas: registro-> (simbolo | numero )
;;Proposito: Construir una lista que contiene todos los datos que no son listas de simbolos o listas de números.
;;Ejemplo:
;; (display(buscar-llave registro1)) -> (1 (2 ()))

(define buscar-nolistas
  (lambda (regis)
     (cond
      [(registro? regis)(cases registro regis
                             (r-vacio () '())
                             (r-novacio (item reg) (cond
                                                     [(eqv? (buscar-nolistas item) #false) (buscar-nolistas reg)]
                                                     [else (list (buscar-nolistas item) (buscar-nolistas reg))])))]
      [(item? regis) (cases item regis
                       (itemElm (key dato) (buscar-nolistas dato)))]
                                             
      [(dato? regis) (cases dato regis
                       (datoElm (element) (cond
                                            [(lnumero? element) #false]
                                            [(lsimbolo? element) #false]
                                            [else element])))])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              PRUEBAS               ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;(display(buscar-llave 'c registro1))
;(display(buscar-llave 'c registro2))
;(display(buscar-llave 'r registro4))
;(display(buscar-llave 'r registro5))

;(display(buscar-listas registro1))
;(display(buscar-listas registro3))
;(display(buscar-listas registro4))

;(display(buscar-nolistas registro1))
;(display(buscar-nolistas registro3))
;(display(buscar-nolistas registro4))
      
   

