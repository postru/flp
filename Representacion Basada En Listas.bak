#lang eopl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Autores:  Stivens Posada Trujillo (1556051)  - nn - nn - nn - nn
;; fecha de creacion: 28/01/2021
;; contrato: calcular-distancia: numero, numero, numero, numero -> numero
;; proposito: calcula la distancia entre dos puntos
;; ejemplo: (calcular-distancia 3 2 4 5) retorna 3.1622
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definicion

;;CONSTRUCTORES

(define r-vacio '())


(define r-novacio
  (lambda (item reg)
    (list 'r-novacio item reg)))


(define itemElm
  (lambda (key dato)
    (list 'itemElm key dato)))

(define datoElm
  (lambda (elemento)
    (list 'datoElm elemento)))


(define lnumvacia '())

(define lnumnovacia
  (lambda (num lst)
    (list 'lnumnovacia num lst)))


(define lsymvacia '())

(define lsymnovacia
  (lambda (sym lst)
    (list 'lsymnovacia sym lst)))



;;Observadores

;;PREDICADOS

(define r-vacio?
  (lambda (n)
    (null? n)))

(define r-novacio?
  (lambda (exp)
    (equal? (car exp) 'r-novacio)))

(define itemElm?
  (lambda (exp)
    (equal? (car exp) 'itemElm)))



(define datoElm?
  (lambda (exp)
    (equal? (car exp) 'datoElm)))


(define lnumvacia?
  (lambda (n)
    (null? n)))

(define lnumnovacia?
  (lambda (exp)
    (equal? (car exp) 'lnumnovacia)))


(define lsymvacia?
   (lambda (n)
    (null? n)))

(define lsymnovacia?
   (lambda (exp)
    (equal? (car exp) 'lsymnovacia)))


;; EXTRACTORES

(define r-novacio->item
  (lambda (reg)
    (cadr reg)))

(define r-novacio->reg
  (lambda (reg)
    (caddr reg)))

(define itemElm->key
  (lambda (reg)
    (caddr reg)))

(define itemElm->dato
  (lambda (reg)
    (caddr reg)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Area del Programador


(define apply-env
  (lambda (env sym)
    (cond
      [(equal? (car env) 'empty-env) (eopl:error "no encuentro la variaable " sym)]
      [else
       (if (eqv? sym(cadr env))
           (caddr env)
           (apply-env (cadddr env) sym))])))
