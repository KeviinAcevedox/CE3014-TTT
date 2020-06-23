#lang racket
; imports needed for this modules
(require "Matrices.rkt")
#| En este módulo se implementan las funciones que hacen posible ejecutar el algoritmo goloso
requerido para que la máquina tome las decisiones acerca de la mejor posición para el tablero.

El algoritmo heurístico se ejecuta siguiendo los siguientes pasos:

* Conjunto de candidatos: Se debe recibir una matriz y de ella obtener pares ordenados representando a
las posiciones que tengan un 0 (esto quiere decir que la posición está vacía)

* Función de selección: Elije un conjunto de posibles soluciones que deben der evaluadas por la funcion de viabilidad

* Función de viabilidad: Se usa para determinar el peso que ejerce cada candidato seleccionado y poder dar la mejor
selección.

* Función objetivo: Asigna el valor a una solución o a una solución parcial

*Función solución: Indica cuando se ha logrado llegar a una solución
|#


#| Función que recibe una matriz mxn con 1's, 2's y 0's. Recorre la matriz y retorna las posiciones que contengan a un 0.
Las posiciones se devuelven como elementos de una lista.
|#
(define (conjunto_candidatos matriz)
  (conjunto_candidatos_aux matriz 1 1 '()))

(define (conjunto_candidatos_aux matriz fila columna candidatos)
  (cond ((null? matriz) (invertir_lista candidatos))
        ((null? (car matriz)) (conjunto_candidatos_aux (cdr matriz) (+ fila 1) 1 candidatos))
        ((zero? (caar matriz)) (conjunto_candidatos_aux (cons (cdar matriz) (cdr matriz)) fila (+ columna 1) (cons (list fila columna) candidatos)))
        (else (conjunto_candidatos_aux (cons (cdar matriz) (cdr matriz)) fila (+ columna 1) candidatos))))

#| Función de selección: Teniendo un conjunto de candidatos, se debe escoger al mejor para contribuir a la solución
Se va a usar una lista que contiene sublistas, estas sublistas representarán las soluciones que están disponibles para
la máquina, pueden ser soluciones parciales, vacías o casi totales.

Cada elemento de una sublista será una tripleta ---> (fila columna valor)
|#

;función que convierte un vector fila o vector columna en posiciones triples --> (fila columna valor)
;Recibe: Una fila o columna, el numero de fila o columna del vector, el valor del jugador (1 si es el usuario, 2 si es la máquina)
;y por último el tipo de vector --> ("fila" o "columna")


(define (vector_solucion vector posicion_vector valor tipo_vector)
  (vector_solucion_aux vector valor '() posicion_vector 1 tipo_vector))

(define (vector_solucion_aux vector valor solucion constante variante tipo_vector)
  (cond ((null? vector) (invertir_lista solucion))
        ((and (equal? tipo_vector "fila") (or (equal? (car vector) valor) (zero? (car vector))))(vector_solucion_aux (cdr vector) valor (cons (list constante variante (car vector)) solucion) constante (+ variante 1) tipo_vector))
        ((and (equal? tipo_vector "columna") (or (equal? (car vector) valor) (zero? (car vector))))(vector_solucion_aux (cdr vector) valor (cons (list variante constante (car vector)) solucion) constante (+ variante 1) tipo_vector))
        (else #f)))