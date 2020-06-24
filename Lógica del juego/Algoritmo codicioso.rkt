#lang racket
; imports needed for this modules
(require "Matrices.rkt")
#| En este módulo se implementan las funciones que hacen posible ejecutar el algoritmo goloso
requerido para que la máquina tome las decisiones acerca de la mejor posición para el tablero.

El algoritmo heurístico se ejecuta siguiendo los siguientes pasos:

* Conjunto de candidatos: Se debe recibir una matriz y de ella obtener pares ordenados representando a
las posiciones que tengan un 0 (esto quiere decir que la posición está vacía)

* Función de selección: Elije un conjunto de posibles soluciones que deben der evaluadas por la funcion de viabilidad
Será fusionada junto con la función de conjunto candidatos. Esto debido a que los candidatos seleccionados serán todos
los que tengan 0´s (estén disponibles)

* Función de viabilidad: Se usa para determinar el peso que ejerce cada candidato seleccionado y poder dar la mejor
selección.

* Función objetivo: Tomará el resultado de la función de viabilidad y dará una nueva posición a la matriz, este es el
candidato que tenga más peso.

*Función solución: Indica cuando se ha logrado llegar a una solución
|#


#| Función que recibe una matriz mxn con 1's, 2's y 0's. Recorre la matriz y retorna las posiciones que contengan a un 0.
Las posiciones se devuelven como pares ordenados dentro de una lista.
|#
(define (seleccionar_candidatos matriz)
  (seleccionar_candidatos_aux matriz 1 1 '()))

(define (seleccionar_candidatos_aux matriz fila columna candidatos)
  (cond ((null? matriz) (invertir_lista candidatos))
        ((null? (car matriz)) (seleccionar_candidatos_aux (cdr matriz) (+ fila 1) 1 candidatos))
        ((zero? (caar matriz)) (seleccionar_candidatos_aux (cons (cdar matriz) (cdr matriz)) fila (+ columna 1) (cons (list fila columna) candidatos)))
        (else (seleccionar_candidatos_aux (cons (cdar matriz) (cdr matriz)) fila (+ columna 1) candidatos))))

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