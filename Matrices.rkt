#lang racket
#|
FUNCIONES PRINCIPALES SOBRE MATRICES:
* Crear matrices limpias de dimensión MxN
* Validar si las dimensiones son correctas

-SIMBOLOGÍA PARA LA MATRIZ:
0 equivale a un espacio vació
1 equivale a un espacio ocupado por el usuario
2 equivale a un espacio ocupado por la máquina

-REPRESENTACIÓN FORMAL DE UNA MATRIZ 3X3 CON TODOS LOS ESPACIOS VACÍOS:

( (0 0 0)
  (0 0 0)
  (0 0 0) )
|#

#|
Función que recibe el número de filas y columnas de una matriz y devuelve un #t si cumple con
las dimensiones adecuadas, y un #f en caso contrario.
|#
(define (verificar_dimensiones columnas filas)
  (and (>= filas 3) (<= filas 10) (>= columnas 3) (<= columnas 10)))

#|
Función que recibe el numero de filas y columnas deseadas. Verifica que las dimensiones sean correctas
y si cumplen con las reglas llama a una función auxilar para crear la matriz mxn
Entrada: El numero de columnas n y el número de filas m
Salida: Una matriz vacía (con 0´s) con m filas y n columnas
|#
(define (crear_matriz filas columnas)
  (cond ((not (verificar_dimensiones columnas filas)) "Las dimensiones dadas no son correctas")
        (else (crear_matriz_aux filas columnas columnas '() '()))))

#|
Función que recibe el numero de columnas y filas deseadas para la matriz, un contador de columna, una lista
y la matriz que se retorna al final.

Entrada: filas - columnas - contador_columnas - lista - matriz_final
Salida: una matriz mxn con todos los espacios vaciós
|# 
(define (crear_matriz_aux filas columnas contador_columnas lista matriz_final)
  (cond ((zero? filas) matriz_final)
        ((zero? contador_columnas) (crear_matriz_aux (- filas 1) columnas columnas '() (cons lista matriz_final)))
        (else (crear_matriz_aux filas columnas (- contador_columnas 1) (cons 0 lista) matriz_final))))
  
; Ejemplo de una definición de la matriz como un tipo de dato
(define tablero (crear_matriz 6 6))

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

; Función que recibe una lista e invierte el orden de sus elementos
(define(invertir_lista lista)
  (cond ((null? lista) '())
        (else (append (invertir_lista (cdr lista)) (list (car lista))))))

#|
Función que recibe una posición (fila columna) y un valor 1 o 2 y además recibe una matriz.
Se tiene que recorrer la matriz desde la posición (1 1) hasta encontrar la posición solicitada.

Cuando la fila es igual al contador, se envía dicha fila a la función auxiliar. En donde se le coloca el nuevo valor
en la posición correspondiente.

Luego se pega la fila retornada por la función auxilar con el resto de la matriz.
|#
(define (modificar_matriz posicion valor matriz)
  (colocar_valor (car posicion) (cadr posicion) 1 valor matriz '()))

(define (colocar_valor fila columna contador valor matriz matriz_final)
  (cond ((null? matriz)(invertir_lista matriz_final))
        ((equal? contador fila)(colocar_valor fila columna (+ contador 1) valor (cdr matriz) (cons (colocar_valor_aux (car matriz) columna 1 valor '()) matriz_final)))
        (else (colocar_valor fila columna (+ contador 1) valor (cdr matriz) (cons (car matriz) matriz_final)))))
                               
(define (colocar_valor_aux lista columna contador valor lista_final)
  (cond ((null? lista)(invertir_lista lista_final))
        ((equal? contador columna)(colocar_valor_aux (cdr lista) columna (+ contador 1) valor (cons valor lista_final)))
        (else (colocar_valor_aux (cdr lista) columna (+ contador 1) valor (cons (car lista) lista_final)))))
    
