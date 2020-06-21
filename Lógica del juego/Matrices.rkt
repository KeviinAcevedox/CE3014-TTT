#lang racket

(provide (all-defined-out))
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

#| Función que recibe el número de filas y columnas de una matriz y devuelve un #t si cumple con
las dimensiones adecuadas, y un #f en caso contrario.
|#
(define (verificar_dimensiones columnas filas)
  (and (>= filas 3) (<= filas 10) (>= columnas 3) (<= columnas 10)))


;Función que recibe una matriz y devuelve el numero de filas de la matriz
(define (numero_filas matriz)
  (length matriz))

;Función que recibe una matriz y devuelve el numero de columnas de la matriz
(define (numero_columnas matriz)
  (length (car matriz)))

#| Función que determina si una matriz es cuadrada. En caso de que lo sea retorna #true
En caso contrario retorna #false.
|#
(define (matriz_cuadrada matriz)
  (cond ((equal? (length matriz) (length (car matriz))) #true)
        (else #false)))

#| Función que recibe el numero de filas y columnas deseadas. Verifica que las dimensiones sean correctas
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



; Función que recibe una lista e invierte el orden de sus elementos
(define(invertir_lista lista)
  (cond ((null? lista) '())
        (else (append (invertir_lista (cdr lista)) (list (car lista))))))



;Función que recibe una matriz y una posicion. Se debe retornar el elemento que se encuentra en dicha posición
(define (dame_valor matriz posicion)
  (obtener_valor matriz posicion 1))

(define (obtener_valor matriz posicion contador_fila)
  (cond ((equal? contador_fila (car posicion)) (obtener_valor_aux (car matriz) (cadr posicion) 1))
        (else (obtener_valor (cdr matriz) posicion (+ contador_fila 1)))))

(define (obtener_valor_aux fila columna contador)
  (cond ((equal? contador columna)(car fila))
        (else (obtener_valor_aux (cdr fila) columna (+ contador 1)))))

;Funcion que recibe una posicion (fila columna) y retorna una lista que contiene las posiciones que
;representan la diagonal (/) desde esa posicion
(define (posiciones_diagonal fila_inicial columna_inicial fila_objetivo columna_objetivo)
  (posiciones_diagonal_aux fila_inicial columna_inicial fila_objetivo columna_objetivo '()))
  
(define (posiciones_diagonal_aux fila_actual columna_actual fila_objetivo columna_objetivo lista)
  (cond ((and (equal? fila_actual fila_objetivo) (equal? columna_actual columna_objetivo)) (cons (list fila_actual columna_actual) lista))
        (else (posiciones_diagonal_aux (+ fila_actual 1) (- columna_actual 1) fila_objetivo columna_objetivo (cons (list fila_actual columna_actual) lista)))))

;Funcion que recibe una posicion (fila columna) y retorna una lista que contiene las posiciones que
;representan la diagonal inversa (\) desde esa posición.
;Para la inversa se toman las posiciones iniciales desde el inicio de la matriz.
(define (posiciones_diagonal_inversa fila_inicial columna_inicial fila_objetivo columna_objetivo)
  (invertir_lista (posiciones_diagonal_inversa_aux fila_inicial columna_inicial fila_objetivo columna_objetivo '())))

(define (posiciones_diagonal_inversa_aux fila_actual columna_actual fila_objetivo columna_objetivo lista)
  (cond ((and (equal? fila_actual fila_objetivo) (equal? columna_actual columna_objetivo)) (cons (list fila_actual columna_actual) lista))
        (else (posiciones_diagonal_inversa_aux (+ fila_actual 1) (+ columna_actual 1) fila_objetivo columna_objetivo (cons (list fila_actual columna_actual) lista)))))
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

#| Funcion que retorna todas las columnas de una matriz por medio de una lista
Usando las funciones sacar_columna y borrar_columna
|#
(define (dame_columnas matriz)
  (dame_columnas_aux matriz '()))
;Función auxilar, llama recursivamente a las funciones de sacar_columnas y borrar_columnas.
(define (dame_columnas_aux matriz lista)
  (cond ((null? matriz) (invertir_lista lista))
        (else (dame_columnas_aux (borrar_columna matriz) (cons (sacar_columna matriz) lista)))))
;Función para obtener el primer elemento de cada fila de una matriz
(define (sacar_columna matriz)
  (cond ((null? matriz) '())
        (else (cons (car (car matriz)) (sacar_columna (cdr matriz))))))
;Función para borrar el primer elemento de cada fila
(define (borrar_columna matriz)
  (cond ((null? matriz) '())
        ((null? (cdr (car matriz))) '())
        (else (cons (cdr (car matriz)) (borrar_columna (cdr matriz))))))

