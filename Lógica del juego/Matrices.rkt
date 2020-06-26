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



;matriz cuadrada para pruebas
(define matriz1 '(((1 1 1) (1 2 2) (1 3 3) (1 4 4))
                  ((2 1 5) (2 2 6) (2 3 7) (2 4 8))
                  ((3 1 9) (3 2 10) (3 3 11) (3 4 12))
                  ((4 1 13) (4 2 14) (4 3 15) (4 4 16))))

;*************************************************************************************************************************
#| Función que recibe el número de filas y columnas de una matriz y devuelve un #t si cumple con
las dimensiones adecuadas, y un #f en caso contrario.
|#
(define (dimensiones_correctas columnas filas)
  (and (>= filas 3) (<= filas 10) (>= columnas 3) (<= columnas 10)))


;*************************************************************************************************************************
;Función que recibe una matriz y devuelve el numero de filas de la matriz
(define (numero_filas matriz)
  (length matriz))


;*************************************************************************************************************************
;Función que recibe una matriz y devuelve el numero de columnas de la matriz
(define (numero_columnas matriz)
  (length (car matriz)))


;************************************************************************************************************************
#| Función que determina si una matriz es cuadrada. En caso de que lo sea retorna #true
En caso contrario retorna #false.
|#
(define (matriz_cuadrada matriz)
  (cond ((equal? (length matriz) (length (car matriz))) #true)
        (else #false)))


;*************************************************************************************************************************
#| Función que recibe el numero de filas y columnas deseadas. Verifica que las dimensiones sean correctas
y si cumplen con las reglas llama a una función auxilar para crear la matriz mxn de formato (valor)
Entrada: El numero de columnas n y el número de filas m
Salida: Una matriz vacía (con 0´s) con m filas y n columnas
|#
(define (crear_matriz filas columnas)
  (cond ((not (dimensiones_correctas columnas filas)) "Las dimensiones dadas no son correctas")
        (else (crear_matriz_aux filas columnas columnas '() '()))))

;*************************************************************************************************************************
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


;*************************************************************************************************************************
;Función que crea una matriz mxn exacta a partir de una matriz mxn normal, pero con formato (fila columna valor)
;Le llamamos matriz exacta porque en cada posición guarda el número de fila, de columna y el valor.
(define (crear_matriz_exacta matriz)
  (exacta_aux matriz 1 1 '() '()))

(define (exacta_aux matriz fila columna lista matriz_exacta)
  (cond ((null? matriz) (invertir_lista matriz_exacta))
        ((null? (car matriz)) (exacta_aux (cdr matriz) (+ fila 1) 1 '() (cons (invertir_lista lista) matriz_exacta)))
        (else (exacta_aux (cons (cdar matriz)
                                (cdr matriz)) fila (+ columna 1) (cons (list fila columna
                                                                             (caar matriz)) lista) matriz_exacta))))


;************************************************************************************************************************
; Función que recibe una lista e invierte el orden de sus elementos
(define(invertir_lista lista)
  (cond ((null? lista) '())
        (else (append (invertir_lista (cdr lista)) (list (car lista))))))


;************************************************************************************************************************
;Función que recibe una matriz y una posicion. Se debe retornar el elemento que se encuentra en dicha posición
(define (dame_valor matriz posicion)
  (obtener_valor matriz posicion 1))

(define (obtener_valor matriz posicion contador_fila)
  (cond ((equal? contador_fila (car posicion)) (obtener_valor_aux (car matriz) (cadr posicion) 1))
        (else (obtener_valor (cdr matriz) posicion (+ contador_fila 1)))))

(define (obtener_valor_aux fila columna contador)
  (cond ((equal? contador columna)(car fila))
        (else (obtener_valor_aux (cdr fila) columna (+ contador 1)))))


;***************************************************************************************************************************
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
        ((equal? contador fila)(colocar_valor fila columna
                                              (+ contador 1) valor (cdr matriz)
                                              (cons (colocar_valor_aux (car matriz) columna 1 valor '()) matriz_final)))
        
        (else (colocar_valor fila columna (+ contador 1) valor (cdr matriz) (cons (car matriz) matriz_final)))))
                               
(define (colocar_valor_aux lista columna contador valor lista_final)
  (cond ((null? lista)(invertir_lista lista_final))
        ((equal? contador columna)(colocar_valor_aux (cdr lista) columna (+ contador 1) valor (cons valor lista_final)))
        (else (colocar_valor_aux (cdr lista) columna (+ contador 1) valor (cons (car lista) lista_final)))))


;************************************************************************************************************************
#| Funcion que retorna todas las columnas de una matriz por medio de una lista
Usando las funciones sacar_columna y borrar_columna
|#
(define (dame_columnas matriz)
  (dame_columnas_aux matriz '()))

(define (dame_columnas_aux matriz lista)
  (cond ((null? matriz) (invertir_lista lista))
        (else (dame_columnas_aux (borrar_columna matriz) (cons (sacar_columna matriz) lista)))))

(define (sacar_columna matriz)
  (cond ((null? matriz) '())
        (else (cons (car (car matriz)) (sacar_columna (cdr matriz))))))

(define (borrar_columna matriz)
  (cond ((null? matriz) '())
        ((null? (cdr (car matriz))) '())
        (else (cons (cdr (car matriz)) (borrar_columna (cdr matriz))))))



; ***********************************************************************************************************************
; Funcion que retorna todas las diagonales (/) de una matriz por medio de una lista
(define (dame_diagonales matriz)
  (dame_diagonales_aux 1 1 (numero_columnas matriz) (dame_columnas matriz) '()))

(define (dame_diagonales_aux pivote_inicial pivote_final num_columnas columnas diagonales)
        ;validando si solo me queda el último en la lista de columnas
  (cond ((equal? (length columnas) 1) (invertir_lista diagonales))

        ;validando si la primera columna de la lista está vacía --> '()
        ((null? (car columnas)) (dame_diagonales_aux pivote_inicial pivote_final num_columnas (cdr columnas) diagonales))

        ;validando si el pivote se encuentra en el inicio de la matriz, se debe eliminar el primer elemento
        ((equal? pivote_inicial pivote_final) (dame_diagonales_aux pivote_inicial (+ pivote_final 1) num_columnas
                                                              (cons (cdar columnas) (cdr columnas)) diagonales))
        
        ;validando si los pivotes son diferentes y además el pivote final no ha llegado a la última columna
        ((and (not(equal? pivote_inicial pivote_final))
              (not (equal? pivote_final num_columnas))) (dame_diagonales_aux pivote_inicial
                                                                             (+ pivote_final 1) num_columnas
                                                                             (eliminar_primeros
                                                                              pivote_final 'no columnas)
                                                                             (cons (sacar_primeros pivote_final
                                                                                                   'no columnas)
                                                                                   diagonales)))

        ;validando si los pivotes son diferentes y el pivote final ya llegó a la última columna
        ((and (not (equal? pivote_inicial pivote_final))
              (equal? pivote_final num_columnas)) (dame_diagonales_aux
                                                   pivote_inicial pivote_final
                                                   num_columnas (eliminar_primeros
                                                                 pivote_final 'limite
                                                                 columnas)
                                                   (cons (sacar_primeros pivote_final 'limite columnas) diagonales)))))
                                                                                           
                                                                                              
;Función que toma una lista de columnas y elimina los primeros elementos de cada columna
;dados los pivotes inicial y final.
;Devuelve una nueva lista de columnas sin sus primeros elementos.
(define (eliminar_primeros pivote_final condicion columnas)
  (cond ((equal? condicion 'limite) (ep1 columnas '()))
        (else (ep2 columnas 1 pivote_final '()))))

;eliminar cuando el pivote final ya ha llegado al límite de columnas
;Funciona a la perfección
(define (ep1 columnas lista_final)
  (cond ((null? columnas) (invertir_lista lista_final))
        (else (ep1 (cdr columnas) (cons (cdar columnas) lista_final)))))

;eliminar cuando el pivote final aún no ha llegado al límite de columnas
(define (ep2 columnas pivote_inicial pivote_final lista)
  (cond ((equal? pivote_inicial pivote_final) (append (invertir_lista (cons (cdar columnas) lista)) (cdr columnas)))
        (else (ep2 (cdr columnas) (+ pivote_inicial 1) pivote_final (cons (cdar columnas) lista)))))


;Función que toma una lista de columnas y extrae sus primeros elementos para entregarlos en una lista
;Se debe pasar como parámetro los pivotes inicial y final.
(define (sacar_primeros pivote_final condicion columnas)
  (cond ((equal? condicion 'limite) (sp1 columnas '()))
        (else (sp2 columnas 1 pivote_final '()))))

;extraer los primeros elementos de las columnas cuando el pivote final ya ha llegado al límite
(define (sp1 columnas lista_final)
  (cond ((null? columnas) (invertir_lista lista_final))
        (else (sp1 (cdr columnas) (cons (caar columnas) lista_final)))))

;extraer los primeros elementos de las columnas cuando el pivote final aún no ha llegado al límite.
(define (sp2 columnas pivote_inicial pivote_final lista)
  (cond ((equal? pivote_inicial pivote_final) (invertir_lista (cons (caar columnas) lista)))
        (else (sp2 (cdr columnas) (+ pivote_inicial 1) pivote_final (cons (caar columnas) lista)))))


;************************************************************************************************************************

;Función que transforma una matriz para poder sacarle sus diagonales inversas

(define (dame_diagonales_inversas matriz)
  (dame_diagonales_aux 1 1 (numero_filas matriz) (t_matriz matriz '()) '()))


;Función para transformar una matriz a columnas inversas para luego extraer las diagonales inversas
(define (t_matriz matriz columnas)
  (cond ((null? matriz) (invertir_lista columnas))
        (else (t_matriz (cdr matriz) (cons (invertir_lista (car matriz)) columnas)))))


;************************************************************************************************************************
;Función que recibe una lista de vectores solución (fila, columna, diagonal(/), diagonal inversa(\))
; y devuelve la lista, pero con los vectores que cumplan las siguientes condiciones:

; 1) Los vectores deben tener tamaño mayor o igual a 3
; 2) Los vectores deben tener un valor de 0(libre) o 2(ocupado por la máquina)

(define (filtro_vector vectores_solucion valor)
  (filtro_vector_aux vectores_solucion valor '()))

(define (filtro_vector_aux lista valor filtro)
  (cond ((null? lista) (invertir_lista filtro))

        ;validando si cumple con las 2 condiciones
        ((and (>= (length (car lista)) 3)
              (disponible? (car lista) valor)) (filtro_vector_aux (cdr lista)
                                                                   valor
                                                                   (cons (car lista) filtro)))
        
        ;En caso de no cumplir las condiciones, se omite el elemento                                                      
        (else (filtro_vector_aux (cdr lista) valor filtro))))

;Función que recibe UN solo vector y valida si sus posiciones no tienen valores del otro jugador
(define (disponible? vector valor)
  (cond ((null? vector) #t)
        ((or (equal? (caddar vector) 0)
             (equal? (caddar vector) valor)) (disponible? (cdr vector) valor))
        (else #f)))


;************************************************************************************************************************
;; Función que determina si algún elemento interseca en algún punto de la lista dada
;; Determina si el elemento dado se encuentra dentro de la lista
(define (intersecan? elemento lista)
  (cond ((null? lista) #f)
        ((equal? elemento (car lista)) #t)
        (else (intersecan? elemento (cdr lista)))))








