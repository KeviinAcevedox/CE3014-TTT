#lang racket

;para poder expoertar todas las definiciones del módulo
(provide (all-defined-out))

#|
ESTE MÓDULO CONTIENE TODAS LAS OPERACIONES SOBRE MATRICES QUE SON NECESARIAS PARA EL FUNCIONAMIENTO DEL ALGORITMO
GOLOSO.
|#


;*************************************************************************************************************************************************************************************
#|
Función que detecta si las dimensiones de una matriz corresponden al tamaño correcto

Entrada: columnas y filas.
Salida: #t si cumple con las condiciones y un #f en el caso contrario.
|#
(define (dimensiones_correctas columnas filas)
  (and (>= filas 3) (<= filas 10) (>= columnas 3) (<= columnas 10)))


;***********************************************************************************************************************************************************************************
#|
Función que indica el número de filas de una matriz

Entrada: una matriz mxn
Salida: el número de filas de la matriz
|#
(define (numero_filas matriz)
  (length matriz))


;********************************************************************************************************************************************************************************+
#|
Función que indica el número de columnas de una matriz mxn

Entrada: una matriz mxn
Salida: el número de columnas de dicha matriz
|#
(define (numero_columnas matriz)
  (length (car matriz)))


;*********************************************************************************************************************************************************************************
#|
Función que construye una matriz vacía (con 0´s)

Entrada: El numero de columnas y el número de filas que debe tener la matriz
Salida: Una matriz vacía (con 0´s) con el tamaño indicado por la entrada.
|#
(define (crear_matriz filas columnas)
  (cond ((not (dimensiones_correctas columnas filas)) "Las dimensiones dadas no son correctas")
        (else (crear_matriz_aux filas columnas columnas '() '()))))


;Función auxiliar para crear la matriz
(define (crear_matriz_aux filas columnas contador_columnas lista matriz_final)
  (cond ((zero? filas) matriz_final)
        ((zero? contador_columnas) (crear_matriz_aux (- filas 1) columnas columnas '() (cons lista matriz_final)))
        (else (crear_matriz_aux filas columnas (- contador_columnas 1) (cons 0 lista) matriz_final))))


;************************************************************************************************************************************************************************************
#|
Función que construye una matriz a partir de otra, son la misma matriz, sin embargo la matriz exacta no solamente va a
representar el valor que hay en cada posición, si no que también indica el número de fila y columna correspondiente a
cada elemento que la conforma.

Entrada: una matriz normal de tamaño mxn
Salida: su versión exacta (con numero de filas y columnas)
|#
(define (crear_matriz_exacta matriz)
  (exacta_aux matriz 1 1 '() '()))


;Función auxiliar para crear la matriz exacta
(define (exacta_aux matriz fila columna lista matriz_exacta)
  (cond ((null? matriz) (invertir_lista matriz_exacta))
        ((null? (car matriz)) (exacta_aux (cdr matriz) (+ fila 1) 1 '() (cons (invertir_lista lista) matriz_exacta)))
        (else (exacta_aux (cons (cdar matriz)
                                (cdr matriz)) fila (+ columna 1) (cons (list fila columna
                                                                             (caar matriz)) lista) matriz_exacta))))


;******************************************************************************************************************************************************************************************************************
#|
Función que se encarga de invertir una lista

Entrada: una lista cualquiera
Salida: la lista invertida
|#
(define(invertir_lista lista)
  (cond ((null? lista) '())
        (else (append (invertir_lista (cdr lista)) (list (car lista))))))


;*******************************************************************************************************************************************************************************************************************
#|
Función que retorna el valor de un elemento de cualquier matriz mxn

Entrada: la matriz y una posición --> (fila columna)
Salida: el elemento que se encuentra en la posición brindada
|#
(define (dame_valor matriz posicion)
  (obtener_valor matriz posicion 1))

;Forma parte de las funciones auxiliares que se encargan de realizar operaciones
;para poder encontrar y devolver el elemento
(define (obtener_valor matriz posicion contador_fila)
  (cond ((equal? contador_fila (car posicion)) (obtener_valor_aux (car matriz) (cadr posicion) 1))
        (else (obtener_valor (cdr matriz) posicion (+ contador_fila 1)))))

;Forma parte de las funciones auxiliares que se encargan de realizar operaciones
;para poder encontrar y devolver el elemento
(define (obtener_valor_aux fila columna contador)
  (cond ((equal? contador columna)(car fila))
        (else (obtener_valor_aux (cdr fila) columna (+ contador 1)))))


;************************************************************************************************************************************************************************************************************************
#|
Función que modifica el valor de un elemento de la matriz

Entrada: una posición (fila columna), el nuevo valor y la matriz que se desea modificar
Salida: la matriz modificada
|#
(define (modificar_matriz posicion valor matriz)
  (colocar_valor (car posicion) (cadr posicion) 1 valor matriz '()))

#|
Función auxiliar, esta se encarga de recorrer la matriz por filas, llevando el conteo de las filas desde la fila
1 hasta que llegue a la fila indicada en la posición. si el contador es diferente al de la posición significa que
aún no se ha llegado a la fila deseada, así que se continúa con la siguiente fila hasta que ambos, el contador y la
fila indicada sean iguales. Cuando sean iguales se toma la fila y se envía a la siguiente función auxiliar
|#
(define (colocar_valor fila columna contador valor matriz matriz_final)
  (cond ((null? matriz)(invertir_lista matriz_final))
        ((equal? contador fila)(colocar_valor fila columna
                                              (+ contador 1) valor (cdr matriz)
                                              (cons (colocar_valor_aux (car matriz) columna 1 valor '()) matriz_final)))
        
        (else (colocar_valor fila columna (+ contador 1) valor (cdr matriz) (cons (car matriz) matriz_final)))))

#|
Segunda función auxiliar busca modifacar una fila con el nuevo valor deseado
esta debe recibir la fila que coincide con la de la posición indicada por parámetro.
Con la ayuda de un contador que inicia en 1, se aumenta hasta que el contador de columnas coincida con el que se desea
llegar (este se indica en la posición). Si el contador y el numero de columna deseada son diferentes, significa que
aún no he llegado a la columna, entonces sigo aumentando el contador hasta que sean iguales. Cuando sean iguales
se pega el nuevo valor en la nueva fila y se retorna para pegar esta fila en la matriz.
|#
(define (colocar_valor_aux lista columna contador valor lista_final)
  (cond ((null? lista)(invertir_lista lista_final))
        ((equal? contador columna)(colocar_valor_aux (cdr lista) columna (+ contador 1) valor (cons valor lista_final)))
        (else (colocar_valor_aux (cdr lista) columna (+ contador 1) valor (cons (car lista) lista_final)))))


;************************************************************************************************************************************************************************************************************************
#|
Función que retorna una lista con todas las columnas que forman parte de una matriz mxn

Entrada: una matriz mxn
Salida: una lista con todas las columnas de la matriz
|#
(define (dame_columnas matriz)
  (dame_columnas_aux matriz '()))

#|
Función auxiliar, su rol es recorrer la matriz desde el primer elemento hasta que la matriz esté vacía
y por cada recursión va guardando listas(las columnas) en una lista final.
El algoritmo consiste en que cada turno se saca la primera columna para guardarla en la lista total
y luego la elimina. Este proceso continúa hasta que la matriz esté vacía.
|#
(define (dame_columnas_aux matriz lista)
  (cond ((null? matriz) (invertir_lista lista))
        (else (dame_columnas_aux (borrar_columna matriz) (cons (sacar_columna matriz) lista)))))

;Esta función recibe la matriz y extrae la primera columna
(define (sacar_columna matriz)
  (cond ((null? matriz) '())
        (else (cons (car (car matriz)) (sacar_columna (cdr matriz))))))

;Esta función recibe la matriz y elimina la primer columna
(define (borrar_columna matriz)
  (cond ((null? matriz) '())
        ((null? (cdr (car matriz))) '())
        (else (cons (cdr (car matriz)) (borrar_columna (cdr matriz))))))



; ********************************************************************************************************************************************************************************************************************
#|
Esta función se encarga de formar una lista con todas las diagonales de la matris , entiendase diagonales a ( / )

Entrada: una matriz mxn
Salida: una lista con todas las diagonales (/) de la matriz
|#
(define (dame_diagonales matriz)
  (dame_diagonales_aux 1 1 (numero_columnas matriz) (dame_columnas matriz) '()))

#|
Esta es la función auxiliar principal para obtener las diagonales de la matriz.

El proceso requiere el uso de dos pivotes, una lista con todas las columnas de la matriz y además
una lista en donde se puedan guardar cada una de las diagonales. Los pivotes se inicializan en la posición
(1 1).
El primer pivote representa el inicio de las líneas diagonales y el segundo pivote representa el final de las líneas.

Este algoritmo consta de cuatro condiciones o casos para ir sacando las diagonales de la matriz.

1- AMBOS PIVOTES SON IGUALES: Si esto se cumple quiere decir que se encuentran en la posición (1 1).
En esa posición no se puede formar una diagonal, así que lo que se hace es ELIMINAR el primer elemento de la primera
columna.
Una vez eliminado, el primer pivote se mantiene en 1 (porque aún hay elementos en la primera columna) y el segundo pivote
se aumenta 1 posición.

2- LOS PIVOTES SON DIFERENTES Y EL SEGUNDO PIVOTE NO ES IGUAL AL NÚMERO DE COLUMNAS DE LA MATRIZ:
Si se visualizan los pivotes en la matriz, se puede ver que desde el primer pivote hasta el segundo se forma una línea en
diagonal.Cuando se cumple la condición se procede a sacar la línea que se forma por medio de una función y después se
elimina dicha línea.

3- LA PRIMERA COLUMNA DE LA MATRIZ ESTÁ VACÍA: Cuando se cumple esta condición, lo que se hace es eliminar la primera
columna por completo y se aumenta en 1 el primer pivote, para empezar a sacar líneas diagonales desde la siguiente
columna, el segundo pivote se mantiene igual.

3- LOS PIVOTES SON DIFERENTES Y EL SEGUNDO PIVOTE ES IGUAL AL NÚMERO DE COLUMNAS DE LA MATRIZ:
Si esta condición se cumple, significa que el segundo pivote ya se encuentra en la cima de la última columna.
Si se visualizaran ambos pivotes se podría notar que cuando esta condición se cumple el primer pivote se encuentra
en el único elemento de la primera columna y que el segundo pivote se encuentra en el primer elemento de la última
columna, y que además se forma una línea diagonal desde el primer pivote hasa el último.
Lo que se procede es a sacar la línea y guardarla en la lista de diagonales y luego se elimina la línea.

4- LA LISTA DE COLUMNAS SOLO TIENE UN ELEMENTO: Esto significa que ya no hay líneas diagonales, así que esta es
la condición de terminación.
|#
(define (dame_diagonales_aux pivote_inicial pivote_final num_columnas columnas diagonales)
  
        ;validando si solo me queda el último en la lista de columnas
  (cond ((equal? (length columnas) 1) (invertir_lista diagonales))

        ;validando si la primera columna de la lista está vacía --> '()
        ((null? (car columnas)) (dame_diagonales_aux (+ pivote_inicial 1) pivote_final num_columnas (cdr columnas) diagonales))

        ;validando si el pivote se encuentra en el inicio de la matriz, se debe eliminar el primer elemento
        ((equal? pivote_inicial pivote_final) (dame_diagonales_aux pivote_inicial (+ pivote_final 1) num_columnas
                                                              (cons (cdar columnas) (cdr columnas)) diagonales))
        
        ;validando si los pivotes son diferentes y además el pivote final no ha llegado a la última columna
        ((and (not(equal? pivote_inicial pivote_final))
              (not (equal? pivote_final num_columnas))) (dame_diagonales_aux pivote_inicial
                                                                             (+ pivote_final 1) num_columnas
                                                                             (eliminar_primeros
                                                                              pivote_inicial
                                                                              pivote_final 'no columnas)
                                                                             (cons (sacar_primeros pivote_inicial
                                                                                                   pivote_final
                                                                                                   'no
                                                                                                   columnas)
                                                                                   diagonales)))

        ;validando si los pivotes son diferentes y el pivote final ya llegó a la última columna
        ((and (not (equal? pivote_inicial pivote_final))
              (equal? pivote_final num_columnas)) (dame_diagonales_aux
                                                   pivote_inicial pivote_final
                                                   num_columnas (eliminar_primeros
                                                                 pivote_inicial
                                                                 pivote_final 'limite
                                                                 columnas)
                                                   (cons (sacar_primeros pivote_inicial pivote_final 'limite columnas) diagonales)))))
                                                                                           
  
;Función auxiliar que elimina líneas diagonales de una matriz, se le debe indicar el primer pivote y también el segundo
;El primer pivote señala el inicio de la diagonal y el segundo pivote señala el final
;Si se cumple la condición (3) se debe indicar a la función con el valor de condición 'limite
;Si se cumple la condición (2) se debe indicar a la función con el valor de condición 'no
(define (eliminar_primeros pivote_inicial pivote_final condicion columnas)
  (cond ((equal? condicion 'limite) (eliminar1 columnas '()))
        (else (eliminar2 columnas pivote_inicial pivote_final '()))))

#|
Esta función auxiliar de eliminar, elimina líneas diagonales de la matriz cuando los pivotes son diferentes y el segundo
pivote ya llegó a la última columna. El proceso de elminar solo requiere ir aumentando el primer pivote, cada valor que
toma el pivote equivale al número de columna en la que se encuentra, y en cada columna elimina el primer elemento hasta
que llegue al segundo pivote. Al final entrega la lista de las columnas pero sin la diagonal superior.
|#
(define (eliminar1 columnas lista_final)
  (cond ((null? columnas) (invertir_lista lista_final))
        (else (eliminar1 (cdr columnas) (cons (cdar columnas) lista_final)))))

#|
Esta función auxiliar de eliminar, elimina líneas diagonales de la matriz cuando se cumple que los pivotes son diferentes
y el segundo pivote aún no ha llegado a la última columna.
El proceso consiste en ir aumentando el primer pivote (este pivote llevará el valor de la columna en la que esté) hasta
que sea igual al segundo pivote, cada valor que tome el primer pivote representa la columna en la que se encuenntra, así
que se elimina el primer elemento de cada fila hasta llegar al segundo pivote.
Al final entrega la lista de las columnas pero sin la diagonal que demarcaban los pivotes.
|#
(define (eliminar2 columnas pivote_inicial pivote_final lista)
  (cond ((equal? pivote_inicial pivote_final) (append (invertir_lista (cons (cdar columnas) lista)) (cdr columnas)))
        (else (eliminar2 (cdr columnas) (+ pivote_inicial 1) pivote_final (cons (cdar columnas) lista)))))




;Función auxiliar que extrae líneas diagonales de una matriz, se le debe indicar el primer pivote y también el segundo
;El primer pivote señala el inicio de la diagonal y el segundo pivote señala el final
;Si se cumple la condición (3) se debe indicar a la función con el valor de condición 'limite
;Si se cumple la condición (2) se debe indicar a la función con el valor de condición 'no
(define (sacar_primeros pivote_inicial pivote_final condicion columnas)
  (cond ((equal? condicion 'limite) (sacar1 columnas '()))
        (else (sacar2 columnas pivote_inicial pivote_final '()))))

#|
Esta función auxiliar de sacar, extrae líneas diagonales de la matriz cuando los pivotes son diferentes y el segundo
pivote ya llegó a la última columna. El proceso de extraer solo requiere ir aumentando el primer pivote, cada valor que
toma el pivote equivale al número de columna en la que se encuentra, y en cada columna extrae el primer elemento hasta
que llegue al segundo pivote. Al final entrega una lista con la diagonal completa.
|#
(define (sacar1 columnas lista_final)
  (cond ((null? columnas) (invertir_lista lista_final))
        (else (sacar1 (cdr columnas) (cons (caar columnas) lista_final)))))

#|
Esta función auxiliar de sacar, extrae líneas diagonales de la matriz cuando se cumple que los pivotes son diferentes
y el segundo pivote aún no ha llegado a la última columna.
El proceso consiste en ir aumentando el primer pivote (este pivote llevará el valor de la columna en la que esté) hasta
que sea igual al segundo pivote, cada valor que tome el primer pivote representa la columna en la que se encuenntra, así
que se extrae el primer elemento de cada fila hasta llegar al segundo pivote.
Al final entrega una lista con la diagonal que demarcaban los pivotes.
|#
(define (sacar2 columnas pivote_inicial pivote_final lista)
  (cond ((equal? pivote_inicial pivote_final) (invertir_lista (cons (caar columnas) lista)))
        (else (sacar2 (cdr columnas) (+ pivote_inicial 1) pivote_final (cons (caar columnas) lista)))))





;************************************************************************************************************************************************************************************************************************+
#|
Esta función se encarga de sacar todas las diagonales inversas de la matriz, entiendase a diagonales inversas a ( \ )
Para hacerlo, se debe acomodar la matriz de tal modo que las diagonales inversas se vean como las diagonales normales
(/). Una vez acomodada la matriz de esa forma, se usa el algoritmo de diagonales y se pueden obtener las diagonales
inversas como si fueran diagonales normales.

Entrada: una matriz mxn
Salida: una lista con las diagonales inversas de la matriz
|#
(define (dame_diagonales_inversas matriz)
  (dame_diagonales_aux 1 1 (numero_filas matriz) (t_matriz matriz '()) '()))

#|
Esta es la función auxiliar, lo que hace es acomodar la matriz para que se puedan extraer las diagonales inversas usando
el algoritmo de diagonales normales. Lo que se hace es tomar cada una de las filas y darles vuelta a sus elementos.
De esa manera se va a obtener una lista donde cada elemento equivale a las columnas de la matriz que se usan en el
algoritmo.
|#
(define (t_matriz matriz columnas)
  (cond ((null? matriz) (invertir_lista columnas))
        (else (t_matriz (cdr matriz) (cons (invertir_lista (car matriz)) columnas)))))


;*******************************************************************************************************************************************************************************************************************
#|
Función que selecciona solamente aquellos que cumplan con las siguientes condiciones:

1) Los vectores deben tener tamaño mayor o igual a 3
2) Los vectores deben tener un valor de 0(libre) o 2(ocupado por la máquina)

Entrada: una lista de vectores
Salida: la misma lista pero conteniendo solamente aquellos que cumplan las condiciones
|#
(define (filtro_vector vectores_solucion valor)
  (filtro_vector_aux vectores_solucion valor '()))

#|
Función auxiliar de filtro, recibe la lista de vectores y un valor de filtro (este valor puede ser 1 o 2)
dependiendo del jugador (1 si son los vectores del usuario y 2 si son los vectores del algoritmo)
|#
(define (filtro_vector_aux lista valor filtro)
  (cond ((null? lista) (invertir_lista filtro))

        ;validando si cumple con las 2 condiciones
        ((and (>= (length (car lista)) 3)
              (disponible? (car lista) valor)) (filtro_vector_aux (cdr lista)
                                                                   valor
                                                                   (cons (car lista) filtro)))
        
        ;En caso de no cumplir las condiciones, se omite el elemento                                                      
        (else (filtro_vector_aux (cdr lista) valor filtro))))

#|
Esta es una de las funciones auxiliares del filtro, recibe un vector y un valor (1 o 2) y revisa si
en el vector existen solamente elementos con 0´s o con el valor.
|#
(define (disponible? vector valor)
  (cond ((null? vector) #t)
        ((or (equal? (caddar vector) 0)
             (equal? (caddar vector) valor)) (disponible? (cdr vector) valor))
        (else #f)))


;**********************************************************************************************************************************************************************************************************************
#|
Esta función me dice si un elemento se encuentra dentro de una lista

Entrada: el elemento y la lista
Salida: #t si se encuentra dentro de la lista y #f en el caso contrario.
|#
(define (intersecan? elemento lista)
  (cond ((null? lista) #f)
        ((equal? elemento (car lista)) #t)
        (else (intersecan? elemento (cdr lista)))))

;**********************************************************************************************************************************************************************************************************************
#|
Esta función se encarga de ordenar una lista de elementos en orden descendente

Entrada: una lista
Salida: la lista ordenada en orden descendente
|#
(define (Quick_Sort lista)
  (cond ((null? lista) lista)
        (else (append (Quick_Sort (car (particion lista))) (list (car lista)) (Quick_Sort (cadr (particion lista)))))))

;Función auxliar del algoritmo, hace una partición de la lista
(define (particion lista)
  (cond ((null? lista) #f)
        (else (particion_aux (car lista) (cdr lista) '() '() ))))

;Segunda función auxiliar                 
(define (particion_aux pivote lista menores mayores)
  (cond ((null? lista) (list menores mayores))
        ((>= (caddar lista) (caddr pivote)) (particion_aux pivote (cdr lista) (cons (car lista) menores) mayores))
        (else (particion_aux pivote (cdr lista) menores (cons (car lista) mayores)))))
