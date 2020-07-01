#lang racket

;importamos las definiciones del módulo Operaciones
(require "Operaciones.rkt")

;exportamos todas las definiciones de este módulo
(provide (all-defined-out))

;reexportamos las definiciones del módulo Operaciones
(provide (all-from-out "Operaciones.rkt"))

#|
EN ESTE MÓDULO SE IMPLEMENTAN FUNCIONES QUE AL RELACIONARLAS SE TIENE UN ALGORITMO GOLOSO QUE ELIJE LA MEJOR POSICIÓN
QUE SE PUEDA ELEGIR EN CADA TURNO DEL JUEGO TIC TAC TOE.

ESTE ALGORITMO SOLO NECESITA RECIBIR UNA MATRIZ NORMAL:

((1 0 0)
 (1 2 0)
 (0 0 0))

Y LUEGO DE PASAR POR TODOS LOS PROCESOS VA A RETORNAR UNA LISTA QUE CONTIENE 3 ELEMENTOS:

(fila columna resultado)

Los primeros 2 elementos equivalen a la fila y la columna del mejor candidato elegido
El ultimo elemento resultado simboliza el estado de la partida en el momento:

* un 0 significa que la partida aún no acaba
* un 1 significa que el usuario ha ganado
* un 2 significa que el algoritmo ha ganado
* un 3 significa que hay un empate

El algoritmo goloso consta de 4 secciones ---> (selección, viabilidad, objetivo y solución)

Cada sección va a contener un conjunto de funciones que hacen posible el funcionamiento del algoritmo en cada sección.

|#


;;*************************************************************************************************************************
; SECCIÓN 1: CONJUNTO DE CANDIDATOS Y SELECCIÓN
;**************************************************************************************************************************

#|
Esta es una fusión entre el conjunto de candidatos y la selección.
El objetivo de esta función es insertar en una lista aquellos elementos que estén formados por un 0
eso quiere decir que los elementos están disponibles para ser elegidos.

Los elementos que conformarán la lista serán de la forma ---> (fila columna 0)
donde el 0 equivale al valor de plus (peso de viabilidad, que es 0 porque aún no se ha evaluado).

Entrada: la matriz del turno actual
Salida: una lista con los elementos de la matriz que están disponibles
|#
(define (seleccionar_candidatos matriz)
  (seleccionar_candidatos_aux matriz 1 1 '()))


#|
Elige solo a los elementos que tienen un 0 y los guarda en una lista con su fila y columna respectiva.
|#
(define (seleccionar_candidatos_aux matriz fila columna candidatos)
  (cond ((null? matriz) (invertir_lista candidatos))
        ((null? (car matriz)) (seleccionar_candidatos_aux (cdr matriz) (+ fila 1) 1 candidatos))
        ((zero? (caar matriz)) (seleccionar_candidatos_aux
                                (cons (cdar matriz)
                                      (cdr matriz)) fila (+ columna 1) (cons (list fila columna 0) candidatos)))
        
        (else (seleccionar_candidatos_aux (cons (cdar matriz) (cdr matriz)) fila (+ columna 1) candidatos))))




;**************************************************************************************************************************
;  SECCIÓN 2 : VIABILIDAD 
;**************************************************************************************************************************


#|
Esta función forma una lista con todas las posibles soluciones (líneas con tamaño igual o mayor a 3) que se pueden
obtener de una matriz (verticales, horizontales, diagonales y diagonales inversas). Se debe tomar en cuenta el filtro
de los vectores ya que hay líneas que son de tamaño correcto pero tienen uno o varios elementos con un valor del otro
jugador, esto quiere decir que esa línea no es una solución parcial.

Entrada: una matrix mxn exacta (o sea que los elementos sean de tipo (fila columna plus)) y el valor del
jugador (1 si se desean todas las posibles soluciones del usuario y un 2 si se desean todas las posibles soluciones
del algoritmo)
Salida: una lista con todos los vectores soluciones posibles para el valor brindado
|#

(define (vectores_soluciones matriz valor)
  (agregar_diagonal_inversa (crear_matriz_exacta matriz) '() valor))

;Se agregan las diagonales inversas (\)
(define (agregar_diagonal_inversa matriz_exacta vectores_total valor)
   (agregar_diagonal matriz_exacta (append (dame_diagonales_inversas matriz_exacta)
                                          vectores_total) valor))

;Se agregan las diagonales (/)
(define (agregar_diagonal matriz_exacta vectores_total valor)
  (agregar_columna matriz_exacta (append (dame_diagonales matriz_exacta)
                                          vectores_total) valor))

;Se agregan las columnas(|)
(define (agregar_columna matriz_exacta vectores_total valor)
  (agregar_fila matriz_exacta (append (dame_columnas matriz_exacta)
                                          vectores_total) valor))

;Se agregan las filas (--) y se pasan todos los vectores por el filtro
(define (agregar_fila matriz_exacta vectores_total valor)
  (filtro_vector (append matriz_exacta vectores_total) valor))

;***********************************************************************************************************************************************************************************************************************
#|
Esta función se va a encargar de entrevistar/evaluar a cada uno de los candidatos y les va a dar un valor de
viabilidad -->(plus) , entre más alto sea el plus, más viable es para ser elegido como la mejor posición

Los criterios de evaluación son los siguientes:

+ 1 si un candidato iterseca (es miembro) con un vector solución (fila, columna, diagonal o diagonal inversa)

+ n si un candidato interseca con un vector solución y ese vector contiene n posiciones ocupadas por un 2

+ 1000 si un candidato interseca con un vector solución y a ese vector solo le falta una posición para formar la
línea completa.

Entrada: una lista con todos los candidatos seleccionados y la lista de todos los vectores solución
Salida: una lista con todos los candidatos pero con su Plus evaluado, o sea con un valor de viabilidad
|#
(define (entrevista candidatos vectores_solucion)
  (entrevista_aux candidatos vectores_solucion '()))


#|
Esta es la función auxilizar de entrevista, su trabajo es tomar a todos y cada uno de los candidatos seleccionados y
enviarlos a la función Plus junto con la lista de vectores soluciones para poder darles un valor de viabilidad.
|#
(define (entrevista_aux candidatos vectores_solucion candidatos_potenciales)
  (cond ((null? candidatos) (invertir_lista candidatos_potenciales))
        (else (entrevista_aux (cdr candidatos)
                              vectores_solucion
                              (cons (Plus (car candidatos) vectores_solucion 0)
                                    candidatos_potenciales)))))

#|
Esta función se encarga de recibir un candidato junto con una lista de vectores soluciones. Toma en cuenta los criterios
de evaluación citados anteriormente y le da un valor a cada candidato que reciba.
|#
(define (Plus candidato vectores_solucion suma)
  (cond ((null? vectores_solucion) (list (car candidato)
                                         (cadr candidato)
                                         suma))
        
        ;El candidato es miembro del vector y a este vector solo le falta un elemento para completar una línea
        ((and (intersecan? candidato (car vectores_solucion))
              (killer? candidato (car vectores_solucion) 2)) (list (car candidato)
                                                                (cadr candidato)
                                                                (+ suma 1000)))
        
        ;El candidato es miembro del vector y este es un vector parcial
        ((intersecan? candidato (car vectores_solucion)) (Plus candidato
                                                               (cdr vectores_solucion)
                                                               (+ suma 1 (Boost (car vectores_solucion) 2 0))))
        ;No es miembro del vector
        (else (Plus candidato (cdr vectores_solucion) suma))))

;*********************************************************************************************************************************************************************************************************************
#|
Esta función se usa cuando un vector tiene varios elementos con  solamente 2's y 0's o cuando tiene varios elementos con
solamente 1's 0's. Se encarga de hacer un conteo de los 2's o 1's que tiene el vector

Entrada: un vector y un número que siempre es 0 (la idea es sumarle 1 por cada coincidencia que encuentre)
Salida: la cantidad de coincidencias que hay en el vector con respecto al valor dado
|#
(define (Boost vector valor suma)
  (cond ((null? vector) suma)
        ((equal? (caddar vector) valor) (Boost (cdr vector)
                                              valor
                                              (+ suma 1)))

        (else (Boost (cdr vector) valor suma))))
;*****************************************************************************************************************************************************************************************************************************                                       
#|
Esta función se encarga de verificar si un candidato es el último elemento que le falta a un vector para formar una
línea completa en la matriz

Entrada: un candidato, un vector o lista y el valor del jugador (1 si quiero verificar el killer del usuario
y 2 si quiero verificar el killer del algoritmo)

Salida: #t si efectivamente es un killer y #f en caso contrario
|#
(define (killer? candidato vector valor)
  (cond ((and (equal? (Boost vector valor 0) (- (length vector) 1))
              (intersecan? candidato vector)) #t)
        (else #f)))


;*************************************************************************************************************************************************************************************************************************
#|
Esta función verifica si el enemmigo (entiendase el usuario que está jugando) tiene un killer en la matriz
Es importante saber si el enemigo cuenta con esta posibilidad ya muy posiblemente se deba tapar esa posición

Entrada: la lista de todos los candidatos y los vectores soluciones del usuario
Salida: un #t si el usuario cuenta con un killer y #f en caso contrario
|#
(define (killer_enemigo? candidatos vectores_soluciones_enemigo)
  (cond ((null? candidatos) #f)
        ((killer_vectores? (list (caar candidatos) (cadar candidatos) 0) vectores_soluciones_enemigo 1) #t)
        (else (killer_enemigo? (cdr candidatos) vectores_soluciones_enemigo))))

;Función auxiliar de killer_enemigo, verifica la posibilidad de killers pero para un candidato
(define (killer_vectores? candidato vectores valor)
  (cond ((null? vectores) #f)
        ((killer? candidato (car vectores) valor) #t)
        (else (killer_vectores? candidato (cdr vectores) valor))))


;****************************************************************************************************************************************************************************************************************************
#|
Esta función se encarga de retornar el la posición de un posible killer del usuario

Entrada: la lista de candidatos evaluados y la lista de los vectores soluciones del usuario
Salida: La posición del elemento killer del enemigo.
|#
(define (dame_killer_enemigo candidatos vectores_soluciones_enemigo)
  (cond ((null? candidatos) #f)
        
         ((killer_vectores? (list (caar candidatos) (cadar candidatos) 0) vectores_soluciones_enemigo 1)        
          (dame_killer_enemigo_aux (list (caar candidatos) (cadar candidatos) 0) vectores_soluciones_enemigo
                                   1))
        (else (dame_killer_enemigo (cdr candidatos) vectores_soluciones_enemigo))))


;Función auxiliar de dame_killer_enemigo, busca al posible killer tomando solo a un candidato de toda la lista de
;candidatos
(define (dame_killer_enemigo_aux  candidato vectores valor)
  (cond ((null? vectores) '())
        ((killer? candidato (car vectores) valor) (list (car candidato) (cadr candidato)))
        (else (dame_killer_enemigo_aux  candidato (cdr vectores) valor))))


 
;**************************************************************************************************************************
; SECCIÓN 3 - OBJETIVO
;**************************************************************************************************************************


#|
Esta función se encarga de seleccionar al mejor candidato para formar una línea completa o parcial en la matriz

Primero se debe recordar que cuando se use esta función debe existir una lista con todos los candidatos con un vaor de
vialbilidad o Plus asociado.

Para tomar la desición del mejor candidato se basa en los siguientes criterios:

1- Existe un killer en la lista de candidatos: si un elemento de la lista de candidatos es un killer, significa que si
selecciono ese elemento estaría completando una línea en la matriz a mi favor. Entonces se selecciona ese elemento
inmediatamente.

2- No hay killer, pero hay un elemento que tiene el mayor Plus: Estas condiciones significan que el elemento que tiene
mayor Plus tiene muchas posibilidades de contribuir a una solución futura. Entonces se selecciona ese elemento.

3- No hay killer y hay más de un elemento que tiene el mayor Plus: Esto significa que existen varios elementos que
tienen las mismas posibilidades de contribuir a una solución parcial, por lo que no importa cual de ellos se elija, voy
a tener un resultado similar. Entonces se elije al candidato aleatoriamente mediante una función


Entrada: La lista de los candidatos entrevistados y ordenados en ordenados en orden descendente y la matriz del turno
Salida: La mejor posición para el turno
|#
(define (el_elegido candidatos matriz)
        ;Tengo killer?
  (cond ((>= (caddar candidatos) 1000) (list (caar candidatos) (cadar candidatos) 0))

        ;El adversario tiene killer?
        ((killer_enemigo? candidatos (vectores_soluciones matriz 1))
         (dame_killer_enemigo candidatos (vectores_soluciones matriz 1)))
        
        
        ;Hay un candidato que tiene el plus más alto
        ((not (equal? (caddar candidatos) (caddar (cdr candidatos)))) (list (caar candidatos)
                                                                            (cadar candidatos)
                                                                            0))


        ;Hay varios candidatos que tienen plus alto e iguales
        (else (elegido_aleatorio candidatos 1 (random 1 (+ (Boost candidatos (caddar candidatos) 0) 1))))))


;***************************************************************************************************************************************************************************************************************************
#|
Esta se encarga de seleccionar un elemento al azar

Entrada: la lista de candidatos entrevistados y un par de posiciones que representan el inicio de la lista de candidatos
y una posición random

Entrada: la lista de candidatos y dos posiciones de la lista de candidatos
Salida: un elemento elegido aleatoriamente
|#
(define (elegido_aleatorio candidatos posicion_actual posicion_destino)
  (cond ((null? candidatos) #f)
        ((equal? posicion_actual posicion_destino) (list (caar candidatos)
                                                          (cadar candidatos)
                                                           0))
        (else (elegido_aleatorio (cdr candidatos) (+ posicion_actual 1) posicion_destino))))


;********************************************************************************************************************************************************************************************************************************+
#|
Esta función se encarga de colocar al mejor candidato seleccionado en la matriz del turno

Entrada: el mejor candidato, el valor del jugador (1 o 2) y la matriz del turno
Salida: la matriz actualizada con el nuevo valor
|#
(define (actualizar_matriz elegido valor matriz)
  (modificar_matriz (list (car elegido) (cadr elegido)) valor matriz))


;**********************************************************************************************************************************************************************************************************************************
#|
Esta función se encarga de colocar al mejor candidato dentro de los vectores soluciones de la matriz (esto si el elemento
forma parte de alguno o algunos de los vectores soluciones)

Entrada: el mejor candidato, la lista de los vectores soluciones y una lista vacía para guardar los vectores actualizados
Salida: una lista con los vectores soluciones actualizados
|#
(define (actualizar_vectores_solucion elegido vectores final)
  (cond ((null? vectores)(invertir_lista final))
        ((intersecan? elegido (car vectores)) (actualizar_vectores_solucion
                                               elegido
                                               (cdr vectores)
                                               (cons (actualiza_vector elegido
                                                                       (car vectores)
                                                                       '()) final)))

        (else (actualizar_vectores_solucion elegido (cdr vectores) (cons (car vectores)
                                                                         final)))))

;Función auxiliar para actualizar los vectores soluciones, esta función actualiza a los vectores individuales
(define (actualiza_vector elegido vector final)
  (cond ((null? vector) (invertir_lista final))
        ((equal? elegido (car vector)) (actualiza_vector elegido
                                                         (cdr vector)
                                                         (cons (list (car elegido)
                                                                     (cadr elegido)
                                                                     2) final)))

        (else (actualiza_vector elegido (cdr vector) (cons (car vector) final)))))


;**************************************************************************************************************************
; SECCIÓN 4 - SOLUCIÓN
;**************************************************************************************************************************

#|
Esta función verifica si el jugador ya tiene una línea completa formada en la matriz

Entrada: la matriz del turno y los vectores soluciones del jugador
Salida: #t si el jugador ya ganó y #f en caso contrario
|#
(define (solucion_jugador? matriz vectores_soluciones)
  (cond ((null? vectores_soluciones) #f)
        ((equal? (Boost (car vectores_soluciones) 1 0) (length (car vectores_soluciones))) #t)
        (else (solucion_jugador? matriz (cdr vectores_soluciones)))))

;**********************************************************************************************************************************************************************************************************************+
#|
Esta función valida si el algoritmo ya formó una línea completa después de colocar al mejor candidato en la matriz del
turno. Además debe retornar la mejor posición junto con un estado del juego (1 2 3)

Entrada: los vectores soluciones actualizados y al mejor candidato
Salida: una lista del tipo ---> (fila columna estado)
|#
;Función solución, valida si ya se ha llegado a una solución una vez puesto el mejor candidato
(define (solucion? matriz_actualizada vectores_solucion_actualizados elegido)
  (cond ((null? vectores_solucion_actualizados) (list (car elegido) (cadr elegido) 0))

        ;Llegué a una solución?
        ((equal? (Boost (car vectores_solucion_actualizados) 2 0)
                 (length (car vectores_solucion_actualizados)))         
         (list (car elegido) (cadr elegido) 2))
               
                                                                                                                  
        (else (solucion? matriz_actualizada (cdr vectores_solucion_actualizados) elegido))))

;*************************************************************************************************************************************************************************+************************************************************************************************************************
#|
Esta función determina si en la matriz ya no hay espacios vacíos, lo que significa que hay un empate.

Entrada: la matriz del turno
Salida: #t si hay empate y #f en caso contrario
|#
(define (Empate? matriz)
  (cond ((null? matriz) #t)

        ;hay algún espacio vació en la fila
        ((not (fila_zero? (car matriz))) #f)
        
        ;si no tiene 0´s, seguimos con la siguiente fila
        (else (Empate? (cdr matriz)))))

;Función auxiliar para ver si hay empate, busca coincidencias de 0's por fila
(define (fila_zero? fila)
  (cond ((null? fila) #t)

        ;la columna actual es 0?
        ((zero? (car fila)) #f)
        
        (else (fila_zero? (cdr fila)))))
         

;********************************************************************************************************************************************************+******************************************************************
;*************************************************************************************************************************************************************************************************************************
;    PRUEBAS Y DEFINICIONES FINALES
;*****************************************************************************************************************************************************************************************************************************
;***********************************************************************************************************************************************************************************************************************************
#|
Esta función va a relacionar todas las secciones para retornar la mejor posición a partir de la matriz del turno

El orden en que se usan las definiciones de las secciones es el siguiente:

1- selección.

2- viabilidad.

3- objetivo.

4- solucion?.
|#
(define (algoritmo_codicioso matriz)
  (cond
    
    ;Todos los espacios están llenos? ---> notifique a la interfaz
    ;Le avisamos a la interfaz por medio de un 3 para indicar que es un empate
    ((Empate? matriz) (list 1 1 3))
    
    ;El usuario ya ganó? ---> notifique a la interfaz
    ;un 1 al final de la lista equivale a un gane del jugador
    ((solucion_jugador? matriz (vectores_soluciones matriz 1)) (list 1 1 1))

    ;Si eso no se cumple significa que la partida no ha terminado
    ;por lo que se debe buscar a la mejor posición del turno
    (else (seleccion matriz))))



#|
Esta función toma la matriz del turno y coloca a todas las posiciones disponibles en una lista de candidatos
Ademas, a cada uno de los candidatos seleccionados, les da un valor de Plus de 0, ya que aun no han sido entrevistados.

Cuando ya se haya completado el proceso de selección se le envía la lista de candidatos seleccionados y la matriz a la
función de viabilidad para que los evalúe.
|#
(define (seleccion matriz)
  (viabilidad matriz (seleccionar_candidatos matriz)))

#|
Esta función toma a la matriz del turno y la lista de candidatos seleccionados y se va a encargar de evaluar a cada
uno de los candidatos para darles un valor de Plus.

Cuando ya se haya finalizado el proceso de viabilidad se le envía la matriz, la lista de los candidatos entrevistados y
la lista de los vectores soluciones a la función objetivo.
|#
(define (viabilidad matriz candidatos_seleccionados)
  (objetivo matriz (entrevista candidatos_seleccionados (vectores_soluciones matriz 2)) (vectores_soluciones matriz 2)))

#|
Esta función toma la lista de candidatos entrevistados la lista de los vectores soluciones para elegir al candidato que
venga con el mayor valor de Plus y así actualizar la matriz y los vectores soluciones.

Cuando haya finalizado el proceso envía la matriz actualizada, los vectores actualizados y el candidato elegido, a la
función de solución.
|#
(define (objetivo matriz candidatos_entrevistados vectores_solucion)
  (objetivo_aux matriz (el_elegido (Quick_Sort candidatos_entrevistados) matriz) vectores_solucion))

;función auxiliar de objetivo
(define (objetivo_aux matriz elegido vectores_solucion)
  (solucion? (actualizar_matriz elegido 2 matriz) (actualizar_vectores_solucion elegido vectores_solucion '()) elegido))