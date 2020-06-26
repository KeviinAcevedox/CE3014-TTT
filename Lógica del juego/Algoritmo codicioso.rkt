#lang racket

; imports needed for this modules
(require "Matrices.rkt")

#| En este módulo se implementan las funciones que hacen posible ejecutar el algoritmo goloso
requerido para que la máquina tome las decisiones acerca de la mejor posición para el tablero.
|#


;************************************************************************************************************************                
#|
Esta es la función de selección, recibe como parámetro la matriz lógica (enviada desde la interfaz). Su rol se basa
en recorrer la matriz y buscar aquellas posiciones que estén disponibles para que la máquina elija la mejor.

Entrada: una matriz lógica que representa la matriz que se ve desde la interfaz gráfica.

Salida: una lista que contiene elementos del siguiente tipo --> (fila columna plus). Un plus es un sumador de viabilidad
|#
(define (seleccionar_candidatos matriz)
  (seleccionar_candidatos_aux matriz 1 1 '()))

(define (seleccionar_candidatos_aux matriz fila columna candidatos)
  (cond ((null? matriz) (invertir_lista candidatos))
        ((null? (car matriz)) (seleccionar_candidatos_aux (cdr matriz) (+ fila 1) 1 candidatos))
        ((zero? (caar matriz)) (seleccionar_candidatos_aux (cons (cdar matriz) (cdr matriz)) fila (+ columna 1) (cons (list fila columna 0) candidatos)))
        (else (seleccionar_candidatos_aux (cons (cdar matriz) (cdr matriz)) fila (+ columna 1) candidatos))))


;**************************************************************************************************************************
#|
Esta es la función de viabilidad, debe recibir la matriz lógica y además la lista de candidatos seleccionados.

Para poder determinar la viabilidad de cada uno de los candidatos seleccionados se tomarán en cuenta todos los
vectores fila, vectores columna, vectores diagonales(/) y vectores diagonales inversos(\) que estén totalmente
disponibles (todos con 0's) o que tengan valores con 2's.

Si se tienen TODOS los vectores de la matriz (los disponibles) se va a proceder a tomar cada uno de los candidatos
seleccionados y se van a pasar por la función llamada entrevista, la cual recibe un elemento de tipo
(fila columna plus) y se encargará de evaluar las intersecciones con los vectores solución.

Los pesos se asignan de la siguiente manera:

* Interseca con un vector solución y este vector tiene todos los espacios disponibles ---> +1
* Interseca con un vector solución y este vector tiene N espacios ocupados con un 2 ---> +1 +N

Casos especiales:


* En caso de tener varios pesos mayores iguales se elegirá aleatoriamente, esto ya que con cualquiera de ellos se
cuenta con las mismas condiciones.
Si alguno de esos candidatos es el elemento que falta para completar una solución se le sumará 100.
En caso contrario, se elegirá uno aleatorio y a ese se le sumará 100.

|#

#|
Función que recibe una matriz y retorna TODOS los vectores solución válidos en el turno.

Entrada:

Salida:
|#

(define (vectores_soluciones matriz valor)
  (agregar_diagonal_inversa (crear_matriz_exacta matriz) '() valor))

 (define (agregar_diagonal_inversa matriz_exacta vectores_total valor)
   (agregar_diagonal matriz_exacta (append (dame_diagonales_inversas matriz_exacta)
                                          vectores_total) valor))


(define (agregar_diagonal matriz_exacta vectores_total valor)
  (agregar_columna matriz_exacta (append (dame_diagonales matriz_exacta)
                                          vectores_total) valor))

(define (agregar_columna matriz_exacta vectores_total valor)
  (agregar_fila matriz_exacta (append (dame_columnas matriz_exacta)
                                          vectores_total) valor))

(define (agregar_fila matriz_exacta vectores_total valor)
  (filtro_vector (append matriz_exacta vectores_total) valor))

;Función que busca los vectores solucion pero del jugador adversario



;Recibe los candidatos seleccionados y los vectores solución
;Toma cada uno de los candidatos y los evalúa en la función Plus para darles viabilidad
(define (entrevista candidatos vectores_solucion)
  (entrevista_aux candidatos vectores_solucion '()))

(define (entrevista_aux candidatos vectores_solucion candidatos_potenciales)
  (cond ((null? candidatos) (invertir_lista candidatos_potenciales))
        (else (entrevista_aux (cdr candidatos)
                              vectores_solucion
                              (cons (Plus (car candidatos) vectores_solucion 0)
                                    candidatos_potenciales)))))

;Función Plus
;Recibiendo un candidato y todos los vectores solucón
;se encarga de sumar el plus de cada candidato de acuerdo a lo explicado arriba
(define (Plus candidato vectores_solucion suma)
  (cond ((null? vectores_solucion) (list (car candidato)
                                         (cadr candidato)
                                         suma))

        ((and (intersecan? candidato (car vectores_solucion))
              (killer? candidato (car vectores_solucion) 2)) (list (car candidato)
                                                                (cadr candidato)
                                                                (+ suma 1000)))

        ((intersecan? candidato (car vectores_solucion)) (Plus candidato
                                                               (cdr vectores_solucion)
                                                               (+ suma 1 (Boost (car vectores_solucion) 2 0))))
        (else (Plus candidato (cdr vectores_solucion) suma))))


;Función Boost
;Recibe un vector y devuelve una suma en base a los valores encontrados
;Le suma 1 por cada coincidencia con el número de valor
(define (Boost vector valor suma)
  (cond ((null? vector) suma)
        ((equal? (caddar vector) valor) (Boost (cdr vector)
                                              valor
                                              (+ suma 1)))

        (else (Boost (cdr vector) valor suma))))
                                       
;Función Killer
;Determina si a un vector solo le falta una posición para convertirse en una solución completa y ganar
(define (killer? candidato vector valor)
  (cond ((and (equal? (Boost vector valor 0) (- (length vector) 1))
              (intersecan? candidato vector)) #t)
        (else #f)))

;Determina si el enemigo tiene un posible killer
;en caso de que así sea, retorna al elemento
;En caso contrario retorna '()
;Se debe tomar en cuenta que los candidatos que llegan ya están evaluados
(define (killer_enemigo? candidatos vectores_soluciones_enemigo)
  (cond ((null? candidatos) #f)
        ((killer_vectores? (list (caar candidatos) (cadar candidatos) 0) vectores_soluciones_enemigo 1) #t)
        (else (killer_enemigo? (cdr candidatos) vectores_soluciones_enemigo))))

(define (killer_vectores? candidato vectores valor)
  (cond ((null? vectores) #f)
        ((killer? candidato (car vectores) valor) #t)
        (else (killer_vectores? candidato (cdr vectores) valor))))



  
;*******************************************************************************************************************
#|
Ahora se define la función objetivo, esta se encarga de ordenar los candidatos seleccionados en orden decendente
debido a que por eficiencia nos sirve tener al candidato con mayor plus o a los candidatos con mayor plus al inicio.

|#

;Algoritmo de ordenamiento Quick Sort para ordenar a los candidatos en orden decendente.
; Tomando como pivote al primer elemento
; Se forma una lista con los elementos menores que el pivote
; y otra con los elementos mayores que el pivote
; luego se palica la misma técnica con las dos listas formadas uniendolas mediante la función append

(define (Quick_Sort lista)
  (cond ((null? lista) lista)
        (else (append (Quick_Sort (car (particion lista))) (list (car lista)) (Quick_Sort (cadr (particion lista)))))))

(define (particion lista)
  (cond ((null? lista) #f)
        (else (particion_aux (car lista) (cdr lista) '() '() ))))
                  
(define (particion_aux pivote lista menores mayores)
  (cond ((null? lista) (list menores mayores))
        ((>= (caddar lista) (caddr pivote)) (particion_aux pivote (cdr lista) (cons (car lista) menores) mayores))
        (else (particion_aux pivote (cdr lista) menores (cons (car lista) mayores)))))


;Función que retorna al candidato con más plus, debe recibir la lista de candidatos entrevistados en la función
;de viabilidad.
;Los candidatos deben estar ordenanos en base a sus plus usando el algoritmo Quick Sort.

(define (el_elegido candidatos matriz)
        ;Tengo killer?
  (cond ((>= (caddar candidatos) 1000) (list (caar candidatos) (cadar candidatos) 0))

        ;El adversario tiene killer?
        ;Esta está en veremos
        
        ;Hay un candidato que tiene el plus más alto
        ((not (equal? (caddar candidatos) (caddar (cdr candidatos)))) (list (caar candidatos)
                                                                            (cadar candidatos)
                                                                            0))


        ;Hay varios candidatos que tienen plus alto e iguales
        (else (elegido_aleatorio candidatos 1 (random 1 (+ (Boost candidatos (caddar candidatos) 0) 1))))))

(define (elegido_aleatorio candidatos posicion_actual posicion_destino)
  (cond ((null? candidatos) #f)
        ((equal? posicion_actual posicion_destino) (list (caar candidatos)
                                                          (cadar candidatos)
                                                           0))
        (else (elegido_aleatorio (cdr candidatos) (+ posicion_actual 1) posicion_destino))))


;Función que actualiza la matriz lógica actual con el candidato elegido
(define (actualizar_matriz elegido valor matriz)
  (modificar_matriz (list (car elegido) (cadr elegido)) valor matriz))


;Función que se encarga de actualizar los vectores_solución
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

;Función que recibe una posición vacía y un vector. Esta función actualiza al vector individual.
(define (actualiza_vector elegido vector final)
  (cond ((null? vector) (invertir_lista final))
        ((equal? elegido (car vector)) (actualiza_vector elegido
                                                         (cdr vector)
                                                         (cons (list (car elegido)
                                                                     (cadr elegido)
                                                                     2) final)))

        (else (actualiza_vector elegido (cdr vector) (cons (car vector) final)))))


;***************************************************************************************************************************
;***************************************************************************************************************************
;***************************************************************************************************************************

;ALGORITMO CODICIOSO EN UNA FUNCION FINAL
(define  (algoritmo_codicioso matriz)
  (seleccion matriz))

;Función de seleccion
(define (seleccion matriz)
  (viabilidad matriz (seleccionar_candidatos matriz)))

;Función de  viabilidad
(define (viabilidad matriz candidatos_seleccionados)
  (objetivo matriz (entrevista candidatos_seleccionados (vectores_soluciones matriz 2)) (vectores_soluciones matriz 2)))

;Función Objetivo
(define (objetivo matriz candidatos_entrevistados vectores_solucion)
  (objetivo_aux matriz (el_elegido (Quick_Sort candidatos_entrevistados) matriz) vectores_solucion))

(define (objetivo_aux matriz elegido vectores_solucion)
  (solucion? (actualizar_matriz elegido 2 matriz) (actualizar_vectores_solucion elegido vectores_solucion '())))
                                                                                              
;Función solución
(define (solucion? matriz_actualizada vectores_solucion_actualizados)
  (cond ((null? vectores_solucion_actualizados) matriz_actualizada)

        ;Llegué a una solución
        ((equal? (Boost (car vectores_solucion_actualizados) 2 0) (length (car vectores_solucion_actualizados))) "Hemos llegado a una solución")

        ;El adversario llegó a una solución
        ((equal? (Boost (car vectores_solucion_actualizados) 1 0) (length (car vectores_solucion_actualizados))) "El jugador llegó a una solución")

        (else (solucion? matriz_actualizada (cdr vectores_solucion_actualizados)))))

             
;*************************************************************************************************************+
;matriz de prueba
(define matriz '((0 1 1) (0 2 0) (0 0 0)))

;Candidatos sin evaluar
(define c (seleccionar_candidatos matriz))

;vectores solución de la matriz dada
(define v (vectores_soluciones matriz 2))

;vectores solución enemigo de la matriz dada
(define v2 (vectores_soluciones matriz 1))

;candidatos entrevistados
(define e (entrevista c v))

;candidatos entrevistados y ordenados
(define e2 (Quick_Sort e))

;elemento elegido
(define k (el_elegido e2 v))

;Resultado al usar el algoritmo con la matriz dada
(define M (algoritmo_codicioso matriz))
















