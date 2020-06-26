#lang racket

; imports needed for this modules
(require "Matrices.rkt")

#| En este módulo se implementan las funciones que hacen posible ejecutar el algoritmo goloso
requerido para que la máquina tome las decisiones acerca de la mejor posición para el tablero.
|#


;;*************************************************************************************************************************
;    CONJUNTO DE CANDIDATOS Y SELECCIÓN
;**************************************************************************************************************************

(define (seleccionar_candidatos matriz)
  (seleccionar_candidatos_aux matriz 1 1 '()))

(define (seleccionar_candidatos_aux matriz fila columna candidatos)
  (cond ((null? matriz) (invertir_lista candidatos))
        ((null? (car matriz)) (seleccionar_candidatos_aux (cdr matriz) (+ fila 1) 1 candidatos))
        ((zero? (caar matriz)) (seleccionar_candidatos_aux
                                (cons (cdar matriz)
                                      (cdr matriz)) fila (+ columna 1) (cons (list fila columna 0) candidatos)))
        
        (else (seleccionar_candidatos_aux (cons (cdar matriz) (cdr matriz)) fila (+ columna 1) candidatos))))



;**************************************************************************************************************************
;    VIABILIDAD
;**************************************************************************************************************************

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



  
;**************************************************************************************************************************
;    OBJETIVO
;**************************************************************************************************************************

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


;**************************************************************************************************************************
;    SOLUCIÓN
;**************************************************************************************************************************

;Función que determina si el jugador ya ganó
(define (solucion_jugador? matriz vectores_soluciones)
  (cond ((null? vectores_soluciones) #f)
        ((equal? (Boost (car vectores_soluciones) 1 0) (length (car vectores_soluciones))) #t)
        (else (solucion_jugador? matriz (cdr vectores_soluciones)))))

;Función solución, valida si ya se ha llegado a una solución una vez puesto el mejor candidato
(define (solucion? matriz_actualizada vectores_solucion_actualizados)
  (cond ((null? vectores_solucion_actualizados) matriz_actualizada)

        ;Llegué a una solución con este primer vector?
        ((equal? (Boost (car vectores_solucion_actualizados) 2 0) (length (car vectores_solucion_actualizados))) "Hemos llegado a una solución")
        
        (else (solucion? matriz_actualizada (cdr vectores_solucion_actualizados)))))

  

;**************************************************************************************************************************
;    PRUEBAS Y DEFINICIONES FINALES
;**************************************************************************************************************************

#|
Esta función usa todas las definiciones anteriores para tomar una desición.

Si no hay gane por parte del usuario el procedimiento es el siguiente:

1- Función de selección.

2- Función de viabilidad.

3- Función objetivo.

4- Función solucion?.
|#

(define  (algoritmo_codicioso matriz)
  (cond ((solucion_jugador? matriz (vectores_soluciones matriz 1)) "El usuario ya ganó")
        (else (seleccion matriz))))

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
                                                                                              
           
;****************************************************************************************************************************
;matriz de prueba
(define matriz '((1 1 1) (2 2 0) (0 0 0)))

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