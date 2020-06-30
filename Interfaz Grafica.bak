#lang racket

;imports
(require (lib "graphics.ss" "graphics"))
(require "Algoritmo.rkt")
;se inician los gráficos
(open-graphics)

;se define la ventana principal
(define ventana (open-viewport "Tic Tac Toe" 1200 680))

;***************************************************************************************************************************************************************************************************************************************************
;matriz que se usará para guardar valores y validar posiciones correctas en el tablero
(define matriz_logica '())

;matriz que se usará para enviarle al algoritmo goloso
(define matriz_encargo '())

;se usa para iterar posiciones en x
(define x 0)

;se usa para iterar posiciones en y
(define y 0)

#|
Lista que representa los margenes de posiciones con respecto a las filas
cada elemento representa --> (fila inicio fin)

|#
(define margenes_filas '((1 10 70) (2 70 130) (3 130 190) (4 190 250) (5 250 310)
                                   (6 310 370) (7 370 430) (8 430 490) (9 490 550) (10 550 610)))



#|
Lista que representa los margenes de posiciones con respecto a las columnas
cada elemento representa --> (fila inicio fin)

|#
(define margenes_columnas '((1 270 330) (2 330 390) (3 390 450) (4 450 510) (5 510 570)
                                   (6 570 630) (7 630 690) (8 690 750) (9 750 810) (10 810 870)))


                                  

;************************************************************************************************************************************
;************************************************************************************************************************************
;************************************************************************************************************************************
;función que inicializa el juego

(define (TTT filas columnas)

  
  ;Creamos una matriz lógica que se usará durante toda la partida
  (set! matriz_logica (crear_matriz_logica filas columnas))

  ;Creamos una matriz lógica sin posiciones, o sea solo con valores (0 1 2)
  (set! matriz_encargo (crear_matriz filas columnas))
  
  ;Ahora se valida que el tamaño de la matriz sea el correcto
  (cond ((not (dimensiones_correctas (numero_columnas matriz_encargo) (numero_filas matriz_encargo)))
         (print "Las dimensiones de la matriz no son válidas")
         (close-viewport ventana))

        (else
          ;se muestra la ventana de bienvenida
         (desplegar_bienvenida)
         ;Dibujamos la matriz en la pantalla
         (crear_tablero filas columnas)
         ;luego se inicia el juego
         (juego filas columnas))))


;***************************************************************************************************************************************************************************************************************************************************
;Ciclo principal del juego, aquí se van a ejecutar todos los turnos, siempre y cuando el usuario
;haya presionado clcik izquierdo.
;Si no lo ha hecho, el programa se queda esperando a que le dé click izquierdo
(define (juego filas columnas)
  (cond
    
    ;¿No se ha presionado click izquierdo?
    ((equal? (left-mouse-click? (get-mouse-click ventana)) #f)
     (juego filas columnas))

    ;Si se presionó click izquierdo          
    (else (let* ([posX (posn-x (query-mouse-posn ventana))]
                 [posY (posn-y (query-mouse-posn ventana))]
                 )
            (cond
              ;tocó un lugar dentro de la matriz?
              ((en_matriz? posX posY filas columnas)
               (let* ([fila_seleccionada (numero_fila posY margenes_filas)]
                      [columna_seleccionada (numero_columna posX margenes_columnas)]
                      )
                 (cond
                 ;Ese lugar está disponible
                 ((disponible? fila_seleccionada columna_seleccionada matriz_logica)
                  
                  ;Se dibuja una X en esa posicion
                  (modificar_tablero_grafico posX posY "X")

                  ;Se modifica la matriz lógica
                  (set! matriz_logica
                        (modificar_matriz_logica matriz_logica fila_seleccionada columna_seleccionada 1))
                  
                  ;Se modifica la matriz que se le va a enviar al algoritmo
                  (set! matriz_encargo
                        (modificar_matriz (list fila_seleccionada columna_seleccionada) 1 matriz_encargo))

                  ;Ahora se le debe enviar la matriz_encargo al algoritmo goloso
                  ;para que me devuelva una posición (fila columna)
                  (let* ([pos (algoritmo_codicioso matriz_encargo)]
                         [nueva_fila (car pos)]
                         [nueva_columna (cadr pos)]
                         [resultado (caddr pos)]
                         )
                    
                    ;actualizamos la matriz lógica con un 2 en la posición recibida     
                    (set! matriz_logica
                        (modificar_matriz_logica matriz_logica nueva_fila nueva_columna 2))
                    
                    ;Dibujamos la nueva posición O en el tablero
                    (modificar_tablero_grafico (dame_pos_c nueva_columna margenes_columnas)
                                               (dame_pos_f nueva_fila margenes_filas)
                                               "O")
                    ;Se modifica la matriz que se le va a enviar al algoritmo
                    (set! matriz_encargo     
                         (modificar_matriz (list nueva_fila nueva_columna) 2 matriz_encargo))

                    ;ya ganó alguien?
                    (cond
                      ;nadie ha ganado pero no ha finalizado la partida
                      ((zero? resultado)
                       ;Se envía al inicio de la función para que espere el siguiente click
                       (juego filas columnas))

                      ;ganó el usuario
                      ((equal? resultado 1)
                       (conclusion resultado)
                       (sleep 2)
                       (inicializar_partida filas columnas)
                       )
                      

                      ;ganó la máquina
                      ((equal? resultado 2)
                       (conclusion resultado)
                       (sleep 2)
                       (inicializar_partida filas columnas))

                      ;Hay un empate?
                      ((equal? resultado 3)
                       ;esperamos 2 segundos
                       (conclusion 2)
                       (sleep 2)
                       ;se empieza otra vez el juego
                       (inicializar_partida filas columnas))
                      )
                    
                    
                    ))

                 ;Ese lugar no está disponible
                 (else 
                        
                  ;Se notifica al usuario
                  (mensaje_alerta "Posición inválida" 1)

                  ;Se envía al inicio de la función para que espere el siguiente click
                  (juego filas columnas)
                  ))))
                 

              ;no tocó dentro de la matriz
              (else
               
               ;Se envía al inicio de la función para que espere el siguiente click
               (juego filas columnas))
              
                  )
            )
          )))

;***************************************************************************************************************************************************************************************************************************************************

;Función que se llama una vez se haya acabado una partida
;se encarga de inicializar las definiciones de matrices y las capas
(define (inicializar_partida filas columnas)
  ;se coloca una nueva capa en toda la pantalla
  ((draw-solid-rectangle ventana) (make-posn 0 0) 1200 680 "black")
   
  ;Creamos una matriz lógica que se usará durante toda la partida
  (set! matriz_logica (crear_matriz_logica filas columnas))

  ;Creamos una matriz lógica sin posiciones, o sea solo con valores (0 1 2)
  (set! matriz_encargo (crear_matriz filas columnas)) 
  
  ;Dibujamos la matriz en la pantalla
  (crear_tablero filas columnas)
  ;luego se inicia el juego
  
  (juego filas columnas))

;***************************************************************************************************************************************************************************************************************************************************
;***************************************************************************************************************************************************************************************************************************************************


;Función que recibe el numero de filas y retorna la posicion los margenes de esa fila como una lista
;debe devolver (margen-inicial margen-final)
(define (margenes_fila fila margenes)
  (cond ((equal? fila (caar margenes))
          (list (cadar margenes)
                (caddar margenes)))
          (else (margenes_fila fila (cdr margenes)))))


;***************************************************************************************************************************************************************************************************************************************************
;Función que recibe el numero de filas y retorna la posicion los margenes de esa fila como una lista
;debe devolver (margen-inicial margen-final)
(define (margenes_columna columna margenes)
  (cond ((equal? columna (caar margenes))
          (list (cadar margenes)
                (caddar margenes)))
          (else (margenes_columna columna (cdr margenes)))))


;***************************************************************************************************************************************************************************************************************************************************
;Función que valida si la posición elegida con el mouse se encuentra dentro de los márgenes de la matriz
;que se creó
(define (en_matriz? posX posY filas columnas)
  (cond    
    ;La posición está dentro de la matriz dibujada?
    ((and (> posX 270)
          (< posX (cadr (margenes_columna columnas margenes_columnas)))
          (> posY 10)
          (< posY (cadr (margenes_fila filas margenes_filas)))) #t)
    
    ;Si se salió de la matriz devuelve false
        (else #f)))


;***************************************************************************************************************************************************************************************************************************************************
;Función que recibe una posición X y Y
;Se debe colocar una X en dicha posición
(define (modificar_tablero_grafico posX posY tipo_figura)
  (colocar_figura tipo_figura
                  (localizar_columna posX margenes_columnas)
                  (localizar_fila posY margenes_filas)
                  ))


;***************************************************************************************************************************************************************************************************************************************************
;Función que recibe una posición en X y valida en cuál columna se encuetra dentro del tablero
;devuelve la posición X que se debe usar para dibujar en dicha columna
(define (localizar_columna posX margenes)
  (cond ((and (>= posX (cadar margenes))
              (< posX (caddar margenes))) (cadar margenes))
        (else (localizar_columna posX (cdr margenes)))))



;***************************************************************************************************************************************************************************************************************************************************
;Función que recibe una posición en X y valida en cuál columna se encuetra dentro del tablero
;devuelve el número de columna en el que se encuentra posX
(define (numero_columna posX margenes)
  (cond ((and (>= posX (cadar margenes))
              (< posX (caddar margenes))) (caar margenes))
        (else (numero_columna posX (cdr margenes)))))


;***************************************************************************************************************************************************************************************************************************************************
;Función que recibe una posición en Y y valida en cuál fila se encuetra dentro del tablero
;devuelve la posición Y que se debe usar para dibujar en dicha fila
(define (localizar_fila posY margenes)
  (cond ((and (>= posY (cadar margenes))
              (< posY (caddar margenes))) (cadar margenes))
        (else (localizar_fila posY (cdr margenes)))))



;***************************************************************************************************************************************************************************************************************************************************
;Función que recibe una posición en Y y valida en cuál fila se encuetra dentro del tablero
;devuelve el número de fila en el que se encuentra posX
(define (numero_fila posY margenes)
  (cond ((and (>= posY (cadar margenes))
              (< posY (caddar margenes))) (caar margenes))
        (else (numero_fila posY (cdr margenes)))))


;***************************************************************************************************************************************************************************************************************************************************
;Función que recibe el número de fila y retorna la posición en Y correspondiente para poder dibujar
(define (dame_pos_f fila margenes)
  (cond ((equal? fila (caar margenes)) (cadar margenes))
        (else (dame_pos_f fila (cdr margenes)))))



;***************************************************************************************************************************************************************************************************************************************************
;Función que recibe el número de columna y retorna la posición en X correspondiente para poder dibujar
(define (dame_pos_c columna margenes)
  (cond ((equal? columna (caar margenes)) (cadar margenes))
        (else (dame_pos_c columna (cdr margenes)))))


;***************************************************************************************************************************************************************************************************************************************************
;Función que dado el numero de filas y columnas dibuja el tablero correspondiente
(define (crear_tablero filas columnas)
  (let* ([ultima_posicion_x (+ 330 (* 60 (- columnas 1)))]
         [pos_vertical_2 (+ 10 (* 60 filas))]
         [ultima_posicion_y (+ 70 (* 60 (- filas 1)))]
         [pos_horizontal_2 (+ 270 (* 60 columnas))]
         [pos_rect_y (+ 60 (dame_pos_f filas margenes_filas))]
         [pos_rect_x (+ 60 (dame_pos_c columnas margenes_columnas))])

    
    ;dibujar las líneas verticales
    (for ([x (in-range 330 ultima_posicion_x 60)])
      ((draw-line ventana) (make-posn x 10) (make-posn x pos_vertical_2) "yellow")
      ;repintando las líneas
      ((draw-line ventana) (make-posn (+ x 1) 10) (make-posn (+ x 1) pos_vertical_2) "yellow")

      )
    
    ;dibujar las líneas horizontales
    (for ([y  (in-range 70 ultima_posicion_y 60)])
      ((draw-line ventana) (make-posn 270 y) (make-posn pos_horizontal_2 y) "yellow")
      ;repintando las líneas
      ((draw-line ventana) (make-posn 270 (+ y 1)) (make-posn pos_horizontal_2 (+ y 1)) "yellow")
      )
    
   ;dibujar los rectángulos que demarcan el tablero en la pantalla
    ;Esto es para colocar los cuadros grises que aparecen al lado del tablerp
    
   ((draw-solid-rectangle ventana) (make-posn 0 0) 270 680 "black")
   ((draw-solid-rectangle ventana) (make-posn 0 0) 1200 10 "black")
    ;rectángulo que tapa abajo de la matriz
    ((draw-solid-rectangle ventana) (make-posn 0 pos_rect_y) 1200 (- 680 pos_rect_y) "black")
    ;rectángulo que tapa a la derecha de la matriz
    ((draw-solid-rectangle ventana) (make-posn pos_rect_x 0) (- 1200 pos_rect_x) 680 "black")    
    ))  
  


;***************************************************************************************************************************************************************************************************************************************************
;Función que muestra la pantalla de bienvenida durante 5 segundos
(define (desplegar_bienvenida)
  ((draw-pixmap ventana) "Recursos/Negro.png" (make-posn 0 0) "black")
  ;se muestra por 1 segundo
  (sleep 1)
  ;se dibuja la imagen 1
  ((draw-pixmap ventana) "Recursos/Tic.png" (make-posn 0 0) "black")
  ;se muestra por 1 segundo
  (sleep 1)
  ;se dibuja la imagen 1
  ((draw-pixmap ventana) "Recursos/Tac.png" (make-posn 0 0) "black")
  ;se muestra por 1 segundo
  (sleep 1)
  ;se dibuja la imagen 1
  ((draw-pixmap ventana) "Recursos/Toe.png" (make-posn 0 0) "black")
  ;se muestra por 1 segundo
  (sleep 1)
   ;se dibuja la imagen 1
  ((draw-pixmap ventana) "Recursos/Silueta.png" (make-posn 0 0) "black")
  ;se muestra por 1 segundo
  (sleep 1)
   ;se dibuja la imagen 1
  ((draw-pixmap ventana) "Recursos/Nombres.png" (make-posn 0 0) "black")
  ;se muestra por 1 segundo
  (sleep 2)
  ;se tapa la capa anterior con un fondo blanco que cubra toda la pantalla
  ((draw-solid-rectangle ventana) (make-posn 0 0) 1200 680 "black"))


;***************************************************************************************************************************************************************************************************************************************************
;Función que despliega una imagen en caso de llegar a un gane
;un 0 equivale a una derrota
;un 1 equivale a una victoria
(define (conclusion resultado)
  (cond ((equal? resultado 2)
         ((draw-pixmap ventana) "Recursos/loser.jpg" (make-posn 0 0) "black"))
        (else ((draw-pixmap ventana) "Recursos/winner.jpg" (make-posn 0 0) "black"))))

        

         
;***************************************************************************************************************************************************************************************************************************************************
;Función que recibe una un valor (X o O) y una posición
;Dibuja una figura del tipo que se le pase por parámetro en la posición dada
(define (colocar_figura tipo posX posY)
  (cond
    ;En caso de que tipo sea X
    ((equal? tipo "X")
     ((draw-pixmap ventana) "Recursos/equis.png" (make-posn (+ posX 4) (+ posY 4)) "black"))

    ;En caso de que tipo sea O
    ((equal? tipo "O")
     ((draw-pixmap ventana) "Recursos/circulo.png" (make-posn (+ posX 4) (+ posY 4)) "black"))
    ))



;***************************************************************************************************************************************************************************************************************************************************
;Función que muestra un mensaje de alerta
;recibe una mensaje de tipo string
(define (mensaje_alerta mensaje tiempo)
  ;Se crea una nueva ventana para mostrar el mensaje
  (define ventana_alerta (open-viewport "Alerta" 300 50))
  ;Se dibuja un string en color rojo
  ((draw-string ventana_alerta) (make-posn 50 20) mensaje "red")
  ;Se muestra la ventana por 2 segundos
  (sleep tiempo)
  ;se cierra la ventana
  (close-viewport ventana_alerta)
  )



;***************************************************************************************************************************************************************************************************************************************************
;Función que se encarga de crear una matriz lógica para guardar la información que se encuentra en el tablero
;gráfico

(define (crear_matriz_logica filas columnas)
  (cond ((and (>= filas 3) (<= filas 10) (>= columnas 3) (<= columnas 10))
         (construir_matriz filas 1 columnas '()))
        
        (else #f)))

(define (construir_matriz filas contador_fila columnas total)
  (cond ((equal? contador_fila (+ filas 1)) (reverse total))
        (else (construir_matriz filas (+ contador_fila 1)
                                columnas (cons (crear_fila contador_fila columnas 1 '())
                                               total)))))

(define (crear_fila fila columnas contador lista)
  (cond ((equal? contador columnas) (reverse (cons (list fila contador 0) lista)))
        (else (crear_fila fila columnas (+ contador 1)
                          (cons (list fila contador 0) lista)))))


;***************************************************************************************************************************************************************************************************************************************************;********************************************************************************************************************************************
;Función que recibe el numero de fila, columna y un valor
;devuelve una matriz con la nueva posición actualizada
(define (modificar_matriz_logica matriz fila columna valor)
  (colocar fila columna 1 valor matriz '()))

(define (colocar fila columna contador valor matriz matriz_final)
  (cond ((null? matriz)(reverse matriz_final))
        ((equal? contador fila)(colocar fila columna
                                              (+ contador 1) valor (cdr matriz)
                                              (cons (colocar_aux (car matriz) columna fila 1 valor '()) matriz_final)))
        
        (else (colocar fila columna (+ contador 1) valor (cdr matriz) (cons (car matriz) matriz_final)))))


;Función auxiliar de colocar
;recibe una matriz del tipo ((1 1 0) (1 2 2) (1 3 0))                               
(define (colocar_aux lista columna fila contador valor lista_final)
  (cond ((null? lista)(reverse lista_final))
        
        ((and (equal? contador columna)
              (zero? (caddar lista)))
         (colocar_aux (cdr lista) columna fila (+ contador 1) valor
                            (cons (list fila contador valor) lista_final)))
        
        (else (colocar_aux (cdr lista) columna fila (+ contador 1) valor (cons (car lista) lista_final)))))



;***************************************************************************************************************************************************************************************************************************************************
;Función que recibe el numero de fila y columna
;me dice si ese espacio tiene un 0, o sea si está disponible
(define (disponible? fila columna matriz)
  (buscar_fila fila columna 1 matriz))

(define (buscar_fila fila columna contador matriz)
  (cond ((equal? contador fila) (buscar_columna columna 1 (car matriz)))
        (else (buscar_fila fila columna (+ contador 1) (cdr matriz)))))

(define (buscar_columna columna contador fila)
  (cond    
    
    ((equal? contador columna) (cond ((zero? (caddar fila)) #t)
                                     (else #f)))
    
    (else (buscar_columna columna (+ contador 1) (cdr fila)))))
    



;***************************************************************************************************************************************************************************************************************************************************;************************************************************************************************************************************
;***************************************************************************************************************************************************************************************************************************************************;***************************************************************************************************************************************************************************************************************************************************
;EN ESTE ESPACIO SE PUEDEN HACER LAS PRUEBAS CORRESPONDIENTES

;se llama a la función principal del juego con el número de filas y el número de columnas
(TTT 3 3)



