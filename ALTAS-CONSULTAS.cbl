      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ALUMNOS ASSIGN TO "..\alumnos.dat"
           ORGANIZATION RELATIVE
           ACCESS MODE is DYNAMIC
           RELATIVE key is rel-alu.
       DATA DIVISION.
       FILE SECTION.
       FD  ALUMNOS.
       01  alu-reg.
           03 al-legajo pic 9(5).
           03 al-dni pic 9(8).
           03 al-puntero pic 9(4).

       WORKING-STORAGE SECTION.
       77  sen pic 9.
           88 fin-de-archivo value 1.
       01  rel-alu pic 9(4).
           88 no-quiere-mas value 0.
       77  w-llave-menu pic 9.
           88 salir-menu VALUE 3.
       01  w-soc-ant pic 9(5).
       01  w-resul pic 9(3).
       01  w-resto pic 9(3).
       77  w-oficina pic 9(4) value 2378.
       01  w-posicion-nula pic 9(3) VALUE ZERO.

      ******************************************************************
      ******************************************************************
       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           PERFORM 100-INICIO.
           PERFORM 200-MENU.
           PERFORM UNTIL salir-menu
                PERFORM 300-PROCESO
               PERFORM 200-MENU
           END-PERFORM.
           PERFORM 800-FIN.

            STOP RUN.
      ******************************************************************
      ******************************************************************
       100-INICIO.
           OPEN I-O ALUMNOS.
           move w-oficina to w-posicion-nula.

       110-PRIMER-POSICION-VACIA.
           MOVE w-oficina TO rel-alu
           START ALUMNOS KEY IS = rel-alu
            INVALID KEY
            DISPLAY "NO HAY LUGAR PARA SINONIMOS"
            NOT INVALID KEY
               READ ALUMNOS
               MOVE al-puntero TO w-posicion-nula.

       200-MENU.

           DISPLAY ".................................... ".
           DISPLAY "  INGRESE UNA OPCION: "
           DISPLAY "  1- ALTA"
           DISPLAY "  2- CONSULTA"
           DISPLAY "  3- FIN"
           DISPLAY "..................................... ".
           ACCEPT w-llave-menu.
           PERFORM 210-VALIDAR-OPCION.

       210-VALIDAR-OPCION.
           PERFORM UNTIL w-llave-menu < 4 AND w-llave-menu >0
               DISPLAY "Opcion incorrecta"
               PERFORM 200-MENU
           END-PERFORM.

       300-PROCESO.
           IF w-llave-menu is EQUAL 1
               PERFORM 400-ALTA
           ELSE
               PERFORM 600-CONSULTA
           END-IF.

       400-ALTA.
           PERFORM 405-PIDO-SOCIO.
           PERFORM 410-INVOCAR-FUNCION-HASHING.
           PERFORM 420-BUSCAR-UBICACION.

       405-PIDO-SOCIO.
           DISPLAY "Ingrese legajo del alumno".
           ACCEPT w-soc-ant.

       410-INVOCAR-FUNCION-HASHING.
           DIVIDE 97 INTO w-soc-ant GIVING w-resul REMAINDER rel-alu.
           add 1 to rel-alu.
           MOVE rel-alu to w-resto.

       420-BUSCAR-UBICACION.
           PERFORM 430-LEER-SOCIO
           IF al-legajo=0
               PERFORM 440-PRIMER-INGRESO
           ELSE
               IF  w-soc-ant = al-legajo
                   PERFORM 460-INGRESO-EXISTENTE

               ELSE
                   PERFORM 480-UBICAR-SINONIMO
               END-IF
           END-IF.

       430-LEER-SOCIO.
           READ ALUMNOS.

       440-PRIMER-INGRESO.
            MOVE w-soc-ant to al-legajo
               PERFORM 450-PIDO-NOMBRE
               PERFORM 470-ACTUALIZAR-SOCIO.

       460-INGRESO-EXISTENTE.
           DISPLAY "El socio ya se encuentra registrado"
           PERFORM 620-MOSTRAR-SOCIO.

       470-ACTUALIZAR-SOCIO.
           REWRITE alu-reg.

       450-PIDO-NOMBRE.
           DISPLAY "Ingrese el DNI".
           ACCEPT al-dni.

       455-MOVER-VARIABLES.
           MOVE al-puntero to rel-alu.
           MOVE w-soc-ant TO al-legajo.
           MOVE ZERO to al-puntero.
           PERFORM 450-PIDO-NOMBRE.
           PERFORM 470-ACTUALIZAR-SOCIO.

       480-UBICAR-SINONIMO.
            PERFORM UNTIL al-puntero is =0
            or w-soc-ant is =al-legajo
             MOVE al-puntero to rel-alu
             PERFORM 430-LEER-SOCIO
            END-PERFORM.
             IF  w-soc-ant = al-legajo
                PERFORM 460-INGRESO-EXISTENTE
             ELSE
                PERFORM 500-BUSCO-LUGAR
            END-IF.

       500-BUSCO-LUGAR.
           add 1 to  w-oficina.
           MOVE w-oficina to al-puntero.
           PERFORM 470-ACTUALIZAR-SOCIO.
           PERFORM 455-MOVER-VARIABLES.
           PERFORM 550-REINICIO-OFICINA.

       550-REINICIO-OFICINA.
           ADD 1 TO rel-alu.
           MOVE rel-alu to al-puntero.
           MOVE w-posicion-nula TO rel-alu.
           MOVE zero to al-legajo.
           move 0 to al-dni.
           PERFORM 470-ACTUALIZAR-SOCIO.

      ******************************************************************
      ******************************************************************

       600-CONSULTA.
            PERFORM 405-PIDO-SOCIO.
            PERFORM 410-INVOCAR-FUNCION-HASHING.
            PERFORM 430-LEER-SOCIO.
             IF  w-soc-ant = al-legajo
                 PERFORM 620-MOSTRAR-SOCIO
             ELSE
                PERFORM 630-BUSCAR-SOCIO-SINONIMO
            END-IF.

       620-MOSTRAR-SOCIO.
            display "LEGAJO: ",al-legajo," DNI: ", al-dni.


       630-BUSCAR-SOCIO-SINONIMO.

           PERFORM UNTIL al-puntero is =0 or w-soc-ant is =al-legajo
               MOVE  al-puntero to rel-alu
               PERFORM 430-LEER-SOCIO
           END-PERFORM
           IF  w-soc-ant = al-legajo
            PERFORM 620-MOSTRAR-SOCIO
           ELSE
               DISPLAY "El legajo ingresado no se encuentra"
           END-IF.

       800-FIN.
           CLOSE ALUMNOS.

       END PROGRAM YOUR-PROGRAM-NAME.
