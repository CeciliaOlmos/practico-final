      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SORT-ALU.
        ENVIRONMENT DIVISION.
       CONFIGURATION SECTION. SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SORT-ALU
           ASSIGN TO "sortwork".
           SELECT LISTADO
           ASSIGN TO PRINTER,
           "..\impOrden.dat".
       DATA DIVISION.
       FILE SECTION.
       COPY "COPY-SORT.cpy".
       FD  LISTADO
           LINAGE IS 60 LINES
           with FOOTING AT 50
           lines at top 3
           lines at BOTTOM 3.
       01  lis-reg pic x(80).

       WORKING-STORAGE SECTION.
       01  cabecera1.
           03 filler       pic x(2).
           03 filler       pic x(57) value "LISTADO DE MOVIMIENTOS DE
      -         "MATERIAS APROBADAS DE ALUMNOS".
           03 filler       pic x(5)  value spaces.
           03 filler       pic x(13) value "NRO. DE PAG. ".
           03 l-nro-pag    pic 99.
           03 filler       pic x(3).
       01  cabecera2.
           03 filler       pic x(80) value all "-".
       01  cabecera3.
           03 filler       pic x(10) value spaces.
           03 filler       pic x(6)  value "LEGAJO".
           03 filler       pic x(2)  value spaces.
           03 filler       pic x(5)  value "CUAT:".
           03 filler       pic x(2)  value spaces.
           03 filler       pic x(7)  value "MATERIA".
           03 filler       pic x(3) value spaces.
           03 filler       pic x(5)  value "CURSO".
           03 filler       pic x(3) value spaces.
           03 filler       pic x(4)  value "NOTA".
           03 filler       pic x(10) value spaces.
       01  cabecera4.
           03 filler       pic x(10) value spaces.
           03 filler       pic x(6)  value all "-".
           03 filler       pic x(2)  value spaces.
           03 filler       pic x(5)  value all "-".
           03 filler       pic x(2)  value spaces.
           03 filler       pic x(7)  value all "-".
           03 filler       pic x(3) value spaces.
           03 filler       pic x(5)  value all "-".
           03 filler       pic x(3) value spaces.
           03 filler       pic x(4)  value all "-".
           03 filler       pic x(10) value spaces.
       01  detalle.
           03 filler       pic x(10) value spaces.
           03 l-leg        pic z(5)  value spaces.
           03 filler       pic x(5)  value spaces.
           03 l-cuat     pic zz.
           03 filler       pic x(4)  value spaces.
           03 l-mat  pic x(2).
           03 filler       pic x(8) value spaces.
           03 l-curso     pic x.
           03 filler       pic x(7)  value spaces.
           03 l-nota    pic zz.
           03 filler       pic x(5)  value spaces.
       01  w-flag-sort pic 9.
       01  w-cont-paginas PIC 99.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            SORT SORT-ALU ASCENDING srt-legajo srt-cuatri srt-materia
            INPUT PROCEDURE IS ENTRADA-TECLADO
            OUTPUT PROCEDURE IS SALIDA-LISTADO.
            STOP RUN.
       ENTRADA-TECLADO.
           PERFORM 100-INICIO.
           PERFORM 200-INGRESO-LEGAJO.
           PERFORM UNTIL srt-legajo=0
               PERFORM 230-INGRESO-RESTO
               PERFORM 300-GRABAR-SORT
               PERFORM 200-INGRESO-LEGAJO
           END-PERFORM.
       100-INICIO.
       200-INGRESO-LEGAJO.
           DISPLAY "Ingrese nro de legajo 0 x fin".
           ACCEPT srt-legajo.
       230-INGRESO-RESTO.
           DISPLAY "Ingrese cuatrimestre".
           ACCEPT srt-cuatri.
           DISPLAY "Ingrese materia".
           ACCEPT srt-materia.
           DISPLAY "Ingrese Curso".
           ACCEPT srt-curso.
           DISPLAY "Ingrese Nota".
           ACCEPT srt-nota.
       300-GRABAR-SORT.
           RELEASE srt-reg.
       SALIDA-LISTADO.
           PERFORM 500-INICIO-LISTADO.
           PERFORM 600-LEER-SORT.
           PERFORM UNTIL w-flag-sort=1
               PERFORM 700-MOSTRAR-SORT
               PERFORM 600-LEER-SORT
           END-PERFORM.
           PERFORM 800-FIN-SORT.
       500-INICIO-LISTADO.
           OPEN OUTPUT LISTADO.
           PERFORM 520-LISTAR-ENCABEZADO.
       520-LISTAR-ENCABEZADO.
           ADD 1 TO w-cont-paginas.
           MOVE w-cont-paginas to l-nro-pag.
           IF w-cont-paginas = 1
               WRITE lis-reg FROM cabecera1
               DISPLAY lis-reg
           ELSE
               WRITE lis-reg FROM cabecera1 AFTER ADVANCING PAGE
           END-IF.
           WRITE lis-reg FROM cabecera2 AFTER 2.
           DISPLAY lis-reg
           WRITE lis-reg FROM cabecera3 AFTER 2.
           DISPLAY lis-reg.
           WRITE lis-reg FROM cabecera4.
           DISPLAY lis-reg.
       600-LEER-SORT.
           RETURN SORT-ALU AT END MOVE 1 TO w-flag-sort.
       700-MOSTRAR-SORT.
           IF LINAGE-COUNTER = 56
               PERFORM 520-LISTAR-ENCABEZADO.
           PERFORM 0453-GENERAR-LINEA.
           PERFORM 0459-LISTAR-DETALLE.
       0453-GENERAR-LINEA.
           MOVE srt-legajo TO l-leg.
           MOVE srt-materia TO l-mat.
           MOVE srt-cuatri TO l-cuat.
           MOVE srt-curso TO l-curso.
           MOVE srt-nota TO l-nota.
       0459-LISTAR-DETALLE.
           WRITE lis-reg FROM detalle AFTER ADVANCING 1 LINE
               AT END-OF-PAGE PERFORM 520-LISTAR-ENCABEZADO.
           DISPLAY lis-reg.
       800-FIN-SORT.
           CLOSE LISTADO.
       END PROGRAM SORT-ALU.
