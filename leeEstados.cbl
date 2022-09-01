      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION. SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ESTADOS ASSIGN TO "..\estados.dat"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS SEQUENTIAL
                   RECORD KEY IS es-llave
                   ALTERNATE RECORD KEY IS es-sec WITH DUPLICATES.
       DATA DIVISION.
       FILE SECTION.
       FD  ESTADOS.
       01  es-reg.
           03 es-llave.
               05 es-dni pic 9(8).
               05 es-materia pic x(2).
               05 es-sec.
                   07 es-cuatri pic 99.
                   07 es-curso pic x.
                   07 es-nota pic 99.
                   07 es-secuen pic 9(3).
       WORKING-STORAGE SECTION.
       77  sen pic 9 value 0.
       01  lin-cabecera.
           03 filler pic x(4) value "DNI:".
           03 filler pic x(8) value spaces.
           03 filler pic x(4) value "MAT:".
           03 filler pic x(2) value spaces.
           03 filler pic x(5) value "CUAT:".
           03 filler pic x(1) value spaces.
           03 filler pic x(6) value "CURSO:".
           03 filler pic x(1) value spaces.
           03 filler pic x(5) value "NOTA:".
           03 filler pic x(2) value spaces.
           03 filler pic x(4) value "SEC:".
           03 filler pic x(4) value spaces.
       01  lin-guarda.
           03 filler pic x(80) value all "-".
       01  lin-detalle.
           03 l-dni pic zz.zzz.zz9 value spaces.
           03 filler pic x(2) value spaces.
           03 l-mat pic x(2) value spaces.
           03 filler pic x(4) value spaces.
           03 l-cuat pic z9 value spaces.
           03 filler pic x(6) value spaces.
           03 l-curso pic x.
           03 filler pic x(5) value spaces.
           03 l-nota pic Z9.
           03 filler pic x(6) value spaces.
           03 l-sec pic zz9.
           03 filler pic x(5) value spaces.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 100-INICIO-LECTURA.
           PERFORM 200-LEE-ARCH-ESTADOS.
           PERFORM UNTIL sen is equal 1
               PERFORM 300-PROCESO-LECTURA
               PERFORM 200-LEE-ARCH-ESTADOS
           END-PERFORM.
           PERFORM 400-FIN-LECTURA.
            STOP RUN.
        100-INICIO-LECTURA.
           PERFORM 130-ABRIR-ARCHIVOS.
           PERFORM 150-LISTAR-ENCABEZADO.

       130-ABRIR-ARCHIVOS.
           OPEN INPUT ESTADOS.

       150-LISTAR-ENCABEZADO.
           DISPLAY lin-guarda.
           DISPLAY lin-cabecera.
           DISPLAY lin-guarda.

       200-LEE-ARCH-ESTADOS.
           READ ESTADOS at end move 1 to sen.

       300-PROCESO-LECTURA.
           move es-dni to l-dni.
           move es-materia to l-mat.
           move es-cuatri to l-cuat.
           move es-curso to l-curso.
           move es-nota to l-nota.
           move es-secuen to l-sec.
           DISPLAY lin-detalle.

       400-FIN-LECTURA.

           CLOSE ESTADOS.
       END PROGRAM YOUR-PROGRAM-NAME.
