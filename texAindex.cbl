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
           SELECT ESTA ASSIGN TO "..\estados.txt"
           ORGANIZATION LINE SEQUENTIAL.
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
       FD  ESTA.
       01  es-tex-reg.
           03 es-tex-dni pic 9(8).
           03 es-tex-materia pic x(2).
           03 es-tex-cuatri pic 99.
           03 es-tex-curso pic x.
           03 es-tex-nota pic 99.
           03 es-tex-secuen pic 9(3).
       WORKING-STORAGE SECTION.
       77  w-flag-esta pic 9.
           88 fin-archivo value 1.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 100-INICIO.
           PERFORM 200-LEER-ARCH-FACT.
           PERFORM UNTIL fin-archivo
            PERFORM 300-PROCESO
            PERFORM 200-LEER-ARCH-FACT
           END-PERFORM.
           PERFORM 400-FIN.
            STOP RUN.
        100-INICIO.
           OPEN INPUT ESTA.
           OPEN OUTPUT ESTADOS.
       200-LEER-ARCH-FACT.
           READ ESTA AT END MOVE 1 TO w-flag-esta.
       300-PROCESO.
           move es-tex-dni to es-dni.
           move es-tex-materia to es-materia.
           move es-tex-cuatri to es-cuatri.
           move es-tex-curso to es-curso.
           move es-tex-nota to es-nota.
           move es-tex-secuen to es-secuen.
           write es-reg.
           DISPLAY es-reg.
       400-FIN.
           CLOSE ESTA.
           CLOSE ESTADOS.
       END PROGRAM YOUR-PROGRAM-NAME.
