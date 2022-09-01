      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.SPECIAL-NAMES. DECIMAL-POINT is COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MATERIAS
           ASSIGN TO
           "..\materias.txt"
           ORGANIZATION is line SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  MATERIAS.
       01  mat-reg.
           03 mat-cuat pic 99.
           03 mat-mat pic x(2).
       WORKING-STORAGE SECTION.
       77  w-flag-mat pic 9.
           88 fin-archivo value 1.
       01  i pic 99.

       01  tabla.
           03 tab-materias  OCCURS 99 TIMES.
               05 mat-cuatri  pic 99.
               05 mat-mater pic x(2).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM 100-INICIO.
            PERFORM 200-LEER-ARCH.
            MOVE 1 to i.
            PERFORM UNTIL fin-archivo
               MOVE mat-cuat to mat-cuatri(i)
               move mat-mat to mat-mater(i)
               ADD 1 TO i
               PERFORM 200-LEER-ARCH
            END-PERFORM.
            PERFORM 700-FIN.
            STOP RUN.
       100-INICIO.
           OPEN INPUT MATERIAS.
       200-LEER-ARCH.
           READ MATERIAS at end move 1 to w-flag-mat.
       300-MOSTRAR.
           MOVE 1 TO i.
           PERFORM VARYING i FROM 1 BY 1 UNTIL i>49
               IF mat-cuatri(i)>0
                   DISPLAY mat-cuatri(i) mat-mater(i)

           END-PERFORM.

       700-FIN.
           PERFORM 300-MOSTRAR.
           CLOSE MATERIAS.
       END PROGRAM YOUR-PROGRAM-NAME.
