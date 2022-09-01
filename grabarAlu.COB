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
           SELECT ALUMNOS ASSIGN to "..\alumnos.dat"
               ORGANIZATION is RELATIVE
               ACCESS MODE IS SEQUENTIAL
               RECORD key is rel-alu.

       DATA DIVISION.
       FILE SECTION.
       FD  ALUMNOS.
       01  alu-reg.
           03 al-legajo pic 9(5).
           03 al-dni pic 9(8).
           03 al-puntero pic 9(4).

       WORKING-STORAGE SECTION.
       01  rel-alu pic 9(4).
       77  i pic 9(4).
           88 sala value 2378.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN OUTPUT ALUMNOS.
           perform varying i from 1 by 1 until i > 2500
               MOVE 0 TO al-dni
               move 0 TO al-legajo
               if sala
                   move 2379 to al-puntero
               else
                   move 0 to al-puntero
               end-if
               write alu-reg INVALID KEY DISPLAY "no pude"
                             NOT INVALID KEY
                               DISPLAY "GRABACION EXITOSA"
           END-PERFORM.
           close ALUMNOS.
            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
