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
               ACCESS MODE IS DYNAMIC
               RECORD key is rel-alu.
       DATA DIVISION.
       FILE SECTION.
       FD  ALUMNOS.
       01  alu-reg.
           03 al-legajo pic 9(5).
           03 al-dni pic 9(8).
           03 al-puntero pic 9(4).
       WORKING-STORAGE SECTION.
       01  rel-alu pic 9(2).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            OPEN OUTPUT ALUMNOS.
            CLOSE ALUMNOS.
            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
