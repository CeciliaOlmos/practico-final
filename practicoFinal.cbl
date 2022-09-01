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
           SELECT SORT-ALU
           ASSIGN TO  "..\impOrden.txt"
               organization is SEQUENTIAL.

           SELECT ALUMNOS ASSIGN to "..\alumnos.dat"
               ORGANIZATION is RELATIVE
               ACCESS MODE IS dynamic
               RECORD key is rel-alu.

           SELECT ESTADOS ASSIGN TO "..\estados.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD key IS es-llave
               ALTERNATE RECORD key is es-sec WITH DUPLICATES
               ALTERNATE RECORD KEY IS es-secuen WITH DUPLICATES.

           SELECT MATERIAS
           ASSIGN TO
           "..\materias.txt"
           ORGANIZATION is line SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  SORT-ALU.
       01  srt-reg.
           03 srt-legajo pic 9(5).
           03 srt-cuatri pic 9(2).
           03 srt-materia pic x(2).
           03 srt-curso pic x.
           03 srt-nota pic 99.

       FD  ESTADOS.
       01  es-reg.
           03 es-llave.
               05 es-dni pic 9(8).
               05 es-materia pic x(2).
               05 es-sec.
                   07 es-cuatri pic 99.
                   07 es-curso pic x.
                   07 es-secuen pic 9(3).
           03 es-nota pic 99.

       FD  ALUMNOS.
       01  alu-reg.
           03 al-legajo pic 9(5).
           03 al-dni pic 9(8).
           03 al-puntero pic 9(4).

       FD  MATERIAS.
       01  mat-reg.
           03 mat-cuat pic 99.
           03 mat-mat pic x(2).

       WORKING-STORAGE SECTION.
       01  w-flag-sort pic 9.
       01  w-flag-alu pic 9.
       01  w-flag-est pic 9.
       01  w-alu-ant pic 9(5).
       01  rel-alu pic 9(5).
       01  w-resto PIC 9(5).
       77  w-flag-mat pic 9.
           88 fin-archivo value 1.
       01  w-resul pic 999.
       01  i pic 99.
       01  j pic 999.
       01  w-cont-cuat pic 9.
       01  w-max-cuat pic 99 value zero.
       01  tabla.
           03 tab-materias  OCCURS 48 TIMES.
               05 vec-cuatri  pic 99.
               05 vec-mater pic x(2).
       01  tabla-sec.
           03 tab-nro-sec OCCURS 999 times.
               05 vec-nro-sec pic 9(3).
       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           PERFORM 100-INICIO.
           PERFORM 300-LEER-SORT.
           perform 320-INICIO-SORT.
           perform until w-flag-sort is =1
              perform 400-INICIO-ALUMNOS
               perform until w-flag-sort is =1 OR
               w-alu-ant IS NOT= srt-legajo
                       PERFORM 700-PROCESO-ALU
                       PERFORM 300-LEER-SORT
               END-PERFORM
               PERFORM 800-FIN-ALUMNOS
           end-perform.
           PERFORM 1000-FIN-GENERAL.
            STOP RUN.
       100-INICIO.
           PERFORM 200-CARGO-VECTOR.
           PERFORM 205-POSICIONAR-ARCH.
           PERFORM 280-CARGAR-SEC.
           PERFORM 120-ABRIR-ARCHIVOS.

       120-ABRIR-ARCHIVOS.
           OPEN INPUT ALUMNOS.
           OPEN INPUT SORT-ALU.
           OPEN I-O ESTADOS.

       200-CARGO-VECTOR.
           PERFORM 210-INICIO-MATERIAS.
           PERFORM 220-LEER-ARCH-MAT.
           MOVE 1 to i.
            PERFORM UNTIL fin-archivo
               MOVE mat-cuat to vec-cuatri(i)
               move mat-mat to vec-mater(i)
               ADD 1 TO i
               PERFORM 220-LEER-ARCH-MAT
            END-PERFORM.
            PERFORM 230-FIN-VECTOR.
       210-INICIO-MATERIAS.
           OPEN INPUT MATERIAS.
       220-LEER-ARCH-MAT.
           READ MATERIAS at end move 1 to w-flag-mat.
       230-FIN-VECTOR.
           CLOSE MATERIAS.

       205-POSICIONAR-ARCH.
           MOVE 100 TO es-secuen.
           START ESTADOS key is > es-secuen
               INVALID key
                   DISPLAY "no encontre"
               not INVALID key
                   PERFORM 280-CARGAR-SEC.
       280-CARGAR-SEC.
           PERFORM 600-LEER-ESTADO.
           PERFORM VARYING j from 1 by 1 until j >999 OR w-flag-est=1
               MOVE es-secuen to vec-nro-sec(j)
               DISPLAY vec-nro-sec(j)
               PERFORM 600-LEER-ESTADO
           END-PERFORM.
       300-LEER-SORT.
           READ SORT-ALU AT END MOVE 1 TO w-flag-sort.
       320-INICIO-SORT.
           MOVE srt-legajo to w-alu-ant.
       400-INICIO-ALUMNOS.
           PERFORM 410-INICIALIZAR-VARIBLES.
           PERFORM 420-OBTENER-POSICION.
           PERFORM 440-LEER-ALUMNO.
           PERFORM 445-BUSCAR-DNI.
       410-INICIALIZAR-VARIBLES.
           MOVE ZERO TO w-cont-cuat.
           MOVE ZERO TO w-max-cuat.
       420-OBTENER-POSICION.
           PERFORM 430-FUNCION-HASHING.
       430-FUNCION-HASHING.
           DIVIDE 2377 INTO w-alu-ant GIVING w-resul REMAINDER rel-alu.
           add 1 to rel-alu.
           MOVE rel-alu to w-resto.
           read ALUMNOS.

       440-LEER-ALUMNO.
           START ALUMNOS KEY IS EQUAL rel-alu
               INVALID KEY
               DISPLAY "EXPLOTO TODO"
               NOT INVALID KEY
                PERFORM 445-BUSCAR-DNI.

       445-BUSCAR-DNI.
            READ ALUMNOS

                if al-legajo=srt-legajo
                    PERFORM 450-BUSCAR-ALU-EN-ESTADO
                else
                    PERFORM 447-BUSCAR-SINONIMO.

       450-BUSCAR-ALU-EN-ESTADO.
           PERFORM 500-ARMAR-CLAVE.

       447-BUSCAR-SINONIMO.
           PERFORM UNTIL al-legajo=srt-legajo OR al-puntero IS =0
               MOVE al-puntero TO rel-alu
               PERFORM 440-LEER-ALUMNO
           END-PERFORM.
               DISPLAY al-dni.
           PERFORM 450-BUSCAR-ALU-EN-ESTADO.
       500-ARMAR-CLAVE.
           MOVE al-dni TO es-dni.
           MOVE srt-materia TO es-materia.
           MOVE srt-cuatri TO es-cuatri.
           PERFORM 510-POS-EN-ESTADO.

       510-POS-EN-ESTADO.
           START ESTADOS KEY IS = es-llave
               INVALID KEY
                   PERFORM 590-GENERAR-NUEVO
               NOT INVALID KEY
                   PERFORM 570-CARGAR-ESTADO.

       570-CARGAR-ESTADO.
           PERFORM 600-LEER-ESTADO.
           PERFORM 610-CAMBIAR-NOTA.

       590-GENERAR-NUEVO.
           MOVE srt-curso TO es-curso.
           MOVE srt-nota TO es-nota.
           add 1 to j.
           MOVE j to es-secuen.
           WRITE es-reg.

       600-LEER-ESTADO.
           READ ESTADOS at end move 1 to w-flag-est.

       610-CAMBIAR-NOTA.
           MOVE srt-nota TO es-nota.
           REWRITE es-reg.
       700-PROCESO-ALU.
           PERFORM 710-CALCULO-MAX.


       710-CALCULO-MAX.
           if srt-cuatri >= w-max-cuat
               move srt-cuatri to w-max-cuat
               add 1 to w-cont-cuat
           else
               move zero to w-cont-cuat
           END-IF.

       800-FIN-ALUMNOS.
           PERFORM 830-EVALUO-REGISTRO.

       830-EVALUO-REGISTRO.
           IF w-cont-cuat >=4
               PERFORM 850-AGREGAR-REGISTRO.


       850-AGREGAR-REGISTRO.
           ADD 1 TO es-cuatri.
           MOVE vec-cuatri(es-cuatri) TO es-cuatri.
           MOVE vec-mater(es-cuatri) TO es-materia.
           WRITE es-reg.

       1000-FIN-GENERAL.
           CLOSE ALUMNOS SORT-ALU ESTADOS.

       END PROGRAM YOUR-PROGRAM-NAME.
