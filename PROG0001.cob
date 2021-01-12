      *================================================================*
       IDENTIFICATION                                          DIVISION.
      *================================================================*
       PROGRAM-ID. VARIAVEIS-GRUPOS.

      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************

      *================================================================*
       DATA                                                    DIVISION.
      *================================================================*
      *----------------------------------------------------------------*
       FILE                                            SECTION.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       WORKING-STORAGE                                 SECTION.
      *----------------------------------------------------------------*
      *-------recebendo retorno em uma string auxiliar-----------------*
       01 WS-DATETIME              PIC X(21).
      *------formatado pegando partes da string por indice-----*
       01 WS-FORMATTED-DT.
           05 WS-FORMATTED-DTE-TME.
               15 WS-FORMATTED-DY    PIC  9(2).
               15 FILLER             PIC X VALUE '-'.
               15 WS-FORMATTED-MONTH PIC  9(2).
               15 FILLER             PIC X VALUE '-'.
               15 WS-FORMATTED-YEAR  PIC  9(4).
               15 FILLER             PIC X VALUE '-'.

               15 WS-FORMATTED-HOUR  PIC  9(2).
               15 FILLER             PIC X VALUE ':'.
               15 WS-FORMATTED-MINS  PIC  9(2).
               15 FILLER             PIC X VALUE ':'.
               15 WS-FORMATTED-SEC   PIC  9(2).
               15 FILLER             PIC X VALUE ':'.
               15 WS-FORMATTED-MS    PIC  9(2).
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                                               DIVISION.
      *================================================================*

      *-------- move resultado para a "string":
               MOVE FUNCTION CURRENT-DATE TO WS-DATETIME.

      *-------- pega "pedaços da "string" por indice:
               MOVE WS-DATETIME(1:4)  TO WS-FORMATTED-YEAR.
               MOVE WS-DATETIME(5:2)  TO WS-FORMATTED-MONTH.
               MOVE WS-DATETIME(7:2)  TO WS-FORMATTED-DY.
               MOVE WS-DATETIME(9:2)  TO WS-FORMATTED-HOUR.
               MOVE WS-DATETIME(11:2) TO WS-FORMATTED-MINS.
               MOVE WS-DATETIME(13:2) TO WS-FORMATTED-SEC.
               MOVE WS-DATETIME(15:2) TO WS-FORMATTED-MS.


               DISPLAY WS-DATETIME.
               DISPLAY WS-FORMATTED-DT.

           STOP RUN.

       END PROGRAM VARIAVEIS-GRUPOS.
