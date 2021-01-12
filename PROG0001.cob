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

       77  WK-MAIOR PIC 999 VALUE ZEROS.
       77  WK-N1     PIC 999 VALUE ZEROS.
       77  WK-N2     PIC 999 VALUE ZEROS.
       77  WK-N3     PIC 999 VALUE ZEROS.
       77  WK-QTDE  PIC 999 VALUE ZEROS.

      *================================================================*
       PROCEDURE                                               DIVISION.
      *================================================================*

           ACCEPT WK-N1.
           ACCEPT WK-N2.
           ACCEPT WK-N3.

           IF WK-N1 > WK-MAIOR THEN
               MOVE WK-N1 TO WK-MAIOR
           END-IF.

           IF WK-N2 > WK-MAIOR THEN
               MOVE WK-N2 TO WK-MAIOR
           END-IF.

           IF WK-N3 > WK-MAIOR THEN
               MOVE WK-N3 TO WK-MAIOR
           END-IF.

           DISPLAY "Maior: " WK-MAIOR.

           STOP RUN.

       END PROGRAM VARIAVEIS-GRUPOS.
