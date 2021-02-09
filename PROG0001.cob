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

       77  WS-QTD	    PIC S9(004) VALUE ZEROES.
       77  WS-IND	    PIC S9(004) VALUE ZEROES.
       77  WS-CHAVE   PIC  X(001) VALUE SPACES.


      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                                               DIVISION.
      *================================================================*

           MAINLINE.
	          PERFORM 100-ROTINA-A.

	          PERFORM 200-ROTINA-B
                 VARYING WS-IND FROM 1 BY 1 UNTIL WS-IND > 10.

	          PERFORM 300-ROTINA-C WITH TEST AFTER
               VARYING WS-IND FROM 1 BY 1  UNTIL  WS-CHAVE = SPACES.

           DISPLAY    WS-QTD.
	          GOBACK.

           100-ROTINA-A.
	              ADD	   1	   TO	   WS-QTD.

           200-ROTINA-B.
	              ADD	   2	   TO	   WS-QTD.

           300-ROTINA-C.
	              ADD	   4	   TO	   WS-QTD.




       END PROGRAM VARIAVEIS-GRUPOS.
