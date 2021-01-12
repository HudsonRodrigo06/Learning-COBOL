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

      *---- VARIÁVEIS ----*
       77  LETRAS              PIC A(20).
       77  ALPHANUMERICOS      PIC X(20).
       77  NUMEROS-INT         PIC 9(5).
       77  NUMEROS-DOUBLE      PIC 9(5)V9(2).
       77  NUMEROS-SINAL       PIC S9(5).
      *-------------------*

      *---- CONSTANTES ----*
       78  C_TOTAL             VALUE 100.
      * -------------------*

      *---- GRUPOS ----*
       01  CLIENTE.
           02  NOME            PIC A(20)   VALUE "HUDSON".
           02  RG              PIC X(11)   VALUE "999999999".
           02  IDADE           PIC 9(3)    VALUE 25.
           02  TOTAL           PIC 999     VALUE 10.
           02  CPF             PIC X(11)   VALUE "99999999999".
           02  ENDERECO        PIC X(20)   VALUE "RUA X".
           02  DTNASC.
               03  DIA         PIC 99      VALUE 11.
               03  MES         PIC 99      VALUE 01.
               03  ANO         PIC 9999    VALUE 2021.

       01  CARRO.
           02  MARCA           PIC X(8)    VALUE "HYUNDAI".
           02  COR             PIC X(10)   VALUE "VERMELHO".



      *================================================================*
       PROCEDURE                                               DIVISION.
      *================================================================*

      *     ADD 4 TO IDADE TOTAL OF CLIENTE

           DISPLAY "Cliente: " CLIENTE " Carro: " CARRO.
           STOP RUN.

       END PROGRAM VARIAVEIS-GRUPOS.
