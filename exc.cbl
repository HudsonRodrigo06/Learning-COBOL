      *================================================================*
       IDENTIFICATION DIVISION.
      *================================================================*

       PROGRAM-ID. MEU-PROGRAMA.

      *================================================================*
       ENVIRONMENT DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                                           SECTION.
      *----------------------------------------------------------------*

           SPECIAL-NAMES.
               DECIMAL-POINT IS COMMA.

      *----------------------------------------------------------------*
       INPUT-OUTPUT                                            SECTION.
      *----------------------------------------------------------------*

           FILE-CONTROL.

              SELECT CADFUNCA ASSIGN TO "C:\FILES\CADFUNCA.dat"
              FILE STATUS IS FS-CADFUNCA
              ORGANIZATION IS LINE SEQUENTIAL.

              SELECT CADMOVTO ASSIGN TO  "C:\FILES\CADMOVTO.dat"
              FILE STATUS IS FS-CADMOVTO
              ORGANIZATION IS LINE SEQUENTIAL.

              SELECT CADFUNCN ASSIGN TO  "C:\FILES\CADFUNCN.dat"
              FILE STATUS IS FS-CADFUNCN
              ORGANIZATION IS LINE SEQUENTIAL.

              SELECT RELATO ASSIGN TO  "C:\FILES\RELATORIO.dat"
              FILE STATUS IS FS-RELATO
              ORGANIZATION IS LINE SEQUENTIAL.

      *================================================================*
       DATA DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                                                    SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *                       FD E REGISTROS                           *
      *----------------------------------------------------------------*
           FD CADFUNCA
               BLOCK CONTAINS 0 RECORDS
               RECORD CONTAINS 80 CHARACTERS
               LABEL RECORD IS STANDARD
               RECORDING MODE IS F.

       01  REG-CADFUNCA.
           88 ENDOFFILE VALUE          HIGH-VALUE.
           05 CODFUN-A PIC 9(4)        VALUE ZEROS.
           05 NOME-A PIC X(35).
           05 FUNCA-A PIC X(20) .
           05 SALARIO-A PIC 9(04)V99.
           05 VENCTOS-A PIC 9(04)V99.
           05 DECTOS-A PIC 9(04)V99.
           05 FILLER PIC X(03).

           FD CADMOVTO
               BLOCK CONTAINS 0 RECORDS
               RECORD CONTAINS 80 CHARACTERS
               LABEL RECORD IS STANDARD
               RECORDING MODE IS F.

       01  REG-CADMOVTO.
               05 CODFUN-M PIC 9(04)       VALUE ZEROS.
               05 NOME-M PIC X(35).
               05 FUNCA-M PIC X(20).
               05 SALARIO-M PIC 9(04)V99.
               05 VENCTOS-M PIC 9(04)V99.
               05 DECTOS-M PIC 9(04)V99.
               05 TIPO-M PIC X(01).
               05 FILLER PIC X(02).

           FD  CADFUNCN
               BLOCK CONTAINS 0 RECORDS
               RECORD CONTAINS 80 CHARACTERS
               LABEL RECORD IS STANDARD
               RECORDING MODE IS F.

       01  REG-CADFUNCN.
               05 CODFUN-N PIC 9(4)        VALUE ZEROS.
               05 NOME-N PIC X(35).
               05 FUNCA-N PIC X(20).
               05 SALARIO-N PIC 9(04)V99.
               05 VENCTOS-N PIC 9(04)V99.
               05 DECTOS-N PIC 9(04)V99.
               05 FILLER PIC X(03).


      *----------------------------------------------------------------*
      *                FALTA CRIAR REGISTROS DO RELATORIO              *
      *----------------------------------------------------------------*

           FD  RELATO
               BLOCK CONTAINS 0 RECORDS
               RECORD CONTAINS 120 CHARACTERS
               LABEL RECORD IS STANDARD
               RECORDING MODE IS F.

       01  REG-CAB1.
           05 FILLER PIC X.

       01  REG-CAB2.
           05 FILLER PIC X.

       01  REG-TITULO.
           05 FILLER PIC X.

       01  REG-RELATO.
           05 FILLER PIC X.


      *----------------------------------------------------------------*
       WORKING-STORAGE                                         SECTION.
      *----------------------------------------------------------------*

       77  FS-CADFUNCA         PIC 9(02)   VALUE ZEROS.
       77  FS-CADMOVTO         PIC 9(02)   VALUE ZEROS.
       77  FS-CADFUNCN         PIC 9(02)   VALUE ZEROS.
       77  FS-RELATO           PIC 9(02)   VALUE ZEROS.
       77  WS-LER              PIC X(20)   VALUE "CAD".
       77  ID-OCORRENCIA       PIC X(30)   VALUE SPACES.
       77  WS-LIDOS            PIC 9(02)   VALUE ZEROS.
       77  WS-GRAVADOS         PIC 9(02)   VALUE ZEROS.
       77  WS-PAG              PIC 9(02)   VALUE ZEROS.

      *================================================================*
       PROCEDURE DIVISION.
      *================================================================*

           MAIN-PROCEDURE.
             PERFORM 10-ABRIR-ARQUIVOS.
             PERFORM 50-LER-ARQUIVOS UNTIL 9999 = CODFUN-A AND CODFUN-M.
             PERFORM 60-FINALIZAR.
             STOP RUN.

      *----------------------------------------------------------------*
           10-ABRIR-ARQUIVOS.
      *----------------------------------------------------------------*

               OPEN INPUT CADFUNCA.
               IF FS-CADFUNCA NOT EQUAL ZEROS
                   DISPLAY "--------------------------------"
                   DISPLAY "ERRO AO ABRIR O ARQUIVO CADFUNCA"
                   DISPLAY "FILE STATUS = " FS-CADFUNCA
                   DISPLAY "--------------------------------"
                   STOP RUN.

               OPEN INPUT CADMOVTO
               IF FS-CADMOVTO NOT EQUAL ZEROS
                   DISPLAY "--------------------------------"
                   DISPLAY "ERRO AO ABRIR O ARQUIVO CADFUNCA"
                   DISPLAY "FILE STATUS = " FS-CADMOVTO
                   DISPLAY "--------------------------------"
                   STOP RUN.

               OPEN OUTPUT CADFUNCN
               IF FS-CADFUNCN NOT EQUAL ZEROS
                   DISPLAY "--------------------------------"
                   DISPLAY "ERRO AO ABRIR O ARQUIVO CADFUNCA"
                   DISPLAY "FILE STATUS = " FS-CADFUNCN
                   DISPLAY "--------------------------------"
                   STOP RUN.

      *         PERFORM 20-LERCADFUNCA.
      *         PERFORM 30-LERCADMOVTO.

      *----------------------------------------------------------------*
           10-ABRIR-ARQUIVOS-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *                         LER ARQUIVOS                           *
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
           20-LERCADFUNCA.
      *----------------------------------------------------------------*

               READ CADFUNCA AT END SET ENDOFFILE TO TRUE.
               IF FS-CADFUNCA EQUAL 10
                   MOVE 9999 TO CODFUN-A
               ELSE
                   IF FS-CADFUNCA NOT EQUAL ZEROS
                       DISPLAY "ERRO AO LER ARQUIVO CADFUNCA = "
                       DISPLAY FS-CADFUNCA
                       MOVE 99 TO RETURN-CODE
                       STOP RUN
               END-IF.

      *----------------------------------------------------------------*
           20-LERCADFUNCA-FIM.     EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
           30-LERCADMOVTO.
      *----------------------------------------------------------------*

               READ CADMOVTO AT END SET ENDOFFILE TO TRUE.
               IF FS-CADMOVTO EQUAL 10
                   MOVE 9999 TO CODFUN-M
               ELSE
                   IF FS-CADMOVTO NOT EQUAL ZEROS
                       DISPLAY "ERRO AO LER O ARQUIVO CADMOVTO = "
                       DISPLAY FS-CADMOVTO
                       MOVE 99 TO RETURN-CODE
                       STOP RUN
               END-IF.

      *----------------------------------------------------------------*
           30-LERCADMOVTO-FIM.     EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *                          EXECUTAR AÇÕES                        *
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
           50-LER-ARQUIVOS.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *          VALIDA CONTEUDO VAZIO  & CONTROLE DE FLUXO            *
      *----------------------------------------------------------------*

               IF WS-LER EQUAL 'CAD'
                   IF CODFUN-A EQUAL ZEROS
                       PERFORM 20-LERCADFUNCA
                       UNTIL CODFUN-A NOT EQUAL ZEROS
                   END-IF

                   IF CODFUN-M EQUAL ZEROS
                       PERFORM 30-LERCADMOVTO
                       UNTIL CODFUN-M NOT EQUAL ZEROS
                   END-IF
               ELSE
                   MOVE 'CAD' TO WS-LER
                   IF CODFUN-M EQUAL ZEROS
                       PERFORM 30-LERCADMOVTO
                       UNTIL CODFUN-M NOT EQUAL ZEROS
                   END-IF
               END-IF.

               PERFORM 100-EXECUTAR-ACOES.

               IF FS-CADMOVTO NOT EQUAL 10
                   MOVE 'MOV' TO WS-LER
               ELSE
                   MOVE 9999 TO CODFUN-A
                   MOVE 9999 TO CODFUN-M
               END-IF.

      *----------------------------------------------------------------*
           50-LER-ARQUIVOS-FIM.       EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
           100-EXECUTAR-ACOES.
      *----------------------------------------------------------------*

               IF CODFUN-A EQUAL CODFUN-M
                   EVALUATE TIPO-M
                       WHEN 'A' PERFORM 200-ROTINA-A
                       WHEN 'E' PERFORM 400-LISTAR-EXCLUIDO-CAD
                       WHEN 'I' PERFORM 500-LISTAR-INVALIDO-MOV
                   END-EVALUATE
                   EXIT.

               IF CODFUN-A < CODFUN-M
                   PERFORM 300-ROTINA-I-CAD
               ELSE
                   IF TIPO-M NOT EQUALS 'A' AND 'E'
                       PERFORM 700-VALIDA-MOV
                   ELSE
                       PERFORM 500-LISTAR-INVALIDO-MOV
                   END-IF
               END-IF.

      *----------------------------------------------------------------*
           100-EXECUTAR-ACOES-FIM.     EXIT.
      *----------------------------------------------------------------*

           700-VALIDA-MOV.

               IF FS-CADMOVTO EQUAL 10 AND CODFUN-M EQUAL ZEROS
                   PERFORM 300-ROTINA-I-CAD
               ELSE
                   PERFORM 600-ROTINA-I-MOV
               END-IF.

           700-VALIDA-MOV-FIM.     EXIT.

      *----------------------------------------------------------------*
           300-ROTINA-I-CAD.
      *----------------------------------------------------------------*

               MOVE REG-CADFUNCA TO REG-CADFUNCN
               WRITE REG-CADFUNCN.

      *----------------------------------------------------------------*
           300-ROTINA-I-CAD-FIM.   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
           600-ROTINA-I-MOV.
      *----------------------------------------------------------------*

               MOVE CODFUN-M TO CODFUN-N
               MOVE NOME-M TO NOME-N
               MOVE FUNCA-M TO FUNCA-N
               MOVE SALARIO-M TO SALARIO-N
               MOVE VENCTOS-M TO VENCTOS-N
               MOVE  DECTOS-M TO DECTOS-N

               WRITE REG-CADFUNCN
               DISPLAY "INCLUIDO =" REG-CADFUNCN
               DISPLAY "REGISTRO INCLUIDO COM SUCESSO".

      *----------------------------------------------------------------*
           600-ROTINA-I-MOV-FIM.        EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
           200-ROTINA-A.
      *----------------------------------------------------------------*
               MOVE "ALTERACAO" TO ID-OCORRENCIA
               DISPLAY "ANTES = " REG-CADFUNCA

               MOVE CODFUN-M TO CODFUN-N
               MOVE NOME-M TO NOME-N
               MOVE FUNCA-M TO FUNCA-N
               MOVE SALARIO-M TO SALARIO-N
               MOVE VENCTOS-M TO VENCTOS-N
               MOVE  DECTOS-M TO DECTOS-N

               WRITE REG-CADFUNCN
               DISPLAY "ATUAL = " REG-CADFUNCN
               DISPLAY "ATUALIZACAO REALIZA COM EXITO".

      *----------------------------------------------------------------*
           200-ROTINA-A-FIM.       EXIT.
      *----------------------------------------------------------------*

      *------------------------------------------------------------------------
      *                            FINALIZACAO
      *------------------------------------------------------------------------
           60-FINALIZAR.
               CLOSE CADFUNCA CADMOVTO CADFUNCN RELATO

               IF FS-CADFUNCA NOT EQUAL ZEROS OR 10
                   DISPLAY "ERRO AO FECHAR CADFUNCA = " FS-CADFUNCA.
                   MOVE 99 TO RETURN-CODE
               STOP RUN.

               IF FS-CADMOVTO NOT EQUAL ZEROS OR 10
                   DISPLAY "ERRO AO FECHAR CADMOVTO =" FS-CADMOVTO.

               IF FS-CADFUNCN NOT EQUAL ZEROS OR 10
                   DISPLAY "ERRO AO FECHAR CADFUNCN =" FS-CADFUNCN.

               IF FS-RELATO NOT EQUAL ZEROS OR 10
                   DISPLAY "ERRO AO FECHAR RELATORIO =" FS-RELATO.

           60-FIM.     EXIT.
