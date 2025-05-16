       IDENTIFICATION DIVISION.
       PROGRAM-ID. rendu.
       AUTHOR.    Terry.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT FICHIER-RENDU ASSIGN TO "compte-rendu.txt"
                                     ORGANIZATION IS LINE SEQUENTIAL.

       SELECT FICHIER-COPIE ASSIGN TO "copie.txt"
                                     ORGANIZATION IS LINE SEQUENTIAL.
    
       DATA DIVISION.
       FILE SECTION.

       FD FICHIER-RENDU.
       01  F-LIGNE-RENDU.
           05 F-LIGNE-IN  PIC X(50).

       FD FICHIER-COPIE.
       01  F-LIGNE-COPIE.
           05 F-LIGNE-OUT PIC X(50).

       WORKING-STORAGE SECTION.
       
       01  WS-LIGNE-STORAGE OCCURS 29 TIMES.
           05 WS-LIGNE    PIC X(50).

       77  WS-INDEX       PIC 9(02) VALUE  1.
       77  WS-MAX-LIGNE   PIC 9(02) VALUE 29.

       77  WS-FIN-FICHIER PIC X(01) VALUE "N".

       PROCEDURE DIVISION.
       
           OPEN INPUT FICHIER-RENDU.

           PERFORM UNTIL WS-FIN-FICHIER = "Y"

               READ FICHIER-RENDU
                   AT END
                       MOVE "Y" TO WS-FIN-FICHIER
                   NOT AT END
                       IF WS-INDEX <= WS-MAX-LIGNE
                           MOVE F-LIGNE-RENDU TO WS-LIGNE(WS-INDEX)
                           ADD 1 TO WS-INDEX
                       END-IF
      
               END-READ
           END-PERFORM.

           CLOSE FICHIER-RENDU.

           OPEN OUTPUT FICHIER-COPIE.

           PERFORM VARYING WS-INDEX FROM 1 BY 1
                               UNTIL WS-INDEX > WS-MAX-LIGNE
                       IF WS-LIGNE(WS-INDEX) NOT EQUAL SPACE
                           MOVE WS-LIGNE(WS-INDEX) TO F-LIGNE-COPIE
                           WRITE F-LIGNE-COPIE
                       END-IF
           END-PERFORM.

           CLOSE FICHIER-COPIE.

           STOP RUN.

