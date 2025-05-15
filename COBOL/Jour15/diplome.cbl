       IDENTIFICATION DIVISION.
       PROGRAM-ID. diplome.
       AUTHOR.    Terry.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT FICHIER-ELEVES ASSIGN TO "eleves.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

       SELECT FICHIER-REUSSITE ASSIGN TO "reussite.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD FICHIER-ELEVES.
       01 F-ELEVES.
           05 F-NOM     PIC X(10).
           05 F-NOTE    PIC 9(02).

       FD FICHIER-REUSSITE.
       01 F-REUSSITE.
           05 F-NOM-REU     PIC X(10).
           05 F-NOTE-REU    PIC 9(02).

       WORKING-STORAGE SECTION.
       
       01 WS-TABLE-ELEVES.
         03 WS-ELEVES OCCURS 15 TIMES.
           05 WS-NOM    PIC X(10).
           05 WS-NOTE   PIC 9(02).

       77  WS-INDEX     PIC 9(02) VALUE 1.
       77  WS-MAX-INDEX PIC 9(02) VALUE 15.

       77  WS-FIN-F     PIC X     VALUE "N".

       PROCEDURE DIVISION.
           
           PERFORM 0100-READ-START
           THRU    0100-READ-END.
           
           PERFORM 0200-OUTPUT-REUSSITE-START
           THRU    0200-OUTPUT-REUSSITE-END.

           STOP RUN.
      ******************************************************************
       0100-READ-START.
           OPEN INPUT FICHIER-ELEVES.

           PERFORM UNTIL WS-FIN-F = "Y"
           
               READ FICHIER-ELEVES
                   AT END
                       MOVE "Y" TO WS-FIN-F
                   NOT AT END
                       IF WS-INDEX <= WS-MAX-INDEX
                            MOVE F-NOM  TO WS-NOM(WS-INDEX)
                            MOVE F-NOTE TO WS-NOTE(WS-INDEX)
                            ADD 1 TO WS-INDEX
                       END-IF
               END-READ
           END-PERFORM.

           CLOSE FICHIER-ELEVES.

           EXIT.
       0100-READ-END.

       0200-OUTPUT-REUSSITE-START.
           OPEN OUTPUT FICHIER-REUSSITE.

           PERFORM VARYING WS-INDEX FROM 1 BY 1
                       UNTIL WS-INDEX > WS-MAX-INDEX
                IF WS-NOTE(WS-INDEX) > 10
                    MOVE WS-NOM(WS-INDEX)  TO F-NOM-REU
                    MOVE WS-NOTE(WS-INDEX) TO F-NOTE-REU
                    WRITE F-REUSSITE
                END-IF
           END-PERFORM.

           CLOSE FICHIER-REUSSITE.

           EXIT.
       0200-OUTPUT-REUSSITE-END.













