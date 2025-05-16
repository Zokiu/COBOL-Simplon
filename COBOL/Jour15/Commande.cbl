       IDENTIFICATION DIVISION.
       PROGRAM-ID. Commande.
       AUTHOR.    Terry.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT FICHIER-CLIENT ASSIGN TO "clients.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

       SELECT FICHIER-COMMANDE ASSIGN TO "num-commandes.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD FICHIER-CLIENT.
       01  F-CLIENT.
           05 F-CLIENT-ID      PIC 9(02).
           05 FILLER           PIC X(06).
           05 F-NOM            PIC X(11).
           05 F-PRENOM         PIC X(08).

       FD FICHIER-COMMANDE.
       01  F-COMMANDE.
           05 F-COMMANDE-ID    PIC 9(02).
           05 FILLER           PIC X(01).
           05 F-ARTICLE        PIC 9(03).
           05 FILLER           PIC X(01).
           05 F-QUANTITE       PIC 9(04).

       WORKING-STORAGE SECTION.
       
       01  WS-CLIENT-TABLE.
           05 WS-CLIENT OCCURS 10 TIMES.
             10 WS-CLIENT-ID   PIC 9(08).
             10 WS-NOM         PIC X(11).
             10 WS-PRENOM      PIC X(08).
       
       77  WS-INDEX-CLIENT     PIC 9(02) VALUE 1.
       77  WS-MAX-CLIENT       PIC 9(02) VALUE 10.
       77  WS-FIN-F-CLIENT     PIC X     VALUE "N".

       01  WS-COMMANDE-TABLE.
           05 WS-COMMANDE OCCURS 17 TIMES.
             10 WS-COMMANDE-ID PIC 9(02).
             10 WS-ARTICLE     PIC 9(04).
             10 WS-QUANTITE    PIC 9(03).

       77  WS-INDEX-COMMANDE   PIC 9(02) VALUE 1.
       77  WS-MAX-COMMANDE     PIC 9(02) VALUE 17.
       77  WS-FIN-F-COMMANDE   PIC X     VALUE "N".

       


       PROCEDURE DIVISION.
       
           PERFORM 0100-READ-CLIENT-START
           THRU    0100-READ-CLIENT-END.

           PERFORM 0200-READ-COMMANDE-START
           THRU    0200-READ-COMMANDE-END.

           PERFORM 0300-OUTPUT-START
           THRU    0300-OUTPUT-END.

           STOP RUN.

      ******************************************************************
       
       0100-READ-CLIENT-START.

           OPEN INPUT FICHIER-CLIENT.

           PERFORM UNTIL WS-FIN-F-CLIENT = "Y"
           
            READ FICHIER-CLIENT
               AT END
                   MOVE "Y" TO WS-FIN-F-CLIENT
               NOT AT END
                   IF WS-INDEX-CLIENT <= WS-MAX-CLIENT
                      MOVE F-CLIENT-ID     TO 
                                      WS-CLIENT-ID(WS-INDEX-CLIENT)
                      MOVE F-NOM    TO 
                                      WS-NOM(WS-INDEX-CLIENT)
                      MOVE F-PRENOM TO 
                                      WS-PRENOM(WS-INDEX-CLIENT)
                      ADD 1 TO WS-INDEX-CLIENT
                   END-IF
             END-READ
           END-PERFORM.

           CLOSE FICHIER-CLIENT.

           EXIT.
       0100-READ-CLIENT-END.

       0200-READ-COMMANDE-START.

           OPEN INPUT FICHIER-COMMANDE.

           PERFORM UNTIL WS-FIN-F-COMMANDE = "Y"
           
             READ FICHIER-COMMANDE
                 AT END
                     MOVE "Y" TO WS-FIN-F-COMMANDE
                 NOT AT END
                     IF WS-INDEX-COMMANDE <= WS-MAX-COMMANDE
                        MOVE F-COMMANDE-ID        TO
                                    WS-COMMANDE-ID(WS-INDEX-COMMANDE)
                        MOVE F-ARTICLE            TO
                                    WS-ARTICLE(WS-INDEX-COMMANDE)
                        MOVE F-QUANTITE           TO
                                    WS-QUANTITE(WS-INDEX-COMMANDE)
                        ADD 1 TO WS-INDEX-COMMANDE
                     END-IF
             END-READ
           END-PERFORM.

           CLOSE FICHIER-COMMANDE.

           EXIT.
       0200-READ-COMMANDE-END.

       0300-OUTPUT-START.
           PERFORM VARYING WS-INDEX-CLIENT FROM 1 BY 1
                       UNTIL WS-INDEX-CLIENT > WS-MAX-CLIENT
                PERFORM VARYING WS-INDEX-COMMANDE FROM 1 BY 1
                            UNTIL WS-INDEX-COMMANDE > WS-MAX-COMMANDE
                    IF WS-CLIENT-ID(WS-INDEX-CLIENT) = 
                                     WS-COMMANDE-ID(WS-INDEX-COMMANDE)
                        DISPLAY WS-NOM(WS-INDEX-CLIENT)
                                  SPACE WITH NO ADVANCING
                        DISPLAY WS-ARTICLE(WS-INDEX-COMMANDE)
                                  SPACE WITH NO ADVANCING
                        DISPLAY WS-QUANTITE(WS-INDEX-COMMANDE)
                    END-IF
                END-PERFORM
           END-PERFORM.
    
           EXIT.
       0300-OUTPUT-END.

