       IDENTIFICATION DIVISION.
       PROGRAM-ID. bidim2.
       AUTHOR.    Terry.

      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT FICHIER-CLASSE ASSIGN TO "input-classes.txt"
       ORGANIZATION IS LINE SEQUENTIAL.

      ******************************************************************
       DATA DIVISION.
       FILE SECTION.

       FD FICHIER-CLASSE.
       01 TABLE-CLASSE.
           05 ELEVE.
               15 CLASSE            PIC X(03).
               15 FILLER            PIC X(02).
               15 NOM               PIC X(09).
               15 PRENOM            PIC X(09).

       WORKING-STORAGE SECTION.
       
       01 WS-TABLE-CLASSE.
           05 WS-CLASSE OCCURS 2 TIMES INDEXED BY WS-IDX-CLASSE.
               10 WS-ELEVE OCCURS 8 TIMES.
                   15 WS-NOM-CLASSE PIC X(03).
                   15 FILLER        PIC X(02).
                   15 WS-NOM        PIC X(09).
                   15 WS-PRENOM     PIC X(09).
      D            15 FILLER        PIC X VALUE X"0A".

       77  WS-IDX-ELEVE             PIC 9(02).
       77  WS-IDX-ELEVE1            PIC 9(02).
       77  WS-IDX-ELEVE2            PIC 9(02).

       77  WS-MAX-CLASSE            PIC 9(01) VALUE 2.
       77  WS-MAX-ELEVE             PIC 9(01) VALUE 8.

       77  WS-FIN                   PIC X VALUE "N".

       01  WS-SAISIE                PIC X(09).

      ******************************************************************
       PROCEDURE DIVISION.
       
           PERFORM 0100-READ-START
           THRU    0100-READ-END.

           PERFORM 0200-SAISIE-AFFICHAGE-START
           THRU    0200-SAISIE-AFFICHAGE-END.


           STOP RUN.
       
      ******************************************************************

       0100-READ-START.

           OPEN INPUT FICHIER-CLASSE.

           PERFORM UNTIL WS-FIN = "Y"
               READ FICHIER-CLASSE
                   AT END
                       MOVE "Y" TO WS-FIN
                   NOT AT END
                     IF CLASSE = "CM1"
                      MOVE 1 TO WS-IDX-CLASSE
                      ADD  1 TO WS-IDX-ELEVE1
                      MOVE WS-IDX-ELEVE1 TO WS-IDX-ELEVE
                     ELSE
                      MOVE 2 TO WS-IDX-CLASSE
                      ADD  1 TO WS-IDX-ELEVE2
                      MOVE WS-IDX-ELEVE2 TO WS-IDX-ELEVE
                     END-IF

                      MOVE CLASSE TO 
                      WS-NOM-CLASSE(WS-IDX-CLASSE WS-IDX-ELEVE)
                      MOVE NOM    TO
                      WS-NOM(WS-IDX-CLASSE WS-IDX-ELEVE)
                      MOVE PRENOM TO
                      WS-PRENOM(WS-IDX-CLASSE WS-IDX-ELEVE)

                END-READ
           END-PERFORM.

           CLOSE FICHIER-CLASSE.

           EXIT.
       0100-READ-END.

       0200-SAISIE-AFFICHAGE-START.

           DISPLAY "Veuillez renseigner le nom de l'eleve recherche: ".
           ACCEPT WS-SAISIE.
           DISPLAY "Prénoms existants: ".
           PERFORM VARYING WS-IDX-CLASSE FROM 1 BY 1
                               UNTIL WS-IDX-CLASSE > WS-MAX-CLASSE
                PERFORM VARYING WS-IDX-ELEVE FROM 1 BY 1
                               UNTIL WS-IDX-ELEVE > WS-MAX-ELEVE
                    IF WS-NOM(WS-IDX-CLASSE WS-IDX-ELEVE) = WS-SAISIE
                     DISPLAY WS-PRENOM(WS-IDX-CLASSE WS-IDX-ELEVE)
                    END-IF
                END-PERFORM
           END-PERFORM.
           
           EXIT.
       0200-SAISIE-AFFICHAGE-END.
