       IDENTIFICATION DIVISION.
       PROGRAM-ID. bidim.
       AUTHOR.    Terry.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01  TAB-ELEVES.
           05 CLASSE OCCURS 2 TIMES.
               10 ELEVE OCCURS 6 TIMES.
                   15 NOM-ELEVE       PIC X(15).
                   15 PRENOM-ELEVE    PIC X(15).

       77  WS-IDX-1                   PIC 9(02).
       77  WS-IDX-2                   PIC 9(02).
       
       77  WS-MAX-TAB-1               PIC 9(01) VALUE 2.
       77  WS-MAX-TAB-2               PIC 9(02) VALUE 6.
       
       01  TAB-ELEVES-ED.
           05 CLASSE-ED OCCURS 2 TIMES.
               10 ELEVE-ED OCCURS 6 TIMES.
                   15 WS-SORTIE       PIC X(150).
      ******************************************************************

       PROCEDURE DIVISION.

           PERFORM VARYING WS-IDX-1 FROM 1 BY 1 
                                UNTIL WS-IDX-1 > WS-MAX-TAB-1
             PERFORM VARYING WS-IDX-2 FROM 1 BY 1
                                UNTIL WS-IDX-2 > WS-MAX-TAB-2

                DISPLAY "Veuillez renseigner le nom de l'élève."
                ACCEPT NOM-ELEVE(WS-IDX-1 WS-IDX-2)

                DISPLAY "Veuillez renseigner le prénom de l'élève."
                ACCEPT PRENOM-ELEVE(WS-IDX-1 WS-IDX-2)
             IF WS-IDX-1 = 1
                STRING  "Classe: CM1 | "
                        "Nom: " 
                        NOM-ELEVE(WS-IDX-1 WS-IDX-2)
                        " | Prénom: "
                        PRENOM-ELEVE(WS-IDX-1 WS-IDX-2)
                        INTO WS-SORTIE(WS-IDX-1 WS-IDX-2)
             ELSE STRING  "Classe: CM2 | "
                        "Nom: " 
                        NOM-ELEVE(WS-IDX-1 WS-IDX-2)
                        " | Prénom: "
                        PRENOM-ELEVE(WS-IDX-1 WS-IDX-2)
                        INTO WS-SORTIE(WS-IDX-1 WS-IDX-2)
             END-IF

             END-PERFORM
           END-PERFORM.

           PERFORM VARYING WS-IDX-1 FROM 1 BY 1 
                                UNTIL WS-IDX-1 > WS-MAX-TAB-1
             PERFORM VARYING WS-IDX-2 FROM 1 BY 1
                                UNTIL WS-IDX-2 > WS-MAX-TAB-2
                DISPLAY WS-SORTIE(WS-IDX-1 WS-IDX-2)
             END-PERFORM
           END-PERFORM.

           STOP RUN.
