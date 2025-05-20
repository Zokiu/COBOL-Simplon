       IDENTIFICATION DIVISION.
       PROGRAM-ID. bidim3.
       AUTHOR.    Terry.

      ******************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *Affichage Debug.
      *SOURCE-COMPUTER. DELL WITH DEBUGGING MODE.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      
      *On assigne un alias pour le fichier de sortie.
       SELECT FICHIER-TRI ASSIGN TO "liste-eleves"
       ORGANIZATION IS LINE SEQUENTIAL.

      ******************************************************************

       DATA DIVISION.
       FILE SECTION.
      *Structure d'écriture.
       FD FICHIER-TRI.
       01 TAB-ELEVES.
           05 CLASSE.
               10 ELEVE.
                   15 NOM-CLASSE      PIC X(06).
                   15 NOM-ELEVE       PIC X(15).
                   15 PRENOM-ELEVE    PIC X(15).
      
      ******************************************************************
       
       WORKING-STORAGE SECTION.
      *Groupe de variable avec tableau bidimensionnel.
       01 WS-TAB-ELEVES.
           05 WS-CLASSE OCCURS 2 TIMES.
               10 WS-ELEVE OCCURS 6 TIMES.
                  12 WS-ID.
                   15 WS-NOM-ELEVE    PIC X(15).
                   15 WS-PRENOM-ELEVE PIC X(15).
      *Index pour gérer la dimension Classe avec son max.
       77  WS-IDX-CLASSE              PIC 9(01).
       77  WS-MAX-CLASSE              PIC 9(01) VALUE 2.
      *Index pour gérer la dimension Eleve avec son max.
       77  WS-IDX-ELEVE               PIC 9(01).
       77  WS-MAX-ELEVE               PIC 9(01) VALUE 6.

      ******************************************************************

       PROCEDURE DIVISION.
    
      *Appel de paragraphe pour la 
           PERFORM 0100-SAISIE-DEB
           THRU    0100-SAISIE-FIN.

           PERFORM 0200-ECRITURE-DEB
           THRU    0200-ECRITURE-FIN.

           STOP RUN.

      ******************************************************************

       0100-SAISIE-DEB.
               
           PERFORM VARYING WS-IDX-CLASSE  FROM 1 BY 1
                                   UNTIL WS-IDX-CLASSE > WS-MAX-CLASSE
             PERFORM VARYING WS-IDX-ELEVE FROM 1 BY 1 
                                   UNTIL WS-IDX-ELEVE  > WS-MAX-ELEVE
                    DISPLAY "Saisir le nom de l'élève"
                            
                    ACCEPT WS-NOM-ELEVE(WS-IDX-CLASSE WS-IDX-ELEVE)
                    DISPLAY "Saisir le prénom de l'élève"
                    ACCEPT WS-PRENOM-ELEVE(WS-IDX-CLASSE WS-IDX-ELEVE)
             END-PERFORM

             SORT WS-ELEVE(WS-IDX-CLASSE) ASCENDING

           END-PERFORM.
      
      D    DISPLAY WS-TAB-ELEVES.
           EXIT.
       0100-SAISIE-FIN.

       0200-ECRITURE-DEB.
       
           OPEN OUTPUT FICHIER-TRI.
           
           PERFORM VARYING WS-IDX-CLASSE  FROM 1 BY 1
                                   UNTIL WS-IDX-CLASSE > WS-MAX-CLASSE
             PERFORM VARYING WS-IDX-ELEVE FROM 1 BY 1
                                   UNTIL WS-IDX-ELEVE  > WS-MAX-ELEVE
               IF WS-IDX-CLASSE = 1
                   MOVE "CM1 | "                                TO 
                       NOM-CLASSE
               ELSE
                   MOVE "CM2 | "                                TO
                       NOM-CLASSE
               END-IF

               MOVE WS-NOM-ELEVE(WS-IDX-CLASSE WS-IDX-ELEVE)    TO
                    NOM-ELEVE
               MOVE WS-PRENOM-ELEVE(WS-IDX-CLASSE WS-IDX-ELEVE) TO
                    PRENOM-ELEVE
                    
               WRITE TAB-ELEVES

             END-PERFORM
           END-PERFORM.
        
           
           CLOSE FICHIER-TRI.

           EXIT.
       0200-ECRITURE-FIN.
