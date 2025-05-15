       IDENTIFICATION DIVISION.
       PROGRAM-ID. Grades.
       AUTHOR. Terry.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01  G1    PIC 9(2) VALUE 12.
       01  G2    PIC 9(2) VALUE 09.
       01  G3    PIC 9(2) VALUE 17.
       01  G4    PIC 9(2) VALUE 08.
       01  G5    PIC 9(2) VALUE 20.
       01  G6    PIC 9(2) VALUE 14.
       01  G7    PIC 9(2) VALUE 06.

       01  MAXGRADE  PIC 9(2).
       01  MINGRADE  PIC 9(2).
       01  AVERAGE   PIC 9(2).

       01  CHOICE    PIC 9(1).

       PROCEDURE DIVISION.
           
           
      *Lancement du programme avec un menu principal
           PERFORM UNTIL CHOICE = 4
               DISPLAY "Bienvenue dans votre carnet de note,"
               DISPLAY "Que voulez-vous faire ?"
               DISPLAY "1- Afficher la moyenne de vos notes"
               DISPLAY "2- Afficher la note la plus basse"
               DISPLAY "3- Afficher la note la plus haute"
               DISPLAY "4- Quitter"
      *Choix de l'utilisateur
               ACCEPT CHOICE
      
           EVALUATE CHOICE
      *Calcul et affichage de la moyenne des variables G
             WHEN = 1
              MOVE FUNCTION MEAN(G1, G2, G3, G4, G5, G6, G7) TO AVERAGE
              DISPLAY "La moyenne de vos notes est : "
              DISPLAY AVERAGE
      *Recherche et affichage de la note la plus basse
             WHEN = 2
              MOVE FUNCTION MIN(G1, G2, G3, G4, G5, G6, G7) TO MINGRADE
              DISPLAY "Votre note la plus basse est : "
              DISPLAY MINGRADE
      *Recherche et affichage de la note la plus haute
             WHEN = 3
              MOVE FUNCTION MAX(G1, G2, G3, G4, G5, G6, G7) TO MAXGRADE
              DISPLAY "Votre note la plus haute est : "
              DISPLAY MAXGRADE
             WHEN OTHER
                   CONTINUE
           END-EVALUATE
           END-PERFORM.

           STOP RUN.
