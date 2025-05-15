       IDENTIFICATION DIVISION.
       PROGRAM-ID. ToDoList.
       AUTHOR.    Terry.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *Tableau comprenant les différentes tâches
       01  WS-TODO.
         03 WS-TASKS      OCCURS 10 TIMES.
           05 WS-ID       PIC 9(02).
           05 FILLER      PIC X VALUE SPACE.
           05 WS-TASK     PIC X(200).
           05 FILLER      PIC X(01) VALUE X"0A".
      *Variable pour la saisie utilisateur
       01  WS-INPUT       PIC 9(02).
      *Variable pour indexer le tableau
       77  WS-INDEX       PIC 9(02).

       PROCEDURE DIVISION.
      *Lancement du programme
           PERFORM 0100-MENU-START
           THRU    0100-MENU-END.


      ******************************************************************

       0100-MENU-START.
      *Menu principal
           PERFORM UNTIL WS-INPUT = 4
                   DISPLAY "Bienvenue dans votre TODO List !"
                   DISPLAY "Choisissez votre option"
                   DISPLAY "1- Ajouter une tâche"
                   DISPLAY "2- Afficher les tâches"
                   DISPLAY "3- Supprimer une tâche"
                   DISPLAY "4- Quitter le programme"
      *Saisie utilisateur
                   ACCEPT WS-INPUT
                   EVALUATE WS-INPUT
                       WHEN = 1
      *Paragraphe pour ajouter des tâches à la liste
                           PERFORM 0200-ADD-TASK-START
                           THRU    0200-ADD-TASK-END
                       WHEN = 2
      *Affiche le tableau complet
                           DISPLAY WS-TODO
                       WHEN = 3
      *Paragraphe qui supprime les tâches
                           PERFORM 0300-DELETE-TASK-START
                           THRU    0300-DELETE-TASK-END
                       WHEN = 4
      *Permet de quitter le programme
                           STOP RUN
                       WHEN OTHER 
      *Contrôle de saisie
                           PERFORM 0400-WRONG-INPUT-START
                           THRU    0400-WRONG-INPUT-END
                   END-EVALUATE
           END-PERFORM.

           EXIT.
       0100-MENU-END.

       0200-ADD-TASK-START.
      *Réinitialisation de l'index
           MOVE 0 TO WS-INDEX.
      *Boucle pour ajouter autant de tâche que nécessaire
           PERFORM UNTIL WS-INDEX > 10
                DISPLAY "Choisissez la tâche à assigner (1-10)"
                DISPLAY "11 pour quitter"
      *Choix utilisateur de la tâche
                ACCEPT WS-INDEX
      *Condition de poursuite pour rester dans le tableau
                IF WS-INDEX < 11 AND WS-INDEX NOT EQUAL 0
                DISPLAY "Quelle est votre tâche ?"
      *Saisie de la tâche par l'utilisateur
                ACCEPT WS-TASK(WS-INDEX)
      *Enregistrement de l'index correspondant
                MOVE WS-INDEX TO WS-ID(WS-INDEX)
                END-IF
           END-PERFORM.

           EXIT.
       0200-ADD-TASK-END.

       0300-DELETE-TASK-START.
      *Réinitialisation de l'index
           MOVE 0 TO WS-INDEX.
      *Boucle pour supprimer autant de tâche que nécessaire
           PERFORM UNTIL WS-INDEX > 10
                DISPLAY "Choisissez la tâche à supprimer (1-10)"
                DISPLAY "11 pour quitter"
      *Choix utilisateur de la tâche
                ACCEPT WS-INDEX
                IF WS-INDEX < 11 AND WS-INDEX NOT EQUAL 0
      *Réinitialisation de la tâche et de l'index
                MOVE 0 TO WS-ID(WS-INDEX)
                MOVE SPACE TO WS-TASK(WS-INDEX)
                END-IF
           END-PERFORM.

           EXIT.
       0300-DELETE-TASK-END.

       0400-WRONG-INPUT-START.
           DISPLAY "Je n'ai pas compris votre demande".

           EXIT.
       0400-WRONG-INPUT-END.



