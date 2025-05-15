       IDENTIFICATION DIVISION.
       PROGRAM-ID. ToDoL.
       AUTHOR. Terry.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01 WS-TASK1        PIC X(50).
       01 WS-TASK2        PIC X(50).
       01 WS-TASK3        PIC X(50).
       01 WS-TASK4        PIC X(50).
       01 WS-TASK5        PIC X(50).
       01 WS-TASKCHOICE   PIC 9.
       01 WS-CHOICE       PIC 9.
       01 WS-QUESTION     PIC X(25) VALUE "Quelle est votre tâche ?".

       PROCEDURE DIVISION.
      *Menu principal avec 4 comme choix de sortie
           PERFORM UNTIL WS-CHOICE = 4
                     DISPLAY "Bienvenue dans votre TODO List !"
                     DISPLAY "Choisissez votre option"
                     DISPLAY "1- Ajouter une tâche"
                     DISPLAY "2- Afficher les tâches"
                     DISPLAY "3- Supprimer une tâche"
                     DISPLAY "4- Quitter le programme"
      *Choix de l'utilisateur
                     ACCEPT   WS-CHOICE

                     EVALUATE WS-CHOICE
      *Choix 1 : Ajout d'une ou plusieurs tâches
                            WHEN = 1
                                   PERFORM 0100-ADDTASK-START
                                      THRU 0100-ADDTASK-END
      *Choix 2 : Affichage des tâches
                            WHEN = 2
                                   PERFORM 0200-DISPLAYTASK-START
                                      THRU 0200-DISPLAYTASK-END
      *Choix 3 : Suppression d'une ou plusieurs tâches
                            WHEN = 3
                                   PERFORM 0300-DELETETASK-START
                                      THRU 0300-DELETETASK-END
      *Relance le choix s'il ne correspond pas à une action définie
                            WHEN OTHER
                                   CONTINUE
                     END-EVALUATE
           END-PERFORM.
           
           STOP RUN.
           

      ******************************************************************
       0100-ADDTASK-START.
      *Menu d'assignation d'une tâche avec 6 pour choix de sortie
           PERFORM UNTIL WS-TASKCHOICE = 6
                  DISPLAY "Choisissez la tâche à assigner (1-5)"
                  DISPLAY "6 pour quitter"
      *Choix de l'utilisateur
                  ACCEPT        WS-TASKCHOICE
      *Assigne la tâche choisie
                  EVALUATE      WS-TASKCHOICE
                         WHEN = 1
                            DISPLAY WS-QUESTION
                            ACCEPT  WS-TASK1
                         WHEN = 2
                            DISPLAY WS-QUESTION
                            ACCEPT  WS-TASK2
                         WHEN = 3
                            DISPLAY WS-QUESTION
                            ACCEPT  WS-TASK3
                         WHEN = 4
                            DISPLAY WS-QUESTION
                            ACCEPT  WS-TASK4
                         WHEN = 5
                            DISPLAY WS-QUESTION
                            ACCEPT  WS-TASK5
                         WHEN OTHER 
                                CONTINUE
                  END-EVALUATE
           END-PERFORM.
      *Réinitialise la variable
           Move 0 TO WS-TASKCHOICE.
           EXIT.
       0100-ADDTASK-END.

       0200-DISPLAYTASK-START.
      *Affiche toutes les tâches
           DISPLAY "1- " WS-TASK1.
           DISPLAY "2- " WS-TASK2.
           DISPLAY "3- " WS-TASK3.
           DISPLAY "4- " WS-TASK4.
           DISPLAY "5- " WS-TASK5.
           EXIT.
       0200-DISPLAYTASK-END.

       0300-DELETETASK-START.
      *Menu de suppression des tâches avec 6 pour choix de sortie
           PERFORM UNTIL WS-TASKCHOICE = 6
                  DISPLAY "Choisissez la tâche à supprimer (1-5)"
                  DISPLAY "6 pour quitter"
      *Choix de l'utilisateur
                  ACCEPT        WS-TASKCHOICE
      *Supprime la tâche choisie
                  EVALUATE      WS-TASKCHOICE
                         WHEN = 1
                                     MOVE " " to WS-TASK1
                         WHEN = 2
                                     MOVE " " to WS-TASK2
                         WHEN = 3
                                     MOVE " " to WS-TASK3
                         WHEN = 4
                                     MOVE " " to WS-TASK4
                         WHEN = 5
                                     MOVE " " to WS-TASK5
                         WHEN OTHER 
                                CONTINUE
                  END-EVALUATE
           END-PERFORM.
      *Réinitialise la variable
           Move 0 TO WS-TASKCHOICE.
           EXIT.
       0300-DELETETASK-END.
