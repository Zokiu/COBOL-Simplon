      * ecrire un prog qui dde a utilisateur de saisir 5 nbres puiq qui affiche le plus petit des 5
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Highest.
       AUTHOR. Terry.Bernadette

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * declaration une variable nombre
       01  WS-NUMBER     PIC 99.
      * declaration une variable de stockage des nombres
       01  WS-STOCK      PIC 99.
      * declaration une variable affichage
       01  WS-RESULT-ED  PIC ZZ.

       PROCEDURE DIVISION.
      * boucle qui demande d'entrer un nombre 5 fois de suite.
           PERFORM 5 TIMES
      * affichage de la demande
                DISPLAY "Saisir un nombre"
      * acceptation du nombre
      * stockage du nombre dans la variable stockage
                ACCEPT WS-STOCK
      * variable stockage : comparaison des nombres
                ??????
      * affichage du nombre le plus petit
                DISPLAY ???