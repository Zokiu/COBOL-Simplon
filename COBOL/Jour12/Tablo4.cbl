       IDENTIFICATION DIVISION.
       PROGRAM-ID. Tablo4.
       AUTHOR.     Terry.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *Tableau comportant les informations d'une personne
      *Avec Filler pour mise en page. 
       01  WS-IDENTITE.
           02 WS-CARTE OCCURS 5 TIMES.
           05 WS-ID        PIC 9(02).
           05 FILLER       PIC X(06) VALUE " Nom: ".
           05 WS-NOM       PIC X(20).
           05 FILLER       PIC X(09) VALUE "Prénom: ".
           05 WS-PRENOM    PIC X(20).
           05 FILLER       PIC X(05) VALUE "Age: ".
           05 WS-AGE       PIC 9(03).
           05 FILLER       PIC X(01) VALUE X"0A".

      *Variable de saisie utilisateur.
       01  WS-SAISIE    PIC X(99).

      *Variable d'index
       77  WS-INDEX     PIC 9(02).
       77  WS-INDEX2    PIC 9(02).
       77  WS-INDEX3    PIC 9(02).


       PROCEDURE DIVISION.
      *Paragraphe pour remplir le tableau
           PERFORM 0100-REMPLISSAGE-DEB
           THRU    0100-REMPLISSAGE-FIN.
      *Paragraphe pour choix d'affichage  
           PERFORM 0200-MENU-DEB
           THRU    0200-MENU-FIN.

           STOP RUN.
      ******************************************************************

       0100-REMPLISSAGE-DEB.
      *Boucle permettant de remplir le tableau
           PERFORM VARYING WS-INDEX
                       FROM 1 BY 1
                       UNTIL WS-INDEX > 5
           DISPLAY "Veuillez renseigner Nom,Prénom,Age de la personne"
           ACCEPT WS-SAISIE
      *Fonction permettant de séparer les informations
      *    dans leurs variables respectives.
           UNSTRING WS-SAISIE DELIMITED BY ','
                   INTO WS-NOM(WS-INDEX)
                     WS-PRENOM(WS-INDEX)
                     WS-AGE(WS-INDEX)
      *On enregistre l'ID de la ligne
           MOVE WS-INDEX TO WS-ID(WS-INDEX)
      *On vide la variable de saisie utilisateur pour s'en resservir
           MOVE SPACE TO WS-SAISIE
           END-PERFORM.
           EXIT.
       0100-REMPLISSAGE-FIN.

       0200-MENU-DEB.
      *Menu utilisateur
           DISPLAY "Que voulez-vous faire ?".
           DISPLAY "1 - Voir toutes les personnes".
           DISPLAY "2 - Choisir 3 personnes"
           ACCEPT WS-SAISIE
           IF WS-SAISIE = "1"
      *On affiche tout le tableau
                   DISPLAY FUNCTION TRIM(WS-IDENTITE)
           ELSE IF WS-SAISIE = "2"
      *Paragraphe permettant la saisie des 3 choix de personnes
                   PERFORM 0210-MENU-2-DEB
                   THRU    0210-MENU-2-FIN
           ELSE 
      *Contrôle de saisie puis relance le paragraphe
               DISPLAY "Mauvaise saisie"
               PERFORM 0200-MENU-DEB
               THRU    0200-MENU-FIN
           END-IF.
           EXIT.
       0200-MENU-FIN.

       0210-MENU-2-DEB.
      *Saisie utilisateur des 3 personnes
           DISPLAY "Veuillez choisir la 1ère personne"
           ACCEPT  WS-INDEX
           DISPLAY "Veuillez choisir la 2ème personne"
           ACCEPT  WS-INDEX2
           DISPLAY "Veuillez choisir la 3ème personne"
           ACCEPT  WS-INDEX3
      *Affichage des 3 personnes choisies
           DISPLAY WS-CARTE(WS-INDEX)
           DISPLAY WS-CARTE(WS-INDEX2)
           DISPLAY WS-CARTE(WS-INDEX3)
           EXIT.
       0210-MENU-2-FIN.
