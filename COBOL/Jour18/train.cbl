       IDENTIFICATION DIVISION.
       PROGRAM-ID. train.
       AUTHOR.    Anaïs&Terry.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

      *SOURCE-COMPUTER. DELL WITH DEBUGGING MODE.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *Fichier d'entrée: traité séquentiellement.
       SELECT FICHIER-TRAIN ASSIGN TO "train.dat"
           ORGANIZATION IS LINE SEQUENTIAL.

      *Fichier de sortie: traité séquentiellement.
       SELECT FICHIER-AFFICHAGE ASSIGN TO "train2.dat"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD FICHIER-TRAIN.
      *On utilise un fichier contenant des variables.
           COPY "train-record.cpy".

                     
       FD FICHIER-AFFICHAGE.
      *Structure d'écriture avec le format
       01  F-TRAIN.
           05 F-TEXTE-TYPE       PIC       X(12).
           05 F-TYPE             PIC       X(03).
           05 F-TEXTE-GARE       PIC       X(23).
           05 F-GARE             PIC       X(18).
           05 F-TEXTE-DEPART     PIC       X(15).
           05 F-DEPART           PIC       X(06).
           05 F-TEXTE-DUREE      PIC       X(13).
           05 F-DUREE            PIC       X(06).
           05 F-TEXTE-ARRET      PIC       X(10).
           05 F-ARRET            PIC       9(02).
           05 F-TEXTE-ARRIVEE    PIC       X(17).
           05 F-ARRIVEE          PIC       X(06).

      *Variable pour afficher le bas de page.
       01  F-FOOTER.
           05 F-NB-TRAIN         PIC       X(29).

       WORKING-STORAGE SECTION.
      
      *Groupe de variable avec tableau 
      *    pour stocker les lignes du fichier d'entrée.
       01 WS-TABLE-TRAIN. 
           05 WS-TRAIN-PLANNING       OCCURS 46 TIMES.
      *Variable pour stocker le type de train.
              10 WS-RECORD-TYPE            PIC X(3).
      *Variable pour stocker la station de départ. 
              10 WS-STATION-DEPART         PIC X(18).
      *Variable pour stocker l'heure de départ du train.
              10 WS-TRAIN-TIME.
                  15 WS-TRAIN-TIME-HH      PIC 99.
                  15 WS-TRAIN-TIME-MM      PIC 99.
      *Variable pour stocker la durée du trajet.        
              10 WS-TRAIN-NBRE-HEURES      PIC 99.
      *Variable pour stocker le nombre d'arrêt en texte brut.
              10 WS-TRAIN-HALT             PIC X(10).
      *Variable pour compter et stocker le nombre d'arrêt.
              10 WS-NB-ARRET               PIC 9(02) VALUE 0.
      *Variable pour stocker l'heure d'arrivée.
              10 WS-H-ARRIVEE              PIC 9(02).
      *Variable pour stocker l'heure complète d'arrivée(HH:MMh).
              10 WS-ARRIVEE                PIC X(06).
      *Variable pour stocker l'heure complète de départ(HH:MMh).
              10 WS-DEPART-FORMAT          PIC X(06).

      *Index pour naviguer dans le tableau.
       77  WS-IDX-TRAIN                    PIC 9(02) VALUE  1.
      *Valeur maximale du tableau. 
       77  WS-MAX-TABLE                    PIC 9(02) VALUE 46.
      *Booléen pour fermer le fichier en fin de lecture.
       77  WS-FIN-FICHIER                  PIC X(01) VALUE "N".

      *Variable indépendante pour gérer les calculs.
       01  WS-CALCUL                       PIC 9(02).

       PROCEDURE DIVISION.
      *Appel de paragraphe pour la lecture du fichier d'entrée.     
           PERFORM 0100-LECTURE-FICHIER-TRAIN-DEB
           THRU 0100-LECTURE-FICHIER-TRAIN-FIN.
      *Appel de paragraphe pour affichage console.
           PERFORM 0200-AFFICHAGE-FICHIER-TRAIN-DEB
           THRU 0200-AFFICHAGE-FICHIER-TRAIN-FIN.
      *Appel de paragraphe pour compter le nombre d'arrêt.
           PERFORM 0300-NB-ARRET-DEB
           THRU    0300-NB-ARRET-FIN.
      *Appel de paragraphe pour calculer et formater l'heure d'arrivée.
           PERFORM 0400-ARRIVEE-DEB
           THRU    0400-ARRIVEE-FIN.
      *Appel de paragraphe pour écriture du fichier de sortie.
           PERFORM 0500-ECRITURE-DEB
           THRU    0500-ECRITURE-FIN.
       

           STOP RUN.
      
      ******************************************************************
       
      *Paragraphe de lecture du fichier d'entrée. 
       0100-LECTURE-FICHIER-TRAIN-DEB.
       OPEN INPUT FICHIER-TRAIN.
      
      *Boucle permettant d'arrêter la lecture à la fin du fichier.
       PERFORM UNTIL WS-FIN-FICHIER = "O"
           READ FICHIER-TRAIN
               AT END
                   MOVE 'O' TO WS-FIN-FICHIER
               NOT AT END
                   IF WS-IDX-TRAIN <= WS-MAX-TABLE
      *On assigne la valeur de chaque ligne au tableau de stockage.
                   MOVE TRAIN-PLANNING
                             TO WS-TRAIN-PLANNING(WS-IDX-TRAIN)
      *On ajoute 1 à l'index afin de passer à la ligne suivante. 
                   ADD 1 TO WS-IDX-TRAIN
                   END-IF
           END-READ
       END-PERFORM.

       CLOSE FICHIER-TRAIN.

           EXIT.
       0100-LECTURE-FICHIER-TRAIN-FIN.
       
      *Paragraphe d'affichage console pour type/gare/départ.
       0200-AFFICHAGE-FICHIER-TRAIN-DEB. 
      *Boucle permettant de parcourir chaque ligne.
       PERFORM VARYING WS-IDX-TRAIN FROM 1 BY 1 
           UNTIL WS-IDX-TRAIN > WS-MAX-TABLE
      *Appel de paragraphe pour formater l'heure de départ.
           PERFORM 0600-FORMATAGE-DEPART-DEB
           THRU    0600-FORMATAGE-DEPART-FIN
      *Affichage des données primaires (type/gare/heure de départ)
           DISPLAY WS-RECORD-TYPE (WS-IDX-TRAIN) 
                                         SPACE WITH NO ADVANCING
           DISPLAY WS-STATION-DEPART (WS-IDX-TRAIN)
                                         SPACE WITH NO ADVANCING
           DISPLAY WS-DEPART-FORMAT (WS-IDX-TRAIN)
           
       END-PERFORM.

           EXIT.
       0200-AFFICHAGE-FICHIER-TRAIN-FIN.

      *Paragraphe permettant de compter le nombre d'arrêts des trains.
       0300-NB-ARRET-DEB.
      *Boucle pour parcourir chaque train.
           PERFORM VARYING WS-IDX-TRAIN FROM 1 BY 1 
                              UNTIL WS-IDX-TRAIN > WS-MAX-TABLE
      *Fonction qui compte le nombre de "H" dans une chaine.
      *On stocke le résultat dans une variable.
           INSPECT WS-TRAIN-HALT(WS-IDX-TRAIN)
                         TALLYING WS-NB-ARRET(WS-IDX-TRAIN) FOR ALL "H"
      *Affichage pour debug.
      D     DISPLAY WS-IDX-TRAIN "NB arret : "
      D     DISPLAY WS-NB-ARRET(WS-IDX-TRAIN)
           
           END-PERFORM.
           EXIT.
       0300-NB-ARRET-FIN.

      *Paragraphe permettant de calculer l'heure d'arrivée des trains.
       0400-ARRIVEE-DEB.
      *Boucle pour parcourir chaque train.
           PERFORM VARYING WS-IDX-TRAIN FROM 1 BY 1
                              UNTIL WS-IDX-TRAIN > WS-MAX-TABLE
      *On ajouter la durée en heure à la valeur de l'heure de départ.
           ADD WS-TRAIN-NBRE-HEURES(WS-IDX-TRAIN) 
                               TO WS-TRAIN-TIME-HH(WS-IDX-TRAIN)
                                        GIVING WS-CALCUL
      *On divise par 24 pour récupérer le reste qui donnera l'heure.
           DIVIDE WS-CALCUL BY 24 
                  GIVING WS-CALCUL REMAINDER WS-H-ARRIVEE(WS-IDX-TRAIN)
      *On crée une chaine de caractère pour formater l'heure.
           STRING WS-H-ARRIVEE(WS-IDX-TRAIN) 
                   ":" 
                   WS-TRAIN-TIME-MM(WS-IDX-TRAIN) 
                   "h"
                        INTO WS-ARRIVEE(WS-IDX-TRAIN)
      *Affichage pour debug.
      D    DISPLAY WS-IDX-TRAIN "Heure arrivee: "
      D    DISPLAY WS-ARRIVEE(WS-IDX-TRAIN)

           END-PERFORM.

           EXIT.
       0400-ARRIVEE-FIN.

      *Paragraphe permettant l'écriture dans un fichier de sortie.
       0500-ECRITURE-DEB.

           OPEN OUTPUT FICHIER-AFFICHAGE.
      *Boucle pour parcourir toutes les lignes.
           PERFORM VARYING WS-IDX-TRAIN FROM 1 BY 1
                               UNTIL WS-IDX-TRAIN > WS-MAX-TABLE
      *On remplie puis on écris chaque ligne.
             MOVE "Train Type: "                     TO F-TEXTE-TYPE
             MOVE WS-RECORD-TYPE(WS-IDX-TRAIN)       TO F-TYPE
             MOVE " | Departure Station: "           TO F-TEXTE-GARE
             MOVE WS-STATION-DEPART(WS-IDX-TRAIN)    TO F-GARE
             MOVE " | Train Time: "                  TO F-TEXTE-DEPART
             MOVE WS-DEPART-FORMAT(WS-IDX-TRAIN)     TO F-DEPART
             MOVE " | Duration: "                    TO F-TEXTE-DUREE
             MOVE WS-TRAIN-NBRE-HEURES(WS-IDX-TRAIN) TO F-DUREE
             MOVE " | Stops: "                       TO F-TEXTE-ARRET
             MOVE FUNCTION NUMVAL(WS-NB-ARRET(WS-IDX-TRAIN))
                                                     TO F-ARRET
             MOVE " | Arrival Time: "                TO F-TEXTE-ARRIVEE
             MOVE WS-ARRIVEE(WS-IDX-TRAIN)           TO F-ARRIVEE
             WRITE F-TRAIN
           END-PERFORM.
      *On enlève 1 car l'index a dépassé le nombre de ligne d'un pas.
           SUBTRACT 1 FROM WS-IDX-TRAIN.
      *On crée une chaine de caractère complète avant de l'écrire.
           STRING WS-IDX-TRAIN " trains ont été traités."
                     INTO F-NB-TRAIN.
           WRITE F-FOOTER.

           CLOSE FICHIER-AFFICHAGE.

           EXIT.
       0500-ECRITURE-FIN.
      
      *Paragraphe permettant le formatage de l'heure de départ.
       0600-FORMATAGE-DEPART-DEB.
      *On crée une chaine de caractère pour formater l'heure.
           STRING WS-TRAIN-TIME-HH(WS-IDX-TRAIN)
                  ":"
                  WS-TRAIN-TIME-MM(WS-IDX-TRAIN)
                  "h"
                  INTO WS-DEPART-FORMAT(WS-IDX-TRAIN).

           EXIT.
       0600-FORMATAGE-DEPART-FIN.

