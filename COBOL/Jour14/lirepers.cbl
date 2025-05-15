       IDENTIFICATION DIVISION.
       PROGRAM-ID. lirepers.
       AUTHOR.    Terry.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *On assigne le fichier à un Allias avec le sens de lecture
           SELECT FICHIER-PERSONNES ASSIGN TO "personnes.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
      *
       FD FICHIER-PERSONNES.
      *Groupe de variable des données du fichier
       01  F-PERSONNES.
           05 F-NOM                   PIC X(15).
           05 F-PRENOM                PIC X(15).
           05 F-DATE.
               10 F-MOIS              PIC 9(02).
               10 F-JOUR              PIC 9(02).
               10 F-ANNEE             PIC 9(04).

       WORKING-STORAGE SECTION.
      *Groupe de variable pour stocker les données après transfert
      *du fichier comprenant un tableau pour chaque ligne de donnée
       01  WS-TABLE-PERSONNES.
           05 WS-PERSONNES OCCURS 10 TIMES.
               10 WS-NOM              PIC X(15).
               10 WS-PRENOM           PIC X(15).
               10 WS-DATE.
                   15 WS-MOIS         PIC 9(02).
                   15 WS-JOUR         PIC 9(02).
                   15 WS-ANNEE        PIC 9(04).
      *Groupe de variable avec filler pour affichage final
       01  WS-TABLE-OUTPUT.
           05 WS-OUTPUT OCCURS 10 TIMES.
               10 FILLER              PIC X(06) 
                                       VALUE "Nom : ".
               10 WS-OUTPUT-NOM       PIC X(15).
               10 FILLER              PIC X(10) 
                                       VALUE "Prénom : ".
               10 WS-OUTPUT-PRENOM    PIC X(15).
               10 FILLER              PIC X(20) 
                                       VALUE "Date de naissance : ".
               10 WS-OUTPUT-DATE.
                   15 WS-OUTPUT-JOUR  PIC 9(02).
                   15 FILLER          PIC X VALUE "/".
                   15 WS-OUTPUT-MOIS  PIC 9(02).
                   15 FILLER          PIC X VALUE "/".
                   15 WS-OUTPUT-ANNEE PIC 9(04).
               10 FILLER              PIC X(10)
                                       VALUE "   Âge : ".
               10 WS-OUTPUT-AGE       PIC 9(03).
               10 FILLER              PIC X VALUE X"0A".
      
      *variable et limiteur pour indexer le tableau
       77  WS-INDEX                   PIC 9(02) VALUE 1.
       77  WS-INDEX-MAX               PIC 9(02) VALUE 10.
      *Variable pour condition de fin de lecture fichier
       77  WS-FIN-FICHIER             PIC X VALUE "N".
      *Variable pour saisie utilisateur
       01  WS-SAISIE                  PIC X(15).
      *Groupe de variable pour stocker la date actuelle
       01  WS-CURRENT-DATE.
           05 WS-CURRENT-ANNEE        PIC 9(04).
           05 WS-CURRENT-MOIS         PIC 9(02).
           05 WS-CURRENT-JOUR         PIC 9(02).

       PROCEDURE DIVISION.
      *Paragraphe permettant la lecture du fichier
      *Et le stockage de ses données
           PERFORM 0100-READ-START
           THRU    0100-READ-END.
      *Saisie utilisateur du nom recherché
           DISPLAY "Veuillez choisir la personne".
           ACCEPT WS-SAISIE.        
      *Paragraphe permettant l'affichage final des données voulues
           PERFORM 0200-OUTPUT-START
           THRU    0200-OUTPUT-END.

           STOP RUN.

      ******************************************************************
       0100-READ-START.
      *On ouvre le fichier
           OPEN INPUT FICHIER-PERSONNES.
      *On boucle jusqu'à la fin du fichier
           PERFORM UNTIL WS-FIN-FICHIER = "Y"
      *On commence à parcourir le fichier
               READ FICHIER-PERSONNES
      *On lui définit l'action à réaliser à la fin du fichier
                   AT END
      *Ici on change la valeur de la variable
      *assignée à la fin de la boucle
                       MOVE "Y" TO WS-FIN-FICHIER
      *On définit les actions à réaliser pendant la lecture du fichier
                   NOT AT END
                       IF WS-INDEX <= WS-INDEX-MAX
      *On transfère les valeurs des variables de la file section
      *Dans les variables de la Working section
                           MOVE F-NOM    TO WS-NOM(WS-INDEX)
                           MOVE F-PRENOM TO WS-PRENOM(WS-INDEX)
                           MOVE F-DATE   TO WS-DATE(WS-INDEX)
      *On incrémente l'index pour parcourir les différentes variables
                           ADD 1 TO WS-INDEX
                       END-IF
               END-READ
           END-PERFORM.
      *On ferme le fichier
           CLOSE FICHIER-PERSONNES.

           EXIT.
       0100-READ-END.

       0200-OUTPUT-START.
      *On transfère toutes les données pour préparer l'affichage final
           PERFORM VARYING WS-INDEX FROM 1 BY 1
                   UNTIL WS-INDEX > WS-INDEX-MAX
               MOVE WS-NOM(WS-INDEX)    TO WS-OUTPUT-NOM(WS-INDEX)
               MOVE WS-PRENOM(WS-INDEX) TO WS-OUTPUT-PRENOM(WS-INDEX)
               MOVE WS-JOUR(WS-INDEX)   TO WS-OUTPUT-JOUR(WS-INDEX)
               MOVE WS-MOIS(WS-INDEX)   TO WS-OUTPUT-MOIS(WS-INDEX)
               MOVE WS-ANNEE(WS-INDEX)  TO WS-OUTPUT-ANNEE(WS-INDEX)
      *Paragraphe permettant le calcul de l'âge
               PERFORM 0300-AGE-START
               THRU    0300-AGE-END
      *On compare le nom demandé à ceux du tableau
               IF WS-SAISIE = WS-NOM(WS-INDEX)
      *On affiche les données correspondantes au nom demandé
                   DISPLAY WS-OUTPUT(WS-INDEX)
               END-IF
           END-PERFORM.
           

           EXIT.
       0200-OUTPUT-END.

       0300-AGE-START.
      *On stocke la date actuelle dans un ensemble de variable
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE.
      *On compare les mois pour le calcul de l'age
           EVALUATE WS-OUTPUT-MOIS(WS-INDEX)
                WHEN < WS-CURRENT-MOIS
                   COMPUTE WS-OUTPUT-AGE(WS-INDEX) =
                       WS-CURRENT-ANNEE - WS-OUTPUT-ANNEE(WS-INDEX)

                WHEN > WS-CURRENT-MOIS
                   COMPUTE WS-OUTPUT-AGE(WS-INDEX) =
                       WS-CURRENT-ANNEE - 1 - WS-OUTPUT-ANNEE(WS-INDEX)

                WHEN = WS-CURRENT-MOIS
      *On compare ici en plus les jour car le mois correspond
                    IF WS-JOUR(WS-INDEX) <= WS-CURRENT-JOUR
                        COMPUTE WS-OUTPUT-AGE(WS-INDEX) =
                       WS-CURRENT-ANNEE - 1 - WS-OUTPUT-ANNEE(WS-INDEX)

                    ELSE 
                        COMPUTE WS-OUTPUT-AGE(WS-INDEX) =
                       WS-CURRENT-ANNEE - WS-OUTPUT-ANNEE(WS-INDEX)
           END-EVALUATE.

           EXIT.
       0300-AGE-END.
