       IDENTIFICATION DIVISION.
       PROGRAM-ID. liregens.
       AUTHOR.    Terry.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT FICHIER-PERSONNES ASSIGN TO "gens.txt"
       ORGANIZATION IS LINE SEQUENTIAL.

       SELECT FICHIER-ORDRE ASSIGN TO "gens-ordre.txt"
       ORGANIZATION IS LINE SEQUENTIAL.

       SELECT FICHIER-INVERSE ASSIGN TO "gens-inverse.txt"
       ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD FICHIER-PERSONNES.
       01  F-PERSONNES.
           05  F-NOM             PIC X(12).
           05  F-PRENOM          PIC X(12).

       FD FICHIER-ORDRE.
       01  F-ORDRE.
           05  F-NOM-ORDRE             PIC X(12).
           05  F-PRENOM-ORDRE          PIC X(12).
       
       FD FICHIER-INVERSE.
       01  F-INVERSE.
           05  F-NOM-INVERSE             PIC X(12).
           05  F-PRENOM-INVERSE          PIC X(12).

       WORKING-STORAGE SECTION.
       
       01 WS-TABLE-PERSONNES.
         05  WS-PERSONNES OCCURS 10 TIMES.
           10  WS-NOM        PIC X(12).
           10  WS-PRENOM     PIC X(12).

       77  WS-INDEX-TABLE    PIC 9(02) VALUE 1.
       77  WS-MAX-TABLE      PIC 9(02) VALUE 10.
       77  WS-INDEX-WRITE    PIC 9(02) VALUE 1.

       77  WS-FIN-FICHIER    PIC X     VALUE "N".

       PROCEDURE DIVISION.
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
                       IF WS-INDEX-TABLE <= WS-MAX-TABLE
      *On transfère les valeurs des variables de la file section
      *Dans les variables de la Working section
                          MOVE F-NOM    TO WS-NOM(WS-INDEX-TABLE)
                          MOVE F-PRENOM TO WS-PRENOM(WS-INDEX-TABLE)
      *On incrémente l'index pour parcourir les différentes variables
                          ADD 1 TO WS-INDEX-TABLE
                       END-IF
               END-READ
           END-PERFORM.
           
           CLOSE FICHIER-PERSONNES.

           PERFORM VARYING WS-INDEX-TABLE FROM 1 BY 1
                   UNTIL WS-INDEX-TABLE > WS-MAX-TABLE
                DISPLAY WS-INDEX-TABLE
                            SPACE WITH NO ADVANCING
                DISPLAY "NOM    : " WS-NOM(WS-INDEX-TABLE)
                            SPACE WITH NO ADVANCING
                DISPLAY "PRENOM : " WS-PRENOM(WS-INDEX-TABLE)
           END-PERFORM.

           OPEN OUTPUT FICHIER-ORDRE.
           
           PERFORM VARYING WS-INDEX-WRITE FROM 1 BY 1
                     UNTIL WS-INDEX-WRITE > WS-MAX-TABLE
                MOVE WS-NOM(WS-INDEX-WRITE)    TO F-NOM-ORDRE
                MOVE WS-PRENOM(WS-INDEX-WRITE) TO F-PRENOM-ORDRE
                WRITE F-ORDRE
           END-PERFORM.

           CLOSE FICHIER-ORDRE.

           OPEN OUTPUT FICHIER-INVERSE.

           PERFORM VARYING WS-INDEX-WRITE FROM WS-MAX-TABLE BY -1
                     UNTIL WS-INDEX-WRITE < 1
                MOVE WS-NOM(WS-INDEX-WRITE)    TO F-NOM-INVERSE
                MOVE WS-PRENOM(WS-INDEX-WRITE) TO F-PRENOM-INVERSE
                WRITE F-INVERSE
           END-PERFORM.

           CLOSE FICHIER-INVERSE.

           STOP RUN.
