       IDENTIFICATION DIVISION.
       PROGRAM-ID. Filler.
       AUTHOR.    Terry.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01  WS-NOTES OCCURS 100 TIMES.
           03 FILLER           PIC X(03) VALUE " : "
           03 WS-NOTE          PIC 9(03).
           03 FILLER           PIC X(04) VALUE " le ".
           03 WS-DATE.
               05 WS-JOUR      PIC X(02).
               05 FILLER       PIC X VALUE "/".
               05 WS-MOIS      PIC X(02).
               05 FILLER       PIC X VALUE "/".
               05 WS-ANNEE     PIC X(04).
 
       77  WS-INDEX            PIC 9(03).

       PROCEDURE DIVISION.
       
           PERFORM VARYING WS-INDEX FROM 1 BY 1
                   UNTIL WS-NOTE(WS-INDEX - 1)  >  99 OR WS-INDEX > 100
           
           DISPLAY "Saisissez votre note (0-99)(100 pour quitter)"
           ACCEPT WS-NOTE(WS-INDEX)
           DISPLAY "Saisissez le jour"
           ACCEPT WS-JOUR(WS-INDEX)
           DISPLAY "Saisissez le mois"
           ACCEPT WS-MOIS(WS-INDEX)
           DISPLAY "Saisissez l'année"
           ACCEPT WS-ANNEE(WS-INDEX)
           DISPLAY "Note n°"WS-INDEX WS-NOTE(WS-INDEX)
                    WS-DATE(WS-INDEX)
           END-PERFORM.

           STOP RUN.
