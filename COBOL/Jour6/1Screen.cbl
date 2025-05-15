       IDENTIFICATION DIVISION.
       PROGRAM-ID. 1Screen.
       AUTHOR. Terry.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01 TASKS.
           02 TASK1 PIC X(15).
           02 TASK2 PIC X(15).
           02 TASK3 PIC X(15).
           02 TASK4 PIC X(15).
           02 TASK5 PIC X(15).

       SCREEN SECTION.

       01 ECRAN1.
           02 BLANK SCREEN BACKGROUND-COLOR 8.
           02 LINE 02 COL 33 PIC X(14) VALUE "Ma To-Do List"
               FOREGROUND-COLOR 2.
           02 LINE 03 COL 02 PIC X(08) VALUE "Tache 1"
               BACKGROUND-COLOR 6.
           02 LINE 04 COL 10 PIC X(16) USING TASK1
               BACKGROUND-COLOR 2 FOREGROUND-COLOR 4.
           02 LINE 05 COL 02 PIC X(08) VALUE "Tache 2"
               BACKGROUND-COLOR 6.
           02 LINE 06 COL 10 PIC X(16) USING TASK2
               BACKGROUND-COLOR 2 FOREGROUND-COLOR 4.
           02 LINE 07 COL 02 PIC X(08) VALUE "Tache 3"
               BACKGROUND-COLOR 6.
           02 LINE 08 COL 10 PIC X(16) USING TASK3
               BACKGROUND-COLOR 2 FOREGROUND-COLOR 4.
           02 LINE 09 COL 02 PIC X(08) VALUE "Tache 4"
               BACKGROUND-COLOR 6.
           02 LINE 10 COL 10 PIC X(16) USING TASK4
               BACKGROUND-COLOR 2 FOREGROUND-COLOR 4.
           02 LINE 11 COL 02 PIC X(08) VALUE "Tache 5"
               BACKGROUND-COLOR 6.
           02 LINE 12 COL 10 PIC X(16) USING TASK5
               BACKGROUND-COLOR 2 FOREGROUND-COLOR 4.
           
           
           
       PROCEDURE DIVISION.
           
           DISPLAY ECRAN1.
           ACCEPT  ECRAN1.
           DISPLAY ECRAN1.
           

           STOP RUN.
