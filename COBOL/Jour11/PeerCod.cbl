      *Demander à l'utilisateur de saisir le nombre de convive et 
      *afficher le nombre de pizza à commander sachant que chaque
      *convive consomme 1,1 Pizza.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PeerCod.
       AUTHOR. 

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CONSIDERATION        PIC X.
           88 WS-CONSIDERATION-OUI VALUE "Y".
           88 WS-CONSIDERATION-NON VALUE "n".

       77  WS-INDEX                 PIC 9(02).

       01  WS-ALEATOIRE                       PIC 9(03).
       01  WS-ALEATOIRE-2                     PIC 9(03).

       01  WS-CONVIVE.
           05 WS-CONVIVE-PRESENT              PIC 9(02).
           05 WS-CONVIVE-PRESENT-DECIDE       PIC 9(02).
           05 WS-CONVIVE-PRESENT-INDECIS      PIC 9(02).
           05 WS-CONVIVE-MALPOLI              PIC 9(02).
           05 WS-CONVIVE-ABSENT               PIC 9(02).

       01  WS-ORGANISATEUR                    PIC 9(02).

       01  WS-PIZZA.
           05 WS-PIZZA-MIN                    PIC 9(03).99.
           05 WS-FILLER                       PIC X(10) VALUE SPACE.
           05 WS-PIZZA-MAX                    PIC 9(03).99.

       01  WS-PIZZA-TYPE.
           05 WS-PIZZA-1-ED     PIC X(20) VALUE "PEPERONI".
           05 WS-PIZZA-2-ED     PIC X(20) VALUE "MARGHERITA".
           05 WS-PIZZA-3-ED     PIC X(20) VALUE "ROMANA".
           05 WS-PIZZA-4-ED     PIC X(20) VALUE "WELSH".
           05 WS-PIZZA-5-ED     PIC X(20) VALUE "VEGETARIENNE".
           05 WS-PIZZA-6-ED     PIC X(20) VALUE "4 FROMAGES".
           05 WS-PIZZA-7-ED     PIC X(20) VALUE "REGINA".
           05 WS-PIZZA-8-ED     PIC X(20) VALUE "HAWAIENNE".
           05 WS-PIZZA-1        PIC 9(03) VALUE ZERO.
           05 WS-PIZZA-2        PIC 9(03) VALUE ZERO.
           05 WS-PIZZA-3        PIC 9(03) VALUE ZERO.
           05 WS-PIZZA-4        PIC 9(03) VALUE ZERO.
           05 WS-PIZZA-5        PIC 9(03) VALUE ZERO.
           05 WS-PIZZA-6        PIC 9(03) VALUE ZERO.
           05 WS-PIZZA-7        PIC 9(03) VALUE ZERO.
           05 WS-PIZZA-8        PIC 9(03) VALUE ZERO.
           05 WS-PIZZA-CHOIX    PIC 9.

       PROCEDURE DIVISION.
           
           PERFORM 9830-LA-CAF
           THRU    5649-AH-OUAIS-T-ES-BIEN.

           PERFORM 3574-T-AS-COMBIEN-DE-POTES
           THRU    4657-C-EST-TOUT.

           PERFORM 3479-LES-VRAIS-POTES
           THRU    6739-ILS-SONT-JOIGNABLES.

           PERFORM 4628-LES-PAS-RELOUS
           THRU    2179-ILS-SACHENT.

           PERFORM 8730-IL-EST-MALPOLI
           THRU    2674-OUI-IL-L-EST.

           PERFORM 2467-TU-CONSIDERE-OU-PAS 
           THRU    1209-TU-AS-FINI-DE-CONSIDERER.

           PERFORM 6438-QUI-VEUT-GAGNER-DE-L-ARGENT-EN-MASSE
           THRU    2516-LA-REPONSE-D.

           STOP RUN.

      ******************************************************************
       6438-QUI-VEUT-GAGNER-DE-L-ARGENT-EN-MASSE.
           DISPLAY "Exact".
           DISPLAY "la réponse exacte".
           EXIT.
       2516-LA-REPONSE-D.

       9830-LA-CAF.
           DISPLAY "Combien de membre comporte votre famille ? ".
           ACCEPT WS-ORGANISATEUR.
           PERFORM WS-ORGANISATEUR TIMES
                DISPLAY "Quel type de pizza a choisi l'organisateur"
                       WS-ORGANISATEUR(WS-INDEX:1)
                PERFORM 2681-C-EST-LA-COMMANDE
                THRU    1689-GENRE-LE-GOUT
           END-PERFORM.
           MOVE 0 TO WS-INDEX.
           EXIT.
       5649-AH-OUAIS-T-ES-BIEN.

       3574-T-AS-COMBIEN-DE-POTES.
           DISPLAY "Veuillez saisir le nombre" WITH NO ADVANCING.
           DISPLAY " de convive que vous avez invité : ".
           ACCEPT WS-CONVIVE.
           EXIT.
       4657-C-EST-TOUT.

       3479-LES-VRAIS-POTES.
           DISPLAY "Combien ont dit oui ?".
           ACCEPT WS-CONVIVE-PRESENT.
           EXIT.
       6739-ILS-SONT-JOIGNABLES.

       4628-LES-PAS-RELOUS.
           DISPLAY "Combien a choisi sa pizza ?".
           ACCEPT WS-CONVIVE-PRESENT-DECIDE.
           COMPUTE WS-CONVIVE-PRESENT-INDECIS = WS-CONVIVE-PRESENT
                                            - WS-CONVIVE-PRESENT-DECIDE.

           PERFORM WS-CONVIVE-PRESENT-DECIDE TIMES
                DISPLAY "Quel type de pizza a choisi le convive"
                       WS-CONVIVE-PRESENT-DECIDE(WS-INDEX:1)
                PERFORM 2681-C-EST-LA-COMMANDE
                THRU    1689-GENRE-LE-GOUT
           END-PERFORM.
           MOVE 0 TO WS-INDEX.
           EXIT.
       2179-ILS-SACHENT.

       2681-C-EST-LA-COMMANDE.
                ACCEPT WS-PIZZA-CHOIX
                EVALUATE WS-PIZZA-CHOIX
                    WHEN = 1
                        ADD 1 TO WS-PIZZA-1
                    WHEN = 2
                        ADD 1 TO WS-PIZZA-2
                    WHEN = 3
                        ADD 1 TO WS-PIZZA-3
                    WHEN = 4
                        ADD 1 TO WS-PIZZA-4
                    WHEN = 5
                        ADD 1 TO WS-PIZZA-5
                    WHEN = 6
                        ADD 1 TO WS-PIZZA-6
                    WHEN = 7
                        ADD 1 TO WS-PIZZA-7
                    WHEN = 8
                        ADD 1 TO WS-PIZZA-8
                    WHEN OTHER 
                        DISPLAY "T'es pas clair frère !"
                        ADD 1 TO WS-CONVIVE-PRESENT-INDECIS
                    END-EVALUATE
           EXIT.
       1689-GENRE-LE-GOUT.
       
       8730-IL-EST-MALPOLI.
           DISPLAY "Combien n'ont pas répondu ?".
           ACCEPT WS-CONVIVE-MALPOLI.
           EXIT.
       2674-OUI-IL-L-EST.

       0384-CONSIDERE-BASE.
           DISPLAY "Voulez-vous les prendre en considération ?".
           ACCEPT WS-CONSIDERATION.
           PERFORM 2467-TU-CONSIDERE-OU-PAS 
           THRU    1209-TU-AS-FINI-DE-CONSIDERER.
           EXIT.
       0568-CONSIDERE-LA.

       8467-C-EST-LA-ROULETTE.
           COMPUTE WS-ALEATOIRE = FUNCTION RANDOM (WS-ALEATOIRE-2).
           EVALUATE WS-ALEATOIRE
                WHEN < 12
                    ADD 1 TO WS-PIZZA-1
                WHEN < 25
                    ADD 1 TO WS-PIZZA-2
                WHEN < 37
                    ADD 1 TO WS-PIZZA-3
                WHEN < 50
                    ADD 1 TO WS-PIZZA-4
                WHEN < 62
                    ADD 1 TO WS-PIZZA-5
                WHEN < 75
                    ADD 1 TO WS-PIZZA-6
                WHEN < 87
                    ADD 1 TO WS-PIZZA-7
                WHEN < 101
                    ADD 1 TO WS-PIZZA-8
           END-EVALUATE.
           EXIT.
       3748-TOUT-SUR-LE-ROUGE.

       2467-TU-CONSIDERE-OU-PAS.
           EVALUATE WS-CONSIDERATION
               WHEN = "Y"
                   PERFORM WS-CONVIVE-PRESENT-INDECIS TIMES
                       PERFORM 8467-C-EST-LA-ROULETTE
                       THRU    3748-TOUT-SUR-LE-ROUGE
                   END-PERFORM
                   COMPUTE WS-PIZZA-MAX = (WS-PIZZA-1
                                         + WS-PIZZA-2
                                         + WS-PIZZA-3
                                         + WS-PIZZA-4
                                         + WS-PIZZA-5
                                         + WS-PIZZA-6
                                         + WS-PIZZA-7
                                         + WS-PIZZA-8)
                                         * 1.1
                   DISPLAY "Vous devez commander" WS-PIZZA-MAX
               WHEN = "y"
                   MOVE FUNCTION UPPER-CASE(WS-CONSIDERATION)
                        TO WS-CONSIDERATION
                  PERFORM 2467-TU-CONSIDERE-OU-PAS 
                  THRU    1209-TU-AS-FINI-DE-CONSIDERER
               WHEN = "N"
                   MOVE FUNCTION LOWER-CASE(WS-CONSIDERATION)
                        TO WS-CONSIDERATION
                   PERFORM 1208-TU-CONSIDERE-OU-PAS-2
                   THRU    2468-TU-AS-FINI-DE-CONSIDERER-2
               WHEN = "n"
                   COMPUTE WS-PIZZA-MIN = (WS-PIZZA-1
                                         + WS-PIZZA-2
                                         + WS-PIZZA-3
                                         + WS-PIZZA-4
                                         + WS-PIZZA-5
                                         + WS-PIZZA-6
                                         + WS-PIZZA-7
                                         + WS-PIZZA-8)
                                         * 1.1
                    DISPLAY "Vous devez commander" WS-PIZZA-MIN
               WHEN OTHER DISPLAY "J'ai pas compris recommence"
                          PERFORM 0384-CONSIDERE-BASE
                          THRU    0568-CONSIDERE-LA 
           END-EVALUATE
           EXIT.
       1209-TU-AS-FINI-DE-CONSIDERER.

       1208-TU-CONSIDERE-OU-PAS-2.
           EVALUATE WS-CONSIDERATION
               WHEN = "Y"
                   COMPUTE WS-PIZZA-MAX = (WS-CONVIVE-PRESENT
                                          + WS-CONVIVE-MALPOLI
                                          + WS-ORGANISATEUR)
                                          * 1.1
               WHEN = "y"
                   MOVE FUNCTION UPPER-CASE(WS-CONSIDERATION)
                        TO WS-CONSIDERATION
                  PERFORM 2467-TU-CONSIDERE-OU-PAS 
                  THRU    1209-TU-AS-FINI-DE-CONSIDERER
               WHEN = "N"
                   MOVE FUNCTION LOWER-CASE(WS-CONSIDERATION)
                        TO WS-CONSIDERATION
                   PERFORM 1208-TU-CONSIDERE-OU-PAS-2
                   THRU    2468-TU-AS-FINI-DE-CONSIDERER-2
               WHEN = "n"
                   COMPUTE WS-PIZZA-MIN = (WS-CONVIVE-PRESENT
                                          + WS-ORGANISATEUR)
                                          * 1.1
               WHEN OTHER DISPLAY "J'ai pas compris recommence"
                          PERFORM 0384-CONSIDERE-BASE
                          THRU    0568-CONSIDERE-LA 
           END-EVALUATE
           EXIT.
       2468-TU-AS-FINI-DE-CONSIDERER-2.
