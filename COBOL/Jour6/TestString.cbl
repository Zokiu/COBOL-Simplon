       IDENTIFICATION DIVISION.
       PROGRAM-ID. TestString.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01 STRINGE PIC Z(20).
       01 CNTR    PIC 99.

       PROCEDURE DIVISION.
       
           STRING "Test1" DELIMITED BY SIZE
                  "Test3" DELIMITED BY SIZE
                          INTO STRINGE.
           STRING FUNCTION TRIM(STRINGE) DELIMITED BY SIZE
                  "Test2" DELIMITED BY SIZE
                          INTO STRINGE.
           INSPECT STRINGE
              TALLYING CNTR 
              FOR ALL CHARACTERS BEFORE INITIAL SPACES.
           MOVE " " TO STRINGE(CNTR:1).
           DISPLAY STRINGE.

           STOP RUN.
