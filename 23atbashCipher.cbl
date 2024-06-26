       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATBASH-CIPHER.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-PHRASE          PIC X(60).
       01 WS-PHRASE-TRIM     PIC X(60).
       01 WS-RESULT          PIC X(60).
       01 WS-RESULT-ALL      PIC X(60).
       01 WS-RESULT-SPACES   PIC X(60).
       01 WS-CHAR            PIC X(01).
       01 WS-LENGTH          PIC 9(03).
       01 WS-GROUP           PIC 9(03).
       01 WS-IND             PIC 9(03).
       01 WS-INDEX           PIC 9(02).
       01 WS-INDEX-SPACES    PIC 9(02).
       01 WS-PLAIN-ALPHABET  PIC X(26)
            VALUE 'abcdefghijklmnopqrstuvwxyz'.
       01 WS-CIPHER-ALPHABET PIC X(26)
            VALUE 'zyxwvutsrqponmlkjihgfedcba'.
       PROCEDURE DIVISION.

       ENCODE.
         PERFORM INIT
         INSPECT WS-RESULT-SPACES CONVERTING WS-PLAIN-ALPHABET
           TO WS-CIPHER-ALPHABET
         PERFORM REMOVE-SPACES
         PERFORM GROUP-BY-5
        .
      
       DECODE.
         PERFORM INIT
         INSPECT WS-RESULT-SPACES CONVERTING WS-CIPHER-ALPHABET
           TO WS-PLAIN-ALPHABET
         PERFORM REMOVE-SPACES
         MOVE WS-RESULT-ALL TO WS-RESULT
       .

       INIT.
         INITIALIZE WS-RESULT
                    WS-RESULT-SPACES
                    WS-RESULT-ALL
         INSPECT WS-PHRASE REPLACING ALL '-' BY ' '
         INSPECT WS-PHRASE REPLACING ALL ',' BY ' '
         INSPECT WS-PHRASE REPLACING ALL '.' BY ' '
         MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-PHRASE))
           TO WS-LENGTH
         MOVE FUNCTION LOWER-CASE(WS-PHRASE) TO WS-RESULT-SPACES
       .
      
       REMOVE-SPACES.
        MOVE 1 TO WS-INDEX
        PERFORM VARYING WS-INDEX-SPACES FROM 1 BY 1 
         UNTIL WS-INDEX-SPACES > WS-LENGTH
          MOVE WS-RESULT-SPACES(WS-INDEX-SPACES:1) TO WS-CHAR
          IF WS-CHAR NOT EQUAL TO SPACE
            MOVE WS-CHAR TO WS-RESULT-ALL(WS-INDEX:1)
            ADD 1 TO WS-INDEX
          END-IF
        END-PERFORM
       .
      
       GROUP-BY-5.
        MOVE 0 TO WS-GROUP
        PERFORM VARYING WS-INDEX FROM 1 BY 5 
         UNTIL WS-INDEX > WS-LENGTH
          COMPUTE WS-IND = WS-INDEX + 1*WS-GROUP
          MOVE WS-RESULT-ALL(WS-INDEX:5)
            TO WS-RESULT(WS-IND:5)
          ADD 1 TO WS-GROUP
        END-PERFORM
       .

       UNGROUP-BY-5.
        MOVE 0 TO WS-GROUP
        PERFORM VARYING WS-INDEX FROM 1 BY 5 
         UNTIL WS-INDEX > WS-LENGTH
          COMPUTE WS-IND = WS-INDEX + 1*WS-GROUP
          MOVE WS-RESULT-ALL(WS-IND:5)
            TO WS-RESULT(WS-INDEX:5)
          ADD 1 TO WS-GROUP
        END-PERFORM
       .