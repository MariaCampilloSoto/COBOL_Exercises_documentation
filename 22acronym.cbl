       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACRONYM.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-ACRONYM        PIC X(80).
       01 WS-RESULT         PIC X(20).
       01 WS-INDEX          PIC 9(3) VALUE 1.
       01 WS-CHAR           PIC X.
       01 WS-POS            PIC 99.
      
       PROCEDURE DIVISION.
       ABBREVIATE.
        INITIALIZE WS-CHAR
                   WS-RESULT
      
        INSPECT WS-ACRONYM REPLACING ALL '-' BY ' '
        INSPECT WS-ACRONYM REPLACING ALL ',' BY ' '
        INSPECT WS-ACRONYM REPLACING ALL '_' BY ' '

        MOVE FUNCTION UPPER-CASE(WS-ACRONYM) TO WS-ACRONYM
        DISPLAY WS-ACRONYM
      *// FIRST LETTER
        MOVE WS-ACRONYM(1:1) TO WS-RESULT(1:1)
        MOVE 2 TO WS-POS
        MOVE 2 TO WS-INDEX

        PERFORM UNTIL WS-INDEX > (LENGTH OF WS-ACRONYM - 1)
           IF WS-ACRONYM(WS-INDEX:1) EQUAL SPACE 
           AND WS-ACRONYM(WS-INDEX + 1:1) NOT EQUAL SPACE
               ADD 1 TO WS-INDEX
               MOVE WS-ACRONYM(WS-INDEX:1) TO WS-CHAR
               MOVE WS-CHAR TO WS-RESULT(WS-POS:1)
               ADD 1 TO WS-POS

           END-IF
           ADD 1 TO WS-INDEX
        END-PERFORM

       .