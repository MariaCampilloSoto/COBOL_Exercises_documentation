       IDENTIFICATION DIVISION.
       PROGRAM-ID. two-fer.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NAME PIC X(16).
       01 WS-RESULT PIC X(64).
       01 WS-LENGTH PIC 99.
       
       PROCEDURE DIVISION.
       TWO-FER.
        IF WS-NAME EQUAL SPACES
         MOVE 'One for you, one for me.'
          TO WS-RESULT
        ELSE
         MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-NAME)) TO WS-LENGTH
         MOVE 'One for ' TO WS-RESULT(1:8)
         MOVE WS-NAME    TO WS-RESULT(9:WS-LENGTH)
         MOVE ', one for me.'
           TO WS-RESULT(9 + WS-LENGTH:13)
        END-IF.
