       IDENTIFICATION DIVISION.
       PROGRAM-ID. BINARY.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-BINARY                PIC X(60).
       01 WS-RESULT                PIC 9999.
       01 WS-ERROR                 PIC X(60).
       01 WS-IND                   PIC 99.
       01 WS-N                     PIC 99.
       01 WS-LENGTH                PIC 99.
       01 WS-DIGIT                 PIC 9.
       01 WS-COUNT-LETTER          PIC 99.
       01 WS-COUNT-DECIMAL         PIC 99.
       
       PROCEDURE DIVISION.
       DECIMAL.
         INITIALIZE WS-RESULT
                    WS-COUNT-DECIMAL
                    WS-COUNT-LETTER
         MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-BINARY))
           TO WS-LENGTH
         MOVE WS-LENGTH TO WS-N
         PERFORM VARYING WS-IND FROM 1 BY 1
          UNTIL WS-IND > WS-LENGTH
           MOVE WS-BINARY(WS-IND:1) TO WS-DIGIT
           COMPUTE WS-RESULT = WS-RESULT 
                             + WS-DIGIT * (2)**(WS-N - 1)
           SUBTRACT 1 FROM WS-N
         END-PERFORM

         INSPECT WS-BINARY TALLYING WS-COUNT-LETTER FOR CHARACTERS
         IF WS-COUNT-LETTER > 0
          MOVE "error: a number containing non-binary characters is invalid"
           TO WS-ERROR
         END-IF
      
         INSPECT WS-BINARY TALLYING WS-COUNT-DECIMAL  
           FOR ALL '2' '3' '4' '5' '6' '7' '8' '9'
         IF WS-COUNT-DECIMAL > 0
          MOVE "error: a number containing non-binary digits is invalid"
           TO WS-ERROR
         END-IF
       .