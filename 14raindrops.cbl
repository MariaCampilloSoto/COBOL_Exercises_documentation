       IDENTIFICATION DIVISION.
       PROGRAM-ID. raindrops.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NUMBER PIC 9(4).
       01 WS-RESULT PIC X(20).
       01 WS-VALUE PIC 9(4).
       01 WS-REM-3 PIC 9(4).
       01 WS-REM-5 PIC 9(4).
       01 WS-REM-7 PIC 9(4).

       PROCEDURE DIVISION.
       RAINDROPS.
      *// COMPUTE WS-MOD-3 = FUNCTION REM(WS-NUMBER, 3)
         DIVIDE WS-NUMBER BY 3 GIVING WS-VALUE REMAINDER WS-REM-3
         DIVIDE WS-NUMBER BY 5 GIVING WS-VALUE REMAINDER WS-REM-5
         DIVIDE WS-NUMBER BY 7 GIVING WS-VALUE REMAINDER WS-REM-7
      
         IF WS-REM-3 EQUAL 0
            MOVE 'Pling' TO WS-RESULT
         END-IF
         IF WS-REM-5 EQUAL 0
            STRING WS-RESULT DELIMITED BY SPACES 'Plang'
               INTO WS-RESULT
         END-IF
         IF WS-REM-7 EQUAL 0
            STRING WS-RESULT DELIMITED BY SPACES 'Plong'
               INTO WS-RESULT 
         END-IF
         IF WS-RESULT EQUAL SPACES
            MOVE WS-NUMBER TO WS-RESULT
         END-IF
       .