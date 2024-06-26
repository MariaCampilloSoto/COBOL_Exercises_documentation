       IDENTIFICATION DIVISION.
       PROGRAM-ID. armstrong-numbers.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY. FUNCTION ALL INTRINSIC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NUMBER PIC X(8).
       01 WS-RESULT PIC 9 VALUE 0.
        88 YES-ARMSTRONG  VALUE 1.
        88 NO-ARMSTRONG   VALUE 0.
       01 WS-LENGTH PIC 9(8).
       01 WS-IND    PIC 9.
       01 WS-RAISED PIC 9(8).
       01 WS-SUM    PIC 9(8).
      *// IMPORTANT TO BE Z --> IF YOU DONT WANT LEADING 0s
       01 WS-NUMBER-SUM PIC Z(8).
       01 WS-DIGIT  PIC 9.

       PROCEDURE DIVISION.
       ARMSTRONG-NUMBERS.
        INITIALIZE WS-IND
                   WS-SUM
        MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-NUMBER))
         TO WS-LENGTH
      
        PERFORM VARYING WS-IND FROM 1 BY 1 UNTIL WS-IND > WS-LENGTH
          MOVE WS-NUMBER(WS-IND:1) TO WS-DIGIT
          COMPUTE WS-RAISED = (WS-DIGIT)**WS-LENGTH
          ADD WS-RAISED TO WS-SUM
        END-PERFORM

        MOVE WS-SUM TO WS-NUMBER-SUM
        IF WS-NUMBER EQUAL FUNCTION TRIM(WS-NUMBER-SUM) 
        OR WS-NUMBER EQUAL 0
         SET YES-ARMSTRONG TO TRUE
        ELSE
         SET NO-ARMSTRONG TO TRUE
        END-IF
       .