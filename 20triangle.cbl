       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRIANGLE.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *Incoming
       01 WS-SIDES PIC X(20).
       01 WS-PROPERTY PIC X(11).
      *Outgoing
       01 WS-RESULT PIC 9.
        88 YES-TRIANGLE VALUE 1.
        88 NO-TRIANGLE VALUE 0.
       01 SIDES.
        02 WS-A PIC 9(2)V9(1).
        02 WS-B PIC 9(2)V9(1).
        02 WS-C PIC 9(2)V9(1).
      
       PROCEDURE DIVISION.
       TRIANGLE.
        SET NO-TRIANGLE TO TRUE
      
      *// GET SIDES VALUES
        UNSTRING WS-SIDES DELIMITED BY ',' INTO WS-A, WS-B, WS-C

      *// TRIANGLE PROPERTIES
        IF ((WS-A + WS-B) GREATER THAN OR EQUAL TO WS-C)
        AND ((WS-B + WS-C) GREATER THAN OR EQUAL TO WS-A)
        AND ((WS-A + WS-C) GREATER THAN OR EQUAL TO WS-B)
         EVALUATE WS-PROPERTY
            WHEN 'equilateral'
             IF WS-A EQUAL WS-B AND WS-B EQUAL WS-C
              SET YES-TRIANGLE TO TRUE
             END-IF
   
            WHEN 'isosceles'
             IF (WS-A EQUAL WS-B) OR (WS-A EQUAL WS-C) OR (WS-B EQUAL WS-C)
               SET YES-TRIANGLE TO TRUE
             END-IF
         
            WHEN 'scalene'
             IF WS-A NOT EQUAL WS-B AND WS-B NOT EQUAL WS-C 
             AND WS-A NOT EQUAL WS-C
              SET YES-TRIANGLE TO TRUE
             END-IF
         
            WHEN OTHER
             SET NO-TRIANGLE TO TRUE
           END-EVALUATE
        END-IF

        IF WS-A EQUAL 0 AND WS-B EQUAL 0 AND WS-C EQUAL 0
         SET NO-TRIANGLE TO TRUE
        END-IF
       .