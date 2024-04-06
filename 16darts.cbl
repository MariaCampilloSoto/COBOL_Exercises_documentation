       IDENTIFICATION DIVISION.
       PROGRAM-ID. DARTS.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-X PIC 99V9.
       01 WS-Y PIC 99V9.
       01 WS-RESULT PIC 99.
       01 WS-RADIUS PIC 99V99.
       01 POINTS.
        02 WS-OUTSIDE PIC 99 VALUE 00.
        02 WS-OUTER   PIC 99 VALUE 01.
        02 WS-MIDDLE  PIC 99 VALUE 05.
        02 WS-INNER   PIC 99 VALUE 10.
     
       PROCEDURE DIVISION.
       DARTS.
         PERFORM CALCULATE-RADIUS
         EVALUATE TRUE
            WHEN WS-RADIUS <= 1.0
               MOVE WS-INNER TO WS-RESULT
            WHEN WS-RADIUS <= 5.0
               MOVE WS-MIDDLE TO WS-RESULT
            WHEN WS-RADIUS <= 10.0
               MOVE WS-OUTER TO WS-RESULT
            WHEN OTHER
               MOVE WS-OUTSIDE TO WS-RESULT
         END-EVALUATE
       .

      *// CENTER = (0,0) --> (H,K)
      *// R^2 = (X-H)^2 + (Y-K)^2
      *// R = SQRT((X-H)^2 + (Y-K)^2)
       CALCULATE-RADIUS.
       COMPUTE WS-RADIUS =
          FUNCTION SQRT((WS-X - 0)**2 + (WS-Y - 0)**2)
       .