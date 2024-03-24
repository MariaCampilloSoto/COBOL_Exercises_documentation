       IDENTIFICATION DIVISION.
       PROGRAM-ID. YACHT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
          01 WS-RESULT PIC 99 VALUE 0.
          01 WS-CATEGORY PIC X(15).
          01 WS-DICE PIC 9(5).
          01 VARIABLES.
            02 WS-DICE-VALUE PIC 9.
            02 WS-FIRST-VALUE PIC 9.
            02 WS-SECOND-VALUE PIC 9.
            02 WS-VALUE PIC 9.
            02 WS-TOTAL PIC 99 VALUE 0.
            02 WS-PAIR-A PIC S9.
            02 WS-PAIR-B PIC 9.
            02 WS-RESULT-A PIC 99 VALUE 0.
            02 WS-RESULT-B PIC 99 VALUE 0.
          01 CATEGORIES.
            02 LT-ONES PIC X(4) VALUE 'ones'.
            02 LT-TWOS PIC X(4) VALUE 'twos'.
            02 LT-THREES PIC X(6) VALUE 'threes'.
            02 LT-FOURS PIC X(5) VALUE 'fours'.
            02 LT-FIVES PIC X(5) VALUE 'fives'.
            02 LT-SIXES PIC X(5) VALUE 'sixes'.
            02 LT-FULL PIC X(10) VALUE 'full house'.
            02 LT-FOURK PIC X(14) VALUE 'four of a kind'.
            02 LT-LITTLES PIC X(15) 
                       VALUE 'little straight'.
            02 LT-BIGS PIC X(12) VALUE 'big straight'.
            02 LT-CHOICE PIC X(6) VALUE 'choice'.
            02 LT-YACHT PIC X(5) VALUE 'yacht'.
          01 INDICES.
            02 IND PIC 9 VALUE 1.
       PROCEDURE DIVISION.
         YACHT.
            INITIALIZE WS-RESULT
                       WS-TOTAL
                       WS-PAIR-A
                       WS-PAIR-B
                       WS-RESULT-A
                       WS-RESULT-B
            EVALUATE WS-CATEGORY
               WHEN LT-ONES
                  MOVE 1 TO IND
                  MOVE 1 TO WS-VALUE
                  PERFORM SUM-VALUE UNTIL IND > 5
               WHEN LT-TWOS
                  MOVE 1 TO IND
                  MOVE 2 TO WS-VALUE
                  PERFORM SUM-VALUE UNTIL IND > 5
               WHEN LT-THREES
                  MOVE 1 TO IND
                  MOVE 3 TO WS-VALUE
                  PERFORM SUM-VALUE UNTIL IND > 5
               WHEN LT-FOURS
                  MOVE 1 TO IND
                  MOVE 4 TO WS-VALUE
                  PERFORM SUM-VALUE UNTIL IND > 5
               WHEN LT-FIVES
                  MOVE 1 TO IND
                  MOVE 5 TO WS-VALUE
                  PERFORM SUM-VALUE UNTIL IND > 5
               WHEN LT-SIXES
                  MOVE 1 TO IND
                  MOVE 6 TO WS-VALUE
                  PERFORM SUM-VALUE UNTIL IND > 5
               WHEN LT-FULL
                  MOVE 1 TO IND
                  MOVE WS-DICE(IND:1) TO WS-FIRST-VALUE
                  MOVE 2 TO WS-PAIR-A
                  MOVE 3 TO WS-PAIR-B
                  PERFORM FIND-PAIR UNTIL IND > 5
                  IF WS-PAIR-A EQUAL 0
                  AND WS-PAIR-B EQUAL 0
                     ADD WS-RESULT-A TO WS-RESULT-B
                     GIVING WS-RESULT
                  ELSE
                     MOVE 0 TO WS-RESULT
                  END-IF
               WHEN LT-FOURK
                  MOVE 1 TO IND
                  MOVE WS-DICE(IND:1) TO WS-FIRST-VALUE
                  MOVE 4 TO WS-PAIR-A
                  MOVE 1 TO WS-PAIR-B
                  PERFORM FIND-PAIR UNTIL IND > 5
                  IF (WS-PAIR-A EQUAL 0
                    AND WS-PAIR-B EQUAL 0)
                  OR WS-PAIR-A EQUAL -1
                     MOVE WS-RESULT-A TO WS-RESULT
                  ELSE
                     MOVE 0 TO WS-RESULT
                  END-IF
               WHEN LT-LITTLES
      *//LAZY METHOD, I KNOW. 12345=15
                  PERFORM SUM-DICE-VALUES
                  IF WS-TOTAL EQUAL 15
                    MOVE 30 TO WS-RESULT
                  END-IF
               WHEN LT-BIGS
      *//LAZY METHOD, I KNOW. 23456=20
                  PERFORM SUM-DICE-VALUES
                  IF WS-TOTAL EQUAL 20
                    MOVE 30 TO WS-RESULT
                  END-IF
               WHEN LT-CHOICE
                  PERFORM SUM-DICE-VALUES
                  MOVE WS-TOTAL TO WS-RESULT
               WHEN LT-YACHT
                  MOVE 1 TO IND
                  MOVE WS-DICE(IND:1) TO WS-FIRST-VALUE
                  MOVE 50 TO WS-RESULT
                  PERFORM SAME-VALUE UNTIL IND > 5
               WHEN OTHER
                  MOVE 0 TO WS-RESULT
            END-EVALUATE
         .
         SUM-VALUE.
            MOVE WS-DICE(IND:1) TO WS-DICE-VALUE
            IF WS-DICE-VALUE EQUAL WS-VALUE
               ADD WS-DICE-VALUE TO WS-RESULT
            END-IF
            ADD 1 TO IND
         .
         SAME-VALUE.
            MOVE WS-DICE(IND:1) TO WS-DICE-VALUE
            IF WS-DICE-VALUE NOT EQUAL WS-FIRST-VALUE
              MOVE 0 TO WS-RESULT
            END-IF
            ADD 1 TO IND
         .
         SUM-DICE-VALUES.
            PERFORM VARYING IND FROM 1 BY 1 UNTIL IND > 5
               MOVE WS-DICE(IND:1) TO WS-DICE-VALUE
               ADD WS-DICE-VALUE TO WS-TOTAL
            END-PERFORM
         .
         FIND-PAIR.
            MOVE WS-DICE(IND:1) TO WS-DICE-VALUE
            IF WS-DICE-VALUE EQUAL WS-FIRST-VALUE
              ADD WS-DICE-VALUE TO WS-RESULT-A
              SUBTRACT 1 FROM WS-PAIR-A
            ELSE
              IF WS-PAIR-B EQUAL 3
                 MOVE WS-DICE-VALUE TO WS-SECOND-VALUE
              END-IF
            END-IF
            IF WS-DICE-VALUE EQUAL WS-SECOND-VALUE
               ADD WS-DICE-VALUE TO WS-RESULT-B
               SUBTRACT 1 FROM WS-PAIR-B
            END-IF
      *// CASE: Yacht is not a full house, the addition is different
            IF WS-PAIR-A EQUAL -1
              SUBTRACT WS-DICE-VALUE FROM WS-RESULT-A
            END-IF
            ADD 1 TO IND
         .
   

