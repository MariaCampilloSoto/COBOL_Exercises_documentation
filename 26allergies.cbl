       IDENTIFICATION DIVISION.
       PROGRAM-ID. allergies.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SCORE       PIC 999.
       01 WS-ITEM        PIC X(12).
       01 WS-RESULT      PIC A.
        88 YES-ALLERGIES VALUE 'Y'.
        88 NO-ALLERGIES  VALUE 'N'.
       01 WS-RESULT-LIST PIC X(108).
       01 WS-REMAINDER   PIC 9.
       01 WS-INDEX       PIC 9.
       01 WS-BINARY-TABLE.
        05 WS-DIGIT      OCCURS 8 PIC 9.
       01 WS-ALLERGIES-TABLE.
        05 WS-ALLERGY    OCCURS 8 PIC X(12).

       PROCEDURE DIVISION.
       ALLERGIC-TO.
        SET NO-ALLERGIES TO TRUE
        PERFORM CONVERT-DECIMAL-TO-BINARY

        EVALUATE WS-ITEM ALSO TRUE
          WHEN 'eggs'         ALSO WS-DIGIT(1) EQUAL 1
          WHEN 'peanuts'      ALSO WS-DIGIT(2) EQUAL 1
          WHEN 'shellfish'    ALSO WS-DIGIT(3) EQUAL 1
          WHEN 'strawberries' ALSO WS-DIGIT(4) EQUAL 1
          WHEN 'tomatoes'     ALSO WS-DIGIT(5) EQUAL 1
          WHEN 'chocolate'    ALSO WS-DIGIT(6) EQUAL 1
          WHEN 'pollen'       ALSO WS-DIGIT(7) EQUAL 1
          WHEN 'cats'         ALSO WS-DIGIT(8) EQUAL 1
            SET YES-ALLERGIES TO TRUE
       END-EVALUATE
      
       .

       LIST-ALLERGENS.
          INITIALIZE WS-ALLERGIES-TABLE
                     WS-RESULT-LIST
          STRING 'eggs        '
                 'peanuts     '
                 'shellfish   '
                 'strawberries'
                 'tomatoes    ' 
                 'chocolate   '
                 'pollen      '
                 'cats        ' 
                 DELIMITED BY SIZE INTO WS-ALLERGIES-TABLE
          PERFORM CONVERT-DECIMAL-TO-BINARY
          PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > 8
            IF WS-DIGIT(WS-INDEX) EQUAL 1
                STRING WS-RESULT-LIST ',' WS-ALLERGY(WS-INDEX) 
                       DELIMITED BY SPACE INTO WS-RESULT-LIST
            END-IF
          END-PERFORM
      *// REMOVE THE FIRST COMMA <,EGGS,...>
          MOVE WS-RESULT-LIST(2:) TO WS-RESULT-LIST
       .
      
       CONVERT-DECIMAL-TO-BINARY.
          INITIALIZE WS-BINARY-TABLE
          PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > 8
            DIVIDE WS-SCORE BY 2 
              GIVING WS-SCORE REMAINDER WS-REMAINDER
            MOVE WS-REMAINDER TO WS-DIGIT(WS-INDEX)
          END-PERFORM
       .
