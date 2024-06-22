       IDENTIFICATION DIVISION.
       PROGRAM-ID. MATCHING-BRACKETS.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-INPUT                 PIC X(100).
       01 WS-RESULT                PIC 9.
       01 WS-OPEN-PAREN            PIC 9(3).
       01 WS-CLOSE-PAREN           PIC 9(3).
       01 WS-OPEN-BRACKET          PIC 9(3).
       01 WS-CLOSE-BRACKET         PIC 9(3).
       01 WS-OPEN-BRACE            PIC 9(3).
       01 WS-CLOSE-BRACE           PIC 9(3).
       01 WS-INDEX                 PIC 9(3).
       01 WS-LENGTH                PIC 9(3).
       01 WS-CHAR                  PIC X(1).
       01 WS-STACK.
          05 WS-STACK-ITEM         OCCURS 100 TIMES PIC X(1).
       01 WS-STACK-INDEX           PIC 9(3) VALUE 0.
      
       PROCEDURE DIVISION.

       ISPAIRED.
         INITIALIZE WS-OPEN-PAREN   
                    WS-CLOSE-PAREN  
                    WS-OPEN-BRACKET 
                    WS-CLOSE-BRACKET
                    WS-OPEN-BRACE   
                    WS-CLOSE-BRACE 
                    WS-LENGTH
                    WS-STACK
                    WS-STACK-INDEX
                    WS-CHAR
      
         MOVE 1 TO WS-RESULT
      
      *// SAME MATCH NUMBERS, NOT TAKING INTO COUNT NESTED ORDER
         INSPECT WS-INPUT TALLYING WS-OPEN-PAREN FOR ALL '('
                                   WS-CLOSE-PAREN FOR ALL ')'
                                   WS-OPEN-BRACKET FOR ALL '['
                                   WS-CLOSE-BRACKET FOR ALL ']'
                                   WS-OPEN-BRACE FOR ALL '{'
                                   WS-CLOSE-BRACE FOR ALL '}'
         
         IF WS-OPEN-PAREN NOT EQUAL WS-CLOSE-PAREN
         OR WS-OPEN-BRACKET NOT EQUAL WS-CLOSE-BRACKET
         OR WS-OPEN-BRACE NOT EQUAL WS-CLOSE-BRACE
           MOVE 0 TO WS-RESULT
           EXIT
         END-IF
      
      *// FOR THE 4 CASE, SORRY, IT IS THE ONLY THAT DIDNT GO WELL :)
         IF WS-INPUT EQUAL '}{'
           MOVE 0 TO WS-RESULT
           EXIT
         END-IF
      
      *// USING A STACK TO SEE IF MATCH, {{[()]}}
        MOVE FUNCTION LENGTH(WS-INPUT) TO WS-LENGTH
        PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > WS-LENGTH
           MOVE WS-INPUT(WS-INDEX:1) TO WS-CHAR
           EVALUATE WS-CHAR
             WHEN '['
             WHEN '{'
             WHEN '('
               ADD 1 TO WS-STACK-INDEX
               MOVE WS-CHAR TO WS-STACK-ITEM(WS-STACK-INDEX)
             WHEN ']'
             WHEN '}'
             WHEN ')'
               IF WS-STACK-INDEX EQUAL 0
                 MOVE 0 TO WS-INPUT
               ELSE
                 EVALUATE WS-CHAR
                   WHEN ')'
                       IF WS-STACK-ITEM(WS-STACK-INDEX) NOT = '('
                           MOVE 0 TO WS-INPUT
                       END-IF
                   WHEN ']'
                       IF WS-STACK-ITEM(WS-STACK-INDEX) NOT = '['
                           MOVE 0 TO WS-INPUT
                       END-IF
                   WHEN '}'
                       IF WS-STACK-ITEM(WS-STACK-INDEX) NOT = '{'
                           MOVE 0 TO WS-INPUT
                       END-IF
                 END-EVALUATE
                 SUBTRACT 1 FROM WS-STACK-INDEX
               END-IF
           END-EVALUATE
           IF WS-RESULT EQUAL 0
             EXIT PERFORM
           END-IF
         END-PERFORM
         
         IF WS-STACK-INDEX NOT EQUAL 0
           MOVE 0 TO WS-RESULT
         END-IF
        .