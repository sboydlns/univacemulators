                   LQ,W      1                   . ASSIGNMENT
                   SQ,W      I
       .
                   J         LOOP
       .
       INCR        LQ,W      1                   . INCREMENT
                   LA,W      I
                   AQ,A
                   SQ,W      I
       .
       LOOP        BOOLEAN EXPR
                   .
                   .
                   SQ,A
                   JT        DONE,,ANOT
                   .
                   .
                   .
                   J         INCR
       .                   
       DONE                           
                                                                           