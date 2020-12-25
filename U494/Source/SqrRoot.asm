                   START     BEGIN
       .
       . CALCULATE THE SQUARE ROOT OF THE NUMBER IN A.
       . THIS IS DONE BY REPEATED SUBTRACTION. WE SUBTRACT
       . 3 THEN 5 THEN 7, ETC. UNTIL A BECOMES NEGATIVE.
       . THE REPEAT COUNT IN B7 WILL BE THE COMPLEMENT OF THE
       . SQUARE ROOT.
       .
       . WHEN THE PROGRAM STOPS YOU MUST ENTER A NUMBER
       . INTO A AND PRESS START. THE ANSWER IS RETURNED
       . IN Q.
       . 
       BEGIN       J         $+1,,STOP
                   LB        B2,2
                   R         77777,,ADDB
                   AN        1,B2,ANEG
                   J         HALT
                   SB,CPW    B7,WORD
                   LQ,W      WORD
       HALT        J         $,,STOP
       WORD        +0
                   END
