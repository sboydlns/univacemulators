                   START
       .                   
       . UNIVAC 494 ALGOL MATH LIBRARY                   
       .
                   XREF      TRUNC, FRAC
       . ++++++++++
       . POWERI
       .
       . RAISE A FLOATING POINT NUMBER TO AN INTEGER POWER.
       .
       . PARAMETERS:
       .   FLOATING POINT NUMBER
       .   INTEGER EXPONENT
       .
       . RETURNS:
       .   RESULT RETURNED IN AQ
       .
       . EXPONENTIATION IS ACHIEVED BY SQUARING TO REDUCE COMPUTATION TIME.
       . ++++++++++
       POWERI*     DPL       0,B1                . GET NUMBER
                   DPS       P$X
                   DPTE      P$ZERO              . IS IT ZERO?
                   J         P$CHKEXP            . NO, OK SO FAR
                   J         P$RTRN              . ZERO TO ANY POWER IS ZERO                   
       P$CHKEXP    LA,W      2,B1                . GET EXPONENT 
                   SA,W      P$N 
       .                     
                   JT        P$NOTZERO,,ANOT     . IS EXPONENT ZERO?
                   DPL       P$ONE               . YES, RESULT = 1
                   J         P$RTRN                   
       P$NOTZERO   JT        P$NOTNEG,,APOS      . IS EXPONENT NEGATIVE
                   NA                            . YES, INVERT EXPONENT AND
                   SA,W      P$N                 . NUMBER
                   DPL       P$ONE
                   FD        P$X
                   DPS       P$X
       .                   
       P$NOTNEG    DPL       P$ONE
                   DPS       P$Y
       P$LOOP      LQ,W      P$N                 . N <= 1?
                   TQ        2D,,YLESS
                   J         P$DONE              . YES, WE'RE DONE
                   LQ        1D                  . IS N EVEN
                   LLP,W     P$N,,ANOT
                   J         P$EVEN              . YES                                      
                   DPL       P$Y                 . NO, Y = Y * X
                   FM        P$X
                   DPS       P$Y
                   DPL       P$X                 . X = X * X
                   FM        P$X
                   DPS       P$X
                   LQ,W      P$N                 . N = (N - 1) // 2
                   ANQ       1D
                   LRSQ      1D
                   SQ,W      P$N
                   J         P$LOOP
       .                   
       P$EVEN      DPL       P$X                 . X = X * X
                   FM        P$X
                   DPS       P$X
                   LQ,W      P$N                 . N = N // 2
                   LRSQ      1D
                   SQ,W      P$N
                   J         P$LOOP       
       .
       P$DONE      DPL       P$X                 . RESULT = X * Y
                   FM        P$Y
       .                                      
       P$RTRN      J         3,B1                . RETURN                               
       . ++++++++++
       . POWER
       .
       . RAISE A FLOATING POINT NUMBER TO A FLOATING POINT POWER.
       .
       . PARAMETERS:
       .   FLOATING POINT NUMBER
       .   FLOATING POINT POWER
       .
       . RETURNS:
       .   RESULT RETURNED IN AQ
       .
       . NOT FULLY IMPLEMENTED AT THIS TIME
       . ++++++++++
       POWER*      SB,W      B1,P$B1             . SAVE RETURN ADDR
                   DPL       2,B1                . GET THE EXPONENT
                   LBPJB1    FRAC
                   DPTE      P$ZERO              . IS IT AN INTEGER?
                   J         P$FEXP              . NO
                   LB,W      B1,P$B1             . YES, USE POWERI INSTEAD
                   DPL       0,B1                . GET NUMBER
                   DPS       $+5
                   DPL       2,B1                . GET THE EXPONENT
                   LBPJB1    TRUNC               . GET THE INTEGER PART
                   SQ,W      $+4
                   LBPJB1    POWERI
                   DLD       0.0
                   +0
                   LB,W      B1,P$B1
                   J         4,B1
       .
       P$FEXP      . RAISING TO A FLOATING POINT POWER NOT IMPLEMENTED YET.
                   LB,W      B1,P$B1
                   J         4,B1                               
       .
       P$X         RES       2D
       P$Y         RES       2D
       P$N         RES       1D
       P$B1        RES       1D
       P$ONE       DLD       1.0
       P$ZERO      DLD       0.0
       .       
                   END