                   START     BEGIN
       .
                   ER$DEF
       . 
                   EDEF      ALG$STACK
       .                                     
                   XREF      ALG$INIT
                   XREF      PUSHA,PUSHQ,PUSHAQ
                   XREF      POPA,POPQ,POPAQ
                   XREF      FM$GETMEM,FM$FREEMEM,NEWARRAY
                   XREF      ARRAYADR,NEWSTRING,STRCPY,STRCOMP,
                   XREF      INT2STRR, TRUNC, FRAC, ENTIER
                   XREF      INTEGER, FLT2STR
                   
                   XREF      POWERI
       .
                   XREF      SETSYSFILE, WR$INT, WR$STR, WR$FLUSH                   
       .
       BEGIN       LBPJB1    ALG$INIT
                   +STACKLEN
       .
                   LQ        12D
                   LBPJB1    NEWSTRING
                   SQ,W      S1
                   LB,W      B5,S1
                   DPL       MSG1
                   DPS       1,B5
       .                   
                   LQ        10D
                   LBPJB1    NEWSTRING
                   SQ,W      S2
                   LB,W      B5,S2 
                   DPL       MSG2
                   DPS       1,B5
       .
       . 
                   LQ,W      S1
                   LBPJB1    PUSHQ
                   LQ        1D
                   LBPJB1    PUSHQ
                   LQ        12D
                   LBPJB1    PUSHQ
                   DPL       F1
                   LBPJB1    FLT2STR
       .                   
                   LQ        4D
                   LBPJB1    SETSYSFILE
       .
                   LQ,W      S1
                   LBPJB1    PUSHQ
                   LQ        1D
                   LBPJB1    PUSHQ
                   LQ        10D
                   LBPJB1    PUSHQ
                   LBPJB1    WR$STR                   
       .
                   LBPJB1    POWERI
                   DLD       10.0
                   +4
                   LBPJB1    INTEGER
                   LBPJB1    WR$INT            
       .                                                                                                                
                   LQ,W      S2
                   LBPJB1    PUSHQ
                   LQ        1D
                   LBPJB1    PUSHQ
                   LQ        10D
                   LBPJB1    PUSHQ
                   LBPJB1    WR$STR 
       .                                     
                   LBPJB1    POWERI
                   DLD       10.0
                   +5            
                   LBPJB1    INTEGER
                   LBPJB1    WR$INT 
                   LBPJB1    WR$FLUSH           
       .                                                                                                                
                   HALT      0       
       
       
       .                   
       S1          RES       1D
       S2          RES       1D
       MSG1        +'VALUE 1 ='
       MSG2        +'VALUE 2 ='
       F1          DLD       123.456
       .
       STACKLEN    EQU       100D
       ALG$STACK   RES       STACKLEN                   
                   
                   
                   
                   END