                   START     BEGIN
       .                   
                   ER$DEF
       .                   
       BEGIN
                   LA        12345D
                   LBPJB1    INT2FLOAT
                   LBPJB1    FLT2STR
                   +3D
                   +FSTR
                   DPL       FSTR
                   DPS       OP1
                   DPL       FS$SPACES
                   DPS       OP2
                   DPS       RSLT
                   LA,W      FS$SPACES
                   SA,W      OPR
                   TYPE      MLEN,MSG                                                         
       .                                      
                   DPL       F123
                   LBPJB1    FLT2STR
                   +3D
                   +FSTR
                   DPL       FSTR
                   DPS       OP1
                   DPL       F456
                   LBPJB1    FLT2STR
                   +3D
                   +FSTR
                   DPL       FSTR
                   DPS       OP2
                   LA,W      PLUS
                   SA,W      OPR
                   DPL       F123
                   FA        F456
                   DPS       F579
                   LBPJB1    FLT2STR
                   +3D
                   +FSTR
                   DPL       FSTR
                   DPS       RSLT
                   TYPE      MLEN,MSG
       .
                   DPL       F123
                   DPN
                   LBPJB1    FLT2STR
                   +3D
                   +FSTR
                   DPL       FSTR
                   DPS       OP1
                   DPL       F456
                   LBPJB1    FLT2STR
                   +3D
                   +FSTR
                   DPL       FSTR
                   DPS       OP2
                   LA,W      PLUS
                   SA,W      OPR
                   DPL       F123
                   DPN
                   FA        F456
                   DPS       F579
                   LBPJB1    FLT2STR
                   +3D
                   +FSTR
                   DPL       FSTR
                   DPS       RSLT
                   TYPE      MLEN,MSG
       .
                   DPL       F579 
                   LBPJB1    FLT2STR
                   +3D
                   +FSTR
                   DPL       FSTR
                   DPS       OP1
                   DPL       F456
                   LBPJB1    FLT2STR
                   +3D
                   +FSTR
                   DPL       FSTR
                   DPS       OP2
                   LA,W      MINUS
                   SA,W      OPR
                   DPL       F579
                   FAN       F456
                   DPS       F123
                   LBPJB1    FLT2STR
                   +3D
                   +FSTR
                   DPL       FSTR
                   DPS       RSLT
                   SA,W      RSLT
                   TYPE      MLEN,MSG
       .                                      
                   DPL       F456  
                   LBPJB1    FLT2STR
                   +3D
                   +FSTR
                   DPL       FSTR
                   DPS       OP1
                   DPL       F123
                   LBPJB1    FLT2STR
                   +3D
                   +FSTR
                   DPL       FSTR
                   DPS       OP2
                   LA,W      DIVIDE
                   SA,W      OPR
                   DPL       F456
                   FD        F123
                   DPS       FRSLT
                   LBPJB1    FLT2STR
                   +3D
                   +FSTR
                   DPL       FSTR
                   DPS       RSLT
                   TYPE      MLEN,MSG
       .
                   DPL       FRSLT
                   LBPJB1    FLT2STR
                   +3D
                   +FSTR
                   DPL       FSTR
                   DPS       OP1
                   DPL       F123
                   LBPJB1    FLT2STR
                   +3D
                   +FSTR
                   DPL       FSTR
                   DPS       OP2
                   LA,W      MULTIPLY
                   SA,W      OPR
                   DPL       FRSLT
                   FM        F123
                   LBPJB1    FLT2STR
                   +3D
                   +FSTR
                   DPL       FSTR
                   DPS       RSLT
                   TYPE      MLEN,MSG
       .
                   EXIT      0                   
       .
       . RETURN THE INTEGER PART OF THE FLOATING POINT NUMBER IN AQ
       . AS A 60-BIT INTEGER IN AQ.
       .
       TRUNC       SZ,W      T$NEG               . CLEAR NEGATIVE FLAG
                   FU        T$EXP               . UNPACK FLOAT
                   JT        T$POS,,APOS
                   DPN                           . MAKE MANTISSA POSITIVE
                   LA        1                   . SET NEGATIVE FLAG
                   SA,W      T$NEG
       T$POS       DPS       T$AQ                . SAVE MANTISSA
                   LA,W      T$EXP
                   AN        1024D,,ANEG         . UNBIAS THE EXPONENT
                   J         T$POS2
                   ZA                            . EXPONENT < 0 THEN INT = 0
                   LQ,A
                   J         0,B1                . AND WE'RE DONE
       T$POS2      LQ        48D                 . # BITS TO SHIFT = 48 - EXP
                   ANQ,A
                   SQ,W      T$EXP               . SHIFT TO ELIMINATE FRACTION
                   DPL       T$AQ
                   RSAQ,L    T$EXP
                   DPS       T$AQ                
                   LA,W      T$NEG,,ANOT         . IS NEGATIVE FLAG SET?
                   J         T$RTRN              . NO
                   DPL       T$AQ                . NEGATIVE FLAG IS SET
                   DPN                           . SO MAKE NEGATIVE
                   DPS       T$AQ
       .            
       T$RTRN      DPL       T$AQ
                   J         0,B1
       .
       . RETURN THE FRACTION PART OF THE FLOATING POINT NUMBER IN AQ
       . AS A FLOATING POINT NUMBER IN AQ
       .
       FRAC        SB,W      B2,F$B2
                   SZ,W      T$NEG               . CLEAR NEGATIVE FLAG
                   FU        T$EXP               . UNPACK FLOAT
                   JT        F$POS,,APOS
                   DPN                           . MAKE MANTISSA POSITIVE
                   LA        1                   . SET NEGATIVE FLAG
                   SA,W      T$NEG
       F$POS       DPS       T$AQ                . SAVE MANTISSA
                   LA,W      T$EXP
                   AN        1024D,,ANEG         . UNBIAS THE EXPONENT
                   J         F$POS2
                   DPL       T$AQ                . EXPONENT < 0 THEN FRAC=PARAM
                   LB,W      B2,F$B2
                   J         0,B1                . AND WE'RE DONE
       F$POS2      AN        1                   . DEC BY 1 FOR LOOP
                   SA,W      T$EXP               . SHIFT TO ELIMINATE FRACTION
                   DPL       T$AQ
                   LB,W      B2,T$EXP            . THIS NEEDS TO BE DONE IN A
       F$SHIFT     LSAQ      1                   . LOOP BECAUSE LEFT SHIFTS ARE
                   LRSQ      1                   . CIRCULAR
                   LSQ       1
                   JBD       B2,F$SHIFT
                   DPS       T$AQ
                   LQ,W      T$LO18              . LOSE SHIFTED OUT BITS
                   LLP,A     
                   SA,W      T$AQ
                   DPL       T$AQ                
                   DPTE      F$ZERO              . MANTISSA = 0 THEN RESULT = 0
                   J         F$NOTZERO           . NO
                   J         F$ISZERO            . YES     
       F$NOTZERO   FP        T$EXP0              . ADD EXP OF ZERO AND NORMALIZE
                   DPS       T$AQ                
                   LA,W      T$NEG,,ANOT         . IS NEGATIVE FLAG SET?
                   J         F$RTRN              . NO
                   DPL       T$AQ                . NEGATIVE FLAG IS SET
                   DPN                           . SO MAKE NEGATIVE
                   DPS       T$AQ
       .            
       F$RTRN      LB,W      B2,F$B2
                   DPL       T$AQ
                   J         0,B1
       .
       F$ISZERO    SZ,W      T$AQ
                   SZ,W      T$AQ+1
                   J         F$RTRN 
       .
       . CONVERT AN INTEGER IN A TO A FLOATING POINT NUMBER IN AQ
       .
       .
       .           LBPJB1    INT2FLOAT
       .
       INT2FLOAT   SZ,W      IF$NEG              . CLEAR NEGATIVE FLAG
                   JT        IF$POS,,APOS
                   LA        1                   . A < 0, SET NEG FLAG
                   SA,W      IF$NEG              
                   NA                            . & MAKE A POSITIVE
       IF$POS      SFS                           . GET # BITS LEFT OF MSB
                   NQ
                   AQ        1024D+28D+20D       . CALC EXPONENT
                   SQ,W      IF$TEMP
                   LQ,A                          . GET NUMBER IN AQ
                   ZA
                   FP        IF$TEMP             . COMBINE A&EXP INTO FLOATING POINT
                   DPS       IF$TEMP             . NEGATE AQ IF REQD
                   LA,W      IF$NEG,,ANOT
                   J         IF$RTRN
                   DPL       IF$TEMP
                   DPN
                   DPS       IF$TEMP
       IF$RTRN     DPL       IF$TEMP
                   J         0,B1
       .                   
       IF$TEMP     RES       2
       IF$NEG      RES       1                   
       .
       . RETURN THE STRING EQUIVALENT OF THE FLOATING POINT NUMBER IN AQ
       . IN THE 3 WORDS POINTED TO BY THE SECOND PARAMETER. THE NUMBER 
       . OF DIGITS AFTER THE DECIMAL IS GIVEN BY THE FIRST PARAMETER.
       .
       .           LBPJB1    FLT2STR
       .           +PRECISION
       .           +RESULT
       . 
       FLT2STR     SB,W      B1,FS$B1
                   DPS       F$AQ
                   LA,W      0,B1                . GET PRECISION
                   TA        7,,YMORE            . PRECISION > 6?
                   LA        6                   . THEN PRECISION = 6
                   SA,W      FS$PREC
                   LSA       1                   . *2 TO GET TABLE OFFSET
                   LB,A      B4
                   LB,W      B5,1,B1             . GET PTR TO RESULT
       .     
                   SZ,W      FS$NEG              . CLEAR NEG FLAG
                   DPL       F$AQ      
                   JT        FS$POS,,APOS        . TAKE ABS AND SET NEG FLAG
                   DPN                           . COMPLEMENT VALUE
                   DPS       F$AQ
                   LA        1                   . SET NEG FLAG
                   SA,W      FS$NEG
       .                   
       FS$POS      DPL       F$AQ                . SHIFT VALUE PRECISION LEFT
                   FM        FS$POW10,B4
                   DPS       F$AQ
                   LBPJB1    TRUNC               . GET THE INTEGER PART
                   DPS       FS$INT
                   DPL       F$AQ
                   LBPJB1    FRAC                . GET FRACTIONAL PART
                   FM        FS$10               . GET 1ST DIGIT OF FRACTION
                   LBPJB1    TRUNC
                   TQ        5D,,YMORE           . >= 5 THEN ROUND
                   J         FS$ROUND
       .                   
       FS$CONT     LA,W      FS$SPACES           . CLEAR RESULT
                   SA,W      0,B5
                   SA,W      1,B5
                   SA,W      2,B5
                   DPL       FS$INT              . SET UP FOR 1ST DIVIDE
                   DPS       FS$P1
       FS$NXTDGT   SLJ       FS$SHIFT
       . GET NEXT DIGIT                   
                   LBPJB1    DIV60
       FS$P1       RES       2
                   +10D
       FS$P2       RES       2
       FS$P3       RES       1
                   LA,W      FS$P3               . GET REMAINDER
                   A         '0'                 . TO FIELDATA
                   LSA       24D                 . ADD TO RESULT
                   ROR,W     0,B5
                   RD,W      FS$PREC,,ANOT       . DEC PRECISION
                   J         FS$DOT              . =0 THEN ADD DECIMAL POINT                              
       FS$CONT2    DPL       FS$P2               . GET PRIOR QUOTIENT
                   DPS       FS$P1               . SET UP FOR NEXT DIVIDE
                   DPTE      F$ZERO              . = ZERO THEN DONE
                   J         FS$NXTDGT                   
       .
                   LA,W      FS$NEG,,ANOT        . NEG FLAG SET
                   J         FS$RTRN             . NO
                   SLJ       FS$SHIFT            . ADD '-' TO STRING
                   LA        '-'
                   LSA       24D
                   ROR,W     0,B5
       FS$RTRN     LB,W      B1,FS$B1
                   J         2,B1
       . SHIFT RESULT 1 DIGIT RIGHT
       FS$SHIFT    +0
                   DPL       1,B5
                   LRSAQ     6D
                   SQ,W      2,B5
                   LSA       6D
                   LQ,A
                   LA,W      0,B5
                   LRSAQ     6D
                   DPS       0,B5
                   J,L       FS$SHIFT
       .                   
       FS$DOT      SLJ       FS$SHIFT
                   LA        '.'
                   LSA       24D
                   ROR,W     0,B5
                   J         FS$CONT2
       .                   
       FS$ROUND    DPL       FS$INT              . ROUND INTEGER PART UP
                   DPA       FS$ONE              . UP BY 1
                   DPS       FS$INT
                   J         FS$CONT 
       .
       . 60-BIT DIVISION
       .
       .           LPBJB1    DIV60
       .           +DIVIDEND   (60-BIT)          INPUT
       .           +DIVISOR    (30-BIT)          INPUT
       .           +QUOTIENT   (60-BIT)          OUTPUT
       .           +REMAINDER  (30-BIT)          OUTPUT
       .
       DIV60       DPL       0,B1                . SAVE PARAMS
                   DPS       D6$DIVIDEND
                   LA,W      2,B1
                   SA,W      D6$DIVISOR
       .                   
                   ZA                            . INITIAL REMAINDER = 0
                   LQ,W      D6$DIVIDEND         . DIVIDE 1ST WORD
                   D,W       D6$DIVISOR
                   SQ,W      D6$Q1
                   LQ,W      D6$DIVIDEND+1       . DIVIDE 2ND WORD INCL REM.
                   D,W       D6$DIVISOR          
                   SQ,W      D6$Q2
                   SA,W      5,B1                . RETURN REMAINDER
                   DPL       D6$Q1               . RETURN QUOTIENT
                   DPS       3,B1                                     
       .
                   J         6,B1                   
                   
                   
       D6$DIVIDEND RES       2
       D6$DIVISOR  RES       1
       D6$Q1       RES       1                   . QUOTIENT (2 WORDS)
       D6$Q2       RES       1                                                              
       .
       . POWERS OF 10 TABLE. MAX DIGITS AFTER DECIMAL IS 6. TO INCREASE
       . THIS VALUE EXPAND THIS TABLE.
       .
       FS$SPACES   DLD       ' '
       FS$POINT5   DLD       0.5
       FS$ONE      DLD       1
       FS$POW10    DLD       1.0
       FS$10       DLD       10.0
                   DLD       100.0
                   DLD       1000.0
                   DLD       10000.0
                   DLD       100000.0
                   DLD       1000000.0
       .                              
       FS$B1       RES       1        
       FS$INT      RES       2
       FS$PREC     RES       1
       FS$NEG      RES       1
       F$AQ        RES       2
       F$B2        RES       1
       F$ZERO      DLD       0
       T$AQ        RES       2
       T$EXP       RES       1 
       T$NEG       RES       1
       T$ZERO      DLD       0.0
       T$LO18      +777777
       T$EXP0      +2000
       .       
       F123        DLD       123.0
       F456        DLD       456.0
       F579        DLD       0
       FRSLT       DLD       0
       FSTR        RES       3
       .
       PLUS        +'  +  '
       MINUS       +'  -  '
       DIVIDE      +'  /  '
       MULTIPLY    +'  *  '
       .
       MSG         EQU       $
       OP1         RES       2D
       OPR         RES       1D
       OP2         RES       2D
                   +'  =  '
       RSLT        RES       2D                   
                   +'    ^'
       MLEN        EQU       $-MSG
       .
                   END                                      