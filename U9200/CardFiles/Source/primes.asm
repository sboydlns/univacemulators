* PRIME NUMBER GENERATOR ETC.  15JAN71  9300 ASSEMBLER                  7022001
* KEY-INS:
*        X'01'--PRINT ONLY MERSENNE NUMBERS (WITH N, WHERE M=2**N-1)
*        X'10'--PRINT ONLY PRIME NUMBER PAIRS
G022     START 0
         USING *,0
         USING *,1
         USING *,2
         USING *,3
BEGN     MVC   LINE+1(131),LINE
         OPEN  PRNT
         MSG   X'333'
         UNPK  PAIR(3),*-1(2)
X03      ZAP   I,P3
X04      ZAP   MD,K
         DP    MD,I                    DIVIDE TEST NO. BY I
         CP    I,M                     SQRT(K) REACHED?
         BC    2,X08                   YES--K IS PRIME
         ZAP   MR,MR                   CHECK REMAINDER
         BC    8,X10                   IF EVENLY DIVISIBLE, TRY ANOTHER
         AP    I,P2                    IF NOT DIVISIBLE, INCREASE I
         BC    15,X04                  TRY DIVIDING AGAIN
* K IS PRIME
X08      CLI   PAIR,X'F1'              ONLY PAIRS TO BE PRINTED?
         BC    7,X09                   NO
         ZAP   NPRI,K
         SP    NPRI,OPRI               SUBTRACT LAST PRIME
         CP    NPRI,P2                 DIFFERENCE 2?
         BC    7,X085                  NO--DON'T PRINT
         MVC   LINE(32),ED31
         ED    LINE(32),OPRI           LAST PRIME TO LINE
         MVC   LINE+40(32),ED31
         ED    LINE+40(32),K           CURRENT PRIME TO LINE
         PUT   PRNT,LINE
X085     ZAP   OPRI,K                  K IS OLD PRIME NEXT TIME
         BC    15,X10                  LOOK FOR NEXT PRIME
X09      CLI   MERS,X'F1'              LOOK FOR MERSENNE NUMBERS?
         BC    7,X095                  NO--JUST PRINT EVERY PRIME
X091     ZAP   MRS,PWR2
         SP    MRS,P1                  DECREMENT BY 1
         CP    MRS,K                   COMPARE WITH LATEST PRIME
         BC    8,X095                  MATCH--PRINT MERSENNE NUMBER
         BC    2,X10                   I**N-1 EXCEEDS K--NOT MERSENNE
         MP    PWR2+1(15),P2           TRY NEXT POWER OF 2
         AP    PWR,P1                  INCREMENT POWER COUNT
         BC    15,X091                 TRY NEXT POWER OF 2
X095     MVC   LINE(32),ED31
         ED    LINE(32),K              PRIME TO LINE
         CLI   MERS,X'F1'              IS IT A MERSENNE NO.?
         BC    7,X099                  NO
         MVC   LINE+40(6),ED5
         ED    LINE+40(6),PWR          POWER OF 2 TO LINE TOO
X099     PUT   PRNT,LINE
X10      AP    K,P2                    TRY NEXT K
         BC    15,X03
FOF      STH   14,S14
         CNTRL PRNT,SK,7
         LH    14,S14
         BC    15,0(,14)
S14      DS    H        
K        DC    XL16'3C'                NUMBER UNDER TEST
I        DC    XL8'C'                  DIVISOR
MD       DS    0CL16                   DIVIDEND
M        DC    XL8'C'                  QUOTIENT
MR       DS    CL8                     REMAINDER
NPRI     DC    XL16'C'                 LATEST PRIME FOR PAIR TEST
OPRI     DC    XL16'C'                 LAST PRIME FOR PAIR TEST
PWR2     DC    XL16'2C'                POWERS OF 2 FOR MERSENNE TEST
PWR      DC    XL3'1C'                 ACTUAL POWER OF 2
MRS      DC    XL16'C'
P3       DC    XL1'3C'
P2       DC    XL1'2C'
P1       DC    XL1'1C'
PAIR     DS    CL1                     ON IF PAIR TEST
MERS     DS    CL1                     ON IF MERSENNE TEST
         DS    CL1                     PROTECTION FROM UNPK
LINE     DC    CL1' '
         DS    CL131
ED31     DC    XL16'40202020202020202020202020202020'
         DC    XL16'20202020202020202020202020202120'
ED5      DC    XL6'402020202120'
         EXTRN PRNT
         ENTRY FOF
         ENTRY BEGN
         END   BEGN
