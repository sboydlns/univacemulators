* PRIME NUMBERS & THINGS  20MAY71  9300 ASSEMBLER                       7105001
*
* KEY-INS
*        0--PRINT ALL PRIME NUMBERS
*        1--PRINT MERSENNE NUMBERS
*        2--MERSENNE CHECK ONLY ON PRIME POWERS OF 2
*        4--SUPPRESS PRINTING & HALT WITH X'FF' WHEN PRIME TABLE FULL
*        X'10'--PRINT PRIME PAIRS
*
G105     START 0
         USING *,0
         USING *,1
         USING *,2
         USING *,3
PTAB     DS    256CL56                 PRIME TABLE
PTND     DS    0H
BEGN     RSTRT RSTR
         ZAP   PTAB(4),P2
         OPEN  PRNT
RSTR     MSG   X'333'
         MVC   LINE,BLNK
         MVC   YPTA,YPTB
         SP    CT,CT
         SP    PCT,PCT
         MVI   OF,1
         TM    RSTR+7,1                MERSENNE NUMBERS?
         BC    7,A200                  YES
         ZAP   K,P3
         MVC   HD1+51(18),CPRM         SET UP HEADING LINE
         TM    RSTR+7,X'10'            PRIME PAIRS?
         BC    8,A100                  NO
         MVC   HD1+51(18),CPMP         HEADING FOR PRIME PAIRS
A100     BAL   13,PTST                 IS K PRIME?
         CLI   PRIM,1
         BC    7,A190                  NO
         MVC   LINE+10(32),ED31
         TM    RSTR+7,X'10'            PRIME PAIRS?
         BC    8,A160                  NO--PRINT PRIME
         ZAP   NPRI,K
         SP    NPRI,OPRI
         CP    NPRI,P2                 DIFFERENCE OF 2 FROM LAST PRIME?
         BC    7,A190                  NO--IT'S NOT A PRIME PAIR
         ED    LINE+10(32),OPRI        PRINT PRIME PAIR
         MVC   LINE+50(32),ED31
         ED    LINE+50(32),K
         ZAP   OPRI,K                  STORE PRIME
         BC    15,A180                 PRINT PAIR
A160     ED    LINE+10(32),K           PRIME TO LINE
A180     BAL   13,PRIN                 PRINT LINE
A190     AP    K,P2                    NEXT EVEN NUMBER
         BC    15,A100                 TEST AGAIN
* FIND MERSENNE NUMBERS
A200     MVC   HD1+51(18),CMRS         SET UP HEADING LINE
         ZAP   PWR2,P2                 POWER OF 2 USED
         ZAP   MRS,P4                  2**2=4
A210     TM    RSTR+7,2                CHECK ONLY PRIME VALUES OF PWR2?
         BC    8,A220                  NO--DO THEM ALL
         ZAP   K,PWR2
         BAL   13,PTST
         CLI   PRIM,1                  IS PWR2 PRIME?
         BC    7,A290                  NO--DON'T EVEN BOTHER TRYING IT
A220     ZAP   K,MRS
         SP    K,P1                    2**PWR2-1
         BAL   13,PTST                 IS IT PRIME (AND MERSENNE NO.)?
         CLI   PRIM,1
         BC    7,A290                  NO
         MVC   LINE+10(32),ED31
         ED    LINE+10(32),K           MERSENNE NUMBER TO LINE
         MVC   LINE+50(6),ED5
         ED    LINE+50(6),PWR2         POWER OF 2
         BAL   13,PRIN                 PRINT LINE
A290     MP    MRS+1(15),P2            NEXT POWER OF 2
         AP    PWR2,P1
         BC    15,A210                 TEST IT
* PRINT SUBROUTINE
PRIN     CLI   OF,1                    PAGE OVERFLOW?
         BC    7,PRT2                  NO
         AP    PCT,P1                  INCREMENT PAGE COUNT
         MVC   HD1+116(4),ED3
         ED    HD1+116(4),PCT          PAGE NO. TO HEADING LINE
         CNTRL PRNT,SK,7
         CNTRL PRNT,SP,0,2
         PUT   PRNT,HD1                PRINT HEADING LINE
         MVI   OF,0                    RESET OVERFLOW INDICATOR
PRT2     AP    CT,P1                   INCREMENT LINE COUNT
         MVC   LINE(8),ED7             LINE COUNT TO LINE
         ED    LINE(8),CT              LINE NUMBER TO LINE
         TM    RSTR+7,4                SUPPRESS PRINTING?
         BC    7,0(,13)                YES
         PUT   PRNT,LINE               PRINT LINE
         MVC   LINE,BLNK               CLEAR LINE
         BC    15,0(,13)               EXIT
FOF      MVI   OF,1
         BC    15,0(,14)
* TEST WHETHER A NUMBER IS PRIME, ADDING ALL PRIMES TO TABLE IF ROOM
PTST     MVI   PRIM,0                  RESET PRIME INDICATOR
         LH    8,YPTB                  USE CURRENT PRIMES AS DIVISORS
PTS1     ZAP   I,0(4,8)                DIVISOR
PTS2     ZAP   MD,K                    NUMBER UNDER TEST
         DP    MD,I                    DIVIDE BY PRIME
         CP    0(4,8),M                SQRT(K) REACHED?
         BC    12,PTS4                 NO--CONTINUE
         MVI   PRIM,1                  IT'S PRIME
         CLC   YPTA,YPTN               TABLE FULL?
         BC    4,PTS3                  NO
         TM    RSTR+7,4                HALT WHEN FULL?
         BC    8,0(,13)                NO
         HPR   X'00FF'                 FLAG TABLE FULL
         BC    15,0(,13)               EXIT
PTS3     LH    8,YPTA                  END OF ACTIVE TABLE
         ZAP   0(4,8),K                LATEST PRIME TO TABLE
         AI    YPTA,4                  INCREMENT ACTIVE USAGE COUNTER
         BC    15,0(,13)               EXIT
PTS4     ZAP   MR,MR                   ANY REMAINDER?
         BC    8,0(,13)                NO--K IS NOT PRIME
         AH    8,H4                    NEXT PRIME
         CH    8,YPTA                  BEYOND RANGE OF TABLE?
         BC    4,PTS1                  NO--CHECK AGAIN
         AP    I,P2                    GO UP BY 2 NOW
         BC    15,PTS2                 TRY AGAIN
***********************************************************************
* STORAGE
***********************************************************************
* HALF-WORDS
H4       DC    Y(4)
YPTB     DC    Y(PTAB)
YPTA     DS    H                       ACTIVE LIMIT OF PTAB
YPTN     DC    Y(PTND)
* I/O AREAS
BLNK     DC    CL1' '
LINE     DS    CL132
* HEADING LINE
HD1      DC    CL16'PROGRAM CJG7105'
         DC    CL16' '
         DC    CL16' '
         DC    CL16' '
         DC    CL16' '
         DC    CL16' '
         DC    CL16' '
         DC    CL16'PAGE'
         DC    CL4' '
* VARIABLES USED IN PRIME TEST
K        DS    CL16                    NUMBER UNDER TEST
I        DS    CL8                     PRIME DIVISOR
MD       DS    0CL16                   DIVISION AREA
M        DS    CL8                     QUOTIENT
MR       DS    CL8                     REMAINDER
NPRI     DS    CL16                    CURRENT PRIME FOR PAIR TEST
OPRI     DC    XL16'C'                 PREVIOUS PRIME FOR PAIR TEST
MRS      DS    CL16                    POWER OF 2 FOR MERSENNE NUMBERS
PWR2     DS    CL3                     POWER OF 2 IN MERSENNE TEST
* CONSTANTS
P1       DC    XL1'1C'
P2       DC    XL1'2C'
P3       DC    XL1'3C'
P4       DC    XL1'4C'
* MESSAGES
CPRM     DC    CL16'   PRIME NUMBERS'
         DC    CL2' '
CPMP     DC    CL16'PRIME NUMBER PAI'
         DC    CL2'RS'
CMRS     DC    CL16' MERSENNE NUMBER'
         DC    CL2'S'
* EDIT PATTERNS
ED3      DC    XL4'40202120'
ED5      DC    XL6'402020202120'
ED7      DC    XL8'4020202020202120'
ED31     DS    0CL32
         DC    XL16'40202020202020202020202020202020'
         DC    XL16'20202020202020202020202020202120'
* OTHER STORAGE
CT       DS    CL4                     LINE COUNTER
PCT      DS    CL3                     PAGE COUNTER
OF       DS    CL1                     PAGE OVERFLOW INDICATOR
PRIM     DS    CL1                     ON IF K IS FOUND TO BE PRIME
* EXTRNS & ENTRYS
         EXTRN PRNT
         ENTRY FOF
         ENTRY BEGN
         END   BEGN
