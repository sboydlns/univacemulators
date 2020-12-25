* PROGRAM 7014--OBJECT CODE DISASSEMBLER  15NOV73  9300 ASSEMBLER       7014001
* INPUT TO THIS PROGRAM CONSISTS OF STANDARD UNIVAC 9300 OBJECT CODE
*  AS PRODUCED BY THE ASSEMBLER OR LINKER.  OUTPUT IS IN THE FORM OF
*  A LISTING SIMILAR TO THE PRINTED OUTPUT OF THE ASSEMBLER.
* AT END OF RUN (TYPE Y CARD PROCESSED) COMPUTER WILL STOP WITH
*  X'1FFF' DISPLAY.  TO DISASSEMBLE ANOTHER OBJECT DECK, LOAD NEW
*  OBJECT DECK, MAKE APPROPRIATE KEY-INS, AND HIT START.
* IF AN OBJECT DECK IS FOLLOWED BY ANOTHER WITH NO INTERVENING
*  SENTINELS, THE PROGRAM WILL IMMEDIATELY START DISASSEMBLING THE
*  SECOND DECK UNDER THE FIRST DECK'S OPTIONS.
*
* KEY-INS:
*        0000 00XX
*               ]'--PUNCH OUTPUT
*               '---SUPPRESS PRINTING
*
* HALTS:
*        1F03--INVALID CARD TYPE--HIT START TO IGNORE.
*        1F05--HOLE COUNT ERROR.  THE ERROR CARD IS THE SECOND TO LAST
*              CARD IN THE STACKER.  EITHER RE-FEED IT AND ALL CARDS
*              FOLLOWING AND HIT START TO RE-TRY, OR KEY A 1 INTO
*              LOCATION 4 AND HIT START TO IGNORE THE ERROR.
*        4300--CARD COUNT ERROR--HIT START TO IGNORE
*
* DURING STATEMENT PROCESSING, R10 POINTS TO POSITION OF CURRENT
*  STATEMENT ON CARD.  THE OBJECT CODE FOR THIS STATEMENT IS STORED
*  IN "INST" DURING PROCESSING (EXECUTABLE STATEMENTS ONLY).
*  R11 POINTS TO CURRENT POSITION IN OUTPUT LINE DURING PROCESSING.
*
* THE RESTART FEATURE IS PROVIDED IN THIS PROGRAM
*
G014     START 0
         USING *,0
         USING *,1
         USING *,2
         USING *,3
BEGN     RSTRT MSSG
MSSG     MSG   X'333'
         OPEN  READ
         OPEN  PRNT
         TM    MSSG+7,1
         BC    8,NPCH
         OPEN  PUNC
NPCH     MVC   LINE,BLNK               INITIAL CLEARING OF LINE
* CLEAR ESID TABLE
ST3      MVI   TESD,0
         MVC   TESD+1(4),BLNK
         MVC   TESD+5(95),TESD
* CLEAR ENTRY SYMBOL TABLE
         MVI   TENT,X'FF'              NULL INDICATOR
         MVC   TENT+2(4),BLNK
         MVC   TENT+6(114),TENT
* READ NEW OBJECT CARD
REED     BAL   13,GET                  READ A CARD
TTST     CLI   CARD+1,C'A'             TYPE A?
         BC    8,A000
         CLI   CARD+1,C'H'             TYPE H?
         BC    8,H000
         BC    8,REED                  IF SO, IGNORE
         CLI   CARD+1,C'J'             TYPE J?
         CLI   CARD+1,C'K'             TYPE K?
         BC    8,K000
         CLI   CARD+1,C'Q'             TYPE Q?
         BC    8,Q000
         CLI   CARD+1,C'Y'             TYPE Y?
         BC    8,Y000
* ERROR--NO VALID CODE IN COL. 2--IGNORE CARD
         MSG   X'1F03'
         BC    15,REED                 GET NEXT CARD
***************************************
* TYPE A CARD--GENERATE START STATEMENT
A000     CLI   CARD+5,X'01'            ABSOLUTE PROGRAM?
         BC    7,A001                  NO
         BAL   13,NST                  STATEMENT NUMBER
         MVC   LINE+57(21),CCTL        CONTROL CARD
         BAL   13,PS1                  PRINT CTL STATEMENT
A001     BAL   13,NST
         MVI   EREF,0
         BAL   13,TESA                 LOOK FOR EMPTY ENTRY
         CLI   0(8),X'FF'              TABLE FULL?
         BC    8,A002                  YES--IGNORE ESID
         MVC   0(1,8),CARD+7           STORE ESID IN TABLE
         MVC   1(4,8),CARD+16          PID
A002     MVC   LINE+48(4),CARD+16      PID
         MVC   LINE+57(8),CST          'START'
         MVC   ACTR,CARD+14            INITIALIZE LOCATION COUNTER
         BAL   13,HXUN                 UNPACK START ADDRESS
         DC    Y(CARD+14)
         DC    Y(LINE+65)
         MVI   LINE+69,C''''
         MVC   LINE+78(25),COM1        ESID AND ELEMENT LENGTH COMMENTS
         BAL   13,HXUN                 UNPACK ESID
YC07     DC    Y(CARD+7)
         DC    Y(LINE+87)
         MVC   LINE+89(3),MSC1
         BAL   13,HXUN                 UNPACK ELEMENT LENGTH
         DC    Y(CARD+34)
         DC    Y(LINE+103)
         MVI   LINE+107,C''''
         MVC   LINE+120(8),CARD+72     PID AND SEQUENCE NUMBER
         BAL   13,PS1
         BC    15,REED                 GET NEXT CARD
***************************************
* TYPE H--GENERATE ENTRY STATEMENT
H000     BAL   13,NST
* PLACE SYMBOL IN ENTRY TABLE
H010     LH    8,YENT
H011     CLI   0(8),X'FF'              NULL ENTRY?
         BC    8,H012                  YES
         AH    8,H6                    TRY NEXT ENTRY
         BC    15,H011
H012     CH    8,YEND                  END OF TABLE REACHED?
         BC    10,H020                 YES
         MVC   0(2,8),CARD+14          STORE ADDRESS
         MVC   2(4,8),CARD+16          STORE SYMBOL NAME
H020     BAL   13,HXUN                 SYMBOL ADDRESS
         DC    Y(CARD+14)
         DC    Y(LINE+10)
         MVC   LINE+57(5),CENT         MNEMONIC
         MVC   LINE+63(4),CARD+16      SYMBOL NAME
         MVC   LINE+120(8),CARD+72
         BAL   13,PS1
         BC    15,REED
***************************************
* TYPE K (EXTRN) PROCESSING
K000     BAL   13,NST
         AI    KQCT,1                  CARD COUNT ON TYPES K AND Q
         MVI   EREF,0
         BAL   13,TESA                 LOOK FOR BLANK ENTRY
         CLI   0(8),X'FF'              TABLE FULL?
         BC    8,K001                  YES--IGNORE ESID
         MVC   0(1,8),CARD+7           STORE ESID
         MVC   1(4,8),CARD+16          STORE SYMBOL NAME
K001     MVC   LINE+57(5),CEXT         MNEMONIC
         MVC   LINE+63(4),CARD+16      SYMBOL NAME
         MVC   LINE+78(9),COM1
         BAL   13,HXUN                 ESID
         DC    Y(CARD+7)
         DC    Y(LINE+87)
         MVI   LINE+89,C''''
         MVC   LINE+90(2),BLNK
         MVC   LINE+120(8),CARD+72
         BAL   13,PS1
         BC    15,REED
***************************************
* TYPE Q (TEXT) CARD PROCESSING
Q000     BAL   13,QNCD
* SEARCH FOR STATEMENT LABEL DEFINED BY ENTRY
Q010     LH    8,YENT
Q011     CLI   0(8),X'FF'              END OF ENTRY TABLE?
         BC    8,Q020                  YES--SYMBOL NOT FOUND
         CLC   0(2,8),ACTR             MATCH WITH CURRENT ADDRESS?
         BC    8,Q012
         AH    8,H6                    TRY NEXT ENTRY
         BC    15,Q011
Q012     MVC   LINE+48(4),2(8)         STATEMENT LABEL TO LINE
Q020     BAL   13,SRCH                 IS OP CODE VALID?
         CLI   TYPE,X'FF'
         BC    7,Q200                  YES--PROCESS INSTRUCTION
* DC ROUTINE
Q100     BAL   13,NST
         BAL   13,HXUN                 DATA ADDRESS
         DC    Y(ACTR)
         DC    Y(LINE+10)
Q101     MVC   LINE+57(8),CDC          MNEMONIC
         LH    11,YL65
Q105     STH   10,Q106+4
         STH   11,Q106+6
Q106     BAL   13,HXUN                 UNPACK DATA
         DC    Y(0)
         DC    Y(0)
         AI    ACTR,2                  INCREMENT LOCATION COUNTER
         AH    11,H4                   NEXT POSITION ON LINE
         AH    10,H2                   NEXT POSITION ON CARD
         CH    10,YLIM                 END OF CARD?
         BC    4,Q112                  NO
         BC    8,Q120                  YES
         SH    11,H2                   OVERRUN--THERE'S ONLY ONE BYTE
         AI    ACTR,-1
         MVC   0(2,11),BLNK            CLEAR OVERRUN BYTE
         BC    15,Q120                 TERMINATE DC
Q112     CH    11,YL97                 16 BYTES UNPACKED?
         BC    10,Q120                 YES--TERMINATE DC
         BAL   13,SRCH                 TRY NEXT TWO BYTES
         CLI   TYPE,X'FF'              VALID OP CODE?
         BC    8,Q105                  NO--TWO MORE BYTES FOR DC
Q120     LH    11,YL15
Q122     MVC   0(2,11),50(11)          DC CODING TO OUTPUT LINE
         AH    11,H2
         CLI   50(11),C' '             END OF DC CODING?
         BC    7,Q122                  NO--MOVE ANOTHER BYTE
         MVI   50(11),C''''            CLOSE OFF DC OPERAND
         MVI   51(11),C' '
         BAL   13,PS1
         CH    10,YLIM                 END OF CARD REACHED?
         BC    10,REED                 YES--GET NEXT CARD
* EXECUTABLE STATEMENTS
Q200     MVC   INST,0(10)              LOAD INSTRUCTION INTO REGISTER
         BAL   13,NST
         BAL   13,HXUN                 INSTRUCTION ADDRESS
         DC    Y(ACTR)
         DC    Y(LINE+10)
         LH    11,YLIM
         STH   10,YTP2
         SH    11,YTP2                 NUMBER OF TXT BYTES LEFT ON CARD
         CH    11,H6
         BC    10,Q205                 IF 6 OR MORE, WE'RE ALL RIGHT
         CH    11,H4
         BC    4,Q201                  IF LESS THAN 4, WE NEED MORE
         CLI   TYPE,4                  IS THIS AN SS INSTRUCTION?
         BC    4,Q205                  NO--THERE'S ENOUGH ON THIS CARD
Q201     BAL   13,GET                  READ NEXT CARD
         CLI   CARD+1,C'Q'             TYPE Q?
         BC    8,Q203                  YES
Q202     MVI   ADV,1                   INDICATE NEXT CARD ALREADY READ
         BC    15,Q101                 TREAT INCOMPLETE DATA AS A DC
Q203     STH   11,YTP2
         LH    15,ACTR
         AH    15,YTP2                 NEXT CARD DATA SHOULD START HERE
         CH    15,CARD+4               DOES IT?
         BC    7,Q202                  NO--TERMINATE AS A DC
         AH    11,YINS                 REST OF INSTRUCTION LOADS HERE
         MVC   0(4,11),CARD+10         COMPLETE INSTRUCTION
         LH    10,YC10                 SET UP ON NEW CARD
         SH    10,YTP2                 PSEUDO-ADDRESS FOR TEXT START
         BAL   13,LESD                 PRINT CARD DATA
Q205     MVC   LINE+57(4),MNEM
         UNPK  LINE+15(9),INST(5)      FIRST 4 INSTRUCTION BYTES
         MVI   LINE+23,C' '
         TR    LINE+15(8),HXTR-240
         AI    ACTR,4
         CLI   TYPE,4                  SS INSTRUCTION?
         BC    4,Q210                  NO
Q208     BAL   13,HXUN                 UNPACK LAST 2 BYTES (SS ONLY)
         DC    Y(INST+4)
         DC    Y(LINE+23)
         AI    ACTR,2
Q210     LH    11,YL63                 R11 -> POSITION IN OPERANDS
         CLI   TYPE,1                  RX INSTRUCTION?
         BC    7,Q220
* RX INSTRUCTION--UNPACK REGISTER FIRST
         MVI   BD,0
         PACK  BD+1(1),INST+1(1)       GET REGISTER
         NI    BD,X'0F'
         BAL   13,DECL                 REGISTER TO LINE
         MVI   0(11),C','              SEPARATOR TO LINE
         AH    11,H1
* GET FIRST CORE ADDRESS
Q220     LH    8,YIN2
         BAL   13,ESDL                 ESID?
         CLI   ESD,1
         BC    8,Q260                  YES
         LH    8,YIN2
         BAL   13,ENTL                 DEFINED BY ENTRY?
         CLI   ENT,1
         BC    8,Q260                  YES
* ADDRESS IN BASE-DISPLACEMENT FORM--NO ESID
Q230     MVC   BD,INST+2               BASE AND DISPLACEMENT
         MVI   FLEN,0                  ASSUME NO LENGTH
         CLI   TYPE,2                  SI?
         BC    8,Q250                  YES
         MVI   FLEN,1                  GET LENGTH OR INDEX REGISTER
         MVI   OLEN,0
         MVC   OLEN+1(1),INST+1        ASSUME SS1 LENGTH FIELD
         CLI   TYPE,4                  SS1?
         BC    8,Q249                  YES
         NI    OLEN+1,X'0F'            KILL FIRST HALF-BYTE
         CLI   TYPE,1                  RX?
         BC    7,Q245                  NO
         MVI   FLEN,2                  INDICATE INDEX REGISTER
         BC    15,Q250
Q245     PACK  OLEN+1(1),INST+1(1)     GET SS2 L1
         NI    OLEN+1,X'0F'
Q249     AI    OLEN,1                  INCREMENT LENGTH CODE
Q250     BAL   13,BDP                  PRINT BASE AND DISPLACEMENT
Q260     CLI   TYPE,1                  RX?
         BC    8,Q900                  YES--WE'RE FINISHED
         MVI   0(11),C','              SEPARATOR FROM NEXT OPERAND
         AH    11,H1
         CLI   TYPE,2                  SI INSTRUCTION?
         BC    8,UB85                  YES--GET IMMEDIATE OPERAND
         MVC   BD,INST+4               GET SS INSTRUCTION OP 2
         MVI   FLEN,0                  ASSUME SS1--NO LENGTH
         CLI   TYPE,5                  IS IT SS2?
         BC    7,Q270                  NO
         MVC   OLEN,H0
         MVN   OLEN+1(1),INST+1        GET SS2 L2
         AI    OLEN,1                  INCREMENT BY 1 TO TRUE LENGTH
         MVI   FLEN,1                  INDICATE LENGTH PROCESSING
Q270     BAL   13,BDP                  PROCESS BASE/DISPLACEMENT
Q900     AH    10,H4                   MOVE DOWN TEXT CARD
         CLI   TYPE,4                  SS?
         BC    4,Q910                  NO
         AH    10,H2                   TWO MORE BYTES FOR SS
Q910     BAL   13,PS1
         CH    10,YLIM                 END OF CARD REACHED?
         BC    10,REED                 IF SO, GET ANOTHER ONE
         BC    15,Q010                 IF NOT, PROCESS NEXT STATEMENT
***************************************
* TYPE Y (TRANSFER CARD) PROCESSING--GENERATE END STATEMENT AND STOP
Y000     BAL   13,NST
         MVC   YTP2(2),CARD+11
         LH    8,YTP2
         CH    8,KQCT                  CHECK CARD COUNT
         BC    8,Y005                  IT AGREES WITH COUNT ON Y CARD
         HPR   X'4300'                 CARD COUNT ERROR
Y005     MVC   LINE+57(8),CEND         MNEMONIC 'END'
         BAL   13,HXUN                 START ADDRESS FOR END STATEMENT
         DC    Y(CARD+14)
         DC    Y(LINE+10)
         MVC   EREF,CARD+69            LOOK FOR END ESID
         BAL   13,TESA
         CLI   0(8),X'FF'              NOT FOUND?
         BC    8,Y010                  NO
         MVC   LINE+63(4),1(8)         END ESID TO LINE AS END OP.
         BC    15,Y020
Y010     MVC   LINE+65(4),LINE+10      ADDRESS IN HEX INSTEAD
         MVI   LINE+69,C''''
Y020     MVC   LINE+120(8),CARD+72
         BAL   13,PS1
         CNTRL PRNT,SP,2,0
         MVC   LINE(22),HDR3
         ED    LINE+16(6),CT           PRINT NUMBER OF CARDS PROCESSED
         PUT   PRNT,LINE
         MVC   LINE,BLNK
         MVI   OF,1                    RESET INDICATORS, COUNTERS, ETC.
         MVC   KQCT,H0
         SP    CT,CT
         SP    PCT,PCT
         SP    STMT,STMT
         BC    15,ST3                  TRY FOR ANOTHER OBJECT DECK
REOF     CLOSE READ
         CLOSE PRNT
         TM    MSSG+7,1
         BC    8,MSSG
         CLOSE PUNC
         BC    15,MSSG
****************************************
* SUBROUTINES                          *
****************************************
* PAGE OVERFLOW TEST
CLER     CLI   OF,1
         BC    7,0(,13)
* NEW PAGE ROUTINE
         AP    PCT,P1
         MVC   HDR1+117(3),ED1+2
         ED    HDR1+116(4),PCT         PAGE NUMBER
         CNTRL PRNT,SK,7,0
         CNTRL PRNT,SP,0,2
         PUT   PRNT,HDR1
         CNTRL PRNT,SP,0,2
         PUT   PRNT,HDR2
         MVI   OF,0                    RESET OVERFLOW INDICATOR
         BC    15,0(,13)
* PAGE OVERFLOW DETECTION
FOF      MVI   OF,1
         BC    15,0(,14)
* ROUTINES FOR NEWLY-READ TYPE Q CARD
QNCD     STH   13,QNC9+2
         AI    KQCT,1
         LH    10,YC10                 R10 -> CURRENT INSTRUCTION
         LH    11,YC10
         MVC   YTMP+1(1),CARD+2
         AH    11,YTMP
         AH    11,H1
         STH   11,YLIM                 YLIM = Y(LAST TEXT BYTE ON CARD)
         CLC   ACTR,CARD+4             CONTINUATION FROM LAST CARD?
         BC    8,QNC1                  YES
         AP    STMT,P1                 GENERATE ORG STATEMENT
         UNPK  OGLE-1(5),STMT
         OI    OGLE+3,X'F0'
         BAL   13,HXUN
         DC    Y(CARD+4)
         DC    Y(OGLE+10)
         MVC   OGLE+65(4),OGLE+10
         MVC   OGLE+120(8),CARD+72
         PUT   PRNT,OGLE
         TM    MSSG+7,1
         BC    8,QNC1
         PUT   PUNC,OGLE+48
QNC1     MVC   ACTR,CARD+4             LOAD LOCATION COUNTER
         BAL   13,LESD                 PRINT LOAD RELATIVE ESID
         TM    ACTR+1,1                IS LOC. CTR. HALF-WORD ALIGNED?
         BC    8,QNC9                  YES--EXIT
         BAL   13,NST
         BAL   13,HXUN                 LOCATION COUNTER TO LINE
         DC    Y(ACTR)
         DC    Y(LINE+10)
         MVC   LINE+57(8),CDC          MAKE UP A DC STATEMENT
         UNPK  LINE+65(3),0(2,10)      TAKE THE FIRST BYTE
         TR    LINE+65(2),HXTR-240
         MVC   LINE+15(2),LINE+65      DUPLICATE HEX IN OBJ. CODE AREA
         MVI   LINE+67,C''''
         PUT   PRNT,LINE
         TM    MSSG+7,1
         BC    8,QNC8
         PUT   PUNC,LINE+48
QNC8     MVC   LINE,BLNK
         AH    10,H1                   SKIP OVER SINGLE BYTE
         AI    ACTR,1                  LOC. CTR. IS NOW ALIGNED
QNC9     BC    15,*                    RETURN ADDRESS STORED HERE
* PRINT LOAD RELATIVE ESID
LESD     MVC   LINE+120(8),CARD+72
         MVC   LINE+78(23),COM2
         UNPK  LINE+101(3),CARD+7(2)   LOAD RELATIVE ESID TO LINE
         TR    LINE+101(2),HXTR-240
         MVI   LINE+103,C''''
         BC    15,0(,13)
* LAY OUT SI IMMEDIATE OPERAND
UB85     CLI   INST+1,16               IS IMMEDIATE OPERAND 16 OR OVER?
         BC    10,UHEX                 YES--DO IT IN HEX
         MVI   BD,0
         MVC   BD+1(1),INST+1
         BAL   13,DECL                 DECIMAL OPERAND TO LINE
         BC    15,Q900                 END OF INSTRUCTION PROCESSING
UHEX     MVC   0(2,11),CX              SET UP HEX OPERAND
         UNPK  2(3,11),INST+1(2)
         TR    2(2,11),HXTR-240
         MVI   4(11),C''''
         BC    15,Q900
* READ A CARD AND CHECK FOR VALIDITY
GET      CLI   ADV,1                   IS NEXT CARD ALREADY READ?
         MVI   ADV,0
         BC    8,0(,13)                YES--DON'T READ ANOTHER ONE YET
         GET   READ,CARD               READ A CARD
         CLI   CARD,X'02'              LOAD KEY IN COL. 1?
         BC    7,GET                   NO--IGNORE CARD
         AP    CT,P1                   INCREMENT CARD COUNT
         TR    CARD+2(70),TBPU         TRANSLATE COMPRESSED CODE
* CHECK HOLE COUNT
HCT      LH    8,YC07                  START CHECKING AT COL. 8
         LH    9,H0                    SUM OF BYTES FORMS HERE
HCT1     MVC   YTMP+1(1),0(8)
         AH    9,YTMP                  TAKE SUM OF BYTES ON CARD
         CH    9,H256                  OVERFLOW?
         BC    4,HCT2                  NO
         SH    9,H256                  GET IT BACK DOWN BELOW 256
HCT2     AH    8,H1                    GO TO NEXT BYTE
         CH    8,YC72                  END OF AREA TO BE CHECKED?
         BC    4,HCT1                  NO--ADD NEXT BYTE
         MVC   YTMP+1(1),CARD+6        HOLE COUNT FROM CARD
         CH    9,YTMP                  COMPARE WITH SUM IN R9
         BC    8,0(,13)                IF ALL RIGHT, EXIT SUBROUTINE
HCT3     MSG   X'1F05'                 HOLE COUNT ERROR
         CLI   HCT3+7,1                IGNORE THE ERROR?
         BC    8,0(,13)                YES--EXIT
         GET   READ,CARD               IGNORE NEXT CARD IMAGE IN BUFFER
         BC    15,GET                  TRY AGAIN
* NEW STATEMENT NUMBER TO LINE
NST      AP    STMT,P1                 INCREMENT STMT. NO.
         UNPK  LINE(4),STMT            STATEMENT NUMBER TO LINE
         OI    LINE+3,X'F0'
         BC    15,0(,13)
* SEARCH FOR NAME CORRESPOSDING TO ESID IN "EREF"
TESA     LH    8,YESD
TES1     CLC   0(1,8),EREF             MATCH?
         BC    8,0(,13)                YES--EXIT SUBR
         AH    8,H5                    TRY NEXT ENTRY
         CLI   0(8),X'FF'              END OF ACTIVE TABLE?
         BC    7,TES1                  NO--TRY NEXT ENTRY
         BC    15,0(,13)               EXIT SUBR--ESID NOT IN TABLE
* SEARCH FOR ESID, MOVING SYMBOL TO LINE IF FOUND
ESDL     STH   13,ESDX+2
         MVI   ESD,0                   RESET FIND INDICATOR
         CLC   0(2,8),H0               IS ADDR ZERO?
         BC    7,0(,13)                NO--USE IT INSTEAD
         CLI   CARD+8,0                ANY RLD INFO ON CARD?
         BC    8,0(,13)                NO--EXIT
         MVC   ENO,H3                  RLD COLUMN COUNTER
         STH   9,YTP3                  SAVE R9
         STH   10,YTP2                 FOR LR 9,10
         LH    9,YTP2
         LH    12,YC69                 FIRST RLD INFO
         STH   12,YTP2                 SAVE R12
         AH    9,H2
         SH    9,YC10                  INST. COL. 11 RELATIVE
ESD1     MVC   YTMP+1(1),2(12)
         NI    YTMP+1,X'7F'
         CH    9,YTMP                  POINTED TO BY RLD PTR?
         BC    7,ESD2                  NO--TRY NEXT ONE, IF ANY
         MVI   ESD,1                   ESID FOUND
         MVC   EREF,0(12)
         BAL   13,TESA                 LOOK UP SYMBOL
         MVC   0(4,11),1(8)            SYMBOL TO OUTPUT LINE
         AH    11,H1                   ROOM FOR ONE LETTER
         CLI   0(11),C' '              IS SYMBOL 1 CHARACTER LONG?
         BC    8,ESD9                  YES--EXIT SUBR
         AH    11,H1                   ROOM FOR SECOND CHARACTER
         CLI   0(11),C' '              IS SYMBOL 2 CHARACTERS LONG?
         BC    8,ESD9                  YES--EXIT
         AH    11,H1                   ROOM FOR 3RD CHARACTER
         CLI   0(11),C' '              IS SYMBOL 3 CHARACTERS LONG?
         BC    8,ESD9                  YES--EXIT
         AH    11,H1                   ROOM FOR 4 CHARACTERS
ESD9     LH    12,YTP2                 RESTORE REGISTERS
         LH    9,YTP3
ESDX     BC    15,*                    RETURN ADDRESS STORED HERE
ESD2     AI    ENO,3                   TRY NEXT RLD
         CLC   ENO+1(1),CARD+8         ALL RLD INFO TESTED?
         BC    2,ESD9                  YES--NO ESID FOUND--EXIT SUBR
         SH    12,H3                   NEXT RLD
         BC    15,ESD1                 TEST IT
* LOOK UP SYMBOL IN ENTRY TABLE AND MOVE TO LINE IF FOUND
ENTL     MVI   ENT,0
         MVC   YTP2,0(8)               ADDRESS TO BE CHECKED
         LH    8,YENT                  SCAN ENTRY TABLE
ENT1     CLI   0(8),X'FF'
         BC    8,0(,13)                NOT FOUND
         CLC   0(2,8),YTP2
         BC    8,ENT2                  ADDRESS IN TABLE
         AH    8,H6
         BC    15,ENT1                 TRY NEXT ENTRY
ENT2     MVI   ENT,1                   SET FOUND INDICATOR
         MVC   0(4,11),2(8)            SYMBOL NAME TO LINE
ENT3     AH    11,H1                   INCREMENT TO END OF SYMBOL
         CLI   0(11),C' '
         BC    7,ENT3
         BC    15,0(,13)
* PRINT LINE, SINGLE SPACE, TESTING FIRST FOR OVERFLOW
PS1      STH   13,PS19+2
         BAL   13,CLER
         PUT   PRNT,LINE
         TM    MSSG+7,1                OUTPUT TO CARDS TOO?
         BC    8,PS11                  NO
         PUT   PUNC,LINE+48            PUNCH SOURCE CARD
PS11     MVC   LINE,BLNK
PS19     BC    15,*                    RETURN ADDRESS STORED HERE
* CONVERT 2 BYTES TO HEX--R13 -> ADDRESSES OF SOURCE AND DESTINATION
*  RESPECTIVELY.  EXIT IS MADE TO R13+4.
HXUN     LH    14,0(,13)               ADDRESS OF SOURCE FIELD
         LH    15,2(,13)               ADDRESS OF DESTINATION
         UNPK  0(5,15),0(3,14)         UNPACK THE TWO BYTES
         MVI   4(15),C' '              WIPE OUR GARBAGE IN NEXT BYTE
         TR    0(4,15),HXTR-240        FIX UP DIGITS A-F
         BC    15,4(,13)               EXIT
* SEARCH MNEMONIC TABLE
SRCH     LH    8,YMTB                  MNEMONIC TABLE INDEX
TEST     CLC   0(1,10),0(8)            OP CODE MATCH?
         BC    8,FND                   YES
         AH    8,H6                    TRY NEXT ONE
         CLI   0(8),X'FF'              END OF TABLE REACHED?
         BC    7,TEST                  NO--TRY NEXT OP CODE
         MVI   TYPE,X'FF'              NO VALID OP CODE
         BC    15,0(,13)               EXIT SUBR
FND      MVC   MNEM,1(8)               STORE MNEMONIC
         MVC   TYPE,5(8)               STORE STMT TYPE
         BC    15,0(,13)               EXIT SUBR
* FORMAT BASE-DISPLACEMENT IN 'BD' TO LINE
* IF THE DISPLACEMENT IS OVER 256, EXPRESS IT IN HEX, OTHERWISE DEC.
* TO INSERT A LENGTH OR BASE REGISTER SPECIFICATION, PUT ITS TRUE VALUE
*  IN 'OLEN' AND SET 'FLEN' TO 1--'FLEN' MUST OTHERWISE BE ZERO.
BDP      STH   13,BDPX+2               SAVE RETURN ADDRESS
         MVI   REG,0                   ASSUME NO REGISTER
         TM    BD,X'80'                IS A BASE REGISTER GIVEN?
         BC    8,BDDA                  NO--TAKE AS DIRECT ADDRESS
         PACK  REG,BD(1)               GET BASE REGISTER
         NI    REG,X'0F'
         NI    BD,X'0F'
BDDA     CLC   BD,H256                 IS DISPLACEMENT OVER 256?
         BC    12,DEC                  NO--PRINT IT IN DECIMAL
         MVC   0(2,11),CX              HEX DELIMITER
         UNPK  2(5,11),BD(3)           UNPACK DISPLACEMENT
         TR    2(4,11),HXTR-240
         MVI   6(11),C''''
         AH    11,H7                   SKIP OVER HEX TERM
         MVC   BD,H0                   CLEAR BD
         BC    15,LBXB                 CHECK FOR LENGTH/INDEX OR BASE
DEC      BAL   13,DECL                 MOVE DECIMAL VALUE TO LINE
LBXB     CLI   REG,0                   IS A BASE REGISTER GIVEN?
         BC    7,LBXP                  YES--START SETTING UP
         CLI   FLEN,1                  IS A LENGTH OR INDEX REG. GIVEN?
         BC    8,LBXP                  YES--IT'S A LENGTH
         BC    4,BDPZ                  NO
         AI    OLEN,0                  IS THE FIELD ZERO?
         BC    8,BDPZ                  YES--NO BASE OR INDEX
LBXP     MVI   0(11),C'('              START WITH ONE OF THESE
         AH    11,H1
         CLI   FLEN,1                  IS A LENGTH OR INDEX GIVEN?
         BC    4,BR                    NO--GET BASE REGISTER
         BC    8,LN                    A LENGTH IS GIVEN
         AI    OLEN,0                  IS AN INDEX REGISTER GIVEN?
         BC    8,NXR                   NO
LN       MVC   BD,OLEN
         BAL   13,DECL                 LENGTH OR INDEX TO LINE
         CLI   REG,0                   IS A BASE REGISTER GIVEN?
         BC    8,LBXX                  NO--PRINT CLOSING PARENTHESIS
NXR      MVI   0(11),C','              SEPARATOR
         AH    11,H1
BR       MVC   BD+1(1),REG             BASE REGISTER
         BAL   13,DECL
LBXX     MVI   0(11),C')'              CLOSING PARENTHESIS
         AH    11,H1
BDPZ     MVI   0(11),C' '              MAKE SURE NEXT BYTE IS CLEAR
BDPX     BC    15,*                    RETURN ADDRESS STORED HERE
* FORMAT DECIMAL VALUE TO LINE
DECL     SP    PDEC,PDEC               CLEAR DECIMAL WORK FIELD
         AI    BD,0                    IS BINARY VALUE ZERO?
         BC    8,DEC3                  YES
DEC2     AP    PDEC,P1                 INCREMENT DECIMAL VALUE
         AI    BD,-1                   DECREMENT BINARY VALUE
         BC    2,DEC2                  IF NOT ZERO, LOOP AGAIN
DEC3     OI    PDEC+2,X'0F'            KILL SIGN BITS NOW
         MVI   DEC5+1,X'22'            ASSUME IT'S A 3-DIGIT NUMBER
         MVI   BD+1,3
         CP    PDEC,P100               IS IT?
         BC    10,DEC5                 YES
         MVI   DEC5+1,X'12'            MAYBE IT'S A TWO-DIGIT NUMBER
         MVI   BD+1,2
         CP    PDEC,P10                IS IT?
         BC    10,DEC5                 YES
         MVI   DEC5+1,X'02'            IT'S A ONE-DIGIT NUMBER
         MVI   BD+1,1
DEC5     UNPK  0(3,11),PDEC            UNPACK DECIMAL VALUE
         AH    11,BD                   INCREMENT R11
         BC    15,0(,13)
****************************************
* DS'S, DC'S                           *
****************************************
* HALF-WORDS
H0       DC    Y(0)
H1       DC    Y(1)
H2       DC    Y(2)
H3       DC    Y(3)
H4       DC    Y(4)
H5       DC    Y(5)
H6       DC    Y(6)
H7       DC    Y(7)
H256     DC    Y(256)
YC10     DC    Y(CARD+10)
YC69     DC    Y(CARD+69)
YC72     DC    Y(CARD+72)
YL15     DC    Y(LINE+15)
YL63     DC    Y(LINE+63)
YL65     DC    Y(LINE+65)
YL97     DC    Y(LINE+97)
YINS     DC    Y(INST)
YIN2     DC    Y(INST+2)
YMTB     DC    Y(MTAB)
YESD     DC    Y(TESD)
YENT     DC    Y(TENT)
YEND     DC    Y(TENT+120)
KQCT     DC    Y(0)                    TO COUNT TYPE K AND TYPE Q CARDS
YTMP     DC    Y(0)
YTP2     DS    H
YTP3     DS    H
ACTR     DS    H                       ADDRESS COUNTER FOR TYPE Q
YLIM     DS    H
ENO      DS    H
BD       DS    H
OLEN     DS    H
INST     DS    CL6                     CURRENT INSTRUCTION STORED HERE
         DS    CL4
* I/O AREAS
CARD     DS    CL80
RBUF     DS    CL80
BLNK     DC    CL2' '
LINE     DS    CL132
XPUN     DS    CL80
         DS    CL1
OGLE     DS    0CL132                  ORG STATEMENT
         DS    CL57
         DC    CL13'ORG   X''    '''
         DS    CL62
* SWITCHES
OF       DC    XL1'1'                  PAGE OVERFLOW INDICATOR
ADV      DC    XL1'0'                  ON IF READER IS ONE CARD AHEAD
ESD      DS    CL1
ENT      DS    CL1
FLEN     DS    CL1
REG      DS    CL1
* MNEMONIC TABLE--6 BYTES PER ENTRY:
*        0--OP CODE
*        1-4--MNEMONIC
*        5--INSTRUCTION TYPE CODE:
*              1--RX
*              2--SI
*              4--SS1
*              5--SS2
MTAB     DC    XL6'40E2E3C84001'       STH
         DC    XL6'45C2C1D34001'       BAL
         DC    XL6'47C2C3404001'       BC
         DC    XL6'48D3C8404001'       LH
         DC    XL6'49C3C8404001'       CH
         DC    XL6'91E3D4404002'       TM
         DC    XL6'92D4E5C94002'       MVI
         DC    XL6'94D5C9404002'       NI
         DC    XL6'95C3D3C94002'       CLI
         DC    XL6'96D6C9404002'       OI
         DC    XL6'A0E2D7E2C302'       SPSC
         DC    XL6'A1E2D9C34002'       SRC
         DC    XL6'A4E7C9D6C602'       XIOF
         DC    XL6'A5E3C9D64002'       TIO
         DC    XL6'A6C1C9404002'       AI
         DC    XL6'A8D3D7E2C302'       LPSC
         DC    XL6'A9C8D7D94002'       HPR
         DC    XL6'AAC1C8404001'       AH
         DC    XL6'ABE2C8404001'       SH
         DC    XL6'D1D4E5D54004'       MVN
         DC    XL6'D2D4E5C34004'       MVC
         DC    XL6'D4D5C3404004'       NC
         DC    XL6'D5C3D3C34004'       CLC
         DC    XL6'D6D6C3404004'       OC
         DC    XL6'DCE3D9404004'       TR
         DC    XL6'DEC5C4404004'       ED
         DC    XL6'F1D4E5D64005'       MVO
         DC    XL6'F2D7C1C3D205'       PACK
         DC    XL6'F3E4D5D7D205'       UNPK
         DC    XL6'F8E9C1D74005'       ZAP
         DC    XL6'F9C3D7404005'       CP
         DC    XL6'FAC1D7404005'       AP
         DC    XL6'FBE2D7404005'       SP
         DC    XL6'FCD4D7404005'       MP
         DC    XL6'FDC4D7404005'       DP
         DC    XL1'FF'                 END OF MNEMONIC TABLE
* OTHER STORAGE
CT       DC    XL3'C'
PCT      DC    XL2'C'
PDEC     DC    XL3'C'
CEND     DC    CL8'END   X'''
TYPE     DS    CL1                     INSTRUCTION TYPE
MNEM     DS    CL4
STMT     DC    XL3'C'
P1       DC    XL1'1C'
P10      DC    XL2'10C'
P100     DC    XL2'100C'
ED1      DC    XL5'2020202120'
EREF     DS    CL1
CDC      DC    CL8'DC    X'''
CX       EQU   CDC+6,2
CENT     DC    CL5'ENTRY'
CEXT     DC    CL5'EXTRN'
CCTL     DC    C'CTL   ABS,16383,'
         DC    C'16383'
CST      DC    CL8'START X'''
COM1     DC    CL16'ESID = X''     LE'
         DC    CL9'NGTH = X'''
COM2     DC    CL16'LOAD RELATIVE ES'
         DC    CL7'ID = X'''
MSC1     DC    CL3''', '
HDR1     DC    CL16'PROGRAM CJG7104'
         DC    CL16' '
         DC    CL10' '
         DC    CL16'UNIVAC 9300 OBJE'
         DC    CL16'CT CODE DISASSEM'
         DC    CL6'BLER'
         DC    CL16' '
         DC    CL16' '
         DC    CL16'PAGE'
         DC    CL4' '
HDR2     DC    CL16'STMT      ADDR'
         DC    CL16'OBJECT CODE'
         DC    CL16' '
         DC    CL16'SOURCE STATEMENT'
         DC    CL16' '
         DC    CL16' '
         DC    CL16' '
         DC    CL16' '
         DC    CL4' '
HDR3     DC    CL16'NUMBER OF CARDS:'
         DC    XL6'402020202120'
HXTR     DC    CL16'0123456789ABCDEF'  HEX FIXUP TRANSLATE TABLE
TESD     DS    20CL5                   ESID TABLE--ROOM FOR 20 ESID'S
         DC    XL1'FF'                 END OF TABLE MARKER
TENT     DS    20CL6                   ENTRY SYMBOL TABLE--ROOM FOR 20
         DC    XL1'FF'
* EXTRNS AND ENTRYS
         EXTRN READ
         EXTRN PRNT
         EXTRN PUNC
         EXTRN TBPU
         ENTRY FOF
         ENTRY REOF
         ENTRY RBUF
         ENTRY XPUN
         ENTRY BEGN
         END   BEGN
