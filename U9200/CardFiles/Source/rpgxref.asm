* RPG CROSS-REFERENCE LISTING  18MAY71  9300 ASSEMBLER                  7103001
* SUBSEQUENT SOURCE DECKS WILL BE PROCESSED WITHOUT STOPPING IF THEY
*  ARE SEPARATED BY EXACTLY ONE SENTINEL CARD
G103     START 0
         USING *,0
         USING *,1
         USING *,2
         USING *,3
* BOTH TABLES MUST BE HALF-WORD ALIGNED
STAB     DS    256CL8                  LABEL TABLE
         DS    209CL8                   ROOM FOR 465 LABELS
STND     DS    0CL1                    MUST FOLLOW STAB
STCL     DC    XL8'4040404040400000'   MUST FOLLOW STND
RTAB     DS    200CL48                 REFERENCE TABLE
RTND     DS    0H
BEGN     RSTRT RSTR
RSTR     MVC   LINE,BLNK               INITIAL CLEARING OF LINE
         BAL   13,TCLR                 CLEAR TABLES
MSSG     MSG   X'333'
         OPEN  READ
         OPEN  PRNT
***********************************************************************
* READ CARD
***********************************************************************
A100     GET   READ,CARD
         CLI   CARD,C'/'               EOF?
         BC    8,A300                  YES
A101     LH    8,YYFS                  SCAN FOR CARD TYPE
A102     CLC   CARD+5(1),0(8)          MATCH?
         BC    7,A103                  NO--TRY ANOTHER
         CLC   TYPE,1(8)               MATCH WITH LAST ONE?
         BC    8,A105                  YES
         BC    2,A104                  TYPE SEQUENCE ERROR
         MVI   OF,1                    NEW PAGE FOR NEW CARD TYPE
         MVC   TYPE,1(8)               SAVE NEW TYPE CODE
         BC    15,A105                 CONTINUE
A103     AH    8,H4                    TRY NEXT TYPE CODE
         CH    8,YYFN                  ANY MORE LEFT?
         BC    4,A102                  YES
         BAL   13,CLER
         PUT   PRNT,LINE               PRINT ILLEGAL TYPE CARD
         MVC   LINE,BLNK
         MVC   LINE(24),CNTP
         PUT   PRNT,LINE               PRINT ILLEGAL TYPE MESSAGE
         MVC   LINE,BLNK               CLEAR MESSAGE
         BC    15,A100                 GET NEXT CARD
A104     MVI   SQER,1                  CARD TYPE SEQUENCE ERROR
A105     CLI   CARD+6,C'*'             COMMENT CARD?
         BC    8,A106                  YES
         AP    CT,P1                   INCREMENT CARD COUNT
         MVC   LINE(4),ED3
         ED    LINE(4),CT              CARD NUMBER TO LINE
A106     CLI   MSSG+7,1                SUPPRESS LISTING?
         BC    8,A108                  YES
         BAL   13,CLER
         PUT   PRNT,LINE               LIST CARD
A108     CLI   CARD+6,C'*'             COMMENT CARD?
         BC    8,A100                  YES--DON'T PROCESS ANY MORE
***********************************************************************
* SCAN CARD FOR FIELDS
***********************************************************************
A200     MVI   VAR,1                   SCAN FOR VARIABLES FIRST
         LH    9,2(,8)                 GET PROPER POINTER
A210     CLI   0(9),X'FF'              END OF GROUP?
         BC    7,A220                  NO--CONTINUE SCAN
         CLI   VAR,1                   SCANNING FOR VARIABLES?
         BC    7,A290                  NO--WE'RE DONE
         MVI   VAR,0                   SCAN FOR INDICATORS
A215     AH    9,H2                    GET NEXT POINTER
         BC    15,A210                 CHECK FOR END
A220     LH    8,0(,9)                 R8->RIGHT SPOT ON CARD
         CLI   VAR,1                   LOOKING FOR VARIABLES?
         BC    7,A222                  NO--CHECK FOR INDICATOR
         CLI   0(8),X'C1'              VALID SYMBOL NAME?
         BC    4,A215                  NO--TRY NEXT POSITION ON CARD
         CLI   0(8),X'E9'              REJECT IF ABOVE Z, TOO
         BC    2,A215
         BC    15,A224                 MOVE NAME TO TABLE
A222     CLI   0(8),X'40'              ANYTHING THERE AT ALL?
         BC    8,A215                  NO
         MVI   SNAM,X'FF'              MOVE IN INDICATOR FLAG
         MVC   SNAM+1(2),0(8)          MOVE INDICATOR TO SNAM
         MVC   SNAM+3(3),HD1+16        BLANK OUT REMAINDER OF SNAM
         BC    15,A230
A224     MVC   SNAM,0(8)               VARIABLE NAME TO SNAM
A230     LH    10,YSTB                 SCAN SYMBOL TABLE
A232     CH    10,YSTA                 END OF ACTIVE TABLE?
         BC    10,A240                 YES--NAME GOES AT END
         CLC   SNAM,0(10)              MATCH WITH SOMETHING ALREADY IN?
         BC    8,A250                  YES--ADD ANOTHER REFERENCE
         BC    4,A240                  NEW NAME GOES HERE
         AH    10,H8                   TRY NEXT ENTRY
         BC    15,A232
* ADD NEW SYMBOL TO LABEL TABLE
A240     CLC   YSTA,YSTN               TABLE FULL?
         BC    4,A241                  NO--CONTINUE
         MVI   STOF,1                  SET OVERFLOW INDICATOR
         BC    15,A215                 IGNORE THIS LABEL
A241     STH   10,YTP2                 REMEMBER WHERE IT GOES
         LH    10,YSTA                 SHIFT ENTRIES TO MAKE ROOM
A242     SH    10,H8                   BACK TO PREVIOUS ENTRY
         CH    10,YTP2                 BACK TO DESIRED POINT?
         BC    4,A243                  YES--ADD NEW ENTRY
         MVC   8(8,10),0(10)           SHIFT TABLE ENTRY RIGHT
         BC    15,A242                 TRY AGAIN
A243     MVC   8(6,10),SNAM            NEW NAME TO TABLE
         AI    YSTA,8                  INDICATE IT HERE
         CLC   YRTA,YRTN               ROOM FOR REFERENCES?
         BC    4,A246                  YES
A244     MVI   RTOF,1                  SET OVERFLOW INDICATOR
         BC    15,A215                 IGNORE THIS REFERENCE
A246     LH    11,YRTA                 END OF ACTIVE REFERENCE TABLE
         ZAP   0(2,11),CT              REFERENCE TO TABLE
         STH   11,14(,10)              LINK FROM NAME TABLE
         AI    YRTA,16                 LEAVE ROOM FOR NEW REFERENCES
         BC    15,A215                 TRY NEXT POSITION ON CARD
* ADD NEW REFERENCE TO REF. TABLE
A250     LH    11,6(,10)               LINK TO REFERENCES
A251     STH   11,YTP2
         AI    YTP2,14                 LIMIT OF REFS IN THIS ENTRY
A252     CLI   0(11),X'FF'             END OF REFS?
         BC    8,A255                  YES--ADD NEW ONE
         AH    11,H2                   TRY NEXT POSITION IN ENTRY
         CH    11,YTP2                 END OF ENTRY?
         BC    4,A252                  NO--TEST THIS ONE
         CLC   0(2,11),H0              MORE REFS LINKED IN?
         BC    8,A254                  NO--START NEW ENTRY
         LH    11,0(,11)               LINK TO NEW ENTRY
         BC    15,A251                 DO THE SAME ROUTINE AGAIN
* MAKE NEW REFERENCE ENTRY (2ND OR HIGHER) AND LINK TO LAST ENTRY
A254     CLC   YRTA,YRTN               ROOM FOR ANOTHER ENTRY?
         BC    10,A244                 NO
         MVC   0(2,11),YRTA            STORE LINK
         LH    11,YRTA                 GO TO NEW AREA
         AI    YRTA,16                 INCREMENT ACTIVE END POINTER
* INSERT NEW REF INTO ENTRY
A255     ZAP   0(2,11),CT              THERE IT GOES
         BC    15,A215                 CHECK NEXT POSITION ON CARD
* PRINT TYPE SEQUENCE RRROR MESSAGE IF CARD WAS IN ERROR
A290     CLI   SQER,1                  SEQUENCE ERROR?
         BC    7,A100                  NO--GET NEXT CARD
         MVC   LINE,BLNK               CLEAR LINE
         MVC   LINE(24),SQMS
         BAL   13,CLER
         PUT   PRNT,LINE               PRINT ERROR MESSAGE
         MVC   LINE,BLNK
         MVI   SQER,0                  RESET SEQUENCE ERROR INDICATOR
         BC    15,A100                 GET NEXT CARD
***********************************************************************
* PRINT THE CROSS-REFERENCE DICTIONARY
***********************************************************************
A300     MVI   XREF,1                  SET INDICATOR FOR HEADING RTN
         CNTRL PRNT,SP,1,0
         CLI   STOF,1                  LABEL TABLE OVERFLOW?
         BC    7,A301                  NO
         MVC   LINE(31),SOFM
         BAL   13,CLER
         PUT   PRNT,LINE               PRINT OVERFLOW MESSAGE
         MVC   LINE,BLNK
         MVI   STOF,0                  RESET INDICATOR
A301     CLI   RTOF,1                  REF TABLE OVERFLOW?
         BC    7,A309                  NO
         MVC   LINE(35),ROFM
         BAL   13,CLER
         PUT   PRNT,LINE               PRINT OVERFLOW MESSAGE
         MVC   LINE,BLNK
         MVI   RTOF,0                  RESET INDICATOR
A309     CNTRL PRNT,SP,0,1
* PRINT CROSS-REFERENCE DICTIONARY
A350     BAL   13,HDG                  NEW PAGE FOR THE GREAT EVENT
         MVC   LINE(25),CVAR
         CNTRL PRNT,SP,0,2
         PUT   PRNT,LINE               TELL USER HERE COME VARIABLES
         MVC   LINE,BLNK
         LH    8,YSTB
         MVI   VAR,1
A355     CLI   0(8),X'FF'              START OF INDICATORS?
         BC    7,A360                  NO--CONTINUE PRINTING VARIABLES
         BAL   13,HDG
         MVC   LINE(15),CIND           PRINT INDICATOR MESSAGE
         PUT   PRNT,LINE
         MVC   LINE,BLNK
         MVI   VAR,0                   SET INDICATOR INDICATOR
A360     CLI   0(8),X'40'              END OF TABLE REACHED?
         BC    8,A900                  YES--DO FINALIZING ROUTINE
         MVC   LINE(6),0(8)            NAME TO LINE
         CLI   LINE,X'FF'              INDICATOR?
         BC    7,A370                  NO--CONTINUE
         MVC   LINE(2),LINE+1          KILL X'FF'
         MVI   LINE+2,X'40'            ALSO THIS GARBAGE
A370     LH    9,6(,8)                 START DOWN REFS
         LH    10,YL10                 SPREAD THEM ACROSS LINE
         STH   9,YTP2
         AI    YTP2,14                 LIMIT FOR REF ENTRY
A375     CLI   0(9),X'FF'              END OF THIS SET OF REFS?
         BC    8,A390                  YES
         UNPK  1(3,10),0(2,9)          REF TO LINE
         OI    3(10),X'F0'             FIX LAST BYTE
         MVI   PRIN,1                  SET PRINT INDICATOR
         AH    9,H2                    CHECK NEXT REF
         CH    9,YTP2                  END OF THIS ENTRY?
         BC    4,A380                  NO--TRY IT
         CLC   0(2,9),H0               ANOTHER ENTRY LINKED IN?
         BC    8,A390                  NO--THIS ONE'S FINISHED
         LH    9,0(,9)                 LINK DOWN TO NEXT ENTRY
         STH   9,YTP2
         AI    YTP2,14                 NEW SCAN LIMIT
A380     AH    10,H4                   NEXT POSITION ON LINE
         CH    10,YLB0                 END OF LINE?
         BC    4,A375                  NO
         BAL   13,CLER
         PUT   PRNT,LINE               PRINT LINE OF REFS
         MVC   LINE,BLNK
         LH    10,YL10                 START ANOTHER LINE
         MVI   PRIN,0                  RESET PRINT INDICATOR
         BC    15,A375                 GET SOME MORE
A390     CLI   PRIN,1                  LAST LINE YET TO BE PRINTED?
         BC    7,A395                  NO
         BAL   13,CLER
         PUT   PRNT,LINE               PRINT LAST LINE OF REFS
         MVC   LINE,BLNK
         MVI   PRIN,0
A395     AH    8,H8                    TRY NEXT LABEL
         CLI   VAR,1                   VARIABLE?
         BC    7,A360                  NO
         BC    15,A355                 YES
***********************************************************************
* FINALIZING ROUTINE
***********************************************************************
A900     BAL   13,TCLR                 CLEAR TABLES
         GET   READ,CARD
         CLI   CARD,C'/'               MORE DECKS TO PROCESS?
         BC    7,A101                  YES
         CLOSE READ
         CLOSE PRNT
         BC    15,MSSG                 BACK TO BEGINNING HALT
***********************************************************************
* SUBROUTINES
***********************************************************************
*
* TEST FOR PAGE OVERFLOW
*
CLER     CLI   OF,1
         BC    7,0(,13)
HDG      AP    PCT,P1                  INCREMENT PAGE COUNT
         MVC   HD1+116(4),ED3
         ED    HD1+116(4),PCT          PAGE NO. TO LINE
         CNTRL PRNT,SK,7
         CNTRL PRNT,SP,0,2
         PUT   PRNT,HD1                PRINT FIRST HEADING LINE
         MVI   OF,0                    RESET OVERFLOW INDICATOR
         CLI   XREF,1                  IN CROSS-REFERENCE DICTIONARY?
         BC    7,0(,13)                NO--EXIT
         CNTRL PRNT,SP,0,2
         PUT   PRNT,HD2                PRINT 2ND HEADING LINE
         BC    15,0(,13)               EXIT
FOF      MVI   OF,1
         BC    15,0(,14)
REOF     BC    15,2(,14)               NOT USED
*
* CLEAR TABLES
*
TCLR     LH    8,YSTB                  LABEL TABLE FIRST
TCL1     MVC   0(8,8),STCL
         AH    8,H8                    NEXT ENTRY
         CH    8,YSTN                  END OF TABLE?
         BC    4,TCL1                  NO--CLEAR NEXT ENTRY
         LH    8,YRTB                  NOW CLEAR REFERENCE TABLE
TCL2     MVC   0(16,8),RTCL
         AH    8,H16                   NEXT ENTRY
         CH    8,YRTN                  END OF TABLE?
         BC    4,TCL2                  NO--CLEAR NEXT ENTRY
         MVC   YRTA,YRTB               RESET ACTIVE LIMIT POINTER
         MVC   YSTA,YSTB
         SP    CT,CT
         SP    PCT,PCT
         MVI   OF,1
         MVI   TYPE,0
         MVI   XREF,0
         BC    15,0(,13)               EXIT
***********************************************************************
* STORAGE
***********************************************************************
* HALF-WORDS
RTCL     DS    0CL16                   TO CLEAR RTAB
         DC    XL14'FFFFFFFFFFFFFFFFFFFFFFFFFFFF'
H0       DC    Y(0)                    MUST FOLLOW RTCL
H2       DC    Y(2)
H4       DC    Y(4)
H8       DC    Y(8)
H16      DC    Y(16)
YSTB     DC    Y(STAB)
YSTN     DC    Y(STND)
YRTB     DC    Y(RTAB)
YRTN     DC    Y(RTND)
YRTA     DS    H                       END OF ACTIVE PORTION OF REF TBL
YSTA     DS    H                       END OF ACTIVE PORION OF STAB
YTP2     DS    H
YL10     DC    Y(LINE+10)
YLB0     DC    Y(LINE+110)
* WHERE IT'S AT ON THE SOURCE CARDS
FSMS     DC    XL2'FFFF'               NO LABELS ON TYPE F CARDS
FINS     DC    XL2'FFFF'               NO INDICATORS, EITHER
ESMS     DC    Y(CARD+26)              LABELS ON TYPE E CARDS
         DC    Y(CARD+45)
         DC    XL2'FFFF'
EINS     DC    XL2'FFFF'               NO INDICATORS, THOUGH
CSMS     DC    Y(CARD+17)              LABELS ON TYPE C CARDS
         DC    Y(CARD+32)
         DC    Y(CARD+42)
         DC    XL2'FFFF'
CINS     DC    Y(CARD+6)               INDICATORS
         DC    Y(CARD+9)
         DC    Y(CARD+12)
         DC    Y(CARD+15)
         DC    Y(CARD+53)
         DC    Y(CARD+55)
         DC    Y(CARD+57)
         DC    XL2'FFFF'
ISMS     DC    Y(CARD+52)
         DC    XL2'FFFF'
IINS     DC    Y(CARD+18)
         DC    Y(CARD+58)
         DC    Y(CARD+60)
         DC    Y(CARD+62)
         DC    Y(CARD+64)
         DC    Y(CARD+66)
         DC    Y(CARD+68)
         DC    XL2'FFFF'
OSMS     DC    Y(CARD+31)
         DC    XL2'FFFF'
OINS     DC    Y(CARD+23)
         DC    Y(CARD+26)
         DC    Y(CARD+29)
         DC    XL2'FFFF'
* WHERE TO FIND WHERE IT'S AT
YFSM     DC    XL2'C601'               TYPE AND CODE TOO
         DC    Y(FSMS)
YESM     DC    XL2'C502'
         DC    Y(ESMS)
YCSM     DC    XL2'C303'
         DC    Y(CSMS)
YISM     DC    XL2'C904'
         DC    Y(ISMS)
YOSM     DC    XL2'D605'
         DC    Y(OSMS)
* WHERE TO GO TO FIND OUT WHERE IT'S AT (OR SOMETHING LIKE THAT)
YYFS     DC    Y(YFSM)
YYFN     DC    Y(YFSM+20)              IF YOU GET HERE, GIVE UP
* I/O AREAS
BLNK     DC    CL1' '
LINE     DS    CL132
CARD     EQU   LINE+10
HD1      DC    CL16'PROGRAM CJG7103'
         DC    CL16' '
         DC    CL14' '
         DC    CL16'RPG CROSS-REFERE'
         DC    CL11'NCE LISTING'
         DC    CL16' '
         DC    CL16' '
         DC    CL7' '
         DC    CL4'PAGE'
         DC    CL16' '
HD2      DC    CL11'SYMBOL'
         DC    CL16'REFERENCES'
         DC    CL16' '
         DC    CL16' '
         DC    CL16' '
         DC    CL16' '
         DC    CL16' '
         DC    CL16' '
         DC    CL9' '
RBUF     DS    CL80
* SWITCHES
XREF     DC    XL1'0'                  ON DURING CROSS-REFERENCE PRINT
OF       DC    XL1'1'
SQER     DC    XL1'0'                  ON IF TYPE SEQUENCE ERROR
VAR      DS    CL1                     ON WHEN VARIABLE NAME PROCESSED
PRIN     DC    XL1'0'                  ON IF REF. LINE TO BE PRINTED
STOF     DC    XL1'0'                  ON IF LABEL TABLE OVERFLOW
RTOF     DC    XL1'0'                  ON IF REF TABLE OVERFLOW
* MESSAGES
CNTP     DC    CL16'***  UNIDENTIFIE'
         DC    CL8'D RECORD'
SOFM     DC    CL16'***  LABEL TABLE'
         DC    CL15' HAS OVERFLOWED'
ROFM     DC    CL16'***  REFERENCE T'
         DC    CL16'ABLE HAS OVERFLO'
         DC    CL3'WED'
CVAR     DC    CL16'***  VARIABLES A'
         DC    CL9'ND TABLES'
CIND     DC    CL15'***  INDICATORS'
SQMS     DC    CL16'***  TYPE SEQUEN'
         DC    CL8'CE ERROR'
* OTHER STORAGE
TYPE     DC    XL1'0'                  CARD TYPE CODE
SNAM     DS    CL6
CT       DC    XL2'C'
PCT      DC    XL2'C'
ED3      DC    XL4'40202120'
P1       DC    XL1'1C'
         EXTRN READ
         EXTRN PRNT
         ENTRY REOF
         ENTRY RBUF
         ENTRY FOF
         ENTRY BEGN
         END   BEGN
