* ASSEMBLER SOURCE CODE ALIGNMENT CHECK  22JUL70  9300 ASSEMBLER        7011001
*********I/O AND FILE SET-UP
*****INPUT
*        1001-P:   ASSEMBLER SOURCE DECK TO BE CHECKED
*****OUTPUT
*        PRINTER:  LISTING OF ALL CARDS NOT CONFORMING TO STANDARD
*                   ALIGNMENT RULES, WITH POSITION NUMBER IN DECK
*
* FOR 360 PROGRAMS, WHEN COMPUTER STOPS WITH X'333' DISPLAY, KEY A 1
*  INTO LOCATION 4.  (9300 PROGRAMS ARE ADDITIONALLY CHECKED FOR ALL
*  BLANKS IN COLS. 5-8 AND NON-BLANK IN COL. 11)
* AT END OF RUN COMPUTER WILL HALT WITH X'1FFF' DISPLAY.  FOR A NEW
*  RUN, LOAD NEW SOURCE DECK, KEY A 1 INTO LOCATION 4 IF IT IS 360
*  SOURCE CODE, AND HIT START.
* IF A CARD JAM OR OTHER FAILURE PRODUCES AN UNRECOVERABLE CONDITION,
*  HIT CLEAR AND START.  X'1FFF' WILL DISPLAY.
*  FOLLOW NORMAL NEW RUN PROCEDURE.
*
* THE FOLLOWING CHECKS ARE PERFORMED:
*        COLS. 2-8 MUST BE BLANK IF COL. 1 IS BLANK
*        COLS. 5-8 MUST ALWAYS BE BLANK (9300 ONLY)
*        COL. 9 MUST ALWAYS BE BLANK
*        COL. 10 MUST NEVER BE BLANK
*        COL. 11 MUST NEVER BE BLANK (9300 ONLY)
*        COL. 15 MUST ALWAYS BE BLANK
*        COL. 16 MUST NEVER BE BLANK
*        COL. 72 MUST ALWAYS BE BLANK
*  ALL CARDS NOT CONFORMING TO THE ABOVE WILL BE PRINTED ALONG WITH
*  THEIR POSITION NUMBER IN THE DECK.
*
*
G011     START 0
         USING *,0
BEGN     MVI   XYZ,8                   SOURCE DECK IN 1001-P
         MVC   CARD+81(42),CARD+80     CLEAR REST OF OUTPUT LINE
         MVC   HDR4+16(116),HDR4+15    CLEAR REMAINDER OF HDR4
         RSTRT RSTR                    FOR SOFTWARE RECOVERY
         MSG   X'333'
         MVC   S360,*-1                DETERMINE WHETHER 360 PROGRAM
NRUN     OPEN  PRNT                    SUBSEQUENT RUNS START HERE
         OPEN  READ
REED     GET   READ,CARD               READ SOURCE CARD
         CLI   CARD,X'61'              TEST FOR END OF FILE
         BC    8,REOF                  IF SO, GO TO END OF RUN ROUTINE
         AP    CT,P1                   INCREMENT CARD COUNT
         CLI   CARD,X'5C'              COMMENT CARD?
         BC    8,REED                  IF SO, SKIP IT
         CLI   CARD,X'40'              LABEL PRESENT?
         BC    7,SYM                   YES
         CLC   CARD(8),BLNK            NO LABEL, COLS 1-8 MUST BE BLANK
         BC    7,PRIN                  IF NOT, PRINT CARD
         BC    15,B09                  GO CHECK COL. 9
SYM      CLI   S360,1                  360 PROGRAM?
         BC    8,B09                   YES
         CLC   CARD+4(4),BLNK          COLS. 5-8 BLANK (9300 ONLY)
         BC    7,PRIN                  IF NOT, PRINT CARD
B09      CLI   CARD+8,X'40'            COL. 9 MUST BE BLANK
         BC    7,PRIN
         CLI   CARD+9,X'40'            COL. 10 MUST NEVER BE BLANK
         BC    8,PRIN
         CLI   S360,1                  360 PROGRAM?
         BC    8,B15                   YES--GO CHECK COL. 15
         CLI   CARD+10,X'40'           COL. 11 NEVER BLANK (9300 ONLY)
         BC    8,PRIN
B15      CLI   CARD+14,X'40'           COL. 15 MUST BE BLANK
         BC    7,PRIN
         CLI   CARD+15,X'40'           COL. 16 MUST NEVER BE BLANK
         BC    8,PRIN
         CLI   CARD+71,X'40'           COL. 72 MUST BE BLANK
         BC    8,REED
* PRINT OUT CARDS NOT CONFORMING TO STANDARD ALIGNMENT RULES
PRIN     MVC   OUT(5),ED1
         ED    OUT-1(6),CT             POSITION NUMBER IN DECK
         BAL   13,CLER
         PUT   PRNT,OUT                PRINT OFFENDING CARD
         AP    MCT,P1                  COUNT IT
         BC    15,REED                 TRY NEXT ONE
* END OF RUN PROCEDURE
REOF     MVC   HDR4+16(7),CCHK         'NUMBER OF CARDS CHECKED'
         MVC   HDR4+25(5),ED1
         ED    HDR4+24(6),CT           NUMBER OF CARDS CHECKED
         CNTRL PRNT,SP,2,2
         PUT   PRNT,HDR4
         MVC   HDR4+16(7),CPRT         'NUMBER OF CARDS PRINTED'
         MVC   HDR4+25(5),ED1
         ED    HDR4+24(6),MCT          NUMBER OF CARDS PRINTED
         PUT   PRNT,HDR4
RSTR     CLOSE PRNT                    RESTART RECOVERY HERE
         CLOSE READ
          MSG   X'1FFF'
         MVC   S360,*-1                NEW DECK 360 SOURCE CODE?
         SP    CT,CT                   CLEAR COUNTERS, ETC.
         SP    PCT,PCT
         SP    MCT,MCT
         MVI   OF,1
         BC    15,NRUN                 START NEXT RUN
* PAGE OVERFLOW TEST
CLER     CLI   OF,1
         BC    7,0(,13)
         AP    PCT,P1                  INCREMENT PAGE COUNTER
         MVC   HDR1+117(3),ED1+2
         ED    HDR1+116(4),PCT         PAGE NUMBER
         CNTRL PRNT,SK,7,0
         CNTRL PRNT,SP,0,2
         PUT   PRNT,HDR1
         PUT   PRNT,HDR2               COLUMN NUMBER (TENS)
         CNTRL PRNT,SP,0,2
         PUT   PRNT,HDR3               COLUMN NUMBER (UNITS)
         MVI   OF,0                    RESET OVERFLOW INDICATOR
         BC    15,0(,13)
* PAGE OVERFLOW DETECTION
FOF      MVI   OF,1
         BC    15,0(,14)
*
***********************************************************************
*
* I/O AREAS AND HEADER LINES
         DC    C' '
OUT      DC    CL8' '                  OUTPUT LINE STARTS HERE
CARD     DS    CL80                    CARD READ IN HERE
         DC    C' '
         DS    CL42                    REST OF OUTPUT LINE
HDR1     DC    CL16'CHARLES J. GIBBS'
         DC    CL16' '
         DC    CL16'   PROGRAM 7011-'
         DC    CL16'-ASSEMBLER SOURC'
         DC    CL16'E CODE ALIGNMENT'
         DC    CL16' CHECK'
         DC    CL16' '
         DC    CL8'PAGE'
HDR2     DC    CL16' '                 COLUMN NUMBER (TENS)
         DC    CL16' 1         2    '
         DC    CL16'     3         4'
         DC    CL16'         5      '
         DC    CL16'   6         7  '
         DC    CL16'       8'
         DC    CL16' '
         DC    CL16' '
         DC    CL4' '
HDR3     DC    CL8' '                  COLUMN NUMBER (UNITS)
         DC    CL16'1234567890123456'
         DC    CL16'7890123456789012'
         DC    CL16'3456789012345678'
         DC    CL16'9012345678901234'
         DC    CL16'5678901234567890'
         DC    CL16' '
         DC    CL16' '
         DC    CL12' '
HDR4     DC    CL16'NUMBER OF CARDS '
         DS    CL116
CCHK     DC    CL7'CHECKED'
CPRT     DC    CL7'PRINTED'
* OTHER STUFF
CT       DC    XL3'C'                  CARD COUNT
PCT      DC    XL2'C'                  PAGE COUNT
MCT      DC    XL3'C'                  MISALIGNED CARD COUNT
P1       DC    XL1'1C'
OF       DC    XL1'1'                  OVERFLOW INDICATOR
S360     DS    CL1                     360 PROGRAM INDICATOR
XYZ      DS    CL1
ED1      DC    XL5'2020202120'
BLNK     EQU   HDR3
RBUF     DS    CL80                    READER IOCS BUFFER
* EXTRNS & ENTRYS
         EXTRN PRNT
         EXTRN READ
         EXTRN TBRD
*         EXTRN TXS3
         ENTRY FOF
         ENTRY REOF
         ENTRY XYZ
         ENTRY RBUF
         ENTRY BEGN
         END   BEGN
