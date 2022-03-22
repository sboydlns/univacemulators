***********************************************************************
*                                                                     *
* A PROGRAM TO TEST ACCESS TO RESTRICTED SUPERVISOR TABLES. AN        *
* EXPERIMENT TO SEE IF I CAN FIGURE OUT HOW THE BEM /DISPLAY JOBS     *
* AND /DISPLAY VOLUMES COMMANDS WORK.                                 *
*                                                                     *
***********************************************************************
*
SUPTST   START
         PRINT NOGEN
         SUPEQU REGS=YES,                                              X
               TCB=YES,                                                X
               PRE=YES,                                                X
               SIB=YES,                                                X
               IO=NO,                                                  X
               TRN=NO
PUBDSECT DSECT
         PUBEQU
SUPTST   CSECT                        
         PRINT GEN               
*               
         BALR  R2,0
         USING *,R2
         USING SB$SIB,R3
         USING JP$PRE,R4
         USING JT$TCB,R5
         USING IP$PUB,R6
************
*
* TRY TO SCAN THE SWITCH LIST LOOKING FOR JOBS
*
************
*
* GET JOB PREAMBLE TO GET ABSOLUTE BASE ADDRESS OF THIS PROGRAM
*
         GETINF PRE,PREBFR,L'PREBFR,0
         LA    R4,PREBFR
*
* GET THE SYSTEM INFORMATION BLOCK (SIB)
*
         GETINF SIB,SIBBFR,L'SIBBFR,0
         LA    R3,SIBBFR
* 
* CALCULATE A POINTER TO THE FIRST NON-SUPERVISOR TCB CHAIN. THIS WILL
* END UP BEING A NEGATIVE ADDRESS WHICH, WHEN ADDED TO THE CURRENT
* RELOCATION REGISTER (JP$JSB), WILL GIVE US THE ADDRESS IN THE
* SUPERVISOR TO READ. FORTUNATELY, OS/3 DOESN'T SEEM TO SET THE
* READ PROTECTED BIT IN THE STORAGE KEY SO WE CAN GET AWAY WITH THIS.
*
         LH    R11,SB$SLA              GET ABS ADDR OF SWITCH LIST
         AH    R11,=H'16'              SKIP SUPERVISOR TCBS
         S     R11,JP$JSB              SUBTRACT BASE ADDR OF PRGM
         MVC   BASEADR,JP$JSB          SAVE BASE ADDR FOR LATER
*         
SWLOOP1  L     R5,0(R11)               GET ADDR OF 1ST TCB IN LIST
         LA    R5,0(R5)                CLEAR MSB
         ST    R5,FSTTCB               SAVE IT
SWLOOP   LTR   R5,R5                   ZERO?
         BZ    SWBUMP                  YES, TRY NEXT CHAIN
         S     R5,BASEADR              MAKE NEGATIVE OFFSET INTO SUPER
         L     R12,JT$ECB              IS ECB PTR ZERO?
         LA    R12,0(R12)
         LTR   R12,R12
         BNZ   SWNTCB                  NO, NOT A PRIMARY TCB
         L     R4,JT$PRE               GET PTR TO PREAMBLE
         LA    R4,0(R4)                CLEAR MSB
         S     R4,BASEADR              MAKE NEGATIVE OFFSET INTO SUPER
         MVC   JOBNAME,JP$NAM
         OPR   JOBNAME,8
SWNTCB   L     R5,JT$LNK               GET PRT TO NEXT TCB
         LA    R5,0(R5)                CLEAR MSB
         C     R5,FSTTCB               NEXT = FIRST?
         BE    SWBUMP                  YES, END OF CHAIN, TRY NEXT
         B     SWLOOP                  NO, LOOP
*         
SWBUMP   LA    R11,4(R11)              BUMP TO NEXT TCB CHAIN
         TM    0(R11),X'FF'            IS 1ST BYTE OF TCB ADDR X'FF'?
         BO    SWDONE                  YES, END OF LIST
         B     SWLOOP1
*
* DONE JOBS, DISPLAY MOUNTED VOLUMES
*
SWDONE   LA    R6,PUBBFR               COVER PUB
         XC    PUBNUM,PUBNUM           SET PUB # TO ZERO
         LH    R1,PUBNUM               
VLOOP    GETINF PUB,PUBBFR,L'PUBBFR,0  GET INFO FOR PUB # GIVEN IN R1
         TM    IP$TYP,X'20'            IS IT A DISK?
         BZ    VNEXT                   NO, TRY NEXT
         LH    R11,IP$TRL              GET ADDR OF PUB TRAILER
         LTR   R11,R11                 ZERO?
         BZ    VNEXT                   YES, TRY NEXT
         S     R11,BASEADR             MAKE NEGATIVE OFFSET INTO SUPER
         MVC   PTRLBFR,0(R11)          COPY TRAILER TO MY BUFFER
         CLI   IP$VSN,0                IS THERE A VOLUME MOUNTED?
         BE    VNEXT                   NO, TRY NEXT
         LA    R1,IP$VSN               SUCCESS, SHOW IT TO THE OPERATOR
         OPR   (1),L'IP$VSN
VNEXT    AI    PUBNUM,1                BUMP PUB #
         LH    R1,PUBNUM
         C     R1,SB$PBC               REACHED MAX PUBS?
         BNH   VLOOP                   NO                
*
         EOJ      
*
JOBNAME  DS    CL8
BASEADR  DS    A         
FSTTCB   DS    A         
PREBFR   DS    XL(JP$LNGTH)
SIBBFR   DS    XL(SB$LNGTH)
PUBBFR   DS    XL(IP$LNGTH)
PTRLBFR  DS    XL(IP$LENT)
PUBNUM   DS    H
*
         END
// FIN