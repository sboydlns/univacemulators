         TITLE '/DISPLAY COMMAND FOR BOYD''S BEM SIMULATOR'
BEMDSP   START
***********************************************************************
*                                                                     *
* AN IMPLEMENTATION OF THE /DISPLAY COMMAND FOR BOYD'S BEM SIMULATOR. *
*                                                                     *
***********************************************************************
*
         PRINT NOGEN
         SUPEQU REGS=YES,                                              X
               TCB=YES,                                                X
               PRE=YES,                                                X
               SIB=YES,                                                X
               IO=NO,                                                  X
               TRN=NO
PUBDSECT DSECT
         PUBEQU
BEMDSP   CSECT         
         ZM#DPIB
         ZM#DIMH
ZA#IMSG  DS    CL2048         
BEMDSP   CSECT 
         BEMDSCTS
*         
         STM   R14,R12,12(R13)
************
* R2  = COVER
* R3  = PIB
* R4  = IMA
* R5  = WORK AREA
* R6  = OMA
* R7  = CDA
* R14 = RETURN ADDRESS
************         
         BALR  R2,0                    SET COVER
         USING *,R2
         USING LOWMEM,R0
         USING ZA#DPIB,R3
         USING ZA#IMH,R4
         USING WORKAREA,R5
         USING ZA#OMH,R6
*         
         L     R3,0(R1)                COVER THE PARAMETERS
         L     R4,4(R1)
         L     R5,8(R1)
         L     R6,12(R1)
         L     R7,16(R1)
*
* GET THE JOB'S TCB TO GET THE JOB NUMBER
*
         USING JT$TCB,R7
         LA    R1,TCBBFR
         GETINF TCB,(1),L'TCBBFR,0
         LA    R7,TCBBFR
         XR    R11,R11                 GET THE JOB KEY
         IC    R11,JT$KEY
         SRL   R11,4                   SHIFT RIGHT TO GET JOB #
         STC   R11,JOBNUM              SAVE IT FOR LATER
         DROP  R7         
*
* GET JOB PREAMBLE TO GET ABSOLUTE BASE ADDRESS OF THIS PROGRAM AND
* ITS JOB NUMBER
*
         USING JP$PRE,R7
         LA    R1,PREBFR
         GETINF PRE,(1),L'PREBFR,0
         LA    R7,PREBFR
         MVC   BASEADR,JP$JSB          SAVE BASE ADDR OF PGM FOR LATER
         MVI   BASEADR,0               CLEAR MSB
         DROP  R7
*
* GET SYSTEM INFORMATION BLOCK (SIB) TO GET ADDRESS OF SWITCH LIST
* AND PUBS AND OTHER STUFF
*
         USING SB$SIB,R7
         LA    R1,SIBBFR
         GETINF SIB,(1),L'SIBBFR,0
         LA    R7,SIBBFR
         LH    R11,SB$SLA              GET SWITCH LIST ADDR.
         LA    R11,16(R11)             BUMP PAST SUPER TCBS
         S     R11,BASEADR             MAKE NEGATIVE OFFSET INTO SUPER
         ST    R11,SWADDR
         L     R11,SB$PBA              GET 1ST PUB ADDR.
         LA    R11,0(R11)              CLEAR MSB
         S     R11,BASEADR             MAKE NEGATIVE OFFSET INTO SUPER
         ST    R11,PUBADDR
         MVC   NUMPUBS,SB$PBC+2        GET # NUM OF PUBS
         MVI   NUMPRI,0                GET # OF USER PRIORITIES
         MVC   NUMPRI+1(1),SB$NOPRI
         LH    R11,SB$MLO              GET ADDR 1ST MEMORY BLOCK
         SLL   R11,8
         S     R11,BASEADR             MAKE NEGATIVE OFFSET INTO SUPER
         ST    R11,MEMLOW
         LH    R11,SB$MHI              GET ADDR LAST MEMORY BLOCK
         SLL   R11,8
         S     R11,BASEADR             MAKE NEGATIVE OFFSET INTO SUPER
         ST    R11,MEMHI
         DROP  R7         
*
* DECODE THE INPUT MESSAGE
*         
         LH    R0,ZA#ITL               SET UP GETOKEN PARAMS
         LA    R1,ZA#IMSG
         L     R15,LM$GTKN
         LA    R11,DTYPE               BURN '/DISPLAY'
         BALR  R14,R15
         LA    R11,DTYPE               GET TYPE OF STATUS
         BALR  R14,R15
*
* CHECK STATUS TYPE AND BRANCH TO APPROPRIATE ROUTINE
*
         CLI   DTYPE,C'J'              JOBS?
         BE    SENDJOB                 YES
         CLI   DTYPE,C'V'              VOLUMES?
         BE    SENDVOL                 YES
*
* SEND 'INVALID OPTION' ERROR
*
         MVC   ZA#OMSG(BADOPTL),BADOPT MOVE ERROR TO OMA
         LA    R11,BADOPTL             SET MSG LENGTH
         STH   R11,ZA#OTL
         MVI   ZA#PSIND,ZA#PSNN        SET NORMAL TERMINATION
         B     DONE                  
*
* SEND JOBS INFO
*
         USING JT$TCB,R7
         USING JP$PRE,R9
SENDJOB  MVC   ZA#OMSG(JOBHDRL),JOBHDR MOVE JOBS HEADER TO OMA
         LA    R11,JOBHDRL             SET MSG LENGTH
         STH   R11,ZA#OTL
         MVI   ZA#PSIND,ZA#PSNN        SET NORMAL TERMINATION
*         
SJLOOP1  L     R7,SWADDR               GET ADDR OF 1ST TCB IN CHAIN
         L     R7,0(R7)
         LA    R7,0(R7)                CLEAR MSB
         ST    R7,FSTTCB               SAVE IT
SJLOOP2  LTR   R7,R7                   TCB ADDR = ZERO?
         BZ    SJBUMP                  YES, TRY NEXT CHAIN
         S     R7,BASEADR              MAKE NEGATIVE OFFSET INTO SUPER
         L     R11,JT$ECB              IS ECB PTR = ZERO?
         LA    R11,0(R11)
         LTR   R11,R11                 
         BNZ   SJNEXT                  YES, NOT PRIMARY TCB
*
* DISPLAY ONE LINE FOR CURRENT JOB 
*         
         L     R9,JT$PRE               GET JOB PREAMBLE ADDR.
         LA    R9,0(R9)                CLEAR MSB
         S     R9,BASEADR              MAKE NEGATIVE OFFSET INTO SUPER
         LA    R12,ZA#OMSG             GET ADDR OF OUTPUT MSG
         AH    R12,ZA#OTL              ADD CRNT OFFSET INTO OMA
         MVC   0(JOBDTLL,R12),JOBDTL   MOVE JOB DETAIL TEMPLATE TO OMA
         AI    ZA#OTL,JOBDTLL          BUMP MSG LENGTH
*         
         MVC   JNAME(8,R12),JP$NAM     COPY JOB NAME TO OMA
         MVC   JEXEC(8,R12),JP$ROOT    COPY EXEC MODULE TO OMA
         L     R11,JP$JOB                   GET MEM. SIZE
         CVD   R11,DTEMP                    CVT TO PACKED
         MVC   CTEMP(L'SIZMASK),SIZMASK     MAKE PRINTABLE
         ED    CTEMP(L'SIZMASK),DTEMP+4
         OI    CTEMP+7,X'F0'
         MVC   JSIZE(6,R12),CTEMP+2         COPY TO OMA
         XR    R11,R11                      GET JOB STEP #
         IC    R11,JP$JSN              
         CVD   R11,DTEMP
         MVC   CTEMP(L'STEPMASK),STEPMASK   MAKE PRINTABLE
         ED    CTEMP(L'STEPMASK),DTEMP+6
         OI    CTEMP+3,X'F0'
         MVC   JSTEP(3,R12),CTEMP+1         COPY TO OMA
         XR    R11,R11                      GET JOB #
         IC    R11,JT$KEY
         SRL   R11,4
         CVD   R11,DTEMP
         MVC   CTEMP(L'JIDMASK),JIDMASK     MAKE PRINTABLE
         ED    CTEMP(L'JIDMASK),DTEMP+5
         OI    CTEMP+5,X'F0'
         MVC   JNUM(5,R12),CTEMP+1          COPY TO OMA
         L     R11,JP$AJT                   GET ACCUM. CPU TIME
         XR    R10,R10                      DIVIDE BY 100
         D     R10,=F'100'
         CVD   R11,DTEMP                    CONVERT QUOTIENT TO PACKED
         MVC   CTEMP(L'JTIMASK),JTIMASK     MAKE PRINTABLE
         ED    CTEMP(L'JTIMASK),DTEMP+4
         OI    CTEMP+8,X'F0'
         MVC   JTIME(8,R12),CTEMP+1
* 
SJNEXT   L     R7,JT$LNK               GET PTR TO NEXT TCB
         LA    R7,0(R7)                CLEAR MSB
         C     R7,FSTTCB               NEXT = FIRST?
         BE    SJBUMP                  YES, END OF CHAIN, TRY NEXT
         B     SJLOOP2
*
SJBUMP   AI    NUMPRI,-1               DECR. # PRIORITIES
         BNP   SJMEM                   <= ZERO, WE'RE DONE WITH JOBS
         L     R11,SWADDR              BUMP TO NEXT TCB CHAIN
         LA    R11,4(R11)
         ST    R11,SWADDR              SAVE IT
         B     SJLOOP1                 & LOOP
*
* FOLLOW THE MEMORY BLOCK CHAIN TO CALC. FREE MEMORY. BLOCK HEADERS
* ARE THE SAME FORMAT AS THE JOB PREAMBLE.
*
SJMEM    XR    R12,R12                 CLR. TTL MEMORY      
         L     R9,MEMLOW               GET PTR TO 1ST BLOCK
SMLOOP   TM    JP$SIZ,X'80'            IS THIS A FREE BLOCK?
         BZ    SMNEXT                  NO                        
         A     R12,JP$SIZ              ADD BLOCK SIZE TO TTL
         LA    R12,0(R12)              CLEAR MSB
SMNEXT   LH    R9,JP$MHI               GET NEXT BLOCK
         LTR   R9,R9                   ZERO?
         BZ    SMSHOW                  YES, END OF LIST
         SLL   R9,8
         S     R9,BASEADR              MAKE NEGATIVE OFFSET INTO SUPER
         B     SMLOOP                  & LOOP
*
SMSHOW   LR    R11,R12                 SAVE TTL MEMORY                    
         LA    R12,ZA#OMSG             GET ADDR OF OUTPUT MSG
         AH    R12,ZA#OTL              ADD CRNT OFFSET INTO OMA
         MVC   0(JOBFREL,R12),JOBFRE   MOVE FREE MEM. TEMPLATE TO OMA
         AI    ZA#OTL,JOBFREL          BUMP MSG LENGTH
         CVD   R11,DTEMP                    CVT FREE MEM TO PACKED
         MVC   CTEMP(L'SIZMASK),SIZMASK     MAKE PRINTABLE
         ED    CTEMP(L'SIZMASK),DTEMP+4
         OI    CTEMP+7,X'F0'
         MVC   JSIZE(6,R12),CTEMP+2         COPY TO OMA
         B     DONE                    WE'RE DONE, RETURN TO TASK MGR
         DROP  R7,R9
* 
* SEND VOLUME INFO
*
         USING IP$PUB,R7
SENDVOL  MVC   ZA#OMSG(VOLHDRL),VOLHDR MOVE VOLUMES HEADER TO OMA
         LA    R11,VOLHDRL             SET MSG LENGTH
         STH   R11,ZA#OTL
         MVI   ZA#PSIND,ZA#PSNN        SET NORMAL TERMINATION
*         
         LA    R11,1                   CALC. BIT TO CHECK FOR ASGNMNT
         IC    R12,JOBNUM  
         SLL   R11,0(R12)
         STC   R11,JOBNUM
*         
         LA    R7,PUBBFR               COVER PUBS
         L     R11,PUBADDR             POINT TO 1ST PUB
SVLOOP   MVC   IP$PUB(IP$LNGTH),0(R11) COPY PUB TO OUR BFR
         CLI   IP$TYP,X'20'            IS IT FOR A DISK?
         BNE   SVNEXT                  NO, TRY NEXT
         LH    R12,IP$TRL              GET PTR TO PUB TRAILER
         S     R12,BASEADR             MAKE NEGATIVE OFFSET INTO SUPER
         MVC   IP$PUBT(IP$LENT),0(R12) COPY TO OUR BUFFER
         LA    R12,ZA#OMSG             GET CRNT OFFSET INTO OMA
         AH    R12,ZA#OTL
         AI    ZA#OTL,8                BUMP MSG LENGTH
         MVI   0(R12),C' '             CLEAR 8 BYTES
         MVC   1(7,R12),0(R12)
         MVC   1(6,R12),IP$VSN         COPY VOL ID TO OMA
         IC    R10,JOBNUM              ASSIGNED TO THIS JOB?
         EX    R10,SVTM
         BZ    SVNEXT                  NO
         MVI   0(R12),C'*'             YES, PREFIX VSN WITH '*'
SVNEXT   LA    R11,IP$LNGTH(R11)       BUMP TO NEXT PUB
         AI    NUMPUBS,-1              DECR. PUB COUNT
         BP    SVLOOP                  > 0, TRY AGAIN
* 
         LA    R12,ZA#OMSG             GET CRNT OFFSET INTO OMA
         AH    R12,ZA#OTL
         MVC   0(VOLTLRL,R12),VOLTLR   COPY MSG TRAILER TO OMA
         AI    ZA#OTL,VOLTLRL          BUMP MSG LENGTH                           
         B     DONE                  
*                
DONE     LM    R14,R12,12(R13)         RESTORE REGISTERS
         BR    R14                     RETURN TO MONITOR
*
SVTM     TM    IP$ALC,0         
*
* JOBS INFO HEADER
*
JOBHDR   ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'27'                 ERASE PROTECTED
         DC    CL1'M'
         DC    XL1'1E'                 SOE
         DC    C'     JOB NAME   SIZE     TIME STEP EXEC     JOB NO'
JOBHDRL  EQU   *-JOBHDR
*
JOBDTL   ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'1E'                 SOE
         DC    C'     '
JNAME    EQU   *-JOBDTL         
         DC    C'XXXXXXXX '
JSIZE    EQU   *-JOBDTL         
         DC    C'XXXXXX '
JTIME    EQU   *-JOBDTL         
         DC    C'XXXXXX.X  '
JSTEP    EQU   *-JOBDTL         
         DC    C'XXX '
JEXEC    EQU   *-JOBDTL         
         DC    C'XXXXXXXX  '
JNUM     EQU   *-JOBDTL         
         DC    C'XXXXX'
JOBDTLL  EQU   *-JOBDTL         
*
JOBFRE   ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'1E'                 SOE
         DC    C'UNUSED MEMORY '
JFREE    EQU   *-JOBFRE         
         DC    C'XXXXXX'
         ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'1E'                 SOE
JOBFREL  EQU   *-JOBFRE         
* 
VOLHDR   ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'27'                 ERASE PROTECTED
         DC    CL1'M'
         DC    XL1'1E'                 SOE
VOLHDRL  EQU   *-VOLHDR
*         
VOLTLR   ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'1E'                 SOE
VOLTLRL  EQU   *-VOLTLR
*         
BADOPT   ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'27'                 ERASE PROTECTED
         DC    CL1'M'
         DC    XL1'1E'                 SOE
         DC    C'INVALID OPTION. VALID OPTIONS ARE: JOBS, VOLUMES'
         ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'1E'                 SOE
BADOPTL  EQU   *-BADOPT
*
SIZMASK  DC    X'4020202020202120'
STEPMASK DC    X'40202120'
JIDMASK  DC    X'402020202120'
JTIMASK  DC    X'402020202021204B20'
*         
         ZM#DOMH
ZA#OMSG  DS    CL2048                  OUTPUT MSG BFR.         
*
WORKAREA DSECT
DTEMP    DS    D                       DOUBLE WORD ALIGNED TEMP VAR.
BASEADR  DS    A                       BASE ADDR. OF BEMSIM JOB 
SWADDR   DS    A                       ADDR. OF SWITCH LIST
PUBADDR  DS    A                       ADDR OF PUB
NUMPUBS  DS    H                       # OF PUBS
NUMPRI   DS    H                       # OF USER PRIORITIES
FSTTCB   DS    A                       ADDR OF 1ST TCB IN CHAIN
MEMLOW   DS    A                       ADDR OF 1ST MEMORY BLOCK
MEMHI    DS    A                       ADDR OF LAST MEMORY BLOCK
DTYPE    DS    CL8                     DISPLAY TYPE (JOBS/VOLUMES)
CTEMP    DS    CL10
JOBNUM   DS    XL1
*
PUBNUM   DS    H                       CURRENT PUB #
         DS    0D
PREBFR   DS    XL(JP$LNGTH)            JOB PREAMBLE BUFFER
         DS    0D
SIBBFR   DS    XL(SB$LNGTH)            SYS. INFO. BLOCK BUFFER
         DS    0D
PUBBFR   DS    XL(IP$LNGTH)            PUB BUFFER
PTRLBFR  DS    XL(IP$LENT)             PUB TRAILER BFR
*
TCBBFR   DS    XL(JT$LNGTH)
*
BEMDSP   CSECT
*
         END
// FIN