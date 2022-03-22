         TITLE 'MAIN PROGRAM FOR BOYD''S BEM SIMULATOR'
***********************************************************************
*                                                                     *
*                         A BEM SIMULATOR.                            *
*                                                                     *
* SINCE BEM RELIED ON UNDOCUMENTED SUPERVISOR CALLS TO IMPLEMENT SOME *
* OF ITS FEATURES THIS SIMULATOR WILL BE LACKING THINGS LIKE THE      *
* ABILITY TO START JOBS, ETC.                                         *
*                                                                     *
***********************************************************************
*
BEMSIM   START
         PRINT NOGEN
         SUPEQU REGS=YES,                                              X
               TCB=NO,                                                 X
               PRE=YES,                                                X
               SIB=NO,                                                 X
               IO=NO,                                                  X
               TRN=NO
         TQ#X                          ALL ICAM PACKETS
         DTFDM
         ZM#DPIB
         ZM#DIMH
ZA#IMSG  DS    CL2032                  INPUT MSG BFR.         
         ZM#DOMH
ZA#OMSG  DS    CL2032                  OUTPUT MSG BFR. 
BEMSIM   CSECT         
         BEMDSCTS
         EJECT
         EXTRN L$OPEN,L$CLOSE,L$DFIRST,L$DNEXT,L$DFIND,L$EHDR
         EXTRN L$EFIRST,L$ENEXT,P$OPEN,P$BRKPT,P$CLOSE,P$PUT
         EXTRN L$DELETE,BS$INIT,BS$CLOSE
***********************************************************************
*                                                                     *
*                            LOW MEMORY                               *
*                                                                     *
* COMMONLY USED VARIABLES ARE STORED HERE AT A WELL KNOWN ADDRESS     *
* (ZERO). THAT MEANS THAT THIS OBJECT MODULE MUST BE THE FIRST ONE    *
* INCLUDED IN LNKEDT.                                                 *
*                                                                     *
***********************************************************************
         DC    8XL1'00'                LOW MEMORY TEST PATTERN
         DC    8XL1'FF'                IF THIS CHANGES WE GOT PROBLEMS
         DC    F'0'                    TTL FREE MEMORY AT BEM START
         DC    A(0)                    START OF FREE MEMORY
         DC    A(0)                    END OF FREE MEMORY
         DC    H'0'                    # OF TERMINALS
         DC    H'0'                    UNUSED
         DC    A(0)                    FIRST TERM. TABLE
         DC    A(0)                    LAST TERM. TABLE 
         DC    A(0)                    LAST TERM. SERVICED
         DC    A(PRGMS)                FIRST PGM. TABLE
         DC    A(PRGMSL)               LAST PGM. TABLE 
         DC    F'0'                    MAX. PGM SIZE
         DC    A(0)                    FIRST USER TABLE
         DC    A(0)                    LAST USER TABLE
         DC    A(ALIASES)              FIRST ALIAS TABLE
         DC    A(ALIASESL)             LAST ALIAS TABLE
         DC    A(MAIN+2)               MAIN PGM COVER
         DC    A(CLRMEM)               CLEAR MEMORY ENTRY PT.
         DC    A(CPYMEM)               COPY MEMORY ENTRY PT.
         DC    A(GETOKEN)              GET TOKEN ENTRY PT.
         DC    A(FNDTNAM)              FIND TERM. NAME ENTRY PT.
         DC    A(0)                    MCT INPUT BUFFER 
         DC    A(0)                    MCT OUTPUT BUFFER 
         DC    A(L$OPEN)               LIBRARY OPEN ENTRY PT.
         DC    A(L$CLOSE)              LIBRARY CLOSE ENTRY PT.
         DC    A(L$DFIRST)             LIBRARY DIR 1ST ENTRY PT.
         DC    A(L$DNEXT)              LIBRARY DIR NEXT ENTRY PT.
         DC    A(L$DFIND)              LIBRARY FIND ELEMENT
         DC    A(L$EHDR)               LIBRARY GET MODULE HDR ENTRY PT.
         DC    A(L$EFIRST)             LIBRARY GET ELE 1ST LINE
         DC    A(L$ENEXT)              LIBRARY GET ELE NEXT LINE
         DC    A(P$OPEN)               OPEN A PRINT FILE
         DC    A(P$BRKPT)              CLOSE A PRINT FILE
         DC    A(P$PUT)                WRITE A PRINT LINE
         DC    A(L$DELETE)             LIBRARY DELETE AN ELEMENT
         DS    18F                     REGISTER SAVE AREA
         EJECT
***********************************************************************
*                                                                     *
*                         MAIN ENTRY POINT                            *
*                                                                     *
* IT IS ASSUMED THAT THE FOLLOWING REGISTER WILL ALWAYS BE INTACT.    *
* MESS WITH IT AT YOUR OWN RISK.                                      *
*                                                                     *
* R2  = COVER REGISTER                                                *
* R1  = MCTI COVER                                                    *
* R11 = VOLATILE WORK REGISTER                                        *
* R12 = VOLATILE WORK REGISTER                                        *
* R13 = REGISTER SAVE AREA                                            *
*                                                                     *
***********************************************************************
         DS    0H
MAIN     BALR  R2,0                    INIT COVER
         USING *,R2  
         USING LOWMEM,R0
         USING TN#MCTDS,R1
         LA    R13,SAVAREA             POINT TO REGISTER SAVE AREA       
*         
         BAL   R14,INIT                PERFORM PROGRAM INTIALIZATION
*
* ATTACH TO OPCOM ISLAND CODE ROUTINE
*
         STXIT OC,OPCOM,OCSAVE,OCMSG,L'OCMSG
*
* START RECEIVING MESSAGES         
*
MRECV    LA    R1,MCTI                 COVER INPUT MCT PACKET
         L     R11,LM$IBFR             POINT TO INPUT BFR
         LA    R11,16(R11)             BUMP PAST IMA
         ST    R11,TN#MCBFA
         LA    R11,2048                SET BFR LENGTH
         STH   R11,TN#MCBAL
         XC    TN#MCCHC,TN#MCCHC       CLEAR INPUT CHAR COUNT
         XC    TN#MCCKB,TN#MCCKB       CLEAR BFR CNTRL BYTE
         LA    R11,1                   SET LINE & TERM IDS TO 1
         STH   R11,TN#MCLID
         STH   R11,TN#MCTIN
         MCPCALL (1)                   START BALL ROLLING         
*
MYIELD   CYIELD         
*
* IF WE WAKE UP HERE IT MEANS THAT WE NEED TO DO SOMETHING FOR THE
* WORKER TASK. RIGHT NOW, THE ONLY SOMETHING WE MIGHT NEED TO DO
* IS SEND PENDING OUTPUT OR PERFORM SHUTDOWN PROCESSING.
*
         CLI   EOJFLG,X'FF'            EOF REQUESTED?
         BE    FIN                     YES
         MCPCALL MCTO
         B     MYIELD
*
FIN      BAL   R14,TERMN8              PERFORM PROGRAM SHUTDOWN
         EOJ         
************
*
* MCT OUTPUT COMPLETION HANDLER
*
*   R3  = MCT PACKET COVER
*
************
         USING TN#MCTDS,R3
         USING TERMTBL,R1
OCMPL    L     R2,LM$MAIN              RE-ESTABLISH COVER
         LR    R3,R1                   COVER MCT PACKET
         L     R1,TN#MCLID             GET LINE / TERM NUMBERS
         LA    R15,FNDTNUM             FIND OUR TERM TABLE
         BALR  R14,R15
         LTR   R1,R1                   TABLE FOUND?
         BZ    OCDONE                  NO, SKIP THE REST
         TM    TT$FLAGS,TT$WAITO       CONTINUOUS OUTPUT IN PROGRESS?
         BZ    OCDONE                  NO
         MVI   TT$DLVCD,0              SHOW SUCCESSFUL OUTPUT DELIVERY
         OI    TT$FLAGS,TT$ODELV       SET OUTPUT DELV. NOTIFICATION
         NI    TT$FLAGS,X'FF'--TT$WAIT CLEAR TERM WAITING FLAGS
         AI    M$ICNT,1                BUMP PENDING INPUT COUNT
         AWAKE TM$ECB                  WAIT UP TASK MANAGER
*
OCDONE   MVI   M$OBUSY,0               CLEAR OUTPUT BUSY FLAG
         B     MYIELD
         DROP  R1,R3
************
*
* MCT INPUT COMPLETION HANDLER
*
*   R3  = MCT PACKET COVER
*   R4  = SAT PCA COVER
*   R5  = I/O BUFFER (IMA) COVER
*   R6  = TERM TABLE COVER
*
************
         USING TN#MCTDS,R3
         USING PCA,R4
         USING ZA#IMH,R5
         USING TERMTBL,R6
ICMPL    L     R2,LM$MAIN              RE-ESTABLISH COVER
         LA    R13,SAVAREA             POINT TO REGISTER SAVE AREA       
         LR    R3,R1                   COVER MCT PACKET
*
* CHECK FOR ERRORS
*
         CLI   TN#MCST1,0              STATUS = 0?
         BE    ISTOK                   YES, OK???
         TM    TN#MCST1,X'80'          BUFFER COMPLETION STATUS?
         BO    ISTOK                   YES, OK
         OPR   ICERR,L'ICERR           TELL OPR OF ERROR
         XR    R0,R0                   GET STATUS CODE TO R0
         IC    R0,TN#MCST1             
         CANCEL (0)                    TAKE A DUMP
*                  
ISTOK    L     R1,TN#MCLID             GET LINE / TERM NUMBERS
         LA    R15,FNDTNUM             FIND OUR TERM TABLE
         BALR  R14,R15
         LTR   R6,R1                   TABLE FOUND?
         BZ    ICDONE                  NO, SKIP THE REST
         TM    TT$FLAGS,TT$IPEND       ALREADY INPUT PENDING?
         BO    ICDONE                  YES, DROP THIS MESSAGE
*
* WRITE INPUT MSG TO DISC BUFFER FOR TERMINAL
*
IFLWAIT  TS    SCR1LOCK                ACQUIRE FILE LOCK
         BZ    IFLOK                   SUCCESS
         SETIME 10,WAIT,M              FAIL, WAIT A BIT
         B     IFLWAIT                 & LOOP
*         
IFLOK    LA    R4,SCR1P1               COVER I/O BUFFER PARTITION
         L     R5,LM$IBFR              COVER INPUT BFR (IMA)
         MVI   0(R5),0                 CLEAR IMA PORTION OF INPUT BFR
         MVC   1(15,R5),0(R5)
         MVC   ZA#ISTID,TT$NAME        COPY TERM NAME TO IMA
         LH    R11,TN#MCCHC            COPY MSG LENGTH TO IMA                  
         STH   R11,ZA#ITL
         ST    R5,PC$A1F               SET PCA I/O BUFFER ADDRESS
         MVI   PC$IOCNT,1              SET TO WRITE 1 BLOCK
         LH    R11,TT$TNUM             GET TERM BFR BLOCK #
         ST    R11,PC$PCAID
         PUT   $SCR1,SCR1P1
         MVI   SCR1LOCK,0              RELEASE FILE LOCK
*                
         NI    TT$FLAGS,X'FF'--TT$WAIT CLEAR TERM WAITING FLAGS
         OI    TT$FLAGS,TT$IPEND       SET INPUT PENDING FLAG
         AI    M$ICNT,1                BUMP PENDING INPUT COUNT
         AWAKE TM$ECB                  WAKE UP THE WORKER TASK
*         
ICDONE   B     MRECV 
         DROP  R3,R4,R5,R6        
************
*
* PROGRAM INITIALIZATION
*
*   R2  = COVER
*   R3  = TERM. TABLE
*   R12 = JOB PREAMBLE COVER
*   R14 = RETURN ADDR.
*
************
         USING JP$PRE,R12
         USING TERMTBL,R3
INIT     ST    R14,SAVR14              SAVE RETURN ADDR.
*
* DETERMINE START AND END OF FREE MEMORY REGION
*
         GETINF PRE,PREBFR,JP$LNGTH,0         
         L     R12,=A(PREBFR)          COVER PREAMBLE INFO
         L     R11,JP$JSE              SAVE END OF PGM ADDR
         DALGN R11                     BUMP TO NEXT DBL WORD
         ST    R11,LM$FSTRT            AS FREE MEM START
         L     R11,JP$JOB              SAVE END OF MEMORY ADDR
         ST    R11,LM$FEND             AS FREE MEM END
         S     R11,LM$FSTRT            CALC. TTL FREE
         ST    R11,LM$FTTL
*
         OPEN  $SCR1                   OPEN THE BFR/CDA FILE 
         L     R15,=A(BS$INIT)         OPEN & INIT THE SCRATCH FILE
         BALR  R14,R15
*
* INITIALIZE TASK MANAGER BY ALLOCATION ACTIVIATION RECORD BUFFERS
* AND STARTING WORKER SUB TASK.
*
         LA    R1,2048+16              2048+16 BYTE IMA
         BAL   R8,GETMEM
         ST    R1,TM$IMA                
         LA    R1,2048+16              2048+16 BYTE OMA
         BAL   R8,GETMEM
         ST    R1,TM$OMA                
         LH    R1,=H'4096'             4096 BYTE WORK AREA
         BAL   R8,GETMEM
         ST    R1,TM$WA                
         LH    R1,=H'4096'             4096 BYTE CDA
         BAL   R8,GETMEM
         ST    R1,TM$CDA
         LA    R1,ZA#PLEN              PROG INFO BLOCK
         BAL   R8,GETMEM
         ST    R1,TM$PIB
* ALLOCATE ICAM BUFFERS
         LA    R1,2048+16              2048+16 BYTE MCT INPUT BFR
         BAL   R8,GETMEM
         ST    R1,LM$IBFR
         LA    R1,2048                 2048 BYTE MCT OUTPUT BFR
         BAL   R8,GETMEM
         ST    R1,LM$OBFR
*         
         ATTACH TM$ECB,TASKMGR         START THE SUB TASK
*
         BAL   R14,CONFIG              READ CONFIG FROM CTL STREAM
*
         BAL   R14,PGMINFO             GET THE SIZE OF ALL PRGMS.
         L     R11,LM$FEND             CALC. SIZE OF REMAINING FREE MEM         
         S     R11,LM$FSTRT
         C     R11,LM$PMAX             ENOUGH TO LOAD LARGEST PGM?
         BNL   IMEMOK                  YES
         OPR   INSUFMEM,L'INSUFMEM     NO, TELL OPERATOR
         CANCEL 102                    ABORT         
* 
* SET UP BEMRDY TO RUN ON ALL TERMINALS
*        
IMEMOK   L     R3,LM$TFRST             SET UP 1ST TERM TO RUN
BRLOOP   L     R11,=A(BEMRDY)          BEMRDY PRGM.
         ST    R11,TT$NXTPG
         MVI   TT$FLAGS,0
         L     R3,TT$NEXT              GET PTR TO NEXT TERM
         LTR   R3,R3                   = ZERO?
         BNZ   BRLOOP                  NO, LOOP         
         AWAKE TM$ECB                  WAKE UP THE TASK MGR.
         
         L     R14,SAVR14              RESTORE RETURN ADDR.
         BR    R14                     RETURN
         DROP  R12
************
*
* TERMINATION PROCESSING
*   R2  = COVER
*   R14 = RETURN ADDR.
*
************
TERMN8   ST    R14,SAVR14              SAVE RETURN ADDR.
         STXIT OC                      DETACH OPCOM ISLAND CODE
         DETACH TM$ECB,TERMN81         STOP THE SUB TASK
TERMN81  CLOSE $SCR1                   CLOSE THE BFR/CDA FILE
         L     R15,=A(BS$CLOSE)        CLOSE THE SCRATCH FILE
         BALR  R14,R15
         L     R15,=A(P$CLOSE)         CLOSE THE PRINTER FILE
         BALR  R14,R15
NREL     NETREL EDT                    CLOSE THE NETWORK
*
         L     R14,SAVR14              RESTORE RETURN ADDR.
         BR    R14         
************
*
* READ CONFIGURATION PARAMETERS FROM THE CONTROL STREAM. EACH PARAMETER
* OCCUPIES 1 RECORD.
*
*   R2  = COVER
*   R14 = RETURN ADDR.
*
* NETWORK-NAME
* TERM-NAME1
* TERM-NAME2
* ...
* TERM-NAMEN 
* USER ID1,ACCT1,PWD1
* USER ID2,ACCT2,PWD2
* ...
* USER IDN,ACCTN,PWDN
*
* WHEN THE NETWORK ID IS READ, ATTEMPT TO OPEN THE NETWORK. AS EACH
* TERMINAL ID IS READ, VALIDATE IT AND CREATE A TERMINAL TABLE ENTRY
* USING INFO FROM THE CCA TABLES.        
*
************
         USING TERMTBL,R3
         USING TC#CCOTP,R4
         USING USERTBL,R5
CONFIG   ST    R14,CSAVR14             SAVE RETURN ADDRESS
CONFIG1  GETCS CSBFR                   GET A CONTROL STREAM RECORD
         LTR   R0,R0                   END OF STREAM?
         BZ    CEOS                    YES
         CLI   CSBFR,C'/'              SKIP /$, /* AND // PARAM
         BE    CONFIG1
         CLI   CSBFR,C'*'              SKIP COMMENTS
         BE    CONFIG1
CSWTCH   NOP   CTERM                   DO TERMS AFTER 1ST RECORD
*
* PROCESS NETWORK ID RECORD
*
         MVC   NREQ+12(4),CSBFR        MOVE NET NAME TO NETREQ PACKET
         MVC   NREL+12(4),CSBFR        MOVE NET NAME TO NETREL PACKET
         MVI   CSWTCH+1,X'F0'          MAKE NOP A BRANCH
*
* OPEN THE NETWORK
*
         MVI   M$FUNC,M$NETREQ         SET CRNT FUNCTION CODE          
NREQ     NETREQ EDT,ERRET=NETERR
         CH    R0,LM$ZERO              ALL OK?
         BNE   NETERR                  NO, OOPS!         
         B     CONFIG1                 LOOP
*
* LOOK UP INFO FOR EACH TERMINAL AND BUILD OUR TERMINAL TABLE
*         
CTERM    CLC   CSBFR(5),=C'USER '      USER DEFINITION?
         BE    CUSER                   YES
         MVC   TNAME,CSBFR             COPY TERM. NAME TO CCACPY PKT
         LH    R11,=H'-1'              SET END OF LIST FLAG
         ST    R11,TNAME+4
         MVI   M$FUNC,M$CCACPY         SET CRNT FUNCTION CODE
         CCACPY TNAME,TNAMEL,                                          X
               CCAINFO,CCAINFOL        GET TERM. INFO
         AI    LM$TCNT,1               BUMP # OF TERMINALS                
         LA    R1,TT$LNGTH             ALLOC. MEM FOR TERM. TABLE
         BAL   R8,GETMEM 
         L     R3,LM$TFRST             IS THIS THE FIRST ENTRY?
         LTR   R3,R3
         BNZ   CTTERM1                 NO
         ST    R1,LM$TFRST             YES, SAVE CRNT AS FIRST
CTTERM1  L     R3,LM$TLAST             GET PTR TO LAST TERM TBL
         LTR   R3,R3                   IS THIS THE FIRST ENTRY?
         BZ    CTTERM2                 YES
         ST    R1,TT$NEXT              NO, SAVE LINK TO NEXT ENTRY         
CTTERM2  ST    R1,LM$TLAST             SAVE CRNT AS LAST TERM TBL.
         LR    R3,R1                   COVER CRNT TERM TBL.
         L     R4,=A(CCAINFO)          COVER CCAINFO PKT.
         MVC   TT$NAME,TC#CTRM         SAVE TERMINAL NAME
         MVC   TT$LINE,TC#CLLN              LINE #         
         MVC   TT$ID,TC#CTIDX               TERM #
         MVC   TT$TNUM,LM$TCNT              BFR FILE REC #
         MVI   TT$FLAGS,TT$WAITI            FLAG BYTE (WAIT FOR INPUT)
*
* CREATE A CDA RECORD FOR THIS TERM
*
         USING PCA,R11 
         L     R11,=A(SCR1P2)          COVER PCA FOR CDA PARTITION
         XC    PC$PCAID,PC$PCAID       SET REC # TO TT$TNUM
         MVC   PC$PCAID+2,TT$TNUM
         MVC   PC$A1F,TM$CDA           SET I/O BFR TO CDA
         MVI   PC$IOCNT,1              SET 1 RECORD TO WRITE
         PUT   $SCR1,SCR1P2            WRITE THE CDA RECORD
         B     CONFIG1                 & LOOP
         DROP  R11
*
* SAVE A USER DEFINITION
*
CUSER    LA    R0,L'CSBFR              INIT. GETOKEN PARAMS
         LA    R1,CSBFR
         L     R15,LM$GTKN
         LA    R11,USERID              BURN 'USER'
         BALR  R14,R15
         LA    R11,USERID              GET USER ID
         BALR  R14,R15
         LA    R11,ACCTID              GET ACCOUNT ID
         BALR  R14,R15
         LA    R11,PASSWD              GET PASSWORD
         BALR  R14,R15
         LA    R1,UT$LNGTH             ALLOC MEM FOR USER TABLE
         BAL   R8,GETMEM
         L     R5,LM$UFRST             IS THIS THE FIRST ENTRY?
         LTR   R5,R5
         BNZ   CUSER1                  NO
         ST    R1,LM$UFRST             YES, SAVE CRNT AS FIRST
CUSER1   L     R5,LM$ULAST             GET PTR TO LAST USER TBL
         LTR   R5,R5                   IS THIS THE FIRST ENTRY?
         BZ    CUSER2                  YES
         ST    R1,UT$NEXT              NO, SAVE LINK TO NEXT ENTRY         
CUSER2   ST    R1,LM$ULAST             SAVE CRNT AS LAST TERM TBL.
         LR    R5,R1                   COVER CRNT USER TABLE
         MVC   UT$ID,USERID            SAVE USER ID
         MVC   UT$ACCT,ACCTID               ACCOUNT ID
         MVC   UT$PWD,PASSWD                PASSWORD
         B     CONFIG1                 & LOOP
*
* CCACPY FAILED, SO ASSUME AN INVALID TERMINAL ID
*
CTERR    MVC   BADTRM(4),TNAME         TERM ID TO ERR MSG
         OPR   BADTRM,L'BADTRM
         B     CONFIG1                 TRY AGAIN
*
CEOS     L     R14,CSAVR14             RESTOR RETURN ADDRESS
         BR    R14                     RETURN
CSAVR14  DS    F
         DROP  R3,R4,R5
************
*
* GET SIZE OF ALL CONFIGURED PROGRAMS
*
*   R3  = PROGRAM TABLE COVER
*   R4  = PHASE HEADER COVER
*   R14 = RETURN ADDRESS
*
************
         USING PGMTBL,R3
         USING PHASEHDR,R4
PGMINFO  LA    R4,PINFO                POINT TO PHASE HDR BFR
         L     R3,LM$PFRST             GET 1ST PGM TBL ENTRY
PILOOP   LTR   R3,R3                   END OF TBL?
         BZ    PIDONE                  YES         
         LA    R1,PT$MNAME             POINT TO MODULE NAME
         LOADI (1),PINFO,L'PINFO,PIBADPGM
         MVC   PT$SIZE,PH$PLNTH        SAVE THE PHASE LENGTH
         L     R11,PT$SIZE             IS SIZE > THAN MAX SO FAR?
         C     R11,LM$PMAX             
         BNH   PINEXT                  NO
         ST    R11,LM$PMAX             YES, SAVE IT
PINEXT   L     R3,PT$NEXT              POINT TO NEXT ENTRY
         B     PILOOP                  & LOOP         

PIDONE   BR    R14
*         
PIBADPGM MVC   BADPGM(8),PT$MNAME      PGM NAME TO MSG
         OPR   BADPGM,L'BADPGM         TELL OPERATOR
         B     PINEXT                  DO NEXT PGM
         DROP  R3,R4
************
*
* ALLOCATE A BLOCK OF MEMORY & CLEAR IT TO ZEROS
*
*   R1 = BLOCK SIZE ON ENTRY
*   R1 = PTR. TO BLOCK ON EXIT
*   R8 = RETURN ADDR
*
************
GETMEM   ST    R8,GMSAVE               SAVE RETURN ADDR.
         DALGN R1                      BUMP SIZE TO NEXT DBL WORD
         L     R11,LM$FSTRT            GET PTR. TO BLOCK START
         LR    R12,R11                 SAVE IT FOR A BIT
         LA    R11,0(R1,R11)           BUMP FREE MEMORY START ADDRESS
         ST    R11,LM$FSTRT            SAVE IT
         C     R11,LM$FEND             PAST END OF MEMORY?
         BH    GMOOM                   YES, ABORT WITH OUT OF MEM ERROR
         LR    R0,R1                   CLEAR BLOCK TO ZERO
         LR    R1,R12
         L     R15,LM$CLRM
         BALR  R8,R15
         LR    R1,R12                  RETURN BLOCK PTR TO CALLER
         L     R8,GMSAVE               RESTORE RETURN ADDR.
         BR    R8
*
GMOOM    OPR   NOMEM,L'NOMEM
         CANCEL 100
*
GMSAVE   DS    F
************
*
* FIND A TERMINAL TABLE USING THE LINE AND TERMINAL NUMBERS
*   R1  = LINE / TERM # TO FIND ON ENTRY
*   R1  = TERM TABLE ADDRESS (ZERO IF NO FIND) ON EXIT
*   R11 = TERM TABLE
*   R14 = RETURN ADDRESS
*   R15 = ENTRY PT.
*
************
         USING *,R15
         USING TERMTBL,R11
FNDTNUM  L     R11,LM$TFRST            POINT TO 1ST TERM TABLE
FTLOOP   C     R1,TT$IDF               IS THIS THE ONE WE WANT?
         BE    FTOK                    YES     
         L     R11,TT$NEXT             GET NEXT TERM TABLE
         LTR   R11,R11                 END OF TABLE?
         BNZ   FTLOOP                  NO, TRY AGAIN
         XR    R11,R11                 SHOW NO FIND    
         
FTOK     LR    R1,R11                  RETURN TERM TABLE ADDRESS         
         BR    R14                     & RETURN
         DROP  R11,R15
************
*
* FIND A TERMINAL TABLE USING THE TERMINAL NAME
*   R1  = TERMINAL NAME TO FIND ON ENTRY
*   R1  = TERM TABLE ADDRESS (ZERO IF NO FIND) ON EXIT
*   R11 = TERM TABLE
*   R14 = RETURN ADDRESS
*   R15 = ENTRY PT.
*
************
         USING *,R15
         USING TERMTBL,R11
FNDTNAM  L     R11,LM$TFRST            POINT TO 1ST TERM TABLE
FNLOOP   C     R1,TT$NAME              IS THIS THE ONE WE WANT?
         BE    FNOK                    YES     
         L     R11,TT$NEXT             GET NEXT TERM TABLE
         LTR   R11,R11                 END OF TABLE?
         BNZ   FNLOOP                  NO, TRY AGAIN
         XR    R11,R11                 SHOW NO FIND    
         
FNOK     LR    R1,R11                  RETURN TERM TABLE ADDRESS         
         BR    R14                     & RETURN
         DROP  R11,R15
************
*
* NETWORK ERROR HANDLER
*   R0 = ERROR CODE
*
************
NETERR   L     R2,LM$MAIN              RE-ESTABISH COVER
         N     R0,=F'65535'            ISOLATE ERROR CODE
         CLI   M$FUNC,M$CCACPY         IS CRNT FUNC CCACPY?
         BE    CTERR                   YES, INVALID TERMINAL ID
         LR    R11,R0                  SAVE ERROR CODE
         XR    R1,R1                   GET NETWORK FUNC CODE
         IC    R1,M$FUNC
         SLL   R1,5                    MULT. BY 32
         A     R1,=A(NERR1ST)          ADD TO 1ST MSG ADDR.
         OPR   (1),32
         LR    R0,R11                  RESTORE ERROR CODE
         CANCEL (0)
************
*
* BUFFER / CDA FILE ERROR HANDLER
*
************
         USING DM$DSCT,R1
SCR1ERR  L     R2,LM$MAIN              RE-ESTABLISH COVER
         OPR   SCRIOERR,L'SCRIOERR     TELL OPERATOR
         LA    R1,$SCR1                COVER THE DTF
         XR    R0,R0                   GET THE ERROR CODE
         IC    R0,DC$ERCD
         CANCEL (0)
         DROP  R1
*
INSUFMEM DC    C'INSUFFICIENT MEMORY TO LOAD LARGEST PROGRAM' 
SCRIOERR DC    C'I/O ERROR ON WORK1'        
BADTRM   DC    C'     INVALID TERMINAL ID'
BADPGM   DC    C'         INVALID PROGRAM NAME'
NOMEM    DC    C'OUT OF MEMORY'
ICERR    DC    C'ERROR DETECTED DURING INPUT COMPLETION'
*
* NETWORK ERROR MESSAGES. THIS MUST BE KEPT IN THIS ORDER.
*
NQERR    DC    CL32'NETWORK REQUEST ERROR'
CCERR    DC    CL32'CCACPY ERROR'
NSERR    DC    CL32'SEND ERROR'
NRERR    DC    CL32'RECEIVE ERROR'
NERR1ST  EQU   NQERR
*
EOJFLG   DC    XL1'00'                 END OF JOB FLAG
SAVR14   DS    F                       RETURN ADDR SAVE AREA
SAVAREA  DS    18F                     REGISTER SAVE AREA
USERID   DS    CL4                     CONFIG USER ID
ACCTID   DS    CL4                     CONFIG ACCOUNT ID
PASSWD   DS    CL4                     CONFIG PASSWORD
************         
*
* THE FOLLOWING DEFINES A BUNCH OF TEMPORARY BUFFERS THAT ARE USED
* AND RE-USED DURING INTIALIZATION. DO NOT DEFINE ANY CONSTANTS HERE
* AS THEY WILL LIKELY BE SMASHED.
* 
************        
         DS    0D
PREBFR   DS    XL(JP$LNGTH)            JOB PREAMBLE BUFFER
*
         ORG   PREBFR                  RE-USE THIS BFR AFTER INIT.
CSBFR    DS    CL80 
TNAME    DS    2F                      TERMINAL NAME PKT FOR CCACPY
TNAMEL   EQU   *-TNAME         
CCAINFO  DS    10F                     TERM INFO RETURNED HERE.               
CCAINFOL EQU   *-CCAINFO
*
         ORG   PREBFR                  RE-USE THIS BFR AFTER INIT.
PINFO    DS    XL70                    PGM PHASE HDR INFO
         ORG   PREBFR+JP$LNGTH
* 
         LTORG
*                 
         EJECT                                      
************
*
* TASK MANAGER
*   R2  = COVER
*   R3  = TERM. TABLE
*   R4  = PRGM. TABLE
*   R5  = PIB
*   R6  = DISK BUFFER FILE PCA
*
************
         USING TERMTBL,R3
         USING PGMTBL,R4
         USING ZA#DPIB,R5
         USING PCA,R6
*
TASKMGR  BALR  R2,0                    ESTABLISH NEW COVER
         USING *,R2
*         
         LA    R13,TM$SAVE             POINT TO SAVE AREA
TMYIELD  CLI   M$ICNT,0                ANY PENDING INPUT?
         BH    TM0                     YES, DON'T YIELD YET
         TYIELD                        WAIT FOR SOMETHING TO HAPPEN

*
* WHEN WE WAKE UP HERE WE SCAN THE TERMINAL TABLE LOOKING FOR
* SOMETHING DO DO. WE START THE SEARCH WITH THE TERMINAL AFTER THE LAST
* TERMINAL SERVICED. THE FIRST TERMINAL THAT WE FIND WITH NO WAIT FLAGS
* SET GETS SCHEDULED AND ITS ENTRY IS SAVED AS THE LAST SERVICED.
*
TM0      L     R3,LM$TLSVC             GET LAST TERM SERVICED
         LTR   R3,R3                   = ZERO?
         BZ    TM1                     YES
         L     R3,TT$NEXT              NO, GET NEXT TERM.
         LTR   R3,R3                   = ZERO?
         BZ    TM1                     YES
         B     TM2         
TM1      L     R3,LM$TFRST             START WITH FIRST TERM.
TM2      ST    R3,TM$SAVE              SAVE STARTING POINT
*
TMSCAN   TM    TT$FLAGS,TT$WAIT        ANY WAITING FLAGS SET?
         BZ    TMINIT                  NO, INIT. ACTIVATION RECORD
TMNEXT   L     R3,TT$NEXT              YES, GET NEXT TERM. TBL ENTRY
         LTR   R3,R3                   = ZERO?
         BNZ   TM3                     NO
         L     R3,LM$TFRST             YES, WRAP TO FIRST
TM3      C     R3,TM$SAVE              EQUAL TO STARTING POINT?
         BE    TMYIELD                 YES, WE'RE DONE
         B     TMSCAN                  NO, LOOP
*         
TMINIT   ST    R3,LM$TLSVC             SAVE CRNT AS LAST SERVICED
*         
* INITIALIZE ACTIVIATION RECORD         
*
         L     R15,LM$CLRM             GET CLRMEM ENTRY PT.
         LA    R0,ZA#PLEN              INIT PIB
         L     R1,TM$PIB               
         BALR  R8,R15
         L     R5,TM$PIB               COVER THE PIB              
         LA    R11,24                  STD MSG LINES
         STH   R11,ZA#PMNL
         LA    R11,80                  STD LINE LENGTH
         STH   R11,ZA#PMLL
         LH    R11,=H'4096'            WORK AREA SIZE
         STH   R11,ZA#PWA
         STH   R11,ZA#PCDIN            CDA INPUT LENGTH
         STH   R11,ZA#PCDO             CDA OUTPUT LENGTH
*         
         LA    R0,2064                 INIT IMA
         L     R1,TM$IMA
         BALR  R8,R15
         L     R1,TM$IMA
         USING ZA#IMH,R1
         MVC   ZA#ISTID,TT$NAME
*         
         LA    R0,2064                 INIT OMA
         L     R1,TM$OMA
         BALR  R8,R15
         L     R1,TM$OMA
         USING ZA#OMH,R1
         MVC   ZA#ODTID,TT$NAME
*         
         LH    R0,=H'4096'             WORK AREA
         L     R1,TM$WA
         BALR  R8,R15
*         
         LH    R0,=H'4096'             CDA
         L     R1,TM$CDA
         BALR  R8,R15
         DROP  R1
*
* READ CDA FROM DISK IF THERE IS A SUCCESSOR PROGRAM
*
         CLC   TT$NXTPG,LM$ZERO        NEXT PGM ADDR = ZERO?
         BE    TMNOCDA                 YES, NO NEED TO LOAD CDA
TMLWAIT2 TS    SCR1LOCK                ACQUIRE FILE LOCK
         BZ    TMLOK2                  SUCCESS
         SETIME 10,WAIT,M              FAIL, WAIT A BIT
         B     TMLWAIT2                & LOOP
*  
TMLOK2   LA    R6,SCR1P2               COVER CDA PARTITION
         L     R11,TM$CDA              POINT TO IMA
         ST    R11,PC$A1F              SET PCA I/O BUFFER ADDRESS
         MVI   PC$IOCNT,1              SET TO WRITE 1 BLOCK
         LH    R11,TT$TNUM             GET TERM BFR BLOCK #
         ST    R11,PC$PCAID
         GET   $SCR1,SCR1P2
         MVI   SCR1LOCK,0              RELEASE FILE LOCK
*
* IF CONTINUOUS OUTPUT ACTIVE, COPY CONT. OTPT. CODE TO IMA
*
         USING ZA#IMH,R11
         L     R11,TM$IMA              COVER IMA
         TM    TT$FLAGS,TT$ODELV       OTPT DELIVERY NOTIFY SET?
         BZ    TMNOCDA                 NO
         MVC   ZA#ITL+4(4),TT$CONT     COPY CONT OTPT CODE TO IMA
         MVC   ZA#ITL+5(1),TT$DLVCD    COPY DELV. NOTIFY CODE TO IMA
         NI    TT$FLAGS,X'FF'--TT$ODELV CLEAR OTPT DELIVERY NOTIFY FLAG
         AI    M$ICNT,-1               DECR. PENDING INPUT COUNT
         DROP  R11
*
* READ ANY PENDING INPUT FROM DISK BUFFER
*
TMNOCDA  TM    TT$FLAGS,TT$IPEND       INPUT PENDING?         
         BZ    TMEXEC                  NO, GO EXECUTE THE PROGRAM
TMLWAIT  TS    SCR1LOCK                YES, ACQUIRE FILE LOCK
         BZ    TMLOK                   SUCCESS
         SETIME 10,WAIT,M              FAIL, WAIT A BIT
         B     TMLWAIT                 & LOOP
*  
TMLOK    LA    R6,SCR1P1               COVER I/O BUFFER PARTITION
         L     R11,TM$IMA              POINT TO IMA
         ST    R11,PC$A1F              SET PCA I/O BUFFER ADDRESS
         MVI   PC$IOCNT,1              SET TO WRITE 1 BLOCK
         LH    R11,TT$TNUM             GET TERM BFR BLOCK #
         ST    R11,PC$PCAID
         GET   $SCR1,SCR1P1
         MVI   SCR1LOCK,0              RELEASE FILE LOCK
         NI    TT$FLAGS,X'FF'--TT$IPEND CLEAR INPUT PENDING FLAG
         AI    M$ICNT,-1               DECR. PENDING INPUT COUNT
*
* GET FIRST TOKEN FROM INPUT
*
         USING ZA#IMH,R1       
         L     R1,TM$IMA               COVER IMA
         LH    R0,ZA#ITL               GET MSG. LENGTH
         LA    R1,ZA#IMSG              POINT TO INPUT MSG.
         LA    R11,TM$TOKEN            POINT TO TOKEN BFR
         L     R15,LM$GTKN             GO GET THE FIRST TOKEN
         BALR  R14,R15
         DROP  R1
*
* EXECUTE THE APPROPRIATE PROGRAM
*                  
TMEXEC   L     R4,TT$NXTPG             GET PTR TO PGM TO EXECUTE
         LTR   R4,R4                   = ZERO?
         BNZ   TMEXEC2                 NO, EXEC THIS PRGM.
*
* LOOK UP PRGM FOR COMMAND CODE GIVEN IN THE IMA
*   
         USING ALIASTBL,R11      
         L     R11,LM$AFRST            POINT TO 1ST ALIAS                      
TMFALIAS C     R11,LM$ALAST            END OF TABLE?
         BH    TMBADPGM                YES, COMMAND CODE NOT FOUND
         CLC   AT$ALIAS,TM$TOKEN       ALIAS = INPUT COMMAND?
         BE    TMEXEC1                 YES, SUCCESS
         LA    R11,AT$LNGTH(R11)       BUMP TO NEXT ALIAS
         B     TMFALIAS                & LOOP
TMEXEC1  L     R4,AT$PGM               GET PTR. TO PROGRAM TABLE
         DROP  R11                       
*         
* LOAD PROGRAM INTO FREE MEM IF NEEDED & EXECUTE IT       
*
TMEXEC2  TM    PT$FLAGS,PT$LOGON       LOGON REQUIRED?
         BZ    TMEXEC3                 NO, OK
         TM    TT$FLAGS,TT$LOGON       USER LOGGED ON?
         BZ    TMLOGON                 NO, SEND ERROR
TMEXEC3  ST    R4,TT$LSTPG             SAVE THIS PGM AS LAST EXECUTED
         C     R4,TM$RESPG             PRGM ALREADY IN MEMORY?
         BNE   TMLOAD                  NO
         L     R1,PT$NTRYA             YES, GET ENTRY PT. ADDR.
         B     TMSTART                 GO START PRGM.
TMLOAD   LA    R1,PT$MNAME             POINT TO PGM NAME
         L     R0,LM$FSTRT             POINT TO PGM LOAD AREA
         LOADR (1),(0),TMLERR          LOAD THE PGM INTO FREE MEM.
         ST    R4,TM$RESPG             SHOW THIS PRGM LOADED
         ST    R1,TM$TEMP              SAVE ENTRY PT. ADDR
         MVC   PT$NTRY,TM$TEMP+1
*
* EXECUTE THE PROGRAM
*         
TMSTART  LR    R15,R1                  ENTRY PT TO R15
         LA    R1,TM$PIB               POINT TO ARG. LIST
         BALR  R14,R15
*
* NORMAL TERMINATION CLEANUP
*
         CLI   ZA#PSIND,ZA#PSNN        TERM. IND = 'N'?
         BNE   TM5                     NO
         XC    TT$NXTPG,TT$NXTPG       CLEAR TERM'S NEXT PGM PTR.
         B     TMDONE
*
* EXTERNAL SUCCESSION
*         
TM5      CLI   ZA#PSIND,ZA#PSNE        TERM. IND = 'E'?
         BNE   TM6                     NO
         USING PGMTBL,R11
         L     R11,LM$PFRST            POINT TO 1ST PGM TABLE
TMFPGM   LTR   R11,R11                 ADDR = ZERO?
         BE    TMBADSUC                YES, END OF LIST
         CLC   ZA#PSID,PT$MNAME        MATCH SUCCESSOR ID?
         BE    TMGOTPGM                YES
         L     R11,PT$NEXT             GET ADDR OF NEXT PGM TABLE
         B     TMFPGM 
*
TMGOTPGM ST    R11,TT$NXTPG            SET SUCCESSOR AS NXT PGM TO EXEC
         B     TMSAVCDA                GO SAVE THE CDA
*
TMBADSUC MVC   TM$BDSC(6),ZA#PSID      MOVE SUCCESSOR ID TO ERR MSG
         OPR   TM$BDSC,L'TM$BDSC
         XC    TT$NXTPG,TT$NXTPG       CLEAR NEXT PGM TO EXECUTE
         B     TMDONE
         DROP  R11          
*
* SAVE CDA TO DISK
*
TMSAVCDA TS    SCR1LOCK                YES, ACQUIRE FILE LOCK
         BZ    TMLOK1                  SUCCESS
         SETIME 10,WAIT,M              FAIL, WAIT A BIT
         B     TMSAVCDA                & LOOP
*  
TMLOK1   LA    R6,SCR1P2               COVER CDA PARTITION
         L     R11,TM$CDA              POINT TO CDA
         ST    R11,PC$A1F              SET PCA I/O BUFFER ADDRESS
         MVI   PC$IOCNT,1              SET TO WRITE 1 BLOCK
         LH    R11,TT$TNUM             GET TERM BFR BLOCK #
         ST    R11,PC$PCAID
         PUT   $SCR1,SCR1P2
         MVI   SCR1LOCK,0              RELEASE FILE LOCK
*
* CONTINUOUS OUTPUT
*
         USING ZA#OMH,R11
         L     R11,TM$OMA              COVER THE OMA
         CLI   ZA#OAUX,ZA#OCO          CONTINUOUS OUTPUT REQUESTED?
         BNE   TMDONE                  NO, WE'RE DONE 
         MVC   TT$CONT,ZA#CONT         SAVE CONT. OUTPUT CODE
         OI    TT$FLAGS,TT$WAITO       SET WAIT FOR OUTPUT FLAG
         B     TMDONE
         DROP  R11
*
TM6      EQU   *         
*         
*         
* SEND OUTPUT TO TERMINAL
*
TMDONE   LR    R0,R3                   POINT TO TERM. TABLE
         L     R1,TM$OMA               POINT TO OUTPUT MSG AREA
         BAL   R14,SEND                SEND IT      
         OI    TT$FLAGS,TT$WAITI       SET TERMINAL WAIT FLAG         
         B     TMNEXT                  PGM DONE, CHECK FOR MORE TO DO
*
* MODULE LOAD ERROR HANDLER
*
TMLERR   MVC   LOADERR(8),PT$MNAME     PGM NAME TO ERR MSG
         OPR   LOADERR,L'LOADERR       TELL OPERATOR ABOUT BOO-BOO
         MVI   TT$FLAGS,TT$WAITI       SET TERM WAIT FOR INPUT FLAG
         B     TMNEXT                  CHECK FOR MORE TO DO
* 
TMBADPGM L     R4,=A(UNKCMD)           POINT TO UNK CMD ERROR PRGM
         B     TMEXEC3                 & EXECUTE IT
*
TMLOGON  L     R4,=A(UNKCMD)           POINT TO UNK CMD ERROR PGM
         USING ZA#IMH,R1               COVER IMA
         L     R1,TM$IMA
         MVI   ZA#IMSG,C'L'            SET FLG TO SEND 'PLS LOGON'
         B     TMEXEC3
         DROP  R1
*                   
         DROP  R2,R3,R4,R5,R6
************ 
*
* TASK MANAGER FIELDS
*
************              
LOADERR  DC    C'         LOAD ERROR'
TM$ECB   ECB
TM$SAVE  DC    18F'0'                  REGISTER SAVE AREA
TM$RESPG DS    A                       PTR. TO RESIDENT PGM TABLE
TM$TOKEN DS    CL8                     TOKEN FROM INPUT MSG.
TM$TEMP  DS    F
TM$BDSC  DS    C'       INVALID SUCCESSOR ID'
*
* WORKER PROGRAM ACTIVATION RECORD. THESE FIELDS MUST ALWAYS BE IN
* THIS ORDER.
*
TM$PIB   DS    A                       PRGM INFO BLOCK
TM$IMA   DS    A                       2048 BYTE INPUT MSG AREA
TM$WA    DS    A                       4096 BYTE WORK AREA
TM$OMA   DS    A                       2048 BYTE OUTPUT MSG AREA
TM$CDA   DS    A                       4096 BYTE CONT. DATA AREA
         EJECT
************
*
* SEND A MESSAGE TO A TERMINAL
*   R0  = PTR. TO TERM. TABLE
*   R1  = PTR. TO OMA
*   R2  = COVER
*   R3  = TERM. TABLE
*   R4  = OMA
*   R5  = MCT PACKET
*   R14 = RETURN ADDRESS
*
************
SEND     STM   R14,R12,12(R13)         SAVE REGISTERS
         BALR  R2,0                    SET NEW COVER
         USING *,R2
         USING TERMTBL,R3
         USING ZA#OMH,R4
         USING TN#MCTDS,R5
         LR    R3,R0                   COVER TERM TBL
         LR    R4,R1                   COVER OMA
         LA    R5,MCTO                 COVER OUTPUT MCT PACKET
         L     R6,LM$OBFR              COVER ICAM OUTPUT BUFFER
*         
SLOCK    TS    M$OBUSY                 SET THE OUTPUT BUSY FLAG
         BZ    SNOTBUSY                SUCCESS!    
         SETIME 10,WAIT,M              FAILED, WAIT 10 MS AND TRY AGAIN
         B     SLOCK
*         
SNOTBUSY LH    R0,ZA#OTL               COPY MSG TO ICAM BUFFER
         LA    R1,ZA#OMSG 
         L     R11,LM$OBFR
         L     R15,LM$CPYM
         BALR  R8,R15
*         
         MVI   M$FUNC,M$SEND           SET CRNT FUNCTION CODE
         LH    R11,TT$LINE             SET THE LINE #
         STH   R11,TN#MCLID
         LH    R11,TT$ID               SET TERM. #
         STH   R11,TN#MCTIN
         L     R11,LM$OBFR             POINT TO MSG BFR.
         ST    R11,TN#MCBFA
         LH    R11,ZA#OTL              SET THE TEXT LENGTH
         STH   R11,TN#MCBAL
         MVI   TN#MCCKB,TN#MCEM1++TN#MCLBW SET LAST BUFFER FLAGS
         CAWAKE                        WAKE UP THE MAIN TASK.
*
         LM    R14,R12,12(R13)         RESTORE REGISTERS
         BR    R14                     RETURN
         DROP  R2,R3,R4,R5
************
*
* MCT PACKETS & FLAGS
*
************
MCTO     MCTPKT OCMPL,SEND,IRL,                                        X
               BUFFERA=0,                                              X
               BAL=0,                                                  X
               ID=(1,1),                                               X
               ENDBUF=1
MCTI     MCTPKT ICMPL,RECEIVE,IRL,                                     X
               BUFFERA=0,                                              X
               BAL=0,                                                  X
               ID=(1,1),                                               X
               ENDBUF=1
*
M$OBUSY  DC    XL1'0'                  MCT OUTPUT BUSY FLAG
M$ICNT   DC    H'0'                    PENDING INPUT COUNT
M$FUNC   DS    XL1                     CURRENT NETWORK FUNCTION CODE
M$NETREQ EQU   0
M$CCACPY EQU   1
M$SEND   EQU   2
M$RECV   EQU   3
************
*
* DTF FOR THE BUFFER FILE. YOU NEED TO INCLUDE // WORK1 IN YOUR
* JOB CONTROL FOR THIS.
*
* THIS FILE HAS 2 PARTITIONS. PCA1 IS USED TO BUFFER INPUT MESSAGES.
* PCA2 IS USED TO HOLD THE CONTINUITY DATA FOR THE VARIOUS
* TRANSACTIONS.
*         
************
$SCR1    DTFPF PCA1=SCR1P1,                                            X
               PCA2=SCR1P2,                                            X
               ERROR=SCR1ERR,                                          X
               WAIT=YES
* I/O BUFFER PARTITION               
SCR1P1   PCA   BLKSIZE=2048,                                           X
               IOAREA1=SCR1IOA,                                        X
               LACE=1,                                                 X
               SIZE=50
* CDA PARTITION                              
SCR1P2   PCA   BLKSIZE=4096,                                           X
               IOAREA1=SCR1IOA,                                        X
               LACE=1,                                                 X
               SIZE=50          
SCR1IOA  EQU   *                       DUMMY I/O BUFFER 
SCR1LOCK DC    XL1'0'   
************
*
* WELL KNOWN PROGRAM TABLE ENTRIES
*
************
BEMRDY   DC    A(UNKCMD)               NEXT ENTRY
         DC    XL8'FFFFFFFFFFFFFFFF'   NO TRANS CODE FOR THIS
         DC    CL8'BEMRDY00'           'BEM READY' MSG TO ALL TERMS
         DC    F'0'                    LOAD MODULE SIZE
         DC    XL1'0'                  FLAG BYTE
         DS    XL3'0'                  ENTRY PT. ADDRESS
UNKCMD   DC    A(BEMHLP)               NEXT ENTRY
         DC    XL8'FFFFFFFFFFFFFFFF'   NO TRANS CODE FOR THIS
         DC    CL8'BADCMD00'           'UNKNOWN COMMAND' ERROR
         DC    F'0'                    LOAD MODULE SIZE
         DC    XL1'0'                  FLAG BYTE
         DS    XL3'0'                  ENTRY PT. ADDRESS
BEMHLP   DC    A(BEMLGN)               NEXT ENTRY
         DC    CL8'/HELP'              TRANS CODE
         DC    CL8'BEMHLP00'           
         DC    F'0'                    LOAD MODULE SIZE
         DC    XL1'0'                  FLAG BYTE
         DS    XL3'0'                  ENTRY PT. ADDRESS
BEMLGN   DC    A(BEMLGF)               NEXT ENTRY
         DC    CL8'/LOGON'             TRANS CODE
         DC    CL8'BEMLGN00'           
         DC    F'0'                    LOAD MODULE SIZE
         DC    XL1'0'                  FLAG BYTE
         DS    XL3'0'                  ENTRY PT. ADDRESS
BEMLGF   DC    A(BEMSTA)               NEXT ENTRY
         DC    CL8'/LOGOFF'            TRANS CODE
         DC    CL8'BEMLGF00'           
         DC    F'0'                    LOAD MODULE SIZE
         DC    YL1(PT$LOGON)           LOGON REQUIRED FLAG
         DS    XL3'0'                  ENTRY PT. ADDRESS
BEMSTA   DC    A(BEMDSP)               NEXT ENTRY
         DC    CL8'/STATUS'            TRANS CODE
         DC    CL8'BEMSTA00'           
         DC    F'0'                    LOAD MODULE SIZE
         DC    YL1(PT$LOGON)           LOGON REQUIRED FLAG
         DS    XL3'0'                  ENTRY PT. ADDRESS
BEMDSP   DC    A(BEMVTC)               NEXT ENTRY
         DC    CL8'/DISPLAY'           TRANS CODE
         DC    CL8'BEMDSP00'           
         DC    F'0'                    LOAD MODULE SIZE
         DC    YL1(PT$LOGON)           LOGON REQUIRED FLAG
         DS    XL3'0'                  ENTRY PT. ADDRESS
BEMVTC   DC    A(BEMFST)               NEXT ENTRY
         DC    CL8'/VTOC'              TRANS CODE
         DC    CL8'BEMVTC00'           
         DC    F'0'                    LOAD MODULE SIZE
         DC    YL1(PT$LOGON)           LOGON REQUIRED FLAG
         DS    XL3'0'                  ENTRY PT. ADDRESS
BEMFST   DC    A(BEMPRT)               NEXT ENTRY
         DC    CL8'/FSTATUS'           TRANS CODE
         DC    CL8'BEMFST00'           
         DC    F'0'                    LOAD MODULE SIZE
         DC    YL1(PT$LOGON)           LOGON REQUIRED FLAG
         DS    XL3'0'                  ENTRY PT. ADDRESS
BEMPRT   DC    A(BEMDEL)               NEXT ENTRY
         DC    CL8'/PRINT'             TRANS CODE
         DC    CL8'BEMPRT00'           
         DC    F'0'                    LOAD MODULE SIZE
         DC    YL1(PT$LOGON)           LOGON REQUIRED FLAG
         DS    XL3'0'                  ENTRY PT. ADDRESS
BEMDEL   DC    A(BEMEDT)               NEXT ENTRY
         DC    CL8'/DELETE'            TRANS CODE
         DC    CL8'BEMDEL00'           
         DC    F'0'                    LOAD MODULE SIZE
         DC    YL1(PT$LOGON)           LOGON REQUIRED FLAG
         DS    XL3'0'                  ENTRY PT. ADDRESS
BEMEDT   DC    A(0)                    NO NEXT ENTRY
         DC    CL8'/EDT'               TRANS CODE
         DC    CL8'BEMEDT00'           
         DC    F'0'                    LOAD MODULE SIZE
         DC    YL1(PT$LOGON)           LOGON REQUIRED FLAG
         DS    XL3'0'                  ENTRY PT. ADDRESS
PRGMSL   EQU   BEMEDT                  LAST ENTRY 
PRGMS    EQU   BEMRDY
************
*
* PROGRAM ALIASES
*
************
         DS    0F
ALIASES  DC    CL8'/HELP'
         DC    A(BEMHLP)
         DC    CL8'/HEL'
         DC    A(BEMHLP)
         DC    CL8'/HE'
         DC    A(BEMHLP)
         DC    CL8'/H'
         DC    A(BEMHLP)
         DC    CL8'/LOGON'
         DC    A(BEMLGN)
         DC    CL8'/LOGOFF'
         DC    A(BEMLGF)
         DC    CL8'/LOGOF'
         DC    A(BEMLGF)
         DC    CL8'/LOGO'
         DC    A(BEMLGF)
         DC    CL8'/LOG'
         DC    A(BEMLGF)
         DC    CL8'/LO'
         DC    A(BEMLGF)
         DC    CL8'/L'
         DC    A(BEMLGF)
         DC    CL8'/STATUS'
         DC    A(BEMSTA)
         DC    CL8'/STATU'
         DC    A(BEMSTA)
         DC    CL8'/STAT'
         DC    A(BEMSTA)
         DC    CL8'/STA'
         DC    A(BEMSTA)
         DC    CL8'/ST'
         DC    A(BEMSTA)
         DC    CL8'/S'
         DC    A(BEMSTA)
         DC    CL8'/DISPLAY'
         DC    A(BEMDSP)
         DC    CL8'/DISPLA'
         DC    A(BEMDSP)
         DC    CL8'/DISPL'
         DC    A(BEMDSP)
         DC    CL8'/DISP'
         DC    A(BEMDSP)
         DC    CL8'/DIS'
         DC    A(BEMDSP)
         DC    CL8'/DI'
         DC    A(BEMDSP)
         DC    CL8'/VTOC'
         DC    A(BEMVTC)
         DC    CL8'/VTO'
         DC    A(BEMVTC)
         DC    CL8'/VT'
         DC    A(BEMVTC)
         DC    CL8'/V'
         DC    A(BEMVTC)
         DC    CL8'/FSTATUS'
         DC    A(BEMFST)
         DC    CL8'/FSTATU'
         DC    A(BEMFST)
         DC    CL8'/FSTAT'
         DC    A(BEMFST)
         DC    CL8'/FSTA'
         DC    A(BEMFST)
         DC    CL8'/FST'
         DC    A(BEMFST)
         DC    CL8'/FS'
         DC    A(BEMFST)
         DC    CL8'/F'
         DC    A(BEMFST)
         DC    CL8'/PRINT'
         DC    A(BEMPRT)
         DC    CL8'/PRIN'
         DC    A(BEMPRT)
         DC    CL8'/PRI'
         DC    A(BEMPRT)
         DC    CL8'/PR'
         DC    A(BEMPRT)
         DC    CL8'/P'
         DC    A(BEMPRT)
         DC    CL8'/DELETE'
         DC    A(BEMDEL)
         DC    CL8'/DELET'
         DC    A(BEMDEL)
         DC    CL8'/DELE'
         DC    A(BEMDEL)
         DC    CL8'/DEL'
         DC    A(BEMDEL)
         DC    CL8'/DE'
         DC    A(BEMDEL)
         DC    CL8'/EDT'
         DC    A(BEMEDT)
         DC    CL8'/ED'
         DC    A(BEMEDT)
         DC    CL8'/E'
         DC    A(BEMEDT)
ALIASESL EQU   *-AT$LNGTH              PTR TO LAST ALIAS TABLE ENTRY         
************
*
* CLEAR A BLOCK OF MEMORY TO ZERO
*
*   R0  = BLOCK LENGTH
*   R1  = BLOCK ADDRESS
*   R8  = RETURN ADDRESS
*   R11 = WORK REGISTER
*   R15 = ENTRY PT.
*
************
         USING *,R15
CLRMEM   LTR   R0,R0                   LEN = 0?
         BZ    CMDONE                  YES
         MVI   0(R1),0                 CLEAR 1ST BYTE TO ZERO
         CH    R0,=H'256'              LEN > 256?
         BNH   CMLAST                  NO, CLEAR # BYTES GIVEN IN R0
         LA    R11,254                 CLEAR REMAINING 255 BYTES
         EX    R11,CMMVC
         SH    R0,=H'256'              DECR. LENGTH
         LA    R1,256(R1)              BUMP ADDRESS
         B     CLRMEM                  & LOOP                      
CMLAST   SH    R0,=H'2'                CLEAR REMAINING R0-1 BYTES
         LR    R11,R0
         EX    R11,CMMVC
CMDONE   BR    R8                      RETURN
*
CMMVC    MVC   1(0,R1),0(R1)
         DROP R15
************
*
* COPY A BLOCK OF MEMORY
*
*   R0  = BLOCK LENGH
*   R1  = SOURCE
*   R8  = RETURN ADDRESS
*   R11 = DESTINATION
*   R12 = WORK REGISTER
*   R15 = ENTRY PT.
************
         USING *,R15
CPYMEM   LTR   R0,R0                   LEN = 0?
         BZ    PMDONE                  YES         
         CH    R0,=H'256'              LEN > 256?
         BNH   PMLAST                  NO, MOVE # BYTES GIVEN IN R0
         LA    R12,255                 MOVE 256 BYTES
         EX    R12,PMMVC
         LA    R1,256(R1)              BUMP BLOCK PTRS
         LA    R11,256(R11)
         SH    R0,=H'256'              DECR. LEN
         B     CPYMEM                  & LOOP
PMLAST   SH    R0,=H'1'                MAKE LEN ZERO RELATIVE
         LR    R12,R0
         EX    R12,PMMVC         
PMDONE   BR    R8
*
PMMVC    MVC   0(0,R11),0(R1)
         DROP  R15
************
*
* GET THE NEXT TOKEN FROM AN INPUT STRING. WHEN CALLED FOR THE FIRST
* TIME FOR A GIVEN STRING, R1 MUST POINT TO THE START OF THE STRING AND
* R0 MUST CONTAIN THE LENGTH. R1 AND R0 WILL BE UPDATED AND MUST
* BE PASSED UNCHANGED TO ANY SUBSEQUENT CALLS FOR THE SAME STRING.
*
*   R0  = STRING LENGTH
*   R1  = STRING ADDRESS
*   R11 = TOKEN ADDRESS (8 BYTES)
*   R12 = TOKEN LENGTH
*   R14 = RETURN ADDRESS
*   R15 = ENTRY ADDRESS
*
************
         USING *,R15
GETOKEN  MVI   0(R11),C' '             INIT. TOKEN
         MVC   1(7,R11),0(R11)
         LA    R12,8                   INIT. TOKEN LENGTH
*         
GTLOOP1  LTR   R0,R0                   END OF STRING?
         BNPR  R14                     YES, RETURN
*
* SKIP LEADING DICE CODES, START OF ENTRY, COMMAS, SPACES.
*
         CLI   0(R1),X'10'             DICE?
         BNE   GT1                     NO
         LA    R1,4(R1)                YES, BUMP PAST DICE CODE
         AH    R0,=H'-4'               DECR. STRING SIZE
         B     GTLOOP1                 & LOOP 
GT1      CLI   0(R1),X'1E'             START-OF-ENTRY
         BE    GTBUMP1                 YES, SKIP
         CLI   0(R1),0                 NULL?
         BE    GTBUMP1
         CLI   0(R1),C','              COMMA?
         BE    GTBUMP1                 YES, SKIP
         CLI   0(R1),C' '              SPACE?
         BNE   GT2                     NO, START OF TOKEN
GTBUMP1  LA    R1,1(R1)                BUMP STRING ADDRESS
         AH    R0,=H'-1'               DECR. STRING LENGTH
         B     GTLOOP1
* 
* COPY INPUT TO TOKEN UNTIL COMMA OR SPACE OR END OF STRING
* ENCOUNTERED
*        
GT2      LTR   R12,R12                 TOKEN FULL?
         BNP   GT3                     YES, SKIP THIS CHAR
         MVC   0(1,R11),0(R1)          COPY CHAR. TO TOKEN
         LA    R11,1(R11)              BUMP TOKEN ADDRESS
         AH    R12,=H'-1'              DECR. TOKEN LENGTH
GT3      LA    R1,1(R1)                BUMP STRING ADDRESS
         AH    R0,=H'-1'               DECR. STRING LENGTH
         BNPR  R14                     END OF STRING, RETURN
         CLI   0(R1),0                 NULL?
         BER   R14                     YES, RETURN
         CLI   0(R1),C','              COMMA?
         BER   R14                     YES, RETURN
         CLI   0(R1),C' '              SPACE?
         BER   R14                     YES, RETURN
         B     GT2                     STILL VALID CHAR, LOOP
         DROP  R15                              
*
************
*
* OPERATOR COMMUNICATION (OPCOM) ISLAND CODE 
*
************
OPCOM    BALR  R2,0
         USING *,R2
*         
         CLC   OCMSG(3),=C'EOJ'        EOJ REQUESTED?
         BE    OCEOJ                   YES
         CLC   OCMSG(8),=C'SHUTDOWN'
         BE    OCEOJ                   YES 
         EXIT  OC
*
OCEOJ    OPR   OCOPR,L'OCOPR
         L     R11,=A(EOJFLG)          SET EOJ FLAG
         MVI   0(R11),X'FF'
         CAWAKE                        WAKE UP MAIN THREAD
         EXIT  OC
*         
OCSAVE   DS    18F
OCMSG    DS    CL60
OCOPR    DC    C'SHUTDOWN IN PROGRESS'
         DROP  R2
*         
         LTORG
*         
         END   MAIN                  
// FIN