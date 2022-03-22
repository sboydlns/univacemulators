VTEST    START
*
* AN ATTEMPT TO ACCESS A DISK'S VTOC. AS WRITTEN, THIS WILL ONLY WORK
* FOR IDA DISKS.
*
         PRINT NOGEN
         SUPEQU
         VTOC  VOL1=YES,                                               X
               F1=YES,                                                 X
               F4=YES
         PRINT GEN
*         
         BALR  R2,0
         USING *,R2
*
*         RDFCB VOLIN,RDFCBE            READ THE FILE CONTROL BLOCK
*
* INSTEAD OF READING THE FCB FROM A DEVICE ASSIGNMENT IN THE JOB
* CONTROL, WE CAN JUST MANUALLY FILL IN THE FCB PORTION OF THE 
* PIOCB BY SEARCHING THE SUPERVISOR'S PUB TABLE FOR THE DESIRED
* VOLUME NAME.
*
         LA    R1,=CL6'REL042'         FIND PUB FOR VOLUME LNS001
         BAL   R14,FNDPUB
         LTR   R1,R1                   SUCCESS?
         BZ    BADVOL                  NO, WTF?
         STH   R1,VOLIN+14             SAVE PTR. TO PUB
         MVC   VOLIN+8(2),=H'16'       SET FCB LENGTH TO 16 (MINIMUM)
*         
         BAL   R14,RDV1                READ THE VOL1 LABEL
*         BAL   R14,OBV1                OBTAIN THE VOL1 LABEL
         BAL   R14,RDF4                READ THE FMT4 LABEL
         BAL   R14,FRSTF1              FIND 1ST FMT1 RECORD IN VTOC
         LTR   R1,R1                   FMT1 FOUND?
         BZ    DONE                    NO
LOOP     OPR   VBFR-1,9
         BAL   R14,NEXTF1              GET NEXT FMT1
         LTR   R1,R1                   FMT1 FOUND?
         BZ    DONE                    NO
         B     LOOP                    YES, TRY AGAIN
*
DONE     EOJ
************         
*
* READ THE VOL1 LABEL. THIS IS ALWAYS AT CYL 0, HEAD 0, REC 3 ON THE
* DISK
*
************         
         USING IB$BCW,R1
RDV1     LA    R1,VBCW
         XC    IB$HEAD,IB$HEAD         HEAD = 0
         XC    IB$CYL,IB$CYL           CYL = 0
         OI    IB$CYL,X'80'            RECALIBRATE ON
         MVI   IB$RECRD,3              REC = 3
         EXCP  VCCB                    START I/O
         WAIT  VCCB,RDV1E              WAIT FOR IT
         USING DL$VL,R1
         LA    R1,VBFR                 COVER VOL1 LBL
         MVC   F4ADDR,DL$VTC           SAVE FMT4 RECORD ADDRESS
         BR    R14                     RETURN
         DROP  R1
************
*
* READ THE VOL1 LABEL USING THE OBTAIN MACRO
*
************
OBV1     MVI   OBPARAMS+8,X'80'        SET FUNC CODE TO 80 (READ VOL1)
         OBTAIN OBPARAMS,OBV1E,1,FCBCORE
         USING DL$VL,R1
         LA    R1,VBFR                 COVER VOL1 LBL
         MVC   F4ADDR,DL$VTC           SAVE FMT4 RECORD ADDRESS
         BR    R14                     RETURN
         DROP  R1
*
         DS    0F
OBPARAMS DC    CL8'VOLIN'
         DC    A(VBFR)         
         DC    A(VOLIN)
************         
*
* READ THE FMT4 LABEL. THE LOCATION OF THIS RECORD IS FOUND IN THE
* VOL1 RECORD
*
************         
         USING IB$BCW,R1
RDF4     LA    R1,VBCW
         MVC   IB$HEAD,F4ADDR+3        MOVE HEAD
         MVC   IB$CYL,F4ADDR           CYL
         MVC   IB$RECRD,F4ADDR+4       & RECORD TO BCW
         EXCP  VCCB                    START I/O
         WAIT  VCCB,RDF4E              WAIT FOR IT
         USING DL$F4,R1
         LA    R1,VBFR                 COVER FMT4 RECORD
         MVC   VTSTRT,DL$VX4+2         SAVE VTOC START/END ADDRESSES
         MVC   VTEND,DL$VX4+6
         BR    R14
         DROP  R1
************         
*
* FIND THE FIRST FMT1 RECORD IN THE VTOC. ON EXIT, R1 = 0 IF 
* NO FMT1 FOUND.
*
************
         USING IB$BCW,R3
         USING DL$F1,R4
FRSTF1   LA    R3,VBCW
         LA    R4,VBFR
         MVC   IB$HEAD,VTSTRT+3        VTOC START HEAD
         MVC   IB$CYL,VTSTRT                      CYL
         MVI   IB$RECRD,1              RECORD 1
         B     NF1READ                 GO READ THIS RECORD
         DROP  R3
************
*
* FIND THE NEXT FMT1 RECORD IN THE VTOC. ON EXIT,  R1 = 0 IF NO 
* FMT1 FOUND.         
*
************
         USING IB$BCW,R3
         USING DL$F1,R4
NEXTF1   LA    R3,VBCW
         LA    R4,VBFR
NF1LOOP  XR    R11,R11                 NO, BUMP RECORD #
         IC    R11,IB$RECRD             
         LA    R11,1(R11)
         STC   R11,IB$RECRD
         CH    R11,=H'40'              END OF TRACK?
         BNH   NF1READ                 NO
         MVI   IB$RECRD,1              YES, RESET RECORD # TO 1
         IC    R11,IB$HEAD             BUMP TRACK
         LA    R11,1(R11)
         STC   R11,IB$HEAD
         CH    R11,VTEND+2             END OF CYL?
         BNH   NF1READ                 NO
         MVI   IB$HEAD,0               YES, RESET TRACK # TO ZERO
         LH    R11,IB$CYL              BUMP CYLINDER
         LA    R11,1(R11)
         STH   R11,IB$CYL
         CH    R11,VTEND               END OF VTOC?
         BH    NF1NOFND                NO
NF1READ  EXCP  VCCB
         WAIT  VCCB,NEXTF1E         
         CLI   DL$ID1,C'1'             IS IT FMT1?
         BNE   NF1LOOP                 NO, TRY AGAIN
         LA    R1,1                    SHOW SUCCESS
         BR    R14                     & RETURN
*         
NF1NOFND XR    R1,R1                   SHOW NOT FOUND         
         BR    R14                     & RETURN
************
*
* FIND THE PUB FOR THE VOLUME POINTED TO BY R1. R1 WILL HAVE THE
* ABSOLUTE ADDRESS OF THE PUB ON EXIT. ZERO IF NOT FOUND.
*
************
         USING SB$SIB,R3
         USING JP$PRE,R5
         USING IP$PUB,R6
FNDPUB   LR    R11,R1                  SAVE R1 FOR LATER
         LA    R5,PREBFR
         GETINF PRE,PREBFR,L'PREBFR,0  GET PREAMBLE INFO
         MVC   BASEADR,JP$JSB          SAVE BASE ADDR OF JOB FOR LATER
*         
         LA    R3,SIBBFR
         GETINF SIB,SIBBFR,L'SIBBFR,0  GET SIB INFO
         MVC   NUMPUBS,SB$PBC+2        SAVE # OF PUBS         
         L     R4,SB$PBA               POINT TO 1ST PUB
         S     R4,BASEADR              MAKE NEGATIVE OFFSET INTO SUPER
         LA    R6,PUBBFR
*        
FPLOOP   MVC   PUBBFR(IP$LNGTH),0(R4)  COPY PUB TO MY BUFFER
         CLI   IP$TYP,X'20'            IS IT DISK?
         BNE   FPNEXT                  NO, TRY NEXT PUB
         LH    R12,IP$TRL              GET TRAILER ADDRESS
         S     R12,BASEADR             MAKE NEGATIVE OFFSET INTO SUPER
         MVC   IP$PUBT(IP$LENT),0(R12) COPY TRAILER TO MY BUFFER
         CLC   IP$VSN,0(R11)           VOLUME ID A MATCH?
         BNE   FPNEXT                  NO, TRY NEXT PUB
         A     R4,BASEADR              MAKE ADDR ABSOLUTE
         LR    R1,R4                   RETURN PTR T O PUB
         BR    R14
*         
FPNEXT   LA    R4,IP$LNGTH(R4)         BUMP PUB PTR
         AI    NUMPUBS,-1              DECR. PUB COUNT
         BP    FPLOOP                  > ZERO, THEN LOOP
*
         XR    R1,R1                   NOT FOUND
         BR    R14                     RETURN
         DROP  R3,R5,R6
*
* RDFCB ERROR ROUTINE
*
RDFCBE   N     R0,=A(X'FF')            ISOLATE ERROR CODE
         CANCEL (0)                    ABORT WITH ERROR CODE IN R0 
*
* ERROR READING VOL1 LABEL
*
         USING IC$CCB,R1
RDV1E    LA    R1,VCCB
         LH    R0,IC$SF                GET DEV / CHAN STATUS TO R0
         CANCEL (0)                    ABORT WITH STATUS
*
OBV1E    CANCEL (0)                    ABORT WITH STATUS
*
* ERROR READING FMT4 LABEL
*
         USING IC$CCB,R1
RDF4E    LA    R1,VCCB
         LH    R0,IC$SF                GET DEV / CHAN STATUS TO R0
         CANCEL (0)                    ABORT WITH STATUS
*
* ERROR READING FMT 1 RECORD
*
         USING IC$CCB,R1
NEXTF1E  LA    R1,VCCB
         LH    R0,IC$SF                GET DEV / CHAN STATUS TO R0
         CANCEL (0)                    ABORT WITH STATUS
*         
BADVOL   OPR   VOLERR,L'VOLERR
         CANCEL 100         
*                  
* AN IDA BCW TO READ 1, 256 BYTE RECORD INTO VBFR
*
VBCW     BCW   X'02',                                                  X
               VBFR,                                                   X
               X'00',                                                  X
               1
*
         DC    CL4' '               
VBFR     DS    XL256
* 
VOLIN    PIOCB MAX
*
VCCB     CCB   VOLIN,VBCW
* 
         DS    0D
SIBBFR   DS    XL(SB$LNGTH)            BUFFER TO HOLD SIB INFO
PREBFR   DS    XL(JP$LNGTH)            BUFFER TO HOLD JOB PREAMBLE INFO
PUBBFR   DS    XL(IP$LNGTH+IP$LENT)    BUFFER TO HOLD PUB & TRAILER
BASEADR  DS    A
NUMPUBS  DS    H
F4ADDR   DS    CL5                     FMT4 RECORD ADDRESS (CCHHR)
         DS    0H
VTSTRT   DS    XL4                     VTOC START ADDRESS (CCHH)
VTEND    DS    XL4                     VTOC END ADDRESS (CCHH)
*                
VOLERR   DC    C'INVALID VOLUME ID'
*         
         END     
// FIN                      