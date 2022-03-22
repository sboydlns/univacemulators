         TITLE '/VTOC COMMAND FOR BOYD''S BEM SIMULATOR'
BEMVTC   START
***********************************************************************
*                                                                     *
* AN IMPLEMENTATION OF THE /VTOC COMMAND FOR BOYD'S BEM SIMULATOR.    *
*                                                                     *
***********************************************************************
*
         PRINT NOGEN
         SUPEQU
         VTOC  FCB=YES,                                                X
               VOL1=YES,                                               X
               F1=YES,                                                 X
               F2=YES,                                                 X
               F3=YES,                                                 X
               F4=YES
PUBDSECT DSECT
         PUBEQU
BEMVTC   CSECT         
         ZM#DPIB
         ZM#DIMH
ZA#IMSG  DS    CL2048         
BEMVTC   CSECT 
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
         USING CDA,R7
*         
         L     R3,0(R1)                COVER THE PARAMETERS
         L     R4,4(R1)
         L     R5,8(R1)
         L     R6,12(R1)
         L     R7,16(R1)
         LA    R11,SAVAREA             SET UP NEW SAVE AREA
         ST    R13,4(R11)
         LR    R13,R11
*
         CLI   FSTPASS,X'00'           IS THIS THE FIRST PASS?
         BNE   PASS2                   NO
*
* FIRST PASS, DECODE THE INPUT MESSAGE
*         
         LH    R0,ZA#ITL               SET UP GETOKEN PARAMS
         LA    R1,ZA#IMSG
         L     R15,LM$GTKN
         LA    R11,VSN                 BURN '/VTOC'
         BALR  R14,R15    
         LA    R11,VSN                 GET VOLUME ID
         BALR  R14,R15
         CLI   VSN,C' '                VOLUME ID PRESENT?
         BE    BADVOL                  NO
         BAL   R14,INIT                INIIIALIZE THE I/O STRUCTURES
         MVC   LINCNT,=H'22'           INIT. LINE COUNTER
         B     FSTF1                   GO FETCH THE FIRST FMT1 RECORD
*
* COME HERE FOR ALL BUT FIRST PASS
*
PASS2    CLI   FSTPASS,X'01'           SHOULD WE SHOW NEXT LINE?
         BE    P2NEXT                  YES, CONTINUE
*
         LH    R0,ZA#ITL               SET UP GETOKEN PARAMS
         LA    R1,ZA#IMSG
         L     R15,LM$GTKN
         LA    R11,YN                  GET RESPONSE TO MORE?
         BALR  R14,R15 
         CLI   YN,C'N'                 RESPONSE = 'N'
         BE    EOJ                     YES, QUIT 
         MVC   LINCNT,=H'22'           NO, INIT LINE COUNT & CONTINUE                
         
P2NEXT   BAL   R14,INIT                INITIALIZE THE I/O STRUCTURES
         B     NXTF1                   GO FETCH THE NEXT FMT1 RECORD
*
* ALL SET, LET'S READ THE VTOC.
*
FSTF1    MVC   ZA#OMSG(HDRL),HDR       MOVE HEADER TO OMA
         LA    R11,HDRL                SET THE MSG LENGTH
         STH   R11,ZA#OTL
*         
         BAL   R14,RDV1                READ THE VOLUME LABEL
         BAL   R14,RDF4                READ THE FMT4 RECORD                           
         BAL   R14,FRSTF1              READ THE NEXT FMT1 RECORD         
CHKOK    LTR   R1,R1                   FMT1 RECORD FOUND?
         BZ    EOJ                     NO, QUIT
*
         USING DL$F1,R9
         LA    R9,VBFR                 COVER FMT1 RECORD 
         MVC   F2ADDR,DL$CP1           SAVE FMT2 ADDR FOR LATER        
         LH    R12,ZA#OTL              GET CRNT OFFSET INTO OMA
         LA    R12,ZA#OMSG(R12)         
         MVC   0(FILMSGL,R12),FILMSG   COPY FILE NAME TEMPLATE TO OMA
         AI    ZA#OTL,FILMSGL          BUMP MSG LENGTH
         MVC   FNAME(20,R12),DL$KEY1   COPY FILE NAME TO OMA
         MVC   FTYPE(4,R12),TUNK       COPY FILE TYPE TO OMA
         CLI   DL$FT1,X'20'
         BNE   P21
         MVC   FTYPE(4,R12),TSAM
P21      CLI   DL$FT1,X'40'
         BNE   P22
         MVC   FTYPE(4,R12),TDA         
P22      CLI   DL$FT1,X'60'
         BNE   P23
         MVC   FTYPE(4,R12),TNI         
P23      CLI   DL$FT1,X'80'
         BNE   P24
         MVC   FTYPE(4,R12),TISAM
P24      CLI   DL$FT1,X'90'
         BNE   P25
         MVC   FTYPE(4,R12),TIRAM         
P25      CLI   DL$FT1,X'02'
         BNE   P26
         MVC   FTYPE(4,R12),TSAT         
P26      XR    R11,R11                 GET # EXTENTS
         IC    R11,DL$XC1
         STH   R11,NUMEXT              SAVE FOR LATER
         CVD   R11,DTEMP               CONVERT TO PACKED
         MVC   FEXTENTS(4,R12),XCMASK  COPY # OF EXTENTS TO OMA
         ED    FEXTENTS(4,R12),DTEMP+6
         BAL   R14,CALCEXT             GO CALC # CYLINDERS IN ALL EXTS
         CVD   R0,DTEMP                CONVERT TO PACKED
         MVC   FCYL(4,R12),CYLMASK     COPY TO OMA
         ED    FCYL(4,R12),DTEMP+6
*
         AI    LINCNT,-1               DECR. LINE COUNTER
         BP    P2CONT                  > ZERO? THEN CONTINUE
         LH    R11,ZA#OTL              CALC OFFSET INTO OMA
         LA    R11,ZA#OMSG(R11)        
         MVC   0(MOREMSGL,R11),MOREMSG MOVE 'MORE?' TO OMA
         AI    ZA#OTL,MOREMSGL         BUMP MSG LENGTH
         MVI   ZA#PSIND,ZA#PSNE        SET EXTERNAL SUCCESSION 
         MVC   ZA#PSID,=C'BEMVTC'      TO OURSELVES
         MVI   FSTPASS,X'02'           SET TO CHECK 'MORE' RESPONSE
         B     DONE                    & RETURN TO MONITOR         
*                  
P2CONT   MVI   ZA#OAUX,ZA#OCO          SET UP CONTINUOUS OUTPUT
         MVI   ZA#PSIND,ZA#PSNE        SET UP EXT SUCCESSION
         MVC   ZA#PSID,=C'BEMVTC'      TO MYSELF
         MVI   FSTPASS,X'01'           SET TO SHOW NEXT LINE
         B     DONE
         DROP  R9
*         
NXTF1    BAL   R14,NEXTF1
         B     CHKOK
*
* EOJ. SEND BLANK LINE
*
EOJ      MVC   ZA#OMSG(EOJMSGL),EOJMSG MOVE MSG TO OMA
         LA    R11,EOJMSGL             SET MSG LENGTH
         STH   R11,ZA#OTL
         MVI   ZA#PSIND,ZA#PSNN        SET NORMAL TERMINATION         
*         
DONE     L     R13,4(R13)              RESTORE REGISTERS
         LM    R14,R12,12(R13)
         BR    R14
************
*
* INITIALIZE THE I/O STRUCTURES
*
************
INIT     STM   R14,R12,12(R13)
*
* INITIALIZE THE CCB
*
         USING IC$CCB,R12
         LA    R12,VCCB
         SETBIT BC$TRAFF,VCCB          SET TRAFFIC (I/O COMPLETE) FLAG
         LA    R11,VBCW                SET BCW ADDRESS
         ST    R11,IC$CCW
         LA    R11,VFCB+14             SET PIOCB (FCB) ADDRESS
         ST    R11,IC$PIO
         DROP  R12
*
* FIND THE PUB FOR THE GIVEN VOLUME AND POPULATE THE FCB
*
         LA    R1,VSN                  POINT TO VOLUME ID
         BAL   R14,FNDPUB              GO FIND PUB
         LTR   R1,R1                   WAS IT FOUND?
         BZ    UNKVOL                  NO
         USING DF$FCB,R12
         LA    R12,VFCB
         MVC   DF$FNM,VSN              SET UP VOLUME ID
         STH   R1,DF$PUB               SET UP ABS. PUB ADDR.
         LA    R11,16                  SET FCB LENGTH (MINIMUM)
         STH   R11,DF$CBL
*
         LM    R14,R12,12(R13)         
         BR    R14
************         
*
* READ THE VOL1 LABEL. THIS IS ALWAYS AT CYL 0, HEAD 0, REC 3 ON THE
* DISK
*
************         
         USING IB$BCW,R1
RDV1     LA    R1,VBCW
         LA    R11,VBFR                SET BFR ADDRESS
         ST    R11,IB$DATA
         MVI   IB$COM,X'02'            COMMAND = READ
         LA    R11,1                   SET RECORD COUNT
         STH   R11,IB$COUNT
         XC    IB$HEAD,IB$HEAD         HEAD = 0
         XC    IB$CYL,IB$CYL           CYL = 0
         OI    IB$CYL,X'80'            RECALIBRATE ON
         MVI   IB$RECRD,3              REC = 3
         LA    R1,VCCB
         EXCP  (1)                     START I/O
         WAIT  (1),RDV1E               WAIT FOR IT
         USING DL$VL,R1
         LA    R1,VBFR                 COVER VOL1 LBL
         MVC   F4ADDR,DL$VTC           SAVE FMT4 RECORD ADDRESS
         BR    R14                     RETURN
         DROP  R1
************         
*
* READ THE FMT2 RECORD. THE LOCATION OF THIS RECORD IS FOUND IN THE
* FMT1 RECORD
*
************         
         USING IB$BCW,R1
RDF2     LA    R1,VBCW
         LA    R11,VBFR                SET BFR ADDRESS
         ST    R11,IB$DATA
         MVI   IB$COM,X'02'            COMMAND = READ
         LA    R11,1                   SET RECORD COUNT
         STH   R11,IB$COUNT
         MVC   IB$HEAD,F2ADDR+3        MOVE HEAD
         MVC   IB$CYL,F2ADDR           CYL
         MVC   IB$RECRD,F2ADDR+4       & RECORD TO BCW
         LA    R1,VCCB
         EXCP  (1)                     START I/O
         WAIT  (1),RDF2E               WAIT FOR IT
         USING DL$SATF2,R1
         LA    R1,VBFR                 COVER FMT2 RECORD
         MVC   F3ADDR,DL$SCID2         SAVE FMT3 RECORD ADDRESS
         BR    R14
         DROP  R1
************         
*
* READ THE FMT3 RECORD. THE LOCATION OF THIS RECORD IS FOUND IN THE
* FMT2 RECORD
*
************         
         USING IB$BCW,R1
RDF3     LA    R1,VBCW
         LA    R11,VBFR                SET BFR ADDRESS
         ST    R11,IB$DATA
         MVI   IB$COM,X'02'            COMMAND = READ
         LA    R11,1                   SET RECORD COUNT
         STH   R11,IB$COUNT
         MVC   IB$HEAD,F3ADDR+3        MOVE HEAD
         MVC   IB$CYL,F3ADDR           CYL
         MVC   IB$RECRD,F3ADDR+4       & RECORD TO BCW
         LA    R1,VCCB
         EXCP  (1)                     START I/O
         WAIT  (1),RDF3E               WAIT FOR IT
         USING DL$F3,R1
         LA    R1,VBFR                 COVER FMT2 RECORD
         MVC   F3ADDR,DL$CP3           SAVE NEXT FMT3 RECORD ADDRESS
         BR    R14
         DROP  R1
************         
*
* READ THE FMT4 RECORD. THE LOCATION OF THIS RECORD IS FOUND IN THE
* VOL1 RECORD
*
************         
         USING IB$BCW,R1
RDF4     LA    R1,VBCW
         LA    R11,VBFR                SET BFR ADDRESS
         ST    R11,IB$DATA
         MVI   IB$COM,X'02'            COMMAND = READ
         LA    R11,1                   SET RECORD COUNT
         STH   R11,IB$COUNT
         MVC   IB$HEAD,F4ADDR+3        MOVE HEAD
         MVC   IB$CYL,F4ADDR           CYL
         MVC   IB$RECRD,F4ADDR+4       & RECORD TO BCW
         LA    R1,VCCB
         EXCP  (1)                     START I/O
         WAIT  (1),RDF4E               WAIT FOR IT
         USING DL$F4,R1
         LA    R1,VBFR                 COVER FMT4 RECORD
         MVC   VTSTRT(4),DL$VX4+2      SAVE VTOC START ADDRESS
         MVI   VTSTRT+4,1              SET STARTING RECORD # = 1
         MVC   VTEND(4),DL$VX4+6       SAVE VTOC END ADDRESS
         MVI   VTEND+4,40              SET ENDING RECORD # = 40
         BR    R14
         DROP  R1
************         
*
* FIND THE FIRST FMT1 RECORD IN THE VTOC. ON EXIT, R1 = 0 IF 
* NO FMT1 FOUND.
*
************
         USING IB$BCW,R8
         USING DL$F1,R9
FRSTF1   LA    R8,VBCW
         LA    R9,VBFR
         MVC   IB$HEAD,VTSTRT+3        VTOC START HEAD
         MVC   IB$CYL,VTSTRT                      CYL
         MVC   IB$RECRD,VTSTRT+4                  RECORD
         B     NF1READ                 GO READ THIS RECORD
         DROP  R8,R9
************
*
* FIND THE NEXT FMT1 RECORD IN THE VTOC. ON EXIT,  R1 = 0 IF NO 
* FMT1 FOUND.    
*
************
         USING IB$BCW,R8
         USING DL$F1,R9
NEXTF1   LA    R8,VBCW
         LA    R9,VBFR
NF1LOOP  MVC   IB$HEAD,VTSTRT+3        SET HEAD IN BCW
         MVC   IB$CYL,VTSTRT               CYL
         XR    R11,R11                 BUMP RECORD #
         IC    R11,VTSTRT+4           
         LA    R11,1(R11)
         STC   R11,IB$RECRD
         STC   R11,VTSTRT+4
         CH    R11,=H'40'              END OF TRACK?
         BNH   NF1READ                 NO
         MVI   IB$RECRD,1              YES, RESET RECORD # TO 1
         MVI   VTSTRT+4,1
         IC    R11,VTSTRT+3            BUMP HEAD
         LA    R11,1(R11)
         STC   R11,IB$HEAD
         STC   R11,VTSTRT+3
         CH    R11,VTEND+2             END OF CYL?
         BNH   NF1READ                 NO
         MVI   IB$HEAD,0               YES, RESET HEAD # TO ZERO
         MVI   VTSTRT+3,0
         LH    R11,VTSTRT              BUMP CYLINDER
         LA    R11,1(R11)
         STH   R11,IB$CYL
         STH   R11,VTSTRT
         CH    R11,VTEND               END OF VTOC?
         BH    NF1NOFND                NO
NF1READ  LA    R11,VBFR                SET BFR ADDRESS
         ST    R11,IB$DATA
         MVI   IB$COM,X'02'            COMMAND = READ
         LA    R11,1                   SET RECORD COUNT
         STH   R11,IB$COUNT
         LA    R1,VCCB
         EXCP  (1)
         WAIT  (1),NEXTF1E         
         CLI   DL$ID1,C'1'             IS IT FMT1?
         BNE   NF1LOOP                 NO, TRY AGAIN
         LA    R1,1                    SHOW SUCCESS
         BR    R14                     & RETURN
*         
NF1NOFND XR    R1,R1                   SHOW NOT FOUND         
         BR    R14                     & RETURN
         DROP  R8,R9
************
*
* CALCULATE THE NUMBER OF CYLINDERS ASSIGNED TO THE CURRENT FILE
* ON EXIT, R0 CONTAINS TTL # CYLINDERS ASSIGNED
*
************
CALCEXT  STM   R14,R12,12(R13)         SAVE REGISTERS
*
* CALC. FOR EXTENTS IN FMT1 RECORD WHICH IS CURRENTLY IN VBFR
*
         USING DL$F1,R1
         LA    R1,VBFR
         LA    R1,DL$XT1               POINT TO 1ST OF 3 EXTENTS
         DROP  R1
         XR    R0,R0                   CLEAR CYLINDER COUNT
         LA    R12,3                   SET LOOP COUNT
CELOOP1  CLI   0(R1),0                 EXTENT VALID?
         BE    CEBUMP1                 NO
         MVC   ELOW(2),2(R1)           GET EXTENT LOWER CYLINDER
         MVC   EHIGH(2),6(R1)          GET EXTENT UPPER CYLINDER
         LH    R11,EHIGH               CALC # CYLINDERS
         SH    R11,ELOW
         LA    R11,1(R11)
         AR    R0,R11                  ADD TO TOTAL
CEBUMP1  LA    R1,10(R1)               BUMP EXTENT TABLE ADDRESS
         BCT   R12,CELOOP1
*
* CALC. FOR EXTENTS GIVEN IN FMT3 RECORD
*
         LH    R11,NUMEXT              # EXTENTS > 3?
         CH    R11,=H'3'
         BNH   CEDONE                  NO, WE'RE DONE
         BAL   R14,RDF2                READ THE FMT2 RECORD
CENXTF3  CLC   F3ADDR,LM$ZERO          FMT3 ADDR = 0?
         BE    CEDONE                  YES, WE'RE DONE
         BAL   R14,RDF3                READ THE FMT3 RECORD
*
* DO EXTENTS 4-7
*         
         USING DL$F3,R1
         LA    R1,VBFR                 COVER FMT3
         LA    R1,DL$XT3               GET ADDR OF EXTENT 4
         DROP  R1
         LA    R12,4                   INIT. LOOP COUNT
CELOOP2  CLI   0(R1),0                 EXTENT VALID?
         BE    CEBUMP2                 NO         
         MVC   ELOW(2),2(R1)           GET EXTENT LOWER CYLINDER
         MVC   EHIGH(2),6(R1)          GET EXTENT UPPER CYLINDER
         LH    R11,EHIGH               CALC # CYLINDERS
         SH    R11,ELOW
         LA    R11,1(R11)
         AR    R0,R11                  ADD TO TOTAL
CEBUMP2  LA    R1,10(R1)               BUMP EXTENT TABLE ADDRESS
         BCT   R12,CELOOP2
*
* DO EXTENTS 8-16
*         
         LA    R1,1(R1)                BUMP PAST FORMAT ID
         LA    R12,9                   INIT LOOP COUNT
CELOOP3  CLI   0(R1),0                 EXTENT VALID?
         BE    CEBUMP3                 NO         
         MVC   ELOW(2),2(R1)           GET EXTENT LOWER CYLINDER
         MVC   EHIGH(2),6(R1)          GET EXTENT UPPER CYLINDER
         LH    R11,EHIGH               CALC # CYLINDERS
         SH    R11,ELOW
         LA    R11,1(R11)
         AR    R0,R11                  ADD TO TOTAL
CEBUMP3  LA    R1,10(R1)               BUMP EXTENT TABLE ADDRESS
         BCT   R12,CELOOP3
         B     CENXTF3                 CHECK NEXT FMT3 IN CHAIN
*         
CEDONE   ST    R0,20(R13)              RETURN R0
         LM    R14,R12,12(R13)         RESTORE REGISTERS
         BR    R14                     & RETURN
************
*
* ERROR READING VOL1 LABEL
*
************
         USING IC$CCB,R1
RDV1E    LA    R1,VCCB
         LH    R0,IC$SF                GET DEV / CHAN STATUS TO R0
         CANCEL (0)                    ABORT WITH STATUS
***********
*
* ERROR READING FMT2 LABEL
*
************
         USING IC$CCB,R1
RDF2E    LA    R1,VCCB
         LH    R0,IC$SF                GET DEV / CHAN STATUS TO R0
         CANCEL (0)                    ABORT WITH STATUS
***********
*
* ERROR READING FMT3 LABEL
*
************
         USING IC$CCB,R1
RDF3E    LA    R1,VCCB
         LH    R0,IC$SF                GET DEV / CHAN STATUS TO R0
         CANCEL (0)                    ABORT WITH STATUS
***********
*
* ERROR READING FMT4 LABEL
*
************
         USING IC$CCB,R1
RDF4E    LA    R1,VCCB
         LH    R0,IC$SF                GET DEV / CHAN STATUS TO R0
         CANCEL (0)                    ABORT WITH STATUS
***********
*
* ERROR READING FMT1 LABEL
*
************
         USING IC$CCB,R1
NEXTF1E  LA    R1,VCCB
         LH    R0,IC$SF                GET DEV / CHAN STATUS TO R0
         CANCEL (0)                    ABORT WITH STATUS
************
*
* SEND VOLUME ID NOT SPECIFIED ERROR
*
************
BADVOL   MVC   ZA#OMSG(NOVOLL),NOVOL   MOVE MSG TO OMA
         LA    R11,NOVOLL              SET MSG LENGTH
         STH   R11,ZA#OTL
         MVI   ZA#PSIND,ZA#PSNN        SET NORMAL TERMINATION
         B     DONE            
************
*
* SEND UNKNOWN VOLUME ID ERROR
*
************
UNKVOL   MVC   ZA#OMSG(ILLVOLL),ILLVOL MOVE MSG TO OMA
         LA    R11,ILLVOLL             SET MSG LENGTH
         STH   R11,ZA#OTL
         MVI   ZA#PSIND,ZA#PSNN        SET NORMAL TERMINATION
         B     DONE            
************
*
* FIND THE PUB FOR THE VOLUME POINTED TO BY R1. R1 WILL HAVE THE
* ABSOLUTE ADDRESS OF THE PUB ON EXIT. ZERO IF NOT FOUND.
*
************
         USING SB$SIB,R10
         USING JP$PRE,R8
         USING IP$PUB,R9
FNDPUB   LR    R11,R1                  SAVE R1 FOR LATER
*
* GET ABSOLUTE BASE ADDR OF BEMSIM FROM THE PREAMBLE
*
         LA    R8,PREBFR
         LR    R1,R8
         GETINF PRE,(1),L'PREBFR,0  GET PREAMBLE INFO
         MVC   BASEADR,JP$JSB          SAVE BASE ADDR OF JOB FOR LATER
*
* GET PTR TO 1ST PUB & NUMBER OF PUBS FROM THE SIB
*         
         LA    R10,SIBBFR
         LR    R1,R10
         GETINF SIB,(1),L'SIBBFR,0  GET SIB INFO
         MVC   NUMPUBS,SB$PBC+2        SAVE # OF PUBS         
         L     R1,SB$PBA               POINT TO 1ST PUB
         S     R1,BASEADR              MAKE NEGATIVE OFFSET INTO SUPER
*
* LOOP THROUGH THE PUB TABLE LOOKING FOR A MATCH
*         
         LA    R9,PUBBFR               COVER PUBBFR
FPLOOP   MVC   PUBBFR(IP$LNGTH),0(R1)  COPY PUB TO MY BUFFER
         CLI   IP$TYP,X'20'            IS IT DISK?
         BNE   FPNEXT                  NO, TRY NEXT PUB
         LH    R12,IP$TRL              GET TRAILER ADDRESS
         S     R12,BASEADR             MAKE NEGATIVE OFFSET INTO SUPER
         MVC   IP$PUBT(IP$LENT),0(R12) COPY TRAILER TO MY BUFFER
         CLC   IP$VSN,0(R11)           VOLUME ID A MATCH?
         BNE   FPNEXT                  NO, TRY NEXT PUB
         A     R1,BASEADR              MAKE PUB ADDR ABSOLUTE
         BR    R14                     RETURN
*         
FPNEXT   LA    R1,IP$LNGTH(R1)         BUMP PUB PTR
         AI    NUMPUBS,-1              DECR. PUB COUNT
         BP    FPLOOP                  > ZERO, THEN LOOP
*
         XR    R1,R1                   SHOW NOT FOUND
         BR    R14                     RETURN
         DROP  R8,R9,R10
************
TSAM     DC    CL4'SAM'
TDA      DC    CL4'D.A.'
TNI      DC    CL4'N.I.'
TISAM    DC    CL4'ISAM'
TIRAM    DC    CL4'IRAM'
TSAT     DC    CL4'SAT'
TUNK     DC    CL4'UNKN'  
XCMASK   DC    XL4'40202120'
CYLMASK  DC    XL4'40202120'       
************
NOVOL    ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'27'                 ERASE PROTECTED
         DC    CL1'M'
         DC    XL1'1E'                 SOE
         DC    C'THE VOLUME ID MUST BE SPECIFIED. /VTOC VOL-ID'
         ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'1E'                 SOE
NOVOLL   EQU   *-NOVOL
************
ILLVOL   ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'27'                 ERASE PROTECTED
         DC    CL1'M'
         DC    XL1'1E'                 SOE
         DC    C'UNKNOWN VOLUME ID'
         ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'1E'                 SOE
ILLVOLL  EQU   *-ILLVOL
************
HDR      ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'27'                 ERASE PROTECTED
         DC    CL1'M'
         DC    XL1'1E'                 SOE
         DC    C'FILENAME             CYL EXTENTS TYPE'
HDRL     EQU   *-HDR         
************
FILMSG   ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'27'                 ERASE PROTECTED
         DC    CL1'M'
         DC    XL1'1E'                 SOE
FNAME    EQU   *-FILMSG
         DC    C'XXXXXXXXXXXXXXXXXXXX'
FCYL     EQU   *-FILMSG
         DC    C' XXX  '
FEXTENTS EQU   *-FILMSG         
         DC    C' XXX   '
FTYPE    EQU   *-FILMSG         
         DC    C'XXXX'
FILMSGL  EQU   *-FILMSG
************
EOJMSG   ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'27'                 ERASE PROTECTED
         DC    CL1'M'
         DC    XL1'1E'                 SOE
EOJMSGL  EQU   *-EOJMSG         
************
MOREMSG  ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'1E'                 SOE
         DC    C'MORE (Y/N)? '
         DC    XL1'1E'                 SOE
MOREMSGL EQU   *-MOREMSG         
************
         ZM#DOMH
ZA#OMSG  DS    CL2048                  OUTPUT MSG BFR.
************
WORKAREA DSECT
DTEMP    DS    D                       DOUBLE WORD ALIGNED TEMP VAR.
ELOW     DS    H                       EXTENT LOWER CYL
EHIGH    DS    H                       EXTENT UPPER CYL
BASEADR  DS    A                       BASE ADDR. OF BEMSIM JOB 
NUMPUBS  DS    H                       # OF PUBS
NUMEXT   DS    H                       # OF EXTENTS
CTEMP    DS    CL10
YN       DS    CL1
         DS    0D
PREBFR   DS    XL(JP$LNGTH)            JOB PREAMBLE BUFFER
         DS    0D
SIBBFR   DS    XL(SB$LNGTH)            SYS. INFO. BLOCK BUFFER
         DS    0D
PUBBFR   DS    XL(IP$LNGTH)            PUB BUFFER
PTRLBFR  DS    XL(IP$LENT)             PUB TRAILER BFR
         DS    0D
VBCW     DS    XL(IB$LNGTH)            DISK BCW
         DS    0H
VBFR     DS    XL256                   DISK I/O BUFFER
         DS    0D
VCCB     DS    XL(IC$LNGTH)            DISK CCB
         DS    0D
VFCB     DS    XL(256)                 DISK FCB                  
F4ADDR   DS    CL5                     FMT4 RECORD ADDRESS (CCHHR)
F2ADDR   DS    CL5                     FMT2 RECORD ADDRESS (CCHHR)
F3ADDR   DS    CL5                     FMT2 RECORD ADDRESS (CCHHR)
SAVAREA  DS    18F
************
CDA      DSECT
FSTPASS  DS    XL1                     FIRST PASS FLAG (0 = FIRST PASS)
         DS    XL1
LINCNT   DS    H                       SCREEN LINE COUNTER         
VSN      DS    CL8                     VOLUME ID
         DS    0H
VTSTRT   DS    XL5                     VTOC START ADDRESS (CCHHR)
         DS    0H
VTEND    DS    XL5                     VTOC END ADDRESS (CCHHR)
*
BEMVTC   CSECT
*
         END
// FIN