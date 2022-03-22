         TITLE 'LIBRARY INTERFACE ROUTINES FOR BOYD''S BEM SIMULATOR'
BEMLIB   START
***********************************************************************
*                                                                     *
*       LIBRARY I/O ROUTINES FOR BOYD'S BEM SIMULATOR                 *
*                                                                     *
***********************************************************************
         PRINT NOGEN
         SUPEQU REGS=YES
         SA$DSECT
         DTFDM
         BEMDSCTS
         ENTRY L$OPEN,L$CLOSE,L$DFIRST,L$DNEXT,L$EHDR
         ENTRY L$DFIND,L$EFIRST,L$ENEXT,L$DELETE
*
         USING DM$DSCT,R3
         USING LIBFIL,R5
         USING SA$DSECT,R13
*         
************
*
* OPEN A LIBRARY FILE.
*   R0  = RETURNS ERROR CODE 
*   R1  = PTR TO PARAMTER PACKET
*   R2  = COVER
*   R5  = PARAM. PACKET COVER
*   R14 = RETURN ADDR.
*   R15 = ENTRY POINT ADDR.
*
************
         DS    0H
L$OPEN   SAVE  (14,12),COVER=2
         LR    R5,R1                   COVER PARAM. PACKET
         BAL   R14,L$LOCK              ACQUIRE SGL THREAD LOCK
         LA    R3,L$DTF                COVER THE DTF   
         MVC   DC$NME,LF$NAME          MDFY DTF WITH FILE NAME
         LA    R11,L$OPNERR            SET ERROR ADDRESS
         ST    R11,DC$ERCD
         OPEN  L$DTF                   OPEN THE FILE
         XC    SA$R0,SA$R0             ERROR CODE = 0
         RETURN (14,12)
         DROP  R2
************
*
* CLOSE A LIBRARY FILE
*   R14 = RETURN ADDR.
*   R15 = ENTRY POINT ADDR.
*
************
L$CLOSE  SAVE  (14,12),COVER=2
         CLI   L$TS,X'FF'              IS FILE OPEN?
         BNE   LC$DONE                 NO, IGNORE REQUEST
LC$OK    LA    R3,L$DTF 
         XR    R11,R11                 CLEAR ERROR ADDRESS
         ST    R11,DC$ERCD
         CLOSE L$DTF
         MVI   L$TS,0                  RELEASE THE LOCK
LC$DONE  XC    SA$R0,SA$R0             CLEAR ERROR CODE
         RETURN (14,12)
         DROP  R2
************
*
* GET THE FIRST DIRECTORY ENTRY FOR THE CURRENT LIBRARY
*   R0  = RETURNS ERROR CODE 
*   R1  = ON ENTRY PTR TO PARAMETER PACKET. ON EXIT, RETURNS PTR
*         TO DIRECTORY ENTRY 
*   R2  = COVER
*   R5  = PARAMETER PACKET COVER
*   R14 = RETURN ADDRESS         
*   R15 = ENTRY POINT ADDR.
*
************
L$DFIRST SAVE  (14,12),COVER=2
         BAL   R14,L$CKLOCK            CHECK FILE HAS BEEN OPENED
         ST    R13,LDF$SAVE+4          SAVE SA BACK LINK
         LA    R13,LDF$SAVE            POINT TO OUR SAVE AREA
         LR    R5,R1                   COVER PARAMETER PACKET
         LA    R11,1                   SET NEXT BLOCK ID TO 1
         ST    R11,LF$DBLK
         L     R15,=A(L$DGET)          READ BLOCK 1
         BALR  R14,R15
         XR    R1,R1                   CLEAR DIR. ENTRY PTR
         LTR   R0,R0                   I/O SUCCESS?
         BNZ   LDF$DONE                NO, QUIT
         LA    R11,LBH$SIZE            SET INITIAL BLK OFFSET
         STH   R11,LF$DOFST
         L     R1,LF$DBFR              CALC PTR. TO DIR. ENTRY
         AR    R1,R11
         XR    R0,R0                   ERROR CODE = 0
LDF$DONE L     R13,SA$BLNK             COVER CALLER SAVE AREA
         ST    R0,SA$R0                RETURN ERROR CODE
         ST    R1,SA$R1                RETURN PTR TO DIR. ENTRY
         RETURN (14,12)
*
LDF$SAVE DS    18F         
         DROP  R2
************
*
* GET THE NEXT DIRECTORY ENTRY FOR THE CURRENT LIBRARY
*
*   R0  = RETURNS ERROR CODE 
*   R1  = ON ENTRY PTR TO PARAMETER PACKET. ON EXIT, RETURNS PTR
*         TO DIRECTORY ENTRY 
*   R2  = COVER
*   R5  = PARAMETER PACKET COVER
*   R14 = RETURN ADDRESS         
*   R15 = ENTRY POINT ADDR.
*
************
L$DNEXT  SAVE  (14,12),COVER=2
         BAL   R14,L$CKLOCK            CHECK FILE HAS BEEN OPENED
         ST    R13,LDN$SAVE+4          SAVE SA BACK LINK
         LA    R13,LDN$SAVE            POINT TO OUT SAVE AREA
         LR    R5,R1                   COVER PARAMETER PACKET
         LH    R11,LF$DOFST            GET OFFSET TO LAST DIR ENTRY
         LA    R11,LDE$SIZE(R11)       BUMP TO NEXT
         CH    R11,LF$DLEN             END OF BLOCK?
         BL    LDN$BOK                 NO
         L     R11,LF$DBLK             YES, BUMP BLOCK #
         LA    R11,1(R11)
         ST    R11,LF$DBLK
         L     R15,=A(L$DGET)          READ NEXT BLOCK
         BALR  R14,R15
         XR    R1,R1                   CLEAR DIR. ENTRY PTR.
         LTR   R0,R0                   I/O SUCCESS?
         BNZ   LDN$DONE                NO, QUIT
         LA    R11,LBH$SIZE            SET OFFSET TO 1ST ENTRY
LDN$BOK  STH   R11,LF$DOFST            SAVE NEW OFFSET         
         L     R1,LF$DBFR              CALC PTR TO DIR. ENTRY
         AR    R1,R11
         XR    R0,R0                   ERROR CODE = 0
LDN$DONE L     R13,SA$BLNK             COVER CALLER SAVE AREA
         ST    R0,SA$R0                RETURN ERROR CODE        
         ST    R1,SA$R1                RETURN PTR TO DIR. ENTRY
         RETURN (14,12)
*
LDN$SAVE DS    18F         
         DROP  R2
************
*
* FIND AN ENTRY IN THE DIRECTORY
*   R0  = RETURNS ERROR CODE 
*   R1  = PTR TO PARAMETER PACKET / RETURNS PTR TO DIR. ENTRY
*         ZERO IF NOT FOUND
*   R2  = COVER
*   R5  = PARAMETER PACKET COVER
*   R14 = RETURN ADDRESS
*   R15 = ENTRY POINT ADDRESS                
************
         USING L$DIRENT,R1
L$DFIND  SAVE  (14,12),COVER=2
         BAL   R14,L$CKLOCK            CHECK FILE HAS BEEN OPENED
         ST    R13,LFN$SAVE+4          SAVE SA BACK LINK
         LA    R13,LFN$SAVE            POINT TO OUT SAVE AREA
         LR    R5,R1                   COVER PARAMETER PACKET
         L     R15,=A(L$DFIRST)        GET 1ST DIRECTORY ENTRY
         BALR  R14,R15
         LTR   R0,R0                   SUCCESS?
         BNZ   LFN$NFND                NO, QUIT
LFN$LOOP CLI   LDE$TYPE,LET$EOF        END OF FILE?
         BE    LFN$NFND                YES
         CLC   LDE$NAME,LF$ELE         NAME MATCH?                  
         BNE   LFN$NEXT                NO
         CLC   LDE$TYPE,LF$ETYP        ELE. TYPE MATCH?
         BE    LFN$FND                 YES
LFN$NEXT LR    R1,R5                   POINT TO PARAM PACKET
         L     R15,=A(L$DNEXT)         GET NEXT DIRECTORY ENTRY
         BALR  R14,R15
         LTR   R0,R0                   SUCCESS?
         BZ    LFN$LOOP                YES
*
LFN$NFND XR    R1,R1
LFN$FND  L     R13,SA$BLNK             RESTORE R13
         ST    R0,SA$R0                RETURN ERROR CODE
         ST    R1,SA$R1                RETURN PTR TO DIR ENTRY
         RETURN (14,12)
*
LFN$SAVE DS    18F         
         DROP  R1
************
*
* DELETE THE ELEMENT SPECIFIED IN THE PARAMETER PACKET.
*
*   R0  = RETURNS ERROR CODE 
*   R1  = PTR TO PARAMETER PACKET. RETURNS ZERO IF NOT FOUND.
*         ONE OTHERWISE.
*   R2  = COVER
*   R14 = RETURN ADDRESS
*   R15 = ENTRY POINT ADDRESS                
************
         USING L$DIRENT,R1
L$DELETE SAVE  (14,12),COVER=2
         BAL   R14,L$CKLOCK            CHECK FILE HAS BEEN OPENED
         ST    R13,LD$SAVE+4           SAVE SA BACK LINK
         LA    R13,LD$SAVE             POINT TO OUT SAVE AREA
         LR    R5,R1                   SAVE PTR TO PARAM PACKET.
         L     R15,=A(L$DFIND)         FIND ELE TO BE DELETED
         BALR  R14,R15
         LTR   R0,R0                   SUCCESS?
         BNZ   LD$NFND                 NO
         LTR   R1,R1                   ELEMENT FOUND?
         BZ    LD$NFND                 NO
         MVI   LDE$TYPE,LET$NULL       SHOW ELEMENT DELETED
         LR    R1,R5                   RESTORE PTR TO PARAM PACKET
         L     R15,=A(L$DPUT)          REWRITE DIRECTORY BLOCK
         BALR  R14,R15
         LA    R1,1                    SHOW RECORD FOUND
         B     LD$FND
*         
LD$NFND  XR    R1,R1                   SHOW NOT FOUND         
LD$FND   L     R13,SA$BLNK             RESTORE R13
         ST    R0,SA$R0                RETURN ERROR CODE
         ST    R1,SA$R1                RETURN FOUND/NOT FOUND
         RETURN (14,12)                & RETURN TO CALLER
*
LD$SAVE  DS    18F         
         DROP  R1
************
*
* READ THE ELEMENT HEADER BELONGING TO THE DIRECTORY ENTRY GIVEN
* BY LF$DIRP.
*   R0  = RETURNS ERROR CODE
*   R1  = PTR TO PARAMETER PACKET / RETURNS PTR TO MODULE HEADER
*   R2  = COVER ADDRESS
*   R4  = DIRECTORY ENTRY COVER
*   R5  = PARAMETER PACKET COVEVER
*   R14 = RETURN ADDRESS
*   R15 = ENTRY POINT
*            
************
         USING L$DIRENT,R4
L$EHDR   SAVE  (14,12),COVER=2
         BAL   R14,L$CKLOCK            CHECK FILE HAS BEEN OPENED
         ST    R13,LEH$SAVE+4          SET SA BACK LINK
         LA    R13,LEH$SAVE            POINT TO OUR SAVE AREA
         LR    R5,R1                   COVER PARAMETER PACKET
         L     R4,LF$DIRP              COVER DIRECTORY ENTRY
         XC    LF$EBLK,LF$EBLK         CLEAR BLOCK # IN PKT
         MVC   LF$EBLK+1(3),LDE$BLK    GET BLOCK # TO READ
         XC    LF$EOFST,LF$EOFST       GET OFFSET TO HEADER
         MVC   LF$EOFST+1,LDE$REC
         L     R15,=A(L$EGET)          READ THE BLOCK
         BALR  R14,R15
         XR    R1,R1                   CLEAR MODULE HEADER PTR
         LTR   R0,R0                   I/O SUCCESS?
         BNZ   LEH$DONE                NO, QUIT
         L     R1,LF$EBFR              CALC. PTR TO MODULE HEADER
         AH    R1,LF$EOFST
*
LEH$DONE L     R13,SA$BLNK             COVER CALLER SAVE AREA
         ST    R0,SA$R0                RETURN ERROR CODE        
         ST    R1,SA$R1                RETURN PTR TO DIR. ENTRY
         RETURN (14,12)
*
LEH$SAVE DS    18F         
         DROP  R2,R4
************
*
* GET THE FIRST RECORD OF AN ELEMENT
*
*   R0  = RETURNS ERROR CODE
*   R1  = PTR TO PARAMETER PACKET
*   R2  = COVER ADDRESS
*   R4  = DIRECTORY ENTRY COVER
*   R5  = PARAMETER PACKET COVEVER
*   R6  = SOURCE MODULE HEADER COVER
*   R14 = RETURN ADDRESS
*   R15 = ENTRY POINT ADDR.
*
************
         USING L$DIRENT,R4
         USING L$MODHDR,R6
L$EFIRST SAVE  (14,12),COVER=2
         BAL   R14,L$CKLOCK            CHECK FILE HAS BEEN OPENED
         ST    R13,LEN$SAVE+4          SET SA BACK LINK
         LA    R13,LEN$SAVE            POINT TO OUR SAVE AREA
         LR    R5,R1                   COVER PARAM PACKET
         L     R4,LF$DIRP              COVER DIRECTORY ENTRY
*
         XC    LF$EBLK,LF$EBLK         SET 1ST DATA BLOCK #
         MVC   LF$EBLK+1(3),LDE$BLK
         L     R15,=A(L$EGET)          READ THE BLOCK
         BALR  R14,R15
         LTR   R0,R0                   SUCCESS?
         BNZ   LEN$DONE
*         
*        BUMP PAST MODULE HEADER         
*
         XR    R6,R6                   GET OFFSET TO MODULE HEADER
         IC    R6,LDE$REC
         STH   R6,LF$EOFST             SAVE IT
         A     R6,LF$EBFR              POINT TO MODULE HDR IN BUFFER
         XR    R11,R11                 GET MODULDE HDR LENGTH
         IC    R11,LMH$LEN
         LA    R11,2(R11)              CALC. OFFSET TO 1ST DATA RECORD
         AH    R11,LF$EOFST            
         STH   R11,LF$EOFST            SAVE IT.
         B     L$ENEXT1                GO GET DATA RECORD
         DROP  R2,R4,R6
************
*
* GET THE NEXT RECORD OF AN ELEMENT
*   R0  = RETURNS ERROR CODE
*   R1  = PTR TO PARAMETER PACKET
*   R2  = COVER ADDRESS
*   R4  = PTR. TO USER'S LINE BFR
*   R5  = PARAMETER PACKET COVEVER
*   R7  = SOURCE MODULE HEADER COVER
*   R14 = RETURN ADDRESS
*   R15 = ENTRY POINT ADRESS
*
************
         USING L$MODHDR,R7
L$ENEXT  SAVE  (14,12),COVER=2
         BAL   R14,L$CKLOCK            CHECK FILE HAS BEEN OPENED
         ST    R13,LEN$SAVE+4          SET SA BACK LINK
         LA    R13,LEN$SAVE            POINT TO OUR SAVE AREA
         LR    R5,R1                   COVER PARAM PACKET
*
L$ENEXT1 BALR  R2,0
         USING *,R2
*         
         L     R4,LF$SBFR              GET USER'S SOURCE BFR ADDR.
         MVI   0(R4),C' '              CLEAR THE USER'S BUFFER
         MVC   1(255,R4),0(R4)
         LH    R11,LF$EOFST            GET OFFSET TO RECORD
         CH    R11,LF$ELEN             END OF BLOCK?
         BL    LEN$OK                  NO
         L     R11,LF$EBLK             BUMP BLOCK #
         LA    R11,1(R11)
         ST    R11,LF$EBLK
         L     R15,=A(L$EGET)          READ THE BLOCK
         BALR  R14,R15
         LTR   R0,R0                   SUCCESS?
         BNZ   LEN$DONE                NO
         LA    R11,LBH$SIZE            SET INITIAL OFFSET
*         
LEN$OK   LR    R7,R11                  CALC. ADDR OF RECORD
         A     R7,LF$EBFR
         XR    R10,R10                 GET LENGTH OF RECORD
         IC    R10,LMH$LEN
         STH   R10,LEN$LEN             SAVE IT FOR LATER
         AR    R10,R11                 CALC. OFFSET TO NEXT RECORD
         LA    R10,2(R10)
         STH   R10,LF$EOFST            SAVE IT
         CLI   LMH$TYPE,X'24'          TRUNCATED?
         BE    LEN$TRNC                YES
         CLI   LMH$TYPE,X'25'          COMPRESSED
         BE    LEN$COMP                YES
         B     LEN$EOF                 IF NOT ONE OF ABOVE, END OF ELE.
*         
LEN$TRNC LH    R10,LEN$LEN             GET LENGTH OF DATA
         AH    R10,=H'-1'              MAKE ZERO RELATIVE
         EX    R10,LEN$MVC             MOVE DATA TO USER BUFFER
         XR    R0,R0                   SHOW NOT END OF FILE
         B     LEN$DONE
*
LEN$COMP LH    R10,LEN$LEN
LEN$LOOP LTR   R10,R10                 END OF RECORD
         BNP   LEN$EOR                 YES
         XR    R11,R11                 GET # BLANKS
         IC    R11,3(R7)                
         AR    R4,R11                  BUMP USER BFR PTR.
         IC    R11,2(R7)               GET DATA LENGTH
         EX    R11,LEN$MVC1            MOVE DATA TO USER BUFFER 
         LA    R4,1(R11,R4)            BUMP USER DATA BFR PTR
         LA    R11,3(R11)              BUMP LENGTH TO INCLUDE HDR.         
         AR    R7,R11                  BUMP DATA BUFFER PTR.
         SR    R10,R11                 DECR. RECORD LENGTH
         B     LEN$LOOP                & LOOP
*         
LEN$EOR  XR    R0,R0                   SHOW NOT END OF FILE
         B     LEN$DONE
*             
LEN$EOF  LA    R0,1                    SHOW END OF FILE
*
LEN$DONE L     R13,LEN$SAVE+4          RESTORE CALLER'S R13
         ST    R0,SA$R0                RETURN R1        
         RETURN (14,12)
*
LEN$SAVE DS    18F
LEN$LEN  DS    H
LEN$MVC  MVC   0(0,R4),2(R7)                        
LEN$MVC1 MVC   0(0,R4),4(R7)  
         DROP  R2,R7                      
************
*
* READ THE DIRECTORY BLOCK WHOSE BLOCK NUMBER IS GIVEN BY LF$DBLK
*   R0  = RETURNS ERROR CODE
*   R2  = COVER ADDRESS
*   R3  = DTF COVER
*   R4  = LIBRARY BLOCK HEADER COVER
*   R5  = PARAMETER PACKET ADDRESS (SET BY CALLER)
*   R6  = PCA COVER
*   R14 = RETURN ADDRESS
*   R15 = ENTRY POINT
*
************
         USING L$BLKHDR,R4
         USING PCA,R6
L$DGET   SAVE  (14,12),COVER=2,SA=L$SAVE
*         
         LA    R3,L$DTF                COVER THE DTF   
         LA    R6,L$DIRPCA             COVER DIRECTORY PCA
         L     R4,LF$DBFR              COVER THE I/O BUFFER
         LA    R11,L$GETERR            SET ERROR ADDRESS
         ST    R11,DC$ERCD
         L     R11,LF$DBLK             SET THE BLOCK # TO READ
         ST    R11,PC$PCAID
         ST    R4,PC$A1F               SET I/O BFR ADDRESS
         MVI   PC$IOCNT,1              SET TO READ 1 BLOCK
         GET   L$DTF,L$DIRPCA          READ THE BLOCK
         XC    FTEMP,FTEMP             CONFIRM THAT BLOCK JUST READ IS
         MVC   FTEMP+1(3),LBH$NUM      THE ONE WE ASKED FOR
         C     R11,FTEMP
         BNE   L$BADBLK                OOPS!
         XR    R11,R11                 GET BLOCK SIZE
         IC    R11,LBH$LEN
         LA    R11,LBH$SIZE(R11)       ADD HEADER SIZE TO GET ACTUAL
         STH   R11,LF$DLEN             SAVE IT
*
         L     R13,SA$BLNK             RESTORE R13
         XC    SA$R0,SA$R0             CLEAR ERROR CODE
         RETURN (14,12)                RESTORE REGS & RETURN
         DROP  R2,R4,R6
************
*
* WRITE THE DIRECTORY BLOCK WHOSE BLOCK NUMBER IS GIVEN BY LF$DBLK
*   R0  = RETURNS ERROR CODE
*   R2  = COVER ADDRESS
*   R3  = DTF COVER
*   R4  = LIBRARY BLOCK HEADER COVER
*   R5  = PARAMETER PACKET ADDRESS (SET BY CALLER)
*   R6  = PCA COVER
*   R14 = RETURN ADDRESS
*   R15 = ENTRY POINT
*
************
         USING L$BLKHDR,R4
         USING PCA,R6
L$DPUT   SAVE  (14,12),COVER=2,SA=L$SAVE
*         
         LA    R3,L$DTF                COVER THE DTF   
         LA    R6,L$DIRPCA             COVER DIRECTORY PCA
         L     R4,LF$DBFR              COVER THE I/O BUFFER
         LA    R11,L$GETERR            SET ERROR ADDRESS
         ST    R11,DC$ERCD
         L     R11,LF$DBLK             SET THE BLOCK # TO READ
         ST    R11,PC$PCAID
         ST    R4,PC$A1F               SET I/O BFR ADDRESS
         MVI   PC$IOCNT,1              SET TO READ 1 BLOCK
         PUT   L$DTF,L$DIRPCA          WRITE THE BLOCK
*
         L     R13,SA$BLNK             RESTORE R13
         XC    SA$R0,SA$R0             CLEAR ERROR CODE
         RETURN (14,12)                RESTORE REGS & RETURN
         DROP  R2,R4,R6
************
*
* READ THE DATA (ELEMENT) BLOCK WHOSE BLOCK NUMBER IS GIVEN BY LF$EBLK
*   R0  = RETURNS ERROR CODE
*   R2  = COVER ADDRESS
*   R3  = DTF COVER
*   R4  = LIBRARY BLOCK HEADER COVER
*   R5  = PARAMETER PACKET ADDRESS (SET BY CALLER)
*   R6  = PCA COVER
*   R14 = RETURN ADDRESS
*   R15 = ENTRY POINT
*
************
         USING L$BLKHDR,R4
         USING PCA,R6
L$EGET   SAVE  (14,12),COVER=2,SA=L$SAVE
*         
         LA    R3,L$DTF                COVER THE DTF   
         LA    R6,L$DTAPCA             COVER DATA PCA
         L     R4,LF$EBFR              COVER THE I/O BUFFER
         LA    R11,L$GETERR            SET ERROR ADDRESS
         ST    R11,DC$ERCD
         L     R11,LF$EBLK             SET THE BLOCK # TO READ
         ST    R11,PC$PCAID
         ST    R4,PC$A1F               SET I/O BFR ADDRESS
         MVI   PC$IOCNT,1              SET TO READ 1 BLOCK
         GET   L$DTF,L$DTAPCA          READ THE BLOCK
         XC    FTEMP,FTEMP             CONFIRM THAT BLOCK JUST READ IS
         MVC   FTEMP+1(3),LBH$NUM      THE ONE WE ASKED FOR
         C     R11,FTEMP
         BNE   L$BADBLK                OOPS!
         XR    R11,R11                 GET BLOCK SIZE
         IC    R11,LBH$LEN
         LA    R11,LBH$SIZE(R11)       ADD HEADER SIZE TO GET ACTUAL
         STH   R11,LF$ELEN             SAVE IT
*
         L     R13,SA$BLNK             RESTORE CALLER'S R13
         XC    SA$R0,SA$R0             CLEAR ERROR CODE
         RETURN (14,12)                RESTORE REGS & RETURN
         DROP  R2,R4,R6         
************
*
* ACQUIRE THE LIBRARY I/O LOCK
*
*   R11 = COVER
*   R14 = RETURN ADDRESS
*
************
L$LOCK   BALR  R11,0
         USING *,R11
L$LOOP   TS    L$TS                    TRY TO SET THE LOCK
         BZ    LL$OK                   SUCCESS
         SETIME 10,WAIT,M              FAIL, WAIT A BIT
         B     L$LOOP                  & TRY AGAIN
LL$OK    BR    R14
         DROP  R11                           
************
*
* CHECK THAT THE LIBRARY LOCK IS SET. IF LOCK IS NOT SET, SET
* THE ERROR CODE IN R0 AND RETURN TO THE ORIGINAL CALLER.
*
*   R0  = RETURNS ERROR CODE
*   R11 = COVER
*   R14 = RETURN ADDRESS
*
************
L$CKLOCK BALR  R11,0
         USING *,R11
         CLI   L$TS,X'FF'              IS FILE OPEN?
         BE    LCL$LOK                 YES, CONTINUE
         LA    R0,X'13'                SET 'FILE NOT OPEN' ERR CODE
         ST    R0,SA$R0
         XC    SA$R1,SA$R1             CLEAR PTR TO DIR. ENTRY
         RETURN (14,12)                RETURN TO CALLER
*
LCL$LOK  BR    R14         
         DROP  R11                           
************
*
* ERROR OCCURRED WHEN ATTEMPTING TO OPEN THE LIBRARY
*
************         
L$OPNERR BALR  R2,0
         USING *,R2
         LA    R1,OP$ERCD              GET ERROR CODE TO ERR MSG
         BAL   R14,L$ER2HEX
         ST    R0,SA$R0                RETURN R0
         OPR   OP$ERR,L'OP$ERR         SHOW MSG TO OPERATOR
         RETURN (14,12)
************
*
* ERROR OCCURRED WHEN ATTEMPTING TO READ THE LIBRARY
*
************         
L$GETERR BALR  R2,0
         USING *,R2
         LA    R1,GT$ERCD              GET ERROR CODE TO ERR MSG
         BAL   R14,L$ER2HEX
         L     R13,SA$BLNK             RESTORE R13
         ST    R0,SA$R0                RETURN R0
         OPR   GT$ERR,L'GT$ERR         SHOW MSG TO OPERATOR
         RETURN (14,12)
************
*
* CONVERT THE CURRENT ERROR CODE (DC$ERCD) TO A 2 DIGIT HEX NUMBER.
* ALSO PUTS DC$ERCD INTO R0 FOR RETURN TO CALLER.
*   R1  = PTR TO RESULT BUFFER
*   R11 = COVER
*   R14 = RETURN ADDRESS
*
************
         DS    0H
L$ER2HEX BALR  R11,0
         USING *,R11
         UNPK  EH$TEMP,DC$ERCD(2)      SEPARATE HEX DIGITS INTO EHTEMP
         TR    EH$TEMP(2),TRHEXB       XLATE TO HEX
         MVC   0(2,R1),EH$TEMP         MOVE TO RESULT BUFFER
         XR    R0,R0                   GET ERR CODE TO R0
         IC    R0,DC$ERCD
         BR    R14                     RETURN
************
         DS    0H
L$ERROR  BALR  R2,0
         USING *,R2
         OPR   OOPS,L'OOPS
         EOJ
*
L$BADBLK BALR  R2,0
         USING *,R2
         OPR   BADBLK,L'BADBLK
         CANCEL 99         
*
L$DTF    DTFPF PCA1=L$DIRPCA,                                          X
               PCA2=L$DTAPCA,                                          X
               PCA3=L$BLKPCA,                                          X
               ERROR=L$ERROR,                                          X
               WAIT=YES
*
L$DIRPCA PCA   BLKSIZE=256,                                            X
               IOAREA1=DUMMY,                                          X
               LACE=18
*
L$DTAPCA PCA   BLKSIZE=256,                                            X
               IOAREA1=DUMMY,                                          X
               LACE=18
*
L$BLKPCA PCA   BLKSIZE=256,                                            X
               IOAREA1=DUMMY,                                          X
               LACE=18
*
DUMMY    EQU   *                       DUMMY I/O AREA
L$SAVE   DS    18F
L$TS     DS    XL1                     LOCK TO MAKE LIB IO SGL THREAD
OP$ERR   DC    C'OPEN ERROR DM  '
OP$ERCD  EQU   OP$ERR+13,2
GT$ERR   DC    C'READ ERROR DM  '
GT$ERCD  EQU   GT$ERR+13,2
EH$TEMP  DS    CL3
TRHEX    DC    C'0123456789ABCDEF'
TRHEXB   EQU   TRHEX-X'F0'
OOPS     DC    C'OOPS!'
BADBLK   DC    C'WRONG BLOCK NUMBER'
FTEMP    DS    F
*
         END  
// FIN                      