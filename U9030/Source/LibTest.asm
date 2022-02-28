LIBTST   START
*
* AN ATTEMPT TO WRITE A SUBROUTINE LIBRARY FOR OS/3 LIBARY FILES
*
         PRINT NOGEN
         RGEQU 
         DTFDM
*         
         BALR  R2,0
         USING *,R2
         USING L$DIRENT,R1
         LA    R13,REGSAV              POINT TO REGISTER SAVE AREA
*
         OPEN  PRNTR  
         LA    R1,FNAME                POINT TO FILE NAME
         L     R15,=A(L$OPEN)          TRY TO OPEN IT
         BALR  R14,R15
* 
GETFUNC  OPR   RQFUNC,L'RQFUNC,,REPLY,                                 X
               OPREPLY,L'OPREPLY
         CLC   OPREPLY(4),=C'EOJ '
         BE    EOJ
         CLC   OPREPLY(4),=C'DIR '
         BE    DODIR
         CLC   OPREPLY(5),=C'LIST '
         BE    DOLIST
         OPR   BADFUNC,L'BADFUNC
         B     GETFUNC
*         
DODIR    BAL   R14,PRTDIR              PRINT DIRECTORY OF THE LIBRARY
         B     GETFUNC
*
DOLIST   BAL   R14,PRTSRC              PRINT A SOURCE ELEMENT
         B     GETFUNC         
*         
EOJ      L     R15,=A(L$CLOSE)         CLOSE THE FILE
         BALR  R14,R15
         CLOSE PRNTR
         EOJ
************
PERROR   OPR   PERR,L'PERR
         CANCEL 01
************
*
* PRINT A DIRECTORY LISTING OF A LIBRARY
*   R14 = RETURN ADDRESS
*
************                   
PRTDIR   STM   R14,R12,12(R13)
         ST    R13,REGSAV2+4
         LA    R13,REGSAV2
*         
         L     R15,=A(L$DFIRST)        GET FIRST DIRECTORY ENTRY        
         BALR  R14,R15
LOOP     CLI   LDE$TYPE,LET$EOF        EOF?
         BE    PDDONE                  YES
         BAL   R12,DSHOW
         L     R15,=A(L$DNEXT)         GET NEXT DIRECTORY ENTRY
         BALR  R14,R15
         B     LOOP
*       
PDDONE   CNTRL PRNTR,SK,7              FORM FEED
         L     R13,REGSAV2+4
         LM    R14,R12,12(R13)
         BR    R14
************
*
* PRINT A SOURCE ELEMENT
*   R14 = RETURN ADDRESS
*
************
PRTSRC   STM   R14,R12,12(R13)
         ST    R13,REGSAV2+4
         LA    R13,REGSAV2
*
         OPR   RQELE,L'RQELE,,REPLY,                                   X
               OPREPLY,L'OPREPLY
         LA    R0,LET$SRC              FIND DIR. ENTRY FOR SRC MODULE
         LA    R1,OPREPLY
         L     R15,=A(L$DFIND)
         BALR  R14,R15
         LTR   R1,R1                   WAS MODULE FOUND?
         BNE   PSGOTIT                 YES
         OPR   BADMOD,L'BADMOD         TELL OPR MODULE NOT FOUND
         B     PSDONE                  & QUIT
*
PSGOTIT  LA    R0,SRCBFR               POINT TO MY BUFFER
         L     R15,=A(L$EFIRST)        GET 1ST LINE OF SRC MODULE
         BALR  R14,R15
PSLOOP   LTR   R1,R1                   END OF FILE?
         BNZ   PSDONE                  YES
         MVI   PBFR,C' '               CLEAR PRINT BUFFER
         MVC   PBFR+1(L'PBFR-1),PBFR
         MVC   PBFR,SRCBFR             SEND SOURCE TO PRINTER
         PUT   PRNTR
         LA    R0,SRCBFR               POINT TO MY BUFFER
         L     R15,=A(L$ENEXT)         GET NEXT LINE
         BALR  R14,R15
         B     PSLOOP                  & LOOP
         
PSDONE   CNTRL PRNTR,SK,7              FORM FEED
         L     R13,REGSAV2+4
         LM    R14,R14,12(R13)
         BR    R14                         
************
*
* DISPLAY A DIRECTORY ENTRY ON THE PRINTER.
*   R1  = PTR TO DIRECTORY ENTRY
*   R12 = RETURN ADDRESS
*
************
*         
DSHOW    MVI   PBFR,C' '               CLEAR BUFFER
         MVC   PBFR+1(L'PBFR-1),PBFR
         MVC   BNAME,LDE$NAME          MOVE NAME TO BUFFER
         XC    DTEMP,DTEMP             CVT BLOCK NUM TO DECIMAL
         MVC   DTEMP+1(3),LDE$BLK
         L     R11,DTEMP
         CVD   R11,DTEMP
         MVC   BBLK,EDBLK              FORMAT IT INTO THE BUFFER
         ED    BBLK,DTEMP+4
         SR    R11,R11                 CVT REC NUM TO DECIMAL
         IC    R11,LDE$REC
         CVD   R11,DTEMP
         MVC   BREC,EDREC              FORMAT IT INTO THE BUFFER
         ED    BREC,DTEMP+6
         LA    R11,DTYPE               LOOK UP DIR ENTRY TYPE DESC.
DLKUP    CLC   0(1,R11),LDE$TYPE       ENTRY TYPE MATCH TABLE?
         BE    DGOTIT                  YES
         CLI   0(R11),X'FF'            END OF TABLE?
         BE    DGOTIT                  YES
         LA    R11,11(R11)             BUMP INDEX
         B     DLKUP                   LOOP
DGOTIT   MVC   BTYPE,1(R11)            MOVE DESC TO BUFFER         
         PUT   PRNTR
         BR    R12                                    
************
*
* LIBRARY DIRECTORY ENTRY TYPE CODE TABLE
*
* EACH TABLE ENTRY CONSISTS OF THE 1 BYTE TYPE CODE AND A 10 BYTE
* DESCRIPTION.
*
************
DTYPE    DC    YL1(LET$NULL)
         DC    CL10'DELETED'
         DC    YL1(LET$NTRY)
         DC    CL10'ENTRY'
         DC    YL1(LET$CSCT)
         DC    CL10'CSECT'
         DC    YL1(LET$OBJ)
         DC    CL10'OBJECT'
         DC    YL1(LET$PHSE)
         DC    CL10'PHASE'
         DC    YL1(LET$BGRP)
         DC    CL10'DEGIN GRP'
         DC    YL1(LET$EOF)
         DC    CL10'EOF'
         DC    YL1(LET$PNME)
         DC    CL10'PROC NAME'
         DC    YL1(LET$PROC)
         DC    CL10'PROC'
         DC    YL1(LET$SRC)
         DC    CL10'SOURCE'
         DC    YL1(LET$EGRP)
         DC    CL10'END GRP'
         DC    YL1(LET$BLK)
         DC    CL10'BLOCK'
         DC    XL1'FF'
         DC    CL10'UNKNOWN'
*
*
PRNTR    DTFPR BLKSIZE=120,                                            X
               CONTROL=YES,                                            X
               ERROR=PERROR,                                           X
               IOAREA1=PBFR,                                           X
               PRINTOV=SKIP                             
* 
         DS    0H              
PBFR     DS    CL120
         ORG   PBFR
BNAME    DS    CL8
         DS    CL1
BTYPE    DS    CL10
BBLK     DS    CL8
BREC     DS    CL4
         ORG   PBFR+L'PBFR
*  
EDBLK    DC    XL8'4020202020202021'
EDREC    DC    XL4'40202021'
*
PERR     DC    C'PRINTER ERROR'                                               
FNAME    DC    CL8'LIBIN'
DTEMP    DS    D
REGSAV   DS    18F
REGSAV2  DS    18F
*
RQFUNC   DC    C'ENTER FUNCTION'
RQELE    DC    C'ENTER MODULE NAME'
OPREPLY  DS    CL60
BADFUNC  DC    C'INVALID FUNCTION'
BADMOD   DC    C'SOURCE MODULE NOT FOUND'
SRCBFR   DS    CL256
*
         LTORG
         EJECT
************
*
* OPEN A LIBRARY FILE. 
*   R1  = PTR TO FILE NAME
*   R14 = RETURN ADDR.
*   R15 = ENTRY POINT ADDR.
*
************
         DS    0H
L$OPEN   STM   R14,R12,12(R13)         SAVE REGISTERS
         BALR  R2,0
*         
         USING *,R2
         USING DM$DSCT,R3
*         
         LA    R3,L$DTF                COVER THE DTF   
         MVC   DC$NME,0(R1)            MDFY DTF WITH FILE NAME
         LA    R11,L$OPNERR            SET ERROR ADDRESS
         ST    R11,DC$ERCD
         OPEN  L$DTF                   OPEN THE FILE
         LM    R14,R12,12(R13)
         BR    R14                     RETURN
************
*
* CLOSE A LIBRARY FILE
*   R14 = RETURN ADDR.
*   R15 = ENTRY POINT ADDR.
*
************
L$CLOSE  STM   R14,R12,12(R13)         SAVE REGISTERS
         BALR  R2,0
*
         USING *,R2
         USING DM$DSCT,R3
*         
         LA    R3,L$DTF
         SR    R11,R11                 CLEAR ERROR ADDRESS
         ST    R11,DC$ERCD
         CLOSE L$DTF
         LM    R14,R12,12(R13)
         BR    R14
************
*
* GET THE FIRST DIRECTORY ENTRY FOR THE CURRENT LIBRARY
*   R1  = RETURNS PTR TO DIRECTORY ENTRY
*   R14 = RETURN ADDRESS         
*   R15 = ENTRY POINT ADDR.
*
************
L$DFIRST STM   R14,R12,12(R13)
         BALR  R2,0
*         
         USING *,R2
*         
         LA    R11,1                   SET NEXT BLOCK ID TO 1
         ST    R11,L$DBLK
         BAL   R12,L$DGET              READ BLOCK 1
         LA    R11,LBH$SIZE            SET INITIAL BLK OFFSET
         STH   R11,L$DOFST
         LA    R1,L$DIRBFR(R11)        RETURN PTR TO DIR. ENTRY
         ST    R1,24(R13)
         LM    R14,R12,12(R13)         RESTORE REGISTERS
         BR    R14                     RETURN
************
*
* GET THE NEXT DIRECTORY ENTRY FOR THE CURRENT LIBRARY
*
*   R1  = RETURNS PTR TO DIRECTORY ENTRY
*   R14 = RETURN ADDRESS         
*   R15 = ENTRY POINT ADDR.
*
************
L$DNEXT  STM   R14,R12,12(R13)
         BALR  R2,0
*         
         USING *,R2
*         
         LH    R11,L$DOFST             GET OFFSET TO LAST DIR ENTRY
         LA    R11,LDE$SIZE(R11)       BUMP TO NEXT
         CH    R11,L$DLEN              END OF BLOCK?
         BL    LDN$BOK                 NO
         L     R11,L$DBLK              YES, BUMP BLOCK #
         LA    R11,1(R11)
         ST    R11,L$DBLK
         BAL   R12,L$DGET              READ NEXT BLOCK
         LA    R11,LBH$SIZE            SET OFFSET TO 1ST ENTRY
LDN$BOK  STH   R11,L$DOFST             SAVE NEW OFFSET         
         LA    R1,L$DIRBFR(R11)        RETURN PTR TO DIR. ENTRY
         ST    R1,24(R13)
         LM    R14,R12,12(R13)
         BR    R14  
************
*
* FIND AN ENTRY IN THE DIRECTORY
*   R1  = PTR TO ELEMENT NAME / RETURNS PTR TO DIR. ENTRY
*         ZERO IF NOT FOUND
*   R0  = ELEMENT TYPE
*   R14 = RETURN ADDRESS
*   R15 = ENTRY POINT ADDRESS                
************
L$DFIND  STM   R14,R12,12(R13)
*         
         BALR  R2,0
*
         USING *,R2
         USING L$DIRENT,R1
*         
         ST    R13,L$RGSAV2+4          SAVE CALLER'S R13
         LA    R13,L$RGSAV2            POINT TO OUR SAVE AREA
         LR    R3,R1                   SAVE PTR TO ELE. NAME
         L     R15,=A(L$DFIRST)        GET 1ST DIRECTORY ENTRY
         BALR  R14,R15
LDF$LOOP CLI   LDE$TYPE,LET$EOF        END OF FILE?
         BE    LDF$NFND                YES
         CLC   LDE$NAME,0(R3)          NAME MATCH?                  
         BNE   LDF$NEXT                NO
         SR    R11,R11                 ELE. TYPE MATCH?
         IC    R11,LDE$TYPE
         CR    R0,R11
         BE    LDF$DONE                YES
LDF$NEXT L     R15,=A(L$DNEXT)         GET NEXT DIRECTORY ENTRY
         BALR  R14,R15
         B     LDF$LOOP                LOOP
*
LDF$NFND SR    R1,R1
*                 
LDF$DONE L     R13,L$RGSAV2+4          RESTORE R13
         ST    R1,24(R13)              RETURN DIR. ENTRY ADDRESS
         LM    R14,R12,12(R13)         RESTORE REGISTERS
         BR    R14                     RETURN
************
*
* GET THE FIRST RECORD OF AN ELEMENT
*
*   R0  = PTR. TO 256 BYTE BFR
*   R1  = PTR. TO DIRECTORY ENTRY
*   R14 = RETURN ADDRESS
*   R15 = ENTRY POINT ADDR.
*
************
L$EFIRST STM   R14,R15,12(R13)
*
         BALR  R2,0
*
         USING *,R2
         USING L$DIRENT,R5
         USING L$MODHDR,R7
*         
         ST    R13,L$RGSAV2+4          SAVE CALLER'S R13
         LA    R13,L$RGSAV2            POINT TO OUR SAVE AREA
         LR    R5,R1                   SAVE PTR TO DIR. ENTRY
         LR    R6,R0                   SAVE PTR TO USER'S BUFFER
*
         XC    L$EBLK,L$EBLK           SET 1ST DATA BLOCK #
         MVC   L$EBLK+1(3),LDE$BLK
         BAL   R12,L$EGET
         
*        BUMP PAST MODULE HEADER         

         SR    R7,R7                   GET OFFSET TO MODULE HEADER
         IC    R7,LDE$REC
         STH   R7,L$EOFST              SAVE IT
         LA    R7,L$DTABFR(R7)         POINT TO MODULE HDR IN BUFFER
         SR    R11,R11                 GET MODULDE HDR LENGTH
         IC    R11,LMH$LEN
         LA    R11,2(R11)              CALC. OFFSET TO 1ST DATA RECORD
         AH    R11,L$EOFST            
         STH   R11,L$EOFST             SAVE IT.
         B     L$ENEXT1                GO GET DATA RECORD
* 
************
*
* GET THE NEXT RECORD OF AN ELEMENT
*   R0  = PTR. TO 256 BYTE BUFFER
*   R1  = RETURNS EOF FLAG (1 = EOF)
*   R14 = RETURN ADDRESS
*   R15 = ENTRY POINT ADRESS
*
************
L$ENEXT STM   R14,R15,12(R13)
*
         BALR  R2,0
*
         USING *,R2
*         
         ST    R13,L$RGSAV2+4          SAVE CALLER'S R13
         LA    R13,L$RGSAV2            POINT TO OUR SAVE AREA
         LR    R6,R0                   SAVE PTR TO USER'S BUFFER
*
L$ENEXT1 BALR  R2,0
*
         USING *,R2
         USING L$MODHDR,R7
*         
         MVI   0(R6),C' '              CLEAR THE USER'S BUFFER
         MVC   1(255,R6),0(R6)
         LH    R11,L$EOFST             GET OFFSET TO RECORD
         CH    R11,L$ELEN              END OF BLOCK?
         BL    LEN$OK                  NO
         L     R11,L$EBLK              BUMP BLOCK #
         LA    R11,1(R11)
         ST    R11,L$EBLK
         BAL   R12,L$EGET              READ THE NEXT BLOCK            
         LA    R11,LBH$SIZE            SET INITIAL OFFSET
*         
LEN$OK   LA    R7,L$DTABFR(R11)        CALC. ADDR OF RECORD
         SR    R10,R10                 GET LENGTH OF RECORD
         IC    R10,LMH$LEN
         STH   R10,LEN$LEN             SAVE IT FOR LATER
         AR    R10,R11                 CALC. OFFSET TO NEXT RECORD
         LA    R10,2(R10)
         STH   R10,L$EOFST             SAVE IT
         CLI   LMH$TYPE,X'24'          TRUNCATED?
         BE    LEN$TRNC                YES
         CLI   LMH$TYPE,X'25'          COMPRESSED
         BE    LEN$COMP                YES
         B     LEN$EOF                 IF NOT ONE OF ABOVE, END OF ELE.
*         
LEN$TRNC LH    R10,LEN$LEN             GET LENGTH OF DATA
         AH    R10,=H'-1'              MAKE ZERO RELATIVE
         EX    R10,LEN$MVC             MOVE DATA TO USER BUFFER
         SR    R1,R1                   SHOW NOT END OF FILE
         B     LEN$DONE
*
LEN$COMP LH    R10,LEN$LEN
LEN$LOOP LTR   R10,R10                 END OF RECORD
         BNP   LEN$EOR                 YES
         SR    R11,R11                 GET # BLANKS
         IC    R11,3(R7)                
         AR    R6,R11                  BUMP USER BFR PTR.
         IC    R11,2(R7)               GET DATA LENGTH
         EX    R11,LEN$MVC1            MOVE DATA TO USER BUFFER 
         LA    R6,1(R11,R6)            BUMP USER DATA BFR PTR
         LA    R11,3(R11)              BUMP LENGTH TO INCLUDE HDR.         
         AR    R7,R11                  BUMP DATA BUFFER PTR.
         SR    R10,R11                 DECR. RECORD LENGTH
         B     LEN$LOOP                & LOOP
*         
LEN$EOR  SR    R1,R1                   SHOW NOT END OF FILE
         B     LEN$DONE
*             
LEN$EOF  LA    R1,1                    SHOW END OF FILE
*
LEN$DONE L     R13,L$RGSAV2+4          RESTORE CALLER'S R13
         ST    R1,24(R13)              RETURN R1        
         LM    R14,R12,12(R13)
         BR    R14
*
LEN$MVC  MVC   0(0,R6),2(R7)                        
LEN$MVC1 MVC   0(0,R6),4(R7)                        
************
*
* READ THE DIRECTORY BLOCK WHOSE BLOCK NUMBER IS GIVEN BY L$DBLK
*   R10 = COVER ADDRESS
*   R12 = RETURN ADDRESS
*
************
L$DGET   BALR  R10,0
*
         USING *,R10
         USING L$BLKHDR,R3
         USING DM$DSCT,R4
*         
         
         ST    R13,L$REGSAV+4          SAVE CALLER'S R13
         LA    R13,L$REGSAV            POINT TO IOCS SAVE AREA
         LA    R3,L$DIRBFR             COVER THE I/O BUFFER
         LA    R4,L$DTF                COVER THE DTF   
         LA    R11,L$GETERR            SET ERROR ADDRESS
         ST    R11,DC$ERCD
         L     R11,L$DBLK              SET THE BLOCK # TO READ
         ST    R11,L$DIRPCA
         GET   L$DTF,L$DIRPCA          READ THE BLOCK
         XC    FTEMP,FTEMP             CONFIRM THAT BLOCK JUST READ IS
         MVC   FTEMP+1(3),LBH$NUM      THE ONE WE ASKED FOR
         C     R11,FTEMP
         BNE   L$BADBLK                OOPS!
         SR    R11,R11                 GET BLOCK SIZE
         IC    R11,LBH$LEN
         LA    R11,LBH$SIZE(R11)       ADD HEADER SIZE TO GET ACTUAL
         STH   R11,L$DLEN              SAVE IT
*
         L     R13,L$REGSAV+4          RESTORE CALLER'S R13
         BR    R12         
************
*
* READ THE DATA (ELEMENT) BLOCK WHOSE BLOCK NUMBER IS GIVEN BY L$EBLK
*   R10 = COVER ADDRESS
*   R12 = RETURN ADDRESS
*
************
L$EGET   BALR  R10,0
*
         USING *,R10
         USING L$BLKHDR,R3
         USING DM$DSCT,R4
*         
         
         ST    R13,L$REGSAV+4          SAVE CALLER'S R13
         LA    R13,L$REGSAV            POINT TO IOCS SAVE AREA
         LA    R3,L$DTABFR             COVER THE I/O BUFFER
         LA    R4,L$DTF                COVER THE DTF   
         LA    R11,L$GETERR            SET ERROR ADDRESS
         ST    R11,DC$ERCD
         L     R11,L$EBLK              SET THE BLOCK # TO READ
         ST    R11,L$DTAPCA
         GET   L$DTF,L$DTAPCA          READ THE BLOCK
         XC    FTEMP,FTEMP             CONFIRM THAT BLOCK JUST READ IS
         MVC   FTEMP+1(3),LBH$NUM      THE ONE WE ASKED FOR
         C     R11,FTEMP
         BNE   L$BADBLK                OOPS!
         SR    R11,R11                 GET BLOCK SIZE
         IC    R11,LBH$LEN
         LA    R11,LBH$SIZE(R11)       ADD HEADER SIZE TO GET ACTUAL
         STH   R11,L$ELEN              SAVE IT
*
         L     R13,L$REGSAV+4          RESTORE CALLER'S R13
         BR    R12         
************
*
* ERROR OCCURRED WHEN ATTEMPTING TO OPEN THE LIBRARY
*
************         
L$OPNERR BALR  R2,0
         USING *,R2
         LA    R1,OP$ERCD              GET ERROR CODE TO ERR MSG
         BAL   R13,L$ER2HEX
         OPR   OP$ERR,L'OP$ERR         SHOW MSG TO OPERATOR
         CANCEL 81                     QUIT WITH PREJUDICE
************
*
* ERROR OCCURRED WHEN ATTEMPTING TO READ THE LIBRARY
*
************         
L$GETERR BALR  R2,0
         USING *,R2
         LA    R1,GT$ERCD              GET ERROR CODE TO ERR MSG
         BAL   R13,L$ER2HEX
         OPR   GT$ERR,L'GT$ERR         SHOW MSG TO OPERATOR
         CANCEL 82                     QUIT WITH PREJUDICE
************
*
* CONVERT THE CURRENT ERROR CODE (DC$ERCD) TO A 2 DIGIT HEX NUMBER
*   R1  = PTR TO RESULT BUFFER
*   R13 = RETURN ADDRESS
*
************
         DS    0H
L$ER2HEX BALR  R3,0
         USING *,R3
         UNPK  EH$TEMP,DC$ERCD(2)      SEPARATE HEX DIGITS INTO EHTEMP
         TR    EH$TEMP(2),TRHEXB       XLATE TO HEX
         MVC   0(2,R1),EH$TEMP         MOVE TO RESULT BUFFER
         BR    R13                     RETURN
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
               IOAREA1=L$DIRBFR,                                       X
               LACE=18
*
L$DTAPCA PCA   BLKSIZE=256,                                            X
               IOAREA1=L$DTABFR,                                       X
               LACE=18
*
L$BLKPCA PCA   BLKSIZE=256,                                            X
               IOAREA1=L$BLKBFR,                                       X
               LACE=18
L$DIRBFR DS    XL256
L$DTABFR DS    XL256
L$BLKBFR DS    XL256
*
*
L$REGSAV DS    18F
L$RGSAV2 DS    18F
OP$ERR   DC    C'OPEN ERROR DM  '
OP$ERCD  EQU   OP$ERR+13,2
GT$ERR   DC    C'READ ERROR DM  '
GT$ERCD  EQU   GT$ERR+13,2
EH$TEMP  DS    CL3
TRHEX    DC    C'0123456789ABCDEF'
TRHEXB   EQU   TRHEX-X'F0'
OOPS     DC    C'OOPS!'
BADBLK   DC    C'WRONG BLOCK NUMBER'
LEN$LEN  DS    H
FTEMP    DS    F
************
*
* LIBRARY OBJECT CONTROL FIELDS
*
************
L$DBLK   DS    F                       BLOCK # OF CURRENT DIR. BLOCK
L$DOFST  DS    H                       OFFSET TO CURRENT DIR. ENTRY
L$DLEN   DS    H                       LENGTH OF CURRENT DIR. BLOCK
L$EBLK   DS    F                       BLOCK # OF CURRENT ELE. BLOCK
L$EOFST  DS    H                       OFFSET TO CURRENT ELE. ENTRY
L$ELEN   DS    H                       LENGTH OF CURRENT ELE. BLOCK
************
*
* LIBRARY BLOCK HEADER DEFINITIONS
*
************
L$BLKHDR DSECT
LBH$NUM  DS    XL3
LBH$LEN  DS    XL1
LBH$CHK  DS    XL1
LBH$SIZE EQU   *-L$BLKHDR
************
*
* LIBRARY DIRECTORY ENTRY
*
************
L$DIRENT DSECT
LDE$NAME DS    CL8
LDE$TYPE DS    XL1
LDE$BLK  DS    XL3
LDE$REC  DS    XL1
LDE$SIZE EQU   *-L$DIRENT
************
*
* LIBRARY MODULE HEADER 
*
************
L$MODHDR DSECT
LMH$LEN  DS    XL1
LMH$TYPE DS    XL1
************
*
* LIBRARY ELEMENT TYPES
*
************
LET$NULL EQU   X'00'
LET$NTRY EQU   X'04'
LET$CSCT EQU   X'08'
LET$OBJ  EQU   X'80'
LET$PHSE EQU   X'90'
LET$BGRP EQU   X'A0'
LET$EOF  EQU   X'A1'
LET$PNME EQU   X'A2'
LET$PROC EQU   X'A3'
LET$SRC  EQU   X'A4'
LET$EGRP EQU   X'A8'
LET$BLK  EQU   X'B0'
*
         END  
// FIN                      