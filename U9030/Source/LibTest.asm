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
         LA    R1,FNAME                POINT TO FILE NAME
         LA    R15,L$OPEN              TRY TO OPEN IT
         BALR  R14,R15
* 
         LA    R15,L$DFIRST            GET FIRST DIRECTORY ENTRY        
         BALR  R14,R15
LOOP     CLI   LDE$TYPE,X'A1'          EOF?
         BE    DONE                    YES
         BAL   R14,DSHOW
         LA    R15,L$DNEXT             GET NEXT DIRECTORY ENTRY
         BALR  R14,R15
         B     LOOP
*       
DONE     LA    R15,L$CLOSE             CLOSE THE FILE
         BALR  R14,R15
         CANCEL FF                     PRODUCE A DUMP
         EOJ                     
************
*
* DISPLAY A DIRECTORY ENTRY ON THE CONSOLE
*   R1  = PTR TO DIRECTORY ENTRY
*   R14 = RETURN ADDRESS
*
************
*         
DSHOW    MVI   BFR,C' '                CLEAR BUFFER
         MVC   BFR+1(L'BFR-1),BFR
         MVC   BNAME,LDE$NAME          MOVE NAME TO BUFFER
         XC    DTEMP,DTEMP             CVT BLOCK NUM TO DECIMAL
         MVC   DTEMP+1(3),LDE$BLK
         L     R11,DTEMP
         CVD   R11,DTEMP
         MVC   EDTEMP(8),EDBLK         FORMAT IT & MOVE TO BUFFER
         ED    EDTEMP(8),DTEMP+4
         MVC   BBLK,EDTEMP
         XC    DTEMP,DTEMP             CVT REC NUM TO DECIMAL
         MVC   DTEMP+3(1),LDE$REC
         L     R11,DTEMP
         CVD   R11,DTEMP
         MVC   EDTEMP(4),EDREC         FORMAT IT & MOVE TO BUFFER
         ED    EDTEMP(4),DTEMP+6
         MVC   BREC,EDTEMP
         LA    R11,DTYPE               LOOK UP DIR ENTRY TYPE DESC.
DLKUP    CLC   0(1,R11),LDE$TYPE       ENTRY TYPE MATCH TABLE?
         BE    DGOTIT                  YES
         CLI   0(R11),X'FF'            END OF TABLE?
         BE    DGOTIT                  YES
         LA    R11,11(R11)             BUMP INDEX
         B     DLKUP                   LOOP
DGOTIT   MVC   BTYPE,1(R11)            MOVE DESC TO BUFFER         
         OPR   BFR,L'BFR               SHOW IT TO THE OPERATOR
         BR    R14                                    
************
*
* LIBRARY DIRECTORY ENTRY TYPE CODE TABLE
*
* EACH TABLE ENTRY CONSISTS OF THE 1 BYTE TYPE CODE AND A 10 BYTE
* DESCRIPTION.
*
************
DTYPE    DC    XL1'00'                  
         DC    CL10'DELETED'
         DC    XL1'04'
         DC    CL10'ENTRY'
         DC    XL1'08'
         DC    CL10'CSECT'
         DC    XL1'80'
         DC    CL10'OBJECT'
         DC    XL1'90'
         DC    CL10'PHASE'
         DC    XL1'A0'
         DC    CL10'DEGIN GRP'
         DC    XL1'A1'
         DC    CL10'EOF'
         DC    XL1'A2'
         DC    CL10'PROC NAME'
         DC    XL1'A3'
         DC    CL10'PROC'
         DC    XL1'A4'
         DC    CL10'SOURCE'
         DC    XL1'A8'
         DC    CL10'END GRP'
         DC    XL1'B0'
         DC    CL10'BLOCK'
         DC    XL1'FF'
         DC    CL10'UNKNOWN'
*
BFR      DS    CL50
         ORG   BFR
BNAME    DS    CL8
         DS    CL1
BTYPE    DS    CL10
BBLK     DS    CL8
BREC     DS    CL4
         ORG   BFR+50
*  
EDBLK    DC    XL8'4020202020202021'
EDREC    DC    XL4'40202021'
EDTEMP   DS    CL8         
                                               
FNAME    DC    CL8'LIBIN'
DTEMP    DS    D
REGSAV   DS    18F
*
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
         BAL   R12,L$DGET
         LA    R11,LBH$SIZE            ALL OK, SET INITIAL BLK OFFSET
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
         BNE   L$BADBLK
         SR    R11,R11                 GET BLOCK SIZE
         IC    R11,LBH$LEN
         LA    R11,LBH$SIZE(R11)       ADD HEADER SIZE TO GET ACTUAL
         STH   R11,L$DLEN              SAVE IT
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
************
*
* LIBRARY OBJECT CONTROL FIELDS
*
************
L$DBLK   DS    F                       BLOCK # OF CURRENT BLOCK
L$DOFST  DS    H                       OFFSET TO CURRENT DIR. ENTRY
L$DLEN   DS    H                       LENGTH OF CURRENT BLOCK
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
*
         END  
// FIN                      