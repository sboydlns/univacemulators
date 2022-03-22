         TITLE '/FSTATUS COMMAND FOR BOYD''S BEM SIMULATOR'
BEMFST   START
***********************************************************************
*                                                                     *
* AN IMPLEMENTATION OF THE /FSTATUS COMMAND FOR BOYD'S BEM SIMULATOR. *
*                                                                     *
***********************************************************************
*
         PRINT NOGEN
         SUPEQU REGS=YES
         ZM#DPIB
         ZM#DIMH
ZA#IMSG  DS    CL2048         
BEMFST   CSECT 
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
         USING LIBFIL,R8
*         
         L     R3,0(R1)                COVER THE PARAMETERS
         L     R4,4(R1)
         L     R5,8(R1)
         L     R6,12(R1)
         L     R7,16(R1)
         LA    R8,LIBPKT               COVER LIB RTNS PARAM PKT
         LA    R11,SAVAREA             SET UP NEW SAVE AREA
         ST    R13,4(R11)
         LR    R13,R11
*         
         CLI   FSTPASS,X'00'           IS THIS THE FIRST PASS?
         BNE   PASS2                   NO
         B     PASS1
*         
DONE     L     R15,LM$LCLS             CLOSE THE LIBRARY
         BALR  R14,R15
         L     R13,4(R13)              RESTORE REGISTERS
         LM    R14,R12,12(R13)
         BR    R14
*
* FIRST PASS, DECODE THE INPUT MESSAGE
*
         USING L$DIRENT,R1         
PASS1    BAL   R14,INIT                INITIALIZE PARAM PKT
         MVC   LINCNT,=H'22'           INITIALIZE THE LINE COUNTER
*
         LH    R0,ZA#ITL               SET UP GETOKEN PARAMS
         LA    R1,ZA#IMSG
         L     R15,LM$GTKN
         LA    R11,FNAME               BURN '/FSTATUS'
         BALR  R14,R15    
         LA    R11,FNAME               GET THE FILE NAME
         BALR  R14,R15    
         LA    R11,VSN                 GET VOLUME ID
         BALR  R14,R15
         LA    R11,OPTN                GET LONG OPTION
         BALR  R14,R15
         CLI   OPTN,C' '               OPTION PRESENT
         BNE   P11                     YES
         MVC   OPTN,VSN                NO, COPY VOLUME TO OPTION
P11      CLI   FNAME,C' '              FILE NAME PRESENT?
         BE    BADFIL                  NO
*         
         MVC   LF$NAME,FNAME           COPY FILE NAME TO PARAM PKT
         LA    R1,LIBPKT               TRY TO OPEN THE LIBRARY
         L     R15,LM$LOPN
         BALR  R14,R15
         LTR   R0,R0                   SUCCESS?
         BNE   OPENERR                 NO
*
         BAL   R14,INITLINE            INIT OMA WITH LINE TEMPLATE
         L     R15,LM$LDFST            GET 1ST DIRECTORY ENTRY
         BALR  R14,R15
P1LOOP   LTR   R0,R0                   SUCCESS?
         BNZ   READERR                 NO
         LTR   R1,R1                   NO ENTRY FOUND?
         BZ    P1EOJ                   YES 
         CLI   LDE$TYPE,LET$EOF        END OF FILE?
         BE    P1EOJ                   YES
         CLI   LDE$TYPE,LET$OBJ        OBJECT MODULE?
         BNE   P1L1                    NO
         MVI   0(R11),C'O'             
         B     P1CONT
P1L1     CLI   LDE$TYPE,LET$PHSE       LOAD MODULE?
         BNE   P1L2                    NO
         MVI   0(R11),C'L'
         B     P1CONT
P1L2     CLI   LDE$TYPE,LET$PROC       PROC MODULE?
         BNE   P1L3                    NO
         MVI   0(R11),C'P'
         B     P1CONT
P1L3     CLI   LDE$TYPE,LET$SRC        SOURCE MODULE?
         BNE   P1NEXT                  NO, NOT INTERESTING TYPE, SKIP
         MVI   0(R11),C'S'
P1CONT   MVI   1(R11),C'-'             MOVE SEPARATOR TO OMA
         MVC   2(8,R11),LDE$NAME       MOVE MODULE NAME TO OAM
         CLI   OPTN,C'L'               LONG LIST REQUESTED?
         BE    P1LONG                  YES
         LA    R11,11(R11)             BUMP INDEX
         BCT   R12,P1NEXT              DECR. COUNT & GET NEXT DIR ENTRY
         B     P1DONE                  COUNT EXHAUSTED, WE DONE
*         
P1NEXT   LA    R1,LIBPKT               POINT TO PARAM PKT
         L     R15,LM$LDNXT            & GET NEXT DIR ENTRY
         BALR  R14,R15
         B     P1LOOP
*
P1DONE   AI    LINCNT,-1               DECR. LINE COUNTER
         BP    P1DCONT                 > ZERO, CONTINUE
         LH    R11,ZA#OTL              CALC OFFSET INTO OMA
         LA    R11,ZA#OMSG(R11)        
         MVC   0(MOREMSGL,R11),MOREMSG MOVE 'MORE?' TO OMA
         AI    ZA#OTL,MOREMSGL         BUMP MSG LENGTH
         MVI   ZA#PSIND,ZA#PSNE        SET EXTERNAL SUCCESSION 
         MVC   ZA#PSID,=C'BEMFST'      TO OURSELVES
         MVI   FSTPASS,X'02'           SET TO CHECK 'MORE' RESPONSE
         B     DONE                    & RETURN TO MONITOR         
*                  
P1DCONT  MVI   ZA#PSIND,ZA#PSNE        SET EXTERNAL SUCCESSION
         MVC   ZA#PSID,=C'BEMFST'      TO OURSELVES
         MVI   ZA#OAUX,ZA#OCO          SET FOR CONTINUOUS OUTPUT
         MVI   FSTPASS,X'01'           SET TO SHOW NEXT LINE
         B     DONE                    & RETURN TO MONITOR
         DROP  R1
*
P1EOJ    CH    R12,=H'7'               IS LINE EMPTY?
         BL    EOJ                     NO
         XC    ZA#OTL,ZA#OTL           YES, SET MSG LENGTH TO ZERO
         
EOJ      LH    R11,ZA#OTL              CALC OFFSET INTO OMA
         LA    R11,ZA#OMSG(R11)
         MVC   0(EOJMSGL,R11),EOJMSG   APPEND BLANK LINE TO END OF OMA
         AI    ZA#OTL,EOJMSGL          BUMP MSG LENGTH
         MVI   ZA#PSIND,ZA#PSNN        SET NORMAL TERMINATION
         B     DONE
*
         USING L$SRCHDR,R1
P1LONG   ST    R1,LF$DIRP              COPY DIR. ENTRY ADDR TO PKT.
         LA    R1,LIBPKT               POINT TO PARAM. PKT.
         L     R15,LM$LMHDR            GO READ THE MODULE HEADER
         BALR  R14,R15
         LTR   R0,R0                   SUCCESS?
         BNZ   READERR                 NO
         MVC   ODCMNT,LSH$CMNT         COPY COMMENTS TO OMA
         BAL   R14,FIXCMNT             FIX NON-PRINTABLE CHARACTERS
         UNPK  CTEMP,LSH$DATE(4)       UNPACK DATE TO TEMP
         MVC   ODDAT(2),CTEMP+1        MOVE DATE TO OMA
         MVC   ODDAT+3(2),CTEMP+3
         MVC   ODDAT+6(2),CTEMP+5
         UNPK  CTEMP(6),LSH$TIME(3)    UNPACK TIME TO TEMP
         MVC   ODTIM(2),CTEMP+1        MOVE TIME TO OMA
         MVC   ODTIM+3(2),CTEMP+3
         B     P1DONE
         DROP  R1         
*                  
PASS2    CLI   FSTPASS,X'01'           DISPLAY NEXT LINE?
         BE    P2NEXT                  YES
*
         LH    R0,ZA#ITL               SET UP GETOKEN PARAMS
         LA    R1,ZA#IMSG
         L     R15,LM$GTKN
         LA    R11,YN                  GET RESPONSE TO MORE?
         BALR  R14,R15 
         CLI   YN,C'N'                 RESPONSE = 'N'
         BE    EOJ                     YES, QUIT 
         MVC   LINCNT,=H'22'           NO, INIT LINE COUNT & CONTINUE                
*
P2NEXT   LA    R1,LIBPKT               TRY TO OPEN THE LIBRARY
         L     R15,LM$LOPN
         BALR  R14,R15
         LTR   R0,R0                   SUCCESS?
         BNE   OPENERR                 NO
         BAL   R14,INITLINE            INIT OMA WITH LINE TEMPLATE
         B     P1NEXT                  GO GET NEXT DIR. ENTRY         
************
*
* INITIALIZE PARAMETER PACKET IN CDA
*   R14 = RETURN ADDRESS
*
************
INIT     LA    R11,DIRBFR              SET PTR TO DIR BUFFER
         ST    R11,LF$DBFR
         LA    R11,DTABFR              SET PTR TO DATA BUFFER
         ST    R11,LF$EBFR
         BR    R14
************
*
* INITIALIZE THE OMA WITH THE LINE TEMPLATE AND SET UP INDEX (R11)
* AND LOOP COUNT (R12).
*   R14 = RETURN ADDRESS
*
************
INITLINE CLI   OPTN,C'L'               LONG LIST REQUESTED?
         BE    ILONG                   YES
         MVC   ZA#OMSG(SMRYLINL),SMRYLIN  COPY SUMMARY TEMPLATE TO OMA
         LA    R11,SMRYLINL               SET TEXT LENGTH
         STH   R11,ZA#OTL
         MVI   OLSTRT,C' '             CLEAR LINE DATA TO SPACES
         MVC   OLSTRT+1(L'OLSTRT-1),OLSTRT
         LA    R11,OLSTRT              INIT. BFR PTR
         LA    R12,7                   INIT. LOOP COUNT
         BR    R14
*
ILONG    MVC   ZA#OMSG(DETLINL),DETLIN COPY DETAIL TEMPLATE TO OMA
         LA    R11,DETLINL             SET TEXT LENGTH
         STH   R11,ZA#OTL
         LA    R11,ODTYP               INIT. BFR PTR
         LA    R12,7                   INIT LOOP COUNT.
         BR    R14
************
*
* SEND FILE NAME ID NOT SPECIFIED ERROR
*
************
BADFIL   MVC   ZA#OMSG(NOFILL),NOFIL   MOVE MSG TO OMA
         LA    R11,NOFILL              SET MSG LENGTH
         STH   R11,ZA#OTL
         MVI   ZA#PSIND,ZA#PSNN        SET NORMAL TERMINATION
         B     DONE 
************
*
* COULD NOT OPEN THE LIBARY
*   R0 = ERROR CODE
*
************
OPENERR  MVC   ZA#OMSG(OEMSGL),OEMSG   COPY ERR MSG TEMPLATE TO OMA
         LA    R11,OEMSGL              SET TEXT LENGTH
         STH   R11,ZA#OTL
         MVC   OFNAM,FNAME             COPY FILE NAME TO ERROR MSG 
         LA    R1,OCODE                CVT ERROR CODE TO HEX                   
         BAL   R14,ER2HEX
         MVI   ZA#PSIND,ZA#PSNN        SET NORMAL TERMINATION
         B     DONE                    & RETURN TO TASKMGR
************
*
* COULD NOT READ THE LIBARY
*   R0 = ERROR CODE
*
************
READERR  MVC   ZA#OMSG(REMSGL),REMSG   COPY ERR MSG TEMPLATE TO OMA
         LA    R11,REMSGL              SET TEXT LENGTH
         STH   R11,ZA#OTL
         MVC   OFNAM1,FNAME            COPY FILE NAME TO ERROR MSG 
         LA    R1,OCODE1               CVT ERROR CODE TO HEX                   
         BAL   R14,ER2HEX
         MVI   ZA#PSIND,ZA#PSNN        SET NORMAL TERMINATION
         B     DONE                    & RETURN TO TASKMGR
************
*
* CONVERT AN ERROR CODE TO A 2 DIGIT HEX NUMBER.
*   R0  = ERROR CODE
*   R1  = PTR TO RESULT BUFFER
*   R14 = RETURN ADDRESS
*
************
ER2HEX   STC   R0,DTEMP                SEPARATE HEX DIGITS INTO CTEMP
         UNPK  CTEMP(3),DTEMP(2)       
         TR    CTEMP(2),TRHEXB         XLATE TO HEX
         MVC   0(2,R1),CTEMP           MOVE TO RESULT BUFFER
         BR    R14                     RETURN
************
*
* MAKE COMMENT PRINTABLE (IN CASE OF CORRUPT SOURCE MODULE HEADER)
*
************
FIXCMNT  LA    R11,ODCMNT              POINT TO COMMENTS
         LA    R12,L'ODCMNT            SET LOOP COUNT
FCLOOP   CLI   0(R11),C' '             CHAR < SPACE?
         BNL   FC1                     NO, OK
         MVI   0(R11),C' '             SUBSTITUTE SPACE
FC1      CLI   0(R11),C'9'             CHAR > 9?
         BNH   FC2                     NO, OK
         MVI   0(R11),C' '             SUBSTITUTE SPACE
FC2      LA    R11,1(R11)              BUMP PTR.
         BCT   R12,FCLOOP              DECR. COUNT & LOOP                             
         BR    R14                     RETURN TO CALLER
************
NOFIL    ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'27'                 ERASE PROTECTED
         DC    CL1'M'
         DC    XL1'1E'                 SOE
         DC    C'THE FILE NAME MUST BE SPECIFIED. /FSTATUS FILE-NAME'
         ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'1E'                 SOE
NOFILL   EQU   *-NOFIL
************
OEMSG    ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'27'                 ERASE PROTECTED
         DC    CL1'M'
         DC    XL1'1E'                 SOE
         DC    C'COULD NOT OPEN '
OEFNAM   EQU   *-OEMSG         
         DC    C'XXXXXXXX. ERROR DM'
OECODE   EQU   *-OEMSG         
         DC    C'XX.'
         ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'1E'                 SOE
OEMSGL   EQU   *-OEMSG
************         
REMSG    ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'27'                 ERASE PROTECTED
         DC    CL1'M'
         DC    XL1'1E'                 SOE
         DC    C'COULD NOT READ '
REFNAM   EQU   *-REMSG         
         DC    C'XXXXXXXX. ERROR DM'
RECODE   EQU   *-REMSG         
         DC    C'XX.'
         ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'1E'                 SOE
REMSGL   EQU   *-REMSG
************         
SMRYLIN  ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'27'                 ERASE PROTECTED
         DC    CL1'M'
         DC    XL1'1E'                 SOE
SMRYSTRT EQU   *-SMRYLIN
         DC    7C'X-XXXXXXXX '
SMRYLINL EQU   *-SMRYLIN
************         
DETLIN   ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'27'                 ERASE PROTECTED
         DC    CL1'M'
         DC    XL1'1E'                 SOE
DETTYP   EQU   *-DETLIN 
         DC    C'X-'
DETNAM   EQU   *-DETLIN         
         DC    C'XXXXXXXX '
DETCMNT  EQU   *-DETLIN         
         DC    C'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX '
DETDAT   EQU   *-DETLIN         
         DC    C'XX/XX/XX '
DETTIM   EQU   *-DETLIN         
         DC    C'XX:XX'        
DETLINL  EQU   *-DETLIN         
************
EOJMSG   ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
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
*
TRHEX    DC    C'0123456789ABCDEF'
TRHEXB   EQU   TRHEX-X'F0'
************
         ZM#DOMH
ZA#OMSG  DS    CL2048                  OUTPUT MSG BFR.
OFNAM    EQU   ZA#OMSG+OEFNAM,8
OCODE    EQU   ZA#OMSG+OECODE,2
OFNAM1   EQU   ZA#OMSG+REFNAM,8
OCODE1   EQU   ZA#OMSG+RECODE,2
OLSTRT   EQU   ZA#OMSG+SMRYSTRT,77
ODTYP    EQU   ZA#OMSG+DETTYP,1
ODNAM    EQU   ZA#OMSG+DETNAM,8
ODCMNT   EQU   ZA#OMSG+DETCMNT,30
ODDAT    EQU   ZA#OMSG+DETDAT,8
ODTIM    EQU   ZA#OMSG+DETTIM,5
************
WORKAREA DSECT
DTEMP    DS    D                       DOUBLE WORD ALIGNED TEMP VAR.
CTEMP    DS    CL8
FNAME    DS    CL8                     LIBRARY FILE NAME
VSN      DS    CL8                     VOLUME ID (NOT USED)
YN       DS    CL8
SAVAREA  DS    18F
************
CDA      DSECT
LINCNT   DS    H                       SCREEN LINE COUNTER
FSTPASS  DS    X                       FIRST PASS FLAG (0 = FIRST PASS)
OPTN     DS    CL8                     OPTIONS (LONG)
         DS    0D
LIBPKT   DS    XL(LF$LNGTH)            LIBRARY ROUTINES PARAM PACKET
DIRBFR   DS    XL256                   LIBRARY DIR. PARTITION BFR
DTABFR   DS    XL256                   LIBRARY DATA PARTITION BFR
*
BEMFST   CSECT
*
         END
// FIN