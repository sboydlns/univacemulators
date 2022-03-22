         TITLE '/DELETE COMMAND FOR BOYD''S BEM SIMULATOR'
BEMDEL   START
***********************************************************************
*                                                                     *
* AN IMPLEMENTATION OF THE /DELETE COMMAND FOR BOYD'S BEM SIMULATOR.  *
*                                                                     *
***********************************************************************
*
         PRINT NOGEN
         SUPEQU REGS=YES
         SA$DSECT
         ZM#DPIB
         ZM#DIMH
ZA#IMSG  DS    CL2048         
BEMDEL   CSECT 
         BEMDSCTS
*         
         SAVE  (14,12),COVER=2
************
* R2  = COVER
* R3  = PIB
* R4  = IMA
* R5  = WORK AREA
* R6  = OMA
* R7  = CDA
* R14 = RETURN ADDRESS
************         
         USING LOWMEM,R0
         USING ZA#DPIB,R3
         USING ZA#IMH,R4
         USING WORKAREA,R5
         USING ZA#OMH,R6
         USING CDA,R7
         USING LIBFIL,R8
         USING SA$DSECT,R13
*         
         L     R3,0(R1)                COVER THE PARAMETERS
         L     R4,4(R1)
         L     R5,8(R1)
         L     R6,12(R1)
         L     R7,16(R1)
         LA    R8,LIBPKT               COVER LIB RTNS PARAM PKT
         ST    R13,SAVAREA+4
         LA    R13,SAVAREA
         BAL   R14,INIT                INIT. LIB. PARAM PACKET
*
* PARSE COMMAND LINE
*
         LH    R0,ZA#ITL               SET UP GETOKEN PARAMS
         LA    R1,ZA#IMSG
         L     R15,LM$GTKN
         LA    R11,ELENAME             BURN '/DELETE'
         BALR  R14,R15    
         LA    R11,ELENAME             GET THE ELEMENT NAME
         BALR  R14,R15    
         LA    R11,FNAME               GET THE FILE NAME
         BALR  R14,R15    
         LA    R11,VSN                 GET VOLUME ID
         BALR  R14,R15
         LA    R11,ELETYP              GET ELEMENT TYPE
         BALR  R14,R15
*
* VALIDATE PARAMS.
*
         CLI   ELENAME,C' '            ELEMENT NAME BLANK?
         BE    NOELE                   YES, OOPS!         
         CLI   FNAME,C' '              FILE NAME BLANK?
         BE    NOFIL                   YES, OOPS!
         CLI   ELETYP,C' '             ELEMENT TYPE BLANK?
         BNE   CHKTYP                  NO, CONTINUE
         MVC   ELETYP,VSN              COPY VOLUME ID TO ELE TYPE
CHKTYP   CLI   ELETYP,C' '             ELEMENT TYPE STILL BLANK?
         BE    BADTYP                  YES, OOPS
         CLI   ELETYP,C'O'             OBJECT ELEMENT?
         BNE   CHKLOD                  NO, CONTINUE
         MVI   LIBTYP,LET$OBJ          YES, ALL GOOD
         B     CHKELE                 
CHKLOD   CLI   ELETYP,C'L'             LOAD ELEMENT?
         BNE   CHKGRP                  NO, CONTINUE
         MVI   LIBTYP,LET$PHSE         YES, ALL GOOD
         B     CHKELE                 
CHKGRP   CLI   ELETYP,C'G'             GROUP HEADER?     
         BNE   CHKSRC                  NO, CONTINUE
         MVI   LIBTYP,LET$BGRP         YES,ALL GOOD
         B     CHKELE                 
CHKSRC   CLI   ELETYP,C'S'             SOURCE ELEMENT?
         BNE   CHKPROC                 NO, CONTINUE
         MVI   LIBTYP,LET$SRC          YES, ALL GOOD
         B     CHKELE                 
CHKPROC  CLI   ELETYP,C'P'             PROC ELEMENT?
         BNE   CHKMAC                  NO, CONTINUE
         MVI   LIBTYP,LET$PROC         YES, ALL GOOD
         B     CHKELE                 
CHKMAC   CLI   ELETYP,C'M'             MACRO ELEMENT?
         BNE   BADTYP                  NO, OOPS!
         MVI   LIBTYP,LET$PROC         YES, ALL GOOD         
*
* SEE IF ELEMENT/FILE IS VALID
*         
CHKELE   MVC   LF$NAME,FNAME           SET UP PARAM PKT
         MVC   LF$ELE,ELENAME
         MVC   LF$ETYP,LIBTYP
         LA    R1,LIBPKT               POINT TO PARAMS.
         L     R15,LM$LOPN             TRY TO OPEN FILE
         BALR  R14,R15
         LTR   R0,R0                   SUCCESS?
         BNZ   BADFIL                  NO
         L     R15,LM$LDEL             TRY TO DELETE IT
         BALR  R14,R15
         LTR   R0,R0                   ERROR CODE = 0?
         BNZ   BADELE                  NO, OOPS!
         LTR   R1,R1                   ELEMENT FOUND?
         BZ    BADELE                  NO, OOPS
         CLI   ELETYP,C'G'             DELETING A GROUP?
         BNE   DDONE                   NO, WE'RE DONE
         MVI   LF$ETYP,LET$EGRP        YES, DELETE END OF GROUP MARKER
         L     R15,LM$LDEL             TRY TO DELETE IT
         BALR  R14,R15
         LTR   R0,R0                   ERROR CODE = 0?
         BNZ   BADELE                  NO, OOPS!
         LTR   R1,R1                   ELEMENT FOUND?
         BZ    BADELE                  NO, OOPS
*         
DDONE    MVC   ZA#OMSG(EOJMSGL),EOJMSG MOVE MSG TO OMA
         LA    R11,EOJMSGL             SET MSG LENGTH
         STH   R11,ZA#OTL
         MVC   OELENAM,ELENAME         MOVE ELEMENT NAME TO OMA
         MVI   ZA#PSIND,ZA#PSNN        SET NORMAL TERMINATION
*         
DONE     L     R15,LM$LCLS             CLOSE THE LIBRARY
         BALR  R14,R15
         L     R13,SA$BLNK             RESTORE R13
         RETURN (14,12)                RESTORE REGS & RETURN TO MON.
************
*
* INITIALIZE PARAMETER PACKET IN WORK AREA
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
* SEND FILE NAME MISSING ERROR
*
************
NOFIL    MVC   ZA#OMSG(FILMSNGL),FILMSNG MOVE MSG TO OMA
         LA    R11,FILMSNGL             SET MSG LENGTH
         STH   R11,ZA#OTL
         MVI   ZA#PSIND,ZA#PSNN        SET NORMAL TERMINATION
         B     DONE 
************
*
* SEND INVALID FILE NAME ERROR
*
************
BADFIL   MVC   ZA#OMSG(INVFILL),INVFIL  MOVE MSG TO OMA
         LA    R11,INVFILL              SET MSG LENGTH
         STH   R11,ZA#OTL
         MVI   ZA#PSIND,ZA#PSNN        SET NORMAL TERMINATION
         B     DONE 
************
*
* SEND ELEMENT NAME MISSING ERROR
*
************
NOELE    MVC   ZA#OMSG(ELEMSNGL),ELEMSNG MOVE MSG TO OMA
         LA    R11,ELEMSNGL             SET MSG LENGTH
         STH   R11,ZA#OTL
         MVI   ZA#PSIND,ZA#PSNN        SET NORMAL TERMINATION
         B     DONE 
************
*
* SEND INVALID ELEMENT NAME ERROR
*
************
BADELE   MVC   ZA#OMSG(INVELEL),INVELE  MOVE MSG TO OMA
         LA    R11,INVELEL              SET MSG LENGTH
         STH   R11,ZA#OTL
         MVI   ZA#PSIND,ZA#PSNN        SET NORMAL TERMINATION
         B     DONE 
************
*
* SEND INVALID ELEMENT TYPE ERROR
*
************
BADTYP   MVC   ZA#OMSG(TYPERRL),TYPERR MOVE MSG TO OMA
         LA    R11,TYPERRL             SET MSG LENGTH
         STH   R11,ZA#OTL
         MVI   ZA#PSIND,ZA#PSNN        SET NORMAL TERMINATION
         B     DONE 
************
*
* SEND LIBRARY I/O ERROR
*
************
BADLIB   MVC   ZA#OMSG(LIBERRL),LIBERR MOVE MSG TO OMA
         LA    R11,LIBERRL             SET MSG LENGTH
         STH   R11,ZA#OTL
         MVI   ZA#PSIND,ZA#PSNN        SET NORMAL TERMINATION
         B     DONE 
************                  
FILMSNG  ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'27'                 ERASE PROTECTED
         DC    CL1'M'
         DC    XL1'1E'                 SOE
         DC    C'FILE NAME MISSING'
         ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'1E'                 SOE
FILMSNGL EQU   *-FILMSNG
************                  
INVFIL  ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'27'                 ERASE PROTECTED
         DC    CL1'M'
         DC    XL1'1E'                 SOE
         DC    C'FILE NAME NOT FOUND'
         ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'1E'                 SOE
INVFILL  EQU   *-INVFIL
************                  
ELEMSNG  ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'27'                 ERASE PROTECTED
         DC    CL1'M'
         DC    XL1'1E'                 SOE
         DC    C'ELEMENT NAME MISSING'
         ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'1E'                 SOE
ELEMSNGL EQU   *-ELEMSNG
************                  
INVELE   ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'27'                 ERASE PROTECTED
         DC    CL1'M'
         DC    XL1'1E'                 SOE
         DC    C'ELEMENT NAME / TYPE NOT FOUND'
         ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'1E'                 SOE
INVELEL  EQU   *-INVELE
************                  
TYPERR   ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'27'                 ERASE PROTECTED
         DC    CL1'M'
         DC    XL1'1E'                 SOE
         DC    C'ELEMENT TYPE MUST BE SPECIFIED (S/P/M/O/L/G)'
         ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'1E'                 SOE
TYPERRL  EQU   *-TYPERR
************                  
LIBERR   ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'27'                 ERASE PROTECTED
         DC    CL1'M'
         DC    XL1'1E'                 SOE
         DC    C'ERROR WHILE READING SOURCE ELEMENT'
         ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'1E'                 SOE
LIBERRL  EQU   *-LIBERR
************                  
EOJMSG   ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'27'                 ERASE PROTECTED
         DC    CL1'M'
         DC    XL1'1E'                 SOE
ELENAM   EQU   *-EOJMSG         
         DC    C'         DELETED'
         ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'1E'                 SOE
EOJMSGL  EQU   *-EOJMSG
************
         ZM#DOMH
ZA#OMSG  DS    CL2048                  OUTPUT MSG BFR.
OELENAM  EQU   ZA#OMSG+ELENAM,8
************
WORKAREA DSECT
DTEMP    DS    D                       DOUBLE WORD ALIGNED TEMP VAR.
CTEMP    DS    CL8
ELENAME  DS    CL8                     SOURCE ELEMENT NAME
FNAME    DS    CL8                     LIBRARY FILE NAME
VSN      DS    CL8                     VOLUME ID (NOT USED)
ELETYP   DS    CL8                     ELEMENT TYPE (S/P)
LIBTYP   DS    XL1                     LIBRARY FRIENDLY ELE TYPE
SAVAREA  DS    18F
         DS    0F
LIBPKT   DS    XL(LF$LNGTH)            LIBRARY ROUTINES PARAM PACKET
DIRBFR   DS    XL256                   LIBRARY DIR. PARTITION BFR
DTABFR   DS    XL256                   LIBRARY DATA PARTITION BFR
************
CDA      DSECT
DUMMY    DS    XL1
*
BEMDEL   CSECT
*
         END
// FIN         