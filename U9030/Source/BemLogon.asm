         TITLE '/LOGON COMMAND FOR BOYD''S BEM SIMULATOR'
BEMLGN   START
***********************************************************************
*                                                                     *
* AN IMPLEMENTATION OF THE /LOGON COMMAND FOR BOYD'S BEM SIMULATOR.   *
*                                                                     *
***********************************************************************
*
         PRINT NOGEN
         ZM#DPIB
         ZM#DIMH
ZA#IMSG  DS    CL2048         
         ZM#DOMH
ZA#OMSG  DS    CL2048                  OUTPUT MSG BFR.         
BEMLGN   CSECT         
         RGEQU
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
* R9  = USER TABLE
* R10 = TERM TABLE
* R14 = RETURN ADDRESS
************         
         BALR  R2,0                    SET COVER
         USING *,R2
         USING LOWMEM,R0
         USING ZA#DPIB,R3
         USING ZA#IMH,R4
         USING WORKAREA,R5
         USING ZA#OMH,R6
         USING USERTBL,R9
         USING TERMTBL,R10
*         
         L     R3,0(R1)                COVER THE PARAMETERS
         L     R4,4(R1)
         L     R5,8(R1)
         L     R6,12(R1)
         L     R7,16(R1)
*
* SEE IF USER IS ALREADY LOGGED ON
*
         L     R1,ZA#ISTID             FIND TERMINAL TABLE
         L     R15,LM$FTNAM         
         BALR  R14,R15
         LTR   R10,R1                  DID WE FIND IT?
         BZ    BADTRM                  NO, WTF???
         CLC   TT$USER,LM$ZERO         IS THERE ALREADY A USER?
         BNE   LOGGEDON                YES
*
* DECODE THE INPUT MESSAGE
*         
         LH    R0,ZA#ITL               SET UP GETOKEN PARAMS
         LA    R1,ZA#IMSG
         L     R15,LM$GTKN
         LA    R11,USERID              BURN '/LOGON'
         BALR  R14,R15
         LA    R11,USERID              GET USER ID
         BALR  R14,R15
         LA    R11,ACCTID              GET ACCT. ID.
         BALR  R14,R15
         LA    R11,PASSWD              GET PASSWORD
         BALR  R14,R15
*
* SEE IF WE CAN FIND THE USER IN THE USER TABLE         
*
         L     R9,LM$UFRST             POINT TO 1ST USER TABLE
LOOP     CLC   UT$ID,USERID            CHECK USER ID
         BNE   NEXT                    NOT VALID
         CLC   UT$ACCT,ACCTID          CHECK ACCT. ID
         BNE   NEXT                    NOT VALID
         CLC   UT$PWD,PASSWD           CHECK PASSWORD
         BE    SUCCESS                 A MATCH!!
NEXT     L     R9,UT$NEXT              POINT TO NEXT USER TABLE
         LTR   R9,R9                   END OF TABLE?
         BNZ   LOOP                    NO, KEEP TRYING
*         
* SEND BAD USER ID MESSAGE
*
         MVC   ZA#OMSG(BADUSERL),BADUSER SET UP OMA WITH MESSAGE
         LA    R11,BADUSERL
         STH   R11,ZA#OTL
         MVI   ZA#PSIND,ZA#PSNN        SET NORMAL TERMINATION
         B     DONE         
* 
* SEND LOGON SUCCESS MESSAGE
*
SUCCESS  MVC   ZA#OMSG(LOGONOKL),LOGONOK SET UP OMA WITH MESSAGE
         LA    R11,LOGONOKL
         STH   R11,ZA#OTL
         MVI   ZA#PSIND,ZA#PSNN        SET NORMAL TERMINATION
         ST    R9,TT$USER              SAVE PTR TO USER TBL
         OI    TT$FLAGS,TT$LOGON       SET LOGGED ON FLAG
         GETIME S                      SAVE LOGGED ON TIME
         ST    R0,TT$LGNDT
         ST    R1,TT$LGNTM
         B     DONE 
*
* SEND ALREADY LOGGED ON MESSAGE
*
LOGGEDON MVC   ZA#OMSG(ALRDYONL),ALRDYON SET UP OMA WITH MESSAGE
         LA    R11,ALRDYONL
         STH   R11,ZA#OTL
         MVI   ZA#PSIND,ZA#PSNN        SET NORMAL TERMINATION
         B     DONE 
*
* SEND UNKNOWN TERMINAL MESSAGE
*
BADTRM   MVC   ZA#OMSG(UNKTERML),UNKTERM SET UP OMA WITH MESSAGE
         LA    R11,UNKTERML
         STH   R11,ZA#OTL
         MVI   ZA#PSIND,ZA#PSNN        SET NORMAL TERMINATION
         B     DONE 
*        
DONE     LM    R14,R12,12(R13)         RESTORE REGISTERS
         BR    R14                     RETURN TO MONITOR
*
BADUSER  ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'27'                 ERASE PROTECTED
         DC    CL1'M'
         DC    XL1'1E'                 SOE
         DC    C'INVALID ID, ACCOUNT, PASSWORD FOR LOGON'
         ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'1E'                 SOE
BADUSERL EQU   *-BADUSER
*
LOGONOK  ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'27'                 ERASE PROTECTED
         DC    CL1'M'
         DC    XL1'1E'                 SOE
         DC    C'USER SUCCESSFULLY LOGGED ON'
         ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'1E'                 SOE
LOGONOKL EQU   *-LOGONOK  
*         
ALRDYON  ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'27'                 ERASE PROTECTED
         DC    CL1'M'
         DC    XL1'1E'                 SOE
         DC    C'TERMINAL ALREADY LOGGED ON, PROCEED'
         ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'1E'                 SOE
ALRDYONL EQU   *-ALRDYON  
*         
UNKTERM  ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'27'                 ERASE PROTECTED
         DC    CL1'M'
         DC    XL1'1E'                 SOE
         DC    C'TERMINAL NOT CONFIGURED'
         ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'1E'                 SOE
UNKTERML EQU   *-UNKTERM
*
WORKAREA DSECT
USERID   DS    CL4
ACCTID   DS    CL4
PASSWD   DS    CL4
*
         END
// FIN