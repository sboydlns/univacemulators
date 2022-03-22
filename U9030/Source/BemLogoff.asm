         TITLE '/LOGOFF COMMAND FOR BOYD''S BEM SIMULATOR'
BEMLGF   START
***********************************************************************
*                                                                     *
* AN IMPLEMENTATION OF THE /LOGOFF COMMAND FOR BOYD'S BEM SIMULATOR.  *
*                                                                     *
***********************************************************************
*
         PRINT NOGEN
         ZM#DPIB
         ZM#DIMH
ZA#IMSG  DS    CL2048         
         ZM#DOMH
ZA#OMSG  DS    CL2048                  OUTPUT MSG BFR.         
BRMLGF   CSECT         
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
* R10 = TERM TABLE
* R14 = RETURN ADDRESS
************         
         BALR  R2,0                    SET COVER
         USING *,R2
         USING LOWMEM,R0
         USING ZA#DPIB,R3
         USING ZA#IMH,R4
         USING ZA#OMH,R6
         USING TERMTBL,R10
*         
         L     R3,0(R1)                COVER THE PARAMETERS
         L     R4,4(R1)
         L     R5,8(R1)
         L     R6,12(R1)
         L     R7,16(R1)
*
* CLEAR THE PTR. TO THE LOGGED ON USER IN THE TERMINAL TABLE
*
         L     R1,ZA#ISTID             FIND TERMINAL TABLE
         L     R15,LM$FTNAM         
         BALR  R14,R15
         LTR   R10,R1                  DID WE FIND IT?
         BZ    DONE                    NO, WTF???
         XC    TT$USER,TT$USER
*         
* SEND USER LOGGED OFF MESSAGE
*
         MVC   ZA#OMSG(LOGOFOKL),LOGOFOK SET UP OMA WITH MESSAGE
         LA    R11,LOGOFOKL
         STH   R11,ZA#OTL
         MVI   ZA#PSIND,ZA#PSNN        SET NORMAL TERMINATION
         XC    TT$USER,TT$USER         CLEAR PTR TO USER TABLE
         NI    TT$FLAGS,X'FF'--TT$LOGON CLEAR LOGGED ON FLAG
*        
DONE     LM    R14,R12,12(R13)         RESTORE REGISTERS
         BR    R14                     RETURN TO MONITOR
*         
LOGOFOK  ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'27'                 ERASE PROTECTED
         DC    CL1'M'
         DC    XL1'1E'                 SOE
         DC    C'USER SUCCESSFULLY LOGGED OFF'
         ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'1E'                 SOE
LOGOFOKL EQU   *-LOGOFOK  
*         
         END
// FIN