         TITLE '/HELP COMMAND FOR BOYD''S BEM SIMULATOR'
BEMHLP   START
***********************************************************************
*                                                                     *
* AN IMPLEMENTATION OF THE /HELP COMMAND FOR BOYD'S BEM SIMULATOR.    *
*                                                                     *
***********************************************************************
*
         PRINT NOGEN
         ZM#DPIB
         ZM#DIMH
         ZM#DOMH
ZA#OMSG  DS    CL2048                  OUTPUT MSG BFR.         
BEMHLP   CSECT         
         RGEQU
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
         USING ZA#DPIB,R3
         USING ZA#IMH,R4
         USING ZA#OMH,R6
*         
         L     R3,0(R1)                COVER THE PARAMETERS
         L     R4,4(R1)
         L     R5,8(R1)
         L     R6,12(R1)
         L     R7,16(R1)
*         
         MVC   ZA#OMSG(CMDHELPL),CMDHELP SET UP OMA WITH MESSAGE
         LA    R11,CMDHELPL
         STH   R11,ZA#OTL
         MVI   ZA#PSIND,ZA#PSNN        SET NORMAL TERMINATION
*
         LM    R14,R12,12(R13)         
         BR    R14
*
CMDHELP  ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'27'                 ERASE PROTECTED
         DC    CL1'M'
         DC    XL1'1E'                 SOE
         DC    C'ALL SYSTEM COMMANDS MUST BE PRECEDED BY A SLASH.'
         ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'1E'                 SOE
         DC    C'VALID COMMANDS ARE:'
         ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'1E'                 SOE
         DC    C'LOGON, LOGOFF, HELP, STATUS, DISPLAY, VTOC, FSTATUS'
         ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'1E'                 SOE
         DC    C'PRINT, DELETE'
         ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'1E'                 SOE
CMDHELPL EQU   *-CMDHELP
*         
         END
// FIN