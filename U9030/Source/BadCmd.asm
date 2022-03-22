         TITLE 'SEND ''BAD COMMAND'' ERRORS FOR BOYD''S BEM SIMULATOR'
BADCMD   START
***********************************************************************
*                                                                     *
* A SIMPLE PROGRAM FOR BOYD'S BEM SIMULATOR TO SEND AN UNKNOWN COMMAND*
* ERROR TO THE USER'S TERMINAL.                                       *
*                                                                     *
***********************************************************************
*
         PRINT NOGEN
         ZM#DPIB
         ZM#DIMH
ZA#IMSG  DS    CL2048         
         ZM#DOMH
ZA#OMSG  DS    CL2048                  OUTPUT MSG BFR.         
BADCMD   CSECT         
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
         CLI   ZA#IMSG,C'L'            SEND 'PLS LOGON'?
         BE    SNDLOGON                YES
         MVC   ZA#OMSG(UNKCMDL),UNKCMD NO, SEND 'UNK COMMAND'
         LA    R11,UNKCMDL
         STH   R11,ZA#OTL
         MVI   ZA#PSIND,ZA#PSNN        SET NORMAL TERMINATION
         B     DONE
*
SNDLOGON MVC   ZA#OMSG(LOGONL),LOGON   MOVE MSG TO OMA
         LA    R11,LOGONL
         STH   R11,ZA#OTL
         MVI   ZA#PSIND,C'N'           SET NORMAL TERMINATION
         B     DONE
*         
DONE     LM    R14,R12,12(R13)         
         BR    R14
*
UNKCMD   ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,22                 LINE 22 COLUMN 1
         DC    XL1'27'                 ERASE PROTECTED
         DC    CL1'M'
         DC    XL1'1E'                 SOE
         DC    C'UNKNOWN COMMAND'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'1E'                 SOE
UNKCMDL  EQU   *-UNKCMD         
*         
LOGON    ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,22                 LINE 22 COLUMN 1
         DC    XL1'27'                 ERASE PROTECTED
         DC    CL1'M'
         DC    XL1'1E'                 SOE
         DC    C'PLEASE LOGON'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'1E'                 SOE
LOGONL   EQU   *-LOGON
*
         END
// FIN