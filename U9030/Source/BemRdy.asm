         TITLE 'SEND ''BEM READY'' MESSAGE FOR BOYD''S BEM SIMULATOR'
BEMRDY   START
***********************************************************************
*                                                                     *
* A SIMPLE PROGRAM TO SEND TO 'BEM READY' MESSAGE TO THE USER'S       *
* TERMINAL.                                                           *
*                                                                     *
***********************************************************************
*
         PRINT NOGEN
         ZM#DPIB
         ZM#DIMH
         ZM#DOMH
ZA#OMSG  DS    CL2048                  OUTPUT MSG BFR.         
BEMRDY   CSECT         
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
         MVC   ZA#OMSG(RDYL),RDY       SET UP OMA WITH MESSAGE
         LA    R11,RDYL
         STH   R11,ZA#OTL
         MVI   ZA#PSIND,ZA#PSNN        SET NORMAL TERMINATION
*
         LM    R14,R12,12(R13)         
         BR    R14
*
RDY      ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 ERASE PROTECTED DISPLAY
         DC    CL1'M'
         ZO#COORD 1,22                 LINE 22 COLUMN 1
         DC    XL1'1E'                 SOE
         DC    C'BOYD''S BEM SIMULATOR READY'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'1E'                 SOE
RDYL     EQU   *-RDY         
*         
         END
// FIN