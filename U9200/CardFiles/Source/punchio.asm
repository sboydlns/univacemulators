        START 0

        USING *,0


***********************************************************
*
*       Card reader / punch I/O (DTFRP)
*
*       For use by assembler programs (not RPG).
*
*       Recreated from disassembled object code.
*
*       Comments are mine based on my interpretation of
*       the code.
*
*       Stephen W. Boyd
*       May 2016
*
***********************************************************
        ENTRY PUNC
        
        USING *,0

*PUNC DTFRP CNTL=YES,
*           IOA1=XPUN,
*           MODE=TRANS,
*           OTBL=TBPU,
*           PUNR=YES,
*           TYPF=OUTPUT
* PUNCH IOCS (CARD)

        EXTRN XPUN
        EXTRN TBPU

PUNC    BC    15,U?OP                  Open
        BC    15,U?CL                  Close
        BC    15,*                     Get
        BC    15,U?CN                  Control
*
* PUT
*      
U?PT    EQU   *      
        MVI   RTRY+1,0                 Clear retry count             
        BAL   15,U?WT                  Wait for previous I/O to finish
        LH    15,0(,14)                Get address of user's bufer
PT1     MVC   XPUN(80),0(15)           Move it to my buffer
PT2     TR    XPUN(80),TBPU            Translate to Hollerith
        BAL   15,U?XF                  Issue the I/O
        NI    U?XF+1,X'F7'             Clear stacker select bit             
        BC    15,X'002'(,14)           Return to caller
*
* CONTROL
*        
U?CN    EQU   *        
        CLI   0(14),X'E2'              1st byte of param = 'S'?
        BC    8,U?SS                   Yes, select stacker
        MVC   XF1+1(1),2(14)           Mdfy MVI instn with rec len
        MVC   TEMP+1(1),2(14)          Get len to temp area
        AI    TEMP,-1                  Decrement by 1
        MVC   PT1+1(1),TEMP+1          Modify MVC & TR instn with
        MVC   PT2+1(1),TEMP+1          record length
        BC    15,4(,14)                Return to caller
U?SS    EQU   *        
        OI    U?XF+1,X'08'             Modify function with select bit
        BC    15,4(,14)                Return to caller
*
* OPEN
*        
U?OP    EQU   *        
        MVI   U?XF+1,X'11'             Set initial punch function
        MVI   XF1+1,80                 Set MVI to rec len of 80
        MVI   PT1+1,79                 Set MVC & TR instn to 80
        MVI   PT2+1,79                 bytes
        BC    15,0(,14)                Return to caller
*
* Issue the I/O
*        
U?XF    EQU   *        
        MVI   XF2+3,0                  Mdfy XIOF instn with function
XF1     MVI   LEN,0                    Set length holding area
XF3     MVC   X'004D'(1),LEN           Set length in BCW
        MVC   X'004E'(2),PT1+2         Set bfr address in BCW
XF2     XIOF  X'0000',2                Issue the I/O
        BC    8,0(,15)                 OK, return
        TIO   U?DI+5,2                 Get the status
U?DI    EQU   *
        MSG   X'6200'                  Halt & display error        
        BC    15,XF2                   Try again
U?D2    EQU   *     
        MSG   X'6200'                  Halt & display error
U?D3    STH   15,TEMP                  Save return address
        BAL   15,XF3                   Try the I/O again
        LH    15,TEMP                  Restore return address
*
* Wait for previous I/O to complete
*        
U?WT    EQU   *               
        TIO   U?D2+5,2                 Get the status
        BC    3,U?WT                   Busy, try again
        TM    U?D2+5,X'FB'             Error?
        BC    8,0(,15)                 No, return
        TM    U?D2+5,X'DB'             Punch Check Error?
        BC    6,U?D2                   No, show error
        AI    RTRY,1                   Yes, bump retry count 
        CLI   RTRY+1,5                 Retry count exceded?
        BC    6,U?D3                   No, retry I/0
        MVI   RTRY+1,0                 Yes, reset retry count
        BC    15,U?D2                  Show error
*
*       Close
*        
U?CL    EQU   *        
        BAL   15,U?WT
        BC    15,X'000'(,14)
TEMP    DC    XL2'0000'
LEN     DC    XL1'00'
        DC    XL1'00'
RTRY    DC    XL2'0000'

        END
