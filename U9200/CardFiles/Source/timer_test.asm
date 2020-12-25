TIMR    START 0

        USING *,0

        EXTRN PRNT
        
        ENTRY FOF
        
BEGN    EQU   *
        LPSC  IPSC,X'64'               Set I/O PSC to our handler
L1      EQU   *        
        XIOF  X'23',X'90'              Start the timer running
L2      EQU   *        
        CLI   TFLG,1                   Has timer fired?
        BC    7,L2                     Nope, keep waiting
        PUT   PRNT,MSG                 Tell that timer has fired
        MVI   TFLG,0                   Reset flag
        BC    15,L1                    Restart timer

IH      EQU   *
        MVI   TFLG,1
        TIO   STAT,X'90'               Clear the BUSY bit
        LPSC  IPSC,X'64'               Reset I/O PSC and rtn PROC mode
        
*
*       Form overflow routine, not used
*        
FOF     EQU   *
        BC    15,0(,14)        

IPSC    DC    XL2'00'                  I/O mode PSC
        DC    YL2(IH)

STAT    DC    XL1'00'        
TFLG    DC    XL1'00'                  Timer fired flag
MSG     DC    CL16'TIMER FIRED'
        DC    CL16' '
        DC    CL16' '
        DC    CL16' '
        DC    CL16' '
        DC    CL16' '
        DC    CL16' '
        DC    CL16' '
        DC    CL4' '
        
        END   BEGN