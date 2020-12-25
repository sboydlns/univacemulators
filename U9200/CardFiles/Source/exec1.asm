***********************************************************
*
*       Exec1
*
*       Recreated from disassembled object code.
*
*       Comments are mine based on my interpretation of
*       the code.
*
*       Stephen W. Boyd
*       April 2016
*
***********************************************************
        START 0

        USING *,0


        ENTRY E?IN
        ENTRY E?SC
        ENTRY E?RE
        ENTRY E?DS
              
PSC     DC    XL1'00'
SRC     DC    XL1'00'
        DC    YL2(E?IH)
ZERO    DC    XL2'0000'
TWO     DC    XL2'0002'
FOUR    DC    XL2'0004'
JTB     DC    YL2(E?IN)
JTA     DC    YL2(E?SC)
TMP     DC    XL3'000000'
        DC    XL1'00'
RA      DC    XL2'0000'
*
*       Table of interrupt handlers for each I/O channel
*        
E?IN    DC    YL2(IH1)
        DS    12H
*
*       SRC handler jump table. The way this table is structure
*       it looks like only even numbered SRC requests can be
*       handled.
*
E?SC    DC    YL2(E?RS)                SRC 0
        DC    XL6'000000000000'
        DC    YL2(E?MS)                SRC 8
        DS    H
SC1     EQU   *        

        ORG   X'0010'

        DC    XL2'0000'
        DC    YL2(E?IH)

        ORG   X'0043'

        DC    XL1'00'

        ORG   SC1
*
*       SRC interrupt handler
*
E?IH    MVC   SRC(1),X'0011'           Save SRC code
        CLI   X'0043',0                Int dev addr = zero?
        BC    8,IH1                    Yes, handle the SRC
        MVC   TMP+1(1),X'0043'         Save the device addr.
        TM    X'0043',X'80'            Device addr > X'80'
        BC    8,IH4                    No
*
*      This isolates the channel number from MUX channel
*      shared sub-channel addresses.
*       
        NI    TMP+1,X'78'              Isolate the channel # 
        LH    15,TMP                   Get it into R15
        AH    15,TMP                   Shift left 1
        MVO   TMP(3),X'003E'(2)        Get chan # frm R15 to LSB of HW
IH4     LH    15,TMP                   Get chan # back to R15
        AH    15,TMP                   Double it
        AH    15,JTB                   Add address of handler tbl
IH2     STH   15,IH3+2                 Modify next inst with JT addr
IH3     LH    15,0                     Get the handler address
        BC    15,0(,15)                Go to it
IH1     LH    15,ZERO                  Zero R15
        MVC   X'003F'(1),SRC           SRC code to LSB R15
        AH    15,JTA                   Add address of jump table
        BC    15,IH2                   Go execute the handler
*
*       Return to the caller
*        
E?RE    MVI   X'0043',0                Clear the dev address
        LPSC  PSC,X'60'                Reset INT addr & go proc mode
*      
*      MSG handler (SRC 8)
*       
E?MS    LH    15,X'0002'               Get proc mode program cntr
        MVC   MSP(2),0(15)             Get HPR code to our param
        BAL   15,E?DS                  Go do display
MSP     DC    XL2'0000'                HPR code parameter for above
        LH    15,X'0002'               Get proc mode program cntr
        MVC   3(1,15),X'0004'          Save the response
        AH    15,FOUR                  Bump past params.
        STH   15,X'0002'               Modify proc mode program cntr
        BC    15,E?RE                  Return to caller
*
*      Do display & response
*       
E?DS    MVI   X'0004',0                Clear user response byte
        MVC   DS1+2(2),0(15)           Modify the HPR with user code
DS1     HPR   X'0000',0                Halt & wait for response
        BC    15,2(,15)                Return to caller
*
*      RSTRT handler (SRC 0)      
*
E?RS    LH    15,X'0002'               Get proc mode program cntr
        MVC   RA(2),0(15)              Save new restart addr
        MVC   X'0016'(4),RS1           Set restart addr to our hndlr
        AH    15,TWO                   Bump past parameters
        STH   15,X'0002'               Set new proc mode program cntr
        BC    15,E?RE                  Return to caller
RS1     BC    15,RS2
*
*      Come here after Clear & Start
*       
RS2     MVC   X'0002'(2),RA            Get user's restart addr
        MVI   X'0043',0                Clear the dev addr
        LPSC  PSC,X'68'                Reset INT addr & go proc mode
*                                      Alter/Display restricted
        END
