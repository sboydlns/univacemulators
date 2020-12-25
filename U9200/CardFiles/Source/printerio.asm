***********************************************************
*
*       Printer I/O (DTFPR)
*
*       For use by assembler programs (not RPG).
*
*       Recreated from old listings & disassembled object
*       code.
*
*       Comments are mine based on my interpretation of
*       the code.
*
*       Stephen W. Boyd
*       June 2015
*
***********************************************************

P?IO    START 0

        USING *,0

*PRNT   DTFPR BKSZ=132,
*             CNTL=YES,
*             PRAD=1,
*             PROV=FOF,
*             FONT=63
* PRINT IOCS (CARD)

        ENTRY PRNT
        ENTRY B?SH

        EXTRN FOF

PRNT    BC    15,B?OP                  Open
        BC    15,B?CL                  Close
        BC    15,*                     Get
        BC    15,B?CN                  Cntrl
*
* Put - Address to user buffer passed as 2 bytes immediately
*       following the BAL instruction
*        
        BAL   15,B?VE                  Wait until prev. I/O complete
        LH    15,0(,14)                Get user buffer address
        MVC   X'0080'(132),0(15)       Move to printer image area
B?SH    MVI   B?SE+1,0                 Set spacing (zero first time)
        MVI   B?SH+1,1                 Set future spacing (PRAD)
        MVC   B?K+3(1),B?X1            Set command in XIOF
B?IS    MVC   X'0050'(1),B?SE+1        Move spacing to BCW
        BAL   15,B?XF                  Go issue the I/O
B?D     BC    15,2(,14)                Return if no overflow
        MVI   B?D+1,X'F0'              Reset overflow flag
        AH    14,B?H                   Bump past parameter
        BC    15,FOF                   Go to user overflow routine
B?F     BAL   15,B?VE                  Wait until prev. I/O complete
        MVI   B?K+3,19                 Set XIOF to control command
        BC    15,B?IS                  Go issue the I/O
*
* Control - Parameters passed in 4 bytes immediately following the
*           BAL instruction as:
*
*           ffba
*
*           where:
*
*           ff = the function (SP for space or SK for skip)
*           b  = # lines before (blank if not given)
*           a  = # lines after (blank if not given) 
*        
B?CN    CLI   3(14),64                 Space after = ' '?
        BC    8,B?O6                   Yes
        MVC   B?SH+1(1),3(14)          No, set spacing for next PUT
        CLI   1(14),C'P'               Are we spacing or skipping?
        BC    8,B?O6                   Spacing
        OI    B?SH+1,8                 Set skipping bit
B?O6    TM    2(14),63                 Space before = 0?
        BC    8,4(,14)                 Yes
        MVC   B?SE+1(1),2(14)          Set the spacing for control cmd
        CLI   1(14),C'P'               Are we spacing or skipping?
        BC    8,B?G                    Spacing
        OI    B?SE+1,8                 Set the skipping bit
B?G     AH    14,B?H                   Adjust rtn addr to be = PUT
        BC    15,B?F                   Go issue a control command
*
* Open
*        
B?OP    MVI   X'0080',64               Blank printer image area
        MVC   X'0081'(131),X'0080'
        MVI   B?SH+1,1                 Set spacing (PRAD)
        MVI   B?C+1,X'F0'              Make B?C unconditional
        MVI   B?D+1,X'F0'              Make B?D unconditional
        BC    15,0(,14)                Return
*
* Close
*        
B?CL    BAL   15,B?VE	Wait for prev. I/O to complete
        BC    15,0(,14)                Return
*
* Error
*        
B?E     TIO   B?DI+5,3                 Still busy? Status to HPR
        BC    3,B?E                    Yes, try again
        TM    B?DI+5,X'FB'
        BC    8,B?C
B?ER    TM    B?DI+5,1
        BC    8,*+8
        MVI   B?C+1,0                  Make B?C NOP
B?M     TM    B?DI+5,2
        BC    8,B?I
        MVI   B?D+1,0                  Make B?D NOP
B?I     TM    B?DI+5,X'F8'
        BC    8,0(,15)
        NI    B?DI+5,X'FC'               
B?DI    MSG   X'6300'
        BC    15,0(,15)
B?C     BC    15,B?VX
        CLI   X'0050',15
        BC    6,B?VX
        MVI   B?C+1,X'F0'              Make B?C unconditional
        MSG   X'6301'
B?VX    BC    15,0                     Branch to saved return addr
*
* Verify
*
B?VE    STH   15,B?VX+2                Save return addr
        BAL   15,B?E                   Check for busy / error
        BAL   15,B?XF                  There was an error, retry I/O
        BC    15,B?VE+4                Loop
B?XF    STH   15,B?N+2                 Save the return addr
B?K     XIOF  X'0000',3                Issue the I/O
B?N     BC    8,0                      Branch to saved return addr
        TIO   B?DI+5,3                 Get the I/O status
B?L     BAL   15,B?ER                  Check for errors
        BC    15,B?K                   There was an error, retry I/O
     
B?SE    DC    X'00'
        DC    X'00'
B?H     DC    Y(2)                     Offset to actual return address
B?X1    DC    YL1(0+17)                Inhibit intr + read
        ORG   *
        DC    C'4'
                     
        END
