***********************************************************
*
*       Card reader I/O (DTFCR)
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

R?IO    START 0

        USING *,0
        
*READ DTFCR EOFA=REOF,
*           IOA1=RBUF,
*           ITBL=TBRD,
*           MODE=TRANS 
* RDR IOCS (CARD)
         
        ENTRY READ

        EXTRN REOF                     EOF
        EXTRN RBUF                     Buffer
        EXTRN TBRD                     Xlate table
       
READ    BC    15,A?OP                  Open
        BC    15,A?CL                  Close
A?GT    BAL   15,A?WT                  Get
*
* Copy last record read to user buffer & translate
*       
        LH    15,0(,14)                Get user record buffer addr
        MVC   0(80,15),RBUF            Move rec just read to user bfr
        TR    0(80,15),TBRD            Translate to EBCDIC
        CLI   RBUF,X'34'               First column = '/'?
        BC    8,A?EF                   Yes, EOF
        BAL   15,A?XF                  No, start next I/O
        BC    15,2(,14)                Return
A?EF    BAL   15,A?XF                  Start next I/O           
        BAL   15,A?WT                  Wait for it to complete
        BC    15,REOF                  Go to user EOF routine
*
* Wait for previous I/O to complete
*       
A?WT    TIO   A?DS+5,1                 Get status of last I/O
        BC    3,A?WT                   Busy, try again
        TM    A?DS+5,243               I/O error?
        BC    4,A?DS                   Yes
        TM    A?DS+5,8                 Photocell check?
        BC    8,0(,15)                 No, return
        CLC   X'0046'(2),A?ST          Last I/O addr > end of bfr?
        BC    2,0(,15)                 Yes, ignore error & return
*
* Error during wait. HPR then reissue I/O & wait
*       
A?DS    MSG   X'6100'       
        STH   15,A?CO+2                Save the return address
        BAL   15,A?XF                  Start the next I/O
        LH    15,A?CO+2                Restore the return address
        BC    15,A?WT                  Wait for the I/O to complete
*
* Issue the I/O
*       
A?XF    MVI   X'0045',80               Init. the BCW
        MVC   X'0046'(2),A?CO
A?X2    XIOF  X'0012',1                Issue the I/O
        BC    8,0(,15)                 No error, return
        TIO   A?DI+5,1                 Get the status
*      
* Error after a I/O. HPR & retry I/O
*
A?DI    MSG   X'6100'                  Show the error      
        BC    15,A?X2                  Try the I/O again
*
* Close       
*
A?CL    BAL   15,A?WT                  Wait for previous I/O to end
        BC    15,0(,14)                Return
*
* Open
*       
A?OP    BAL   15,A?XF                  Issue first I/O
        BC    15,0(,14)                Return
       
A?CO    DC    YL2(RBUF)                Start of buffer addr
        DS    CL2
A?ST    DC    YL2(RBUF+79)             End of buffer addr
        ORG   *                        
        DC    C'4'                     
        
        END
       
       
       
