***********************************************************
*
*       Card reader tester
*
***********************************************************

CRTS    START 0
        
        USING *,0 
        
        EXTRN READ                     Reader I/O entry point
        EXTRN PRNT

        ENTRY FOF
        ENTRY RBUF        
        ENTRY REOF
        
CARD    DS    CL80                     My record buffer        
RBUF    DS    CL80                     IOCS reader buffer
PBUF    DS    CL132                    Printer buffer
        
STRT    EQU   *    

        MVI   PBUF,C' '                Clear printer buffer
        MVC   PBUF+1(131),PBUF

        OPEN  READ
        OPEN  PRNT
LOOP    EQU   *
        GET   READ,CARD
        MVC   PBUF(80),CARD
        PUT   PRNT,PBUF
        BC    15,LOOP
*
* End-of-file 
*        
REOF    EQU   *        
        MVC   PBUF(80),CARD
        PUT   PRNT,PBUF
        CLOSE READ
        CLOSE PRNT
EOF1    EQU   *        
        MSG   X'1FFF'
        BC    15,EOF1
        
*
* Form overflow
*
FOF     EQU   *
        BC    15,0(,14)
                
        END   STRT
        