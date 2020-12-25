***********************************************************
*
*       Card punch tester
*
***********************************************************

CPTS    START 0
        
        USING *,0 
        
        EXTRN READ                     Reader I/O entry point
        EXTRN PUNC

        ENTRY RBUF        
        ENTRY REOF
        ENTRY XPUN
        
CARD    DS    CL80                     My reader buffer        
RBUF    DS    CL80                     IOCS reader buffer
XPUN    DS    CL80                     My punc buffer
        
STRT    EQU   *    

        OPEN  READ
        OPEN  PUNC
LOOP    EQU   *
        GET   READ,CARD
        PUT   PUNC,CARD
        BC    15,LOOP
*
* End-of-file 
*        
REOF    EQU   *        
        PUT   PUNC,CARD
        CLOSE READ
        CLOSE PUNC
EOF1    EQU   *        
        MSG   X'1FFF'
        BC    15,EOF1
                
        END   STRT
        