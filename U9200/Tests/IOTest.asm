        START
        DS    CL5
*
* TEST1 - Reader
* TEST2 - Punch
*        
TEST    DS    XL1
        DS    CL12        
*
* Address of first instruction executed after load (STRT)
*        
        DC    XL2'0080'
        DS    XL2
*
* Instruction executed after Clear - Start
*
        B     RSET
*                
        DS    CL102
*
* Halt and wait for operator to enter test # at address 5
*      
STRT    HPR   X'0001'  
        MVI   ALLT,0      
LOOP    CLI   TEST,1
        BEQ   TST1
        CLI   TEST,2
        BEQ   TST2
        CLI   TEST,255
        BEQ   ALL
* Invalid test #       
        B     STRT
*
* Do all tests
*
ALL     MVI   ALLT,1
        MVI   TEST,1
        B     LOOP        
*
* TEST 1 - Reader
*
TST1    MVC   X'44'(4),RBCW            READ A CARD
        XIOF  2,1                      
        BZ    T11                      COMMAND ACCEPTED, ALL GOOD SO FAR
        HPR   X'0101'                 
        B     STRT
T11     TIO   STAT,1                   CHECK STATUS
        BC    2,T11                    STILL BUSY
        BC    12,T12                   I/O COMPLETED - ALL OK
        HPR   X'0102'
        B     STRT                              
        
T12     B     DONE
*
* TEST 2 - Punch
*
TST2    MVC   X'48'(4),RBCW            Read a card                       
        XIOF  2,2
        BZ    T21                      COMMAND ACCEPTED, ALL GOOD SO FAR
        HPR   X'0201'
        B     STRT
T21     TIO   STAT,2                   CHECK STATUS
        BC    2,T21                    STILL BUSY
        BC    12,T22                   I/O COMPLETED - ALL OK
        HPR   X'0202'
        B     STRT
T22     MVC   X'4C'(4),PBCW            WRITE A CARD
        XIOF  1,2
        BZ    T23                      COMMAND ACCEPTED, ALL GOOD SO FAR
        HPR   X'0203'
        B     STRT
T23     TIO   STAT,2                   CHECK STATUS
        BC    2,T23                    STILL BUSY
        BC    12,T24                   I/O COMPLETED - ALL OK
        HPR   X'0204'
        B     STRT
T24     B     DONE                                

* Test successful, notify operator
DONE    CLI   ALLT,1
        BNE   STOP
        AI    TEST-1,1
        B     LOOP
STOP    HPR   X'0002'                  TEST SUCCESSFUL        
        B     STRT
*
* Test Clear - Start (reset) functionality
*
RSET    HPR   X'7FFF'
        B     STRT         

RBCW    DC    XL2'0050'                READ 80 BYTES TO RBFR
        DC    Y(RBFR)
PBCW    DC    XL2'0050'                WRITE 80 BYTES FROM PBFR
        DC    Y(PBFR)
RBFR    DS    CL80
PBFR    DC    XL10'31511121417161098132'
        DC    XL70'00'
ALLT    DC    XL1'0' 
STAT    DS    XL1       

        END   STRT