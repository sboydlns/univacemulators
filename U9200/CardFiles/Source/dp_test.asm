***********************************************************
*
*       EXEC-1 tester
*
***********************************************************

E1TS    START 0
        
        USING *,0 
        
        DS    0H
STRT    EQU   *    
*
*       Ask user for test number    
*
LOOP    MSG   X'001',REPLY
RSP1    EQU   LOOP+7
        CLI   RSP1,1
        BC    8,T1
        CLI   RSP1,2
        BC    8,T2
        BC    15,LOOP
*
*       T1 - Divide no remainder
*       
T1      ZAP   RSLT,P10
        DP    RSLT,P5
        CP    RSLT(2),P2
        BC    8,T1A
        MSG   X'002'                   OOPS! QUOTIENT <> 2
        BC    15,LOOP
T1A     CP    RSLT+2(2),P0
        BC    8,LOOP
        MSG   X'003'                   OOPS! REMO <> 0
        BC    15,LOOP        
*
*       T2 - Divide with remainder
*
T2      EQU   *
        ZAP   RSLT,P10
        DP    RSLT,P3
        CP    RSLT(2),P3
        BC    8,T2A
        MSG   X'004'                   OOPS! QUOTIENT <> 3
        BC    15,LOOP
T2A     CP    RSLT+2(2),P1
        BC    8,LOOP
        MSG   X'005'                   OOPS! REM <> 1
        BC    15,LOOP               

RSLT    DS    CL4
P10     DC    XL2'010C'
P5      DC    XL2'005C'
P3      DC    XL2'003C'
P2      DC    XL2'002C'
P1      DC    XL2'001C'
P0      DC    XL2'000C'
        
        END   STRT                
