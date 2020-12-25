***********************************************************
*
*       EXEC-1 tester
*
***********************************************************

E1TS    START 0
        
        USING *,0 
        
        DS    1H
STRT    EQU   *    
*
*       Ask user for test number    
*
LOOP    MSG   X'001',REPLY
RSP1    EQU   LOOP+7
        CLI   RSP1,1
        BC    8,T1
        BC    15,LOOP
*
*       T1 - Set the restart address
*       
T1      RSTRT RSTR
        MSG   X'002'
        BC    15,LOOP
*
*       Restart here when Clear & Start pressed 
*       
RSTR    MSG   X'003'        
        BC    15,LOOP
        
        END   STRT                
