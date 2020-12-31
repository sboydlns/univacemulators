                   START
       .                   
       . UNIVAC 494 ALGOL RUN-TIME LIBRARY                   
       .
       . REGISTER USAGE:
       .   B1 = SUBROUTINE RETURN ADDRESS
       .   B2 = USER SUBROUTINE RETURN ADDRESS
       .   B4 = PTR TO TOP OF CURRENT STACK
       .   B5 = PTR TO LAST STACK BASED VARIABLE FOR THE
       .        CURRENT BLOCK.
       .
                   ER$DEF
                   LCB$DEF
                   L$ITEM$DEF
                   FMCB$DEF
       .
                   XREF      ALG$STACK
                   XREF      POWERI                   
       .
       . EXIT CODES
       .
       ESOVFL      EQU       1D                  . STACK OVERFLOW
       ESUNFL      EQU       2D                  . STACK UNDERFLOW
       EHMTY       EQU       3D                  . HEAP EXHAUSTED 
       EHCRUPT     EQU       4D                  . HEAP CORRUPTED
       EABADIDX    EQU       5D                  . INVALID ARRAY INDEX 
       .
       . VARIABLE TYPE CODES
       .
       DTINTEGER   EQU       0D                  
       DTREAL      EQU       1D
       DTSTRING    EQU       3D
       DTLOGICAL   EQU       4D                 
       .                                      
       . +++++++++++++++++++++++++++++++++++++++++++++++
       .
       . STACK ROUTINES
       .
       . +++++++++++++++++++++++++++++++++++++++++++++++                   
       .
       S$LOW       +0                            . LOWEST STACK ADDRESS
       S$HIGH      +0                            . HIGHEST STACK ADDRESS
       .
       . ++++++++++
       . ALG$INIT
       .
       . INITIALIZE THE STACK
       .
       . PARAMETERS:
       .   STACK SIZE
       .
       . RETURNS:
       .   NONE
       . ++++++++++
       ALG$INIT*   LA        ALG$STACK           . SAVE LOW STACK ADDR
                   SA,W      S$LOW
                   A,W       0,B1                . COMPUTE HEAP LOW ADDR
                   SA,W      AH$HEAD+FMCB$START
                   AN        1                   . COMPUTE STACK HIGH ADDR
                   SA,W      S$HIGH              
                   MEMSIZ                        . GET THE PGM SIZE
                   AN        1                   . COMPUTE HEAP END
                   SA,W      AH$HEAD+FMCB$END
                   LA        AH$HEAD             . INIT HEAP FREE LIST
                   SA,W      ALG$HEAP+LCB$HEAD   
                   SA,W      ALG$HEAP+LCB$TAIL 
                   LA        ALG$HEAP
                   SA,W      AH$HEAD+LI$LIST                 
                   LB,W      B4,S$HIGH           . INIT CURRENT TOP PTR
                   J         1,B1                . RETURN
       . ++++++++++
       . PUSHA
       .
       . PUSH THE A REGISTER ONTO THE STACK
       .
       . PARAMETERS:
       .   VALUE TO PUSH IN A
       .
       . RETURNS:
       .   NONE                   
       . ++++++++++
       PUSHA*      SA,W      S$SAVE
                   SA,W      0,B4                . SAVE A ON TOP
                   SLJ       S$DECR
                   LA,W      S$SAVE
                   J         0,B1
       . ++++++++++
       . PUSHQ
       .
       . PUSH THE Q REGISTER ONTO THE STACK
       .
       . PARAMETERS:
       .   VALUE TO PUSH IN Q
       .
       . RETURNS:
       .   NONE                   
       . ++++++++++
       PUSHQ*      SA,W      S$SAVE
                   SQ,W      0,B4
                   SLJ       S$DECR
                   LA,W      S$SAVE
                   J         0,B1
       . ++++++++++
       . PUSHAQ
       .
       . PUSH THE AQ REGISTER PAIR ONTO THE STACK
       .
       . PARAMETERS:
       .   VALUE TO PUSH IN AQ
       .
       . RETURNS:
       .   NONE                   
       . ++++++++++
       PUSHAQ*     DPS       S$SAVE              . SAVE VALUE TO PUSH
                   SLJ       S$DECR              . DEC STACK TO ALLOW 2 WORDS
                   DPL       S$SAVE              . PUSH VALUE
                   DPS       0,B4
                   SLJ       S$DECR
                   DPL       S$SAVE              . RESTORE ORIGINAL AQ
                   J         0,B1
       . ++++++++++
       . DECREMENT THE STACK POINTER AND CHECK FOR OVERFLOW
       . ++++++++++            
       S$DECR      +0
                   JBD       B4,$+1
                   SB,A      B4
                   TA,W      S$LOW,,YMORE
                   J,L       S$DECR
       .
       . STACK OVERFLOW DETECTED
       .
                   TYPE      SO$LEN,S$OVFL
                   HALT      ESOVFL
       .
       S$SAVE      RES       2D
       S$OVFL      +'STACK OVERFLOW'
                   +'    ^'
       SO$LEN      EQU       $-S$OVFL
       . ++++++++++
       . POPA
       .
       . POP THE TOP OF THE STACK INTO A
       .
       . PARAMETERS:
       .   NONE
       .
       . RETURNS:
       .   POPPED VALUE IN A                   
       . ++++++++++
       POPA*       SLJ       S$INCR
                   LA,W      0,B4
                   J         0,B1
       . ++++++++++
       . POPQ
       .
       . POP THE TOP OF THE STACK INTO Q
       .
       . PARAMETERS:
       .   NONE
       .
       . RETURNS:
       .   POPPED VALUE IN Q                   
       . ++++++++++
       POPQ*       SA,W      S$SAVE
                   SLJ       S$INCR
                   LQ,W      0,B4
                   LA,W      S$SAVE
                   J         0,B1
       . ++++++++++
       . POPAQ
       .
       . POP THE TOP 2 WORDS OF THE STACK INTO AQ
       .
       . PARAMETERS:
       .   NONE
       .
       . RETURNS:
       .   POPPED VALUE IN AQ                   
       . ++++++++++
       POPAQ*      SLJ       S$INCR
                   DPL       0,B4
                   DPS       S$SAVE
                   SLJ       S$INCR
                   DPL       S$SAVE
                   J         0,B1
       . ++++++++++
       . INCREMENT THE STACK POINTER AND CHECK FOR UNDERFLOW
       . ++++++++++            
       S$INCR      +0
                   SB,A      B4
                   TA,W      S$HIGH,,YMORE
                   J         S$UERR
                   TBI       B4,77777
                   J,L       S$INCR
                   HALT      666
       .
       . STACK UNDERFLOW DETECTED
       .
       S$UERR      TYPE      SU$LEN,S$UNFL
                   HALT      ESUNFL
       .
       S$UNFL      +'STACK UNDERFLOW'
                   +'    ^'
       SU$LEN      EQU       $-S$UNFL
       . +++++++++++++++++++++++++++++++++++++++++++++++
       .
       . LIST ROUTINES
       .
       . USED BY THE HEAP MANAGER TO ADD AND REMOVE ITEMS
       . FROM THE FREE MEMORY LIST
       .
       . +++++++++++++++++++++++++++++++++++++++++++++++                   
       .
       . ++++++++++
       . INSERT AN ITEM INTO THE MIDDLE OF A LIST
       .
       . PARAMETERS:
       .   LIST
       .   ITEM TO INSERT
       .   INSERT BEFORE ITEM. IF ZERO ADD TO TAIL.
       .
       . REGISTERS:
       .   B2 = QUEUE
       .   B3 = PARAMS
       .   B5 = ITEM TO INSERT
       .   B6 = MISC.
       .   B7 = INSERT BEFORE THIS ITEM
       . ++++++++++
       L$INSERT    +0
                   SBW       L$SAVE              . SAVE REGISTERS
                   LA,L      L$INSERT            . POINT TO PARAMS
                   LB,A      B3
                   A         3                   . SAVE RTRN ADDR
                   SA,L      LIN$J                   
                   LB,W      B2,0,B3             . GET PTR TO LIST
                   LB,W      B5,1,B3             . GET PTR TO ITEM TO INSERT
                   LA,W      2,B3,ANOT           . GET PTR TO INSERT B4 ITEM
                   J         LIN$END             . ZERO? ADD TO END OF LIST
                   LB,A      B7
                   LA,W      LI$PRIOR,B7,ANOT    . INSERT AT HEAD OF LIST?
                   J         LIN$NEWHD           . YES
                   LB,A      B6
                   SA,W      LI$PRIOR,B5         . UPDATE ITEM PRIOR
                   SB,W      B7,LI$NEXT,B5       . UPDATE ITEM NEXT
                   SB,W      B2,LI$LIST,B5       . UPDATE PTR TO LIST
                   SB,W      B5,LI$NEXT,B6       . UPDATE PRIOR'S NEXT
                   SB,W      B5,LI$PRIOR,B7      . UPDATE NEXT'S PRIOR
                   J         LIN$RTRN
       .
       LIN$NEWHD   SZ,W      LI$PRIOR,B5         . UPDATE ITEM PRIOR
                   SB,W      B7,LI$NEXT,B5       . UPDATE ITEM NEXT
                   SB,W      B2,LI$LIST,B5       . UPDATE PTR TO LIST
                   SB,W      B5,LI$PRIOR,B7      . UPDATE NEXT'S PRIOR
                   SB,W      B5,LCB$HEAD,B2      . UPDATE LIST HEAD                      
       .       
       LIN$RTRN    LBW       L$SAVE              . RESTORE REGISTERS
       LIN$J       J         $                   . RETURN TO CALLER
       .
       LIN$END     LA,W      LCB$TAIL,B2,ANOT    . GET END OF LIST
                   J         LIN$EMPTY           . THERE ISN'T ONE
                   LB,A      B6
                   SA,W      LI$PRIOR,B5         . ITEM PRIOR = TAIL
                   SZ,W      LI$NEXT,B5          . ITEM NEXT = ZERO
                   SB,W      B2,LI$LIST,B5       . SET ITEM LIST PTR
                   SB,W      B5,LI$NEXT,B6       . TAIL NEXT = ITEM                  
                   SB,W      B5,LCB$TAIL,B2      . TAIL = ITEM
                   J         LIN$RTRN
       .
       LIN$EMPTY   SZ,W      LI$NEXT,B5          . CLEAR NEXT / PRIOR
                   SZ,W      LI$PRIOR,B5
                   SB,W      B2,LI$LIST,B5       . SET ITEM LIST PTR
                   SB,W      B5,LCB$HEAD,B2      . HEAD / TAIL = ITEM
                   SB,W      B5,LCB$TAIL,B2
                   J         LIN$RTRN                   
       . ++++++++++
       . DELETE AN ITEM FROM A QUEUE
       .
       . PARAMETERS:
       .   QUEUE
       .   ITEM TO DELETE
       .
       . REGISTERS:
       .   B2 = QUEUE
       .   B3 = PARAMS       
       .   B5 = ITEM TO INSERT
       .   B7 = MISC
       .
       . ++++++++++
       L$DELETE    +0
                   SBW       L$SAVE
                   LA,L      L$DELETE            . POINT TO PARAMS
                   LB,A      B3
                   A         2                   . SAVE RTRN ADDR
                   SA,L      LDE$J
                   LB,W      B2,0,B3             . GET PTR TO QUEUE
                   LB,W      B5,1,B3             . GET PTR TO ITEM TO DELETE
                   LA,W      LI$PRIOR,B5,ANOT    . GET PRIOR
                   J         LDE$NOPRIOR         . THERE ISN'T ONE
                   LB,A      B7
                   LA,W      LI$NEXT,B5          . PRIOR'S NEXT = ITEM NEXT
                   SA,W      LI$NEXT,B7
                   J         LDE$DONEXT
       LDE$NOPRIOR LA,W      LI$NEXT,B5          . LIST HEAD = ITEM NEXT
                   SA,W      LCB$HEAD,B2
       LDE$DONEXT  LA,W      LI$NEXT,B5,ANOT     . GET NEXT
                   J         LDE$NONEXT          . THERE ISN'T ONE
                   LB,A      B7
                   LA,W      LI$PRIOR,B5         . NEXT'S PRIOR = ITEM PRIOR
                   SA,W      LI$PRIOR,B7
                   J         LDE$DONE
       LDE$NONEXT  LA,W      LI$PRIOR,B5         . LIST TAIL = ITEM PRIOR
                   SA,W      LCB$TAIL,B2
       LDE$DONE    SZ,W      LI$LIST,B5          . CLEAR LIST PTR
                   SZ,W      LI$NEXT,B5          . CLEAR NEXT / PRIOR
                   SZ,W      LI$PRIOR,B5
       .
       LDE$RTRN    LBW       L$SAVE              . RESTORE REGISTERS
       LDE$J       J         $                   . RETURN TO CALLER
       .
       L$SAVE      RES       7D
       .
       . +++++++++++++++++++++++++++++++++++++++++++++++
       . FREE MEMORY (HEAP) MANAGER
       . +++++++++++++++++++++++++++++++++++++++++++++++
       . ++++++++++
       .
       . FM$GETMEM
       .
       . ALLOCATE A BLOCK OF MEMORY
       .
       . PARAMETERS: 
       .   SIZE TO ALLOCATE
       .
       . RETURNS:
       .   SIZE TO ALLOCATE PARAMETER OVERWRITTEN WITH BLOCK ADDRESS.
       .   ZERO IF REQUEST CANNOT BE GRANTED
       .
       . REGISTERS:
       .   B2 = PTR TO PARAMETER / RETURN VALUE
       .   B5 = FMCB
       .   B6 = MISC
       .   
       .
       . THE BLOCK ALLOCATED WILL BE AT LEAST 1 WORD LARGER THAN REQUESTED.
       . THIS FIRST WORD OF THE BLOC WILL CONTAIN THE BLOCK SIZE.
       . THE ADDRESS RETURNED TO THE CALLER WILL BE THE 2ND WORD
       . OF THE BLOCK.
       .
       . ++++++++++
       FM$GETMEM   +0
                   SA,W      FM$A                . SAVE REGISTERS
                   SQ,W      FM$Q
                   SBW       FM$B
                   LA,L      FM$GETMEM           . GET RETURN ADDRESS
                   LB,A      B2
                   A         1                   . BUMP PAST PARAMS
                   SA,L      FMG$J               . SAVE RETURN ADDRESS
                   LA,W      0,B2,ANOT           . GET SIZE TO ALLOCATE
                   J         FMG$RTRN            . ZERO BYTES REQUESTED, QUIT
                   A         1                   . ADD 1 FOR OVERHEAD
                   TA        FMCB$LEN,,YLESS     . ALLOC. ENOUGH TO HOLD FMCB
                   LA        FMCB$LEN+1
                   SA,W      FM$SIZE                   
                   SZ,W      0,B2                . CLR RETURN VALUE
                   LB        B5,ALG$HEAP         . GET PTR TO TAIL OF FREE LIST
                   LA,W      LCB$TAIL,B5,ANOT
                   J         HEAPBAD             . LIST EMPTY, NEVER HAPPEN
       .                   
       FMG$LOOP    LB,A      B5
                   LA,W      FMCB$END,B5         . CALC SIZE OF THIS BLOCK
                   AN,W      FMCB$START,B5
                   A         1
                   TA,W      FM$SIZE,,YLESS      . ENOUGH FOR THIS REQUEST?
                   J         FMG$NXTBLK          . NO
                   AN,W      FM$SIZE             . AT LEAST FMCB$LEN LEFT?
                   TA        FMCB$LEN,,YMORE
                   J         FMG$SIZOK           . YES
                   A,W       FM$SIZE             . NO, ALLOCATE ENTIRE BLOCK
                   SA,W      FM$SIZE
       FMG$SIZOK   LA,W      FMCB$END,B5         . YES, CALC NEW END ADDR
                   AN,W      FM$SIZE
                   SA,W      FMCB$END,B5
                   A         1                   . CALC START ADDR THIS BLOCK
                   LB,A      B6                  
                   ZQ                            . CLEAR BUFFER TO ZERO
                   R,W       FM$SIZE,,ADV        
                   SQ,W      0,B6
                   LQ,W      FM$SIZE             . SAVE BLK SIZE AS 1ST WORD
                   SQ,W      0,B6
                   A         1                   . BUMP PAST OVERHEAD
                   SA,W      0,B2                . SAVE FOR RTRN
       . IF FMCB IS EMPTY, FREE IT UNLESS IT IS THE HEAD OF THE 
       . FREE MEMORY LIST
                   LA,W      FMCB$PRIOR,B5,ANOT  . HEAD OF LIST?
                   J         FMG$RTRN            . YES, WE'RE DONE
                   LA,W      FMCB$END,B5         . CALC SIZE REMAINING IN BLOCK
                   AN,W      FMCB$START,B5,ANEG
                   J         FMG$RTRN            . BLOCK NOT EMPTY, QUIT
                   SB,W      B5,FMG$P1           . EMPTY, REMOVE FROM FREE LIST
                   SLJ       L$DELETE
                   +ALG$HEAP
       FMG$P1      +0
       .                        
       FMG$RTRN    LA,W      FM$A                . RESTORE REGISTERS
                   LQ,W      FM$Q
                   LBW       FM$B
       FMG$J       J         $                   . RETURN TO CALLER
       .
       FMG$NXTBLK  LA,W      LI$PRIOR,B5,ANOT    . GET PTR TO PRIOR BLOCK
                   J         FMG$HMTY            . NO MORE, QUIT
                   J         FMG$LOOP
       .
       FMG$HMTY    TYPE      HELEN,HEMSG
                   HALT      EHMTY
       .
       HEMSG       +'HEAP EXHAUSTED'
                   +'    ^'
       HELEN       EQU       $-HEMSG
       .                                      
       . ++++++++++
       . FREE A BLOCK OF MEMORY
       .
       . PARAMETERS:
       .   BLOCK START ADDRESS
       .
       . REGISTERS:
       .   B2 = PARAMETER
       .   B5 = FMCB
       .   B6 = MISC
       .
       . FIRST WE TRY TO ADD THE BLOCK TO BE FREE'D TO AN EXISTING FREE
       . BLOCK.  IF THAT DOESN'T WORK, WE ADD A NEW FREE BLOCK TO THE CHAIN.
       . ++++++++++
       FM$FREEMEM  +0
                   SA,W      FM$A                . SAVE REGISTERS
                   SQ,W      FM$Q
                   SBW       FM$B
                   LA,L      FM$FREEMEM          . GET RETURN ADDRESS
                   LB,A      B2
                   A         1                   . BUMP PAST PARAMS
                   SA,L      FMF$J               . SAVE RETURN ADDRESS
                   LA,W      0,B2,ANOT           . SAVE BLK START ADDR
                   J         FMF$RTRN            . ZERO, QUIT
                   AN        1                   . DEC. FOR OVERHEAD
                   SA,W      FM$BEGIN
                   LB,A      B5                  . GET THE BLK SIZE
                   LA,W      0,B5
                   SA,W      FM$SIZE
                   LB        B5,ALG$HEAP         . GET PTR TO HEAD OF FREE LIST
                   LA,W      LCB$HEAD,B5,ANOT
                   J         HEAPBAD             . LIST EMPTY, NEVER HAPPEN
       .                   
       FMF$LOOP    LB,A      B5
                   LA,W      FMCB$END,B5         . GET END OF FREE SPACE
                   A         1                   . BUMP BY 1
                   AN,W      FM$BEGIN,,AZERO     . = START OF BLK TO FREE?
                   J         FMF$CHKNEW          . NO
                   LA,W      FMCB$END,B5         . YES, CALC NEW END
                   A,W       FM$SIZE
                   SA,W      FMCB$END,B5         . UPDATE CRNT FMCB
       . CONSOLIDATE ADJACENT FMCB'S IF THEY WOULD FORM ONE CONTIGUOUS
       . BLOCK                   
       FMF$CONS    LA,W      FMCB$NEXT,B5,ANOT   . GET PTR TO NEXT FMCB
                   J         FMF$RTRN            . THERE ISN'T ONE
                   LB,A      B6
                   LA,W      FMCB$END,B5         . GET END THIS BLK
                   A         1                   . BUMP TO GET START OF NEXT
                   AN,W      FMCB$START,B6,AZERO . END OF THIS = START OF NEXT?
                   J         FMF$RTRN            . NO, WE'RE DONE
                   LA,W      FMCB$END,B6         . SET THIS END TO NEXT END
                   SA,W      FMCB$END,B5
                   SB,W      B6,FMF$P3           . REMOVE FCMB FROM LIST 
                   SLJ       L$DELETE
                   +ALG$HEAP
       FMF$P3      +0
       .
       FMF$RTRN    LA,W      FM$A                . RESTORE REGISTERS
                   LQ,W      FM$Q
                   LBW       FM$B
       FMF$J       J         $                   . RETURN TO CALLER
       .
       FMF$CHKNEW  TA        0,,YLESS            . NEED TO INSERT FMCB?
                   J         FMF$NXTBLK          . NO, NEXT BLOCK
       FMF$ADDNEW  LA,W      FM$BEGIN            . ADD TO FREE LIST B4 CRNT BLK
                   SA,W      FMF$P1
                   SB,W      B5,FMF$P2
                   SLJ       L$INSERT
                   +ALG$HEAP
       FMF$P1      +0
       FMF$P2      +0
                   LB,W      B5,FM$BEGIN         . POINT TO NEW BLOCK
                   LA,W      FM$BEGIN            . SAVE START / END
                   SA,W      FMCB$START,B5
                   A,W       FM$SIZE
                   AN        1
                   SA,W      FMCB$END,B5
                   J         FMF$CONS                                     
       .                   
       FMF$NXTBLK  LA,W      LI$NEXT,B5,AZERO    . GET PTR TO NEXT BLOCK
                   J         FMF$LOOP            . GOT IT, CONTINUE
                   LB,A      B5
                   J         FMF$ADDNEW          . END OF LIST
       .
       HEAPBAD     TYPE      HBLEN,HBMSG
                   HALT      EHCRUPT
       .
       HBMSG       +'HEAP FREE LIST CORRUPTED'
                   +'    ^'
       HBLEN       EQU       $-HBMSG
       .                                                  
       FM$BEGIN    RES       1
       FM$SIZE     RES       1
       FM$A        RES       1
       FM$Q        RES       1
       FM$B        RES       7D
       
       . ++++++++++++++++++++++++++++++++++++++++++++++
       . ARRAY ROUTINES
       . ++++++++++++++++++++++++++++++++++++++++++++++
       . ++++++++++
       . NEWARRAY
       .
       . ALLOCATE MEMORY FOR AN ARRAY. THE NUMBER OF DIMENSIONS IS 
       . LIMITED TO 10. IF THE COMPILE ALLOWS MORE THAN THIS THE
       . SIZE OF NA$LIMITS MUST BE ADJUSTED ACCORDINGLY.
       .
       . AN ARRAY IS PREFACED BY A HEADER IN THE FOLLOWING FORMAT:
       .   NUMBER OF DIMENSIONS (N)
       .   SIZE OF ARRAY ITEM IN WORDS
       .   LOWER-LIMIT-1
       .   UPPER-LIMIT-1
       .   .
       .   .
       .   .
       .   LOWER-LIMIT-N
       .   UPPER-LIMIT-N
       .       
       . PARAMETERS:
       .   LOWER-LIMIT1
       .   UPPER-LIMIT1
       .   .
       .   .
       .   .
       .   LOWER-LIMITN
       .   UPPER-LIMITN
       .   N
       .   ITEM-SIZE-IN-WORDS
       .
       . RETURNS:
       .   ADDRESS OF ARRAY IN Q
       . 
       . ++++++++++
       NEWARRAY*   SA,W      NA$A                . SAVE REGISTERS
                   SBW       NA$B
       .
                   LQ,W      1,B4                . GET ITEM SIZE
                   SQ,W      NA$ITEMSZ
                   SQ,W      NA$TOTSZ
       .       
                   LA,W      2,B4                . GET # DIMENSIONS
                   SA,W      NA$DIM
                   LB        B4,2,B4             . POP THE STACK
       .
       . CALCULATE TOTAL ARRAY SIZE       
       .
                   LB,W      B2,NA$DIM           . INIT LOOP COUNT
                   JBD       B2,$+1
                   SB,A      B2                  . OFFSET TO LAST LIMIT
                   LSA       1D                  . TABLE ENTRY
                   LB,A      B3
       NA$LOOP     LQ,W      1,B4                . GET UPPER LIMIT
                   LA,W      2,B4                . GET LOWER LIMIT
                   LB        B4,2,B4             . POP THE STACK
                   SA,W      NA$LIMITS,B3
                   SQ,W      NA$LIMITS+1,B3
                   ANQ,A                         . TOTAL # ITEMS
                   AQ        1D
                   ZA
                   M,W       NA$TOTSZ
                   SQ,W      NA$TOTSZ
                   SB,A      B3                  . DEC LIMIT TABLE OFFSET
                   AN        2D
                   LB,A      B3
                   JBD       B2,NA$LOOP          . LOOP UNTIL DONE
       .                   
       . ALLOCATE ENOUGH TO HOLD ARRAY FROM HEAP
       .
                   LA,W      NA$DIM              . ADD ENOUGH TO HOLD LIMIT TBL
                   LSA       1D
                   A         2D
                   A,W       NA$TOTSZ
                   SA,W      NA$P1           
                   SLJ       FM$GETMEM
       NA$P1       +0
       .
                   LB,W      B5,NA$P1
                   LQ,W      NA$DIM              . XFER # DIMENSIONS TO ARRAY
                   SQ,W      0,B5
                   LQ,W      NA$ITEMSZ           . XFER ITEM SZ TO ARRAY
                   SQ,W      1,B5
                   SB,A      B5                  . BUMP ARRAY BFR POINTER
                   A         2D
                   LB,A      B5      
                   LB        B3,NA$LIMITS        . XFER LIMITS TBL TO ARRAY
       NA$LOOP2    DPL       0,B3
                   DPS       0,B5
                   LB        B3,2D,B3            . BUMP INDEX
                   LB        B5,2D,B5            . BUMP INDEX
                   RD,W      NA$DIM              . DECR LOOP COUNT
                   JT        NA$LOOP2,,ANOT
       .                                                
       NA$RTRN     LQ,W      NA$P1
                   LA,W      NA$A                . RESTORE REGISTERS
                   SB,W      B4,NA$B+3
                   LBW       NA$B
                   J         0,B1 
       . ++++++++++
       . STRARRAY
       .
       . ALLOCATE MEMORY FOR A STRING ARRAY. 
       .
       . PARAMETERS:
       .   SAME AS FOR NEWARRAY WITH THE ADDITION OF A NEW PARAMETER
       .   HOLDING THE LENGHT OF THE STRING IN CHARACTERS.
       .
       . RETURNS:
       .   SAME AS FOR NEWARRAY
       .
       . ++++++++++
       STRARRAY*   SA,W      SA$A                . SAVE REGISTERS
                   SBW       SA$B
                   LBPJB1    POPQ                . SAVE STRING LENGTH
                   SQ,W      SA$LENGTH
       .                   
                   LBPJB1    NEWARRAY
                   SQ,W      SA$Q                . SAVE ARRAY ADDRESS
                   LB,W      B5,SA$Q             . GET # DIMENSIONS
                   LA,W      0,B5
                   LSA       1D                  . POINT TO ARRAY DATA
                   A         2D
                   A,W       SA$Q
                   LB,A      B5
       .                   
                   LQ,W      NS$SPACES           . INITIALIZE ALL STRINGS
       SA$LOOP     R,W       NA$ITEMSZ,,ADV      . TO SPACES
                   SQ,W      0,B5
                   LA,W      SA$LENGTH          
                   SA,W      0,B5
                   SB,A      B5                  . BUMP POINTER
                   A,W       NA$ITEMSZ
                   LB,A      B5
                   LA,W      NA$TOTSZ            . DECREMENT LOOP COUNT
                   AN,W      NA$ITEMSZ
                   SA,W      NA$TOTSZ,,AZERO
                   J         SA$LOOP
       .
                   LA,W      SA$A
                   LQ,W      SA$Q
                   SB,W      B4,SA$B+3
                   LBW       SA$B
                   J         0,B1
       .
       SA$A        RES       1D
       SA$Q        RES       1D
       SA$B        RES       7D
       SA$LENGTH   RES       1D
       SA$DIM      RES       1D                   
       .                   
       . ++++++++++
       . FREEARRAY
       .
       . RELEASE THE MEMORY ASSIGNED TO AN ARRAY
       .
       . PARAMETERS:
       .   ARRAY ADDRESS IN Q
       .
       . RETURNS:
       .   NONE
       . ++++++++++
       FREEARRAY*  SQ,W      $+2
                   SLJ       FM$FREEMEM
                   +0
                   J         0,B1      
       . ++++++++++
       . ARRAYADR
       .
       . CALCULATE THE ADDRESS OF AN ARRAY ITEM
       .
       . PARAMETERS:
       .   INDEX1
       .   .
       .   .
       .   .
       .   INDEXN
       .   ARRAY POINTER
       .   N
       .
       . RETURNS
       .   ADDRESS OF ARRAY ITEM IN A
       .
       . ++++++++++
       ARRAYADR*   SB,L      B1,AA$RTRN          . SAVE RETURN ADDR
                   LQ,W      1,B4                . GET # OF INDEXES
                   SQ,W      NA$DIM
                   LA,W      2,B4                . GET ARRAY BUFFER PTR
                   LB,A      B5                   
                   ANQ,W     0,B5,AZERO          . # DIMENSIONS MATCH?
                   J         AA$BADIDX           . NO, HARD FAIL
                   LQ,W      1,B5                . GET ARRAY ITEM SIZE
                   SQ,W      NA$ITEMSZ
                   LQ,W      NA$DIM              . POINT TO LAST DIMENSION
                   LSQ       1D
                   AQ,A
                   SQ,A
                   LB,A      B5                  . B5 = LAST LOWER LIMIT
                   A         2D                  . POINT TO ARRAY DATA
                   SA,W      NA$ADDR
                   LB        B4,2,B4             . POP 2 ITEMS FROM STACK
       .
       AA$LOOP     LQ,W      1,B4                . GET SUBSCRIPT
                   LB        B4,1,B4             . POP STACK
                   ANQ,W     0,B5                . MAKE ZERO RELATIVE 
                   ZA
                   M,W       NA$ITEMSZ           . GET OFFSET INTO DIMENSION
                   AQ,W      NA$ADDR             . ADD TO TOTAL SO FAR
                   SQ,W      NA$ADDR
                   LQ,W      1,B5                . GET UPPER LIMIT
                   ANQ,W     0,B5                . GET # ITEMS THIS DIMENSION
                   AQ        1D
                   ZA
                   M,W       NA$ITEMSZ           . GET TTL SIZE THIS DIMENSION
                   SQ,W      NA$ITEMSZ 
                   SB,A      B5                  . DECREMENT LIMIT TBL OFFSET
                   AN        2D
                   LB,A      B5
                   RD,W      NA$DIM
                   JT        AA$LOOP,,ANOT
       .       
                   LA,W      NA$ADDR             . RETURN ADDR OF ITEM
       AA$RTRN     J         $
       .
       AA$BADIDX   TYPE      AA$LEN,AA$MSG
                   HALT      EABADIDX       
       .       
       AA$MSG      +'RUN-TIME ERROR: WRONG NUMBER OF ARRAY INDICES'
                   +'    ^'
       AA$LEN      EQU       $-AA$MSG                                      
       . ++++++++++
       . GETARRAYQ
       .
       . DECODE THE ARRAY SUBSCRIPTS PASSED ON THE STACK AND RETURN
       . THE CONTENTS OF THE ADDRESSED ARRAY ITEM IN Q
       .
       . PARAMETERS:
       .   
       .   INDEX1
       .   .
       .   .
       .   .
       .   INDEXN
       .   ARRAY POINTER
       .   N
       .       
       . RETURNS:
       .   ARRAY ITEM IN Q
       . ++++++++++
       GETARRAYQ*  SA,W      NA$A                . SAVE THE REGISTERS
                   SBW       NA$B
       .
                   LBPJB1    ARRAYADR            . DECODE SUBSCRIPTS
       .
                   LB,A      B5                  . GET CONTENTS OF ARRAY ITEM
                   LQ,W      0,B5
       .
                   LA,W      NA$A                . RESTORE REGISTERS
                   SB,W      B4,NA$B+3
                   LBW       NA$B
                   J         0,B1
       . ++++++++++
       . GETARRAYAQ
       .
       . DECODE THE ARRAY SUBSCRIPTS PASSED ON THE STACK AND RETURN
       . THE CONTENTS OF THE ADDRESSED ARRAY ITEM IN AQ
       .
       . PARAMETERS:
       .   SEE GETARRAYQ
       .
       . ++++++++++
       GETARRAYAQ* SBW       NA$B                . SAVE THE REGISTERS
       .
                   LBPJB1    ARRAYADR            . DECODE SUBSCRIPTS
       .
                   LB,A      B5                  . GET CONTENTS OF ARRAY ITEM
                   DPL       0,B5
       .
                   SB,W      B4,NA$B+3
                   LBW       NA$B                . RESTORE THE REGISTERS
                   J         0,B1
       . 
       . ++++++++++
       . GETSARRAY
       .
       . DECODE THE ARRAY SUBSCRIPTS PASSED ON THE STACK AND RETURN
       . THE ADDRESS OF THE STRING ARRAY ITEM IN Q.
       .
       . PARAMETERS:
       .   SEE GETARRAYQ
       .
       . ++++++++++
       GETSARRAY*  SA,W      NA$A                . SAVE THE REGISTERS
                   SBW       NA$B
       .
                   LBPJB1    ARRAYADR            . GET ADDRESS OF ITEM
                   LQ,A
       .
                   LA,W      NA$A                . RESTORE THE REGISTERS
                   SB,W      B4,NA$B+3
                   LBW       NA$B
                   J         0,B1
       .                                                 
       . ++++++++++
       . PUTARRAYQ
       .
       . DECODE THE ARRAY SUBSCRIPTS PASSED ON THE STACK AND UPDATE
       . THE ADDRESSED ARRAY ITEM WITH THE VALUE PASSED AS THE FIRST 
       . PARAMETER.
       .
       . PARAMETERS:
       .   
       .   NEW ARRAY ITEM VALUE
       .   INDEX1
       .   .
       .   .
       .   .
       .   INDEXN
       .   ARRAY POINTER
       .   N
       .   NEW VALUE IN Q
       .       
       . RETURNS:
       .   NEW ARRAY ITEM VALUE IN Q
       . ++++++++++
       PUTARRAYQ*  SA,W      NA$A                . SAVE THE REGISTERS
                   SBW       NA$B
       .
                   LBPJB1    ARRAYADR            . DECODE SUBSCRIPTS
       .
                   LB,A      B5                  
                   LBPJB1    POPQ                . GET NEW VALUE
                   SQ,W      0,B5                . UPDATE CONTENTS OF ITEM
       .
                   LA,W      NA$A                . RESTORE REGISTERS
                   SB,W      B4,NA$B+3
                   LBW       NA$B
                   J         0,B1
       . ++++++++++
       . PUTARRAYAQ
       .
       . DECODE THE ARRAY SUBSCRIPTS PASSED ON THE STACK AND UPDATE
       . THE ADDRESSED ARRAY ITEM WITH THE VALUE PASSED AS THE FIRST 
       . PARAMETER.
       .
       . PARAMETERS:
       .   SEE GETARRAYQ
       .
       . RETURNS:
       .   NEW ARRAY ITEM VALUE IN AQ
       . ++++++++++
       PUTARRAYAQ* SBW       NA$B                . SAVE THE REGISTERS
       .
                   LBPJB1    ARRAYADR            . DECODE SUBSCRIPTS
       .
                   LB,A      B5
                   LBPJB1    POPAQ               . GET NEW VALUE
                   DPS       0,B5                . UPDATE CONTENTS OF ITEM
       .
                   SB,W      B4,NA$B+3
                   LBW       NA$B                . RESTORE THE REGISTERS
                   J         0,B1
       .                   
       NA$DIM      RES       1D                  . # DIMENSIONS (MAX 10)
       NA$LIMITS   RES       20D                 . TBL OF DIM LIMITS
       NA$ITEMSZ   RES       1D
       NA$TOTSZ    RES       1D
       NA$A        RES       1D
       NA$Q        RES       1D
       NA$B        RES       7D
       NA$ADDR     RES       1D
       NA$BFR      RES       1D
       .
       . ++++++++++++++++++++++++++++++++++++++++++++++
       . STRING ROUTINES
       . ++++++++++++++++++++++++++++++++++++++++++++++
       . ++++++++++
       .
       . NEWSTRING       
       .
       . ALLOCATE A NEW STRING FROM THE HEAP
       .
       . PARAMETERS:
       .   STRING LENGTH IN Q
       .
       . RETURNS:
       .   PTR TO STRING IN Q
       .
       . ++++++++++
       NEWSTRING*  SA,W      NA$A
                   SBW       NA$B
       .
                   SQ,W      NS$LEN              . SAVE LENGTH FOR LATER
                   ANQ       1D                  . CALC # WORDS REQUIRED TO
                   ZA                            . HOLD STRING +1 FOR LENGTH
                   D         5D
                   AQ        2D
                   SQ,W      $+2                 . ALLOC STRING BUFFER
                   SLJ       FM$GETMEM           
                   +0
                   LB,W      B5,$-1              . GET BUFFER ADDRESS
                   LA,W      NS$SPACES           . CLR BFR TO SPACES
                   SQ,L      $+1
                   R         0,,ADV                  
                   SA,W      0,B5
                   LA,W      NS$LEN              . SET LENGTH IN CHARACTERS
                   SA,W      0,B5
                   SB,A      B5                  . RETURN ADDR OF STRING                         
                   LQ,A
       .                   
                   LA,W      NA$A
                   LBW       NA$B
                   J         0,B1
       .
       NS$LEN      RES       1D
       NS$SPACES   +'     '
       . ++++++++++
       . FREESTRING
       . 
       . RETURN A STRING BUFFER TO THE HEAP
       .
       . PARAMETERS:
       .   ADDRESS OF STRING BUFFER IN Q
       .
       . RETURNS:
       .   NONE
       .
       . ++++++++++
       FREESTRING* SQ,W      $+2
                   SLJ       FM$FREEMEM
                   +0
                   J         0,B1
       . ++++++++++
       . STRCPY
       .
       . COPY A (SUB)STRING TO ANOTHER (SUB)STRING
       .
       . PARAMETERS:
       .   ALL PARAMETERS ARE PASSED ON THE STACK
       .   SOURCE-STRING-POINTER
       .   SOURCE-START
       .   SOURCE-END
       .   DEST-STRING-POINTER
       .   DEST-START
       .   DEST-END
       .
       . RETURNS:
       .   NONE
       .
       . ++++++++++
       STRCPY*     SLJ       SC$SAVE             . SAVE REGISTERS
                   SLJ       SC$INIT             . GET PARAMS, ETC.
       .
                   LQ,W      SC$DSTART
                   TQ,W      SC$DEND,,YMORE      . PAST END OF DEST?
                   J         SC$RTRN             . YES
                   LQ,W      SC$SSTART
                   TQ,W      SC$SEND,,YMORE      . PAST END OF SOURCE?
                   J         SC$BLANKS           . YES, DEST = SPACES
       .                   
       SC$LOOP     LQ,L      SC$SRC              . SET UP SRC INSTRUCTIONS
                   SQ,L      SC$LOAD1
       .                   
                   LQ,L      SC$DEST             . SET UP DEST INSTRUCTIONS
                   SQ,L      SC$LOAD2
                   SQ,L      SC$STORE
       .                   
       SC$LOAD1    LQ,W      0,B5                . GET SOURCE WORD
                   LSAQ,U    SC$SRC              . GET BYTE INTO A
                   NOT,X     77700               . CLEAR ALL BUT BYTE
                   SA,W      SC$BYTE
       SC$LOAD2    LQ,W      0,B6                . GET DEST WORD
                   LSAQ,U    SC$DEST             . GET BYTE INTO A
                   NOT       77                  . CLEAR IT
                   OR,W      SC$BYTE             . COPY BYTE TO A
                   RSAQ,U    SC$DEST             . BACK INTO Q
       SC$STORE    SQ,W      0,B6                . SAVE TO DEST
       .                   
                   RI,W      SC$DSTART           . BUMP DEST BYTE #
                   TA,W      SC$DEND,,YMORE      . END OF STRING?
                   J         SC$RTRN             . YES
                   LA        6D                  . BUMP DEST SHIFT COUNT
                   RA,U      SC$DEST
                   TA        31D,,YLESS          . NEXT WORD?
                   J         SC$SBUMP            . NO
                   LA        6D                  . YES, RESET SHIFT COUNT
                   SA,U      SC$DEST
                   RI,L      SC$DEST             . BUMP WORD #
       SC$SBUMP    RI,W      SC$SSTART           . BUMP SRC BYTE #
                   TA,W      SC$SEND,,YMORE      . END OF STRING?
                   J         SC$BLANKS           . YES
                   LA        6D                  . BUMP SRC SHIFT COUNT
                   RA,U      SC$SRC                     
                   TA        31D,,YLESS          . NEXT WORD?
                   J         SC$LOOP             . NO
                   LA        6D                  . YES, RESET SHIFT COUNT
                   SA,U      SC$SRC
                   RI,L      SC$SRC              . BUMP WORD #
                   J         SC$LOOP
       .                         
       SC$RTRN     SLJ       SC$RST              . RESTORE REGISTERS
                   J         0,B1
       .
       . RIGHT FILL DESTINATION WITH SPACES
       .
       SC$BLANKS   LQ,L      SC$DEST             . SET UP INSTRUCTIONS
                   SQ,L      SC$LOAD3
                   SQ,L      SC$STORE2
       SC$LOAD3    LQ,W      0,B6
                   LSAQ,U    SC$DEST                     
                   NOT       77
                   OR        ' '
                   RSAQ,U    SC$DEST
       SC$STORE2   SQ,W      0,B6                   
                   RI,W      SC$DSTART           . BUMP DEST BYTE #
                   TA,W      SC$DEND,,YMORE      . END OF STRING?
                   J         SC$RTRN             . YES
                   LA        6D                  . BUMP DEST SHIFT COUNT
                   RA,U      SC$DEST
                   TA        31D,,YLESS          . NEXT WORD?
                   J         SC$BLANKS           . NO
                   LA        6D                  . YES, RESET SHIFT COUNT
                   SA,U      SC$DEST
                   RI,L      SC$DEST             . BUMP WORD #
                   J         SC$BLANKS
       . ++++++++++
       . STRCOMP
       .
       . COMPARE A (SUB)STRING TO ANOTHER (SUB)STRING
       .
       . PARAMETERS:
       .   ALL PARAMETERS ARE PASSED ON THE STACK
       .   SOURCE-STRING-POINTER
       .   SOURCE-START
       .   SOURCE-END
       .   DEST-STRING-POINTER
       .   DEST-START
       .   DEST-END
       .
       . RETURNS:
       .   RESULT OF COMPARISON IN A
       .     -1 IF SOURCE < DEST
       .      0 IF SOURCE = DEST
       .      1 IF SOURCE > DEST
       .
       . ++++++++++
       STRCOMP*    SLJ       SC$SAVE
                   SLJ       SC$INIT  
       .
                   SB,A      B5                  . POINT TO INTIAL WORD
                   A,L       SC$SRC
                   LB,A      B5
                   SB,A      B6
                   A,L       SC$DEST
                   LB,A      B6
                   SZ,W      SC$A                . ASSUME STRINGS ARE EQUAL
       .                             
                   LQ,W      SC$DSTART
                   TQ,W      SC$DEND,,YMORE      . PAST END OF SUBSTRING?
                   J         SCO$CHKLSP          . YES, CHK LEFT FOR ALL SPACES
                   LQ,W      SC$SSTART
                   TQ,W      SC$SEND,,YMORE      . PAST END OF SUBSTRING?
                   J         SCO$CHKRSP          . YES, CHK RIGHT FOR ALL SPACES
       .                   
       SCO$LOOP    LQ,W      0,B6                . GET RIGHT HAND CHAR
                   LSAQ,U    SC$DEST
                   NOT,X     77700
                   SA,W      SC$BYTE
                   LQ,W      0,B5                . GET LEFT HAND CHAR
                   LSAQ,U    SC$SRC
                   NOT,X     77700
                   AN,W      SC$BYTE,,AZERO      . SUBTRACT RIGHT HAND CHAR               
                   J         SCO$NEQ             . NOT EQUAL, WE'RE ALMOST DONE
       .                     
                   RI,W      SC$DSTART           . BUMP DEST BYTE #
                   TA,W      SC$DEND,,YMORE      . END OF STRING?
                   J         SCO$CHKLSP          . YES, CHECK LEFT FOR SPACES
                   LA        6D                  . BUMP DEST SHIFT COUNT
                   RA,U      SC$DEST
                   TA        31D,,YLESS          . NEXT WORD?
                   J         SCO$SBUMP           . NO
                   LA        6D                  . YES, RESET SHIFT COUNT
                   SA,U      SC$DEST
                   TBI       B6,77777            . BUMP WORD PTR
                   NOP
       SCO$SBUMP   RI,W      SC$SSTART           . BUMP SRC BYTE #
                   TA,W      SC$SEND,,YMORE      . END OF STRING?
                   J         SCO$CHKRSP          . YES, CHECK RIGHT FOR SPACES
                   LA        6D                  . BUMP SRC SHIFT COUNT
                   RA,U      SC$SRC                     
                   TA        31D,,YLESS          . NEXT WORD?
                   J         SCO$LOOP            . NO
                   LA        6D                  . YES, RESET SHIFT COUNT
                   SA,U      SC$SRC
                   TBI       B5,77777
                   NOP
                   J         SC$LOOP
       .
       SCO$NEQ     LA,A      0,,APOS
                   LA,X      -1D,,ANEG           . SRC < DEST
                   LA        1D                  . SRC > DEST
                   SA,W      SC$A
       .                   
       SCO$RTRN    SLJ       SC$RST              . RESTORE REGISTERS
                   J         0,B1
       .
       SCO$CHKLSP  LA,W      SC$SSTART           . END OF LEFT STRING?
                   TA,W      SC$SEND,,YMORE      .
                   J         SCO$RTRN            . YES, STRINGS EQUAL
                   LA        6D
                   SA,U      SC$DEST
                   LB        B6,NS$SPACES
                   J         SCO$SBUMP
       .
       SCO$CHKRSP  LA,W      SC$DSTART
                   TA,W      SC$DEND,,YMORE      . END OF RIGHT STRING?
                   J         SCO$RTRN            . YES, STRINGS EQUAL
                   LA        6D
                   SA,U      SC$SRC                   
                   LB        B5,NS$SPACES
                   J         SCO$LOOP
       . ++++++++++
       . INT2STRR
       .
       . CONVERT AN INTEGER IN Q TO THE RIGHT JUSTIFIED STRING GIVEN
       . BY THE STACK PARAMETERS.
       . 
       . PARAMETERS:
       .   INTEGER TO CONVERT IN Q
       .     STACK:
       .   DEST-STRING-POINTER
       .   DEST-START
       .   DEST-END
       .
       . RETURNS:
       .   NONE
       .
       . ++++++++++
       INT2STRR*   SLJ       SC$SAVE
                   SLJ       SC$INEG
                   SQ,W      SC$NUM
                   SLJ       SC$INIT1R
       .                   
       .           CLEAR STRING TO SPACES
       I2S$CLEAR   LQ,W      0,B6                . CLEAR CRNT CHAR TO SPACE
                   LSAQ,U    SC$DEST
                   NOT       77
                   OR        ' '
                   RSAQ,U    SC$DEST
                   SQ,W      0,B6
                   SLJ       SC$DECR             . DECREMENT STRING POINTERS
                   J         I2S$CLEAR
       .
                   SLJ       SC$INIT1R           . RESTORE STRING INFO
       I2S$DIGIT   LQ,W      SC$NUM
                   ZA                            . GET NEXT DIGIT
                   D         10D
                   OR        '0'                 . CVT TO FIELDATA
                   SA,W      SC$BYTE
                   SQ,W      SC$NUM
                   LQ,W      0,B6                             
                   LSAQ,U    SC$DEST
                   NOT       77
                   OR,W      SC$BYTE
                   RSAQ,U    SC$DEST
                   SQ,W      0,B6
                   LA,W      SC$NUM,,ANOT        . NUM = 0?
                   J         I2S$DONE            . YES, WE'RE DONE
                   SLJ       SC$DECR             . DECREMENT STRING POINTERS
                   J         I2S$DIGIT
       .                   
       I2S$DONE    SLJ       SC$DECR             . DECREMENT STRING POINTERS
                   J         $+2
                   J         I2S$RTRN
                   LQ,W      0,B6                . ADD THE SIGN
                   LSAQ,U    SC$DEST
                   NOT       77
                   OR,W      SC$SIGN
                   RSAQ,U    SC$DEST
                   SQ,W      0,B6
       .       
       I2S$RTRN    SB,A      B4                  . RESET STACK PTR
                   A         3D                         
                   LB,A      B4
                   SLJ       SC$RST              . RESTORE REGISTERS
                   J         0,B1
       . ++++++++++
       . FLT2STR
       .
       . CONVERT A FLOATING POINT IN AQ TO 12 CHARACTER SCIENTIFIC FORMAT
       . IN THE STRING GIVEN BY THE STACK PARAMETERS.
       .
       . FORMAT -X.XXXX,+/-NNN
       . 
       . PARAMETERS:
       .   NUMBER TO CONVERT IN AQ
       .     STACK:
       .   DEST-STRING-POINTER
       .   DEST-START
       .   DEST-END
       .
       . RETURNS:
       .   NONE
       .
       . ++++++++++
       FLT2STR*    SLJ       SC$SAVE
                   SLJ       SC$FNEG
                   DPS       F2S$NUM             . SAVE NUMBER FOR LATER
                   SLJ       SC$INIT1R
       .
       . CALCULATE THE POWER OF 10 EQUIVALENT TO THE NUMBER'S EXPONENT.
       . EXP10 = TRUNC(EXP2 * LOG10(2))
       .
                   DPL       F2S$NUM             . GET NUMBER
                   FU        F2S$EXP             . UNPACK NUMBER TO GET EXPONENT
                   LQ,W      F2S$EXP             . UNBIAS IT
                   ANQ       1024D
                   LBPJB1    INT2FLOATQ          . BACK TO FLOAT
                   FM        LOG2                
                   LBPJB1    TRUNC
                   JT        $+2,,QNEG           . RESULT POSITIVE?
                   AQ        1D                  . YES, BUMP BY 1
                   SQ,W      F2S$EXP             . SAVE IT
       .
       . SHIFT THE NUMBER BY THE APPROPRIATE # OF DECIMAL DIGITS TO GET
       . 5 DIGITS TO THE LEFT OF THE DECIMAL.
       . NUM = NUM * POW10(5 - EXP10)
       .
                   DPL       TEN                 . SET UP POWERI PARAMS
                   DPS       F2S$P1
       F2S$LOOP    LQ        5D                  . MAX. PRECISION
                   ANQ,W     F2S$EXP             . - EXP
                   SQ,W      F2S$P2              . = # DECIMAL DIGITS TO SHIFT
                   LBPJB1    POWERI              . POW10(5 - EXP10)
       F2S$P1      RES       2D
       F2S$P2      RES       1D
                   FM        F2S$NUM             . NUM = NUM * POW10(5 - EXP10)
                   DPTL      F2S$10K             . RESULT < REQ'D PRECISION?
                   J         F2S$NUMOK           . NOPE, WE'RE OK
                   RD,W      F2S$EXP             . YES, DECR EXPONENT BY 1
                   J         F2S$LOOP            . AND TRY AGAIN
       F2S$NUMOK   LBPJB1    INTEGER             . ROUND TO INTEGER
                   SQ,W      F2S$NUM
       .
       . FORMAT THE EXPONENT
       .
                   RD,W      F2S$EXP             . DOWN BY 1
                   LA,W      SC$SPACE            . SET SIGN TO POSITIVE
                   SA,W      F2S$SIGN
                   LQ,W      F2S$EXP,,QNEG       . EXP NEGATIVE?
                   J         F2S$ECONT           . NO
                   LA,W      SC$MINUS            . YES
                   SA,W      F2S$SIGN
                   NQ                            . NEGATE ORIGINAL EXP
                   SQ,W      F2S$EXP
       F2S$ECONT   LB        B2,2D               . INIT LOOP COUNT
       F2S$ELOOP   LQ,W      F2S$EXP
                   ZA                            . GET NEXT DIGIT
                   D         10D
                   OR        '0'                 . CVT TO FIELDATA
                   SA,W      SC$BYTE
                   SQ,W      F2S$EXP
                   LQ,W      0,B6                             
                   LSAQ,U    SC$DEST
                   NOT       77
                   OR,W      SC$BYTE
                   RSAQ,U    SC$DEST
                   SQ,W      0,B6
                   SLJ       SC$DECR             . DECREMENT STRING POINTERS
                   JBD       B2,F2S$ELOOP        . LOOP
       .                   
                   LQ,W      0,B6                . ADD EXPONENT SIGN                          
                   LSAQ,U    SC$DEST
                   NOT       77
                   OR,W      F2S$SIGN
                   RSAQ,U    SC$DEST
                   SQ,W      0,B6
                   SLJ       SC$DECR             . DECREMENT STRING POINTERS
                   J         $+2
                   J         F2S$RTRN
       .                   
                   LQ,W      0,B6                . ADD COMMA SEPARATOR
                   LSAQ,U    SC$DEST
                   NOT       77
                   OR,W      F2S$COMMA
                   RSAQ,U    SC$DEST
                   SQ,W      0,B6
                   SLJ       SC$DECR             . DECREMENT STRING POINTERS
                   J         $+2
                   J         F2S$RTRN
       .
       . FORMAT THE NUMBER WITH 4 DECIMALS
       .                   
                   LB        B2,3D
       F2S$NLOOP   LQ,W      F2S$NUM             . GET NEXT DIGIT
                   ZA                                  
                   D         10D
                   OR        '0'                 . CVT TO FIELDATA
                   SA,W      SC$BYTE
                   SQ,W      F2S$NUM
                   LQ,W      0,B6                             
                   LSAQ,U    SC$DEST
                   NOT       77
                   OR,W      SC$BYTE
                   RSAQ,U    SC$DEST
                   SQ,W      0,B6
                   SLJ       SC$DECR             . DECREMENT STRING POINTERS
                   JBD       B2,F2S$NLOOP        . LOOP
                   J         $+2
                   J         F2S$RTRN
       .                   
                   LQ,W      0,B6                . ADD DECIMAL POINT                          
                   LSAQ,U    SC$DEST
                   NOT       77
                   OR,W      F2S$PERIOD
                   RSAQ,U    SC$DEST
                   SQ,W      0,B6
                   SLJ       SC$DECR             . DECREMENT STRING POINTERS
                   J         $+2
                   J         F2S$RTRN
       .                   
                   LQ,W      0,B6                . ADD FINAL DIGIT
                   LSAQ,U    SC$DEST
                   NOT       77
                   OR,W      F2S$NUM
                   OR        '0'                 . CVT TO FIELDATA
                   RSAQ,U    SC$DEST
                   SQ,W      0,B6
                   SLJ       SC$DECR             . DECREMENT STRING POINTERS
                   J         $+2
                   J         F2S$RTRN
       .                   
                   LQ,W      0,B6                . ADD NUMBER'S SIGN
                   LSAQ,U    SC$DEST
                   NOT       77
                   OR,W      SC$SIGN
                   RSAQ,U    SC$DEST
                   SQ,W      0,B6
       .
       F2S$RTRN    SB,A      B4                  . RESET STACK PTR
                   A         3D                         
                   LB,A      B4
                   SLJ       SC$RST              . RESTORE REGISTERS
                   J         0,B1
       .
       TEN         DLD       10.0
       LOG2        DLD       0.30103             . LOG10(2) 
       F2S$EXP     RES       2D
       F2S$NUM     RES       2D
       F2S$10K     DLD       10000.0
       F2S$SIGN    RES       1D
       F2S$COMMA   +0, 0, 0, 0, 56
       F2S$PERIOD  +0, 0, 0, 0, 75
       . ++++++++++
       . SC$INIT
       .
       . COMMON SETUP FOR ROUTINES WITH 2 STRINGS
       .
       . ++++++++++
       SC$INIT     +0                                    
                   LB,W      B5,6,B4             . GET PARAMETERS
                   LQ,W      5,B4                . SRC START INDEX
                   ANQ       1D                  
                   SQ,W      SC$SSTART
                   LQ,W      4,B4
                   TQ,W      0,B5,YMORE          . PAST END OF STRING?
                   LQ,W      0,B5                . YES, FORCE TO END
                   SQ,W      SC$SEND
                   LB,W      B6,3,B4                               
                   LQ,W      2,B4                . DEST START INDEX
                   ANQ       1D
                   SQ,W      SC$DSTART
                   LQ,W      1,B4
                   TQ,W      0,B6,YMORE          . PAST END OF STRING?
                   LQ,W      0,B6                . YES, FORCE TO END
                   SQ,W      SC$DEND                               
                   SB,A      B4                  . RESET STACK PTR
                   A         6D                         
                   LB,A      B4
       .                    
                   LQ,W      SC$DSTART           . GET DEST START
                   ZA                            . NO
                   D         5D                  . INITIAL DEST WORD #
                   AQ        1D
                   SQ,L      SC$DEST
                   A         1D                  . INITIAL DEST SHIFT COUNT
                   LQ,A
                   ZA
                   M         6D
                   SQ,U      SC$DEST
       .
                   LQ,W      SC$SSTART           . GET SOURCE START
                   ZA                            . NO
                   D         5D                  . INITIAL SRC WORD #
                   AQ        1D
                   SQ,L      SC$SRC
                   A         1D                  . INITIAL SRC SHIFT COUNT
                   LQ,A
                   ZA
                   M         6D
                   SQ,U      SC$SRC
       .
                   J,L       SC$INIT
       . ++++++++++
       . SC$INIT1R
       .
       . COMMON SETUP ROUTINE FOR ROUTINES WITH 1 RIGHT JUSTIFIED STRING
       . ++++++++++
       SC$INIT1R   +0
                   LB,W      B6,3,B4                               
                   LQ,W      2,B4                . DEST START INDEX
                   ANQ       1D
                   SQ,W      SC$DSTART
                   LQ,W      1,B4
                   TQ,W      0,B6,YMORE          . PAST END OF STRING?
                   LQ,W      0,B6                . YES, FORCE TO END
                   ANQ       1D                  . MAKE ZERO RELATIVE
                   SQ,W      SC$DEND                               
       .
                   LQ,W      SC$DEND             . GET DEST END
                   ZA
                   D         5D                  . INITIAL DEST WORD #
                   AQ        1D
                   SQ,L      SC$DEST
                   A         1D                  . INITIAL DEST SHIFT COUNT
                   LQ,A
                   ZA
                   M         6D
                   SQ,U      SC$DEST
                   SB,A      B6                  . POINT TO INITIAL WORD
                   A,L       SC$DEST
                   LB,A      B6
       .
                   J,L       SC$INIT1R                   
       .
       SC$SAVE     +0
                   SA,W      SC$A
                   SQ,W      SC$Q
                   SBW       SC$B
                   J,L       SC$SAVE                                     
       .
       SC$RST      +0
                   LA,W      SC$A
                   LQ,W      SC$Q
                   SB,W      B4,SC$B+3
                   LBW       SC$B
                   J,L       SC$RST
       . ++++++++++
       . SC$DECR
       .
       . DECREMENT STRING POINTERS FOR ROUTINES WITH 1 RIGHT JUSTIFIED STRING
       . ++++++++++
       SC$DECR     +0
                   RD,W      SC$DEND             . DECR END CHAR PTR
                   TA,W      SC$DSTART,,YLESS    . REACHED START OF SUBSTRING?
                   J         SCD$DONE            . YES, WE'RE DONE
                   LA,U      SC$DEST             . DECR SHIFT COUNT
                   AN        6D
                   SA,U      SC$DEST,,AZERO      . ZERO?
                   J,L       SC$DECR             . NO, KEEP GOING
                   LA        30D                 . RESET SHIFT COUNT
                   SA,U      SC$DEST
                   JBD       B6,$+1              . DECR WORD PTR
                   J,L       SC$DECR             . KEEP GOING
       SCD$DONE    LA,L      SC$DECR             . STRING FINISHED
                   A         1D                  . RETURN TO P+1
                   LB,A      B1
                   J         0,B1                   
       . ++++++++++
       . SC$INEG
       .
       . IF INTEGER IN Q IS NEGATIVE, SET THE SIGN TO '-' AND COMPLEMENT Q.
       . OTHERWISE MAKE SIGEN ' '.
       . ++++++++++
       SC$INEG     +0
                   JT        SC$IPOS,,QPOS
                   LA,W      SC$MINUS
                   SA,W      SC$SIGN
                   NQ
                   J,L       SC$INEG       
       SC$IPOS     LA,W      SC$SPACE
                   SA,W      SC$SIGN
                   J,L       SC$INEG       
       .                   
       . ++++++++++
       . SC$FNEG
       .
       . IF FLOATING POINT NUMBER IN AQ IS NEGATIVE, SET THE SIGN TO '-'
       . AND COMPLEMENT AQ.
       . OTHERWISE MAKE SIGEN ' '.
       . ++++++++++
       SC$FNEG     +0
                   JT        SC$FPOS,,APOS
                   LB,W      B2,SC$MINUS
                   SB,W      B2,SC$SIGN
                   DPN
                   J,L       SC$FNEG       
       SC$FPOS     LB,W      B2,SC$SPACE
                   SB,W      B2,SC$SIGN
                   J,L       SC$FNEG       
       .                   
       SC$A        RES       1D
       SC$Q        RES       1D
       SC$B        RES       7D
       SC$SSTART   RES       1D
       SC$SEND     RES       1D
       SC$DSTART   RES       1D
       SC$DEND     RES       1D
       SC$SRC      RES       1D
       SC$DEST     RES       1D
       SC$BYTE     RES       1D
       SC$NUM      RES       1D                   
       SC$SIGN     RES       1D
       SC$SPACE    +0, 0, 0, 0, 5
       SC$MINUS    +0, 0, 0, 0, 41
       .       
       . ++++++++++++++++++++++++++++++++++++++++++++++
       . CONVERSTION ROUTINES
       . ++++++++++++++++++++++++++++++++++++++++++++++
       . ++++++++++
       .
       . CONVERT AN INTEGER IN A TO A FLOATING POINT NUMBER IN AQ
       .
       .
       .           LBPJB1    INT2FLOAT
       .
       . ++++++++++
       INT2FLOATA* SZ,W      IF$NEG              . CLEAR NEGATIVE FLAG
                   JT        IF$POS,,APOS
                   LQ        1                   . A < 0, SET NEG FLAG
                   SQ,W      IF$NEG              
                   NA                            . & MAKE A POSITIVE
       IF$POS      SFS                           . GET # BITS LEFT OF MSB
                   NQ
                   AQ        1024D+28D+20D       . CALC EXPONENT
                   SQ,W      IF$TEMP
                   LQ,A                          . GET NUMBER IN AQ
                   ZA
                   FP        IF$TEMP             . COMBINE A&EXP INTO FLOATING POINT
                   DPS       IF$TEMP             . NEGATE AQ IF REQD
                   LA,W      IF$NEG,,ANOT
                   J         IF$RTRN
                   DPL       IF$TEMP
                   DPN
                   DPS       IF$TEMP
       IF$RTRN     DPL       IF$TEMP
                   J         0,B1
       . ++++++++++
       .
       . CONVERT AN INTEGER IN Q TO A FLOATING POINT NUMBER IN AQ
       .
       .
       .           LBPJB1    INT2FLOAT
       .
       . ++++++++++
       INT2FLOATQ* SQ,A
                   J         INT2FLOATA
       . ++++++++++
       . SIGNQ
       .
       . RETURNS -1 IF Q IS NEGATIVE, 0 IF Q IS ZERO OR 1 IF Q IS POSITIVE.
       . RESULT RETURNED IN Q
       . ++++++++++
       SIGNQ*      SA,W      MATH$A
                   SQ,A
                   JT        SQ$NEG,,ANEG
                   JT        SQ$ZERO,,AZERO
                   LQ        1D                  . POSITIVE
                   J         SQ$RTRN
       SQ$NEG      LQ,X      -1D
                   J         SQ$RTRN
       SQ$ZERO     LQ,0
       SQ$RTRN     LA,W      MATH$A
                   J         0,B1                                      
       . ++++++++++
       . SIGNAQ
       .
       . RETURNS -1 IF AQ IS NEGATIVE, 0 IF AQ IS ZERO OR 1 IF AQ IS POSITIVE.
       . RESULT RETURNED IN Q
       . ++++++++++
       SIGNAQ*     JT        SAQ$NEG,,ANEG
                   JT        SAQ$ZERO,,AZERO
                   LQ        1D                  . POSITIVE
                   J         SAQ$RTRN
       SAQ$NEG     LQ,X      -1D
                   J         SAQ$RTRN
       SAQ$ZERO    LQ,0
       SAQ$RTRN    J         0,B1                                      
       .
       MATH$A      RES       1D       
       IF$TEMP     RES       2D
       IF$NEG      RES       1D                   
       . ++++++++++
       . TRUNC
       .
       . RETURN THE INTEGER PART OF THE FLOATING POINT NUMBER IN AQ
       . AS A 60-BIT INTEGER IN AQ.
       . ++++++++++
       TRUNC*      DPS       IF$TEMP             . SAVE ORIGINAL NUMBER
                   SZ,W      IF$NEG              . ASSUME NUMBER IS POSITIVE
                   JT        T$POS,,APOS         . IS IT NEGATIVE?
                   DPN                           . YES, MAKE MANTISSA POSITIVE
                   DPS       IF$TEMP
                   LA        1D                  . SET NEGATIVE FLAG
                   SA,W      IF$NEG
       T$POS       DPL       IF$TEMP             . RESTORE NUMBER
                   FU        T$EXP               . UNPACK FLOAT
                   DPS       IF$TEMP             . SAVE MANTISSA
                   LA,W      T$EXP
                   AN        1024D,,ANEG         . UNBIAS THE EXPONENT
                   J         T$POS2
                   ZA                            . EXPONENT < 0 THEN INT = 0
                   LQ,A
                   J         0,B1                . AND WE'RE DONE
       T$POS2      LQ        48D                 . # BITS TO SHIFT = 48 - EXP
                   ANQ,A
                   SQ,W      T$EXP               . SHIFT TO ELIMINATE FRACTION
                   DPL       IF$TEMP
                   RSAQ,L    T$EXP
                   DPS       IF$TEMP                
                   LA,W      IF$NEG,,ANOT        . IS NEGATIVE FLAG SET?
                   J         T$RTRN              . NO
                   DPL       IF$TEMP             . NEGATIVE FLAG IS SET
                   DPN                           . SO MAKE NEGATIVE
                   DPS       IF$TEMP
       .            
       T$RTRN      DPL       IF$TEMP
                   J         0,B1
       .
       T$EXP       RES       1D
                   
       . ++++++++++
       . FRAC
       .
       . RETURN THE FRACTION PART OF THE FLOATING POINT NUMBER IN AQ
       . AS A FLOATING POINT NUMBER IN AQ
       . ++++++++++
       FRAC*       SB,W      B2,F$B2
                   DPS       F$ORIG              . SAVE ORIGINAL NUMBER
                   DPS       IF$TEMP
                   SZ,W      IF$NEG              . ASSUME NUMBER IS POSITIVE
                   JT        F$POS,,APOS         . IS IT NEGATIVE?
                   DPN                           . YES, MAKE MANTISSA POSITIVE
                   DPS       IF$TEMP
                   LA        1D                  . SET NEGATIVE FLAG
                   SA,W      IF$NEG
       F$POS       DPL       IF$TEMP             . RESTORE NUMBER
                   FU        T$EXP               . UNPACK FLOAT
                   DPS       IF$TEMP             . SAVE THE MANTISSA
                   LA,W      T$EXP
                   AN        1024D,,ANEG         . UNBIAS THE EXPONENT
                   J         F$POS2
                   DPL       F$ORIG              . EXPONENT < 0 THEN FRAC=PARAM
                   LB,W      B2,F$B2
                   J         0,B1                . AND WE'RE DONE
       F$POS2      AN        1D                  . DEC BY 1 FOR LOOP
                   SA,W      T$EXP               . SHIFT TO ELIMINATE FRACTION
                   DPL       IF$TEMP
                   LB,W      B2,T$EXP            . THIS NEEDS TO BE DONE IN A
       F$SHIFT     LSAQ      1D                  . LOOP BECAUSE LEFT SHIFTS ARE
                   LRSQ      1D                  . CIRCULAR
                   LSQ       1D
                   JBD       B2,F$SHIFT
                   DPS       IF$TEMP
                   LQ,W      F$LO18              . LOSE SHIFTED OUT BITS
                   LLP,A     
                   SA,W      IF$TEMP
                   DPL       IF$TEMP                
                   DPTE      F$ZERO              . MANTISSA = 0 THEN RESULT = 0
                   J         F$NOTZERO           . NO
                   SZ,W      IF$TEMP             . YES
                   SZ,W      IF$TEMP+1
                   J         F$RTRN 
       .                   
       F$NOTZERO   FP        F$EXP0              . ADD EXP OF ZERO AND NORMALIZE
                   DPS       IF$TEMP               
                   LA,W      IF$NEG,,ANOT        . IS NEGATIVE FLAG SET?
                   J         F$RTRN              . NO
                   DPL       IF$TEMP             . NEGATIVE FLAG IS SET
                   DPN                           . SO MAKE NEGATIVE
                   DPS       IF$TEMP
       .            
       F$RTRN      LB,W      B2,F$B2
                   DPL       IF$TEMP
                   J         0,B1
       .                   
       F$B2        RES       1D
       F$ORIG      RES       2D
       F$LO18      +777777
       F$ZERO      DLD       0.0
       F$EXP0      +2000
       . ++++++++++
       . ENTIER
       .
       . RETURN THE INTEGER PART OF THE FLOATING POINT NUMBER IN AQ
       . SUCH THAT THE RESULT IS NOT GREATER THAN THE ORIGINAL. 
       . I.E. ENTIER(3.5) = 3 WHILE ENTIER(-4.5) = -5.
       .
       . RESULT RETURNED AS A 60-BIT INTEGER IN AQ
       . ++++++++++
       ENTIER*     SB,W      B1,E$B1
                   DPS       E$ORIG              . SAVE ORIGINAL #
                   LBPJB1    TRUNC               . GET INTEGER PART
                   JT        E$RTRN,,APOS        . IF RESULT POSITIVE, WE'RE DONE 
                   DPS       E$TEMP              . NEGATIVE   
                   DPL       E$ORIG              . GET FRACTIONAL PART
                   LBPJB1    FRAC
                   DPTE      F$ZERO              . FRAC = ZERO?
                   J         E$ADJUST            . NO, NEED TO ADJUST RESULT
       E$RTRN      LB,W      B1,E$B1
                   J         0,B1
       .            
       E$ADJUST    DPL       E$TEMP              . RESULT = RESULT - 1
                   DPAN      E$ONE
                   J         E$RTRN
       .
       E$ORIG      RES       2D
       E$B1        RES       1D
       E$TEMP      RES       2D
       E$ONE       DLD       1
       . ++++++++++
       . INTEGER
       .
       . RETURN THE INTEGER PART OF THE FLOATING POINT NUMBER IN AQ
       . ROUNDED. RESULT RETURNED IN AQ
       .
       . ++++++++++
       INTEGER*    FA        I$POINT5
                   J         ENTIER
       .
       I$POINT5    DLD       0.5
       . 
       . +++++++++++++++++++++++++++++++++++++++++++++++++++
       . FREE MEMORY LIST FOR THE HEAP
       . +++++++++++++++++++++++++++++++++++++++++++++++++++
       ALG$HEAP    L$IST              
       AH$HEAD     FM$CB
                   END