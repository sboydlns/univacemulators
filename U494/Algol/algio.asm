                   START
       .                   
       . UNIVAC 494 ALGOL I/O LIBRARY                   
       .
                   ER$DEF
       .
       FT$UNKNOWN  EQU       0D
       FT$PRINT    EQU       1D
       FT$PUNCH    EQU       2D
       FT$READ     EQU       3D
       FT$CONSOLE  EQU       4D
       FT$SEQFILE  EQU       5D
       .
                   XREF      PUSHA, PUSHQ, INT2STRR, STRCPY, FLT2STR                   
       .                                      
       . ++++++++++
       . SETSYSFILE
       .
       . SETS A FLAG INDICATING WHAT KIND OF SYSTEM FILE (READER, PRINTER,
       . PUNCH, CONSOLE, ETC. IS TO BE USED BY THE I/O ROUTINES. THE
       . SELECTION REMAINS IN EFFECT UNTIL SETFILE IS CALLED AGAIN.
       .
       . PARAMETERS:
       .   FILE-TYPE IN Q
       .
       . RETURNS:
       .   NONE
       . ++++++++++            
       SETSYSFILE* SLJ       IO$SAVE
                   SQ,W      FILE$TYPE       
                   LA        72D                 . DFLT I/O BFR T0 72 CHARS
                   TQ        2D,,YLESS
                   LA        120D                . 102 CHARS FOR PRINTER
                   SA,W      IOB$SIZE
                   SLJ       IO$BINIT            . INIT THE I/O BFR
                   SLJ       IO$RST
                   J         0,B1       
       .       
       . ++++++++++
       . WR$FLUSH
       .
       . WRITE THE I/O BUFFER TO THE APPROPRIATE DEVICE AND INIT THE BUFFER
       . ++++++++++
       WR$FLUSH*   SLJ       IO$SAVE
                   SLJ       IO$WRITE
                   SLJ       IO$RST
                   J         0,B1
       .                   
       . ++++++++++
       . WR$INT
       .
       . WRITE AN UNFORMATTED INTEGER TO THE I/O BUFFER
       .
       . PARAMETERS:
       .   INTEGER TO PRINT IN Q
       .
       . RETURNS:
       .   NONE
       . ++++++++++
       WR$INT*     SLJ       IO$SAVE
                   SLJ       IO$BFULL            . CHECK IF BUFFER IS FULL
                   LA        IOB$SIZE            . SET UP STRING PARAMS
                   LBPJB1    PUSHA
                   LA,W      IOB$OFST
                   LBPJB1    PUSHA
                   A         11D
                   LBPJB1    PUSHA
                   LBPJB1    INT2STRR            . CVT TO STRING
                   SLJ       IO$BINC             . BUMP OFFSET
       
                   SLJ       IO$RST
                   J         0,B1      
       .
       . ++++++++++
       . WR$REAL
       .
       . WRITE AN UNFORMATTED FLOATING POINT TO THE I/O BUFFER
       .
       . PARAMETERS:
       .   FLOATING POINT TO PRINT IN AQ
       .
       . RETURNS:
       .   NONE
       . ++++++++++
       WR$REAL*    SLJ       IO$SAVE
                   SLJ       IO$BFULL            . CHECK IF BUFFER IS FULL
                   LA        IOB$SIZE            . SET UP STRING PARAMS
                   LBPJB1    PUSHA
                   LA,W      IOB$OFST
                   LBPJB1    PUSHA
                   A         11D
                   LBPJB1    PUSHA
                   DPL       IO$A                . RESTORE AQ
                   LBPJB1    FLT2STR             . CVT TO STRING
                   SLJ       IO$BINC             . BUMP OFFSET
       .       
                   SLJ       IO$RST
                   J         0,B1      
       .
       . ++++++++++
       . WR$BOOL
       .
       . WRITE AN UNFORMATTED TRUE/FALSE TO THE I/O BUFFER
       .
       . PARAMETERS:
       .   BOOLEAN TO PRINT IN Q
       .
       . RETURNS:
       .   NONE
       . ++++++++++
       WR$BOOL*    SLJ       IO$SAVE
                   SLJ       IO$BFULL            . CHECK IF BUFFER IS FULL
                   SQ,A
                   JT        WR$TRUE,,ANOT       . VALUE = TRUE?
                   LA        FALSE               . NO
                   LBPJB1    PUSHA
                   LA        1D
                   LBPJB1    PUSHA
                   LA,W      FALSE
                   LBPJB1    PUSHA
                   J         WR$BCONT
       WR$TRUE     LA        TRUE                . YES
                   LBPJB1    PUSHA
                   LA        1D
                   LBPJB1    PUSHA
                   LA,W      TRUE
                   LBPJB1    PUSHA
       WR$BCONT    LA        IOB$SIZE            . SET UP STRING PARAMS
                   LBPJB1    PUSHA
                   LA,W      IOB$OFST
                   LBPJB1    PUSHA
                   A         11D
                   LBPJB1    PUSHA
                   LBPJB1    STRCPY
                   SLJ       IO$BINC             . BUMP OFFSET
       .       
                   SLJ       IO$RST
                   J         0,B1      
       .
       . ++++++++++
       . WR$STR
       .
       . WRITE A STRING IN A RECORD OF IT'S OWN
       .
       . PARAMETERS:
       .   ALL PARAMETERS PASSED ON STACK
       .   POINTER-TO-STRING
       .   START-BYTE-NUM
       .   END-BYTE-NUM
       .
       . RETURNS:
       .   NONE
       .
       . ++++++++++
       WR$STR*     SLJ       IO$SAVE
                   LA,W      IOB$OFST            . GET BFR OFFSET
                   TA        2D,,YMORE           . BFR EMPTY?
                   SLJ       IO$WRITE            . NO, FLUSH IT
                   LQ        IOB$SIZE            . SET DEST FOR STRPY
                   LBPJB1    PUSHQ
                   LQ        1D
                   LBPJB1    PUSHQ
                   LQ,W      IOB$SIZE
                   LBPJB1    PUSHQ
                   LBPJB1    STRCPY
                   SLJ       IO$WRITE                   
       .       
                   SLJ       IO$RST
                   J         0,B1      
       .
       . ++++++++++
       . WR$IARRAY
       .
       . WRITE THE CONTENTS OF AN INTEGER ARRAY TO THE CURRENT DEVICE.
       .
       . PARAMETERS:
       .   POINTER TO ARRAY IN Q
       .
       . RETURNS:
       . NONE
       . ++++++++++
       WR$IARRAY*  SLJ       IO$ARSAVE
                   SLJ       IO$BFULL            . CHECK IF BUFFER IS FULL
                   SLJ       IO$ARINIT           . GET ARRAY INFO TO WORK AREA
       .       
       WRIA$LOOP   SLJ       IO$ARNEXT           . GET ADDR OF NEXT ITEM
                   JT        WRIA$RTRN,,ANEG            
                   LB,A      B5
                   LQ,W      0,B5
                   LBPJB1    WR$INT
                   J         WRIA$LOOP
       .              
       WRIA$RTRN   SLJ       IO$ARRST
                   J         0,B1
       .
       . ++++++++++
       . WR$RARRAY
       .
       . WRITE THE CONTENTS OF A FLOATING POINT ARRAY TO THE CURRENT DEVICE.
       .
       . PARAMETERS:
       .   POINTER TO ARRAY IN Q
       .
       . RETURNS:
       . NONE
       . ++++++++++
       WR$RARRAY*  SLJ       IO$ARSAVE
                   SLJ       IO$BFULL            . CHECK IF BUFFER IS FULL
                   SLJ       IO$ARINIT           . GET ARRAY INFO TO WORK AREA
       .       
       WRRA$LOOP   SLJ       IO$ARNEXT           . GET ADDR OF NEXT ITEM
                   JT        WRRA$RTRN,,ANEG            
                   LB,A      B5
                   DPL       0,B5
                   LBPJB1    WR$REAL
                   J         WRRA$LOOP
       .              
       WRRA$RTRN   SLJ       IO$ARRST
                   J         0,B1
       .
       . ++++++++++
       . WR$BARRAY
       .
       . WRITE THE CONTENTS OF A BOOLEAN ARRAY TO THE CURRENT DEVICE.
       .
       . PARAMETERS:
       .   POINTER TO ARRAY IN Q
       .
       . RETURNS:
       . NONE
       . ++++++++++
       WR$BARRAY*  SLJ       IO$ARSAVE
                   SLJ       IO$BFULL            . CHECK IF BUFFER IS FULL
                   SLJ       IO$ARINIT           . GET ARRAY INFO TO WORK AREA
       .       
       WRBA$LOOP   SLJ       IO$ARNEXT           . GET ADDR OF NEXT ITEM
                   JT        WRBA$RTRN,,ANEG            
                   LB,A      B5
                   LQ,W      0,B5
                   LBPJB1    WR$BOOL
                   J         WRBA$LOOP
       .              
       WRBA$RTRN   SLJ       IO$ARRST
                   J         0,B1
       .
       . ++++++++++
       . WR$SARRAY
       .
       . WRITE THE CONTENTS OF A STRING ARRAY TO THE CURRENT DEVICE.
       .
       . PARAMETERS:
       .   POINTER TO ARRAY IN Q
       .
       . RETURNS:
       . NONE
       . ++++++++++
       WR$SARRAY*  SLJ       IO$ARSAVE
                   SLJ       IO$BFULL            . CHECK IF BUFFER IS FULL
                   SLJ       IO$ARINIT           . GET ARRAY INFO TO WORK AREA
       .       
       WRSA$LOOP   SLJ       IO$ARNEXT           . GET ADDR OF NEXT ITEM
                   JT        WRSA$RTRN,,ANEG            
                   LB,A      B5
                   LQ,W      0,B5
                   LBPJB1    WR$STR
                   J         WRSA$LOOP
       .              
       WRSA$RTRN   SLJ       IO$ARRST
                   J         0,B1
       .
       . ++++++++++
       . IO$ARINIT
       .
       . ARRAY I/O INITIALIZATION. EXTRACT THE INDEX INFO TO A WORK AREA FOR
       . EASY ACCESS AND MAKE ALL INDEX REFERENCES ZERO RELATIVE.
       . ADDRESS OF ARRAY PASSED IN Q.
       . ++++++++++
       IO$ARINIT   +0
                   SZ,W      ARR$DONE            . CLEAR ARRAY EXHAUSTED FLAG
                   SQ,W      ARR$DATA            . SAVE ARRAY BUFFER ADDRESS
                   SQ,A                          . GET ARRAY ADDRESS
                   LB,A      B6
                   LA,W      1,B6                . GET ITEM SIZE
                   SA,W      ARR$SIZE
                   LA,W      0,B6                . GET # OF INDICES
                   SA,W      ARR$DIM
                   AN        1D
                   LB,A      B7
                   LB        B5,ARR$IDX          . POINT TO WORK AREA
       IOA$LOOP    LQ,W      2,B6                . COPY LOWER LIMIT TO WA
                   SQ,W      0,B5
                   LQ,W      3,B6                . CALC # ITEMS
                   ANQ,W     0,B5
                   AQ        1D
                   SQ,W      1,B5
                   SZ,W      0,B5                . MAKE LOWER LIMIT ZERO
                   LB        B5,2,B5             . BUMP POINTERS
                   LB        B6,2,B6
                   JBD       B7,IOA$LOOP         . LOOP UNTIL DONE
       .
       . CALCULATE ACTUAL SIZE OF EACH DIMENSION
       .
                   LA,W      ARR$SIZE            . INIT DIMENSION SIZES 
                   R         10D,,ADV            . TO ITEM SIZE
                   SA,W      ARR$DIMSZ
                   LA,W      ARR$DIM             . GET LOOP COUNT
                   AN        1D           
                   LB,A      B5                  
                   LSA       1D
                   LB,A      B6
       IOA$LOOP2   LQ,W      ARR$IDX+1,B6        . # ITEMS THIS DIMENSION
                   ZA                            . # SIZE OF THIS DIMENSION
                   M,W       ARR$DIMSZ+1,B5                   
                   SQ,W      ARR$DIMSZ,B5
                   SB,A      B6                  . DECREMENT INDEXES
                   AN        2D
                   LB,A      B6
                   JBD       B5,IOA$LOOP2        . AND LOOP UNTIL DONE
       .                   
       IOA$RTRN    LA,W      ARR$DIM             . CALC ADDR OF ARRAY DATA
                   LSA       1D
                   A         2D
                   RA,W      ARR$DATA
                   J,L       IO$ARINIT 
       . ++++++++++
       . IO$ARNEXT
       .
       . RETURN THE ADDRESS OF THE NEXT ARRAY ITEM BASED ON THE CONTENTS
       . OF THE INDEX WORK AREA. RETURN THE ADDRESS IN A. RETURN -1 IF
       . NO MORE ITEMS. INDEXES ARE INCREMENTED FROM LEFT TO RIGHT.
       . ++++++++++
       IO$ARNEXT   +0
                   LA,W      ARR$DONE,,AZERO     . ARRAY EXHAUSTED
                   J         ION$NOMORE          . YES                   
                   LB        B5,0                . INIT INDEXES
                   LB        B6,0
                   SZ,W      ION$TEMP
                   LB,W      B7,ARR$DIM          . INIT LOOP COUNT
                   JBD       B7,$+1
       ION$LOOP    LQ,W      ARR$IDX,B5          . GET CRNT IDX THIS DIMENSION
                   ZA                            . CALC OFFSET TO THIS IDX
                   M,W       ARR$DIMSZ+1,B6      . OFFSET TO 1ST OF THIS DIM
                   AQ,W      ION$TEMP            . BUMP ADDRESS SO FAR
                   SQ,W      ION$TEMP            . SAVE IT
                   LB        B5,2,B5             . BUMP INDEXES
                   LB        B6,1,B6       
                   JBD       B7,ION$LOOP
       .
                   LB        B5,0                . INIT INDEX
                   LB,W      B7,ARR$DIM          . INIT LOOP COUNT
                   JBD       B7,$+1
       ION$LOOP2   RI,W      ARR$IDX,B5
                   TA,W      ARR$IDX+1,B5,YLESS  . END OF IDX?
                   J         ION$DONE            . NO, WE'RE DONE
                   SZ,W      ARR$IDX,B5          . YES, RESET CURRENT IDX
                   LB        B5,2,B5
                   JBD       B7,ION$LOOP2
                   LA        1D                  . WE'VE EXHAUSTED ALL INDEXED
                   SA,W      ARR$DONE            . SET ARRAY EXHAUSTED FALG
       .       
       ION$DONE    LA,W      ION$TEMP            . CALC ADDR OF ARRAY ITEM
                   A,W       ARR$DATA
       ION$RTRN    J,L       IO$ARNEXT                  
       .
       ION$NOMORE  LA,X      -1D                 . SHOW END OF ARRAY
                   J         ION$RTRN
       .                   
       ION$TEMP    RES       1D
       .                   
       IO$SAVE     +0                            . SAVE REGISTERS
                   DPS       IO$A
                   SBW       IO$B
                   J,L       IO$SAVE
       .                   
       IO$RST      +0                            . RESTORE REGISTERS
                   DPL       IO$A
                   SB,W      B4,IO$B+3
                   LBW       IO$B
                   J,L       IO$RST 
       .
       IO$ARSAVE   +0                            . SAVE REGISTERS 4 ARRAY RTNS
                   DPS       IOA$A
                   SBW       IOA$B
                   J,L       IO$ARSAVE
       .
       IO$ARRST   +0                            . RESTORE REGISTERS 4 ARRAY RTNS
                   DPL       IOA$A
                   SB,W      B4,IOA$B+3
                   LBW       IOA$B
                   J,L       IO$ARRST 
       .                   
       . ++++++++++
       . IO$BFULL
       .
       . IF I/O BUFFER IS FULL, WRITE AND CLEAR IT
       . ++++++++++
       IO$BFULL    +0
                   LA,W      IOB$OFST            . CHECK IF BUFFER FULL
                   TA,W      IOB$SIZE,,YLESS
                   J,L       IO$BFULL            . NOT FULL, RETURN
                   SLJ       IO$WRITE
                   SLJ       IO$BINIT
                   J,L       IO$BFULL
       . ++++++++++
       . IO$BINIT
       .
       . CLEAR I/O BUFFER TO SPACES AND RESET CURRENT OFFSET
       . ++++++++++
       IO$BINIT    +0
                   LA        1D                  . INIT CRNT OFFSET
                   SA,W      IOB$OFST
                   LA,W      SPACES              . CLEAR I/O BFR TO SPACES
                   R         24D,,ADV
                   SA,W      IO$BFR
                   J,L       IO$BINIT
       . ++++++++++
       . IO$BINC
       .
       . BUMP THE BUFFER OFFSET BY 12 FOR NON-FORMATTED WRITES
       . ++++++++++
       IO$BINC     +0
                   LA        12D
                   RA,W      IOB$OFST
                   J,L       IO$BINC
       . ++++++++++
       . IO$WRITE
       . 
       . WRITE THE BUFFER TO THE APPROPRIATE DEVICE & INIT THE BUFFER
       . ++++++++++
       IO$WRITE    +0                   
                   LA,W      CR
                   SA,W      IO$BFR+16D
                   EXRN      ERTYPE$
                   +17D
                   +IO$BFR
                   SLJ       IO$BINIT
                   J,L       IO$WRITE
       .
       IO$A        RES       1D
       IO$Q        RES       1D
       IO$B        RES       7D
       IOA$A       RES       1D
       IOA$Q       RES       1D
       IOA$B       RES       7D
       .
       ARR$DONE    RES       1D
       ARR$DATA    RES       1D
       ARR$SIZE    RES       1D
       ARR$DIM     RES       1D                  . MAX 10 INDICES
       ARR$IDX     RES       21D
       ARR$DIMSZ   RES       11D                 . MEMORY SIZE OF EACH DIM.
       .
       FILE$TYPE   RES       1D
       IOB$SIZE    RES       1D                  . MAX SIZE OF I/O BFR
       IO$BFR      RES       24D                 . I/O BUFFER
       IOB$OFST    RES       1D                  . CRNT OFFSET INTO I/O BFR
       .
       SPACES      +'     '
       CR          +'    ^'
       TRUE        +4
                   +'TRUE'
       FALSE       +5
                   +'FALSE'
       .                                                
                   END