                   START     INIT
       .
       . A MINIMUM OPERATING SYSTEM FOR THE UNIVAC 494 EMULATOR
       .
       . ALL CODE IN THIS MODULE ASSUMES EXECUTIVE PRIVILEDGE AND
       . ABSOLUTE ADDRESSING.
       . 
       . EXECUTIVE REGISTER USAGE:
       .   B4 - INTERRUPT RETURN ADDRESS. DO NOT USE THIS REGISTER
       .        OUTSIDE OF ANY INTERRUPT HANDLER.
       .
       .   B6 - NON-INTERRUPT SUBROUTINE RETURN ADDRESS. 
       .
       . MISC
       .
       BCW         FORM      12D,1D,17D
       FH8CMD      FORM      6D,1D,23D
       .
                   LCB$DEF
                   L$ITEM$DEF
                   IO$REQ$DEF
                   TCB$DEF
                   IFR$DEF
                   FH8$DEF
                   CARD$DEF
                   PRN$DEF
                   FMCB$DEF 
                   MFD$DEF
                   WA$DEF
                   ER$DEF
                   DTF$DEF
       . ++++++++++
       . INTERRUPT HANDLERS
       . ++++++++++
                   LBPJB4    IL$                 . ILLEGAL INSTRUCTION
                   LBPJB4    PP$                 . PROGRAM PROTECTION
                   RES       7-$
                   LBPJB4    ER$                 . EXECUTIVE RETURN
                   LBPJB4    FFU$                . FLOATING POINT UNDERFLOW
                   LBPJB4    FFO$                . FLOATING POINT OVERFLOW
                   RES       14-$
                   LBPJB4    RT$                 . REAL TIME CLOCK
                   LBPJB4    DC$                 . DAY CLOCK
                   RES       24-$
                   LBPJB4    ISI$EXT             . ISI EXTERNAL INTERRUPT
                   LBPJB4    ISI$IN              . ISI INPUT MONITOR
                   LBPJB4    ISI$OUT             . ISI OUTPUT MONITOR 
                   RES       30-$
                   LBPJB4    TS$                 . TEST AND SET                  
                   RES       140-$
       . ++++++++++
       . CONSTANTS
       . ++++++++++
                   IFR$CONST                  
       CHANZERO    +0
       CHANCARD    +1
       CHANPRN     +2
       . ++++++++++
       . I/O QUEUES
       . ++++++++++
       CONS$OUTQ   L$IST                         . CONSOLE OUTPUT QUEUE
       CARD$Q      L$IST                         . CARD PERIPERAL QUEUE
       PRN$Q       L$IST                         . PRINTER QUEUE
       FH8$CHAN5Q  L$IST                         . QUEUE FOR FH880 ON CHANNEL 5
       .
       CRDR$INUSE  +0                            . CARD READER IN USE
       CPUN$INUSE  +0                            . CARD PUNCH IN USE 
       CPRN$INUSE  +0                            . PRINTER IN USE
       . ++++++++++
       . LIST OF INSTALLED EMULATED FH880 DRUMS
       . ++++++++++
       FH8$DEVS    L$IST                         
       FH8$CHAN5   FH8$DCB   5,FH8$CHAN5Q        . FCB FOR FH880 ON CHANNEL 5
       . ++++++++++
       . MEMORY MANAGER STUFF
       . ++++++++++
       FM$LIST     L$IST                         . LIST OF FREE MEMORY BLOCKS       
       . ++++++++++
       . TABLE OF ACTIVE JOBS
       .
       . THERE CAN BE A MAXIMUM OF 7 JOBS ACTIVE AT ONE TIME
       . ++++++++++
       JOBS$       +0
                   +0
                   +0
                   +0
                   +0
                   +0
                   +0
       . ++++++++++
       . JUMP TABLE USED TO EXECUTE THE HANDLER FOR A PARTICULAR
       . CHANNEL'S I/O INTERRUPT HANDLER
       . ++++++++++
       J$IMON      EQU       $
                   J         CONS$IMON
                   J         CARD$IMON       
                   J         PRN$IMON       
                   J         ILL$DEV       
                   J         ILL$DEV       
                   J         FH8$IMON
                   J         ILL$DEV       
                   J         ILL$DEV       
                   J         ILL$DEV       
                   J         ILL$DEV       
                   J         ILL$DEV       
                   J         ILL$DEV       
                   J         ILL$DEV       
                   J         ILL$DEV       
                   J         ILL$DEV       
                   J         ILL$DEV       
                   J         ILL$DEV       
                   J         ILL$DEV       
                   J         ILL$DEV       
                   J         ILL$DEV       
                   J         ILL$DEV       
                   J         ILL$DEV       
                   J         ILL$DEV       
                   J         ILL$DEV       
       J$OMON      EQU       $
                   J         CONS$OMON                   
                   J         CARD$OMON       
                   J         PRN$OMON       
                   J         ILL$DEV       
                   J         ILL$DEV       
                   J         FH8$OMON
                   J         ILL$DEV       
                   J         ILL$DEV       
                   J         ILL$DEV       
                   J         ILL$DEV       
                   J         ILL$DEV       
                   J         ILL$DEV       
                   J         ILL$DEV       
                   J         ILL$DEV       
                   J         ILL$DEV       
                   J         ILL$DEV       
                   J         ILL$DEV       
                   J         ILL$DEV       
                   J         ILL$DEV       
                   J         ILL$DEV       
                   J         ILL$DEV       
                   J         ILL$DEV       
                   J         ILL$DEV       
                   J         ILL$DEV
       J$EXT       EQU       $
                   J         CONS$EXT
                   J         CARD$EXT       
                   J         PRN$EXT       
                   J         ILL$DEV       
                   J         ILL$DEV       
                   J         FH8$EXT
                   J         ILL$DEV       
                   J         ILL$DEV       
                   J         ILL$DEV       
                   J         ILL$DEV       
                   J         ILL$DEV       
                   J         ILL$DEV       
                   J         ILL$DEV       
                   J         ILL$DEV       
                   J         ILL$DEV       
                   J         ILL$DEV       
                   J         ILL$DEV       
                   J         ILL$DEV       
                   J         ILL$DEV       
                   J         ILL$DEV       
                   J         ILL$DEV       
                   J         ILL$DEV       
                   J         ILL$DEV       
                   J         ILL$DEV
       . ++++++++++
       . JUMP TABLE OF EXECUTIVE RETURN ROUTINES
       . ++++++++++
       ER$JUMP     +EREXIT$,ER$EXIT
                   +ERYIELD$,ER$YIELD
                   +ERWAIT$,ER$WAIT
                   +ERACCEPT$,ER$ACCEPT
                   +ERTYPE$,ER$TYPE
                   +EROPEN$,ER$OPEN
                   +ERCLOSE$,ER$CLOSE
                   +ERREAD$,ER$READ
                   +ERWRITE$,ER$WRITE
                   +ERD2I$, ER$D2I
                   +ERTOKEN$,ER$TOKEN                         
                   +ERDCLOCK$,ER$DCLOCK
                   +ERRTCLOCK$,ER$RTCLOCK
                   +ERI2D$,ER$I2D
                   +ERMEMSZ$,ER$MEMSZ
       . ++++++++++
       . TASK CONTROL
       . ++++++++++
       SWTCH$LIST  L$IST                         . LIST OF ACTIVE TASKS
       LAST$TASK   +0                            . PTR TO MOST RECENTLY ACTIVE TASK                   
       . ++++++++++
       . O/S INITIALIZATION
       . ++++++++++
       INIT        LPLR      OI$PLR              . LOAD INITIAL PLR
                   EIR       OI$IFR              . LOAD INITIAL IFR & RIR
       .
                   ECSR      CHANZERO            . ENABLE CONSOLE PRT & KBD
                   EXF,W     CONSENB
       .
                   R         SY$BMPSIZE,,ADV     . CLEAR SYS POOL BITMAP
                   SZ,W      SY$BITMAP
       .
                   LADD      FH8$DEVS,FH8$CHAN5  . CREATE LIST OF INSTALLED FH880'S
       .
                   LADD      FM$LIST,FM$HEAD     . INIT FREE MEMORY LIST
       .
                   LADD      SWTCH$LIST,CONS$TCBO . ADD CONS OTPT TO SWTCH LIST
                   LADD      SWTCH$LIST,CONS$TCBI . ADD CONS INPT TO SWTCH LIST
                   LADD      SWTCH$LIST,FH8$TCB   . ADD FH880 I/O TO SWTCH LIST
                   LADD      SWTCH$LIST,CARD$TCB  . ADD CARD I/O TO SWTCH LIST
                   LADD      SWTCH$LIST,PRN$TCB   . ADD PRINTER I/O TO SWTCH LIST
                   LADD      SWTCH$LIST,CCP$TCB   . ADD CONSOLE CMD PROCESSOR
       . +++++++++++++++++++++++++++++++++
       . TESTING
       . +++++++++++++++++++++++++++++++++
       .
                   ENQUEUE   CONS$OUTQ,BANRREQ   . QUEUE BANNER TO CONSOLE
                   ECSR      CHANZERO            . START CONSOLE INPUT
                   INMON,W   CI$BCW
                   J         SWITCHER  
       .
       CONSENB     +0000000033                   . ENABLE PRT & KBD
       BANNER      +'MINIMUM OPERATING SYSTEM (MOS) FOR UNIVAC 494'
                   +' EMULATOR VER 0.5  ^'
       BANRLEN     EQU       $-BANNER
       BANRREQ     IOREQ     BANRLEN,BANNER
       EXEC$IFR    EQU       IFR$B17             . B4-7 ARE 17 BIT, NO GUARD MODE
       USER$IFR    EQU       IFR$B17+IFR$PLRENB+IFR$BWORK+IFR$GUARDRW
       EXEC$RIR    EQU       0
       EXEC$PLR    EQU       0
       OI$IFR      +EXEC$IFR
       OI$RIR      +EXEC$RIR
       OI$PLR      +EXEC$PLR
       UI$IFR      +USER$IFR
       . ++++++++++
       . TASK SWITCHER
       .
       . STARTING WITH THE TASK AFTER THE MOST RECENTLY SCHEDULED TASK,
       . PASS THROUGH THE SWITCH LIST LOOKING FOR TASKS THAT ARE READY TO
       . BE SCHEDULED. RESUME THE FIRST ONE THAT WE FIND AND SET THE MOST
       . RECENT TASK POINTER TO THIS TASKS TCB.
       .
       . B5 = PTR TO TCB
       . B6 = PTR TO IOREQ
       . ++++++++++
       SWITCHER    J         $+1,,STOP5  
                   LA,W      LAST$TASK,,ANOT     . GET MOST RECENT TASK
                   J         SW$GETHEAD
                   LB,A      B5
                   LA,W      LI$NEXT,B5,ANOT     . GET NEXT TASK IN LIST
                   J         SW$GETHEAD
       SW$GOTHEAD  SA,W      LAST$TASK           . UPDATE MOST RECENT TASK PTR
                   LB,A      B5
                   LA,L      TCB$FLAGS,B5,ANOT   . GET FLAGS
                   J         SW$RESUME           . ZERO, NOTHING TO DO
       . CHECK THE FLAGS FOR ANY WAIT CONDITIONS
       .
       . I/O DONE
                   LQ        TCB$WDONE           . WAITING FOR I/O DONE?
                   LLP,L     TCB$FLAGS,B5,ANOT
                   J         SW$CFIN             . NO
                   LA,W      TCB$EVTDTA,B5,ANOT  . YES, GET PTR TO IOREQ
                   J         SW$RESUME           . ZERO, HMMM!
                   LB,A      B6
                   LQ        IR$DONE             . IS IR$DONE SET?
                   LLP,W     IR$FLAGS,B6,ANOT
                   J         SWITCHER            . NO, NEXT TASK
                   J         SW$RESUME           . YES, RESUME THIS TASK
       . I/O FIN                   
       SW$CFIN     LQ        TCB$WFIN            . WAITING FOR I/O FIN?
                   LLP,L     TCB$FLAGS,B5,ANOT
                   J         SW$CNZRO            . NO, CHECK NON-IO EVENTS
                   LA,W      TCB$EVTDTA,B5,ANOT  . GET PTR TO IOREQ
                   J         SW$RESUME           . ZERO, HMMM!
                   LB,A      B6
                   LQ        IR$FIN              . IS IR$FIN SET?
                   LLP,W     IR$FLAGS,B6,ANOT
                   J         SWITCHER            . NO, NEXT TASK
                   J         SW$RESUME           . YES, RESUME THIS TASK
       . WORD BECOMES NON-ZERO
       SW$CNZRO    LQ        TCB$WNZRO           . WAIT FOR NON-ZERO?
                   LLP,L     TCB$FLAGS,B5,ANOT
                   J         SW$CCONS            . NO, CHECK CONSOLE INPUT
                   LA,W      TCB$EVTDTA,B5,ANOT  . GET PTR TO WORD TO CHECK
                   J         SW$RESUME           . ZERO, HMMM!
                   LB,A      B6
                   LA,W      0,B6,ANOT           . IS WORD ZERO?
                   J         SWITCHER            . YES, NEXT TASK
                   J         SW$RESUME           . NO, RESUME THIS TASK
       . CONSOLE INPUT
       SW$CCONS    LQ        TCB$WCONS           . CONS BFR PTR (EVTDTA) <> 0?
                   LLP,L     TCB$FLAGS,B5,ANOT
                   J         SW$TSET             . NO, CHECK TEST AND SET
                   LA,W      TCB$EVTDTA,B5,ANOT  . IS BFR PTR ZERO?
                   J         SWITCHER            . YES, NEXT TASK
                   J         SW$RESUME           . NO, RESUME THIS TASK
       . TEST AND SET
       SW$TSET     LQ        TCB$WTSET           . TEST AND SET FREE?
                   LLP,W     TCB$FLAGS,B5,ANOT
                   J         SW$RESUME           . NO, RESUME THIS TASK
                   LA,W      TCB$EVTDTA,B5,ANOT  . GET ADDR OF TSET CELL
                   J         SW$RESUME           . ZERO, HMMM!
                   LB,A      B6                  . CHECK IF BIT 14 CLEAR
                   LQ        40000
                   LLP,L     0,B6,ANOT
                   J         SW$RESUME           . IT'S CLEAR, RESUME
       .                   
                   J         SWITCHER            . YES, NEXT TASK
       .                   
       SW$RESUME   LA,L      TCB$FLAGS,B5        . CLEAR ANY WAIT FLAGS
                   NOT       TCB$ALLWAIT
                   SA,L      TCB$FLAGS,B5
                   LB        B6,0                . XFER REGS TO TEMP
       SW$RLOOP    LA,W      0,B5
                   SA,W      SW$REGS,B6
                   TBI       B5,77777
                   TBI       B6,TCB$B7
                   J         SW$RLOOP
                   LQ,W      CIFR$BWORK          . EXEC OR WORKER REGS?
                   LLP,W     SW$IFR,,AZERO    
                   J         SW$BWORK
                   LB,W      B1,SW$B1            . LOAD EXEC REGISTERS
                   LB,W      B2,SW$B2
                   LB,W      B3,SW$B3
                   LB,W      B4,SW$B4
                   LB,W      B5,SW$B5
                   LB,W      B6,SW$B6
                   LB,W      B7,SW$B7
                   J         SW$RESTORE2
       SW$BWORK    LBW       SW$B1               . LOAD WORKER REGISTERS
                   J         $+1,,STOP6          . STOP TO ALLOW DEBUG OF JOBS
       SW$RESTORE2 LA,L      SW$P                . RESUME TASK
                   SA,L      SW$GO
                   LA,W      SW$A
                   LQ,W      SW$Q
                   LPLR      SW$PLR
                   EIR       SW$IFR
       SW$GO       J         $                   . RESUME TASK
       SW$GETHEAD  LA,W      SWTCH$LIST+LCB$HEAD,,ANOT  . GET LIST HEAD
                   J         SWITCHER            . LIST EMPTY, LOOP
                   J         SW$GOTHEAD
       . ++++++++++
       . ALLOW THE ACTIVE TASK TO YIELD CONTROL OF THE PROCESSOR.
       .
       . B5 = PTR TO TCB
       . ++++++++++
       YIELD       +0
                   SLJ       Y$SAVE              . SAVE REGISTERS
                   LA,W      LAST$TASK,,ANOT     . SAVE OUR RETURN ADDR TO P
                   J         SWITCHER
                   LB,A      B5
                   LA,L      YIELD
                   SA,L      TCB$P,B5
       .
                   J         SWITCHER            . LET SWTCHR DO IT'S THING
       . ++++++++++
       . ALLOW THE ACTIVE TASK TO WAIT FOR A PARTICULAR EVENT     
       .
       . PARAMETERS:
       .   EVENT DATA
       .   FLAGS - WAIT FOR A EVENT. SPECIFYING MULTIPLE EVENTS
       .           WILL GIVE INCONSISTENT RESULTS.
       .
       . B5 = PTR TO TCB
       . ++++++++++
       WAIT        +0
                   SLJ       Y$SAVE              . SAVE REGISTERS                  
                   LA,W      LAST$TASK,,ANOT     . SAVE OUR RETURN ADDR TO P
                   J         SWITCHER
                   LB,A      B5
                   LA,L      WAIT  
                   A         2D
                   SA,L      TCB$P,B5
                   LB,L      B6,WAIT             . SET THE WAIT PARAMETERS
                   LA,W      0,B6
                   SA,W      TCB$EVTDTA,B5
                   LA,L      1,B6
                   ROR,L     TCB$FLAGS,B5
       .
                   J         SWITCHER            . LET SWTCHR DO IT'S THING                  
       . ++++++++++
       . CANCEL (KILL) A JOB
       .
       . PARAMETERS:
       .  B5 - PTR TO TCB
       .  B7 - PTR TO EXIT CODE
       . ++++++++++
       KILL        +0
                   SYALLOC   B1                  . GET WORK AREA
       .                   
                   LQ,W      0,B7                . GET EXIT CODE
                   SLJ       INT2DEC             . SAVE EXIT CODE IN MSG
                   SA,W      KILL$STAT,B1
                   DPL       TCB$NAME,B5         . SAVE TASK NAME IN MSG
                   DPS       KILL$NAME,B1
                   DPL       KILL$MSG            . SET CONSTANT TEXT
                   DPS       KILL$TEXT,B1
                   LA,W      KILL$CR
                   SA,W      KILL$TEXT2,B1
                   LA        6                   . SET UP IOREQ
                   SA,W      IR$COUNT,B1
                   LA        KILL$BFR,B1
                   SA,W      IR$BFR,B1
                   SB,W      B1,KILL$P1
                   SB,W      B1,KILL$P2
                   LBPJB6    L$INSERT            . QUEUE IT
                   +CONS$OUTQ
       KILL$P1     +0
                   +0                   
                   SLJ       WAIT                . WAIT FOR IT
       KILL$P2     +0
                   +TCB$WFIN
       .
                   SYFREE    B1
       .
                   LA,W      LI$NEXT,B5          . SET LAST TASK TO NEXT
                   SA,W      LAST$TASK                                      
                   SB,W      B5,KILL$P3          . DEL CRNT TASK FROM SWTCHR
                   LBPJB6    L$DELETE
                   +SWTCH$LIST
       KILL$P3     +0
       .
                   LA,U      TCB$FLAGS,B5        . GET THE JOB #
                   AN        1
                   LB,A      B6
                   SZ,W      JOBS$,B6            . FREE JOB SLOT
       .
                   LQ        TCB$CRASG           . IS READER ASSIGNED?
                   LLP,L     TCB$FLAGS,B5,AZERO
                   SZ,W      CRDR$INUSE          . YES, RELEASE IT
                   LQ        TCB$CPASG           . IS PUNCH ASSIGNED?
                   LLP,L     TCB$FLAGS,B5,AZERO
                   SZ,W      CPUN$INUSE          . YES, RELEASE IT
                   LQ        TCB$PRASG           . IS PRINTER ASSIGNED?
                   LLP,L     TCB$FLAGS,B5,AZERO
                   SZ,W      CPRN$INUSE          . YES, RELEASE IT
                                      
                   LA,W      TCB$ER$RIR,B5       . FREE MEMORY ASSIGNED TO PGM
                   SA,W      KILL$P5
                   SLJ       FM$FREEMEM
       KILL$P5     +0
       .                   
                   J,L       KILL
       .       
       KILL$IOREQ  EQU       0                   . OFFSETS INTO WORK AREA
       KILL$BFR    EQU       IR$LEN
       KILL$NAME   EQU       KILL$BFR
       KILL$TEXT   EQU       KILL$BFR+2
       KILL$STAT   EQU       KILL$BFR+4
       KILL$TEXT2  EQU       KILL$BFR+5
       
       KILL$MSG    +' FIN STAT '
       KILL$CR     +'    ^' 

       
       
                   
       . ++++++++++
       . SAVE ALL REGISTERS (EXCEPT P) TO THE TCB
       . ++++++++++
       Y$SAVE      +0
                   SIFR      SW$IFR              . SAVE TASK'S REGISTERS
                   SA,W      SW$A
                   SQ,W      SW$Q
                   LQ,W      CIFR$BWORK          . EXEC OR WORKER REGS?
                   LLP,W     SW$IFR,,AZERO    
                   J         Y$BWORK
                   SB,W      B1,SW$B1            . SAVE EXEC REGISTERS
                   SB,W      B2,SW$B2
                   SB,W      B3,SW$B3
                   SB,W      B4,SW$B4
                   SB,W      B5,SW$B5
                   SB,W      B6,SW$B6
                   SB,W      B7,SW$B7
                   J         Y$GETTCB
       Y$BWORK     SBW       SW$B1               . SAVE WORKER REGISTERS
       Y$GETTCB    LA,W      LAST$TASK,,ANOT     . GET PTR TO ACTIVE TCB       
                   J         $,,STOP             . NO ACTIVE TASK, NEVER HAPPEN
                   LB,A      B5
                   LA,W      SW$IFR              . COPY SAVED REGS TO TCB
                   SA,W      TCB$IFR,B5
                   LB        B5,TCB$P,B5
                   LB        B6,TCB$P
       Y$COPY      LA,W      SW$REGS,B6
                   SA,W      0,B5
                   TBI       B5,77777
                   TBI       B6,TCB$B7
                   J         Y$COPY
       .
                   J,L       Y$SAVE                   
       .                          
       SW$COUNT    +0
       SW$REGS     RES       16D
       SW$IFR      EQU       SW$REGS+3D
       SW$RIR      EQU       SW$REGS+4D
       SW$PLR      EQU       SW$REGS+5D
       SW$P        EQU       SW$REGS+6D          . RELATIVE TO RIR
       SW$A        EQU       SW$REGS+7D
       SW$Q        EQU       SW$REGS+8D
       SW$B1       EQU       SW$REGS+9D
       SW$B2       EQU       SW$REGS+10D
       SW$B3       EQU       SW$REGS+11D
       SW$B4       EQU       SW$REGS+12D
       SW$B5       EQU       SW$REGS+13D
       SW$B6       EQU       SW$REGS+14D
       SW$B7       EQU       SW$REGS+15D
       . ++++++++++ 
       . ILLEGAL INSTRUCTION INTERRUPT HANDLER                
       . ++++++++++
       IL$         SLJ       LOG$ERROR
                   +IL$MSG
                   J         $,,STOP                   
       IL$MSG      +'ILLEGAL INSTRUCTION  '
       . ++++++++++
       . PROGRAM PROTECTION INTERRUPT
       . ++++++++++
       PP$         SLJ       LOG$ERROR
                   +PP$MSG
                   J         $,,STOP
       PP$MSG      +'PROGRAM PROTECTION  '
       . ++++++++++
       . ILLEGAL (UNCONFIGURED) DEVICE
       . ++++++++++
       ILL$DEV     SLJ       LOG$ERROR
                   +ID$MSG
                   J         $,,STOP
       ID$MSG      +'ILLEGAL DEVICE      '
       . ++++++++++
       . FLOATING POINT OVERFLOW
       . ++++++++++
       FFO$        SLJ       LOG$ERROR
                   +FFO$MSG
                   J         $,,STOP
       FFO$MSG     +'FP OVERFLOW         '
       . ++++++++++
       . FLOATING POINT UNDERFLOW
       . ++++++++++
       FFU$        SLJ       LOG$ERROR
                   +FFU$MSG
                   J         $,,STOP
       FFU$MSG      +'FP UNDERFLOW        '
       . ++++++++++
       . TEST AND SET INTERRUPT
       .
       . IF A TSET INSTRUCTION FAILS TO ACQUIRE A LOCK WE COME HERE.
       . PUT THE CURRENT TASK INTO A TCB$WTSET STATE WHICH WILL PAUSE
       . THE TASK UNTIL THE TSET LOCK IS RELEASED. AT WHICH TIME THE
       . TASK WILL RESUME AT THE NEXT SEQUENTIAL INSTRUCTION.
       . ++++++++++
       TS$         SLJ       Y$SAVE              . SAVE REGISTERS
                   LB,W      B5,LAST$TASK        . GET PTR TO TCB
                   LA,W      TCB$IFR,B5          . GET TSET OPERAND ADDR
                   LSA       13D
                   LRSA      13D
                   SA,W      TCB$EVTDTA,B5       . SET TO WAIT FOR TSET FREE
                   LA        TCB$WTSET
                   SA,L      TCB$FLAGS,B5
                   SB,A      B4                  . GET RELATIVE INT. ADDR
                   AN,W      TCB$RIR,B5
                   SA,W      TCB$P,B5            
                   I$UNLOCKJ SWITCHER
       . ++++++++++
       . EXECUTIVE RETURN INTERRUPT
       . ++++++++++
       ER$         SB,W      B4,ER$SAVEB4        . SAVE RETURN ADDRESS
                   LB,W      B5,LAST$TASK
                   SA,W      TCB$ER$A,B5         . SAVE WORKER REGS
                   SQ,W      TCB$ER$Q,B5
                   SBW       TCB$ER$B1,B5
                   SB,A      B4                  . GET INTERRUPUT RTN ADDR
                   AN,W      TCB$RIR,B5          . MAKE REL TO RIR
                   SA,W      TCB$ER$P,B5
                   LA,W      TCB$RIR,B5
                   SA,W      TCB$ER$RIR,B5
                   SIFR      TCB$ER$IFR,B5       . SAVE WORKER IFR
                   SZ,W      TCB$RIR,B5          . ZERO EXEC RIR
                   LA,L      TCB$ER$IFR,B5       . GET ER #
                   TA        ERMAX$+1,,YMORE     . <= MAX?
                   J         ER$INVALID          . NO, BAD ER #
                   LB,A      B6
                   LB,L      B6,ER$JUMP,B6       . GET ADDR OF APPROPRIATE RTN
                   LB,W      B7,ER$SAVEB4        . PASS PARAM PTR IN B7
                   EIR       OI$IFR              . LOAD THE EXEC IFR
                   NOP                           . WAIT OUT EIR DELAY
                   I$UNLOCK
                   J         0,B6
       .                   
       ER$INVALID  SLJ       LOG$ERROR
                   +ER$MSG
                   J         $,,STOP
       ER$MSG      +'INVALID EXEC RETURN '
       ER$SAVEB4   RES       1
       . ++++++++++
       . RETURN TO TASK AFTER EXEUTIVE RETURN FINISHED
       . ++++++++++
       ER$RTRN     LB,W      B5,LAST$TASK        . GET CURRENT TCB
                   LA,W      TCB$ER$P,B5         . RESTORE WORKER REGS
                   SA,W      TCB$P,B5
                   LA,W      TCB$ER$A,B5         
                   SA,W      TCB$A,B5
                   LA,W      TCB$ER$Q,B5         
                   SA,W      TCB$Q,B5
                   LA,W      TCB$ER$B1,B5        
                   SA,W      TCB$B1,B5
                   LA,W      TCB$ER$B2,B5        
                   SA,W      TCB$B2,B5
                   LA,W      TCB$ER$B3,B5        
                   SA,W      TCB$B3,B5
                   LA,W      TCB$ER$B4,B5        
                   SA,W      TCB$B4,B5
                   LA,W      TCB$ER$B5,B5        
                   SA,W      TCB$B5,B5
                   LA,W      TCB$ER$B6,B5        
                   SA,W      TCB$B6,B5
                   LA,W      TCB$ER$B7,B5        
                   SA,W      TCB$B7,B5
                   LA,W      TCB$ER$IFR,B5
                   SA,W      TCB$IFR,B5
                   LA,W      TCB$ER$RIR,B5
                   SA,W      TCB$RIR,B5
       .                   
                   J         SWITCHER
       . ++++++++++
       . REAL TIME CLOCK INTERRUPT
       . ++++++++++
       RT$         LOG       RT$BLEN
                   I$UNLOCKJ 0,B4
       RT$BLEN     +3
                   +'REAL TIME CLOCK'
       . ++++++++++
       . FATAL ERROR - OUT OF MEMORY
       . ++++++++++                          
       NOMEM       SLJ       LOG$ERROR
                   +         NOMEM$MSG
                   J         $,,STOP
       NOMEM$MSG   +'OUT OF MEMORY       '                               
       .       
       . ++++++++++
       . DAY CLOCK INTERRUPT
       . ++++++++++
       DC$         DPS       DC$SAVE
                   RD,W      DC$COUNT,,ANOT      . COUNT DOWN 1 MINUTE
                   J         DC$LOG
                   J         DC$RTRN
       .             
       DC$LOG      LQ,W      16                  . GET DAY CLOCK
                   ZA                            . HOUR TENS                    
                   LSAQ      2D                  
                   OR        '0'
                   LSA       24D
                   SA,W      DC$BFR
                   ZA                            . HOUR UNITS
                   LSAQ      4D                 
                   OR        '0'
                   LSA       18D
                   OR,W      DC$BFR
                   SA,W      DC$BFR
                   LA        ':'                 . ADD THE COLON
                   LSA       12D
                   OR,W      DC$BFR
                   SA,W      DC$BFR
                   ZA                            . MINUTE TENS
                   LSAQ      4D                  
                   OR        '0'
                   LSA       6D
                   OR,W      DC$BFR
                   SA,W      DC$BFR
                   ZA                            . MINUTE UNITS
                   LSAQ      4D
                   OR        '0'
                   OR,W      DC$BFR
                   SA,W      DC$BFR
                   LOG       DC$BLEN
       .
                   LA        10D                 . RESET 1 MINUTE COUNT
                   SA,W      DC$COUNT                   
       .
       DC$RTRN     DPL       DC$SAVE            
                   I$UNLOCKJ 0,B4
       .
       DC$SAVE     RES       2                   . A & Q SAVE AREA
       DC$COUNT    +10D                          . 1 MINUTE COUNTDOWN
       DC$BLEN     +1
       DC$BFR      +0
       . ++++++++++
       . ISI INPUT MONITOR HANDLER
       . ++++++++++
       ISI$IN      SA,W      IO$SAVEA
                   SQ,W      IO$SAVEQ
                   SB,W      B5,IO$SAVEB5
                   SCN       IO$CHAN             . GET THE CHANNEL # OF INT.
                   LB,W      B5,IO$CHAN          
                   J         J$IMON,B5           . GO EXECUTE THE HANDLER
       . ++++++++++
       . ISI OUTPUT MONITOR HANDLER
       . ++++++++++
       ISI$OUT     SA,W      IO$SAVEA
                   SQ,W      IO$SAVEQ
                   SB,W      B5,IO$SAVEB5
                   SCN       IO$CHAN             . GET THE CHANNEL # OF INT.
                   LB,W      B5,IO$CHAN          
                   J         J$OMON,B5           . GO EXECUTE THE HANDLER
       . ++++++++++
       . ISI EXTERNAL INTERRUPT HANDLER
       . ++++++++++
       ISI$EXT     SA,W      IO$SAVEA
                   SQ,W      IO$SAVEQ
                   SB,W      B5,IO$SAVEB5
                   SCN       IO$CHAN             . GET THE CHANNEL # OF INT.
                   LB,W      B5,IO$CHAN          
                   J         J$EXT,B5            . GO EXECUTE THE HANDLER
       . ++++++++++
       . CONSOLE OUTPUT MONITOR HANDLER
       . ++++++++++
       CONS$OMON   LA,W      CONS$OUTQ,LCB$HEAD,ANOT
                   J         COM$RTRN            . QUEUE EMPTY, DONE
                   LB,A      B5                  
                   LA        IR$DONE             . SET THE DONE FLAG
                   ROR,W     IR$FLAGS,B5
       COM$RTRN    LA,W      IO$SAVEA
                   LQ,W      IO$SAVEQ
                   LB,W      B5,IO$SAVEB5                   
                   I$UNLOCKJ 0,B4
       . ++++++++++
       . CONSOLE INPUT MONITOR HANDLER. AN INTERRUPT OCCURS FOR
       . EACH CHARACTER RECEIVED FROM THE CONSOLE. WE BUFFER THESE
       . CHARACTERS IN A SMALL BUFFER HERE WHILE WAITING FOR THE
       . CONSOLE INPUT HANDLER TO GRAB THEM.
       . ++++++++++
       CONS$IMON   LB,L      B5,CI$HT            . GET BFR TAIL
                   LA,W      CI$BFR              . SAVE THE RECV'D CHAR.
                   SA,W      CI$CBFR,B5
                   RD,L      CI$HT,,ANEG         . DEC BFR TAIL
                   J         CIM$READ
                   LA        CI$BLEN             . TAIL WRAPPED, RESET IT
                   SA,L      CI$HT    
       CIM$READ    ECSR      CHANZERO            . RE-ISSUE THE INPUT BCW
                   INMON,W   CI$BCW   
       CIM$RTRN    LA,W      IO$SAVEA
                   LQ,W      IO$SAVEQ
                   LB,W      B5,IO$SAVEB5                   
                   I$UNLOCKJ 0,B4
       .
       CI$BCW      BCW       1,0,CI$BFR
       CI$BFR      RES       1
       CI$CBFR     RES       10D
       . U = HEAD OF BFR, L = TAIL OF BFR
       CI$BLEN     EQU       9D
       CI$HT       +CI$BLEN,CI$BLEN
       . ++++++++++
       . CONSOLE EXTERNAL INTERRUPT HANDLER 
       . ++++++++++
       CONS$EXT    EQU       $   
       CE$RTRN     LA,W      IO$SAVEA
                   LQ,W      IO$SAVEQ
                   LB,W      B5,IO$SAVEB5                   
                   I$UNLOCKJ 0,B4
       . ++++++++++
       . CARD DEVICE OUTPUT MONITOR HANDLER
       . ++++++++++                   
       CARD$OMON   LA,W      CARD$Q,LCB$HEAD,ANOT
                   J         CROM$RTRN           . QUEUE EMPTY, DONE
                   LB,A      B5                  
                   SC,W      IR$STAT,B5          . SAVE THE STATUS
                   LA        IR$DONE             . SET THE DONE FLAG
                   ROR,W     IR$FLAGS,B5
       CROM$RTRN   LA,W      IO$SAVEA
                   LQ,W      IO$SAVEQ
                   LB,W      B5,IO$SAVEB5                   
                   I$UNLOCKJ 0,B4
       . ++++++++++
       . CARD DEVICE INPUT MONITOR HANDLER
       . ++++++++++               
       CARD$IMON   J         CARD$OMON    
       . ++++++++++
       . CARD DEVICE EXTERNAL INTERRUPT HANDLER
       . ++++++++++               
       CARD$EXT    J         CARD$OMON    
       . ++++++++++
       . PRINTER OUTPUT MONITOR HANDLER
       . ++++++++++                   
       PRN$OMON    LA,W      PRN$Q,LCB$HEAD,ANOT
                   J         POM$RTRN            . QUEUE EMPTY, DONE
                   LB,A      B5                  
                   SC,W      IR$STAT,B5          . SAVE THE STATUS
                   LA        IR$DONE             . SET THE DONE FLAG
                   ROR,W     IR$FLAGS,B5
       POM$RTRN    LA,W      IO$SAVEA
                   LQ,W      IO$SAVEQ
                   LB,W      B5,IO$SAVEB5                   
                   I$UNLOCKJ 0,B4
       . ++++++++++
       . PRINTER INPUT MONITOR HANDLER
       . ++++++++++               
       PRN$IMON    J         PRN$OMON    
       . ++++++++++
       . PRINTER EXTERNAL INTERRUPT HANDLER
       . ++++++++++               
       PRN$EXT     J         PRN$OMON    
       .
       . ++++++++++
       . FH880 OUTPUT MONITOR INTERRUPT HANDLER
       . ++++++++++
       FH8$OMON    SCN       IO$CHAN             . GET THE CHANNEL #
                   LB        B5,FH8$DEVS         . GET PTR TO DEV LIST
                   LA,W      LCB$HEAD,B5,ANOT    . GET 1ST DEV ON LIST
                   J         FOM$RTRN            . LIST EMPYT, DONE
       FOM$LOOP    LB,A      B5
                   LA,W      IO$CHAN             . IS DEV ON THIS CHANNEL?
                   LQ        37
                   MATE      FH8$CHAN,B5         
                   J         FOM$NXTDEV          . NO
                   LA,W      FH8$QUE,B5,ANOT     . YES, GET PTR TO QUEUE
                   J         $,,STOP             . NO QUEUE, NEVER HAPPEN
                   LB,A      B5
                   LA,W      LCB$HEAD,B5,ANOT    . GET 1ST ITEM ON QUEUE
                   J         FOM$RTRN            . QUEUE EMPTY
                   LB,A      B5
                   SC,W      IR$STAT,B5          . SAVE THE STATUS
                   LA        IR$DONE             . SET THE DONE FLAG
                   ROR,W     IR$FLAGS,B5
                   J         FOM$RTRN
       .                   
       FOM$NXTDEV  LA,W      LI$NEXT,B5,AZERO    . GET NEXT DEV                   
                   J         FOM$LOOP
       .                   
       FOM$RTRN    LA,W      IO$SAVEA
                   LQ,W      IO$SAVEQ
                   LB,W      B5,IO$SAVEB5                   
                   I$UNLOCKJ 0,B4
       .                   
       . ++++++++++
       . FH880 INPUT MONITOR INTERRUPT HANDLER
       . ++++++++++
       FH8$IMON    J         FH8$OMON            . SAME AS OMON?
       .                   
       . ++++++++++
       . FH880 EXERNAL INTERRUPT HANDLER
       . ++++++++++
       FH8$EXT     J         FH8$OMON            . SAME AS OMON?
       .                   
       IO$CHAN     +0
       IO$SAVEA    +0
       IO$SAVEQ    +0
       IO$SAVEB5   +0
       . ++++++++++
       . LOG AN ERROR MESSAGE TO THE VIRTUAL MAINTENANCE PANEL.
       . ADDRESS OF 4 WORD MESSAGE FOLLOWING SLJ WHICH CALLS THIS
       . ROUTINE.
       . ++++++++++
       LOG$ERROR   +0
                   LQ,W      LE$SPACES           . CLEAR THE BUFFER
                   LB        B1,0
       LE$CLEAR    SQ,W      LE$BFR,B1
                   TBI       B1,3
                   J         LE$CLEAR
       .                   
                   LB,W      B6,LOG$ERROR        . COPY MSG TO BUFFER
                   LB,W      B6,0,B6
                   LB        B1,0
       LE$COPY     LQ,W      0,B6
                   SQ,W      LE$BFR,B1
                   TBI       B6,77777
                   TBI       B1,3D
                   J         LE$COPY
       .
                   SB,Q      B4                  . GET CONTENTS OF P
                   SLJ       INT2OCT             . CONVERT TO OCTAL
                   LSAQ      24D                 . LJUST 6 OCTAL DIGITS
                   SA,W      LE$P
                   SQ,A
                   LRSA      24D
                   LSA       24D
                   OR,W      LE$SPACE
                   SA,W      LE$P+1
                   LOG       LE$BLEN             . WRITE IT TO THE LOG
       .                   
       . RETURN TO 2ND WORD FOLLOWING SLJ INSTRUCTION
       .
                   LB,W      B6,LOG$ERROR 
                   J         1,B6                                             
       .
       LE$SPACES   +'     '
       LE$SPACE    +0,5,5,5,5
       LE$BLEN     +7D
       LE$BFR      +'                    '
                   +' AT  '
       LE$P        +'          '
       LE$OCTAL    +'     '           
                   +'     '           
                   +'     '           
                   +'     '           
                   +'     '           
                   +'     '           
                   +'     '           
                   +'     '           
                   +'     '           
                   +'     '           
       . ++++++++++
       . REMOVE THE FIRST ITEM FROM A QUEUE.
       .
       . PARAMETERS:
       .   POINTER TO QUEUE
       .
       . REGISTERS:
       .   B1 = QUEUE
       .   B5 = ITEM TO REMOVE
       .   B7 = MISC
       . ++++++++++
       L$DEQUEUE   I$LOCK
                   SA,W      LDQ$SAVEA
                   SB,W      B5,LDQ$SAVEB5
                   SB,W      B6,LDQ$SAVEB6
                   LA,W      0,B6
                   SA,W      L$CALLDEL+1                   
                   LB,A      B5
                   LA,W      LCB$HEAD,B5
                   SA,W      L$CALLDEL+2
       L$CALLDEL   LBPJB6    L$DELETE
                   +0
                   +0                   
                   LA,W      LDQ$SAVEA
                   LB,W      B5,LDQ$SAVEB5
                   LB,W      B6,LDQ$SAVEB6
                   J         1,B6
       .
       LDQ$SAVEA   RES       1
       LDQ$SAVEB5  RES       1                   
       LDQ$SAVEB6  RES       1                   
       . ++++++++++
       . INSERT AN ITEM INTO THE MIDDLE OF A LIST
       .
       . PARAMETERS:
       .   QUEUE
       .   ITEM TO INSERT
       .   INSERT BEFORE ITEM. IF ZERO ADD TO TAIL.
       .
       . REGISTERS:
       .   B1 = QUEUE
       .   B5 = ITEM TO INSERT
       .   B6 = MISC.
       .   B7 = INSERT BEFORE THIS ITEM
       . ++++++++++
       L$INSERT    I$LOCK
                   SA,W      L$SAVEA             . SAVE REGS
                   SB,W      B1,L$SAVEB1
                   SB,W      B5,L$SAVEB5
                   SB,W      B6,L$SAVEB6
                   SB,W      B7,L$SAVEB7
                   LB,W      B1,0,B6             . GET PTR TO LIST
                   LB,W      B5,1,B6             . GET PTR TO ITEM TO INSERT
                   LA,W      2,B6,ANOT           . GET PTR TO INSERT B4 ITEM
                   J         LIN$END             . ZERO? ADD TO END OF LIST
                   LB,A      B7
                   LA,W      LI$PRIOR,B7,ANOT    . INSERT AT HEAD OF LIST?
                   J         LIN$NEWHD           . YES
                   LB,A      B6
                   SA,W      LI$PRIOR,B5         . UPDATE ITEM PRIOR
                   SB,W      B7,LI$NEXT,B5       . UPDATE ITEM NEXT
                   SB,W      B1,LI$LIST,B5       . UPDATE PTR TO LIST
                   SB,W      B5,LI$NEXT,B6       . UPDATE PRIOR'S NEXT
                   SB,W      B5,LI$PRIOR,B7      . UPDATE NEXT'S PRIOR
                   J         LIN$RTRN
       .
       LIN$NEWHD   SZ,W      LI$PRIOR,B5         . UPDATE ITEM PRIOR
                   SB,W      B7,LI$NEXT,B5       . UPDATE ITEM NEXT
                   SB,W      B1,LI$LIST,B5       . UPDATE PTR TO LIST
                   SB,W      B5,LI$PRIOR,B7      . UPDATE NEXT'S PRIOR
                   SB,W      B5,LCB$HEAD,B1      . UPDATE LIST HEAD                      
       .       
       LIN$RTRN    LA,W      L$SAVEA             . RESTORE REGS
                   LB,W      B1,L$SAVEB1
                   LB,W      B5,L$SAVEB5
                   LB,W      B6,L$SAVEB6
                   LB,W      B7,L$SAVEB7
                   I$UNLOCKJ 3,B6
       .
       LIN$END     LA,W      LCB$TAIL,B1,ANOT    . GET END OF LIST
                   J         LIN$EMPTY           . THERE ISN'T ONE
                   LB,A      B6
                   SA,W      LI$PRIOR,B5         . ITEM PRIOR = TAIL
                   SZ,W      LI$NEXT,B5          . ITEM NEXT = ZERO
                   SB,W      B1,LI$LIST,B5       . SET ITEM LIST PTR
                   SB,W      B5,LI$NEXT,B6       . TAIL NEXT = ITEM                  
                   SB,W      B5,LCB$TAIL,B1      . TAIL = ITEM
                   J         LIN$RTRN
       .
       LIN$EMPTY   SZ,W      LI$NEXT,B5          . CLEAR NEXT / PRIOR
                   SZ,W      LI$PRIOR,B5
                   SB,W      B1,LI$LIST,B5       . SET ITEM LIST PTR
                   SB,W      B5,LCB$HEAD,B1      . HEAD / TAIL = ITEM
                   SB,W      B5,LCB$TAIL,B1
                   J         LIN$RTRN                   
       . ++++++++++
       . DELETE AN ITEM FROM A QUEUE
       .
       . PARAMETERS:
       .   QUEUE
       .   ITEM TO DELETE
       .
       . REGISTERS:
       .   B1 = QUEUE
       .   B5 = ITEM TO INSERT
       .   B7 = MISC
       .
       . ++++++++++
       L$DELETE    I$LOCK
                   SA,W      L$SAVEA             . SAVE REGS
                   SB,W      B1,L$SAVEB1
                   SB,W      B5,L$SAVEB5
                   SB,W      B6,L$SAVEB6
                   SB,W      B7,L$SAVEB7
                   LB,W      B1,0,B6             . GET PTR TO QUEUE
                   LB,W      B5,1,B6             . GET PTR TO ITEM TO DELETE
                   LA,W      LI$PRIOR,B5,ANOT    . GET PRIOR
                   J         LDE$NOPRIOR         . THERE ISN'T ONE
                   LB,A      B7
                   LA,W      LI$NEXT,B5          . PRIOR'S NEXT = ITEM NEXT
                   SA,W      LI$NEXT,B7
                   J         LDE$DONEXT
       LDE$NOPRIOR LA,W      LI$NEXT,B5          . LIST HEAD = ITEM NEXT
                   SA,W      LCB$HEAD,B1
       LDE$DONEXT  LA,W      LI$NEXT,B5,ANOT     . GET NEXT
                   J         LDE$NONEXT          . THERE ISN'T ONE
                   LB,A      B7
                   LA,W      LI$PRIOR,B5         . NEXT'S PRIOR = ITEM PRIOR
                   SA,W      LI$PRIOR,B7
                   J         LDE$DONE
       LDE$NONEXT  LA,W      LI$PRIOR,B5         . LIST TAIL = ITEM PRIOR
                   SA,W      LCB$TAIL,B1
       LDE$DONE    SZ,W      LI$LIST,B5          . CLEAR LIST PTR
                   SZ,W      LI$NEXT,B5          . CLEAR NEXT / PRIOR
                   SZ,W      LI$PRIOR,B5
       .
       LDE$RTRN    LA,W      L$SAVEA             . RESTORE REGS
                   LB,W      B1,L$SAVEB1
                   LB,W      B5,L$SAVEB5
                   LB,W      B6,L$SAVEB6
                   LB,W      B7,L$SAVEB7
                   I$UNLOCKJ 2,B6

       .
       L$SAVEA     RES       1
       L$SAVEB1    RES       1
       L$SAVEB5    RES       1
       L$SAVEB6    RES       1
       L$SAVEB7    RES       1
                   
       . ++++++++++
       . SYSTEM POOL MANAGER
       .
       . ALLOCATE A BLOCK FROM THE SYSTEM POOL
       .
       . PARAMETERS:
       .   NONE
       .
       . RETURNS:
       .   ADDRESS OF START OF BLOCK.
       . ++++++++++
       SY$ALLOC    +0
                   SA,W      SY$SAVEA            . SAVE REGISTERS
                   SQ,W      SY$SAVEQ
                   SB,W      B1,SY$SAVEB1
                   SB,W      B2,SY$SAVEB2
                   SB,W      B7,SY$SAVEB7
                   LA,L      SY$ALLOC            . GET RETURN ADDRESS
                   LB,A      B1
                   A         1                   . BUMP PAST RTRN VALUE
                   SA,L      SYA$J               . SAVE RTRN ADDRESS
                   SZ,W      0,B1                . ZERO THE RTRN VALUE
       .
                   LB        B1,0                . INIT LOOP VARS
                   LB        B2,0
                   LQ,W      SY$HIGHBIT
       SYA$LOOP    LLP,W     SY$BITMAP,B1,ANOT   . BLOCK ALLOCATED?
                   J         SYA$GOT1            . NO, GOT A FREE BLOCK
                   LRSQ      1                   . NEXT BIT
                   TQ        1,,YLESS            . DONE?
                   J         SYA$NEXTW           . YES, NEXT WORD
                   TBI       B2,77777            . NO, BUMP BLOCK ADDR
                   J         SYA$LOOP            . & LOOP
       SYA$NEXTW   LQ,W      SY$HIGHBIT          . YES, TRY NEXT WORD OF BMP
                   TBI       B1,SY$NUMBLK        
                   J         SYA$LOOP            . NOT DONE YET
                   SLJ       LOG$ERROR           . POOL EXHAUSTED, CRASH
                   +SY$MSG
                   J         $,,STOP
       .                   
       SYA$GOT1    SQ,A                          . SET BIT SHOWING BLK ALLOC'D
                   ROR,W     SY$BITMAP,B1
                   LB,L      B1,SY$ALLOC         . SAVE BLOCK #
                   SB,W      B2,0,B1 
                   LQ        SY$BLKSIZE          . CALC ADDR OF BLOCK
                   M,W       0,B1
                   AQ        SY$POOL
                   SQ,W      0,B1 
                   LB,W      B1,0,B1             . CLEAR BLOCK TO ZEROS
                   R         SY$BLKSIZE,,ADV
                   SZ,W      0,B1                  
       .                                  
       SYA$RTRN    LA,W      SY$SAVEA            . RESTORE REGISTERS
                   LQ,W      SY$SAVEQ            
                   LB,W      B1,SY$SAVEB1 
                   LB,W      B2,SY$SAVEB2           
                   LB,W      B7,SY$SAVEB7
       SYA$J       J         $                   . RETURN 
       . ++++++++++
       . FREE A BLOCK IN THE SYSTEM POOL
       .
       . PARAMETERS:
       .   BLOCK ADDRESS
       . ++++++++++
       SY$FREE     +0       
                   SA,W      SY$SAVEA            . SAVE REGISTERS
                   SQ,W      SY$SAVEQ
                   SB,W      B1,SY$SAVEB1
                   SB,W      B2,SY$SAVEB2
                   LA,L      SY$FREE             . GET RETURN ADDRESS
                   LB,A      B1
                   A         1                   . BUMP PAST PARAMETER
                   SA,L      SYF$J               . SAVE RTRN ADDRESS
       .
                   LQ,W      0,B1                . GET BLOCK ADDRESS
                   ANQ       SY$POOL,,QPOS       . MAKE RELATIVE TO POOL START
                   J         SYF$RTRN            . INALID BLOCK ADDRESS
                   ZA                            . DIVIDE BY BLOCK SIZE
                   D         SY$BLKSIZE          . TO GET BLOCK #
                   TQ        SY$NUMBLK,,YMORE
                   J         SYF$RTRN            . INVALID BLOCK ADDRESS
                   D         30D                 . DIVIDE BY BITS / WORD
                   SA,L      SYF$SHFT            . REM = BIT # WITHIN WORD
                   SQ,L      SYF$NOT             . QUOT = WORD # WITHIN BMP
                   LA,W      SY$HIGHBIT          . CALC. BIT TO FREE
       SYF$SHFT    LRSA      0
                   SA,W      SY$BIT                   
                   LB        B1,SY$BITMAP
       SYF$NOT     LA,W      0,B1
                   NOT,W     SY$BIT
                   SA,W      0,B1
       .
       SYF$RTRN    LA,W      SY$SAVEA            . RESTORE REGISTERS
                   LQ,W      SY$SAVEQ            
                   LB,W      B1,SY$SAVEB1 
                   LB,W      B2,SY$SAVEB2           
       SYF$J       J         $                   . RETURN 
       .
       SY$HIGHBIT  +4000000000
       SY$BIT      RES       1
       SY$MSG      +'SYS POOL EXHAUSTED  '
       SY$SAVEA    RES       1
       SY$SAVEQ    RES       1
       SY$SAVEB1   RES       1
       SY$SAVEB2   RES       1
       SY$SAVEB7   RES       1
       . ++++++++++
       . WORK AREA MANAGER. WORK AREAS ARE USED FOR REGISTER SAVE AREAS AND
       . SCRATCH SPACES BY ROUTINES THAT NEED TO BE RE-ENTRANT.
       .
       . ALLOCATE A WORK AREA AND SAVE ALL REGISTERS TO IT
       . ++++++++++
       WA$ALLOC    +0
                   SA,W      WA$SAVEA            . SAVE VOLATILE REGISTERS
                   SB,W      B1,WA$SAVEB1        
                   LA,L      WA$ALLOC            . SAVE RETURN ADDRESS
                   LB,A      B1
                   A         1
                   SA,L      WAA$J
       .                                      
                   SLJ       SY$ALLOC            . GET A BFR FROM SYS POOL
                   +0
                   LA,W      $-1
                   SA,W      0,B1                . RETURN BFR ADDRESS
                   LB,A      B1
       .
                   LA,W      WA$SAVEA            . SAVE ALL REGS TO WORK AREA
                   SA,W      WA$A,B1
                   SQ,W      WA$Q,B1
                   LA,W      WA$SAVEB1
                   SA,W      WA$B1,B1
                   SB,W      B2,WA$B2,B1
                   SB,W      B3,WA$B3,B1
                   SB,W      B4,WA$B4,B1
                   SB,W      B5,WA$B5,B1
                   SB,W      B6,WA$B6,B1
                   SB,W      B7,WA$B7,B1
       .                 
       WAA$RTRN    LA,W      WA$SAVEA
                   LB,W      B1,WA$SAVEB1                  
       WAA$J       J         $                       
       .                   
       WA$SAVEA    RES       1
       WA$SAVEB1   RES       1
       . ++++++++++
       . RESTORE ALL REGISTERS FROM A WORK AREA AND RELEASE IT BACK TO
       . THE SYSTEM POOL
       . ++++++++++
       WA$FREE     +0
                   LA,L      WA$FREE             . SAVE RETURN ADDRESS
                   LB,A      B1
                   A         1
                   SA,L      WAF$J
                   LB,W      B1,0,B1             . GET WORK AREA ADDRESS
                   SB,W      B1,WAF$P1
       .                                      
                   LA,W      WA$A,B1             . RESTORE REGISTER FROM W/A
                   LQ,W      WA$Q,B1
                   LB,W      B2,WA$B2,B1
                   LB,W      B3,WA$B3,B1
                   LB,W      B4,WA$B4,B1
                   LB,W      B5,WA$B5,B1
                   LB,W      B6,WA$B6,B1
                   LB,W      B7,WA$B7,B1
                   LB,W      B1,WA$B1,B1
       .                   
                   SLJ       SY$FREE
       WAF$P1      +0
       .                 
       WAF$J       J         $                       
       . ++++++++++
       . MEMORY MANAGER
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
       .   B1 = PTR TO PARAMETER / RETURN VALUE
       .   B5 = FMCB
       .   B6 = MISC
       .   
       .
       . THE BLOCK ALLOCATED WILL BE 1 WORD LARGER THAN REQUESTED.
       . THIS FIRST WORD OF THE BLOC WILL CONTAIN THE BLOCK SIZE.
       . THE ADDRESS RETURNED TO THE CALLER WILL BE THE 2ND WORD
       . OF THE BLOCK.
       . ++++++++++
       FM$GETMEM   +0
                   SA,W      FM$SAVEA            . SAVE REGISTERS
                   SQ,W      FM$SAVEQ
                   SB,W      B1,FM$SAVEB1
                   SB,W      B5,FM$SAVEB5
                   SB,W      B6,FM$SAVEB6
                   LA,L      FM$GETMEM           . GET RETURN ADDRESS
                   LB,A      B1
                   A         1                   . BUMP PAST PARAMS
                   SA,L      FMG$J               . SAVE RETURN ADDRESS
                   LA,W      0,B1,ANOT           . GET SIZE TO ALLOCATE
                   J         FMG$RTRN            . ZERO BYTES REQUESTED, QUIT
                   A         1                   . ADD 1 FOR OVERHEAD
                   SA,W      FM$SIZE
                   LQ,A                          . ADJUST TO MULTIPLE OF 64
                   ZA
                   D         64D
                   JT        FM$IS64,,AZERO      . REM = ZERO
                   AN        64D                 . NO, ADD 64 MINUS REM SIZE
                   NA
                   RA,W      FM$SIZE
       FM$IS64     SZ,W      0,B1                . CLR RETURN VALUE
                   LB        B5,FM$LIST          . GET PTR TO TAIL OF FREE LIST
                   LA,W      LCB$TAIL,B5,ANOT
                   J         $,,STOP             . LIST EMPTY, NEVER HAPPEN
       .                   
       FMG$LOOP    LB,A      B5
                   LA,W      FMCB$END,B5         . CALC SIZE OF THIS BLOCK
                   AN,W      FMCB$START,B5
                   A         1
                   TA,W      FM$SIZE,,YLESS      . ENOUGH FOR THIS REQUEST?
                   J         FMG$NXTBLK          . NO
                   LA,W      FMCB$END,B5         . YES, CALC NEW END ADDR
                   AN,W      FM$SIZE
                   SA,W      FMCB$END,B5
                   A         1                   . CALC START ADDR THIS BLOCK
                   LB,A      B6                  . SAVE BLK SIZE AS 1ST WORD
                   LQ,W      FM$SIZE
                   SQ,W      0,B6
                   A         1                   . BUMP PAST OVERHEAD
                   SA,W      0,B1                . SAVE FOR RTRN
       . IF FMCB IS EMPTY, FREE IT UNLESS IT IS THE HEAD OF THE 
       . FREE MEMORY LIST
                   LA,W      FMCB$PRIOR,B5,ANOT  . HEAD OF LIST?
                   J         FMG$RTRN            . YES, WE'RE DONE
                   LA,W      FMCB$END,B5         . CALC SIZE REMAINING IN BLOCK
                   AN,W      FMCB$START,B5,ANEG
                   J         FMG$RTRN            . BLOCK NOT EMPTY, QUIT
                   SB,W      B5,FMG$P1           . EMPTY, REMOVE FROM FREE LIST
                   LBPJB6    L$DELETE
                   +FM$LIST
       FMG$P1      +0
                   SYFREE    B5
       .                        
       FMG$RTRN    LA,W      FM$SAVEA
                   LQ,W      FM$SAVEQ
                   LB,W      B1,FM$SAVEB1
                   LB,W      B5,FM$SAVEB5
                   LB,W      B6,FM$SAVEB6
       FMG$J       J         $                   
       .
       FMG$NXTBLK  LA,W      LI$PRIOR,B5,ANOT    . GET PTR TO PRIOR BLOCK
                   J         FMG$RTRN            . NO MORE, QUIT
                   J         FMG$LOOP
       . ++++++++++
       . FREE A BLOCK OF MEMORY
       .
       . PARAMETERS:
       .   BLOCK START ADDRESS
       .
       . REGISTERS:
       .   B1 = PARAMETER
       .   B5 = FMCB
       .   B6 = MISC
       .
       . FIRST WE TRY TO ADD THE BLOCK TO BE FREE'D TO AN EXISTING FREE
       . BLOCK.  IF THAT DOESN'T WORK, WE ADD A NEW FREE BLOCK TO THE CHAIN.
       . ++++++++++
       FM$FREEMEM  +0
                   SA,W      FM$SAVEA            . SAVE REGISTERS
                   SQ,W      FM$SAVEQ
                   SB,W      B1,FM$SAVEB1
                   SB,W      B5,FM$SAVEB5
                   SB,W      B6,FM$SAVEB6
                   LA,L      FM$FREEMEM          . GET RETURN ADDRESS
                   LB,A      B1
                   A         1                   . BUMP PAST PARAMS
                   SA,L      FMF$J               . SAVE RETURN ADDRESS
                   LA,W      0,B1,ANOT           . SAVE BLK START ADDR
                   J         FMF$RTRN            . ZERO, QUIT
                   AN        1                   . DEC. FOR OVERHEAD
                   SA,W      FM$BEGIN
                   LB,A      B5                  . GET THE BLK SIZE
                   LA,W      0,B5
                   SA,W      FM$SIZE
                   LB        B5,FM$LIST          . GET PTR TO HEAD OF FREE LIST
                   LA,W      LCB$HEAD,B5,ANOT
                   J         $,,STOP             . LIST EMPTY, NEVER HAPPEN
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
                   SB,W      B6,FMF$P3           . REMOVE FCMB FROM LIST &
                   SB,W      B6,FMF$P4           . FREE IT
                   LBPJB6    L$DELETE
                   +FM$LIST
       FMF$P3      +0
                   SLJ       SY$FREE
       FMF$P4      +0                                                
       .
       FMF$RTRN    LA,W      FM$SAVEA
                   LQ,W      FM$SAVEQ
                   LB,W      B1,FM$SAVEB1
                   LB,W      B5,FM$SAVEB5
                   LB,W      B6,FM$SAVEB6
       FMF$J       J         $
       .
       FMF$CHKNEW  TA        0,,YLESS            . NEED TO INSERT FMCB?
                   J         FMF$NXTBLK          . NO, NEXT BLOCK
       FMF$ADDNEW  SLJ       SY$ALLOC            . YES, ALLOC BLK FROM SYS POOL
       FMF$R1      +0
                   LA,W      FMF$R1              . ADD TO FREE LIST B4 CRNT BLK
                   SA,W      FMF$P1
                   SB,W      B5,FMF$P2
                   LBPJB6    L$INSERT
                   +FM$LIST
       FMF$P1      +0
       FMF$P2      +0
                   LB,W      B5,FMF$P1           . POINT TO NEW BLOCK
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
       FM$BEGIN    RES       1
       FM$SIZE     RES       1            
       FM$SAVEA    RES       1
       FM$SAVEQ    RES       1
       FM$SAVEB1   RES       1
       FM$SAVEB5   RES       1 
       FM$SAVEB6   RES       1 
       .                          
       . ++++++++++
       . SERVICE CONSOLE OUTPUT QUEUE TASK
       .
       . B1 = LIST CONTROL BLOCK
       . B5 = IOREQ
       . ++++++++++
       CONS$TCBO   T$CB      EXEC$IFR EXEC$RIR EXEC$PLR CONS$SVCOQ 'CONS$SVCOQ'
       .
       CONS$SVCOQ  LA,L      CSO$SUSP,,ANOT      . ARE WE SUSPENDED?
                   J         CSO$GETHEAD         . NO, CONTINUE
                   TA        1,,YLESS            . ACTUALLY SUSPENDED?
                   J         CSO$YIELD           . YES
                   LA        2                   . SUSPEND REQUESTED -
                   SA,L      CSO$SUSP            . SHOW REQUEST GRANTED
                   J         CSO$YIELD           
       .                   
       CSO$GETHEAD LB        B1,CONS$OUTQ        . GET PTR TO LCB
                   LA,W      LCB$HEAD,B1,AZERO   . GET PTR TO 1ST ITEM
                   J         CSO$GOTREQ          . GOT ONE
       CSO$YIELD   SLJ       YIELD               . WAIT UNTIL Q NOT EMPTY
                   J         CONS$SVCOQ
       .                                      
       CSO$GOTREQ  LB,A      B5
                   LA        IR$BUSY             . SET THE BUSY FLAG
                   ROR,W     IR$FLAGS,B5
       CSO$NEXT    LA,L      IR$COUNT,B5,ANOT    . END OF BUFFER?
                   J         CSO$DONE            . YES
                   LB,W      B6,IR$BFR,B5        . GET 1 WORD FROM BFR
                   LA,W      0,B6                
                   CUL       CSO$BFR             . UNPACK INTO 5 WORD BUFFER
                   I$LOCK
                   ECSR      CHANZERO
                   OUTMON,W  CSO$BCW
                   LA,W      IR$FLAGS,B5         . CLEAR I/O DONE FLAG
                   NOT       IR$DONE
                   SA,W      IR$FLAGS,B5
                   I$UNLOCK
                   RD,L      IR$COUNT,B5         . DECREMENT COUNT
                   RI,W      IR$BFR,B5           . INCREMENT BFR ADDRESS
       CSO$WAITIO  SB,W      B5,$+2              . SAVE PTR TO IOREQ
                   SLJ       WAIT                . WAIT FOR I/O DONE
                   +0
                   +TCB$WDONE
                   J         CSO$NEXT         
       .
       CSO$DONE    LA,W      IR$FLAGS,B5         
                   NOT       IR$BUSY             . CLEAR BUSY
                   OR        IR$FIN              . SET REQ CPLT             
                   SA,W      IR$FLAGS,B5 
                   DEQUEUE   CONS$OUTQ           . REMOVE FROM QUEUE
                   J         CONS$SVCOQ          . BACK TO START
       .
       CSO$BCW     BCW       5,0,CSO$BFR
       CSO$BFR     RES       5
       CSO$SUSP    +0                            . 1=SUSP REQ'D, 2=SUSPENDED
       . ++++++++++
       . SERVICE THE CONSOLE INPUT QUEUE. 
       .
       . WE WAIT FOR A <CR> FROM THE KEYBOARD. WHEN ONE IT DETECTED,
       . WE REQUEST THAT CONS$SVCOQ SUSPEND ITSELF. WHEN IT IS SUSPENDED
       . WE SEND A PROMPT TO THE CONSOLE PRINTER AND ACCEPT SUBSEQUENT
       . CHARACTERS INTO A HOLDING BUFFER UNTIL ANOTHER <CR> IS DETECTED
       . OR 80 CHARACTERS ARE RECEIVED.
       . ++++++++++
       CONS$TCBI   T$CB      EXEC$IFR EXEC$RIR EXEC$PLR CONS$SVCIQ 'CONS$SVCIQ'
       CONS$SVCIQ  SLJ       CSI$CHARW           . WAIT FOR INPUT
       .
       . WAIT FOR A <CR> TO SIGNAL START OF INPUT, SUSPEND CONSOLE
       . OUTPUT TASK AND SEND READY PROMPT
       .
                   SLJ       CSI$GET             . GET CHAR
                   LQ        77
                   MATE      CSI$CR              . IS IT <CR>?
                   J         CSI$YIELD           . NO
                   LA        1                   . ASK OTPT TASK TO SUSPEND
                   SA,W      CSO$SUSP            
       CSI$SUSPW   LA        1
                   TA,W      CSO$SUSP,,YLESS     . OTPUT SUSPENDED?
                   J         CSO$SENDP           . YES
                   SLJ       YIELD               . NO, WAIT
                   J         CSI$SUSPW
       CSO$SENDP   I$LOCK                        . SEND <CR><CR>-> PROMPT
                   ECSR      CHANZERO
                   OUT,W     CSI$PBCW
                   I$UNLOCK
       . READ TEXT UNTIL ANOTHER <CR> OR BUFFER FULL
                   LA,W      CSI$IIDX            . INIT BFR INDEX
                   SA,W      CSI$BIDX
                   R         16D,,ADV            . CLEAR BUFFER
                   SZ,W      CSI$BFR
       CSI$READ    SLJ       CSI$CHARW
                   SLJ       CSI$GET
       CSI$ECHO    SA,W      CSI$EBFR
                   I$LOCK
                   ECSR      CHANZERO
                   JACTO     CSI$WAITIO
                   OUT,W     CSI$EBCW
                   I$UNLOCK
                   LQ        77
                   MATE      CSI$CR              . <CR>?
                   J         CSI$CHKBS           . NO
                   J         CSI$DONE            . YES
       CSI$CHKBS   MATE      CSI$BSPACE          . BACK SPACE?
                   J         CSI$SAVE            . NO
                   J         CSI$ERASE           . YES
       . WE HAVE A CHARACTER THAT IS NOT A SPECIAL CHARACTER.
       . SAVE IT IN THE BUFFER.
       CSI$SAVE    LSA,U     CSI$BIDX            . SHIFT CHAR TO PROPER POSN
                   LB,L      B1,CSI$BIDX         . COMBINE WITH PRIOR CHARS
                   ROR,W     CSI$BFR,B1                   
                   LA,U      CSI$BIDX            . DEC SHIFT COUNT
                   AN        6,,APOS
                   J         CSI$NXTWRD          . END OF WORD
                   SA,U      CSI$BIDX            . SAVE SHIFT COUNT & LOOP
                   J         CSI$READ
       CSI$NXTWRD  LA        24D                 . RESET SHIFT COUNT
                   SA,U      CSI$BIDX
                   RI,L      CSI$BIDX            . INCREMENT WORD COUNT
                   TA        16D,,YMORE          . END OF BUFFER?
                   J         CSI$DONE            . YES, QUIT
                   J         CSI$READ            . NO, LOOP
       . WE'VE READ AN ENTIRE LINE. QUEUE TO INPUT TO THE CONSOLE
       . COMMAND PROCESSOR (CCP).                   
       CSI$DONE    SYALLOC   B1                  . GET A BFR FROM SYS POOL
                   LB        B2,LI$LEN,B1        . BUMP PAST LIST HEADER
                   LB        B3,0                . POINT TO START OF INPUT BFR
       CSI$COPY    LA,W      CSI$BFR,B3          . COPY INPUT TO WORK AREA
                   SA,W      0,B2
                   TBI       B2,77777            . BUMP INDICES
                   TBI       B3,15D
                   J         CSI$COPY
                   SB,W      B1,CSI$P1           . ADD WORK ARE TO CCP QUEUE
                   LBPJB6    L$INSERT
                   +CCP$Q
       CSI$P1      +0
                   +0                                       
                   SZ,W      CSO$SUSP            . RELEASE OUTPUT TASK
       CSI$YIELD   SLJ       YIELD
                   J         CONS$SVCIQ
       .
       CSI$ERASE   LA,U      CSI$BIDX            . INC SHIFT COUNT
                   A         6D
                   TA        25D,,YMORE          . BEGINNING OF WORD?
                   J         CSI$PRVWORD         . YES
                   SA,U      CSI$BIDX
       CSI$ERASE1  LA        77                  . LOAD MASK
                   LSA,U     CSI$BIDX            . SHIFT TO PROPER PLACE
                   SA,W      CSI$MASK
                   LB,L      B1,CSI$BIDX         . CLEAR BYTE IN CURRENT WORD
                   LA,W      CSI$BFR,B1
                   NOT,W     CSI$MASK
                   SA,W      CSI$BFR,B1
                   J         CSI$READ
       CSI$PRVWORD ZA                            . RESET SHIFT COUNT
                   SA,U      CSI$BIDX                  
                   RD,L      CSI$BIDX,,ANEG      . DECREMENT WORD COUNT
                   J         CSI$ERASE1
                   LA,W      CSI$IIDX            . FORCE START OF BUFFER
                   SA,W      CSI$BIDX
                   J         CSI$READ
       .
       CSI$WAITIO  I$UNLOCK
                   SLJ       YIELD
                   J         CSI$ECHO
       .
       . WAIT FOR A CHARACTER TO APPEAR IN THE KEYBOARD BUFFER
       .
       CSI$CHARW   +0
                   LA,U      CI$HT               . GET KBD BFR HEAD
                   LQ        77777               
                   MATE      CI$HT               . IS THERE ANYTHING IN KBD BFR?
                   J,L       CSI$CHARW           . YES, RETURN
                   SLJ       YIELD               . NO, YIELD AND TRY AGAIN
                   J         CSI$CHARW+1
       .                   
       . GET 1ST CHARACTER FROM KEYBOARD BUFFER INTO A
       .
       CSI$GET     +0
                   LB,U      B1,CI$HT            . GET HEAD PTR
                   RD,U      CI$HT,,ANEG         . DECREMENT IT          
                   J         CSI$GET2
                   LA        CI$BLEN             . HEAD WRAPPED
                   SA,U      CI$HT               . RESET IT
       CSI$GET2    LA,W      CI$CBFR,B1          . GET CHAR IN A
                   J,L       CSI$GET
       .
       . BUFFER INDEX. U = SHIFT COUNT, L = WORD NUMBER
       CSI$IIDX    +24D,0
       CSI$BIDX    +0
       CSI$BLEN    +0
       CSI$BFR     RES       16D
       CSI$SPACE   +'     '                      . SPACE
       CSI$BSPACE  +0,0,0,0,77                   . BACK SPACE
       CSI$CR      +'    ^'                      . CONSOLE CARRIAGE RTN
                   +77
                   +'    -'
                   +'    >'
                   +'     '
       CSI$PBCW    BCW       5,0,CSI$CR
       CSI$EBFR    +0
       CSI$EBCW    BCW       1,0,CSI$EBFR
       CSI$MASK    +0
       . ++++++++++
       . COMMAND PARSING ROUTINES
       . 
       . GET A TOKEN FROM A 16 WORD BUFFER STARTING AT THE
       . GIVEN OFFSET AND WORD. RETURNS TOKEN AS A DOUBLE WORD.
       . TOKENS ARE SPACE AND/OR COMMA DELIMITED. A COMMA IS A TOKEN
       . UNTO ITSLEF. A CHARACTER OF ZERO INDICATES END OF BUFFER.
       .
       . PARAMETERS:
       .   H1 = OFFSET H2 = WORD
       .   BFR ADDR
       .
       . RETURNS:
       .   NEW OFFSET & WORD
       .   TOKEN WORD 1
       .   TOKEN WORD 2
       . ++++++++++
       GETTKN      +0
                   SA,W      GT$A                . SAVE REGISTERS
                   SQ,W      GT$Q
                   SB,W      B5,GT$B5
                   LA,L      GETTKN              . SAVE RETURN ADDR
                   LB,A      B5
                   A         3
                   SA,L      GT$J
       .
                   LA,W      0,B5                . GET PARAMETERS
                   SA,W      GT$BIDX
                   LA,W      1,B5                . GET BFR ADDR
                   SA,W      GT$BFRSTRT          . SAVE START OF BFR ADDR
                   A         15D                 . SAVE END OF BFR ADDR
                   SA,W      GT$BFREND
                   LA,W      1,B5                . GET BFR ADDR AGAIN
                   A,L       GT$BIDX             . BUMP TO 1ST WORD OF INTEREST
                   LB,A      B5       
       .
                   DPL       GT$SPACE            . CLEAR TOKEN
                   DPS       GT$TOKEN                   
       . SKIP LEADING SPACES
       GT$SKPSPC   LQ,W      0,B5                . LOAD WORD FROM BFR
                   LSQ,U     GT$BIDX             . SHIFT TO 1ST CHARACTER
                   ZA                            . GET NEXT CHARACTER TO A
                   LSAQ      6D,,ANOT
                   J         GT$RTRN             . ZERO = END OF BFR                  
                   LQ        77                  . IS IT A SPACE?
                   MATE      GT$SPACE
                   J         GT$NOTSPC           . NO, CHECK FOR COMMA
                   SLJ       GT$BUMP             . BUMP SHIFT COUNT  
                   JT        GT$SKPSPC,,APOS     . NOT END OF BFR
                   J         GT$RTRN             . END OF BFR
       GT$NOTSPC   MATE      GT$COMMA            . IS IT A COMMA?
                   J         GT$SVTKN            . NO
                   SA,W      GT$TEMP             . YES, RETURN COMMA AS TOKEN
                   LA,W      GT$SPACE
                   NOT       77
                   OR,W      GT$TEMP
                   SA,W      GT$TOKEN
                   SLJ       GT$BUMP             . BUMP PAST COMMA
                   J         GT$LJUST                   
       .                                      
       GT$SVTKN    SA,W      GT$TEMP             . SAVE CHARACTER JUST FOUND
                   DPL       GT$TOKEN            . MAKE SPACE IN TOKEN
                   LSAQ      6D
                   DPS       GT$TOKEN
                   LA,W      GT$TOKEN+1          . ADD NEW CHARACTER TO TOKEN
                   NOT       77
                   OR,W      GT$TEMP
                   SA,W      GT$TOKEN+1
                   SLJ       GT$BUMP             . BUMP SHIFT COUNT
                   JT        GT$LJUST,,ANEG      . BFR EXHAUSTED, LEFT JUSTIFY
       GT$NXTCHR   LQ,W      0,B5                . GET NEXT CHARACTER FROM BFR                   
                   LSQ,U     GT$BIDX             . SHIFT TO 1ST CHARACTER
                   ZA                            . GET NEXT CHARACTER TO A
                   LSAQ      6D,,ANOT
                   J         GT$LJUST            . ZERO = END OF BFR                  
                   LQ        77                  . IS IT A SPACE?
                   MATE      GT$SPACE
                   J         GT$NXTCHR1          . NO, CHECK FOR COMMA
                   J         GT$LJUST            . YES, END OF TOKEN
       .
       GT$NXTCHR1  MATE      GT$COMMA            . IS IT A COMMA?
                   J         GT$SVTKN            . NO, CONTINUE                   
       .                   
       GT$LJUST    LA,W      GT$TOKEN            . IS 1ST CHAR OF TOKEN = ' '?
                   LQ        77000
                   LSQ       15D
                   MATE      GT$SPACE
                   J         GT$RTRN             . NO, WE'RE DONE
                   DPL       GT$TOKEN            . YES, SHIFT LEFT
                   LSAQ      6D
                   DPS       GT$TOKEN
                   J         GT$LJUST
       .       
       GT$RTRN     SB,A      B5                  . CALC NEW WORD OFFSET
                   AN,W      GT$BFRSTRT
                   SA,L      GT$BIDX
                   LB,L      B5,GETTKN           . POINT TO PARAMS
                   LA,W      GT$BIDX             . UPDATE RETURN VALUES
                   SA,W      0,B5
                   DPL       GT$TOKEN
                   DPS       1,B5
                   LA,W      GT$A                . RESTORE REGISTERS
                   LQ,W      GT$Q
                   LB,W      B5,GT$B5
       GT$J        J         $
       .
       GT$BUMP     +0
                   LA        6D                  . YES, BUMP SHIFT COUNT
                   RA,U      GT$BIDX
                   TA        25D,,YLESS          . END OF WORD?
                   J,L       GT$BUMP             . NO, RETURN
                   SZ,U      GT$BIDX             . YES, RESET SHIFT COUNT
                   TBI,W     B5,GT$BFREND        . BUMP WORD #
                   J,L       GT$BUMP             . RETURN
                   LA,X      -1                  . SHOW END OF BFR
                   J,L       GT$BUMP             . RETURN
       . 
       GT$A        RES       1
       GT$Q        RES       1
       GT$B5       RES       1
       GT$BIDX     RES       1
       GT$BFRSTRT  RES       1
       GT$BFREND   RES       1
       GT$TOKEN    RES       2
       GT$SPACE    +'          '
       GT$COMMA    +'    ,'
       GT$TEMP     RES       1
       . ++++++++++
       . CONVERT SINGLE WORD INTEGER TO FIELDATA OCTAL
       . 
       . VALUE TO CONVERT PASSED IN Q
       .
       . FIELDATA OCTAL VALUE RETURNED IN AQ
       . ++++++++++
       INT2OCT     +0
                   SB,W      B1,I2O$B1           . SAVE REGS
                   LB        B1,0                . INIT LOOP COUNT
       I2O$LOOP    ZA                            . CLEAR A
                   LSAQ      3                   . GET 1 DIGIT INTO A
                   OR        60                  . CVT TO FIELDATA
                   SA,W      I2O$BFR,B1          . SAVE TO BUFFER
                   TBI       B1,9D               . BUMP COUNTER
                   J         I2O$LOOP
                   CPL       I2O$BFR             . PACK RESULT INTO DOUBLE WORD
                   SA,W      I2O$BFR
                   CPL       I2O$BFR+5
                   SA,W      I2O$BFR+1
                   DPL       I2O$BFR             . RETURN RSLT IN AQ
                   LB,W      B1,I2O$B1           . RESTORE REGS
                   J,L       INT2OCT
       .
       I2O$B1      RES       1                   
       I2O$BFR     RES       10D
       . ++++++++++
       . CONVERT A SINGLE WORD INTEGER TO FIELDATA DECIMAL
       .
       . VALUE PASSED IN Q
       .
       . FIELDATA DECIMAL VALUE RETURNED LEFT JUSTIFIED IN AQ
       . ++++++++++
       INT2DEC     +0
                   SZ,W      I2D$NEG             . CLEAR NEGATIVE FLAG
                   JT        I2D$POS,,QPOS
                   LA        1                   . SET NEGATIVE FLAG
                   SA,W      I2D$NEG
                   NQ                            . MAKE NUMBER POSITIVE
       I2D$POS     SQ,W      GT$Q                . SAVE Q
                   DPL       GT$SPACE            . CLEAR RESULT
                   DPS       GT$TOKEN
       I2D$LOOP    ZA
                   LQ,W      GT$Q
                   D         10D                 . DIV BY 10 TO GET REM
                   SQ,W      GT$Q                . SAVE QUOTIENT
                   OR        60                  . CVT REM TO FIELDATA
                   LSA       24D                 . LEFT JUSTIFY
                   SA,W      GT$TEMP             . SAVE IT
                   DPL       GT$TOKEN            . ADD NEW DIGIT TO RESULT
                   LRSAQ     6D
                   OR,W      GT$TEMP
                   DPS       GT$TOKEN
                   LA,W      GT$Q,,ANOT          . ANYTHING LEFT TO CVT?
                   J         I2D$DONE            . NO
                   LQ,A                          . YES KEEP GOING
                   J         I2D$LOOP
       I2D$DONE    LA,W      I2D$NEG,,ANOT       . NEAGTIVE?
                   J         I2D$RTRN            . NO
                   DPL       GT$TOKEN
                   LRSAQ     6D
                   OR,W      I2D$MINUS
                   DPS       GT$TOKEN
       I2D$RTRN    DPL       GT$TOKEN
                   J,L       INT2DEC
       .
       I2D$NEG     +0                   
       I2D$MINUS   +41,0,0,0,0
       . ++++++++++
       . CONVERT A DOUBLE WORD FIELDATA DECIMAL NUMBER
       . TO A SINGLE WORD INTEGER
       .
       . NUMBER PASSED IN AQ
       .
       . INTEGER RETURNED IN A
       . ++++++++++
       DEC2INT     +0
                   SZ,W      I2D$NEG             . CLEAR NEG FLAG
                   SB,W      B5,GT$B5
                   DPS       GT$A                . SAVE NUMBER
                   SZ,W      D2I$RSLT            . CLEAR RESULT
                   LB        B5,9D               . INIT LOOP COUNT
       D2I$LOOP    DPL       GT$A
                   LSAQ      6D                  . GET NEXT DIGIT
                   DPS       GT$A
                   SQ,A                          . ELIMINATE ZONE
                   LQ        77
                   MATE      GT$SPACE            . IS DIGIT A SPACE?
                   J         D2I$CONT            . NO
                   J         D2I$DONE            . YES
       D2I$CONT    MATE      D2I$MINUS           . IS DIGIT '-'
                   J         D2I$CONT2           . NO
                   RI,W      I2D$NEG             . YES, SET NEG FLAG
                   J         D2I$BUMP
       D2I$CONT2   LLP,A
                   TA        '0',,YLESS          . FORCE NON DECIMALS TO ZERO
                   LA        '0'
                   TA        72,,YMORE
                   LA        '0'     
                   NOT       60
                   SA,W      GT$TEMP
                   LQ,W      D2I$RSLT            . ADD TO 10 * RSLT
                   M         10D
                   AQ,W      GT$TEMP
                   SQ,W      D2I$RSLT 
       D2I$BUMP    JBD       B5,D2I$LOOP         . LOOP 10 TIMES
       .                   
       D2I$DONE    LQ,W      D2I$RSLT
                   LA,W      I2D$NEG,,AZERO      . NEG FLAG SET?
                   NQ                            . YES, NEGATE RESULT
                   SQ,A
                   LB,W      B5,GT$B5
                   J,L       DEC2INT                                 
       .
       D2I$RSLT    +0
       D2I$MINUS   +'    -'
       . ++++++++++
       . CONSOLE COMMAND PROCESSOR
       . ++++++++++
       CCP$TCB     T$CB      EXEC$IFR EXEC$RIR EXEC$PLR CCP$ 'CCP$      '
       .
       CCP$Q       L$IST     
       .
       CCP$        WAITFOR   TCB$WNZRO,CCP$Q+LCB$HEAD . WAIT 4 A COMMAND
       .
                   LB        B1,CCP$Q            . GET PTR TO COMMAND
                   LA,W      LCB$HEAD,B1,ANOT
                   J         CCP$                . ZERO, WAIT SOME MORE
                   SA,W      CCP$P3              . SAVE AS PARAM FOR SY$FREE
                   DEQUEUE   CCP$Q               . REMOVE ITEM FROM QUEUE
       .
                   A         LI$LEN              . BUMP PAST QUE OVERHEAD
                   SZ,W      CCP$P1
                   SA,W      CCP$P2
                   SA,W      CCP$BFR
                   SLJ       GETTKN              . GET 1ST TOKEN (COMMAND)
       CCP$P1      +0
       CCP$P2      +0
                   +0
       .
                   LQ,X      77777
                   LA,W      CCP$P2              . GET TOKEN
                   LB        B2,0                . FIND TOKEN IN TABLE
       CCP$LOOP    MATE      CCP$CMDS,B2
                   J         CCP$NEXT            . NOT FOUND
                   LA,W      CCP$P1              . FOUND, COPY GETTKN PARAMS
                   SA,W      CCP$P4
                   LA,W      CCP$BFR
                   SA,W      CCP$P5      
                   SLJ,L     CCP$PROCS,B2        . EXECUTE COMMAND PROC
       CCP$P4      +0
       CCP$P5      +0
                   J         CCP$DONE
       .                   
       CCP$NEXT    TBI       B2,CCP$NUMCMD-1     . LOOP TO CHECK NEXT                   
                   J         CCP$LOOP
       .
                   SA,W      CCP$ERR             . SEND 'INVALID COMMAND' ERROR
                   PUT       CONS$OUTQ CCP$CREQ CCP$ELEN,CCP$ERR
                   WAITFOR   TCB$WFIN,CCP$CREQ
                   J         CCP$DONE
       .                   
       CCP$DONE    SLJ       SY$FREE
       CCP$P3      +0                   
                   J         CCP$
       .
       CCP$BFR     RES       1
       CCP$ERR     RES       1
                   +' IS NOT A VALID COMMAND ^'
       CCP$ELEN    EQU       $-CCP$ERR                   
       CCP$CREQ    IOREQ     CCP$ELEN,CCP$ERR                   
       .
       . JUMP TABLE OF CONSOLE COMMANDS
       .
       CCP$CMDS    +'TS'                         . TASK STATUS
                   +'SS'                         . SYSTEM POOL STATUS
                   +'MS'                         . FREE MEMORY STATUS
                   +'ST'                         . START A JOB
                   +'CA'                         . CANCEL (KILL) A JOB
                   +'1'                          . OPERATOR INPUT FOR JOB 1
                   +'2'                          . OPERATOR INPUT FOR JOB 2
                   +'3'                          . OPERATOR INPUT FOR JOB 3
                   +'4'                          . OPERATOR INPUT FOR JOB 4
                   +'5'                          . OPERATOR INPUT FOR JOB 5
                   +'6'                          . OPERATOR INPUT FOR JOB 6
                   +'7'                          . OPERATOR INPUT FOR JOB 7
       CCP$NUMCMD  EQU       $-CCP$CMDS
       CCP$PROCS   +CCP$TS
                   +CCP$SS
                   +CCP$MS
                   +CCP$ST
                   +CCP$CA
                   +CCP$ACCEPT1
                   +CCP$ACCEPT2
                   +CCP$ACCEPT3
                   +CCP$ACCEPT4
                   +CCP$ACCEPT5
                   +CCP$ACCEPT6
                   +CCP$ACCEPT7
       CCP$SPACES  +'     '
       CCP$CR      +'    ^'
       . ++++++++++
       . TASK STATUS CONSOLE COMMAND
       . ++++++++++
       CCP$TS      +0
                   LA,L      CCP$TS              . SAVE RTRN ADDRESS
                   A         2
                   SA,L      CTS$J
                   PUT       CONS$OUTQ CCP$CREQ CTS$HLEN,CTS$HDR
                   WAITFOR   TCB$WFIN,CCP$CREQ
       .                   
                   LB,W      B5,SWTCH$LIST+LCB$HEAD . FIRST TASK
       CTS$LOOP    LA,W      CCP$SPACES          . CLEAR THE BUFFER
                   R         7D,,ADV                 
                   SA,W      CTS$BFR
                   LA,W      CCP$CR              . ADD <CR> AT END
                   SA,W      CTS$BFR+6D
                   DPL       TCB$NAME,B5
                   DPS       CTS$BFR+CTS$NAME 
                   LQ        TCB$WDONE           . INTERPRET FLAGS
                   LLP,L     TCB$FLAGS,B5,ANOT
                   J         CTS$F1
                   LA,W      CTS$WDONE
                   J         CTS$FDONE
       CTS$F1      LQ        TCB$WFIN
                   LLP,L     TCB$FLAGS,B5,ANOT
                   J         CTS$F2
                   LA,W      CTS$WFIN
                   J         CTS$FDONE
       CTS$F2      LQ        TCB$WNZRO
                   LLP,L     TCB$FLAGS,B5,ANOT
                   J         CTS$F3
                   LA,W      CTS$WNZRO
                   J         CTS$FDONE
       CTS$F3      LQ        TCB$WCONS
                   LLP,L     TCB$FLAGS,B5,ANOT
                   J         CTS$F4
                   LA,W      CTS$WCONS
                   J         CTS$FDONE
       CTS$F4      LQ        TCB$WTSET
                   LLP,L     TCB$FLAGS,B5,ANOT
                   J         CTS$F5
                   LA,W      CTS$WTSET
                   J         CTS$FDONE
       CTS$F5      LA,W      CCP$SPACES
       CTS$FDONE   LQ,A
                   LA,W      CCP$SPACES
                   LSAQ      24D
                   DPS       CTS$BFR+CTS$FLAGS
                   SB,A      B5                  . IS THIS THE ACTIVE TASK?
                   AN,W      LAST$TASK,,AZERO
                   J         CTS$JOBNO           . NO
                   LA,W      CTS$STAR            . YES
                   SA,W      CTS$BFR+CTS$ACTV
       CTS$JOBNO   LA,U      TCB$FLAGS,B5        . JOB #
                   OR,W      CTS$SPZRO           . CVT TO FIELDATA
                   LSA       18D                 . SHIFT 3 CHARS LEFT
                   SA,W      CTS$BFR+CTS$JOB
       .          
       CTS$WRITE   PUT       CONS$OUTQ CCP$CREQ 7D,CTS$BFR
                   WAITFOR   TCB$WFIN,CCP$CREQ
       .                   
                   LA,W      LI$NEXT,B5,ANOT
                   J         CTS$J
                   LB,A      B5
                   J         CTS$LOOP
       CTS$J       J         $
       .
       CTS$HDR     +'TASK       FLAGS  ACTIVE JOB ^'
       CTS$HLEN    EQU       $-CTS$HDR
       CTS$BFR     RES       7D
       CTS$NAME    EQU       0
       CTS$FLAGS   EQU       2D      
       CTS$ACTV    EQU       4D
       CTS$JOB     EQU       5D
       CTS$WDONE   +'WDONE'
       CTS$WFIN    +'WFIN'
       CTS$WNZRO   +'WNZRO'
       CTS$WCONS   +'WCONS'
       CTS$WTSET   +'WTSET'
       CTS$STAR    +'*'
       CTS$SPZRO   +5,5,5,5,60
       . ++++++++++
       . SYSTEM POOL STATUS CONSOLE COMMAND
       . ++++++++++
       CCP$SS      +0
                   LA,L      CCP$SS              . SAVE RTRN ADDR
                   A         2
                   SA,L      CSS$J
       .
                   DPL       CSS$BS              . BLOCK SIZE
                   DPS       CSS$MSG
                   LQ        SY$BLKSIZE
                   SLJ       INT2DEC
                   SA,W      CSS$VAL
                   PUT       CONS$OUTQ CCP$CREQ CSS$MLEN,CSS$MSG
                   WAITFOR   TCB$WFIN,CCP$CREQ
       .
                   DPL       CSS$NB              . NUMBER OF BLOCKS
                   DPS       CSS$MSG
                   LQ        SY$NUMBLK
                   SLJ       INT2DEC
                   SA,W      CSS$VAL                                      
                   PUT       CONS$OUTQ CCP$CREQ CSS$MLEN,CSS$MSG
                   WAITFOR   TCB$WFIN,CCP$CREQ
       .
                   DPL       CSS$BU              . BLOCKS USED
                   DPS       CSS$MSG
                   LB        B1,0
                   LB        B2,0
                   SZ,W      CSS$COUNT
                   LQ,W      SY$HIGHBIT
       CSS$LOOP    LLP,W     SY$BITMAP,B1,AZERO  . BLOCK ALLOCATED?
                   RI,W      CSS$COUNT           . YES, BUMP COUNT
                   LRSQ      1                   . NEXT BIT
                   TQ        1,,YMORE            . DONE?
                   J         CSS$LOOP            . NO, KEEP GOING
                   TBI       B1,SY$NUMBLK        . YES, TRY NEXT WORD
                   J         CSS$LOOP
                   LQ,W      CSS$COUNT
                   SLJ       INT2DEC
                   SA,W      CSS$VAL
                   PUT       CONS$OUTQ CCP$CREQ CSS$MLEN,CSS$MSG
                   WAITFOR   TCB$WFIN,CCP$CREQ
       .                   
       CSS$J       J         $                                      
       .
       CSS$MSG     RES       2
       CSS$VAL     RES       1
                   +'    ^'
       CSS$MLEN    EQU       $-CSS$MSG                   
       CSS$BS      +'BLK SIZE'
       CSS$NB      +'NUM BLKS'
       CSS$BU      +'BLK USED'
       CSS$COUNT   +0
       . ++++++++++
       . FREE MEMORY STATUS CONSOLE COMMAND
       . ++++++++++
       CCP$MS      +0
                   LA,L      CCP$MS              . SAVE RTRN ADDR
                   A         2
                   SA,L      CMS$J
       .
                   PUT       CONS$OUTQ CCP$CREQ CMS$HLEN,CMS$HDR
                   WAITFOR   TCB$WFIN,CCP$CREQ
       .
                   LB,W      B5,FM$LIST+LCB$HEAD . GET 1ST FREE BLOCK
       CMS$LOOP    LA,W      CCP$SPACES          . CLEAR BUFFER
                   LQ,W      FMCB$START,B5       . START ADDRESS
                   SLJ       INT2OCT
                   LSAQ      24D                 . LJUST 6 OCTAL DIGITS
                   SA,W      CTS$BFR
                   SQ,A
                   LRSA      24D
                   LSA       24D
                   OR,W      CMS$SPACE
                   SA,W      CTS$BFR+1                                              
                   LQ,W      FMCB$END,B5         . END ADDRESS
                   SLJ       INT2OCT
                   LSAQ      24D                 . LJUST 6 OCTAL DIGITS
                   SA,W      CTS$BFR+2
                   SQ,A
                   LRSA      24D
                   LSA       24D
                   OR,W      CMS$SPACE
                   SA,W      CTS$BFR+3
                   LA,W      FMCB$END,B5         . CALC BLK SIZE
                   AN,W      FMCB$START,B5
                   A         1
                   LQ,A
                   SLJ       INT2DEC
                   SA,W      CTS$BFR+4
                   SQ,A
                   LRSA      6D                  . ADD <CR> TO LAST CHARACTER
                   LSA       6D                                              
                   OR        4
                   SA,W      CTS$BFR+5
       .
                   PUT       CONS$OUTQ CCP$CREQ 6D,CTS$BFR
                   WAITFOR   TCB$WFIN,CCP$CREQ                   
       .
                   LA,W      LI$NEXT,B5,ANOT     . GET NEXT FREE BLOCK
                   J         CMS$J
                   LB,A      B5
                   J         CMS$LOOP
       .                                      
       CMS$J       J         $                                      
       .
       CMS$HDR     +'START     END       SIZE^'
       CMS$HLEN    EQU       $-CMS$HDR
       CMS$SPACE   +0,5,5,5,5
       . ++++++++++
       . START JOB CONSOLE COMMAND
       .
       . ST FILE CHANNEL
       . ++++++++++
       CCP$ST      +0                  
                   LA,L      CCP$ST              . SAVE RTRN ADDR
                   LB,A      B1
                   A         2
                   SA,L      CST$J
       .
                   LB        B2,0                . SEE IF THERE IS AN OPEN JOB
       CST$CKJOB   LA,W      JOBS$,B2,ANOT
                   J         CST$GOTJOB
                   TBI       B2,6D
                   J         CST$CKJOB
                   J         CST$NOJOBS
       CST$GOTJOB  SB,A      B2                  
                   A         1                   . MAKE 1 RELATIVE
                   SA,W      CST$JOBNO           . AND SAVE FOR LATER                  
       .
                   LA,W      0,B1                . GET THE NEXT TOKEN FROM CMD
                   SA,W      CST$P1              . THIS IS FILE NAME TO START
                   LA,W      1,B1
                   SA,W      CST$P2
                   SA,W      CST$CMD                  
                   SLJ       GETTKN
       CST$P1      +0
       CST$P2      +0                   
                   +0
                   DPL       CST$P2              . SAVE IT IN MFD PACKET
                   DPS       CST$MFD+MFD$FNAME
       . FINISH PARSING COMMAND LINE
                   LA,W      CST$P1              . GET NEXT TOKEN
                   SA,W      CST$P3              . THIS IS THE DRUM CHAN #
                   LA,W      CST$CMD
                   SA,W      CST$P4
                   SLJ       GETTKN
       CST$P3      +0
       CST$P4      +0
                   +0
                   DPL       CST$P4
                   SLJ       DEC2INT             . CONVERT FD TO INTEGER
                   LA,A      0,,ANOT             . = ZERO?
                   LA        5D                  . YES, DEFAULT TO 5
       . FIND FILE IN MFD                   
                   SA,W      CST$MFD+MFD$CHAN
                   SLJ       MFD$FIND            . TRY TO FIND FILE ON DRUM
                   +CST$MFD
                   LA,W      CST$MFD+MFD$STAT,,AZERO . CHECK STATUS
                   J         CST$NTFND           . OOPS!
       . ALLOCATE REQUIRED MEMORY
                   SZ,W      CST$PBFR
                   SZ,W      CST$IOBFR
                   LA,U      CST$MFD+MFD$TRACKS  . GET SIZE IN TRACKS
                   LSA       10D                 . * 1024 FOR WORDS
                   SA,W      CST$PLEN            . SAVE IT FOR LATER
                   SA,W      CST$P6              . ALLOCATE A BUFFER FOR PGM
                   SLJ       FM$GETMEM
       CST$P6      +0
                   LA,W      $-1,,ANOT           . SUCCESS?
                   J         CST$NOMEM           . NO
                   SA,W      CST$PBFR
       .
                   LA        1024D
                   SA,W      CST$P12
                   SLJ       FM$GETMEM           . ALLOCATE AN I/O BUFFER
       CST$P12     +0
                   LA,W      $-1,,ANOT           . SUCCESS?
                   J         CST$NOMEM           . NO                                                         
                   SA,W      CST$IOBFR
                   A         1023D               . CALC END OF BFR
                   SA,W      CST$EIOBFR
       . COPY PROGRAM TO MEMORY
                   LA,X      -1
                   SA,W      CST$XFER
                   LB,W      B7,CST$PBFR         . GET START OF PGM MEMORY
                   LA,W      CST$MFD+MFD$CHAN    . GET DRUM CHANNEL #
                   LBPJB6    FH8$FDCB            . FIND THE DCB FOR THE DRUM
                   LA,W      FH8$QUE,B2
                   SA,W      CST$P9              . ADD READ REQ TO QUEUE
                   LQ,L      CST$MFD+MFD$TRACKS  . GET 1ST TRACK #
       CST$LOAD    LSQ       10D                 . * 1024 TO GET WORD ADDRESS
                   LA        FH8$RDCONT          . GET READ CONTINUOUS CMD
                   LSA       24D
                   AQ,A                          . ADD TO TRACK ADDRESS
                   SQ,W      CST$DREQ+IR$CMD     . UPDATE I/O PACKET
                   SZ,W      CST$DREQ+IR$FLAGS   . CLEAR FLAGS
                   LBPJB6    L$INSERT
       CST$P9      +0
                   +CST$DREQ
                   +0
                   WAITFOR   TCB$WFIN,CST$DREQ   . WAIT FOR I/O TO FINISH
                   LA,W      CST$DREQ+IR$STAT    . READ OK?
                   RSA       24D,,AZERO
                   J         CST$IOERR           . NO
                   LB,W      B5,CST$IOBFR        . POINT TO I/O BUFFER
                   TBI,X     B5,77777            . BUMP PAST NEXT TRACK LINK
                   LA,W      CST$XFER,,ANEG      . 1ST PASS?
                   J         CST$COPY            . NO
                   LA,W      0,B5                . YES, SAVE TRANSFER ADDR
                   SA,W      CST$XFER
                   TBI,X     B5,77777
       CST$COPY    LA,W      0,B5                . COPY DATA TO PGM MEMORY
                   SA,W      0,B7
                   TBI,X     B7,77777 
                   TBI,W     B5,CST$EIOBFR
                   J         CST$COPY
                   LB,W      B5,CST$IOBFR        . GET NEXT TRACK LINK
                   LA,W      0,B5,ANOT
                   J         CST$CPYDUN          . NO MORE, COPY DONE
                   LQ,A
                   J         CST$LOAD
       . SET UP THE INITIAL TCB
       CST$CPYDUN  LB,W      B5,CST$PBFR         . TCB IS 1ST 140 WORDS OF PGM
                   R         140,,ADV            . CLEAR TCB
                   SZ,W      0,B5
                   LA,W      UI$IFR
                   SA,W      TCB$IFR,B5
                   LA,W      CST$PBFR    
                   LRSA      6D
                   SA,L      TCB$PLR,B5
                   LA,W      CST$PBFR            . CALC END OF PGM ADDR
                   A,W       CST$PLEN            
                   AN        1
                   LRSA      6D
                   SA,U      TCB$PLR,B5
                   LA,W      CST$PBFR
                   SA,W      TCB$RIR,B5
                   LA,W      CST$XFER
                   SA,W      TCB$P,B5
                   LA,W      CST$JOBNO
                   SA,U      TCB$FLAGS,B5
                   DPL       CST$MFD+MFD$FNAME
                   DPS       TCB$NAME,B5
                   DPS       CST$STMSG
                   LA,W      CST$PLEN
                   SA,W      TCB$PLEN,B5
                   SB,W      B5,CST$P11          . ADD TCB TO SWITCH LIST
                   LBPJB6    L$INSERT
                   +SWTCH$LIST
       CST$P11     +0                   
                   +0
                   LB,W      B1,CST$JOBNO        . ADD TCB TO ACTIVE JOB ARRAY
                   SB,W      B5,JOBS$-1,B1
       .          
                   LA,W      CST$IOBFR           . FREE THE I/O BUFFER
                   SA,W      CST$P10
                   SLJ       FM$FREEMEM
       CST$P10     +0 
       .
                   PUT       CONS$OUTQ CCP$CREQ CST$STLEN,CST$STMSG                                     
                   WAITFOR   TCB$WFIN,CCP$CREQ
       .                   
       CST$J       J         $
       .
       CST$FMEM    +0
                   LA,W      CST$PBFR,,ANOT
                   J         CST$NOMEM1
                   SA,W      CST$P7
                   SLJ       FM$FREEMEM
       CST$P7      +0
       CST$NOMEM1  LA,W      CST$IOBFR,,ANOT
                   J         CST$NOMEM2
                   SA,W      CST$P8
                   SLJ       FM$FREEMEM
       CST$P8      +0                                      
       CST$NOMEM2  J,L       CST$FMEM
       .       
       CST$NTFND   DPL       CST$MFD+MFD$FNAME
                   DPS       CST$NFMSG
                   PUT       CONS$OUTQ CCP$CREQ CST$NFLEN,CST$NFMSG
                   WAITFOR   TCB$WFIN,CCP$CREQ
                   J         CST$J
       .
       CST$NOMEM   SLJ       CST$FMEM
                   PUT       CONS$OUTQ CCP$CREQ CST$NMLEN,CST$NMMSG
                   WAITFOR   TCB$WFIN,CCP$CREQ
                   J         CST$J                   
       .
       CST$IOERR   LQ,A      
                   SLJ       INT2OCT
                   DPS       CST$IOSTAT
                   PUT       CONS$OUTQ CCP$CREQ CST$IOLEN,CST$IOMSG
                   WAITFOR   TCB$WFIN,CCP$CREQ
                   J         CST$J
       .
       CST$NOJOBS  PUT       CONS$OUTQ CCP$CREQ CST$NJLEN,CST$NJMSG
                   WAITFOR   TCB$WFIN,CCP$CREQ
                   J         CST$J
       .                    
       CST$NFMSG   RES       2
                   +' NOT FOUND    ^'
       CST$NFLEN   EQU       $-CST$NFMSG
       CST$STMSG   RES       2
                   +' STARTED ^'
       CST$STLEN   EQU       $-CST$STMSG
       CST$NMMSG   +'OUT OF MEMORY ^'
       CST$NMLEN   EQU       $-CST$NMMSG
       CST$NJMSG   +'NO AVAILABLE JOB SLOT   ^'
       CST$NJLEN   EQU       $-CST$NJMSG                  
       CST$CMD     RES       1
       CST$MFD     M$FD
       CST$PBFR    RES       1
       CST$PLEN    RES       1
       CST$EIOBFR  RES       1
       CST$DREQ    IOREQ     1024D,0 FH8CMD FH8$RDCONT,0,0
       CST$IOBFR   EQU       CST$DREQ+IR$BFR
       CST$IOMSG   +'I/O ERROR '
       CST$IOSTAT  RES       2
       CST$IOLEN   EQU       $-CST$IOMSG
       CST$XFER    RES       1
       CST$JOBNO   RES       1
       . ++++++++++
       . CCP$CA
       .
       . CANCEL (KILL) A JOB
       . ++++++++++
       CCP$CA      +0
                   LA,L      CCP$CA              . SAVE RTRN ADDR
                   LB,A      B1
                   A         2
                   SA,L      CCN$J
       .              
                   LA,W      0,B1                . GET THE NEXT TOKEN FROM CMD
                   SA,W      CCN$P1              . THIS IS FILE NAME TO START
                   LA,W      1,B1
                   SA,W      CCN$P2
                   SLJ       GETTKN
       CCN$P1      +0
       CCN$P2      +0                   
                   +0
                   DPL       CCN$P2              . CVT TO INTEGER
                   SLJ       DEC2INT
                   AN        1,,APOS             . MAKE ZERO RELATIVE                   
                   J         CCN$ILLJOB          . ILLEGAL JOB #
                   TA        7D,,YMORE           . IN RANGE?
                   J         CCN$ILLJOB          . NO
                   LB,A      B2                  . GET PTR TO JOB'S TCB
                   LA,W      JOBS$,B2,ANOT
                   J         CCN$ILLJOB          . JOB NOT ACTIVE
                   LB,A      B5       
                   LB        B7,CCN$ECODE        . POINT TO EXIT CODE
                   SLJ       KILL
                   LA        CCP$TCB             . RESET LAST TASK TO THIS
                   SA,W      LAST$TASK           . BECAUSE KILL MESSES WITH IT
       CCN$J       J         $
       .
       CCN$ILLJOB  PUT       CONS$OUTQ CCN$IOREQ CCN$IJLEN,CCN$IJMSG
                   J         CCN$J
       .                   
       CCN$IJMSG   +'ILLEGAL JOB NUMBER'
                   +'    ^'
       CCN$IJLEN   EQU       $-CCN$IJMSG 
       CCN$IOREQ   IOREQ                          
       CCN$ECODE   +49D       
       . ++++++++++
       . ROUTE OPERATOR INPUT TO THE APPROPRIATE JOB
       .
       . B1 = CONS INPUT BFR
       . B2 = COPY OF CONS INPUT FOR JOB
       . ++++++++++
       CCP$ACCEPT  LB,A      B5                  . GET TCB ADDR
                   LQ        TCB$WCONS           . IS IT WAITING FOR CONS IPT?
                   LLP,L     TCB$FLAGS,B5,ANOT
                   J         CCA$BADJOB          . NO
       .
                   LA,W      0,B1                . GET CRNT CONS BFR OFFSET
                   SA,W      CCA$CIDX
                   LA,W      1,B1                . GET CONS BFR ADDRESS
                   SA,W      CCA$CBFR
                   A         15D                 . SAVE END OF BFR ADDR
                   SA,W      CCA$ECBFR
                   LA,W      CCA$CBFR            . GET BFR ADDR AGAIN
                   A,L       CCA$CIDX            . BUMP TO CRNT WORD
                   LB,A      B1
                   SYALLOC   B2                  . ALLOC BFR FOR JOB
                   SB,W      B2,CCA$JBFR
                   SZ        CCA$JIDX            . INIT JOB BFR OFST AND WRD
                   LA        24D                 
                   SA,U      CCA$JIDX
       . SKIP LEADING SPACES
       CCA$SKPSPC  LQ,W      0,B1                . GET WORD FROM CONS BFR
                   LSQ,U     CCA$CIDX            . SHIFT TO CRNT CHAR
                   ZA
                   LSAQ      6D
                   LQ        77                  . IS IT A SPACE
                   MATE      CCP$SPACES
                   J         CCA$NOTSPC          . NO, FOUND START OF IPT
                   LA        6D                  . BUMP SHIFT COUNT
                   RA,U      CCA$CIDX
                   TA        25D,,YLESS          . END OF WORD?
                   J         CCA$SKPSPC          . NO, LOOP
                   SZ,U      CCA$CIDX            . YES, RESET SHIFT COUNT
                   TBI,W     B1,CCA$ECBFR        . BUMP WORD #
                   J         CCA$SKPSPC
                   J         CCA$DONE
       . COPY FROM CONS BFR TO JOB BFR
       CCA$NOTSPC  LSA,U     CCA$JIDX            . SHIFT TO CRNT OFFSET
                   ROR,W     0,B2                . ADD TO CRNT WORD
                   LA        6D                  . BUMP CONS INDEX
                   RA,U      CCA$CIDX
                   TA        25D,,YLESS
                   J         CCA$BMPJOB
                   SZ,U      CCA$CIDX  
                   TBI,W     B1,CCA$ECBFR
                   J         CCA$BMPJOB
                   J         CCA$DONE
       CCA$BMPJOB  LA,U      CCA$JIDX            . DUMP JOB INDEX
                   AN        6D
                   SA,U      CCA$JIDX,,ANEG      . END OF WORD?
                   J         CCA$NXTCHR          . NO
                   LA        24D
                   SA,U      CCA$JIDX
                   TBI       B2,77777
       CCA$NXTCHR  LQ,W      0,B1                . GET NEXT CHAR
                   LSQ,U     CCA$CIDX
                   ZA
                   LSAQ      6D
                   J         CCA$NOTSPC          . LOOP                                     
       .                   
       CCA$DONE    LA,W      CCA$JBFR            . COPY BFR ADDR TO TCB
                   SA,W      TCB$EVTDTA,B5
       .                                                  
       CCA$J       J         $
       .       
       CCP$ACCEPT1 +0
                   LA,L      CCP$ACCEPT1         . SAVE RTRN ADDR
                   LB,A      B1
                   A         2
                   SA,L      CCA$J
                   LA,W      JOBS$,,ANOT         . GET TCB FOR JOB
                   J         CCA$BADJOB
                   J         CCP$ACCEPT
       .       
       CCP$ACCEPT2 +0
                   LA,L      CCP$ACCEPT2         . SAVE RTRN ADDR
                   LB,A      B1
                   A         2
                   SA,L      CCA$J
                   LA,W      JOBS$+1,,ANOT       . GET TCB FOR JOB
                   J         CCA$BADJOB
                   J         CCP$ACCEPT
       .       
       CCP$ACCEPT3 +0
                   LA,L      CCP$ACCEPT3         . SAVE RTRN ADDR
                   LB,A      B1
                   A         2
                   SA,L      CCA$J
                   LA,W      JOBS$+2,,ANOT       . GET TCB FOR JOB
                   J         CCA$BADJOB
                   J         CCP$ACCEPT
       .       
       CCP$ACCEPT4 +0
                   LA,L      CCP$ACCEPT4         . SAVE RTRN ADDR
                   LB,A      B1
                   A         2
                   SA,L      CCA$J
                   LA,W      JOBS$+3,,ANOT       . GET TCB FOR JOB
                   J         CCA$BADJOB
                   J         CCP$ACCEPT
       .       
       CCP$ACCEPT5 +0
                   LA,L      CCP$ACCEPT5         . SAVE RTRN ADDR
                   LB,A      B1
                   A         2
                   SA,L      CCA$J
                   LA,W      JOBS$+4,,ANOT       . GET TCB FOR JOB
                   J         CCA$BADJOB
                   J         CCP$ACCEPT
       .       
       CCP$ACCEPT6 +0
                   LA,L      CCP$ACCEPT6         . SAVE RTRN ADDR
                   LB,A      B1
                   A         2
                   SA,L      CCA$J
                   LA,W      JOBS$+5,,ANOT       . GET TCB FOR JOB
                   J         CCA$BADJOB
                   J         CCP$ACCEPT
       .       
       CCP$ACCEPT7 +0
                   LA,L      CCP$ACCEPT7         . SAVE RTRN ADDR
                   LB,A      B1
                   A         2
                   SA,L      CCA$J
                   LA,W      JOBS$+6,,ANOT       . GET TCB FOR JOB
                   J         CCA$BADJOB
                   J         CCP$ACCEPT
       .
       CCA$BADJOB  PUT       CONS$OUTQ CCP$CREQ CCA$JELEN,CCA$JOBERR
                   WAITFOR   TCB$WFIN,CCP$CREQ
                   J         CCA$J
       .
       CCA$CIDX    RES       1
       CCA$CBFR    RES       1
       CCA$ECBFR   RES       1
       CCA$JIDX    RES       1
       CCA$JBFR    RES       1
       CCA$TEMP    RES       1
       CCA$JOBERR  +'INVALID JOB NUMBER ^'
       CCA$JELEN   EQU       $-CCA$JOBERR
       . ++++++++++
       . SERVICE ALL INSTALLED FH880 DRUM DEVICES.
       . 
       . WE LOOP THROUGH THE LIST OF INSTALLED DEVICES LOOKING 
       . FOR ACTIVITY ON EACH DEVICE'S QUEUE.
       .
       . B1 = CURRENT DCB
       . B2 = QUEUE
       . B5 = QUEUE ITEM
       . ++++++++++
       FH8$TCB     T$CB      EXEC$IFR EXEC$RIR EXEC$PLR FH8$SVCQ 'FH8$SVCQ'
       .
       FH8$SVCQ    LB        B1,FH8$DEVS         . GET 1ST DEVICE
                   LA,W      LCB$HEAD,B1,ANOT
                   J         FH8$YIELD           . LIST EMPTY
                   LB,A      B1
       .
       FH8$LOOP    I$LOCK                        . CHECK IF DEVICE BUSY
                   ECSR      FH8$CHAN,B1
                   JACTO     FH8$BUSY            
                   JACTI     FH8$BUSY
                   I$UNLOCK
       .
                   LB,W      B2,FH8$QUE,B1       . GET 1ST ITEM ON QUEUE
                   LA,W      LCB$HEAD,B2,ANOT
                   J         FH8$NXTDEV          . QUEUE EMPTY
                   LB,A      B5
       .
                   LQ        IR$BUSY             . IS REQUEST ACTIVE?
                   LLP,W     IR$FLAGS,B5,AZERO
                   J         FH8$CHKDUN          . YES, CHECK IF I/O DONE 
       .
                   LA        IR$BUSY             . NO, SET BUSY FLAG
                   ROR,W     IR$FLAGS,B5
                   I$LOCK                        . TERMINATE PRIOR I/O
                   ECSR      FH8$CHAN,B1
                   EXF,W     FH8$TERM
                   I$UNLOCK
       FH8$WAITIO  SB,W      B5,$+2              . SAVE PTR TO IOREQ
                   SLJ       WAIT                . WAIT FOR I/O DONE
                   +0
                   +TCB$WDONE
                   LA,W      IR$FLAGS,B5         . CLEAR THE DONE FLAG
                   NOT       IR$DONE
                   SA,W      IR$FLAGS,B5
                   J         FH8$CONT            . IT'S DONE, CONTINUE
       .
       FH8$CONT    SZ,W      FH8$BCW,B1          . INIT BCW
                   LA,L      IR$COUNT,B5         . GET COUNT
                   LSA       18D
                   SA,W      FH8$BCW,B1
                   LA,W      IR$BFR,B5           . GET BUFFER ADDRESS
                   LSA       13D
                   LRSA      13D
                   ROR,W     FH8$BCW,B1
       .                   
                   LQ        70000               . IS FUNCTION A WRITE?
                   LLP,U     IR$CMD,B5,AZERO
                   J         FH8$TRM             . NO
                   I$LOCK                        . YES
                   ECSR      FH8$CHAN,B1         . START THE I/O
                   EXF,W     IR$CMD,B5                   
                   OUTMON,W  FH8$BCW,B1
                   I$UNLOCK
                   J         FH8$NXTDEV          . GO CHECK NEXT DEVICE
       .                    
       FH8$TRM     LQ        20000               . IS IT A TERM?
                   LLP,U     IR$CMD,B5,ANOT
                   J         FH8$SRCH            . NO
                   I$LOCK                        . YES
                   ECSR      FH8$CHAN,B1         . SEND THE COMMAND
                   EXF,W     IR$CMD,B5
                   I$UNLOCK
                   J         FH8$NXTDEV          . GO CHECK NEXT DEVICE
       .                   
       FH8$SRCH    LQ        04000               . IS IT A SEARCH?
                   LLP,U     IR$CMD,B5,ANOT
                   J         FH8$READ            . NO
                   LQ        02000               . YES, IS IT A SEARCH READ?
                   LLP,U     IR$CMD,B5,AZERO
                   J         FH8$SRCHR           . YES
                   I$LOCK                        . NO
                   ECSR      FH8$CHAN,B1         . START THE I/O
                   EXF,W     IR$CMD,B5           . SEND FUNC CODE
                   EXF,W     IR$CMD2,B5          . SEND SEARCH VALUE
                   I$UNLOCK
                   J         FH8$NXTDEV          . GO CHECK NEXT DEVICE
       .                   
       FH8$SRCHR   I$LOCK                        . YES
                   ECSR      FH8$CHAN,B1         . START THE I/O
                   INMON,W   FH8$BCW,B1          . ACTIVE THE BUFFER
                   EXF,W     IR$CMD,B5           . SEND FUNC CODE
                   EXF,W     IR$CMD2,B5          . SEND SEARCH VALUE
                   I$UNLOCK
                   J         FH8$NXTDEV          . GO CHECK NEXT DEVICE
       .
       FH8$READ    I$LOCK                        
                   ECSR      FH8$CHAN,B1         . START THE I/O
                   INMON,W   FH8$BCW,B1          . ACTIVE THE BUFFER
                   EXF,W     IR$CMD,B5           . SEND FUNC CODE
                   I$UNLOCK
                   J         FH8$NXTDEV          . GO CHECK NEXT DEVICE
       .
       FH8$CHKDUN  LQ        IR$DONE             . IS I/O COMPLETE?
                   LLP,W     IR$FLAGS,B5,ANOT
                   J         FH8$NXTDEV          . NO
                   LA,W      IR$FLAGS,B5         . YES
                   NOT       IR$BUSY             . CLEAR BUSY
                   OR        IR$FIN              . SET FIN
                   SA,W      IR$FLAGS,B5
                   LA,W      FH8$QUE,B1          . REMOVE REQUEST FROM QUEUE
                   SA,W      FH8$DQUE+1
       FH8$DQUE    LBPJB6    L$DEQUEUE
                   +0
                   J         FH8$NXTDEV
       .
       FH8$BUSY    I$UNLOCK            
       FH8$NXTDEV  LA,W      LI$NEXT,B1,ANOT     . GET NEXT DEVICE
                   J         FH8$YIELD           . NO MORE DEVICES
                   LB,A      B1
                   J         FH8$LOOP                   
       .                   
       FH8$YIELD   SLJ       YIELD
                   J         FH8$SVCQ
       .
       FH8$TERM    FH8CMD    FH8$TRWINT,0,0
       . ++++++++++
       . FIND THE DCB FOR THE FH880 ON A GIVEN CHANNEL #
       .
       . PARAMETERS:
       .   CHANNEL TO FIND IN A
       .
       . RETURNS:
       .   PTR TO DCB IN B2 (ZERO IF NOT FOUND)
       . ++++++++++
       FH8$FDCB    SA,W      FH8$SAVEA
                   SQ,W      FH8$SAVEQ
                   LB        B2,FH8$DEVS
                   LA,W      LCB$HEAD,B2,ANOT
                   J         FH8F$NFND
       FH8F$LOOP   LB,A      B2
                   LQ        77
                   LA,W      FH8$SAVEA
                   MATE      FH8$CHAN,B2
                   J         FH8F$NEXT
                   J         FH8F$RTRN
       .
       FH8F$NEXT   LA,W      FH8$NEXT,B2,AZERO
                   J         FH8F$LOOP 
       FH8F$NFND   LB        B2,0                            
       .                   
       FH8F$RTRN   LA,W      FH8$SAVEA
                   LQ,W      FH8$SAVEQ
                   J         0,B6                   
       .                   
       FH8$SAVEA   RES       1
       FH8$SAVEQ   RES       1
       . ++++++++++
       . SERVICE THE CARD DEVICE QUEUE
       .
       . B1 = QUEUE
       . B5 = QUEUE ITEM
       . ++++++++++
       CARD$TCB    T$CB      EXEC$IFR EXEC$RIR EXEC$PLR CARD$SVCQ 'CARD$SVCQ'
       .
       CARD$SVCQ   LB        B1,CARD$Q
                   LA,W      LCB$HEAD,B1,ANOT    . GET 1ST ITEM ON QUEUE
                   J         CRSQ$YIELD          . NOTHING TO DO
                   LB,A      B5
       .                   
                   LQ        IR$BUSY             . IS REQUEST ACTIVE?
                   LLP,W     IR$FLAGS,B5,AZERO
                   J         CRSQ$CHKDUN         . YES, SEE IF IT IS FINISHED
       .
                   LA        IR$BUSY             . NO, SET THE BUSY FLAG
                   ROR,W     IR$FLAGS,B5
                   I$LOCK                        . TERMINATE THE PRIOR I/O
                   ECSR      CHANCARD
                   EXF,W     CRSQ$TERM
                   I$UNLOCK
                   SB,W      B5,CRSQ$WAIT+1
       CRSQ$WAIT   SLJ       WAIT
                   +0
                   +TCB$WDONE
                   LA,W      IR$FLAGS,B5         . CLEAR THE DONE FLAG
                   NOT       IR$DONE
                   SA,W      IR$FLAGS,B5
       .                                                         
                   SZ,W      CRSQ$BCW            . INIT BCW
                   LA,L      IR$COUNT,B5         . GET THE COUNT
                   LSA       18D
                   SA,W      CRSQ$BCW
                   LA,W      IR$BFR,B5           . GET BFR ADDRESS
                   LSA       13D
                   LRSA      13D
                   ROR,W     CRSQ$BCW
                   LA        CARD$WINT           . MAKE SURE WITH INT FLAG SET
                   LSA       24D
                   ROR,W     IR$CMD,B5           
       .                   
                   LQ        40000               . IS IT A READ?
                   LSQ       15D
                   LLP,A     IR$CMD,B5,AZERO
                   J         CRSQ$READ           . YES                   
                   LQ        20000               . IS IT A TERM?
                   LSQ       15D
                   LLP,A     IR$CMD,B5,AZERO
                   J         CRSQ$TRM            . YES
       .           
                   I$LOCK                        . SEND A WRITE COMMAND
                   ECSR      CHANCARD
                   OUT,W     CRSQ$BCW
                   EXF,W     IR$CMD,B5
                   I$UNLOCK
                   J         CRSQ$YIELD
       .
       CRSQ$READ   I$LOCK                        . SEND READ COMMAND
                   ECSR      CHANCARD
                   IN,W      CRSQ$BCW
                   EXF,W     IR$CMD,B5
                   I$UNLOCK  
                   J         CRSQ$YIELD 
       .                   
       CRSQ$TRM    I$LOCK                        . SEND TERMINATE COMMAND
                   ECSR      CHANCARD
                   EXF,W     IR$CMD,B5
                   I$UNLOCK
                   J         CRSQ$YIELD                                                        
       .
       CRSQ$CHKDUN LQ        IR$DONE             . IS I/O COMPLETE?
                   LLP,W     IR$FLAGS,B5,ANOT
                   J         CRSQ$YIELD          . NO, WAIT SOME MORE
                   LA,W      IR$FLAGS,B5         . YES
                   NOT       IR$BUSY             . CLEAR BUSY
                   OR        IR$FIN              . SET FIN
                   SA,W      IR$FLAGS,B5
                   SB,W      B1,CRSQ$DQUE+1      . REMOVE REQUEST FROM QUEUE
       CRSQ$DQUE   LBPJB6    L$DEQUEUE
                   +0
                   J         CRSQ$YIELD                
       .                          
       CRSQ$YIELD  SLJ       YIELD               . WAIT A BIT
                   J         CARD$SVCQ           . AND LOOP
       . 
       CRSQ$TERM   +CARD$TERM+CARD$WINT,0,0,0,0
       CRSQ$BCW    +0
       . ++++++++++
       . SERVICE THE PRINTER QUEUE
       .
       . B1 = QUEUE
       . B5 = QUEUE ITEM
       . ++++++++++
       PRN$TCB     T$CB      EXEC$IFR EXEC$RIR EXEC$PLR PRN$SVCQ 'PRN$SVCQ'
       .
       PRN$SVCQ    LB        B1,PRN$Q
                   LA,W      LCB$HEAD,B1,ANOT    . GET 1ST ITEM ON QUEUE
                   J         PSQ$YIELD           . NOTHING TO DO
                   LB,A      B5
       .                   
                   LQ        IR$BUSY             . IS REQUEST ACTIVE?
                   LLP,W     IR$FLAGS,B5,AZERO
                   J         PSQ$CHKDUN          . YES, SEE IF IT IS FINISHED
       .
                   LA        IR$BUSY             . NO, SET THE BUSY FLAG
                   ROR,W     IR$FLAGS,B5
                   I$LOCK                        . TERMINATE THE PRIOR I/O
                   ECSR      CHANPRN
                   EXF,W     PSQ$TERM
                   I$UNLOCK
                   SB,W      B5,PSQ$WAIT+1
       PSQ$WAIT    SLJ       WAIT
                   +0
                   +TCB$WDONE
                   LA,W      IR$FLAGS,B5         . CLEAR THE DONE FLAG
                   NOT       IR$DONE
                   SA,W      IR$FLAGS,B5
       .                                                         
                   SZ,W      PSQ$BCW             . INIT BCW
                   LA,L      IR$COUNT,B5         . GET THE COUNT
                   LSA       18D
                   SA,W      PSQ$BCW
                   LA,W      IR$BFR,B5           . GET BFR ADDRESS
                   LSA       13D
                   LRSA      13D
                   ROR,W     PSQ$BCW
                   LA        PRN$WINT            . MAKE SURE WITH INT FLAG SET
                   LSA       24D
                   ROR,W     IR$CMD,B5           
       .                   
                   LQ        20000               . IS IT A TERM?
                   LSQ       15D
                   LLP,A     IR$CMD,B5,AZERO
                   J         PSQ$TRM             . YES
       .           
                   I$LOCK                        . SEND A WRITE COMMAND
                   ECSR      CHANPRN
                   OUT,W     PSQ$BCW
                   EXF,W     IR$CMD,B5
                   I$UNLOCK
                   J         PSQ$YIELD
       .
       PSQ$TRM     I$LOCK                        . SEND TERMINATE COMMAND
                   ECSR      CHANPRN
                   EXF,W     IR$CMD,B5
                   I$UNLOCK
                   J         PSQ$YIELD                                                        
       .
       PSQ$CHKDUN  LQ        IR$DONE             . IS I/O COMPLETE?
                   LLP,W     IR$FLAGS,B5,ANOT
                   J         PSQ$YIELD           . NO, WAIT SOME MORE
                   LA,W      IR$FLAGS,B5         . YES
                   NOT       IR$BUSY             . CLEAR BUSY
                   OR        IR$FIN              . SET FIN
                   SA,W      IR$FLAGS,B5
                   SB,W      B1,PSQ$DQUE+1       . REMOVE REQUEST FROM QUEUE
       PSQ$DQUE    LBPJB6    L$DEQUEUE
                   +0
                   J         PSQ$YIELD                
       .                          
       PSQ$YIELD   SLJ       YIELD               . WAIT A BIT
                   J         PRN$SVCQ            . AND LOOP
       . 
       PSQ$TERM    +PRN$TERM++PRN$WINT,0,0,0,0
       PSQ$BCW     +0
       .
       . ++++++++++
       . MASTER FILE DIRECTORY (MFD) ROUTINES
       . ++++++++++
       . ++++++++++
       . SETUP COMMON TO ALL MFD ROUTINES
       .
       . PARAMETERS:
       .   PTR TO MFD FILE DESCRIPTOR CONTAINING CHANNEL AND FILE NAME TO FIND
       .
       . RETURNS:
       .   B1 = PARAMETERS
       .   B2 = DCB
       .   B3 = WORK AREA
       .   B5 = USER FILE DESCRIPTOR
       .   B7 = TEMP FILE DESCRIPTOR
       . ++++++++++
       MFD$COMMON  SLJ       WA$ALLOC            . GET A WORK AREA & SAVE REGS
                   +0
                   LB,W      B3,$-1
       .           
                   SB,L      B6,MFD$J
                   JBD       B6,$+1              . PARAMS ARE WORD BEFORE CALL        
                   JBD       B6,$+1
                   LA,W      0,B6
                   LB,A      B1                  . GET PTR TO PARAMS
                   A         1                   . SAVE RTRN ADDR
                   SA,L      MFD$RTADR,B3
                   LB,W      B5,0,B1             . GET USER FILE DESCRIPTOR
       .          
                   LA        FH8$TSIZE           . ALLOC A READ BUFFER
                   SA,W      MFD$P0         
                   SLJ       FM$GETMEM
       MFD$P0      +0
                   LA,W      $-1,,ANOT           . GET ADDRESS
                   J         MFD$NOMEM           . OUT OF MEMORY!
                   LB,A      B7
       . SET UP INITIAL IOREQ
                   SA,W      MFD$IO+IR$BFR,B3    . SAVE BFR ADDR TO I/O PACKET
                   LA        FH8$TSIZE           . SET BFR SIZE
                   SA,W      MFD$IO+IR$COUNT,B3
                   LA        FH8$RDCONT          . GET READ COMMAND
                   LSA       24D                 . TO HIGH ORDER BITS
                   OR,W      MFD$START           . ADD MFD STRT ADDR
                   SA,W      MFD$IO+IR$CMD,B3    . UPDATE I/O REQ
       .                   
                   LA,W      MFD$CHAN,B5         . GET CHANNEL #
                   LBPJB6    FH8$FDCB            . FIND DCB FOR CHANNEL
                   JBD       B2,MFD$CONT         . DCB FOUND?
                   J         MFD$NFND            . NO
       MFD$CONT    TBI       B2,77777            . UNDO PRIOR JBD
       MFD$J       J         $
       .                   
       MFD$IOERR   LA        MFD$EIO             . SHOW I/O ERROR
                   SA,W      MFD$STAT,B5
                   J         MFD$RTRN
       .                   
       MFD$NOMEM   LA        MFD$ENOMEM          . SHOW OUT OF MEMORY
                   SA,W      MFD$STAT,B5
                   J         MFD$RTRN
       .                   
       MFD$NFND    LA        MFD$ENTFND          . SHOW FILE NOT FOUND
                   SA,W      MFD$STAT,B5
       .                                                               
       MFD$RTRN    LA,L      MFD$RTADR,B3        . SET RETURN ADDR
                   SA,L      MFD$J2
                   LA,W      MFD$IO+IR$BFR,B3    . FREE I/O BUFFER
                   JT        MFD$NOFREE,,AZERO
                   SA,W      $+2
                   SLJ       FM$FREEMEM
                   +0
       MFD$NOFREE  SB,W      B3,$+2
                   SLJ       WA$FREE             . FREE W/A AND RESTORE REGS
                   +0
       MFD$J2      J         $
       . ++++++++++
       . READ 1 TRACK
       . ++++++++++
       MFD$RDTRK   SB,W      B6,MFD$IOB6,B3
                   SZ,W      MFD$IO+IR$FLAGS,B3  . ZERO FLAGS
                   LA,W      FH8$QUE,B2          . GET PTR TO DEVICE QUEUE
                   SA,W      MFDRT$P1            . SAVE AS PARAM
                   LA        MFD$IO,B3           . GET PTR TO IOREQ
                   SA,W      MFDRT$P2            . SAVE AS PARAM
                   SA,W      MFDRT$P3            . SAVE AS PARAM
                   LBPJB6    L$INSERT            . ADD READ CMD TO QUEUE
       MFDRT$P1    +0
       MFDRT$P2    +0
                   +0
                   SLJ       WAIT                . WAIT FOR I/O TO COMPLETE
       MFDRT$P3    +0
                   +TCB$WFIN
       .       
                   LB,W      B6,MFD$IOB6,B3
                   J         0,B6
       . ++++++++++
       . REWRITE LAST TRACK
       . ++++++++++            
       MFD$WRTRK   SB,W      B6,MFD$IOB6,B3
                   LQ,W      MFD$IO+IR$CMD,B3    . GET LAST READ CMD
                   LSAQ      6D                  . SHIFT OUT CMD 
                   LA        FH8$WRITE           . REPLACE WITH WRITE CMD
                   RSAQ      6D
                   SQ,W      MFD$IO+IR$CMD,B3                   
                   SZ,W      MFD$IO+IR$FLAGS,B3  . ZERO FLAGS
                   LA,W      FH8$QUE,B2          . GET PTR TO DEVICE QUEUE
                   SA,W      MFDC$P6             . SAVE AS PARAM
                   LA        MFD$IO,B3           . GET PTR TO IOREQ
                   SA,W      MFDC$P7             . SAVE AS PARAM
                   LBPJB6    L$INSERT            . ADD READ CMD TO QUEUE
       MFDC$P6     +0
       MFDC$P7     +0
                   +0
                   LA        MFD$IO,B3           . GET IOREQ ADDR
                   SA,W      MFDC$P8             . SAVE AS PARAM
                   SLJ       WAIT                . WAIT FOR I/O TO COMPLETE
       MFDC$P8     +0
                   +TCB$WFIN
       .                   
                   LB,W      B6,MFD$IOB6,B3
                   J         0,B6
       . ++++++++++
       . FIND A FILE IN THE MFD
       .
       . PARAMETERS:
       .   PTR TO MFD FILE DESCRIPTOR CONTAINING CHANNEL AND FILE NAME TO FIND
       .
       . RETURNS:
       .   POPULATED MFD FILE DESCRIPTOR
       .   STATUS 0 = OK, STATUS <> 0 FILE NOT FOUND
       .
       . REGISTERS:
       .   B1 = PARAMETERS
       .   B2 = DCB
       .   B3 = WORK AREA
       .   B5 = USER FILE DESCRIPTOR
       .   B7 = MFD FILE DESCRIPTOR
       . ++++++++++
       MFD$FIND    +0
                   LBPJB6    MFD$COMMON          . DO COMMON SETUP
                   LBPJB6    MFD$FIND2
                   J         MFD$RTRN
       .                   
       MFD$FIND2   SB,W      B6,MFD$B6,B3
       MFDF$READ   LBPJB6    MFD$RDTRK           . READ NEXT MFD TRACK
                   LA,W      MFD$IO+IR$STAT,B3,AZERO . CHECK STATUS
                   J         MFD$IOERR           . OOPS!
                   LB,W      B7,MFD$IO+IR$BFR,B3 . B7 = TOP OF BFR                   
       MFDF$LOOP   DPL       MFD$FNAME,B5        . NAMES MATCH?
                   DPTE      MFD$FNAME,B7
                   J         MFDF$BUMP           . NO
                   SZ,W      MFD$STAT,B5         . YES, SHOW FILE FOUND
                   DPL       MFD$FNAME,B7        . COPY FILE DESCRIPTOR TO USER
                   DPS       MFD$FNAME,B5
                   DPL       MFD$TRACKS,B7
                   DPS       MFD$TRACKS,B5
       .
       MFDF$RTRN   LB,W      B6,MFD$B6,B3                   
                   J         0,B6
       .
       MFDF$BUMP   SB,A      B7                  . BUMP BFR PTR
                   A         MFD$LEN
                   LB,A      B7
                   LA,W      MFD$TRACKS,B7       . LASTLEN = 7777777777 (EOT)?
                   LQ,X      77777
                   MATE      MFDF$ALL1
                   J         MFDF$LOOP           . NO, TRY AGAIN
                   LA,W      MFD$LASTLEN,B7,ANOT . YES, NEXT TRACK
                   J         MFDF$NFND           . NO MORE, NOT FOUND
                   LSA       10D                 . MULT BY TRACK SIZE (1024)
                   SA,W      MFD$TEMP,B3
                   LA        FH8$RDCONT          . GET READ COMMAND
                   LSA       24D                 . TO HIGH ORDER BITS
                   OR,W      MFD$TEMP,B3         . ADD NEXT TRACK ADDR
                   SA,W      MFD$IO+IR$CMD,B3    . UPDATE IOREQ
                   J         MFDF$READ           
       .
       MFDF$NFND   LA        MFD$ENTFND
                   SA,W      MFD$STAT,B5
                   J         MFDF$RTRN                   
       . ++++++++++
       . CREATE A NEW FILE IN THE MFD OR OVERWRITE AN EXISTING ONE.
       .
       . PARAMETERS:
       .   PTR TO MFD FILE DESCRIPTOR CONTAINING CHANNEL AND FILE NAME TO
       .   BE CREATED / OVERWRITTEN.
       .
       . RETURNS:
       .   POPULATED MFD FILE DESCRIPTOR
       .   STATUS 0 = OK, STATUS <> 0 FILE NOT CREATED
       .
       . REGISTERS:
       .   B1 = PARAMETERS
       .   B2 = DCB
       .   B3 = WORK AREA
       .   B5 = USER FILE DESCRIPTOR
       .   B7 = MFD FILE DESCRIPTOR
       . ++++++++++
       MFD$CREATE  +0
                   LBPJB6    MFD$COMMON          . DO COMMON SETUP
                   TSET      MFD$DLOCK           . LOCK MFD UNTIL DONE
                   LBPJB6    MFD$FIND2           . CHECK IF FILE EXISTS
                   LA,W      MFD$STAT,B5,ANOT    . CHECK STATUS
                   J         MFDC$RMFILE         . FILE EXISTS
       .
       MFDC$NEW    LA        FH8$RDCONT          . RESET TO 1ST TRACK OF MFD
                   LSA       24D
                   OR,W      MFD$START
                   SA,W      MFD$IO+IR$CMD,B3
                   DPL       MFD$FNAME,B5        . SAVE FILE NAME
                   DPS       MFD$SVNAME,B3
                   DPL       MFDC$ZERO           . FIND AN EMPTY SLOT IN MFD
                   DPS       MFD$FNAME,B5
                   LBPJB6    MFD$FIND2           
                   LA,W      MFD$STAT,B5,AZERO   . DID WE FIND ONE?
                   J         MFDC$MTY            . NO, NEED TO EXPAND MFD                   
                   LA,W      MFD$CHAN,B5         . YES, ALLOC A TRACK
                   SA,W      MFDC$P5
                   SLJ       MFD$ALLOC
       MFDC$P5     +0
                   LA,L      MFDC$P5,,APOS       . UPDATE MFDS 
                   J         MFDC$DFULL          . OOPS!
                   SA,L      MFD$TRACKS,B5
                   SA,L      MFD$TRACKS,B7
                   LA        1
                   SA,U      MFD$TRACKS,B5
                   SA,U      MFD$TRACKS,B7
                   SZ,W      MFD$LASTLEN,B5
                   SZ,W      MFD$LASTLEN,B7
                   DPL       MFD$SVNAME,B3
                   DPS       MFD$FNAME,B7
                   LBPJB6    MFD$WRTRK           . REWRITE THE TRACK
                   LA,W      MFD$IO+IR$STAT,B3,AZERO . CHECK STATUS
                   J         MFDC$IOERR          . OOPS! 
       .
                   SZ,W      MFD$STAT,B5
       MFDC$RTRN   SZ,W      MFD$DLOCK
                   J         MFD$RTRN
       .
       MFDC$DFULL  LA        MFD$EDFULL
                   SA,W      MFD$STAT,B5
                   J         MFDC$RTRN
       .                   
       MFDC$MTY    LOG       MFD$MTYMSG
                   J         $,,STOP
       .
       . DELETE AN EXISTING FILE IN PREPARTION FOR CREATING A NEW ONE
       .
       MFDC$RMFILE LA,W      MFD$IO+IR$CMD,B3    . SAVE CRNT MFD DRUM ADDR
                   LSA       6D
                   LRSA      6D
                   SA,W      MFD$CURTRK
                   LA,L      MFD$TRACKS,B7       . SAVE STARTING TRACK ADDR
                   SA,W      MFD$TEMP,B3
                   SZ,W      MFD$FNAME,B7        . CLEAR MFD ENTRY
                   SZ,W      MFD$FNAME+1,B7
                   SZ,W      MFD$TRACKS,B7
                   SZ,W      MFD$LASTLEN,B7
                   LBPJB6    MFD$WRTRK           . REWRITE TRACK
                   LA,W      MFD$IO+IR$STAT,B3,AZERO . CHECK STATUS
                   J         MFDC$IOERR          . OOPS!
       . FREE A TRACK
       MFDC$FREE   LA,L      MFD$CHAN,B5         . RETURN TRACK TO POOL
                   SA,U      MFD$P15
                   LA,L      MFD$TEMP,B3
                   SA,L      MFD$P15
                   SLJ       MFD$FREE
       MFD$P15     +0
       . READ NEXT TRACK OF FILE
                   LA,W      MFD$TEMP,B3         . CALC WORD ADDRESS OF TRACK
                   LSA       10D
                   SA,W      MFD$TEMP,B3
                   LA        FH8$RDCONT          . SET UP READ COMMAND
                   LSA       24D
                   OR,W      MFD$TEMP,B3
                   SA,W      MFD$IO+IR$CMD,B3 
                   LBPJB6    MFD$RDTRK           . 
                   LA,W      MFD$IO+IR$STAT,B3,AZERO . CHECK STATUS
                   J         MFDC$IOERR          . OOPS! 
       .
                   LB,W      B6,MFD$IO+IR$BFR,B3 . GET NEXT TRACK # 
                   LA,W      0,B6,ANOT
                   J         MFDC$NEW            . NO MORE, GO CREATE FILE
                   SA,W      MFD$TEMP,B3
                   J         MFDC$FREE
       .
       MFDC$IOERR  LA        MFD$EIO             . SHOW I/O ERROR
                   SA,W      MFD$STAT,B5
                   J         MFDC$RTRN
       .                   
       MFDC$MFD    M$FD 
       . ++++++++++
       . UPDATE A FILE'S MFD ENTRY
       .
       . PARAMETERS
       .   MFD
       .
       . RETURN
       .   MFD WITH STATUS CODE SET
       .
       . REGISTERS
       .   B1 = PARAMETERS
       .   B2 = DCB
       .   B3 = WORK AREA
       .   B5 = USER FILE DESCRIPTOR
       .   B7 = MFD FILE DESCRIPTOR
       . ++++++++++
       MFD$UPDATE  +0
                   LBPJB6    MFD$COMMON          . DO COMMON SETUP
                   LA,U      MFD$TRACKS,B5       . SAVE IMPORTANT STUFF
                   SA,U      MFD$UTEMP,B3
                   LA,L      MFD$LASTLEN,B5
                   SA,L      MFD$UTEMP,B3
                   TSET      MFD$DLOCK           . LOCK MFD UNTIL DONE
                   LBPJB6    MFD$FIND2           . CHECK IF FILE EXISTS
                   LA,W      MFD$STAT,B5,AZERO   . CHECK STATUS
                   J         MFDU$NFND           . FILE DOESN'T EXISTS
       .
                   LA,U      MFD$UTEMP,B3        . UPDATE MFD ENTRY
                   SA,U      MFD$TRACKS,B7
                   LA,L      MFD$UTEMP,B3
                   SA,W      MFD$LASTLEN,B7
                   LBPJB6    MFD$WRTRK                   
                   LA,W      MFD$IO+IR$STAT,B3,AZERO . CHECK STATUS
                   J         MFDU$IOERR          . OOPS!
       .       
                   SZ,W      MFD$STAT,B5
       MFDU$RTRN   SZ,W      MFD$DLOCK
                   J         MFD$RTRN
       .       
       MFDU$IOERR  LA        MFD$EIO             . SHOW I/O ERROR
                   SA,W      MFD$STAT,B5
                   J         MFDU$RTRN
       .                   
       MFDU$NFND   LA        MFD$ENTFND          . SHOW FILE NOT FOUND
                   SA,W      MFD$STAT,B5
                   J         MFDU$RTRN                   
       . 
       . ++++++++++
       . ALLOCATE A TRACK FROM FREE TRACK POOL
       .
       . PARAMETERS:
       .   DRUM CHANNEL # 
       .
       . RETURNS:
       .   ALLOCATED TRACK #, -1 IF DRUM FULL
       .
       . REGISTERS:
       .   B3 = WORK AREA
       .   B7 = I/0 BFR
       . ++++++++++
       MFD$ALLOC   +0
                   SLJ       WA$ALLOC            . GET A WORK AREA & SAVE REGS
                   +0
                   LB,W      B3,$-1
                   LA,L      MFD$ALLOC
                   LB,A      B1                  . GET PTR TO PARAMS
                   A         1                   . SAVE RTRN ADDR
                   SA,L      MFD$RTADR,B3
       .          
                   LA        FH8$VSIZE           . ALLOC A READ BUFFER
                   SA,W      MFDA$P0         
                   SLJ       FM$GETMEM
       MFDA$P0     +0
                   LA,W      $-1,,ANOT           . GET ADDRESS
                   J         MFD$NOMEM           . OUT OF MEMORY!
                   LB,A      B7
       . SET UP INITIAL IOREQ
                   SA,W      MFD$IO+IR$BFR,B3    . SAVE BFR ADDR TO I/O PACKET
                   LA        FH8$VSIZE           . SET BFR SIZE
                   SA,W      MFD$IO+IR$COUNT,B3
                   LA        FH8$RDCONT          . GET READ COMMAND
                   LSA       24D                 . TO HIGH ORDER BITS
                   OR,W      MFD$VOLST           . ADD VOL ID STRT ADDR
                   SA,W      MFD$IO+IR$CMD,B3    . UPDATE I/O REQ
       .                   
                   LA,W      0,B1                . GET THE CHANNEL #
                   LBPJB6    FH8$FDCB            . FIND DCB FOR CHANNEL
                   JBD       B2,MFDA$CONT        . DCB FOUND?
                   J         MFD$NFND            . NO
       MFDA$CONT   TBI       B2,77777            . UNDO PRIOR JBD
                   TSET      MFD$VLOCK           . LOCK VOLUME ID
                   LBPJB6    MFD$RDTRK           . READ THE VOLUME ID 
                   LA,W      MFD$IO+IR$STAT,B3,AZERO . CHECK STATUS
                   J         MFDA$IOERR          . OOPS!                   
       .
                   SB,A      B7                  . CALC START / END OF BMP
                   A         2D
                   LB,A      B7
                   A         205D
                   SA,W      MFD$BMPEND
                   LA,X      -30D                . INIT TRACK #
                   SA,W      MFD$TRKNO
       MFDA$LOOP   LA,W      MFD$TRKNO
                   A         30D
                   SA,W      MFD$TRKNO
                   LA,W      0,B7
                   LQ,X      77777               . ANY FREE TRACKS
                   MATE      MFDF$ALL1
                   J         MFDA$GOT1           . YES
                   TBI,W     B7,MFD$BMPEND       . BUMP
                   J         MFDA$LOOP           . & LOOP
                   J         MFDA$DRMFUL         . NO MORE ROOM                                                      
       .
       MFDA$GOT1   LQ        1                   . INIT MASK
                   LSQ       29D
       MFDA$LOOP2  LLP,W     0,B7,ANOT           . TRACK AVAILABLE?
                   J         MFDA$GOTTRK                          
                   RI,W      MFD$TRKNO           . BUMP TRACK #
                   LRSQ      1D                  . SHIFT MASK
                   J         MFDA$LOOP2          . TRY AGAIN
       .
       MFDA$GOTTRK SQ,A                          . SET TRACK USED BIT
                   ROR,W     0,B7
                   LA        FH8$WRITE           . GET WRITE COMMAND
                   LSA       24D                 . TO HIGH ORDER BITS
                   OR,W      MFD$VOLST           . ADD VOL ID STRT ADDR
                   SA,W      MFD$IO+IR$CMD,B3    . UPDATE I/O REQ
                   LBPJB6    MFD$WRTRK           . REWRITE THE TRACK
                   LA,W      MFD$IO+IR$STAT,B3,AZERO . CHECK STATUS
                   J         MFDA$IOERR           . OOPS!                   
                   LA,W      MFD$TRKNO
                   SA,W      0,B1                  
       .       
       MFDA$RTRN   SZ,W      MFD$VLOCK
                   LA,L      MFD$RTADR,B3        . SET RETURN ADDR
                   SA,L      MFDA$J
                   LA,W      MFD$IO+IR$BFR,B3    . FREE I/O BUFFER
                   SA,W      $+2
                   SLJ       FM$FREEMEM
                   +0
                   SB,W      B3,$+2
                   SLJ       WA$FREE             . FREE W/A AND RESTORE REGS
                   +0
       MFDA$J      J         $
       .                   
       MFDA$IOERR  LA        MFD$EIO             . SHOW I/O ERROR
                   SA,W      MFD$STAT,B5
                   J         MFDA$RTRN
       .                   
       MFDA$DRMFUL LA,X      -1                  . SHOW DRUM FULL
                   SA,W      0,B1
                   J         MFDA$RTRN
       . ++++++++++
       . RETURN A TRACK TO THE FREE TRACK POOL
       .
       . PARAMETERS:
       .   DRUM CHANNEL #,TRACK TO FREE
       . ++++++++++
       MFD$FREE    +0
                   SLJ       WA$ALLOC            . GET A WORK AREA & SAVE REGS
                   +0
                   LB,W      B3,$-1
                   LA,L      MFD$FREE
                   LB,A      B1                  . GET PTR TO PARAMS
                   A         1                   . SAVE RTRN ADDR
                   SA,L      MFD$RTADR,B3
       .          
                   LA        FH8$VSIZE           . ALLOC A READ BUFFER
                   SA,W      MFDR$P0         
                   SLJ       FM$GETMEM
       MFDR$P0     +0
                   LA,W      $-1,,ANOT           . GET ADDRESS
                   J         MFD$NOMEM           . OUT OF MEMORY!
                   LB,A      B7
       . SET UP INITIAL IOREQ
                   SA,W      MFD$IO+IR$BFR,B3    . SAVE BFR ADDR TO I/O PACKET
                   LA        FH8$VSIZE           . SET BFR SIZE
                   SA,W      MFD$IO+IR$COUNT,B3
                   LA        FH8$RDCONT          . GET READ COMMAND
                   LSA       24D                 . TO HIGH ORDER BITS
                   OR,W      MFD$VOLST           . ADD VOL ID STRT ADDR
                   SA,W      MFD$IO+IR$CMD,B3    . UPDATE I/O REQ
       .                   
                   LA,U      0,B1                . GET THE CHANNEL #
                   LBPJB6    FH8$FDCB            . FIND DCB FOR CHANNEL
                   JBD       B2,MFDR$CONT        . DCB FOUND?
                   J         MFD$NFND            . NO
       MFDR$CONT   TBI       B2,77777            . UNDO PRIOR JBD
                   TSET      MFD$VLOCK           . LOCK VOLUME ID
                   LBPJB6    MFD$RDTRK           . READ THE VOLUME ID
                   LA,W      MFD$IO+IR$STAT,B3,AZERO . CHECK STATUS
                   J         MFDR$IOERR          . OOPS!                   
       .
                   LQ,L      0,B1                . GET TRACK TO FREE
                   ZA
                   D         30D                 . GET WORD # IN Q, BIT # IN A        
                   SA,U      MFD$TEMP,B3         . SAVE BIT #
                   SQ,L      MFD$TEMP,B3         . SAVE WORD #
                   SB,A      B7                  . CALC INDEX OF WORD
                   A,L       MFD$TEMP,B3
                   A         2D
                   LB,A      B7
                   LA        29D                 . CALC BIT MASK
                   AN,U      MFD$TEMP,B3
                   SA,U      MFD$TEMP,B3
                   LA        1
                   LSA,U     MFD$TEMP,B3
                   SA,W      MFD$TEMP,B3
                   LA,W      0,B7                . CLEAR THE BIT
                   NOT,W     MFD$TEMP,B3
                   SA,W      0,B7
                   LA        FH8$WRITE           . GET WRITE COMMAND
                   LSA       24D                 . TO HIGH ORDER BITS
                   OR,W      MFD$VOLST           . ADD VOL ID STRT ADDR
                   SA,W      MFD$IO+IR$CMD,B3    . UPDATE I/O REQ
                   LBPJB6    MFD$WRTRK           . REWRITE THE TRACK
                   LA,W      MFD$IO+IR$STAT,B3,AZERO . CHECK STATUS
                   J         MFDR$IOERR           . OOPS!                   
       .       
       MFDR$RTRN   SZ,W      MFD$VLOCK
                   LA,L      MFD$RTADR,B3        . SET RETURN ADDR
                   SA,L      MFDR$J
                   LA,W      MFD$IO+IR$BFR,B3    . FREE I/O BUFFER
                   SA,W      $+2
                   SLJ       FM$FREEMEM
                   +0
                   SB,W      B3,$+2
                   SLJ       WA$FREE             . FREE W/A AND RESTORE REGS
                   +0
       MFDR$J      J         $
       .                   
       MFDR$IOERR  LA        MFD$EIO             . SHOW I/O ERROR
                   SA,W      MFD$STAT,B5
                   J         MFDR$RTRN
       .
       MFDF$ALL1   +7777777777
       MFDC$ZERO   DLD       0
       MFD$DLOCK   +0
       MFD$VLOCK   +0
       MFD$BMPEND  +0
       MFD$TRKNO   +0
       MFD$CURTRK  +0
       MFD$MTYMSG  +6
                   +'MFD EXPANSION NOT IMPLEMENTED'
       MFD$START   +FH8$MFD
       MFD$VOLST   +FH8$VOLID
       MFD$RTADR   EQU       WA$USER
       MFD$IO      EQU       MFD$RTADR+1
       MFD$TEMP    EQU       MFD$IO+IR$LEN
       MFD$B6      EQU       MFD$TEMP+1
       MFD$IOB6    EQU       MFD$B6+1
       MFD$SVNAME  EQU       MFD$IOB6+1
       MFD$UTEMP   EQU       MFD$SVNAME+2
       . ++++++++++
       . EXECUTIVE RETURN ROUTINES
       .
       . ON ENTRANCE TO ALL OF THESE ROUTINES:
       .   B5 = CURRENT TCB
       .   B7 = RETURN ADDRESS / PARAMETER LIST PTR
       . ++++++++++
       .
       . ++++++++++
       . ER$EXIT
       .
       . TERMINATE THE CURRENTLY ACTIVE TASK (JOB)
       . 
       . PARAMETERS:
       .   EXIT CODE
       . ++++++++++
       ER$EXIT     SLJ       KILL
                   J         SWITCHER
       . ++++++++++
       . ER$YIELD
       . 
       . ALLOW CURRENT TASK TO YIELD THE PROCESSOR
       .
       . PARAMETERS:
       .   NONE
       .
       . ++++++++++       
       ER$YIELD    SLJ       YIELD
                   J         ER$RTRN
       .                   
       ER$WAIT     .
       . ++++++++++
       . ER$ACCEPT
       .
       . WRITE A PROMPT TO THE CONSOLE AND WAIT FOR INPUT
       .
       . PARAMETERS:
       .   PROMPT LENGTH
       .   PROMPT BFR ADDRESS
       .   REPLY LENGTH
       .   REPLY BFR ADDRESS
       .
       . ++++++++++
       ER$ACCEPT   SYALLOC   B1                  . GET A BFR FOR IOREQ
                   SYALLOC   B2                  . GET AN I/O BFR
                   LQ,W      0,B7                . SET UP OUTPUT REQ
                   TQ        16D,,YMORE          . LIMIT PROMPT TO 15 WORDS
                   LQ        15D
                   SQ,W      IR$COUNT,B1
                   SB,A      B2                  . SET COPY LIMIT
                   AQ,A
                   SQ,L      ERA$PLIMIT
                   SB,W      B2,IR$BFR,B1
       .
                   LA,U      TCB$FLAGS,B5        . GET JOB # 
                   LSA       6D                  . LEFT 1 CHAR
                   OR,W      ERA$PROMPT          . TO FIELDATA
                   SA,W      0,B2                . PUT IN I/O BFR
                   RI,W      IR$COUNT,B1         . BUMP COUNT TO INC THIS
       .                   
                   LQ,W      1,B7                . GET PROMPT ADDR
                   TOABSQ                        . CVT TO ABSOLUTE ADDR
                   SQ,A
                   LB,A      B6
       .
                   LB        B3,1,B2             . COPY PROMPT TO MY I/O BFR
       ERA$PCOPY   LA,W      0,B6
                   SA,W      0,B3
                   TBI,X     B6,77777
       ERA$PLIMIT  TBI       B3,0
                   J         ERA$PCOPY
       .
                   SB,W      B1,ERA$P1           . WRITE PROMPT TO CONSOLE
                   SB,W      B1,ERA$P2
                   LBPJB6    L$INSERT
                   +CONS$OUTQ         
       ERA$P1      +0
                   +0
                   SLJ       WAIT
       ERA$P2      +0
                   TCB$WFIN
                   SYFREE    B2                  . FREE THE OUTPUT BFRS
                   SYFREE    B1
       .
                   WAITFOR   TCB$WCONS,0         . WAIT FOR OPERATOR REPLY
                   LA,W      TCB$EVTDTA,B5       . GET CONS BFR ADDR
                   LB,A      B1
                   LQ,W      2,B7                . GET REPLY SIZE
                   TQ        17D,,YMORE          . LIMIT TO 16 WORDS
                   LQ        16D
                   AQ,A                          . CALC COPY LIMIT
                   ANQ       1
                   SQ,L      ERA$CLIMIT
                   LQ,W      3,B7                . GET REPLY BFR ADDR
                   TOABSQ                        . CVT TO ABSOLUTE ADDR
                   SQ,A
                   LB,A      B6
       .
       ERA$CCOPY   LA,W      0,B1                . COPY TO USER BFR
                   SA,W      0,B6
                   TBI,X     B6,77777
       ERA$CLIMIT  TBI       B1,0 
                   J         ERA$CCOPY
       .
                   LB,W      B1,TCB$EVTDTA,B5    . FREE THE CONS BFR
                   SYFREE    B1
                   SZ,W      TCB$EVTDTA,B5
       .                    
                   LA,W      TCB$ER$P,B5         . BUMP PAST PARAMS
                   A         4
                   SA,W      TCB$ER$P,B5
                   J         ER$RTRN
       .
       ERA$PROMPT  +50,50,5,60,41                . ** 0-
       . ++++++++++
       . ER$TYPE
       .
       . WRITE A MESSAGE TO THE CONSOLE
       .
       . PARAMETERS:
       .   BUFFER LENGTH
       .   BUFFER ADDRESS
       .
       . ++++++++++
       ER$TYPE     SYALLOC   B1                  . GET A BFR FROM SYS POOL
                   LA,W      0,B7                . COPY PARAMS TO I/O PKT
                   SA,W      IR$COUNT,B1
                   LQ,W      1,B7                . GET BFR ADDR
                   TOABSQ                        . CVT TO ABSOLUTE ADDR
                   SQ,W      IR$BFR,B1
                   SB,W      B1,ERT$P1
                   SB,W      B1,ERT$P2
                   LBPJB6    L$INSERT            . SEND REQ TO CONSOLE
                   +CONS$OUTQ
       ERT$P1      +0
                   +0                            
                   SLJ       WAIT                . WAIT FOR I/O TO FIN
       ERT$P2      +0                   
                   TCB$WFIN
       .
                   LA,W      TCB$ER$P,B5         . BUMP RTN ADDR PAST PARAMS
                   A         2
                   SA,W      TCB$ER$P,B5                                      
       .       
                   SYFREE    B1                  . FREE SYS POOL
                   J         ER$RTRN
       . ++++++++++
       . ER$OPEN
       .
       . OPEN A FILE
       .                   
       . PARAMETERS:
       .   DTF
       .
       . ++++++++++
       ER$OPEN     SYALLOC   B1                  . GET A WORK AREA
                   RI,W      TCB$ER$P,B5         . BUMP PAST PARAM
                   LA,W      0,B7,ANOT           . GET PTR TO DTF
                   J         ERO$RTRN            . ZERO, HMMM!
                   TOABS                         . CVT TO ABSOLUTE
                   LB,A      B6
                   LQ        DTF$TYPE            . GET FILE TYPE
                   LLP,L     DTF$FLAGS,B6
                   LB,A      B3
                   J,L       ERO$JUMP,B3         . GO TO PROPER ROUTINE
       
       .
       ERO$ISOPN   LA        DTF$EOPEN           . SET ALREADY OPEN STATUS
                   SA,U      DTF$FLAGS,B6
                   J         ERO$ERTRN
       .
       ERO$NFND    LA        DTF$ENFND           . SET FILE NOT FOUND STATUS
                   SA,U      DTF$FLAGS,B6
                   J         ERO$ERTRN
       .
       ERO$UNK     LA        DTF$EUNK            . SET UNKNOW FILE TYPE STATUS
                   SA,U      DTF$FLAGS,B6
                   J         ERO$ERTRN
       .
       ERO$DFULL   LA        DTF$EDFULL          . SET DRUM FULL ERROR
                   SA,U      DTF$FLAGS,B6
                   J         ERO$ERTRN
       .
       ERO$RDERR   LA        DTF$ERDERR          . SHOW I/O ERROR ON READ
                   SA,U      DTF$FLAGS,B6
                   J         ERO$ERTRN
       .
       ERO$NOMEM   LA        DTF$ENOMEM          . SHOW OUT OF MEMORY
                   SA,U      DTF$FLAGS,B6
                   J         ERO$ERTRN
       .                   
       ERO$INUSE   LA        DTF$EINUSE          . SHOW OUT OF MEMORY
                   SA,U      DTF$FLAGS,B6
                   J         ERO$ERTRN
       .                   
       ERO$ERTRN   LA,W      DTF$ERRPROC,B6,AZERO . RETURN TO ERROR RTN
                   SA,W      TCB$ER$P,B5                   
       .                          
       ERO$RTRN    SYFREE    B1
                   J         ER$RTRN
       .
       ERO$JUMP    +ERO$UNK
                   +ER$OPENSD
                   +ER$OPENCR
                   +ER$OPENCP
                   +ER$OPENPR
                   +ERO$UNK
                   +ERO$UNK
                   +ERO$UNK
       . ++++++++++
       . ER$OPENSD
       .
       . OPEN A SEQUENTIAL DRUM FILE
       .
       . REGISTERS:
       .   B1 = WORK AREA
       .   B6 = DTF
       . ++++++++++
       ER$OPENSD   SB,W      B6,ERO$B6,B1        . SAVE DTF PTR
       .
                   LA,L      DTF$CHAN,B6         . SET UP MFD REQUEST
                   SA,W      MFD$CHAN,B1
                   DPL       DTF$NAME,B6
                   DPS       MFD$FNAME,B1
                   SB,W      B1,ERO$P2
       .                             
                   LQ        DTF$OPEN            . ALREADY OPEN?
                   LLP,L     DTF$FLAGS,B6,AZERO
                   J         ERO$ISOPN           . YES, OOPS
                   LQ        DTF$INPUT           . OPEN FOR INPUT?
                   LLP,L     DTF$FLAGS,B6,ANOT
                   J         ERO$OUTPUT          . NO
       .                   
                   SLJ       MFD$FIND            . FIND THE FILE
       ERO$P2      +0
                   LB,W      B6,ERO$B6,B1        . RESTORE DTF PTR
                   LA,W      MFD$STAT,B1,AZERO   . CHECK STATUS
                   J         ERO$ERR             . OOPS!
       .
       ERO$SUCCESS LA,W      MFD$TRACKS,B1       . COPY START TRACK #
                   SA,W      DTF$TRACKS,B6
                   LA,W      MFD$LASTLEN,B1      . COPY # WORDS LAST TRACK
                   SA,W      DTF$LASTLEN,B6                   
                   SZ,W      DTF$POSN,B6         . INIT POSITION
                   LA,L      DTF$CHAN,B6         . FIND DCB FOR CHANNEL
                   LBPJB6    FH8$FDCB
                   LA,W      FH8$QUE,B2          . GET QUE PTR FOR DCB
                   LB,W      B6,ERO$B6,B1
                   SA,W      DTF$QUEUE,B6
                   SZ,U      DTF$FLAGS,B6        . CLEAR STATUS
                   LA,L      DTF$FLAGS,B6
                   OR        DTF$OPEN            . SET THE OPEN FLAG
                   NOT       DTF$EOF             . CLEAR THE EOF FLAG
                   SA,L      DTF$FLAGS,B6
       . 
                   J         ERO$RTRN                   
       .
       ERO$OUTPUT  SB,W      B1,ERO$P3           . CREATE A NEW FILE
                   SLJ       MFD$CREATE
       ERO$P3      +0
                   LB,W      B6,ERO$B6,B1        . RESTORE DTF PTR
                   LA,W      MFD$STAT,B1,AZERO   . CHECK STATUS
                   J         ERO$ERR             . OOPS!
                   LA        1023D               . SET # WORDS IN TRACK
                   SA,W      DTF$TRKLEN,B6
                   J         ERO$SUCCESS         . OK!
       .
       ERO$ERR     LB,A      B7                  . GO TO APPROPRIATE ERROR RTN
                   J         ERO$MFDERR,B7      
       . 
       . JUMP TABLE FOR MFD ERROR CODES
       .
       ERO$MFDERR  +0
                   J         ERO$NFND
                   J         ERO$NOMEM
                   J         ERO$RDERR
                   J         ERO$DFULL       
       . ++++++++++
       . ER$OPENCR
       .
       . OPEN A CARD READER FILE
       . 
       . PARAMETERS:
       .    DTF
       .
       . REGISTERS:
       .   B1 = WORK AREA
       .   B5 = TCB
       .   B6 = DTF
       . ++++++++++
       ER$OPENCR   LQ        DTF$OPEN            . ALREADY OPEN?
                   LLP,L     DTF$FLAGS,B6,AZERO
                   J         ERO$ISOPN           . YES, OOPS
       .                   
                   LA,W      CRDR$INUSE,,AZERO   . IS READER IN USE?
                   J         ERO$INUSE           . YES
                   LA,X      -1                  . NO SET IN USE FLAG
                   SA,W      CRDR$INUSE
       . SET READER TRANSLATE MODE
                   LQ        DTF$COLIMG          . COLUMN IMAGE?
                   LLP,L     DTF$FLAGS,B6,ANOT
                   J         EROR$CHKROW         . NO
                   LA,W      EROR$COLIMG
                   J         EROR$MSET
       EROR$CHKROW LQ        DTF$ROWIMG          . ROW IMAGE?
                   LLP,L     DTF$FLAGS,B6,ANOT
                   J         EROR$ISXLAT         . NO
                   LA,W      EROR$ROWIMG
                   J         EROR$MSET 
       EROR$ISXLAT LA,W      EROR$XLATE 
       EROR$MSET   SA,W      EROR$CMD
                   ECSR      CHANCARD
                   EXF,W     EROR$CMD
       . POPULATE THE DTF
                   LA        CARD$Q
                   SA,W      DTF$QUEUE,B6        . SET QUEUE PTR
                   LA,W      CHANCARD            . SET CHANNEL #
                   SA,L      DTF$CHAN,B6
                   SZ,U      DTF$FLAGS,B6        . CLEAR STATUS
                   LA,L      DTF$FLAGS,B6
                   OR        DTF$OPEN            . SET OPEN FLAG
                   NOT       DTF$EOF             . CLEAR EOF FLAG
                   SA,L      DTF$FLAGS,B6
                   LA        TCB$CRASG           . SET CR ASSIGNED FLAG IN TCB
                   ROR,L     TCB$FLAGS,B5
       .                   
                   J         ERO$RTRN            . DONE
       .
       EROR$XLATE  +CARD$RXLATE+CARD$WINT,0,0,0,0
       EROR$COLIMG +CARD$RCIMG+CARD$WINT,0,0,0,0
       EROR$ROWIMG +CARD$RRIMG+CARD$WINT,0,0,0,0
       EROR$CMD    +0                   
       .                   
       . ++++++++++
       . ER$OPENCP
       .
       . OPEN A CARD PUNCH FILE
       . 
       . PARAMETERS:
       .    DTF
       .
       . REGISTERS:
       .   B1 = WORK AREA
       .   B5 = TCB
       .   B6 = DTF
       . ++++++++++
       ER$OPENCP   LQ        DTF$OPEN            . ALREADY OPEN?
                   LLP,L     DTF$FLAGS,B6,AZERO
                   J         ERO$ISOPN           . YES, OOPS
       .                   
                   LA,W      CPUN$INUSE,,AZERO   . IS PUNCH IN USE?
                   J         ERO$INUSE           . YES
                   LA,X      -1                  . NO SET IN USE FLAG
                   SA,W      CPUN$INUSE
       . SET PUNCH TRANSLATE MODE
                   LQ        DTF$COLIMG          . COLUMN IMAGE?
                   LLP,L     DTF$FLAGS,B6,ANOT
                   J         EROU$CHKROW         . NO
                   LA,W      EROU$COLIMG
                   J         EROU$MSET
       EROU$CHKROW LQ        DTF$ROWIMG          . ROW IMAGE?
                   LLP,L     DTF$FLAGS,B6,ANOT
                   J         EROU$ISXLAT         . NO
                   LA,W      EROU$ROWIMG
                   J         EROU$MSET 
       EROU$ISXLAT LA,W      EROU$XLATE 
       EROU$MSET   SA,W      EROU$CMD
                   ECSR      CHANCARD
                   EXF,W     EROU$CMD
       . POPULATE THE DTF
                   LA        CARD$Q
                   SA,W      DTF$QUEUE,B6        . SET QUEUE PTR
                   LA,W      CHANCARD            . SET CHANNEL #
                   SA,L      DTF$CHAN,B6
                   SZ,U      DTF$FLAGS,B6        . CLEAR STATUS
                   LA,L      DTF$FLAGS,B6
                   OR        DTF$OPEN            . SET OPEN FLAG
                   NOT       DTF$EOF             . CLEAR EOF FLAG
                   SA,L      DTF$FLAGS,B6
                   LA        TCB$CPASG           . SET CP ASSIGNED FLAG IN TCB
                   ROR,L     TCB$FLAGS,B5
       .                   
                   J         ERO$RTRN            . DONE
       .
       EROU$XLATE  +CARD$PXLATE+CARD$WINT,0,0,0,0
       EROU$COLIMG +CARD$PCIMG+CARD$WINT,0,0,0,0
       EROU$ROWIMG +CARD$PRIMG+CARD$WINT,0,0,0,0
       EROU$CMD    +0                   
       .                   
       . ++++++++++
       . ER$OPENPR
       .
       . OPEN A PRINTER FILE
       . 
       . PARAMETERS:
       .    DTF
       .
       . REGISTERS:
       .   B1 = WORK AREA
       .   B5 = TCB
       .   B6 = DTF
       . ++++++++++
       ER$OPENPR   LQ        DTF$OPEN            . ALREADY OPEN?
                   LLP,L     DTF$FLAGS,B6,AZERO
                   J         ERO$ISOPN           . YES, OOPS
       .                   
                   LA,W      CPRN$INUSE,,AZERO   . IS PRINTER IN USE?
                   J         ERO$INUSE           . YES
                   LA,X      -1                  . NO SET IN USE FLAG
                   SA,W      CPRN$INUSE
       . POPULATE THE DTF
                   LA        PRN$Q
                   SA,W      DTF$QUEUE,B6        . SET QUEUE PTR
                   LA,W      CHANPRN             . SET CHANNEL #
                   SA,L      DTF$CHAN,B6
                   SZ,U      DTF$FLAGS,B6        . CLEAR STATUS
                   LA,L      DTF$FLAGS,B6
                   OR        DTF$OPEN            . SET OPEN FLAG
                   NOT       DTF$EOF             . CLEAR EOF FLAG
                   SA,L      DTF$FLAGS,B6
                   LA        TCB$PRASG           . SET PR ASSIGNED FLAG IN TCB
                   ROR,L     TCB$FLAGS,B5
       .                   
                   J         ERO$RTRN            . DONE
       .
       ERO$B6      EQU       IR$LEN                                      
       . ++++++++++
       . ER$CLOSE
       .
       . CLOSE A FILE
       .                   
       . PARAMETERS:
       .   DTF
       .
       . ++++++++++
       ER$CLOSE    SYALLOC   B1                  . GET A WORK AREA
                   RI,W      TCB$ER$P,B5         . BUMP PAST PARAM
                   LA,W      0,B7,ANOT           . GET PTR TO DTF
                   J         ERO$RTRN            . ZERO, HMMM!
                   TOABS                         . CVT TO ABSOLUTE
                   LB,A      B6
       .                   
                   LQ        DTF$OPEN            . IS FILE OPEN?
                   LLP,L     DTF$FLAGS,B6,ANOT
                   J         ERO$RTRN            . NO, NOTHING TO DO
                   LA,L      DTF$FLAGS,B6        . YES, RESET OPEN & EOF FLAGS
                   NOT       DTF$OPEN+DTF$EOF    
                   SA,L      DTF$FLAGS,B6
       .                   
                   LQ        DTF$TYPE            . GET FILE TYPE
                   LLP,L     DTF$FLAGS,B6
                   LB,A      B3
                   J,L       ERC$JUMP,B3         . GO TO PROPER ROUTINE
       .
                   J         ERO$RTRN
       .
       ERC$JUMP    +ERO$UNK
                   +ER$CLOSESD
                   +ER$CLOSECR
                   +ER$CLOSECP
                   +ER$CLOSEPR
                   +ERO$UNK
                   +ERO$UNK
                   +ERO$UNK
       . ++++++++++
       . ER$CLOSESD
       .
       . CLOSE A SEQUENTIAL DRUM FILE
       .
       . REGISTERS:
       .   B1 = WORK AREA
       .   B6 = DTF
       . ++++++++++                   
       ER$CLOSESD  LQ        DTF$OUTPUT          . IS IT AN OUTPUT FILE?
                   LLP,L     DTF$FLAGS,B6,ANOT
                   J         ERO$RTRN            . NO, WE'RE DONE
                   SB,W      B6,ERR$B7,B1        . SAVE DTF PTR
       .                   
                   SLJ       ERR$IOCALC          . CALC I/O BFR ADDRS
                   LB,W      B5,ERR$IOBFR,B1     . CLEAR NEXT TRACK
                   SZ,W      0,B5
                   LA,U      DTF$POSN,B6         . SAVE # WORDS LAST TRK
                   SA,W      ERR$LSTWRD,B1
                   LA,L      DTF$POSN,B6         . GET TRACK #
                   SA,W      ERC$P0
                   SLJ       ERW$WRTRACK         . WRITE THE LAST TRACK
       ERC$P0      +0                   
       .                   
                   LA,L      DTF$CHAN,B6         . SET UP MFD REQUEST
                   SA,W      MFD$CHAN,B1
                   DPL       DTF$NAME,B6
                   DPS       MFD$FNAME,B1
                   SB,W      B1,ERC$P1
       .                   
                   LA,U      DTF$TRACKS,B6
                   SA,U      MFD$TRACKS,B1
                   LA,W      ERR$LSTWRD,B1
                   SA,W      MFD$LASTLEN,B1
                   SLJ       MFD$UPDATE
       ERC$P1      +0
                   LB,W      B6,ERO$B6,B1
                   LA,W      MFD$STAT,B1,AZERO   . CHECK STATUS
                   J         ERO$ERR             . OOPS!
                   J         ERO$RTRN                          
       . ++++++++++
       . ER$CLOSECR
       .
       . CLOSE A CARD READER FILE
       .
       . ++++++++++                   
       ER$CLOSECR  SZ,W      CRDR$INUSE          . CLEAR READER IN USE FLAG
                   LA,L      TCB$FLAGS,B5        . CLR CR ASSIGNED FLAG IN TCB
                   NOT       TCB$CRASG
                   SA,L      TCB$FLAGS,B5
                   J         ERO$RTRN                   
       .                   
       . ++++++++++
       . ER$CLOSECP
       .
       . CLOSE A CARD PUNCH FILE
       .
       . ++++++++++                   
       ER$CLOSECP  SZ,W      CPUN$INUSE          . CLEAR PUNCH IN USE FLAG
                   LA,L      TCB$FLAGS,B5        . CLR CP ASSIGNED FLAG IN TCB
                   NOT       TCB$CPASG
                   SA,L      TCB$FLAGS,B5
                   J         ERO$RTRN                   
       .                   
       . ++++++++++
       . ER$CLOSEPR
       .
       . CLOSE A PRINTER FILE
       .
       . ++++++++++                   
       ER$CLOSEPR  SZ,W      CPRN$INUSE          . CLEAR PRINTER IN USE FLAG
                   LA,L      TCB$FLAGS,B5        . CLR PR ASSIGNED FLAG IN TCB
                   NOT       TCB$PRASG
                   SA,L      TCB$FLAGS,B5
                   J         ERO$RTRN                   
       . ++++++++++
       . ER$READ
       .
       . READ A RECORD FROM A FILE
       .
       . PARAMETERS:
       .   DTF
       .
       . ++++++++++
       ER$READ     SYALLOC   B1                  . GET A WORK AREA
                   RI,W      TCB$ER$P,B5         . BUMP PAST PARAM
                   LA,W      0,B7,ANOT           . GET PTR TO DTF
                   J         ERR$RTRN            . ZERO, HMMM!
                   TOABS                         . CVT TO ABSOLUTE
                   LB,A      B6
                   LQ        DTF$OPEN            . FILE OPEN?
                   LLP,L     DTF$FLAGS,B6,ANOT
                   J         ERR$NOTOPEN         . NO
                   LQ        DTF$EOF             . AT END OF FILE?
                   LLP,L     DTF$FLAGS,B6,AZERO
                   J         ERR$EOF             . YES
                   LQ        DTF$TYPE            . GET FILE TYPE
                   LLP,L     DTF$FLAGS,B6
                   LB,A      B3
                   J,L       ERR$JUMP,B3         . GO TO PROPER ROUTINE
       .
       ERR$EOF     LB,W      B5,LAST$TASK        . SET EOF FLAG
                   LA        DTF$EOF
                   ROR,L     DTF$FLAGS,B1
                   LA,W      DTF$EOFPROC,B6,AZERO . RETURN TO EOF ROUTINE
                   SA,W      TCB$ER$P,B5
                   J         ERR$RTRN
       .
       ERR$NOTOPEN LA        DTF$ENOTOPN         . SHOW FILE NOT OPEN
                   SA,U      DTF$FLAGS,B6
                   J         ERR$ERTRN
       .                   
       ERR$ERTRN   LB,W      B5,LAST$TASK        . RE-GET PTR TO TCB
                   LA,W      DTF$ERRPROC,B6,AZERO . RETURN TO ERR ROUTINE
                   SA,W      TCB$ER$P,B5
       .                   
       ERR$RTRN    SYFREE    B1                  . RELEASE WORK AREA
                   J         ER$RTRN            
                   
       .
       ERR$JUMP    +ERR$RTRN
                   +ER$READSD
                   +ER$READCR
                   +ERR$RTRN                     . PUNCH
                   +ERR$RTRN                     . PRINTER
                   +ERR$RTRN                     . UNUSED
                   +ERR$RTRN                     . UNUSED
                   +ERR$RTRN                     . UNUSED
       . ++++++++++
       . ER$READSD
       .
       . READ A SEQUENTIAL DRUM FILE
       .
       . REGISTERS:
       .   B1 = WORK AREA
       .   B5 = I/O BFR
       .   B6 = DTF
       .   B7 = RECORD BFR
       . ++++++++++
       ER$READSD   LQ        DTF$INPUT           . FILE OPEN FOR INPUT?
                   LLP,L     DTF$FLAGS,B6,ANOT
                   J         ERR$NOTOPEN         . NO
                   LA,U      DTF$POSN,B6,AZERO   . GET CRNT BFR OFFSET
                   J         ERR$CONT1           . NOT ZERO, CONTINUE
                   LA,L      DTF$TRACKS,B6       . ZERO, READ 1ST TRACK
                   SA,W      $+2
                   SLJ       ERR$RDTRACK
                   +0
       .                   
       ERR$CONT1   SB,W      B7,ERR$B7,B1
       .
                   SLJ       ERR$IOCALC          . CALC BFR POINTERS
                   SLJ       ERR$RCALC
       ERR$LOOP    LA,W      0,B5                . COPY 1 WORD
                   SA,W      0,B7 
                   SA,W      ERR$LSTWRD,B1       . SAVE FOR LATER                                 
       ERR$FIXED   TBI,W     B5,ERR$IOEND,B1     . BUMP I/O BFR PTR
                   J         ERR$TSTVAR          . OK
                   J         ERR$FRD             . END, READ NEXT TRACK
       ERR$TSTVAR  LQ        DTF$VARREC          . VARIABLE LENGTH RECORDS?
                   LLP,L     DTF$FLAGS,B6,ANOT   
                   J         ERR$BMPREC          . NO
                   LA,W      ERR$LSTWRD,B1       . LAST WORD = 7777777777?
                   NA        0,,ANOT
                   J         ERR$DONE            . YES           
       ERR$BMPREC  TBI,W     B7,ERR$RECEND,B1    . BUMP REC BFR PTR
                   J         ERR$LOOP            . OK
       .
       ERR$DONE    SB,A      B5                  . CALC CRNT BFR OFFSET
                   AN,W      ERR$IOBFR,B1
                   SA,U      DTF$POSN,B6         . SAVE IT 
       .
                   LB,W      B5,LAST$TASK        . RESTORE REGS
                   LB,W      B7,ERR$B7,B1                                                                           
                   J         ERR$RTRN            . END OF REC BFR
       .
       ERR$FRD     LB,W      B5,ERR$IOBFR,B1     . GET NEXT TRACK #
                   LA,W      0,B5,ANOT
                   J         ERR$EOF             . ZERO, END OF FILE
                   SA,W      $+2                 . NOT ZERO, READ IT
                   SLJ       ERR$RDTRACK
                   +0
                   SLJ       ERR$IOCALC
                   J         ERR$TSTVAR                   

       .
       . READ 1 TRACK FROM FILE
       .
       ERR$RDTRACK +0
                   LB,W      B5,LAST$TASK        . RE-GET PTR TO TCB
                   LA,L      ERR$RDTRACK         . GET RTRN ADDR
                   LB,A      B2
                   A         1                   . BUMP PAST PARAM
                   SA,W      ERR$RTADR,B1        . SAVE IT FOR LATER
       .                   
                   LA,W      DTF$IOBFR,B6        . SET UP IOREQ FOR READ
                   TOABS
                   SA,W      IR$BFR,B1
                   LA        1024D
                   SA,W      IR$COUNT,B1         
                   LA,W      0,B2                . GET TRACK #
                   SA,L      DTF$POSN,B6
                   LSA       10D                 . MULT BY 1024 TO GET WORDS
                   SA,W      ERR$TEMP,B1
                   LA        FH8$RDCONT          . COMBINE COMMAND WITH ADDR
                   LSA       24D
                   OR,W      ERR$TEMP,B1
                   SA,W      IR$CMD,B1
                   SZ,W      IR$FLAGS,B1         . ZERO FLAGS
                   LA,W      DTF$QUEUE,B6        . QUEUE THE REQUEST
                   SA,W      ERR$P1
                   SB,W      B1,ERR$P2
                   SB,W      B1,ERR$P3
                   SB,W      B6,ERR$TEMP,B1      . SAVE B6
                   LBPJB6    L$INSERT
       ERR$P1      +0
       ERR$P2      +0
                   +0                   
                   LB,W      B6,ERR$TEMP,B1      . RESTORE B6
                   SLJ       WAIT
       ERR$P3      +0
                   TCB$WFIN
                   LA,W      IR$STAT,B1,AZERO    . CHECK THE STATUS
                   J         ERR$RDERR           . OOPS!
                   LA        1                   . SET BFR OFFSET TO 1
                   SA,U      DTF$POSN,B6
                   LA        1023D               . SET # WORDS IN TRACK
                   SA,W      DTF$TRKLEN,B6
                   SB,W      B7,ERR$B7,B1
                   LB,W      B7,IR$BFR,B1        . DID WE JUST READ LAST TRK?
                   LA,W      0,B7,AZERO          
                   J         ERR$CONT2           . NO
                   LA,W      DTF$LASTLEN,B6      . YES, GET # WORDS LAST TRACK
                   SA,W      DTF$TRKLEN,B6
       .
       ERR$CONT2   LB,W      B7,ERR$B7,B1
                   LB,W      B2,ERR$RTADR,B1     . GET RETURN ADDR
                   J         0,B2                . & RETURN TO CALLER   
       .
       ERR$RDERR   LA        DTF$ERDERR          . SHOW I/O ERROR ON READ
                   SA,U      DTF$FLAGS,B6
                   J         ERR$ERTRN
       .
       . CALCULATE ADDRESS OF CRNT WORK OF I/O BUFFER AND THE
       . END OF BUFFER ADDRESS
       .
       ERR$IOCALC  +0
                   LB,W      B5,LAST$TASK        . RESTORE TCB PTR
                   LA,W      DTF$IOBFR,B6        . GET I/O BFR ADDR
                   TOABS
                   SA,W      ERR$IOBFR,B1
                   LA,U      DTF$POSN,B6         . GET CRNT BFR OFFSET
                   A,W       ERR$IOBFR,B1        . ADD TO BFR ADDRESS
                   LB,A      B5                  . B5 = ADDR CRNT WORD
                   LA,W      ERR$IOBFR,B1         . GET I/O BFR ADDR
                   A,W       DTF$TRKLEN,B6       . CALC END OF BFR ADDR
                   SA,W      ERR$IOEND,B1
                   J,L       ERR$IOCALC
       .
       ERR$RCALC   +0
                   SB,W      B5,ERR$TEMP,B1
                   LB,W      B5,LAST$TASK        . RESTORE TCB PTR
                   LA,L      DTF$RECBFR,B6       . GET RECORD BFR ADDR
                   TOABS
                   LB,A      B7                  . B7 = ADDR REC BFR
                   A,U       DTF$RECBFR,B6       . CALC END OF BFR ADDR
                   AN        1
                   SA,W      ERR$RECEND,B1
                   LB,W      B5,ERR$TEMP,B1
                   J,L       ERR$RCALC
       .                                      
       ERR$TEMP    EQU       IR$LEN
       ERR$RTADR   EQU       ERR$TEMP+1
       ERR$B7      EQU       ERR$RTADR+1
       ERR$IOBFR   EQU       ERR$B7+1
       ERR$IOEND   EQU       ERR$IOBFR+1
       ERR$RECEND  EQU       ERR$IOEND+1
       ERR$LSTWRD  EQU       ERR$RECEND+1
       . ++++++++++
       . ER$READCR
       .
       . READ A CARD READER FILE
       .
       . REGISTERS:
       .   B1 = WORK AREA
       .   B6 = DTF
       .   B7 = RECORD BFR 
       . ++++++++++
       ER$READCR   LA,L      DTF$RECBFR,B6       . GET PTR TO RECORD BFR
                   TOABS
                   SA,W      IR$BFR,B1
                   LA,U      DTF$RECBFR,B6       . GET BFR SIZE
                   SA,W      IR$COUNT,B1
                   LA        CARD$READ+CARD$WINT . READ AND FEED WITH INT
                   LSA       24D
                   SA,W      IR$CMD,B1
                   SZ,W      IR$FLAGS,B1
                   LA        CARD$Q              . SET PARAMS FOR INSERT
                   SA,W      ERCR$INS+1    
                   SB,W      B1,ERCR$INS+2
                   SB,W      B1,ERCR$WAIT+1
                   SB,W      B6,ERR$TEMP,B1                   
       ERCR$INS    LBPJB6    L$INSERT
                   +0
                   +0
                   +0
                   LB,W      B6,ERR$TEMP,B1        . RESTORE B6
       ERCR$WAIT   SLJ       WAIT
                   +0
                   +TCB$WFIN
                   LA,W      IR$STAT,B1          . CHECK THE STATUS
                   LRSA      24D
                   JT        ERCR$CHKEOF,,AZERO  . ZERO, ODD BUT OK
                   JNE       CARD$OK,ERCR$RDERR  . NOT NORMAL COMPLETION
       ERCR$CHKEOF LB,W      B7,IR$BFR,B1        . GET 1ST WORD OF BFR
                   LA,W      0,B7
                   LQ        77770
                   LSQ       15D
                   MATE      ERCR$EOF
                   J         ERR$RTRN            . NOT EOF
                   J         ERR$EOF
       .
       ERCR$RDERR  JNE       CARD$EINTLK,ERR$RDERR . NOT INTERLOCK ERROR?
                   LA        DTF$EINTLK          . SHOW INTERLOCK ERROR IN DTF
                   SA,U      DTF$FLAGS,B6
                   J         ERR$ERTRN
       
       ERCR$EOF    +'/*'
       . ++++++++++
       . ER$WRITE
       .
       . WRITE A RECORD TO A FILE
       .
       . PARAMETERS:
       .   DTF
       .
       . ++++++++++
       ER$WRITE    SYALLOC   B1                  . GET A WORK AREA
                   RI,W      TCB$ER$P,B5         . BUMP PAST PARAM
                   LA,W      0,B7,ANOT           . GET PTR TO DTF
                   J         ERR$RTRN            . ZERO, HMMM!
                   TOABS                         . CVT TO ABSOLUTE
                   LB,A      B6
                   LQ        DTF$TYPE            . GET FILE TYPE
                   LLP,L     DTF$FLAGS,B6
                   LB,A      B3
                   J,L       ERW$JUMP,B3         . GO TO PROPER ROUTINE
       .
       ERW$JUMP    +ERR$RTRN
                   +ER$WRITESD
                   +ERR$RTRN                     . CARD READER                   
                   +ER$WRITECP
                   +ER$WRITEPR
                   +ERR$RTRN                     . UNUSED
                   +ERR$RTRN                     . UNUSED
                   +ERR$RTRN                     . UNUSED
       .
       . ++++++++++
       . ER$WRITESD
       .
       . WRITE A SEQUENTIAL DRUM FILE
       .
       . REGISTERS:
       .   B1 = WORK AREA
       .   B5 = I/O BFR
       .   B6 = DTF
       .   B7 = RECORD BFR
       . ++++++++++
       ER$WRITESD  LA,U      DTF$POSN,B6,AZERO   . GET CRNT BFR OFFSET
                   J         ERW$CONT1           . NOT ZERO, CONTINUE
                   SLJ       ERR$IOCALC          . ZERO NEXT TRACK #
                   SZ,W      0,B5
                   LA        1                   . SET BFR OFFSET TO 1
                   SA,U      DTF$POSN,B6
                   LA,L      DTF$TRACKS,B6       . SET 1ST TRACK #
                   SA,L      DTF$POSN,B6
       .                   
       ERW$CONT1   SB,W      B7,ERR$B7,B1        . SAVE B7
       .
                   SLJ       ERR$IOCALC          . CALC BFR POINTERS
                   SLJ       ERR$RCALC
       ERW$LOOP    LA,W      0,B7                . COPY 1 WORD
                   SA,W      0,B5
                   SA,W      ERR$LSTWRD,B1       . SAVE FOR LATER
                   TBI,W     B5,ERR$IOEND,B1     . BUMP I/O BFR PTR
                   J         ERW$TSTVAR          . OK
                   J         ERW$WTRK            . END, WRITE TRACK
       ERW$TSTVAR  LQ        DTF$VARREC          . VARIABLE LENGTH RECORDS?
                   LLP,L     DTF$FLAGS,B6,ANOT
                   J         ERW$BMPREC          . NO
                   LA,W      ERR$LSTWRD,B1       . LAST WORD = 7777777777?
                   NA        0,,ANOT
                   J         ERW$DONE            . YES
       ERW$BMPREC  TBI,W     B7,ERR$RECEND,B1    . BUMP REC PTR
                   J         ERW$LOOP
       .
       ERW$DONE    SB,A      B5                  . CALC CRNT BFR OFFSET
                   AN,W      ERR$IOBFR,B1
                   SA,U      DTF$POSN,B6         . SAVE IT
       .
                   LB,W      B5,LAST$TASK        . RESTORE REGS
                   LB,W      B7,ERR$B7,B1
                   J         ERR$RTRN                                                         
       .       
       ERW$WTRK    LA,L      DTF$CHAN,B6         . ALLOCATE A TRACK
                   SA,W      ERW$P1
                   SLJ       MFD$ALLOC
       ERW$P1      +0
                   LA,W      ERW$P1,,APOS        . GET THE TRACK #
                   J         ERW$DFULL           . OOPS!
                   LB,W      B5,ERR$IOBFR,B1     . SET NEXT TRACK #
                   SA,W      0,B5
                   SA,W      ERR$TEMP,B1         . SAVE IT FOR LATER
                   LA,L      DTF$POSN,B6         . GET CRNT TRACK #
                   SA,W      ERW$P5
                   LA,W      ERR$TEMP,B1         . SET NEW FILE TRACK
                   SA,L      DTF$POSN,B6
                   SLJ       ERW$WRTRACK         . WRITE IT
       ERW$P5      +0
                   RI,U      DTF$TRACKS,B6       . BUMP THE TRACK COUNT
                   J         ERW$TSTVAR
       .
       . WRITE 1 TRACK TO THE FILE
       .                   
       ERW$WRTRACK +0
                   LA,L      ERW$WRTRACK         . GET RETURN ADDR
                   LB,A      B2
                   A         1                   . BUMP PAST PARAM
                   SA,W      ERR$RTADR,B1        . SAVE IT FOR LATER
       .
                   LA,W      ERR$IOBFR,B1        . SET UP IOREQ FOR WRITE
                   SA,W      IR$BFR,B1
                   LA        1024D
                   SA,W      IR$COUNT,B1
                   LA,W      0,B2                . GET THE TRACK #
                   LSA       10D                 . MULT X 1024
                   SA,W      ERR$TEMP,B1
                   LA        FH8$WRITE           . COMBINE COMMAND WITH ADDR
                   LSA       24D
                   OR,W      ERR$TEMP,B1
                   SA,W      IR$CMD,B1
                   SZ,W      IR$FLAGS,B1
                   LA,W      DTF$QUEUE,B6        . QUEUE THE REQUEST
                   SA,W      ERW$P2
                   SB,W      B1,ERW$P3
                   SB,W      B1,ERW$P4
                   SB,W      B6,ERR$TEMP,B1
                   LBPJB6    L$INSERT
       ERW$P2      +0
       ERW$P3      +0
                   +0
                   LB,W      B6,ERR$TEMP,B1
                   SLJ       WAIT
       ERW$P4      +0
                   +TCB$WFIN
                   LA,W      IR$STAT,B1,AZERO    . CHECK THE STATUS
                   J         ERW$WRERR           . OOPS!
                   LA        1                   . SET BFR OFFSET TO 1
                   SA,U      DTF$POSN,B6
       .           
                   LB,W      B2,ERR$RTADR,B1        
                   J         0,B2                                                         
       .
       ERW$WRERR   LA        DTF$EWRERR          . SHOW WRITE ERROR
                   SA,U      DTF$FLAGS,B6
                   J         ERR$ERTRN                   
       .                   
       ERW$DFULL   LA        DTF$EDFULL          . SHOW DRUM FULL
                   SA,U      DTF$FLAGS,B6
                   J         ERR$ERTRN                                      
       . ++++++++++
       . ER$WRITECP
       .
       . WRITE A CARD PUNCH FILE
       .
       . REGISTERS:
       .   B1 = WORK AREA
       .   B6 = DTF
       .   B7 = RECORD BFR 
       . ++++++++++
       ER$WRITECP  LA,L      DTF$RECBFR,B6       . GET PTR TO RECORD BFR
                   TOABS
                   SA,W      IR$BFR,B1
                   LA,U      DTF$RECBFR,B6       . GET BFR SIZE
                   SA,W      IR$COUNT,B1
                   LB        B2,CARD$STKR0       . DEFAULT TO STACKER 0
                   LQ        DTF$STKR1           . STACKER 1?
                   LLP,L     DTF$FLAGS,B6,AZERO
                   LB        B2,CARD$STKR1       . YES
                   SB,A      B2                  . GET INTO A
                   OR        CARD$WINT           . SET INTERRUPT REQUIRED
                   LSA       24D
                   SA,W      IR$CMD,B1
                   SZ,W      IR$FLAGS,B1
                   LA        CARD$Q              . SET PARAMS FOR INSERT
                   SA,W      ERWP$INS+1    
                   SB,W      B1,ERWP$INS+2
                   SB,W      B1,ERWP$WAIT+1
                   SB,W      B6,ERR$TEMP,B1                   
       ERWP$INS    LBPJB6    L$INSERT
                   +0
                   +0
                   +0
                   LB,W      B6,ERR$TEMP,B1        . RESTORE B6
       ERWP$WAIT   SLJ       WAIT
                   +0
                   +TCB$WFIN
                   LA,W      IR$STAT,B1          . CHECK THE STATUS
                   LRSA      24D
                   JT        ERR$RTRN,,AZERO     . ZERO, ODD BUT OK
                   JNE       CARD$OK,ERWP$WRERR  . NOT NORMAL COMPLETION
                   J         ERR$RTRN            . ALL GOOD
       .
       ERWP$WRERR  JNE       CARD$EINTLK,ERW$WRERR . NOT INTERLOCK ERROR?
                   LA        DTF$EINTLK          . SHOW INTERLOCK ERROR IN DTF
                   SA,U      DTF$FLAGS,B6
                   J         ERR$ERTRN
       . ++++++++++
       . ER$WRITEPR
       .
       . WRITE A PRINTER FILE
       .
       . REGISTERS:
       .   B1 = WORK AREA
       .   B6 = DTF
       .   B7 = RECORD BFR 
       . ++++++++++
       ER$WRITEPR  LA,L      DTF$RECBFR,B6       . GET PTR TO RECORD BFR
                   TOABS
                   SA,W      IR$BFR,B1
                   LA,U      DTF$RECBFR,B6       . GET BFR SIZE
                   SA,W      IR$COUNT,B1
                   LA        PRN$PRINT++PRN$WINT . PRINT WITH INTERRUPT
                   LSA       6D
                   OR,U      DTF$SPCB4,B6        . ADD SPACE B4 COUNT
                   LSA       18D
                   SA,W      IR$CMD,B1
                   SZ,W      IR$FLAGS,B1
                   LA        PRN$Q               . SET PARAMS FOR INSERT
                   SA,W      ERWPR$INS+1    
                   SB,W      B1,ERWPR$INS+2
                   SB,W      B1,ERWPR$WAIT+1
                   SB,W      B6,ERR$TEMP,B1                   
       ERWPR$INS   LBPJB6    L$INSERT
                   +0
                   +0
                   +0
                   LB,W      B6,ERR$TEMP,B1        . RESTORE B6
       ERWPR$WAIT  SLJ       WAIT
                   +0
                   +TCB$WFIN
                   LA,W      IR$STAT,B1          . CHECK THE STATUS
                   LRSA      24D
                   JT        ERR$RTRN,,AZERO     . ZERO, ODD BUT OK
                   JNE       PRN$OK,ERWPR$WRERR  . NOT NORMAL COMPLETION
                   J         ERR$RTRN            . ALL GOOD
       .
       ERWPR$WRERR JNE       PRN$EINTLK,ERW$WRERR . NOT INTERLOCK ERROR?
                   LA        DTF$EINTLK          . SHOW INTERLOCK ERROR IN DTF
                   SA,U      DTF$FLAGS,B6
                   J         ERR$ERTRN
       . ++++++++++
       . ER$D2I
       .
       . CONVERT FIELDATA TO INTEGER
       .
       . PARAMETERS:
       .   DOUBLE WORD FIELDATA STRING IN AQ
       .
       . RETURNS:
       .   INTEGER EQUIVALENT IN A
       . ++++++++++
       ER$D2I      DPL       TCB$ER$A,B5
                   SLJ       DEC2INT
                   SA,W      TCB$ER$A,B5
                   J         ER$RTRN
       . ++++++++++
       . ER$I2D
       .
       . CONVERT INTEGER TO FIELDATA
       .
       . PARAMETERS:
       .   INTEGER TO CONVERT IN A
       .
       . RETURNS:
       .   FIELDATA EQUIVALENT IN AQ
       . ++++++++++
       ER$I2D      LA,W      TCB$ER$A,B5
                   LQ,A
                   SLJ       INT2DEC
                   DPS       TCB$ER$A,B5
                   J         ER$RTRN
       . ++++++++++
       . ER$TOKEN
       .
       . RETURN THE NEXT TOKEN FROM A STRING
       .
       . PARAMETERS:
       .   U = CHARACTER IN WORD, L = WORD IN BUFFER
       .   BUFFER
       .
       . RETURNS:
       .   DOUBLE WORD TOKEN IN AQ
       . ++++++++++
       ER$TOKEN    LA,W      TCB$ER$P,B5         . BUMP RTRN ADDR PAST PARAMS
                   A         2
                   SA,W      TCB$ER$P,B5
                   LA,W      0,B7                . GET BFR INDEX
                   SA,W      ERTK$P1
                   LA,W      1,B7                . GET BFR ADDR
                   TOABS
                   SA,W      ERTK$P2
                   SLJ       GETTKN
       ERTK$P1     +0
       ERTK$P2     +0
                   +0
                   LA,W      ERTK$P1             . UPDATE BFR INDEX
                   SA,W      0,B7                
                   DPL       ERTK$P2             . GET TOKEN
                   DPS       TCB$ER$A,B5
                   J         ER$RTRN 
       . ++++++++++
       . ER$DCLOCK
       .
       . RETURN THE VALUE OF THE DAY CLOCK IN A
       . ++++++++++
       ER$DCLOCK   LA,W      16
                   SA,W      TCB$ER$A,B5
                   J         ER$RTRN                                                        
       . ++++++++++
       . ER$RTCLOCK
       .
       . RETURN THE VALUE OF THE REAL TIME CLOCK IN A
       . ++++++++++
       ER$RTCLOCK  LA,W      17
                   SA,W      TCB$ER$A,B5
                   J         ER$RTRN                                                        
       . ++++++++++
       . ER$MEMSZ
       .
       . RETURN THE SIZE OF PROGRAM MEMORY IN A
       . ++++++++++
       ER$MEMSZ    LA,W      TCB$PLEN,B5
                   SA,W      TCB$ER$A,B5
                   J         ER$RTRN                                                        
       . ++++++++++
       . SMALL POOL OF FIXED SIZE MEMORY BLOCKS FOR OPERATING SYSTEM
       . USE. THIS POOL CONTAINS A FIXED NUMBER OF BLOCKS AND WHEN IT
       . IS EXHAUSTED THE OS WILL HALT. ALLOCATED BLOCKS ARE TRACKED
       . USING A BITMAP.
       . ++++++++++
       SY$BLKSIZE  EQU       25D
       SY$NUMBLK   EQU       60D
       SY$BMPSIZE  EQU       SY$NUMBLK//30D            
       SY$BITMAP   RES       SY$BMPSIZE
       SY$POOL     RES       SY$BLKSIZE*SY$NUMBLK
       . ++++++++++
       . MEMORY BEYOND THIS POINT IS FREE TO BE DYNAMICALLY ALLOCATED
       . AS REQURED. DUE TO LIMITATIONS OF THE 494 HARDWARE, THIS FREE
       . AREA MUST BE ALLOCATED ON A 64 WORD BOUNDARY AND ALL BLOCKS MUST
       . BE ALLOCATED IN 64 WORD INCREMENTS. SEE PLR & RIR REGISTER DESCRIPTIONS.
       . ++++++++++
       LOWFREE     EQU       $
       FM$HEAD     FM$CB     ((LOWFREE+FMCB$LEN)//64D*64D)-1,(128D*1024D)-2D
       .
       .
                   END                  