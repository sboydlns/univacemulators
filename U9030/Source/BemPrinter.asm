         TITLE 'PRINTER SUPPORT FOR BOYD''S BEM SIMULATOR'
BEMPRNTR START
***********************************************************************
*                                                                     *
*               PRINTER SUPPORT FOR BEM SIMULATOR                     *
*                                                                     *
***********************************************************************
         PRINT NOGEN
         SUPEQU REGS=YES
         SA$DSECT
         BEMDSCTS
*
         USING LOWMEM,R0        
         USING SA$DSECT,R13
         ENTRY P$OPEN,P$BRKPT,P$CLOSE,P$PUT
************
*
* OPEN A NEW PRINTER FILE.
*   R2  = COVER
*   R14 = RETURN ADDRESS
*   R15 = ENTRY PT.
*
************
P$OPEN   SAVE  (14,12),COVER=2
         BAL   R14,P$LOCK              ACQUIRE THE SINGLE THREAD LOCK
         CLI   POPEN,X'FF'             IS IT ALREADY OPEN?
         BE    PO$DONE                 YES, WE'RE DONE
         OPEN  PRNTR                   OPEN THE PRINTER
         MVI   POPEN,X'FF'             SHOW FILE OPEN
PO$DONE  XC    SA$R0,SA$R0             CLEAR ERROR CODE
         RETURN (14,12)                RETURN TO CALLER
************
*
* ACQUIRE THE SINGLE THREAD LOCK
*
************
P$LOCK   TS    PLOCK                   TRY TO ACQUIRE THE LOCK
         BZ    PL$OK                   WE GOT IT
         SETIME 10,WAIT,M              BUSY, WAIT A BIT
         B     P$LOCK                  & LOOP
PL$OK    BR    R14                     RETURN TO CALLER
         DROP  R2
************
*
* BREAKPOINT THE PRINT FILE
*   R2  = COVER
*   R14 = RETURN ADDRESS
*   R15 = ENTRY PT.
*
************
P$BRKPT  SAVE  (14,12),COVER=2
         CLI   PLOCK,X'FF'             IS PRINTER OURS?
         BNE   PC$SKIP                 NO, SKIP REQUEST
         BRKPT PRNTR                   BREAKPOINT THE FILE
         MVI   PLOCK,X'00'             RELEASE THE LOCK
PC$SKIP  RETURN (14,12)
         DROP  R2
************
*
* CLOSE THE PRINTER. ONLY CALLED AT EOJ
*   R2  = COVER
*   R14 = RETURN ADDRESS
*   R15 = ENTRY PT.
*
************
P$CLOSE  SAVE  (14,12),COVER=2
         CLOSE PRNTR
         MVI   POPEN,X'00'
         MVI   PLOCK,X'00'
         RETURN (14,12) 
         DROP  2 
************
*
* WRITE A LINE TO THE PRINTER FILE
*   R0  = ADDRESS OF PRINT LINE BUFFER (132 CHARS). RETURNS ERROR
*         CODE.
*   R2  = COVER
*   R14 = RETURN ADDRESS
*   R15 = ENTRY PT.
*
************
P$PUT    SAVE  (14,12),COVER=2
         CLI   PLOCK,X'FF'             IS PRINTER OURS?
         BE    PP$OK                   YES, CONTINUE
         LA    R0,X'02'                SET 'INVALID MACRO SEQ' ERROR
         ST    R0,SA$R0
         RETURN (14,12)                & RETURN TO CALLER
*
PP$OK    ST    R13,PP$SAVE+4           SAVE SA BACK LINK
         LA    R13,PP$SAVE             POINT TO OUR SAVE AREA
         PUT   PRNTR,(0)               WRITE THE PRINT LINE
         L     R13,SA$BLNK             RESTORE R13
         XC    SA$R0,SA$R0             CLEAR ERROR CODE
         RETURN (14,12)                & RETURN TO CALLER         
*
PP$SAVE  DS    18F         
         DROP  R2                
************
*
* ERROR HANDLER
*   R2  = COVER
*
************
PRTERR   BALR  R2,0
         USING *,R2
         CLC   SA$BLNK(4),LM$ZERO      SA BACK LINE = ZERO?
         BE    PE$OK                   YES, CONTINUE
         L     R13,SA$BLNK             RESTORE R13 FROM BACK LINK
PE$OK    XR    R0,R0                   ERROR CODE TO R0
         IC    R0,PRNTRC 
         ST    R0,SA$R0                RETURN ERROR CODE
         RETURN (14,12)                RETURN TO CALLER
         DROP  R2
************
PLOCK    DC    X'00'
POPEN    DC    X'00'        
         DS    0H
PRTBFR   DS    CL132
*
PRNTR    DTFPR BLKSIZE=132,                                            X
               ERROR=PRTERR,                                           X
               IOAREA1=PRTBFR,                                         X
               PRAD=1,                                                 X
               PRINTOV=SKIP,                                           X
               WORKA=YES
*                        
         END
// FIN        