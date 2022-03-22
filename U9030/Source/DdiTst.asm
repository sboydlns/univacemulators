DDITST   START
*
* An attempt to write an ICAM Direct Data Interface (DDI) 
* Communications User Program (CUP).
*
         PRINT GEN
         SUPEQU REGS=YES,TCB=NO,PRE=YES,SIB=NO,IO=NO,TRN=NO
         TQ#X                          ALL ICAM PACKETS
*         
         BALR  R2,0
         USING *,R2
************
* 
* GET INFO FROM THE JOB PREAMBLE SO THAT CAN KNOW HOW MUCH MEMORY
* WE HAVE TO PLAY WITH.
*
************
         GETINF PRE,PREWA,JP$LNGTH,0
************
*
* OPEN THE NETWORK
*
************
         LA    R11,NQEMSG              POINT TO APPROPRIATE ERR MSG
         ST    R11,EMSGADR       
         XR    R0,R0              
         NETREQ EDT,ERRET=NETERR       OPEN THE NETWORK
         LTR   R0,R0                   SUCCESS?
         BNZ   NETERR                  NO
         OPR   NQOK,L'NQOK             YES, TELL OPERATOR
************
*
* GET DEVICE INFO FOR KNOWN TERMINALS
*
************
         LA    R11,CCEMSG              POINT TO APPROPRIATE ERR MSG
         ST    R11,EMSGADR       
         CCACPY TNAMES,TNAMESL,CCAINFO,CCAINFOL
*         CANCEL 1                      TAKE A DUMP
************
*
* SEND 'DDITST READY' MESSAGE TO A TERIMAL. CURRENTLY, THIS SENDS
* ONE MESSAGE AND WAITS FOR THE OUTPUT COMPLETION ROUTINE TO ADJUST
* THE MCTPKT TO THE NEXT TERMINAL ID BEFORE SENDING ANOTHER. I SUSPECT
* THAT, ALTHOUGH THIS WORKS FOR THIS TEST, I WILL NEED A MCTPKT FOR 
* EACH TERMINAL IF THINGS LIKE BUFFER TOGGLING ARE TO WORK PROPERLY.
* THAT WOULD ALSO ALLOW ME TO SEND MESSAGES TO MULTIPLE TERMINALS
* CONCURRENTLY AND LET ICAM SORT OUT THE DELIVERY TIMING.
*
************
         USING TN#MCTDS,R1
SNRDY    LA    R1,MCTO                 COVER OUTPUT MCT PACKET
         LA    R11,SNEMSG              POINT TO APPROPRIATE ERROR MSG
         ST    R11,EMSGADR
         LA    R11,RDYMSG              POINT TO MESSAGE
         ST    R11,TN#MCBFA         
         LA    R11,L'RDYMSG            SET MSG LENGTH
         STH   R11,TN#MCBAL
         MVI   TN#MCCKB,TN#MCEM1++TN#MCLBW SET LAST BUFFER FLAGS
         MCPCALL (1)                   SEND IT
         OPR   CALLDONE,L'CALLDONE     WE SHOULD NEVER COME HERE
         CANCEL 1                      TAKE A DUMP
************
*
* ECHO MOST RECENTLY RECEIVED INPUT BACK TO ORIGINATING TERMINAL
*
************
         USING TN#MCTDS,R1
ECHO     MVI   BFRI,C' '               CLEAR DICE CODE FROM INPUT
         MVC   BFRI+1(3),BFRI
         LA    R1,MCTI                 COVER INPUT MCT PACKET
         LH    R11,TN#MCCHC            GET MSG LENGTH
         LH    R12,TN#MCTIN            GET SRC TERMINAL ID
         LA    R1,MCTO                 COVER OUTPUT MCT PACKET
         STH   R11,TN#MCBAL            SET MSG LENGTH
         STH   R12,TN#MCTIN            SET TERMINAL ID
         LA    R11,BFRI                SET TO SEND FROM INPUT BFR
         ST    R11,TN#MCBFA
         MVI   TN#MCCKB,TN#MCEM1++TN#MCLBW SET LAST BUFFER FLAGS
         MCPCALL (1)                   SEND IT
         OPR   CALLDONE,L'CALLDONE     WE SHOULD NEVER COME HERE
         CANCEL 1                      TAKE A DUMP
************
*
* REQUEST INPUT FROM LINE 1
*
************
         USING TN#MCTDS,R1
INPUT    LA    R1,MCTI                 COVER INPUT MCT PACKET                  
         LA    R11,RCEMSG              POINT TO APPROPRIATE ERROR MSG
         ST    R11,EMSGADR
         LA    R11,BFRI                POINT TO BUFFER
         ST    R11,TN#MCBFA         
         LA    R11,L'BFRI              SET BUFFER LENGTH
         STH   R11,TN#MCBAL
         XC    TN#MCCHC,TN#MCCHC       CLEAR RECV'D CHAR COUNT
         LA    R11,1                   SET TERM. ID TO 1
         STH   R11,TN#MCTIN  
         XC    TN#MCCKB,TN#MCCKB       CLEAR BFR CTRL BYTE          
         MCPCALL (1)                   GET THE INPUT         
         OPR   CALLDONE,L'CALLDONE     WE SHOULD NEVER COME HERE
         CANCEL 3                      TAKE A DUMP
************
*
* RELEASE THE NETWORK
*
************         
         NETREL EDT        
*
         EOJ
***********
*
* NETWORK REQUEST ERROR HANDLER
*   R0 = ERROR CODE
*
***********
NETERR   LR    R11,R0
         LA    R1,EMSGADR
         LA    R0,20
         OPR   (1),(0)
         LR    R0,R11
         CANCEL (0)
***********
*
* INPUT COMPLETION ROUTINE
*   R1 = PTR TO MCTPKT
*
* THIS NEEDS TO BE ABLE TO HANDLE ERRORS, RETRIES, ETC.
*
***********
         USING TN#MCTDS,R1
ICMPL    LR    R11,R1                  SAVE MCTPKT PTR
         OPR   IDONE,L'IDONE           TELL OPR INPUT COMPLETE
         LR    R1,R11                  RESTORE R1
         B     ECHO
***********
*
* OUTPUT COMPLETION ROUTINE
*   R1 = PTR TO MCTPKT
*
* THIS NEEDS TO BE ABLE TO HANDLE ERRORS, RETRIES, ETC.
*
***********
         USING TN#MCTDS,R1
OCMPL    NOP   RDYDONE                 
         LR    R11,R1                  SAVE MCTPKT PTR
         OPR   ODONE,L'ODONE           TELL OPR OUTPUT COMPLETE
         LR    R1,R11                  RESTORE R1
         LH    R11,TN#MCTIN            GET LAST TERIMINAL ID
         LA    R11,1(R11)              BUMP BY 1
         STH   R11,TN#MCTIN            UPDATE MCTPKT
         CH    R11,MAXTERMS            > MAX. CONFIGURED TERMINALS?
         BNH   SNRDY                   NO, SEND RDY MSG TO NEXT TERM.
*         
         MVI   OCMPL+1,X'FF'           MAKE NOP A BRANCH
RDYDONE  B     INPUT
************
*
* CONFIGURATION VALES
*
************
MAXTERMS DC    H'2'    
************
EMSGADR  DS    A
NQOK     DC    C'NETREQ OK'
NQEMSG   DC    CL20'NETWORK ERROR'
CCEMSG   DC    CL20'CCACPY ERROR'
SNEMSG   DC    CL20'SEND ERROR'
RCEMSG   DC    CL20'RECEIVE ERROR'
CALLDONE DC    C'BACK FROM MCPCALL'
ODONE    DC    C'OUTPUT COMPLETE'
IDONE    DC    C'INPUT COMPLETE'
RDYMSG   DC    C'DDITST READY'
         DS    0F
* LIST OF KNOWN TERMINALS PASSED TO CCACPY TO GET DVC INFO.         
TNAMES   DC    CL4'T001'
         DC    CL4'T002'
         DC    F'-1' 
TNAMESL  EQU   *-TNAMES         
CCAINFO  DS    25F                     ENOUGH ROOM FOR 4 TERMINALS
CCAINFOL EQU   *-CCAINFO
* MCT PACKET FOR OUTPUT
MCTO     MCTPKT OCMPL,SEND,                                            X
               BUFFERA=BFRO,                                           X
               BAL=L'BFRO,                                             X
               ID=(1,1),                                               X
               ENDBUF=1
* MCT PACKET FOR INPUT
MCTI     MCTPKT ICMPL,RECEIVE,                                         X
               BUFFERA=BFRI,                                           X
               BAL=L'BFRI,                                             X
               ID=(1,1),                                               X
               ENDBUF=1
* WORK AREA FOR JOB PREAMBLE
         DS    0D
PREWA    DS    (JP$LNGTH)XL1
BFRI     DS    CL2048                              
BFRO     EQU   BFRI
*
         END 
// FIN                 