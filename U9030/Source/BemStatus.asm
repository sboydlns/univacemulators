         TITLE '/STATUS COMMAND FOR BOYD''S BEM SIMULATOR'
BEMSTA   START
***********************************************************************
*                                                                     *
* AN IMPLEMENTATION OF THE /STATUS COMMAND FOR BOYD'S BEM SIMULATOR.  *
*                                                                     *
***********************************************************************
*
         PRINT NOGEN
         ZM#DPIB
         ZM#DIMH
ZA#IMSG  DS    CL2048         
BEMSTA   CSECT         
         RGEQU
         BEMDSCTS
*         
         STM   R14,R12,12(R13)
************
* R2  = COVER
* R3  = PIB
* R4  = IMA
* R5  = WORK AREA
* R6  = OMA
* R7  = CDA
* R9  = TERMTBL
* R14 = RETURN ADDRESS
************         
         BALR  R2,0                    SET COVER
         USING *,R2
         USING LOWMEM,R0
         USING ZA#DPIB,R3
         USING ZA#IMH,R4
         USING WORKAREA,R5
         USING ZA#OMH,R6
         USING TERMTBL,R9
*         
         L     R3,0(R1)                COVER THE PARAMETERS
         L     R4,4(R1)
         L     R5,8(R1)
         L     R6,12(R1)
         L     R7,16(R1)
*
* DECODE THE INPUT MESSAGE
*         
         LH    R0,ZA#ITL               SET UP GETOKEN PARAMS
         LA    R1,ZA#IMSG
         L     R15,LM$GTKN
         LA    R11,STYPE               BURN '/STATUS'
         BALR  R14,R15
         LA    R11,STYPE               GET TYPE OF STATUS
         BALR  R14,R15
*
* CHECK STATUS TYPE AND BRANCH TO APPROPRIATE ROUTINE
*
         CLI   STYPE,C'R'              RESOURCE?
         BE    SENDRES                 YES
         CLI   STYPE,C'T'              TERMINALS?
         BE    SENDTERM                YES
*
* SEND CURRENT USER STATUS
*
         MVC   ZA#OMSG(USERMSGL),USERMSG SET UP OMA WITH MSG TEMPLATE
         LA    R11,USERMSGL
         STH   R11,ZA#OTL         
         MVI   ZA#PSIND,ZA#PSNN        SET NORMAL TERMINATION
* 
         USING USERTBL,R10        
         L     R1,ZA#ISTID             FIND TERM TABLE FOR INPUT TERM
         L     R15,LM$FTNAM
         BALR  R14,R15
         LTR   R9,R1                   TERM TABLE FOUND?
         BZ    SU1                     NO, WTF?
         MVC   OUTERM,TT$NAME          COPY TERM NAME TO OMA
         L     R10,TT$USER             GET PTR TO LOGGED ON USER TBL
         LTR   R10,R10                 ZERO?
         BZ    SU1                     YES, WTF?
         MVC   OUUSER,UT$ID            COPY USER NAME TO OMA
         MVC   CTEMP,TMMASK            FORMAT LOGGED ON TIME
         ED    CTEMP,TT$LGNTM
         MVC   OULGNTM,CTEMP+2         COPY HH:MM TO OMA
SU1      GETIME S                      GET CURRENT DATE/TIME
         ST    R0,DTEMP                FORMAT DATE         
         MVC   CTEMP,DTMASK
         ED    CTEMP,DTEMP
         MVC   OUDATE,CTEMP+2          MOVE DATE TO OMA
         ST    R1,DTEMP                FORMAT TIME
         MVC   CTEMP,TMMASK
         ED    CTEMP,DTEMP
         MVC   OUTIME,CTEMP+2
         B     DONE         
         DROP  R10
*
* SEND RESOURCE STATUS
*
SENDRES  EQU   *
*
* COUNT # OF LOGGED ON TERMINALS
*
         XR    R12,R12                 CLEAR TERMINAL COUNT
         L     R9,LM$TFRST             POINT TO 1ST TERM TABLE
SRLOOP   LTR   R9,R9                   END OF TABLE?
         BZ    SRCONT                  YES, CONTINUE       
         TM    TT$FLAGS,TT$LOGON       TERM LOGGED ON?
         BZ    SRNEXT                  NO, TRY NEXT TERM.
         LA    R12,1(R12)              YES, BUMP TERMINAL COUNT
SRNEXT   L     R9,TT$NEXT              GET PTR TO NEXT TERM TABLE
         B     SRLOOP                  & LOOP         
*
SRCONT   MVC   ZA#OMSG(RESMSGL),RESMSG SET UP OMA WITH MSG TEMPLATE
         LA    R11,RESMSGL
         STH   R11,ZA#OTL
         MVI   ZA#PSIND,ZA#PSNN        SET NORMAL TERMINATION
*
         CVD   R12,DTEMP               CVT # TERMS TO PACKED
         ED    OTERMS,DTEMP+6          UNPACK TO OMA         
         L     R11,LM$FEND             GET TTL MEMORY FOR JOB
         CVD   R11,DTEMP               CVT TO PACKED
         ED    OMAXMEM,DTEMP+4         UNPACK TO OMA
         S     R11,LM$FSTRT            CALC. FREE MEM LEFT
         CVD   R11,DTEMP               CVT TO PACKED
         ED    OFREEMEM,DTEMP+4        UNPACK TO OMA
         L     R11,LM$FTTL             GET TTOL FREE MEMORY AT BEM STRT
         CVD   R11,DTEMP               CVT TO PACKED
         ED    OAVALMEM,DTEMP+4        UNPACK TO MOA
         B     DONE                    
* 
* SEND TERMINALS STATUS
*
SENDTERM MVC   ZA#OMSG(TERMHDRL),TERMHDR MOVE MSG HDR TO OMA
         LA    R12,TERMHDRL            GET OFFSET TO NEXT LINE IN OMA 
         L     R9,LM$TFRST             POINT TO 1ST TERM TABLE
STLOOP   LTR   R9,R9                   END OF TABLE?
         BZ    STDONE                  YES
         CH    R12,=H'1920'            END OF MSG
         BH    STDONE                  YES, QUIT
         TM    TT$FLAGS,TT$LOGON       TERMINAL LOGGED ON?
         BZ    STNEXT                  NO, TRY NEXT TERMINAL
         LA    R11,ZA#OMSG(R12)        MOVE LINE TEMPLATE TO OMA
         MVC   0(TERMLNEL,R11),TERMLNE
         MVC   TTERM(4,R11),TT$NAME          MOVE TERMINAL NAME TO OMA
         USING PGMTBL,R10
         L     R10,TT$LSTPG                 PROGRAM NAME
         LTR   R10,R10
         BZ    STNOP                   
         MVC   TCMD(7,R11),PT$CODE+1                   
         USING USERTBL,R10
STNOP    L     R10,TT$USER                  USER NAME
         LTR   R10,R10
         BZ    STNOU
         MVC   TUSER(4,R11),UT$ID
STNOU    LA    R12,TERMLNEL(R12)       BUMP OMA OFFSET
STNEXT   L     R9,TT$NEXT              GET PTR TO NEXT TERM TBL
         B     STLOOP                  & LOOP
         
STDONE   LA    R11,ZA#OMSG(R12)        MOVE TERM TRAILER TO OMA
         MVC   0(TERMTLRL,R11),TERMTLR
         LA    R12,TERMTLRL(R12)       BUMP MSG LENGTH
         STH   R12,ZA#OTL              SAVE TTL MSG LENGTH
         MVI   ZA#PSIND,ZA#PSNN        SET NORMAL TERMINATION
         B     DONE                  
*                
DONE     LM    R14,R12,12(R13)         RESTORE REGISTERS
         BR    R14                     RETURN TO MONITOR
*
* RESOURCE STATUS TEMPLATE
*
RESMSG   ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'27'                 ERASE PROTECTED
         DC    CL1'M'
         DC    XL1'1E'                 SOE
         DC    C'TASKS TERMS ----------MEMORY----------'
         ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'1E'                 SOE
         DC    C'                 MAX   AVAIL    FREE'
         ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'1E'                 SOE
         DC    C'    1 '
TERMS    EQU   *-RESMSG         
         DC    XL4'40202021'         
         DC    C'  '
MAXMEM   EQU   *-RESMSG         
         DC    XL8'4020202020202021'
AVAILMEM EQU   *-RESMSG         
         DC    XL8'4020202020202021'
FREEMEM  EQU   *-RESMSG         
         DC    XL8'4020202020202021'
         ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'1E'                 SOE
RESMSGL  EQU   *-RESMSG
*
* TERMINAL STATUS TEMPLATES
*
TERMHDR  ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'27'                 ERASE PROTECTED
         DC    CL1'M'
         DC    XL1'1E'                 SOE
         DC    C'TERMINAL   COMMAND   SCRATCH SPACE   USER'
TERMHDRL EQU   *-TERMHDR         
*
TERMLNE  ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'1E'                 SOE
         DC    C'  '
TTERM    EQU   *-TERMLNE         
         DC    C'    '
         DC    C'     '
TCMD     EQU   *-TERMLNE                           
         DC    C'                          '
TUSER    EQU   *-TERMLNE         
         DC    C'    '
TERMLNEL EQU   *-TERMLNE                                  
*  
TERMTLR  ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'1E'                 SOE
TERMTLRL EQU   *-TERMTLR
*
* CURRENT USER STATUS TEMPLATE
*
USERMSG  ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'27'                 ERASE PROTECTED
         DC    CL1'M'
         DC    XL1'1E'                 SOE
         DC    C'TERMINAL   USER   LOGON     DATE     CUR-TIME'
         ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'1E'                 SOE
         DC    C'  '
UTERM    EQU   *-USERMSG         
         DC    C'    '         
         DC    C'     '
UUSER    EQU   *-USERMSG
         DC    C'    '
         DC    C'   '
ULGNTM   EQU   *-USERMSG
         DC    C'     '
         DC    C'   '
UDATE    EQU   *-USERMSG
         DC    C'        '
         DC    C'   '
UTIME    EQU   *-USERMSG
         DC    C'        '                                                               
         ZO#COORD 1,1                  CURSOR HOME
         DC    XL1'27'                 DELETE LINE
         DC    CL1'k'
         ZO#COORD 1,23                 LINE 23 COLUMN 1
         DC    XL1'1E'                 SOE
USERMSGL EQU   *-USERMSG         
* 
TMMASK   DC    C' '
         DC    X'202120'
         DC    C':'
         DC    X'2020'
         DC    C':'
         DC    X'2020'
DTMASK   DC    C' '
         DC    X'202120'
         DC    C'/'
         DC    X'2020'
         DC    C'/'
         DC    X'2020'
         ZM#DOMH
ZA#OMSG  DS    CL2048                  OUTPUT MSG BFR.         
OTERMS   EQU   ZA#OMSG+TERMS,4
OMAXMEM  EQU   ZA#OMSG+MAXMEM,8
OAVALMEM EQU   ZA#OMSG+AVAILMEM,8
OFREEMEM EQU   ZA#OMSG+FREEMEM,8
OUTERM   EQU   ZA#OMSG+UTERM,4
OUUSER   EQU   ZA#OMSG+UUSER,4
OULGNTM  EQU   ZA#OMSG+ULGNTM,5
OUDATE   EQU   ZA#OMSG+UDATE,8
OUTIME   EQU   ZA#OMSG+UTIME,8
*
WORKAREA DSECT
STYPE    DS    CL8
DTEMP    DS    D
CTEMP    DS    CL10
*
BEMSTA   CSECT
*
         END
// FIN