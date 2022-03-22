// JOB LIBS
// DVC 20 // LFD PRNTR
// DVCVOL LNS001
// LBL LNSMAC // LFD D0
// EXEC LIBS
/$
         FIL   D0=D0
         DEL   D0,P,BEMDSCTS
         ELE   D0,P,BEMDSCTS
         PROC  &P,1
BEMDSCTS NAME  
************
* 
* LOW MEMORY DESCRIPTION 
*
************
LOWMEM   DSECT
LM$TEST  DS    16XL1                   LOW MEMORY TEST PATTERN
LM$ZERO  EQU   LM$TEST                 WORD OF ZEROS
LM$FTTL  DS    F                       TTL FREE MEMORY AT BEM START
LM$FSTRT DS    A                       FREE MEMORY START
LM$FEND  DS    A                       FREE MEMORY END 
LM$TCNT  DS    H                       # TERMINALS
         DS    H                       UNUSED
LM$TFRST DS    A                       FIRST TERM. TABLE
LM$TLAST DS    A                       LAST TERM. TABLE
LM$TLSVC DS    A                       LAST TERM. SERVICED
LM$PFRST DS    A                       FIRST PGM. TABLE
LM$PLAST DS    A                       LAST PGM. TBLE 
LM$PMAX  DS    F                       MAX. PGM. SIZE
LM$UFRST DS    A                       FIRST USER TABLE
LM$ULAST DS    A                       LAST USER TABLE 
LM$AFRST DS    A                       FIRST ALIAS TABLE
LM$ALAST DS    A                       LAST ALIAS TABLE
LM$MAIN  DS    A                       MAIN PGM COVER
LM$CLRM  DS    A                       CLRMEM ENTRY POINT
LM$CPYM  DS    A                       CPYMEM ENTRY POINT
LM$GTKN  DS    A                       GETOKEN ENTRY POINT
LM$FTNAM DS    A                       FIND TERM. NAME ENTRY POINT
LM$IBFR  DS    A                       ICAM INPUT BUFFER
LM$OBFR  DS    A                       ICAM OUTPUT BUFFER
LM$LOPN  DS    A                       LIBRARY OPEN RTN (L$OPEN)
LM$LCLS  DS    A                       LIBRARY CLOSE RTN (L$CLOSE)
LM$LDFST DS    A                       LIBRARY DIR 1ST RTN (L$DFIRST)
LM$LDNXT DS    A                       LIBRARY DIR NEXT RTN (L$DNEXT)
LM$LFIND DS    A                       LIBRARY FIND ELEMENT (L$DFIND)
LM$LMHDR DS    A                       LIBRARY GET MODULE HDR (L$EHDR)
LM$LEFST DS    A                       LIBRARY 1ST ELE REC (L$EFIRST)
LM$LENXT DS    A                       LIBRARY 1ST ELE REC (L$ENEXT)
LM$POPEN DS    A                       OPEN A PRINTER FILE (P$OPEN)
LM$PCLSE DS    A                       CLOSE A PRINTER FILE (P$BRKPT)
LM$PWRIT DS    A                       WRITE A PRINT LINE (P$PUT)
LM$LDEL  DS    A                       LIBRARY DELETE AN ELE (L$DELETE)
LM$SAVE  DS    18F                     REGISTER SAVE AREA      
LM$LNGTH EQU   *-LOWMEM
************
*
* TERMINAL TABLE DESCRIPTION
*
************
TERMTBL  DSECT
TT$NEXT  DS    A                       PTR. TO NEXT ENTRY
TT$NAME  DS    CL4                     TERM. NAME
TT$LINE  DS    H                       LOGICAL LINE #
TT$ID    DS    H                       LOGICAL TERM. #
TT$IDF   EQU   TT$LINE,4               FULL WORD LINE # / TERM #
TT$TNUM  DS    H                       TERM. # (REC # IN BFR FILE)
TT$FLAGS DS    XL1                     FLAG BYTE
TT$WAITO EQU   X'80'                   WAIT FOR OUTPUT COMPLETION
TT$WAITI EQU   X'40'                   WAIT FOR INPUT
TT$WAIT  EQU   TT$WAITO++TT$WAITI      
TT$IPEND EQU   X'20'                   INPUT PENDING
TT$LOGON EQU   X'10'                   TERMINAL LOGGED ON
TT$ODELV EQU   X'08'                   OUTPUT DELV. NOTIFICATION PEND.
TT$DLVCD DS    XL1                     CONTINUOUS OUTPUT DELIVERY CODE
TT$NXTPG DS    A                       PTR. TO NEXT PGM TO BE SCHEDULED
TT$USER  DS    A                       PTR. TO LOGGED ON USER TABLE 
TT$LSTPG DS    A                       PTR. TO LAST PGM EXECUTED 
TT$LGNDT DS    F                       LOGON DATE
TT$LGNTM DS    F                       LOGON TIME 
TT$CONT  DS    F                       CONTINUOUS OUTPUT CODE      
TT$LNGTH EQU   *-TERMTBL
************
*
* PROGRAM TABLE DEFINTION
*
************
PGMTBL   DSECT
PT$NEXT  DS    A                       LINK TO NEXT PROG TBL ENTRY
PT$CODE  DS    CL8                     COMMAND CODE
PT$MNAME DS    CL8                     EXECUTABLE MODULE NAME
PT$SIZE  DS    F                       LOAD MODULE SIZE
PT$FLAGS DS    XL1                     FLAG BYTE
PT$DISBL EQU   X'80'                   DISABLED
PT$LOGON EQU   X'40'                   LOGON REQUIRED
PT$NTRY  DS    XL3                     ENTRY PT. ADDRESS
PT$NTRYA EQU   PT$FLAGS,4              FULL WORD ENTRY PT. ADDRESS
PT$LNGTH EQU   *-PGMTBL
************
*
* PROGRAM ALIAS TABLE DEFINITION
*
************
ALIASTBL DSECT
AT$ALIAS DS     CL8                    ALIAS
AT$PGM   DS     A                      PTR. TO PROGRAM TABLE
AT$LNGTH EQU    *-ALIASTBL             TABLE ENTRY LENGTH
************
*
* PROGRAM PHASE HEADER
*
************
PHASEHDR DSECT
         DS    H
PH$NUM   DS    XL1                     PHASE NUMBER
PH$FLGS  DS    XL2                     FLAGS
PH$LADDR DS    XL4                     LOAD ADDRESS
PH$PLNTH DS    XL4                     PHASE LENGTH
PH$NAME  DS    CL8                     PHASE NAME
PH$DATE  DS    XL3                     DATE
PH$TIME  DS    XL3                     TIME
PH$MLNTH DS    XL4                     MODULE LENGTH
PH$ANAME DS    CL8                     ALIAS NAME
PH$CMNTS DS    CL30                    COMMENTS 
************
*
* USER SECURITY TABLE
*
************
USERTBL  DSECT
UT$NEXT  DS    A                       LINK TO NEXT USER TABLE ENTRY
UT$ID    DS    CL4                     USER ID
UT$ACCT  DS    CL4                     ACCOUNT ID
UT$PWD   DS    CL4                     PASSWORD
UT$LNGTH EQU   *-USERTBL
************
*
* SAT PARTITION (PCA) DEFINITION
*
************
PCA      DSECT
PC$PCAID DS    A                        CURRENT PCA RELATIVE ADDRESS    
PC$PMBA  DS    A                        MAX PCA BLOCK ADDRESS           
PC$BPT   DS    A                        BLOCKS PER TRACK                
PC$PID   DS    CL1                      PCA ID                          
PC$EODID DS    AL3                      END OF DATA ID
PC$IOCNT DS    XL1                      I/O COUNT
PC$A1    DS    XL3                      IOAREA1 ADDRESS                 
PC$A1F   EQU   PC$IOCNT,4               FULL WORK I/O AREA1 ADDRESS
PC$PBKS  DS    H                        PCA RELATIVE BLOCK SIZE         
         DS    CL1                      RESERVED                        
PC$PSPB  DS    CL1                      SECTORS PER BLOCK               
PC$LACE  DS    H                        INTERLACE FACTOR                
PD$KLE   EQU   PC$LACE                  KEY LENGTH                      
PC$UOS   DS    H                        UNIT OF STORE                   
PC$LADJ  DS    A                        INTERLACE ADJUSTMENT FACTOR     
PC$PFG1  DS    CL1                      PCA FLAG BYTE                   
*                BIT 0  FORMAT WRITE                                    
*                BIT 1  INTERLACED FILE                                 
*                BIT 2  SEQUENTIAL = YES                                
*                BIT 3  WRITE VERIFY                                    
*                BIT 4  VERIFY REQUIRED/INITIAL ALLOCATION              
*                BIT 5  NO EXTENSION PERMITTED                          
*                BIT 6  INTERLACE ADJUST/KEYED DATA                     
*                BIT 7  SIZE SPECIFIED IN TRACKS                        
PC$EOD   DS    AL3                      END OF DATA ADDRESS             
PS$EOFA  EQU   PC$PFG1,4 
************
*
* LIBRARY FILE PARAMETER PACKET
*
************
LIBFIL   DSECT
LF$NAME  DS    CL8                     FILE NAME AS GIVEN ON // LFD
LF$ELE   DS    CL8                     ELEMENT NAME
LF$ETYP  DS    XL1                     ELEMENT TYPE
         DS    XL3                     UNUSED
LF$DIRP  DS    A                       PTR TO ELEMENT DIR. ENTRY         
LF$DBFR  DS    A                       DIRECTORY PARTITION I/O BFR
LF$EBFR  DS    A                       DATA (ELEMENT) PARTITION I/O BFR
LF$DBLK  DS    F                       BLOCK # OF CURRENT DIR. BLOCK
LF$DOFST DS    H                       OFFSET TO CURRENT DIR. ENTRY
LF$DLEN  DS    H                       LENGTH OF CURRENT DIR. BLOCK
LF$EBLK  DS    F                       BLOCK # OF CURRENT ELE. BLOCK
LF$EOFST DS    H                       OFFSET TO CURRENT ELE. ENTRY
LF$ELEN  DS    H                       LENGTH OF CURRENT ELE. BLOCK
LF$SBFR  DS    A                       PTR TO 256 CHAR SRC RECORD BFR.
LF$LNGTH EQU   *-LIBFIL
*
************
*
* LIBRARY BLOCK HEADER DEFINITIONS
*
************
L$BLKHDR DSECT
LBH$NUM  DS    XL3
LBH$LEN  DS    XL1
LBH$CHK  DS    XL1
LBH$SIZE EQU   *-L$BLKHDR
************
*
* LIBRARY DIRECTORY ENTRY
*
************
L$DIRENT DSECT
LDE$NAME DS    CL8                     MODULE NAME
LDE$TYPE DS    XL1                     MODULE TYPE
LDE$BLK  DS    XL3                     DATA PARTITION BLOCK #
LDE$REC  DS    XL1                     OFFSET WITHIN BLOCK
LDE$BKRC EQU   LDE$BLK,4               BLOCK & OFFSET COMBINED
LDE$SIZE EQU   *-L$DIRENT
************
*
* LIBRARY MODULE HEADER 
*
************
L$MODHDR DSECT
LMH$LEN  DS    XL1
LMH$TYPE DS    XL1
************
*
* LIBRARY SOURCE / PROC MODULE HEADER
*
************
L$SRCHDR DSECT
LSH$LEN  DS    XL1                     RECORD LENGTH
LSH$TYPE DS    XL1                     RECORD TYPE (A3 OR A4)
         DS    XL1                     UNUSED
LSH$FLGS DS    XL2
         DS    XL9                     UNUSED
LSH$NAME DS    CL8                     MODULE NAME
LSH$DATE DS    XL3                     DATE
LSH$TIME DS    XL2                     TIME
         DS    XL1                     UNUSED
LSH$CMNT DS    XL30                    COMMENTS                           
LSH$SIZE EQU   *-L$SRCHDR
************
*
* LIBRARY ELEMENT TYPES
*
************
LET$NULL EQU   X'00'
LET$NTRY EQU   X'04'
LET$CSCT EQU   X'08'
LET$OBJ  EQU   X'80'
LET$PHSE EQU   X'90'
LET$BGRP EQU   X'A0'
LET$EOF  EQU   X'A1'
LET$PNME EQU   X'A2'
LET$PROC EQU   X'A3'
LET$SRC  EQU   X'A4'
LET$EGRP EQU   X'A8'
LET$BLK  EQU   X'B0'
*
************
*
* SCRATCH FILE DSECTS
*
************
*
************
*
* THE BITMAP BLOCK. A 1 BIT INDICATES AN ALLOCATED BLOCK.
*
************
SF$BMAP  DSECT
SF$BITS  DS    2048XL1                 2048 1 BYTE ENTRIES
************
*
************
*
* INDEX BLOCK. 1023 HALF WORD ENTRIES CONTAINING THE BLOCK # FOR
* A DATA BLOCK CONTAINING 24 SOURCE LINES. SO, ENTRY ZERO IS FOR
* LINES 0-23, ENTRY 1 IS FOR 24-47, ETC.
*
************
SF$IDXBK DSECT
SF$INEXT DS    H                       BLOCK # OF NEXT BLK IN CHAIN
SF$INDEX DS    1023H                   BLOCK #S OF DATA BLOCKS
*
************
*
* DATA BLOCK. 24 84 BYTE ENTRIES FOR THE SOURCE LINES CONTAINED
* IN THIS BLOCK. DATA BLOCKS FOR INTEGER LINE #S WILL CONTAIN
* 24 ENTRIES. DATA BLOCKS FOR FRACTIONAL LINE #S WILL CONTAIN 10
* ENTRIES. THERE CAN BE UP TO 4 LEVELS OF OVERFLOW BLOCKS FOR EACH
* INTEGER LINE #. ONE FOR EACH DECIMAL DIGIT OF THE FRACTION.
*
************
SF$DTABK DSECT
SF$DNEXT DS    H                       BLOCK # OF NEXT BLK IN CHAIN
SF$LINES DS    24XL84                  24 SOURCE LINES / BLOCK
*
************
*
* SOURCE LINE.
*
************
SF$SRCLN DSECT
SF$DEL   DS    XL1                     NON ZERO = DELETED
         DS    XL1                     UNUSED
SF$OVLF  DS    H                       BLOCK # OF LINE INSERTED
*                                      FOLLOWING THIS LINE
SF$SRC   DS    CL80                    THE SOURCE CODE
*
&SYSECT  CSECT
         END                                               
         EOD
         COP.D D0,P,BEMDSCTS
/*
/&          
// FIN