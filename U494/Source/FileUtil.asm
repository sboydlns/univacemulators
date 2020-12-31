       .
       . FILE UTILITIES FOR THE UNIVAC 494 EMULATOR
       .
       . COMMAND FORMAT:
       .   VERB [[SOURCE] DESTINATION]
       .
       . WHERE SOURE AND DESTINATION ARE FILE IDENTIFIERS.
       .
       . VALID VERBS ARE:
       .   HELP
       .   COPY
       .   QUIT
       .
       . VALID FILE INDENTIFIERS ARE:
       .   RDR - CARD READER
       .   PUN - CARD PUNCH
       .   PRN - LINE PRINTER
       .   FILENAME,FIXED|VAR,CHANNEL
       . WHERE FILENAME IS THE NAME OF A FILE IN THE SIMULATED DRUM,
       . FIXED DENOTES FIXED, 16 WORD RECORDS, VAR DENOTES VARIABLE LENGTH
       . RECORDS AND CHANNEL IS THE CHANNEL NUMBER OF THE SIMULATED DRUM
       . (DEFAULT 5).
       . 
                   START     BEGIN
       .
                   ER$DEF
                   DTF$DEF
       .                                      
       BEGIN       TYPE      BLEN,BANNER
       .
       LOOP        ACCEPT    PLEN,PROMPT RLEN,REPLY
                   SLJ       INITPARSE           . INIT GETTOKEN
                   SLJ       EXECCMD             . GO EXECUTE THE VERB
                   J         LOOP
       .                   
                   HALT      0
       . ++++++++++
       . EXECCMD
       .
       . EXTRACT THE VERB FROM THE COMMAND LINE AND EXECUTE IT.
       . ++++++++++
       EXECCMD    +0
                   SLJ       NXTTOKEN            . GET THE VERB
                   LB        B1,0                . FIND IT IN THE TABLE
                   LQ,X      77777
       VLOOP       MATE      VTABLE,B1
                   J         VBUMP               . NO MATCH
                   J,L       VJUMP,B1            . FOUND ONE, GO DO IT
       VBUMP       TBI,L     B1,VTEND            . LOOP
                   J         VLOOP
                   TYPE      UVLEN,UNKVERB       . UNKNOWN VERB
                   J,L       EXECCMD             . RETURN       
       . 
       INITPARSE   +0
                   SZ,W      NT$P1               . INITIALIZE GETTOKEN PARAMS.
                   LA        REPLY
                   SA,W      NT$P2
                   J,L       INITPARSE
       .
       NXTTOKEN    +0
                   EXRN      ERTOKEN$            . GET THE NEXT TOKEN
       NT$P1       +0
       NT$P2       +0
                   J,L       NXTTOKEN                                                         
       . ++++++++++
       . HELP
       .
       . DISPLAY THE HELP MESSAGE
       . ++++++++++                   
       HELP        LB        B1,0
       HLOOP       LA,U      HTABLE,B1
                   SA,W      H$P1
                   LA,L      HTABLE,B1
                   SA,W      H$P2
                   EXRN      ERTYPE$
       H$P1        +0
       H$P2        +0
                   TBI,W     B1,HTEND
                   J         HLOOP
                   J,L       EXECCMD
       . ++++++++++
       . COPY
       .
       . COPY A FILE
       .
       . COPY SOURCE TO DESTINATION
       . ++++++++++                                      
       COPY        SLJ       PARSEID             . PARSE THE FILE IDENTIFIERS
                   SLJ       DTFSETUPS           . SET UP IN/OUT DTFS
       . OPEN THE FILES                   
                   EXRN      EROPEN$
       OPDTF1      +0
                   EXRN      EROPEN$
       OPDTF2      +0                                            
       .                   
       CPNEXT      EXRN      ERREAD$             . READ NEXT RECORD
       RDDTF       +0
       .
                   LA,W      SPACES              . CLEAR OUTPUT RECORD
                   R         17D,,ADV
                   SA,W      OTREC
       .                   
                   LB        B1,0                . COPY TO OUTPUT BFR
       CPLOOP      LQ,W      INREC,B1
                   SQ,W      OTREC,B1
                   LA,W      SOURCE+FI$FV        . VARIABLE LENGTH RECORDS?
                   AN,W      FIXED,,AZERO
                   J         CPCHKEND            . YES, CHECK FOR EOR
       CPBUMP      TBI       B1,15D              . NO BUMP & LOOP
                   J         CPLOOP                   
       .
                   LA,W      DEST+FI$FV          . VARIABLE LENGTH RECORDS?
                   AN,W      FIXED,,ANOT
                   J         CPWRITE             . NO
                   SLJ       TRIMRIGHT           . YES
       .                                      
       CPWRITE     EXRN      ERWRITE$
       WRDTF       +0
                   J         CPNEXT              . GET NEXT CARD 
       . 
       CPCHKEND    SQ,A                          . WAS LAST WORD EOR?
                   NA        0,,AZERO
                   J         CPBUMP              . NO
                   LA,W      SPACES              . YES OVERWRITE EOR WITH SPACE
                   SA,W      OTREC,B1
                   J         CPWRITE             . AND WRITE THE RECORD
       . ++++++++++
       . TRIMRIGHT
       .
       . MARK START OF TRAILING BLANKS WITH A WORD OF -0
       . ++++++++++
       TRIMRIGHT   +0
                   LB        B1,15D              . START AT END OF REC
       TRLOOP      LA,W      OTREC,B1            . WORD = ALL SPACES
                   AN,W      SPACES,,AZERO                   
                   J         TRMARK              . NO
                   JBD       B1,TRLOOP           . YES, LOOP
       .
       TRMARK      LA,X      77777               . STORE -0 IN LAST BLANK WORD
                   TBI       B1,77777
                   SA,W      OTREC,B1
                   J,L       TRIMRIGHT
       . ++++++++++
       . QUIT - EXIT THE PROGRAM
       . ++++++++++
       QUIT        HALT      0
       . ++++++++++
       . PARSEID
       .
       . PARSE THE SOURCE AND DESTINATION FILE IDENTIFIERS
       . ++++++++++
       PARSEID     +0
                   LA,W      PI$GOTSRC           . INIT GOT DEST JUMP
                   SA,W      DODEST
                   LB        B1,SOURCE           . POINT TO SOURCE FILE
                   LA,W      FIXED               . INIT IDENTIFIER
                   SA,W      FI$FV,B1            
                   LA        5D
                   SA,W      FI$CHAN,B1
       .                   
                   SLJ       NXTTOKEN            . GET THE FILE NAME
                   DPTE      SPACES
                   J         GOTFNAME            . IT WAS ENTERED
                   J         PI$INVFILE          . NOT ENTERED, FAIL
       GOTFNAME    DPTE      CPUN                . PUNCH?
                   J         NOTPUN
                   J         PI$INVIPT           . YES, NOT VALID FOR INPUT
       NOTPUN      DPTE      CPRN                . PRINTER?
                   J         NOTPRN
                   J         PI$INVIPT           . YES, NOT VALID FOR INPUT                   
       NOTPRN      DPS       FI$NAME,B1          . SAVE IT
       .
       GETFV       SLJ       NXTTOKEN            . GET FIXED/VAR
                   DPTE      COMMA               . IS IT A COMMA?
                   J         DODEST              . NO, MUST BE DEST FILE NAME
                   SLJ       NXTTOKEN            . YES, GET FIXED/VAR
                   DPTE      FIXED               . IS IT VALID?
                   J         CHKVAR              
                   J         SAVFV               . YES
       CHKVAR      DPTE      VAR
                   J         PI$INVFV            . NOT VALID
       SAVFV       SA,W      FI$FV,B1
       .       
                   SLJ       NXTTOKEN            
                   DPTE      COMMA               . IS IT A COMMA?
                   J         DODEST              . NO, MUST BE DEST FILE NAME
                   SLJ       NXTTOKEN            . YES, GET CHANNEL
                   DEC2INT                       . YES, CONVERT TO INTEGER
                   JT        PI$INVCHAN,,AZERO   . ZERO, OOPS!
                   SA,W      FI$CHAN,B1          . SAVE IT                   
       .
                   SLJ       NXTTOKEN            . GET THE NEXT FILE NAME
       .       
       DODEST      J         $+1                 . ALTERED IN CODE
                   DPTE      SPACES              . IS FILE NAME PRESENT?
                   J         DODEST1             . YES
                   J         PI$INVFILE          . NO, OOPS!
       DODEST1     DPTE      CRDR                . READER?
                   J         NOTRDR
                   J         PI$INVOPT           . YES, NOT VALID FOR OUTPUT
       NOTRDR      LB        B1,DEST
                   DPS       FI$NAME,B1          . SAVE FILE NAME
                   LA,W      FIXED               . INIT IDENTIFIER
                   SA,W      FI$FV,B1            
                   LA        5D
                   SA,W      FI$CHAN,B1
                   LA,W      PI$GOTDEST          . SET TO AFTER GETTING DEST
                   SA,W      DODEST
                   J         GETFV               . GO GET FIXED/VAR                    
       .
       PI$INVFILE  TYPE      IFLEN,INVFILE       . SHOW ERROR
                   J,L       EXECCMD             . AND RETURN TO MAIN        
       PI$INVFV    TYPE      IFVLEN,INVFV        . SHOW ERROR
                   J,L       EXECCMD             . AND RETURN TO MAIN
       PI$INVCHAN  TYPE      ICLEN,INVCHAN       . SHOW ERROR
                   J,L       EXECCMD             . AND RETURN TO MAIN
       PI$INVIPT   DPS       INVIPT              . SHOW FILE NOT VALID FOR INPT
                   TYPE      IILEN,INVIPT
                   J,L       EXECCMD             . AND RETURN TO MAIN                   
       PI$INVOPT   DPS       INVOPT              . SHOW FILE NOT VALID FOR OTPT
                   TYPE      IOLEN,INVOPT
                   J,L       EXECCMD             . AND RETURN TO MAIN                   
       .                   
       PI$GOTSRC   J         DODEST+1
       PI$GOTDEST  J,L       PARSEID
       . ++++++++++
       . DTFSETUP
       .
       . SET UP THE INPUT AND OUTPUT DTFS BASED ON THE FILE IDENTIFIERS
       . GIVEN ON THE COMMAND LINE
       . ++++++++++
       DTFSETUPS   +0
       . SOURCE
                   LB        B1,SOURCE
                   DPL       FI$NAME,B1          . GET THE FILE NAME
                   DPTE      CRDR
                   J         DS$DRUM1            . NOT THE READER
                   LB        B2,RDR              . POINT TO RDR DTF
                   LA,W      FIXED               . FORCE TO FIXED
                   SA,W      FI$FV,B1
                   J         DS$FV1              . CONTINUE
       DS$DRUM1    LB        B2,DRUMIN           . POINT TO DRUM INPUT DTF
                   DPS       DTF$NAME,B2         . SET THE FILE NAME
       DS$FV1      LA,W      FI$FV,B1            . FIXED OR VARIABLE RECORDS?
                   AN,W      FIXED,,ANOT         
                   J         DS$FIXED1           . FIXED
                   LA        DTF$VARREC          . SET VARIABLE FLAG
                   ROR,W     DTF$FLAGS,B2
                   LA        17D                 . SET REC LEN TO 17
                   SA,U      DTF$RECBFR,B2
                   J         DS$CHAN1
       DS$FIXED1   LA,W      DTF$FLAGS,B2        . CLEAR VARIABLE FLAG
                   NOT       DTF$VARREC
                   SA,W      DTF$FLAGS,B2
                   LA        16D                 . SET REC LEN TO 16
                   SA,U      DTF$RECBFR,B2                   
       DS$CHAN1    LA,W      FI$CHAN,B1          . SET CHANNEL #
                   SA,L      DTF$CHAN,B2
                   SB,W      B2,OPDTF1           . SET PARAMS FOR OP/CL/RD
                   SB,W      B2,CLDTF1
                   SB,W      B2,RDDTF
                   SB,W      B2,SRCDTF
       . DEST                                     
                   LB        B1,DEST
                   DPL       FI$NAME,B1          . GET THE FILE NAME
                   DPTE      CPUN
                   J         DS$PRN              . NOT THE PUNCH
                   LB        B2,PUN              . POINT TO PUN DTF
                   LA,W      FIXED               . FORCE TO FIXED
                   SA,W      FI$FV,B1
                   J         DS$FV2              . CONTINUE
       DS$PRN      DPTE      CPRN                . NOT THE PRINTER
                   J         DS$DRUM2
                   LB        B2,PRN              . POINT TO PRN DTF
                   LA,W      FIXED               . FORCE TO FIXED
                   SA,W      FI$FV,B1
                   J         DS$FV2              . CONTINUE                   
       DS$DRUM2    LB        B2,DRUMOT
                   DPS       DTF$NAME,B2         . SET THE FILE NAME
       DS$FV2      LA,W      FI$FV,B1            . FIXED OR VARIABLE RECORDS?
                   AN,W      FIXED,,ANOT         
                   J         DS$FIXED2           . FIXED
                   LA        DTF$VARREC          . SET VARIABLE FLAG
                   ROR,W     DTF$FLAGS,B2
                   LA        17D                 . SET REC LEN TO 17
                   SA,U      DTF$RECBFR,B2
                   J         DS$CHAN2
       DS$FIXED2   LA,W      DTF$FLAGS,B2        . CLEAR VARIABLE FLAG
                   NOT       DTF$VARREC
                   SA,W      DTF$FLAGS,B2                   
                   LA        16D                 . SET REC LEN TO 16
                   SA,U      DTF$RECBFR,B2                   
       DS$CHAN2    LA,W      FI$CHAN,B1          . SET CHANNEL #
                   SA,L      DTF$CHAN,B2
                   SB,W      B2,OPDTF2           . SET PARAMS FOR OP/CL/WR
                   SB,W      B2,CLDTF2
                   SB,W      B2,WRDTF
                   SB,W      B2,DESTDTF
       .       
                   J,L       DTFSETUPS
       . ++++++++++
       . EOF
       . ++++++++++
       EOF         EXRN      ERCLOSE$
       CLDTF1      +0
                   EXRN      ERCLOSE$
       CLDTF2      +0                                      
       .                   
                   J,L       EXECCMD
       . ++++++++++
       . RDERR
       . ++++++++++
       RDERR       LB,W      B2,SRCDTF           . POINT TO INPUT DTF
                   J         SHOWERR             
       . ++++++++++
       . WRERR
       . ++++++++++
       WRERR       LB,W      B2,DESTDTF          . POINT TO OUTPUT DTF
                   J         SHOWERR
       . ++++++++++
       . DISPLAY I/O ERROR TEXT
       . ++++++++++
       SHOWERR     LA,U      DTF$FLAGS,B2
                   TA        11D,,YMORE
                   J         SHOWUNK             . UNKNOWN ERROR #
                   LB,A      B3                  . GET JUMP TBL INDEX
                   LA,U      DTFERR,B3           . GET MSG LENGTH & ADDR
                   LB,L      B3,DTFERR,B3
                   J         SECONT
       SHOWUNK     LA        LEUNKERR
                   LB        B3,EUNKERR
       SECONT      SA,W      SETYPE+1            . SET ERTYPE$ PARAMS.
                   SB,W      B3,SETYPE+2
                   DPL       DTF$NAME,B2
                   DPS       0,B3       
       SETYPE      EXRN      ERTYPE$
                   +0
                   +0                                      
                   J         EOF
       . ++++++++++
       . TABLE OF VALID VERBS
       . ++++++++++
       VTABLE      +'HELP'
                   +'COPY'
                   +'QUIT'
                   +'Q'
       VTEND       +$-VTABLE-1
       . ++++++++++
       . JUMP TABLE CORRESPONDING TO VTABLE
       . ++++++++++
       VJUMP       +HELP
                   +COPY
                   +QUIT
                   +QUIT
       . ++++++++++
       . FILE IDENTIFIER STUFF
       . ++++++++++
       FI$NAME     EQU       0                   . FILE NAME
       FI$FV       EQU       2                   . FIXED / VAR
       FI$CHAN     EQU       3                   . CHANNEL #
       .
       SOURCE      RES       4
       DEST        RES       4
       SRCDTF                1
       DESTDTF               1
       .
       SPACES      DLD       ' '
       FIXED       DLD       'FIXED'
       VAR         DLD       'VAR'
       COMMA       DLD       ','
       CRDR        DLD       'RDR'
       CPUN        DLD       'PUN'
       CPRN        DLD       'PRN'
       .
       RDR         DTFCR     16D,INREC
      -                      EOF 
      -                      RDERR 
       PUN         DTFCP     16D,OTREC
      -                      WRERR
       PRN         DTFPR     27D,OTREC
      -                      WRERR
      -                      1
       DRUMIN      DTFSD     ' '
      -                      DTF$INPUT
      -                      16D,INREC
      -                      INBFR
      -                      EOF
      -                      RDERR
       DRUMOT      DTFSD     ' '
      -                      DTF$OUTPUT
      -                      16D,OTREC
      -                      OTBFR
      -                      EOF
      -                      WRERR
       INREC       RES       16D
       OTREC       RES       27D
       INBFR       RES       1024D
       OTBFR       RES       1024D
       .
       BANNER      +'FILE UTILITIES FOR THE UNIVAC 494 EMULATOR VER 0.1'
                   +'    ^'
       BLEN        EQU       $-BANNER                                                         
       PROMPT      +'ENTER COMMAND'
                   +'    ^'
       PLEN        EQU       $-PROMPT
       UNKVERB     +'COMMAND NOT RECOGNIZED'
                   +'    ^'
       UVLEN       EQU       $-UNKVERB 
       INVFILE     +'INVALID FILE NAME'
                   +'    ^'
       IFLEN       EQU       $-INVFILE                  
       INVCHAN     +'INVALID CHANNEL'
                   +'    ^'
       ICLEN       EQU       $-INVCHAN 
       INVFV       +'INVALID FIXED/VAR'
                   +'    ^'
       IFVLEN      EQU       $-INVFV 
       INVIPT      RES       2D
                   +' NOT VALID FOR INPUT'
                   +'    ^'
       IILEN       EQU       $-INVIPT                                                      
       INVOPT      RES       2D
                   +' NOT VALID FOR OUTPUT'
                   +'    ^'
       IOLEN       EQU       $-INVOPT 
       .
       . TABLE OF DTF ERROR MESSAGES
       .
       DTFERR      +LEUNKERR,EUNKERR
                   +LEOPEN,EOPEN
                   +LENFND,ENFND
                   +LEUNK,EUNK
                   +LERDERR,ERDERR
                   +LEDFULL,EDFULL
                   +LENOMEM,ENOMEM
                   +LEWRERR,EWRERR
                   +LEINUSE,EINUSE
                   +LENOTOPN,ENOTOPN
                   +LEINTLK,EINTLK
       .
       EUNKERR     DLD       ' '
                   +' UNKNOWN ERROR NUMBER'
                   +'    ^'
       LEUNKERR    EQU       $-EUNKERR                          
       EOPEN       DLD       ' '
                   +' ALREADY OPEN'
                   +'    ^'
       LEOPEN      EQU       $-EOPEN                   
       ENFND       DLD       ' '
                   +' FILE NOT FOUND'
                   +'    ^'
       LENFND      EQU       $-ENFND                   
       EUNK        DLD       ' '
                   +' UNKNOWN FILE TYPE'
                   +'    ^'
       LEUNK       EQU       $-EUNK                   
       ERDERR      DLD       ' '
                   + ' READ ERROR'
                   +'    ^'
       LERDERR     EQU       $-ERDERR                   
       EDFULL       DLD       ' '
                   +' DRUM FULL'
                   +'    ^'
       LEDFULL     EQU       $-EDFULL                   
       ENOMEM      DLD       ' '
                   +' OUT OF MEMORY'
                   +'    ^'
       LENOMEM     EQU       $-ENOMEM                  
       EWRERR      DLD       ' '
                   +' WRITE ERROR'
                   +'     ^'
       LEWRERR     EQU       $-EWRERR                   
       EINUSE      DLD       ' '
                   +' DEVICE IN USE'
                   +'    ^'
       LEINUSE     EQU       $-EINUSE                   
       ENOTOPN     DLD       ' '
                   +' ALREADY OPEN'
                   +'    ^'
       LENOTOPN    EQU       $-ENOTOPN                   
       EINTLK      DLD       ' '
                   +' INTERLOCK'
                   +'    ^'
       LEINTLK     EQU       $-EINTLK                   
       .                                                     
       REPLY       RES       16D
       RLEN        EQU       16D
       .                                             
       HELP1       +'COMMAND FORMAT:'
                   +'    ^'
       H1LEN       EQU       $-HELP1                                     
       HELP2       +'  VERB [SOURCE [DESTINATION]]'
                   +'    ^'
       H2LEN       EQU       $-HELP2                                     
       HELP3       +'WHERE SOURE AND DESTINATION ARE FILE IDENTIFIERS.'
                   +'    ^'
       H3LEN       EQU       $-HELP3                                     
       HELP4       +'VALID VERBS ARE:'
                   +'    ^'
       H4LEN       EQU       $-HELP4                                     
       HELP5       +'HELP'
                   +'    ^'
       H5LEN       EQU       $-HELP5                                     
       HELP6       +'COPY'
                   +'    ^'
       H6LEN       EQU       $-HELP6                                     
       HELP7       +'QUIT'
                   +'    ^'
       H7LEN       EQU       $-HELP7
       HELP8       +'VALID FILE INDENTIFIERS ARE:'
                   +'    ^'
       H8LEN       EQU       $-HELP8
       HELP9       +'RDR'
                   +'    ^'
       H9LEN       EQU       $-HELP9
       HELP10      +'PUN'
                   +'    ^'
       H10LEN      EQU       $-HELP10
       HELP11      +'PRN'
                   +'    ^'
       H11LEN       EQU       $-HELP11
       HELP12       +'FILENAME,FIXED/VAR,CHANNEL'
                   +'    ^'
       H12LEN      EQU       $-HELP12
       .
       HTABLE      +H1LEN,HELP1
                   +H2LEN,HELP2
                   +H3LEN,HELP3
                   +H4LEN,HELP4
                   +H5LEN,HELP5
                   +H6LEN,HELP6
                   +H7LEN,HELP7
                   +H8LEN,HELP8
                   +H9LEN,HELP9
                   +H10LEN,HELP10
                   +H11LEN,HELP11
                   +H12LEN,HELP12
       HTEND       +$-HTABLE-1
       .                   
                   END                          