                   START     BEGIN
       .
                   ER$DEF
                   DTF$DEF                   
       .
       BEGIN       LA,W      SETUP,,AZERO
                   J         L1
                   LA        1
                   SA,W      SETUP
                   SA,W      I
                   OPEN      INFILE
       .
       . READ THE FIRST LINE OF A GROUP OF RECORDS TO GET THE RECORD
       . TYPE. RECORD TYPE OCCUPIES THE FIRST WORD OF THE RECORD.
       .                   
       L1002       READ      INFILE
                   SLJ       GETRTYPE
                   JT        CFG$ERR,,ANEG       . REC TYPE NOT IN RANGE
                   TA        7D,,YMORE
                   J         CFG$ERR             . REC TYPE NOT IN RANGE
                   SA,W      IKIND
                   LB,A      B1
                   J,L       RT$JUMP,B1          . GO TO APPROPRIATE ROUTINE
       .
       . READ LOCATION TYPE INFO INTO THE LLINE TABLE
       .                   
       L1004       READ      INFILE
                   SLJ       GETRTYPE            . GET LOCATION ID
                   JE        9999D,L1002         . ID = 9999, END OF TABLE
                   SA,W      JKIND
                   LQ,W      I                   . LLINE ITEM INDEX
                   ZA                            . MULT BY 17
                   M         LLSIZE
                   AQ        LLINE
                   SQ,W      LLCRNT              . SAVE FOR LATER
                   LB,W      B1,LLCRNT           . B1 = LLINE ITEM
                   LB        B2,0                . B2 = INPUT TEXT
       L1006       LA,W      INREC+1D,B2
                   SA,W      LLTEXT,B1
                   NA        0,,ANOT             . WORD = ALL ONES?
                   J         L1007               . YES, END OF TEXT
                   TBI       B1,77777            . NO, LOOP
                   TBI       B2,14D
                   J         L1006
       L1007       LA,W      CR                  . ADD <CR> TO END OF LINE
                   SA,W      LLTEXT,B1
                   LB,W      B1,LLCRNT           . SAVE ITEM LENGTH
                   TBI       B2,77777 
                   SB,W      B2,LLLEN,B1
                   LA,W      IKIND
                   JE        6,L1023             . REC TYPE = 6?
                   JE        5,L1011             . REC TYPE = 5?
                   JE        1,L1008             . REC TYPE = 1?
       .
       . RECORD TYPE 2 (SHORT ROOM LABELS)
       .                         
                   LB,W      B1,JKIND
                   LA,W      I
                   SA,W      STEXT,B1
                   J         L1010
       . 
       . RECORD TYPE 1 (LONG DESCRIPTIONS)
       .
       L1008       LB,W      B1,JKIND            . LTEXT(JKIND) <> 0
                   LA,W      LTEXT,B1,AZERO
                   J         L1009
                   LA,W      I                   . LTEXT(JKIND) = I
                   SA,W      LTEXT,B1
                   J         L1010
       L1009       LA,W      LLCRNT              . LLINE(I-1,1) = I
                   AN        LLSIZE
                   LB,A      B1
                   LA,W      I
                   SA,W      LLNEXT,B1
       L1010       RI,W      I                   . I = I + 1
                   TA        601D,,YLESS         . TABLE FULL?                   
                   J         L1004
                   TYPE      TMLLEN,TMLERR       . YES
                   HALT      1D      
       .
       . RECORD TYPE 5 (STATIC GAME STATES)
       .
       L1011       LA,W      JKIND               . JKIND < 200
                   TA        200D,,YLESS
                   J         L1012               . YES
                   AN        100D                . JKIND - 100
                   LB,A      B1
                   LA,W      BTEXT,B1,AZERO      . BTEXT(JKIND-100) = 0?
                   J         L1009               . NO
                   LQ,W      I
                   SQ,W      BTEXT,B1            . BTEXT(JKIND-100) = I
                   LA,W      JKIND
                   AN        200D
                   LB,A      B1                  
                   SQ,W      BTEXT,B1            . BTEXT(JKIND-200) = I
                   J         L1010
       L1012       LB,W      B1,JKIND            
                   LA,W      BTEXT,B1,AZERO      . BTEXT(JKIND) = 0? 
                   J         L1009               . NO
                   LA,W      I
                   SA,W      BTEXT,B1
                   J         L1010
       .
       . RECORD TYPE 3 (TRAVEL DATA)
       .                                                  
       L1013       LA        1                   . I = 1
                   SA,W      I
       L1014       READ      INFILE                  
                   SLJ       GETRTYPE            . GET JKIND
                   JE        9999D,L1002         . ID = 9999, END OF TABLE
                   SA,W      JKIND
                   LA,W      INREC+1D            . GET LKIND
                   LQ,W      SPACES
                   DEC2INT
                   SA,W      LKIND
       . 
       . GET TABLE OF UP TO 10 NUMBERS
       .
                   R         11D,,ADV            . CLEAR TABLE
                   SZ,W      TK
                   LB        B1,2                . B1 = INREC
                   LB        B2,1                . B2 = TK
       L1014A      LA,W      INREC,B1            . GET # FROM IN REC
                   NA        0,,ANOT             . = 7777777777?
                   J         L1014B              . YES, END OF RECORD
                   LA,W      INREC,B1            . NO, CVT TO INTEGER
                   LQ,W      SPACES
                   DEC2INT
                   SA,W      TK,B2               . SAVE IT
                   TBI       B1,77777            . BUMP INDICES
                   TBI       B2,10D
                   J         L1014A              . LOOP
       L1014B      LB,W      B1,JKIND            
                   LA,W      KEY,B1,AZERO        . KEY(JKIND) <> 0                   
                   J         L1016               . YES
                   LA,W      I                   . KEY(JKIND) = I
                   SA,W      KEY,B1
                   J         L1017
       L1016       LB,W      B1,I                . TRAVEL(I-1) = -TRAVEL(I-1)
                   JBD       B1,$+1
                   LA,W      TRAVEL,B1
                   NA
                   SA,W      TRAVEL,B1       
       L1017       LB        B2,1                . FOR L=1 TO 10
       L1017A      LA,W      TK,B2,ANOT          . TK(L) = 0?
                   J         L1019               . YES
                   LQ,W      LKIND               . LKIND*1024
                   LSQ       10D
                   AQ,A                          . LKIND*1024+TK(L)
                   LB,W      B1,I
                   SQ,W      TRAVEL,B1           . TRAVEL(I)=LKIND*1024+TK(L)
                   RI,W      I                   . I = I+1
                   JNE       400D,L1018          . IF I<>1000 GOTO L1017
                   TYPE      TM3LEN,TM3ERR
                   HALT      2D
       L1018       TBI       B2,10D
                   J         L1017A                   
       L1019       LB,W      B1,I                . TRAVEL(I-1)=-TRAVEL(I-1)
                   JBD       B1,$+1
                   LA,W      TRAVEL,B1
                   NA
                   SA,W      TRAVEL,B1
                   J         L1014
       .
       . RECORD TYPE 4 (GROUPED VOCABULARY KEYWORDS)
       .
       L1020       LB        B1,1                . FOR IU=1 TO 200
       L1020A      READ      INFILE
                   SLJ       GETRTYPE            . GET ID
                   SA,W      KTAB,B1
                   LQ,W      INREC+1D
                   SQ,W      ATAB,B1
                   JE        9999D,L1002         . ID = 9999, END OF TABLE
                   TBI       B1,200D
                   J         L1020A
                   TYPE      TMWLEN,TMWERR
                   HALT      3D
       .
       . RECORD TYPE 6 (HINTS AND EVENTS)
       .
       L1023       LB,W      B1,JKIND            . RTEXT(JKIND) = 0?
                   LA,W      RTEXT,B1,AZERO
                   J         L1009               . NO
                   LA,W      I                   . RTEXT(JKIND) = I
                   SA,W      RTEXT,B1       
                   J         L1010
       .
       . CONFIGURATION FILE LOADED
       .
       L1100       TYPE      CLLEN,CFGLOADED
                   CLOSE     INFILE
                   
                   LB        B1,1                . FOR I=1 TO 100
       L1101       LA,W      IPLT,B1
                   SA,W      IPLACE,B1
                   LA,W      IFIXT,B1
                   SA,W      IFIXED,B1
                   SZ,W      ICHAIN,B1
                   TBI       B1,100D
                   J         L1101
                   
                   LB        B1,1                . FOR I=1 TO 300
       L1102       SZ,W      COND,B1
                   SZ,W      ABB,B1
                   SZ,W      IOBJ,B1
                   TBI       B1,300D
                   J         L1102
                   LA        1
                   R         10D,,ADV            . FOR I=1 TO 10
                   SA,W      COND+1D
       L1103       LA        2
                   SA,W      COND+16D                          
                   SA,W      COND+20D                          
                   SA,W      COND+21D                          
                   SA,W      COND+22D                          
                   SA,W      COND+23D                          
                   SA,W      COND+24D                          
                   SA,W      COND+25D
                   SA,W      COND+26D
                   SA,W      COND+31D
                   SA,W      COND+32D                          
                   SA,W      COND+79D
                   
                   LB        B1,1                . FOR I=1 TO 100
       L1107A      LA,W      IPLACE,B1,ANOT      . IPLACE(I) = 0?
                   J         L1107               . YES, CONTINUE
                   LB,A      B2                  . KTEM = IPLACE(I)
                   LA,W      IOBJ,B2,AZERO       . IOBJ(KTEM) = 0?
                   J         L1104               . NO
                   SB,W      B1,IOBJ,B2          . IOBJ(KTEM) = I
                   J         L1107
       L1104       LB,W      B2,IOBJ,B2          . KTEM = IOBJ(KTEM)
       L1105       LA,W      ICHAIN,B2,AZERO     . ICHAIN(KTEM) = 0?
                   J         L1106               . NO
                   SB,W      B1,ICHAIN,B2        . ICHAIN(KTEM) = I
                   J         L1107
       L1106       LB,W      B2,ICHAIN,B2        . KTEM = ICHAIN(KTEM)
                   J         L1105
       L1107       TBI       B1,100D             . BUMP I
                   J         L1107A                   
                   
                   SZ,W      IDWARF
                   SZ,W      IWEST
                   SZ,W      IDETAL
                   LA        1
                   SA,W      IFIRST
                   SA,W      ILONG
       .                   
                   TYPE      IDLEN,INITDONE
       .
       . WELCOME TO CAVE
       .
       L1          SLJ       YES                 . DISPLAY 'WELCOME TO CAVE'
                   +65D
                   +1D
                   +0
                   +0
                   LA        1
                   SA,W      LL
                   SA,W      LOC
       L2          LB        B1,1                . FOR I=1 TO 3
       L73A        LA,W      ODLOC,B1            . ODLOC(I) <> L?
                   AN,W      LL,,AZERO
                   J         L73                 . YES
                   LA,W      DSEEN,B1,ANOT       . DSEEN(I) = 0?
                   J         L73                 . YES
                   LA,W      LOC                 . L = LOC
                   SA,W      LL
                   SLJ       SPEAK               . A DWARF BLOCKS YOUR WAY
                   +2D
                   J         L74
       L73         TBI       B1,3D               . I = I + 1
                   J         L73A                   
       L74         LA,W      LL                  . LOC = L
                   SA,W      LOC
       .
       . DWARF STUFF
       .
                   LA,W      IDWARF,,AZERO       . IDWARF = 0?
                   J         L60                 . NO    
                   LA,W      LOC                 . LOC = 15?
                   JNE       15D,L71             . NO
                   LA        1                   . YES, IDWARF = 1
                   SA,W      IDWARF               
                   J         L71
       L60         JNE       1D,L63              . IDWARF <> 1, THEN 63
       . GET A RANDOM NUMBER. THERE IS A 5% CHANCE THAT THE FOLLOWING CODE
       . WILL DISPLAY A MESSAGE ABOUT THE DWARF THROWING AN AXE AND MISSING.       
                   SLJ       RANDOM              . GET RANDOM # < 100
                   +100D
                   JG        5,L71               . RANDOM # > 5 THEN 71
                   LA        2                   . IDWARF = 2
                   SA,W      IDWARF
                   LB        B1,1                . FOR I=1 TO 3
       L61         SZ,W      DLOC,B1
                   SZ,W      ODLOC,B1
                   SZ,W      DSEEN,B1
                   TBI       B1,3D
                   J         L61
                   SLJ       SPEAK               . DWARF THREW AXE AND MISSED
                   +3D
                   LB,W      B1,AXE              . CHAIN(AXE) = IOBJ(LOC)
                   LB,W      B2,LOC
                   LA,W      IOBJ,B2
                   SA,W      ICHAIN,B1
                   SB,W      B1,IOBJ,B2
                   SB,W      B2,IPLACE,B1
                   J         L71
       .
       L63         RI,W      IDWARF              . IDWARD = IDWARF + 1
                   SZ,W      ATTACK
                   SZ,W      DTOT
                   SZ,W      STICK                   
                   LB        B1,1                . FOR I = 1 TO 3
       L66A        SB,A      B1                  . 2*I+IDWARF
                   LSA       1
                   A,W       IDWARF
                   SA,W      L66$TEMP            . SAVE FOR LATER
                   JL        8D,L66              . < 8, THEN 66
                   JG        23D,L66C            . > 23 THEN CHECK DSEEN
                   J         L66D                . NO
       L66C        LA,W      DSEEN,B1,ANOT       . DSEEN(I) = 0?       
                   J         L66                 . YES, CONTINUE       
       L66D        LA,W      DLOC,B1             . ODLOC(I) = DLOC(I)
                   SA,W      ODLOC,B1
                   LA,W      DSEEN,B1,ANOT       . DSEEN(I) <> 0 THEN CHK LOC
                   J         L66F                . DSEEN = 0 CONTINUE
       L66E        LA,W      LOC                 . LOC > 14 THEN 65
                   JG        14D,L65
       L66F        LB,W      B2,L66$TEMP         . GET 2*I+IDWARF
                   LA,W      DTRAV,B2            . DLOC(I) = DTRAV(2*I+IDWARF)
                   SA,W      DLOC,B1
                   SZ,W      DSEEN,B1            . DSEEN(I) = 0
                   LA,W      DLOC,B1             . DLOC(I) <> LOC?
                   AN,W      LOC,,ANOT
                   J         L65                 . EQUAL, CONTINUE
                   LA,W      ODLOC,B1            . ODLOC(I) <> LOC?
                   AN,W      LOC,,AZERO
                   J         L66                 . YES THEN 66
       L65         LA        1
                   SA,W      DSEEN,B1            . DSEEN(I) = 1
                   LA,W      LOC                 . DLOC(I) = LOC
                   SA,W      DLOC,B1
                   RI,W      DTOT                . DTOT = DTOT + 1
                   LA,W      DLOC,B1             . ODLOC(I) <> DLOC(I) THEN 66
                   AN,W      ODLOC,B1,AZERO
                   J         L66
                   RI,W      ATTACK              . ATTACK = ATTACK + 1
                   SLJ       RANDOM              . GET RANDOM #
                   +100D
                   LA,W      $-1
                   TA        11D,,YMORE          . < 10?
                   RI,W      STICK               . YES, STICK = STICK + 1
       L66         TBI       B1,3D               . BUMP I
                   J         L66A                . LOOP
                   J         L66B
       L66$TEMP    +0
       . MULTIPLE THREATENING DWARVES IN ROOM
       L66B        LA,W      DTOT,,ANOT          . DTOT = 0?
                   J         L71                 . YES
                   JE        1,L75               . DTOT = 1 THEN 75
                   INT2DEC                       . CVT # TO FIELDATA
                   SA,W      DTNUM
                   TYPE      DTLEN,DTHREAT
                   J         L77
       . ONE THREATENING DWARF IN ROOM                          
       L75         SLJ       SPEAK
                   +4D    
       . MULTIPLE DWARVES THROW A KNIFE
       L77         LA,W      ATTACK,,ANOT        . ATTACK = 0?
                   J         L71                 . YES
                   JE        1,L79               . ATTACK = 1 THEN 79
                   INT2DEC                       . CVT TO FIELDATA
                   SA,W      DKNIFE
                   TYPE      DKLEN,DKNIFE
                   J         L81
       . ONE DWARF THROWS KNIFE
       L79         SLJ       SPEAK
                   +5D
                   LA,W      STICK               . IT HITS/MISSES
                   A         52D
                   SA,W      $+2
                   SLJ       SPEAK
                   +0
                   LA,W      STICK
                   JE        1D,L83              . YOU'RE DEAD
                   J         L71
       . MULTIPLE DWARVES HIT YOU
       L81         LA,W      STICK,,ANOT         . STICK = 0?
                   J         L69                 . YES
                   JE        1D,L82              . STICK = 1 THEN 82                                     
                   INT2DEC                       . CVT TO FIELDATA
                   SA,W      DHIT
                   TYPE      DHLEN,DHIT
                   J         L83
       . ONE DWARF HITS YOU
       L82         SLJ       SPEAK
                   +6D     
       L83         ACCEPT    GOLEN,GAMEOVER RPLYLEN,RPLYBFR
                   J         L71
       . NO DWARVES HIT YOU
       L69         SLJ       SPEAK
                   +7D
       .
       . PLACE DESCRIPTION
       .                                      
       L71         LB,W      B1,LL               . B1 = L
                   LQ,W      STEXT,B1            . KK = STEXT(L)
                   LA,W      ABB,B1,ANOT         . ABB(L) = 0?
                   J         L71A                . YES
                   SQ,A      0,,AZERO            . KK = 0?
                   J         L71B                . NO
       L71A        LQ,W      LTEXT,B1            . KK = LTEXT(L)
       L71B        SQ,A      0,,ANOT             . KK = 0?
                   J         L7                  . YES
                   M         LLSIZE              . CALC ADDR OF LLINE ITEM
                   AQ        LLINE
                   SQ,A
                   LB,A      B2                  . B2 = KK
                   LA,W      LLLEN,B2            . SET UP PARAMS
                   SA,W      L71$P1
                   LA        LLTEXT,B2
                   SA,W      L71$P2
                   EXRN      ERTYPE$
       L71$P1      +0
       L71$P2      +0
                   LA,W      LLNEXT,B2           . GET INDEX OF NEXT
                   LQ,A
                   JNE       0,L71B              . THERE IS MORE
                   TYPE      1D,CR
                   
       L7          LB,W      B1,LL               
                   LA,W      COND,B1
                   JE        2D,L8               . COND(L) = 2? THEN 8
                   LA,W      LOC
                   JNE       33D,L7A             . LOC <> 33 THEN SKIP
                   SLJ       RANDOM              . GET RANDOM #
                   +100D
                   JG        25D,L7A             . # >= 25 THEN SKIP
                   JE        25D,L7A
                   SLJ       SPEAK               . HOLLOW VOICE SAYS PLUGH
                   +8D
       L7A         LA,W      LOC                 
                   SA,W      JJ                  . J = LOC
                   J         L2000
       .
       . PROCESS MOVEMENT WORDS. I.E. THOSE WITH IDS <= 999.
       .            
       . GO GET A NEW LOCATION
       .
       L8          LB,W      B1,LOC
                   LA,W      KEY,B1
                   SA,W      KK,,ANOT            . KK = KEY(LOC)
                   J         L19                 . KK=0 THEN 19
                   LA,W      K
                   JE        57D,L32             . K=57 THEN 32
                   JE        67D,L40             . K=67 THEN 40
                   JE        8D,L12              . K=8 THEN 12 
                   LA,W      LL
                   SA,W      LOLD                . LOLD = L
       L9          LB,W      B1,KK
                   LA,W      TRAVEL,B1           . LL = TRAVEL(KK)
                   TA        0,,YLESS
                   NA                            . LL<0 THEN LL=-LL
                   SA,W      LLL
                   SA,W      L9$P1
                   SLJ       MOD
       L9$P1       +0                   
                   1024D
                   JE        1D,L10              . MOD(LL,1024)=1 THEN 10
                   AN,W      K,,ANOT
                   J         L10                 . MOD(LL,1024)=K THEN 10
                   LB,W      B1,KK
                   LA,W      TRAVEL,B1
                   JL        0,L11               . TRAVEL(KK)<0 THEN 11
                   RI,W      KK                  . KK=KK+1
                   J         L9
       .
       L12         LA,W      LOLD                . TEMP=LOLD
                   LQ,W      LL
                   SQ,W      LOLD                . LOLD=L
                   SA,W      LL                  . L=TEMP
                   J         L21
       .
       L10         LA,W      LLL
                   LRSA      10D
                   SA,W      LL                  . L=LL/1024
                   J         L21
       .                          
       L11         LA        12D                 . 
                   SA,W      JSPK                . JSPK=12/CAN'T APPLY WORD
                   LA,W      K
                   JL        43D,L11CONT1        . K>=43 AND K<=46 THEN JSPK=9.
                   JG        46D,L11CONT1
       L11SET9     LQ        9D                  . NO WAY TO GO THERE
                   SQ,W      JSPK
                   J         L11CONT5
       L11CONT1    JE        29D,L11SET9         . K=29 OR K=30 THEN JSPK=9
                   JE        30D,L11SET9                   
                   JE        7D,L11CONT2         . K=8,9,36,37,68 THEN JSPK=10
                   JE        8D,L11CONT2
                   JE        36D,L11CONT2
                   JE        37D,L11CONT2
                   JE        68D,L11CONT2
                   J         L11CONT3
       L11CONT2    LQ        10D
                   SQ,W      JSPK                . UNSURE HOW YOU ARE FACING
                   J         L11CONT5
       L11CONT3    JE        11D,L11CONT4        . K=11 OR 19 THEN JSPL=11
                   JE        19D,L11CONT4
                   J         L11CONT5
       L11CONT4    LQ        11D
                   SQ,W      JSPK                . DON'T KNOW IN FROM OUT
       L11CONT5    LA,W      JVERB
                   JNE       1D,L11CONT6         . JVERB=1 THEN JSPK=59
                   LQ        59D
                   SQ,W      JSPK                . CAN'T TELL WHERE THINGS ARE
       L11CONT6    LA,W      K    
                   JNE       48D,L11CONT7         
                   LA        42D
                   SA,W      JSPK                . NOTHING HAPPENS
       L11CONT7    JNE       17D,L11CONT8
                   LA        80D
                   SA,W      JSPK                . WHICH WAY?
       L11CONT8    LA,W      JSPK  
                   SA,W      L11$P1
                   SLJ       SPEAK
       L11$P1      +0
                   J         L2                                      
       .         
       L19         SLJ       SPEAK               . I DON'T UNDERSTAND THAT
                   +13D
                   LA,W      LOC
                   SA,W      LL                  . L = LOC
                   LA,W      IFIRST,,AZERO       . IFIRST = 0?
                   J         L21                 . NO
                   SLJ       SPEAK               .
                   +14D                          . I UNDERSTAND COMPASS DIRNS
       .
       L21         LA,W      LL
                   JL        300D,L2             . L<300 THEN 2
                   AN        299D
                   JG        15D,L2              . OUT OF RANGE
                   LB,A      B1
                   J,L       L21$J,B1            . GO TO APPROPRIATE ROUTINE
       .
       L21$J       +0
                   +L22                          . 300                        
                   +L23                          . 301
                   +L24                          . 302
                   +L25                          . 303
                   +L26                          . 304
                   +L31                          . 305
                   +L27                          . 306
                   +L28                          . 307
                   +L29                          . 308
                   +L30                          . 309
                   +L33                          . 310
                   +L34                          . 311
                   +L36                          . 312
                   +L37                          . 313
                   +L39                          . 314
       .
       . INVOKED FROM FOREST/DEEP VALLEY WHEN FOREST/FORWARD/BACK/NORTH
       .
       L22         LA        6D
                   SA,W      LL                  . L = 6 (FOREST NEAR ROAD)
                   SLJ       RANDOM
                   +100D
                   JL        50D,L2            
                   LA        5D                  . RAN>=0.5 THEN L=5 
                   SA,W      LL                  . (FOREST/DEEP VALLEY)
                   J         L2
       .
       . INVOKED FROM GRATE WHEN DOOR/DOWNSTREAM/INWARD/DOWN                  
       .         
       L23         LA        23D
                   SA,W      LL                  . L = 23 (CAN'T GO THRU LOCKED)
                   LB,W      B1,GRATE
                   LA,W      PROP,B1,ANOT        . PROP(GRATE><>0?
                   J         L2                  . NO
                   LA        9D
                   SA,W      LL                  . L = 9 (BELOW GRATE)
                   J         L2
       .
       . INVOKED FROM BELOW GRATE WHEN LEAVE/BUILDING
       .                   
       L24         LA        9D
                   SA,W      LL                  . L = 9 (BELOW GRATE)
                   LB,W      B1,GRATE,ANOT       . PROP(GRATE)<>0?
                   J         L2                  . NO
                   LA        8D
                   SA,W      LL                  . L = 8 (GRATE)
                   J         L2
       .
       . INVOKED FROM SMALL PIT WHEN DOWN/PIT/STEPS
       .                   
       L25         LA        20D
                   SA,W      LL                  . L = 20 (BROKEN NECK)
                   LB,W      B1,NUGGET
                   LA,W      IPLACE,B1,APOS      . IPLACE(NUGGET)<0?
                   J         L2                  . YES
                   LA        15D
                   SA,W      LL                  . L = 15 (HALL OF MISTS)
                   J         L2
       . 
       . INVOKED FROM HALL OF MISTS WHEN UP/PIT/STEPS/DOME/PASSAGE/EAST
       .                  
       L26         LA        22D
                   SA,W      LL                  . L = 22 (DOME UNCLIMBABLE)
                   LB,W      B1,NUGGET
                   LA,W      IPLACE,B1,APOS      . IPLACE(NUGGET)<0?
                   J         L2                  . YES
                   LA        14D
                   SA,W      LL                  . L = 14 (SMALL PIT)
                   J         L2
       .
       . INVOKED FROM EAST BANK OF FISSURE WHEN MAGIC/OVER/ACROSS/WEST
       . /INWARD/JUMP
       .                   
       L27         LA        27D                 
                   SA,W      LL                  . L = 27 (W SIDE OF FISSURE)
       . PROP(12) = CRYSTAL BRIDGE                   
                   LA,W      PROP+12D,,AZERO     . PROP(12)=0?
                   J         L2                  . NO
                   LA        31D
                   SA,W      LL                  . L = 31 (NO WAY ACROSS)                  
                   J         L2
       . 
       . INVOKED FROM HALL OF MOUNTAIN KING WHEN NORTH/LEFT
       .                  
       L28         LA        28D
                   SA,W      LL                  . L = 28 (LOW N/S PASSAGE)
                   LB,W      B1,SNAKE
                   LA,W      PROP,B1,AZERO       . PROP(SNAKE)=0?
                   J         L2                  . NO
                   LA        32D
                   SA,W      LL                  . L = 32 (CAN'T PASS SNAKE)
                   J         L2
       . 
       . INVOKED FROM HALL OF MOUNTAIN KING WHEN SOUTH/RIGHT
       .                  
       L29         LA        29D
                   SA,W      LL                  . L = 29 (SOUTH SIDE CHAMBER)
                   LB,W      B1,SNAKE
                   LA,W      PROP,B1,AZERO       . PROP(SNAKE)=0?
                   J         L2                  . NO
                   LA        32D
                   SA,W      LL                  . L = 32 (CAN'T PASS SNAKE)
                   J         L2
       . 
       . INVOKED FROM HALL OF MOUNTAIN KING WHEN WEST/ONWARD
       .                  
       L30         LA        30D
                   SA,W      LL                  . L = 30 (WEST SIDE CHAMBER)
                   LB,W      B1,SNAKE
                   LA,W      PROP,B1,AZERO       . PROP(SNAKE)=0?
                   J         L2                  . NO
                   LA        32D
                   SA,W      LL                  . L = 32 (CAN'T PASS SNAKE)
                   J         L2
       .
       . INVOKED FROM EAST BANK OF FISSURE WHEN ONWARD
       .                   
       L31         TYPE      GOLEN,GAMEOVER
                   J         L1100
       .                   
       L32         LA,W      IDETAL
                   JG        2D,L32A             . IDETAL>=3
                   SLJ       SPEAK               . NOT ALLOWED TO GIVE MORE DTL
                   +15D
       L32A        RI,W      IDETAL              . IDETAL=IDETAL+1
                   LA,W      LOC
                   SA,W      LL                  . L = LOC
                   LB,A      B1
                   SZ,W      ABB,B1              . ABB(L) = 0
                   J         L2
       .
       . INVOKED FROM DEBRIS ROOM WHEN DEPRESSION
       .                                      
       L33         LA        8D
                   SA,W      LL                  . L = 8 (GRATE)
                   LB,W      B1,GRATE
                   LA,W      PROP,B1,AZERO       . PROP(GRATE)=0?
                   J         L2                  . NO
                   LA        9D
                   SA,W      LL                  . L = 9 (BELOW GRATE)
                   J         L2
       . 
       . INVOKED FROM BEDQUILT WHEN SOUTH
       .                  
       L34         SLJ       RANDOM
                   +100D
                   JG        20D,L35             . RAN>0.2 THEN 35
                   LA        68D
                   SA,W      LL                  . L = 68 (SLAB ROOM)
                   J         L2
       L35         LA        65D
                   SA,W      LL                  . L = 65 (BEDQUILT)
       L38         SLJ       SPEAK               . BACK IN THE MAIN PASSAGE
                   +56D
                   J         L2
       . 
       . INVOKED FROM BEDQUILT WHEN UP
       .                  
       L36         SLJ       RANDOM
                   +100D
                   JG        20D,L35             . RAN>0.2 THEN 35
                   LA        39D
                   SA,W      LL                  . L = 39 (DUSTY ROCKS)
                   SLJ       RANDOM
                   +100D
                   JL        50D,L2              . RAN<0.5 THEN 2
                   LA        70D
                   SA,W      LL                  . L = 70 (SECRET N/S CANYON)
                   J         L2
       . 
       . INVOKED WHEN GOING NORTH FROM SWISS CHEESE ROOM                  
       .
       L37         LA        66D
                   SA,W      LL                  . L = 66 (SWISS CHEESE ROOM)
                   SLJ       RANDOM
                   +100D
                   JG        40D,L38             . RAN>0.4 THEN 38
                   LA        71D
                   SA,W      LL                  . L = 71 (SECRET CANYON)
                   SLJ       RANDOM
                   +100D
                   JL        25D,L2              . RAN<0.25 THEN 2
                   LA        72D
                   SA,W      LL                  . L = 72 (LARGE LOW ROOM)
                   J         L2
       .
       . INVOKED WHEN GOING SOUTH FROM SWISS CHEESE ROOM                  
       .
       L39         LA        66D
                   SA,W      LL                  . L = 66 (SWISS CHEESE ROOM)
                   SLJ       RANDOM
                   +100D
                   JG        20D,L38             . RAN>0.2 THEN 38
                   LA        77D
                   SA,W      LL                  . L = 77 (E/W CANYON)
                   J         L2
       .                                      
       L40         LA,W      LOC
                   JG        7D,L40A             . LOC>=7 THEN SKIP
                   SLJ       SPEAK               . I DON'T KNOW WHERE CAVE IS
                   +57D
                   J         L40B
       L40A        SLJ       SPEAK               . CAN ONLY TELL WHAT YOU SEE
                   +58D
       L40B        LA,W      LOC
                   SA,W      LL                  . L = LOC
                   J         L2                                              
       .
       . DO NEXT INPUT
       .
       . THIS BIT SEEMS TO HAVE TO DO WITH HAVING LIGHT IN A DARK ROOM
       .                  
       L2000       SZ,W      LTRUBL              . LTRUBL = 0
                   LB,W      B1,JJ               . LOC = J
                   SB,W      B1,LOC 
                   LA,W      ABB,B1              . ABB(J) + 1
                   A         1
                   SA,W      L2000$P1            . SET UP MOD PARAMS
                   SLJ       MOD
       L2000$P1    +0
                   +5D
                   SA,W      ABB,B1              . ABB(J) = MOD(ABB(J)+1,5)
                   SZ,W      IDARK               . IDARK = 0
                   LA,W      COND,B1             . MOD(COND(J),1)
                   LQ        1
                   LLP,A     0,,AZERO
                   J         L2003               . MOD(COND(J),1)=1
                   LQ,X      77777
                   LA,W      IPLACE+2D
                   MATE      JJ
                   J         L2000A
                   J         L2000B              . IPLACE(2) = J THEN CONTINUE
       L2000A      MATE      NEG1
                   J         L2001               . IPLACE(2) <> -1 THEN 2001
       L2000B      LA,W      PROP+2D
                   JE        1D,L2003            . PROP(2) = 1 THEN 2003
       L2001       SLJ       SPEAK               . IT IS NOW PITCH DARK
                   +16D                   
                   LA        1                   . IDARK = 1
                   SA,W      IDARK
       .
       . DISPLAY ALL OBJECTS FOUND AT THE CURRENT LOCATION.
       . 
       L2003       LB,W      B1,JJ               
                   LA,W      IOBJ,B1
                   SA,W      I                   . I = OBJ(J)
       L2004       LA,A      0,,ANOT             . I = 0?
                   J         L2011               . YES
                   JE        6D,L2004A           . I = 6
                   JE        9D,L2004A           . I = 9
                   J         L2004B              . I <> 6 AND I <> 9
       L2004A      LA,W      IPLACE+10D,,APOS    . IPLACE(10) < 0
                   J         L2008               . YES    
       L2004B      LA,W      I
                   SA,W      ILK                 . ILK = I
                   LB,A      B1
                   LA,W      PROP,B1,ANOT        . PROP(I) <> 0?
                   J         L2004C              . NO
                   LA,W      ILK
                   A         100D
                   SA,W      ILK                 . ILK=ILK+100
       L2004C      LB,W      B1,ILK             
                   LA,W      BTEXT,B1       
                   SA,W      KK,,ANOT            . KK = BTEXT(ILK)       
                   J         L2008               . IF KK = 0 THEN 2008
       L2005       LQ,W      KK                  . CALC OFFSET INTO LLINE
                   M         LLSIZE
                   AQ        LLINE
                   SQ,A
                   LB,A      B2  
                   LA,W      LLLEN,B2            . SET UP TYPE PARAMS
                   SA,W      L2005$P1
                   LA        LLTEXT,B2
                   SA,W      L2005$P2
                   EXRN      ERTYPE$
       L2005$P1    +0
       L2005$P2    +0
                   LA,W      LLNEXT,B2,ANOT      . IS THERE MORE?
                   J         L2005A              . NO
                   RI,W      KK                  . KK=KK+1
                   J         L2005
       L2005A      TYPE      1D,CR
       L2008       LB,W      B1,I
                   LA,W      ICHAIN,B1           
                   SA,W      I                   . I = CHAIN(I)
                   J         L2004
       .
       . K = 1 MEANS ANY INPUT
       .
       L2012       LA,W      WD2                 . WD2 = A
                   SA,W      AA
                   SZ,W      TWOWDS              . TWOWDS = 0
                   J         L2021
       .
       . DISPLAY A MESSAGE RELATED TO YOUR LAST COMMAND.
       . LIKE, 'YOUR FEET ARE NOW WET'
       .
       . MESSAGE NUMBER IN K
       .
       L2009       LA        54D                 . K =54
                   SA,W      K
       L2010       LA,W      K                   . JSPK = K
                   SA,W      JSPK
       L5200       LA,W      JSPK                   
                   SA,W      L2010$P1
                   SLJ       SPEAK
       L2010$P1    +0                   
       .              
       L2011       SZ,W      JVERB
                   SZ,W      JOBJ
                   SZ,W      TWOWDS
       L2020       SLJ       GETIN
       L2020$P1    +0
       L2020$P2    +0
       L2020$P3    +0
       L2020$P4    +0                                     
                   LA,W      L2020$P1
                   SA,W      TWOWDS
                   LA,W      L2020$P2
                   SA,W      AA
                   LA,W      L2020$P3
                   SA,W      WD2
                   LA,W      L2020$P4
                   SA,W      BB
       .
                   LA        70D                 . K = 70 (YOUR FEET ARE NOW WET)
                   SA,W      K
                   LQ,X      77777
                   LA,W      AA
                   MATE      SQUIT               . A = 'QUIT'?
                   J         L2020C              . NO
                   HALT      0                   . YES, END GAME
       L2020C      MATE      SENTER              . A = 'ENTER'?
                   J         L2021               . NO
                   LA,W      WD2
                   MATE      SSTREAM             . WD2 = 'STREA'
                   J         L2020A              . NO, CHECK 'WATER'
                   J         L2010               . YES
       L2020A      MATE      SWATER              . WD2 = 'WATER'
                   J         L2020B              . NO
                   J         L2010               . YES
       L2020B      LA,W      TWOWDS,,AZERO       . TWOWDS = 0?
                   J         L2012               . NO                
       . COUNT # TIMES 'WEST' ENTERED. IF EXACTLY 10 TIMES, TELL PLAYER
       . THAT HE CAN ENTER 'W' INSTEAD.                   
       L2021       LA,W      AA
                   MATE      SWEST               . A = 'WEST'?
                   J         L2023               . NO
                   RI,W      IWEST               . YES IWEST = IWEST+1
                   JNE       10D,L2023           . IWEST <> 10 THEN 2023
                   SLJ       SPEAK               . YOU CAN TYPE W NOT WEST
                   +17D
       .
       . TRY TO FIND FIRST WORD IN WELL KNOWN WORD TABLE
       .                   
       L2023       LB        B1,1                . FOR I=1 TO 200
                   LQ,X      77777
       L2023A      LA,W      KTAB,B1
                   JE        9999D,L3000         . END OF TABLE?
                   LA,W      ATAB,B1             . ATAB(I) = A?
                   MATE      AA
                   J         L2024               . NO
                   J         L2025               . YES                               
       L2024       TBI       B1,200D
                   J         L2023A
       . 
       . NO END SENTINAL (ID = 99999) IN KTAB
       .                   
                   TYPE      E6LEN,ERR6          . OOPS!
                   HALT      4D
       .
       . GO TO THE APPROPRIATE ROUTINE TO PROCESS THE CLASS OF THE WORD.
       . ACTION WORDS ARE BETWEEN 0 AND 999, OJBECTS ARE BETWEEN 
       . 1000 AND 1999, NOISE WORDS ARE BETWEE 2000 AND 2999 AND
       . MISCELLANEOUS OTHER WORDS ARE >= 3000.
       .
       L2025       LA,W      KTAB,B1 
                   SA,W      L2025$P1
                   SLJ       MOD
       L2025$P1    +0
                   +1000D
                   SA,W      K                   . K = MOD(KTAB(I),1000)
                   LQ,W      KTAB,B1                   
                   ZA
                   D         1000D
                   AQ        1                   . KQ = KTAB(I)/1000+1
                   SQ,A
                   JG        4D,L2025A           . OUT OF RANGE
                   LB,A      B1
                   J,L       L2025$J,B1          . GOTO ... KQ
       .                   
       L2025A      TYPE      NNLEN,NONO
                   HALT      5D                   
       .
       L2025$J     +0
                   +L5014                        . 0-999
                   +L5000                        . 1000-1999
                   +L2026                        . 2000-2999
                   +L2010                        . 3000-3999
       .
       . PROCESS NOISE WORDS
       .                                                   
       L2026       LB,W      B1,K                
                   SB,W      B1,JVERB            . JVERB = K
                   LA,W      JSPKT,B1
                   SA,W      JSPK                . JSPK = JSPKT(JVERB)       
                   LA,W      TWOWDS,,AZERO       . TWOWDS = 0?
                   J         L2028               . NO
                   LA,W      JOBJ,,ANOT          . JOBJ = 0?
                   J         L2036               . YES
       L2027       LA,W      JVERB
                   JG        16D,L2026A          . JVERB > 16 THEN ERROR
                   LB,A      B1
                   J,L       L2026$J,B1          . GO TO APPROPRAITE ROUTINE       
       .                   
       L2026A      TYPE      E5LEN,ERR5
                   HALT      6D     
       . 
       . JUMP TABLE FOR 2000 SERIES WORDS WITH AN OBJECT
       .      
       L2026$J     +0
                   +L9000                        . TAKE
                   +L5066                        . DROP
                   +L3000                        . DUMMY
                   +L5031                        . OPEN
                   +L2009                        . HOLD
                   +L5031                        . CLOSE
                   +L9404                        . ON
                   +L9406                        . OFF
                   +L5081                        . STRIKE
                   +L5200                        . WAVE
                   +L5200                        . GO
                   +L5300                        . ATTACK
                   +L5506                        . POUR
                   +L5502                        . EAT
                   +L5504                        . DRINK
                   +L5505                        . RUB
       .
       . MOVE 2ND WORD TO 1ST WORD AND TRY AGAIN
       .
       L2028       LA,W      WD2
                   SA,W      AA                  . A= WD2
                   LA,W      SPACES
                   SA,W      BB                  . B = ' '
                   SZ,W      TWOWDS              . TWOWDS = 0
                   J         L2023
       .       
       . UNRECOGNIZED WORD. DISPLAY ONE OF 3 RANDOM REPSONSES.
       .
       L3000       LQ        60D                 . I DON'T KNOW THAT WORD
                   SLJ       RANDOM
                   +100D
                   JL        80D,L3000A
                   LQ        61D                 . WHAT?
       L3000A      SLJ       RANDOM
                   +100D
                   JL        80D,L3000B                   
                   LQ        13D                 . I DON'T UNDERSTAND THAT
       L3000B      SQ,W      L3000$P1
                   SLJ       SPEAK
       L3000$P1    +0                                             
                   RI,W      LTRUBL              . LTRUBL = LTRUBL + 1
                   JNE       3D,L2020            . LTRUBL <> 3 THEN 2020
       . SOMETHING ABOUT TRYING TO CATCH THE BIRD                   
                   LA,W      JJ
                   JNE       13D,L2032           . J <> 13 THEN 2032
                   LA,W      IPLACE+7D
                   JNE       13D,L2032           . OR IPLACE(7) <> 13 THEN 2020
                   LA,W      IPLACE+5D,,APOS
                   J         L2032               . OR IPLACE95) = -1 THEN 2020
                   SLJ       YES
                   +18D
                   +19D
                   +54D
                   +0
                   LA,W      $-1
                   SA,W      YEA
                   J         L2033
       . SOMETHING ABOUT ATTACKING THE SNAKE                   
       L2032       LA,W      JJ
                   JNE       19D,L2034           . J <> 19 THEN 2034
                   LA,W      PROP+11D,,AZERO
                   J         L2034               . OR PROP(11) <> 0 THEN 2034
                   LA,W      IPLACE+7D,,APOS
                   J         L2034               . OR IPLACE(7) = -1 THEN 2034
                   SLJ       YES
                   +20D
                   +21D
                   +54D
                   +0
                   LA,W      $-1
                   SA,W      YEA
                   J         L2033
       . SOMETHING ABOUT TRYING TO GET INTO THE CAVE
       L2034       LA,W      JJ
                   JNE       8D,L2035            . J <> 8 THEN 2035
                   LB,W      B1,GRATE
                   LA,W      PROP,B1,AZERO
                   J         L2035               . OR PROP(GRATE)<>0 THEN 2035
                   SLJ       YES
                   +62D
                   +63D
                   +54D
                   +0
                   LA,W      $-1
                   SA,W      YEA
       L2033       LA,W      YEA,,ANOT           . PLAYER ANSWERED NO?
                   J         L2011               . YES
                   J         L2020               . NO                   
       . MY WORD FOR HITTING SOMETHING WITH THE ROD IS STRIKE                                      
       L2035       LA,W      IPLACE+5D
                   AN,W      JJ,,ANOT            . IPLACE(5) <> J?
                   J         L2035A              . NO
                   LA,W      IPLACE+5D
                   JL        0,L2020             . AND IPLACE(5) < 0 THEN 2020
       L2035A      LA,W      JOBJ
                   JNE       5D,L2020            . JOBJ <> 5 THEN 2020
                   SLJ       SPEAK
                   +22D
                   J         L2020
       .                                      
       L2036       LA,W      JVERB
                   JG        16D,L2036A          . NOT IN RANGE
                   LB,A      B1
                   J,L       L2036$J,B1          . GO TO APPROPRIATE RTN
       .
       L2036A      TYPE      OLEN,OOPS
                   HALT      6D
       .
       . JUMP TABLE FOR 2000 SERIES WORDS WITHOUT AN OBJECT
       .
       L2036$J     +0
                   +L2037                        . TAKE
                   +L5062                        . DROP
                   +L5062                        . DUMMY
                   +L9403                        . OPEN
                   +L2009                        . HOLD
                   +L9403                        . CLOSE
                   +L9404                        . ON
                   +L9406                        . OFF
                   +L5062                        . STRIKE
                   +L5062                        . WAVE
                   +L5200                        . GO
                   +L5300                        . ATTACK
                   +L5062                        . POUR
                   +L5062                        . EAT
                   +L5062                        . DRINK
                   +L5062                        . RUB
       .                   
       . SOMETHING TO DO WITH PICKING STUFF UP
       L2037       LB,W      B1,JJ
                   LA,W      IOBJ,B1,ANOT        . IOBJ(J) = 0?
                   J         L5062               . YES
                   LB,A      B1
       L5312       LA,W      ICHAIN,B1,AZERO     . OR ICHAIN(IOBJ(J))<>0?
                   J         L5062               . YES
                   LB        B1,1                . FOR I=1 TO 3
                   LA,W      DSEEN,B1,AZERO      . DSEEN(I) = 0?
                   J         L5062               . NO
                   TBI       B1,3D               . BUMP I
                   J         L5312               . AND LOOP
                   LB,W      B1,JJ
                   LA,W      IOBJ,B1
                   SA,W      JOBJ                . JOBJ = IOBJ(J)
                   J         L2027  
       .       
       L5062       LA,W      BB                  . 2ND WORD OF REPLY
                   LQ,X      77777
                   MATE      SPACES
                   J         L5333               . BB <> ' ' THEN 5333
                   LA,W      AA
                   SA,W      WHAT                . ASK PLAYER WHAT OBJECT
                   TYPE      WLEN,WHAT
                   J         L2020            
       . DOUBLE WORD VERSION OF L5062
       L5333       LA,W      AA
                   SA,W      WHAT2
                   LA,W      BB
                   SA,W      WHAT2+1
                   TYPE      W2LEN,WHAT2
                   J         L2020            
       .
       . CHECK IF DARK. IF IT IS THEN THERE IS A 25% CHANCE OF
       . FALLING INTO A PIT.
       .
       L5014       LA,W      IDARK,,ANOT         . IDARK = 0?
                   J         L8                  . YES                  
                   SLJ       RANDOM
                   +100D
                   JG        25D,L8              . RAN > .25 THEN 8
       L5017       SLJ       SPEAK
                   23D                           . FELL INTO A PIT
                   TYPE      GOLEN,GAMEOVER
                   J         L2011
       .                                      
       L5000       LA,W      K
                   SA,W      JOBJ                . JOBJ = K
                   LA,W      TWOWDS,,AZERO       . TWOWDS = 0?
                   J         L2028               . NO
                   LB,W      B1,K
                   LA,W      JJ
                   AN,W      IPLACE,B1,ANOT      . K=IPLACE(K)?
                   J         L5004               . YES
                   LA,W      IPLACE,B1,APOS      . IPLACE(K)<0
                   J         L5004               . YES
                   LA,W      GRATE
                   AN,W      K,,AZERO            . K<>GRATE?
                   J         L502                . YES
                   LA,W      JJ
                   AN        1D,,ANOT            . J=1?
                   J         L5098               . YES
                   LA,W      JJ
                   AN        4D,,ANOT            . J=4?
                   J         L5098               . YES
                   LA,W      JJ
                   AN        7D,,ANOT            . J=7?
                   J         L5098               . YES
                   LA,W      JJ
                   JL        10D,L502            . J>9? NO SKIP
                   JL        15D,L5097           . J<15 THEN L5097
       L502        LA,W      BB
                   LQ,X      77777
                   MATE      SPACES              . B = SPACES
                   J         L5316               . NO
                   LA,W      AA                  . INS WRD INTO MSG
                   SA,W      SEEWHAT             
                   TYPE      SNLEN,SEENO
                   J         L2011
       L5316       LA,W      AA                  . INS 2 WRDS INTO MSG
                   SA,W      SEEWHAT2                                      
                   LA,W      BB
                   SA,W      SEEWHAT2+1
                   TYPE      SN2LEN,SEENO2
                   J         L2011
       .
       L5098       LA        49D
                   SA,W      K                   . K = 49
                   J         L5014
       .                   
       L5097       LA        50D
                   SA,W      K                   . K = 50
                   J         L5014
       .
       L5004       LA,W      K
                   SA,W      JOBJ                . JOBJ = K
                   LA,W      JVERB,,AZERO        . JVERB=0?
                   J         L2027               . NO
       .                   
       L5064       LA,W      BB
                   LQ,X      77777
                   MATE      SPACES              . B=' '?
                   J         5314                . NO
                   LA,W      AA                  . INS WRD INTO MSG
                   SA,W      WHATWHAT
                   TYPE      WDLEN,WHATDO
                   J         L2020
       L5314       LA,W      AA                  . INS 2 WRDS INTO MSG
                   SA,W      WHATWHAT2
                   LA,W      BB
                   SA,W      WHATWHAT2+1
                   TYPE      WD2LEN,WHATWHAT2
                   J         L2020
       .
       . CARRY
       .                                      
       L9000       LA,W      JOBJ
                   JE        18D,L2009           . JOBJ=18 THEN 2009
                   LB,A      B1
                   LA,W      IPLACE,B1
                   AN,W      JJ,,AZERO           . IPLACE(JOBJ)<>J?
                   J         L5200               . YES
       L9001       LB,W      B1,JOBJ
                   LA,W      IFIXED,B1,ANOT      . IPLACE(JOBJ)=0?
                   J         L9002               . YES
                   SLJ       SPEAK               . YOU CAN'T BE SERIOUS
                   +25D                                                
                   J         L2011
       L9002       LA,W      JOBJ
                   AN,W      BIRD,,AZERO         . JOBJ=BIRD?
                   J         L9004               . NO
                   LB,W      B1,ROD
                   LA,W      IPLACE,B1,ANEG      . IPLACE(ROD)<0?
                   J         L9003               . NO
                   SLJ       SPEAK               . BIRD AFRAID
                   +26D
                   J         L2011
       L9003       LA,W      IPLACE+4D,,APOS     . IPLACE(4)<0?
                   J         L9004               . YES
                   AN,W      JJ,,ANOT            . IPLACE(4)=J?                   
                   J         L9004               . YES
                   SLJ       SPEAK               . CANNOT CARRY BIRD
                   +27D
                   J         L2011
       L9004       LB,W      B1,JOBJ
                   LA,X      -1
                   SA,W      IPLACE,B1           . IPLACE(JOBJ)=-1
       L9005       LB,W      B1,JJ
                   LA,W      IOBJ,B1
                   AN,W      JOBJ,,AZERO         . IOBJ(J)<>JOBJ?
                   J         L9006               . YES
                   LB,W      B2,JOBJ
                   LA,W      ICHAIN,B2
                   SA,W      IOBJ,B1             . IOBJ(J)=ICHAIN(JOBJ)
                   J         L2009       
       L9006       LB,W      B1,JJ
                   LA,W      IOBJ,B1
                   SA,W      ITEMP               . ITEMP=IOBJ(J)       
       L9007       LB,W      B1,ITEMP
                   LA,W      ICHAIN,B1
                   AN,W      JOBJ,,ANOT          . ICHAIN(ITEMP)=JOBJ?
                   J         L9008               . YES
                   LA,W      ICHAIN,B1
                   SA,W      ITEMP               . ITEMP=ICHAIN(ITEMP)
                   J         L9007
       L9008       LB,W      B1,ITEMP
                   LB,W      B2,JOBJ
                   LA,W      ICHAIN,B2
                   SA,W      ICHAIN,B1           . ICHAIN(ITEMP)=ICHAIN(JOBJ)
                   J         L2009
       .
       . LOCK, UNLOCK, NO OBJECT YET
       .
       L9403       LA,W      JJ
                   JE        8D,L5105            . J=8 THEN 5105
                   JE        9D,L5105            . J=9 THEN 5105
       L5032       SLJ       SPEAK               . NOTHING HERE WITH A LOCK
                   +28D                   
                   J         L2011
       L5105       LA,W      GRATE
                   SA,W      JOBJ                . JOBJ=GRATE
                   J         L2027                   
       .
       . DISCARD OBJECT
       .                                                                
       L5066       LA,W      JOBJ
                   JE        18D,L2009           . JOBJ=18 THEN 2009
                   LB,A      B1
                   LA,W      IPLACE,B1,ANEG      . IPLACE(JOBJ)<0
                   J         L5200               . NO
       L5012       LA,W      JOBJ
                   AN,W      BIRD,,AZERO         . JOBJ=BIRD?
                   J         L9401               . NO
                   LA,W      JJ
                   JNE       19D,L9401           . J<>19 THEN 9401
                   LA,W      PROP+11D
                   JE        1D,L9401            . PROP(11)=1 THEN 9401
                   SLJ       SPEAK               . BIRD ATTACKS SNAKE
                   +30D
                   LA        1
                   SA,W      PROP+11D            . PROP(11)=1
       L5160       LB,W      B1,JOBJ
                   LB,W      B2,JJ
                   LA,W      IOBJ,B2
                   SA,W      ICHAIN,B1           . ICHAIN(JOBJ)=IOBJ(J)
                   SB,W      B1,IOBJ,B2          . IOBJ(J)=JOBJ
                   SB,W      B2,IPLACE,B1        . IPLACE(JOBJ)=J
                   J         L2011
       .
       L9401       SLJ       SPEAK               . OK
                   +54D                                      
                   J         L5160
       .
       . LOCK, UNLOCK OBJECT
       .                   
       L5031       LB,W      B1,KEYS
                   LA,W      IPLACE,B1,APOS      . IPLACE(KEYS)<0
                   J         L5031A              . YES
                   AN,W      JJ,,AZERO           . IPLACE(KEYS)<>J
                   J         L5200               . YES
       L5031A      LA,W      JOBJ
                   JNE       4D,L5102            . JOBJ<>4 THEN 5102
                   SLJ       SPEAK               . IT HAS NO LOCK
                   +32D
                   J         L2011
       L5102       LA,W      JOBJ
                   AN,W      KEYS,,AZERO         . JOBJ<>KEYS?
                   J         L5104               . YES                   
                   SLJ       SPEAK               . YOU CAN'T UNLOCK KEYS
                   +55D       
                   J         L2011
       L5104       LA,W      JOBJ
                   AN,W      GRATE,,ANOT         . JOBJ=GRATE?
                   J         L5107               . YES
                   SLJ       SPEAK               . DON'T KNOW TO LOCK/UNLOCK
                   +33D                   
                   J         L2011
       L5107       LA,W      JVERB
                   JE        4D,L5033            . JVERB=4 THEN 5033
                   LB,W      B1,GRATE
                   LA,W      PROP,B1,AZERO       . PROP(GRATE)<>0
                   J         L5034               . YES
                   SLJ       SPEAK               . ALREADY LOCKED
                   +34D
                   J         L2011
       L5034       SLJ       SPEAK               . GRATE IS NOW LOCKED
                   +35D                   
                   LB,W      B1,GRATE
                   SZ,W      PROP,B1             . PROP(GRATE)=0
                   SZ,W      PROP+8D             . PROP(8)=0
                   J         L2011
       L5033       LB,W      B1,GRATE                   
                   LA,W      PROP,B1,ANOT        . PROP(GRATE)=0
                   J         L5109               . YES
                   SLJ       SPEAK               . ALREADY UNLOCKED
                   +36D
                   J         L2011
       L5109       SLJ       SPEAK               . GRATE NOW UNLOCKED
                   +37D                   
                   LB,W      B1,GRATE
                   LA        1
                   SA,W      PROP,B1             . PROP(GRATE)=1
                   SA,W      PROP+8D             . PROP(8)=1
                   J         L2011
       .
       . LIGHT LAMP
       .       
       L9404       LA,W      IPLACE+2D
                   AN,W      JJ,,ANOT            . IPLACE(2)<>J?
                   J         L9404A              . NO
                   LA,W      IPLACE+2D,,ANEG     . IPLACE(2)<0?
                   J         L5200               . NO
       L9404A      LA        1
                   SA,W      PROP+2D             . PROP(2)=1
                   SZ,W      IDARK               . IDARK=0
                   SLJ       SPEAK               . LAMP IS NOW ON
                   +39D                   
                   J         L2011
       .
       . LAMP OFF
       .
       L9406       LA,W      IPLACE+2D
                   AN,W      JJ,,ANOT            . IPLACE(2)<>J?
                   J         L9406A              . NO
                   LA,W      IPLACE+2D,,ANEG     . IPLACE(2)<0?
                   J         L5200               . NO
       L9406A      SZ,W      PROP+2D             . PROP(2)=1
                   SLJ       SPEAK               . LAMP IS NOW OFF
                   +40D
                   J         L2011
       .
       . STRIKE
       .                   
       L5081       LA,W      JOBJ
       . JOBJ = 12 = FISSURE
                   JNE       12D,L5200           . JOBJ<>12 THEN 5200
                   LA        1
       . CREATE CRYSTAL BRIDGE                   
                   SA,W      PROP+12D            . PROP(12)=1
                   J         L2003
       .
       . ATTACK
       .                   
       L5300       LB        B1,1                . FOR ID=1 TO 3
       L5313       SB,W      B1,IID              . IID=ID
                   LA,W      DSEEN,B1,AZERO      . DSEEN(ID)<>0
                   J         L5307               . YES
                   TBI       B1,3D
                   J         L5313
                   LA,W      JOBJ
                   JE        0,L5062             . JOBJ=0 THEN 5062
                   AN,W      SNAKE,,ANOT         . JOBJ=SNAKE?
                   J         L5200               . YES
                   LA,W      JOBJ
                   AN,W      BIRD,,ANOT          . JOBJ=BIRD?
                   J         L5302               . YES
                   SLJ       SPEAK               . NOTHING TO ATTACK
                   +44D
                   J         L2011
       L5302       SLJ       SPEAK               . BIRD IS NOT DEAD
                   +45D                   
                   LB,W      B1,JOBJ
                   LA        300D
                   SA,W      IPLACE,B1           . IPLACE(JOBJ)=300
                   J         L9005
       .                   
       L5307       SLJ       RANDOM
                   +100D
                   JG        40D,L5309           . RAN>0.4 THEN 5309
                   LB,W      B1,IID
                   SZ,W      DSEEN,B1            . DSEEN(IID)=0
                   SZ,W      ODLOC,B1            . ODLOC(IID)=0
                   SZ,W      DLOC,B1             . DLOC(IID)=0
                   SLJ       SPEAK               . YOU KILLED A DWARF
                   +47D                   
                   J         L5311
       L5309       SLJ       SPEAK               . DWARF DODGES OUT OF WAY
                   +48D                   
       L5311       LA        21D
                   SA,W      K                   . K = 21
                   J         L5014
       .
       . EAT
       .                                      
       L5502       LB,W      B1,FOOD
                   LA,W      IPLACE,B1
                   AN,W      JJ,,ANOT
                   J         L5502A              . IPLACE(FOOD)=J THEN CONTINUE
                   LA,W      IPLACE,B1,ANEG
                   J         L5200               . IPLACE(FOOD)<0 THEN 5200
       L5502A      LA,W      PROP,B1,AZERO
                   J         L5200               . PROP(FOOD)<>0 THEN 5200
                   LA,W      JOBJ
                   AN,W      FOOD,,AZERO
                   J         L5200               . JOBJ<>FOOD THEN 5200
                   LA        1
                   SA,W      PROP,B1             . PROP(FOOD)=1
       L5501       LA        72D
                   SA,W      JSPK                . JSPK=72
                   J         L5200                                             
       .
       . DRINK
       .
       L5504       LB,W      B1,WATER
                   LA,W      IPLACE,B1
                   AN,W      JJ,,ANOT
                   J         L5504A              . IPLACE(WATER)=J THEN CONTINUE
                   LA,W      IPLACE,B1,ANEG
                   J         L5200               . IPLACE(WATER)<0 THEN 5200
       L5504A      LA,W      PROP,B1,AZERO
                   J         L5200               . PROP(WATER)<>0 THEN 5200
                   LA,W      JOBJ
                   AN,W      WATER,,AZERO
                   J         L5200               . JOBJ<>WATER THEN 5200
                   LA        1
                   SA,W      PROP,B1             . PROP(WATER>=1
                   LA        74D
                   SA,W      JSPK                . JSPK=74
                   J         L5200
       .
       . RUB
       .
       L5505       LA,W      JOBJ
                   AN,W      LAMP,,ANOT          . JOBJ<>LAMP?
                   J         L5200               . NO
                   LA        76D
                   SA,W      JSPK
                   J         L5200
       .
       . POUR
       .                   
       L5506       LA,W      JOBJ
                   AN,W      WATER,,ANOT         . JOBJ<>WATER?
                   J         L5506A              . NO
                   LA        78D
                   SA,W      JSPK                . JSPK=78
       L5506A      LB,W      B1,WATER
                   LA        1
                   SA,W      PROP,B1             . PROP(WATER)=1
                   J         L5200
       .
                   HALT      0
       . ++++++++++
       . MOD
       . 
       . PARAMETERS:
       .   VALUE
       .   MODULUS
       .
       . RETURNS:
       .   RESULT IN A
       . ++++++++++
       MOD         +0
                   LA,L      MOD                 . GET PARAM ADDR
                   LB,A      B7
                   A         2
                   SA,L      MOD$J
       .
                   LQ,W      0,B7                . GET VALUE
                   LA,W      1,B7                . GET MODULUS
                   SA,W      MOD$TEMP
                   ZA                            . DIVIDE TO GET RSLT IN A
                   D,W       MOD$TEMP
       MOD$J       J         $
       MOD$TEMP    RES       1D                                     
       .                    
       . ++++++++++
       . YES
       .
       . B1 = PARAM LIST
       . ++++++++++
       YES         +0
                   LA,L      YES                 . GET PTR TO PARAMS
                   LB,A      B7
                   A         4                   . SET RTRN ADDR
                   SA,L      YES$J
       .
                   LA,W      0,B7
                   SA,W      YES$P1
                   SLJ       SPEAK
       YES$P1      +0
                   SLJ       GETIN
                   +0
       IA1         +0
                   +0
                   +0                   
       .
                   LA,W      IA1                 . GET OPR RESPONSE
                   LQ,X      77777
                   MATE      NO1                 . = 'N'?
                   J         YES$CKNO            . NO
                   J         YES$1               . YES
       YES$CKNO    MATE      NO2                 . = 'NO'?                              
                   J         YES$YES             . NO, MUST BE 'YES'
                   J         YES$1               . YES            
       .                   
       YES$YES     LA        1                   . RETRUN 'YES' TO CALLER
                   SA,W      3,B7
                   LA,W      1,B7,ANOT           . Y = 0?
                   J         YES$J               . YES, WE'RE DONE
                   SA,W      YES$P2              . NO, SHOW MESSAGE
                   SLJ       SPEAK
       YES$P2      +0         
                   J         YES$J
       .                             
       YES$1       SZ,W      3,B7                . RETURN 'NO' TO CALLER
                   LA,W      2,B7,ANOT           . Z = 0?
                   J         YES$J               . YES, WE'RE DONE
                   SA,W      YES$P3              . NO, SHOW MESSAGE
                   SLJ       SPEAK
       YES$P3      +0                   
       .                   
       YES$J       J         $                          
       .
       NO1         +'N'
       NO2         +'NO'
       . ++++++++++
       . SPEAK
       .
       . B2 = PARAM LIST/IT
       . B3 = KKT
       . ++++++++++
       SPEAK       +0
                   SB,W      B2,SPEAK$B2         . SAVE REGISTERS
                   SB,W      B3,SPEAK$B3
       .                   
                   LA,L      SPEAK               . GET PTR TO PARAMS
                   LB,A      B2
                   A         1                   . SET RTRN ADDR
                   SA,L      SPEAK$J
       .
                   LB,W      B2,0,B2             . GET PARAM (IT)
                   LA,W      RTEXT,B2,ANOT       . RTEXT(IT) = 0?
                   J         SPEAK$RTRN          . YES, QUIT
                   LQ,A                          . MULT KKT BY 17D TO GET OFST
                   ZA
                   M         LLSIZE
                   AQ        LLINE               . CALC ADDR OF ITEM
                   SQ,A
       .                   
       SPEAK$LOOP  LB,A      B3
                   LA,W      LLLEN,B3            . TRANSFER LEN TO PARAM
                   SA,W      SPEAK$P1
                   LA        LLTEXT,B3           . XFER TEXT ADDR TO PARAM
                   SA,W      SPEAK$P2
                   EXRN      ERTYPE$             . WRITE LINE TO CONSOLE
       SPEAK$P1    +0
       SPEAK$P2    +0
                   LA,W      LLNEXT,B3,ANOT      . IS THERE MORE TEXT?
                   J         SPEAK$CR            . NO
                   SB,A      B3
                   A         LLSIZE              . BUMP KKT
                   J         SPEAK$LOOP          . LOOP
       .
       SPEAK$CR    TYPE      1D,CR
       .    
       SPEAK$RTRN  LB,W      B2,SPEAK$B2         . RESTORE REGISTERS 
                   LB,W      B3,SPEAK$B3                  
       SPEAK$J     J         $
       .
       SPEAK$B2    RES       1D
       SPEAK$B3    RES       1D 
       . ++++++++++
       . GETIN
       .
       . WAIT FOR OPERATOR INPUT, THEN PARSE IT INTO 2 WORDS.
       .
       . B2 = PARAMS ADDR
       . ++++++++++
       GETIN       +0
                   SB,W      B2,GI$B2
                   SB,W      B7,GI$B7
                   LA,L      GETIN               . GET RETURN ADDRESS
                   LB,A      B2
                   A         4
                   SA,L      GETIN$J
                   LA        1
                   SA,W      0,B2                . DEFAULT TO 2 TOKENS FOUND
       .
                   LA,W      SPACES              . CLEAR BFR
                   R         RPLYLEN,,ADV
                   SA,W      RPLYBFR
       .                   
                   ACCEPT    PROMPTLEN,PROMPT RPLYLEN,RPLYBFR 
                   SZ,W      GI$P1               . GET 1ST TOKEN
                   SLJ       NXTTKN
                   SA,W      1,B2                . RETURN IT
                   SQ,W      3,B2
                   SLJ       NXTTKN              . GET 2ND TOKEN
                   SA,W      2,B2                . RETURN IT
                   LQ,X      77777               . IS IT BLANK?
                   MATE      SPACES
                   J         GETIN$RTRN          . NO, WERE DONE
                   SZ,W      0,B2                . SHOW ONLY 1 TOKEN
       .
       GETIN$RTRN  LB,W      B2,GI$B2
                   LB,W      B7,GI$B7
       GETIN$J     J         $                  
       .
       NXTTKN      +0
                   EXRN      ERTOKEN$
       GI$P1       +0
                   +RPLYBFR                  
                   J,L       NXTTKN       
       .                   
       PROMPT      +'??  ^'
       PROMPTLEN   EQU       $-PROMPT
       RPLYBFR     RES       15D
       RPLYLEN     EQU       15D
       GI$B2       RES       1
       GI$B7       RES       1
       .
       . INFILE ERROR
       .
       INERR       LA,U      INFILE+DTF$FLAGS     . GET STATUS
                   OR,W      SPCZERO
                   SA,W      FILSTAT
                   TYPE      FELEN,FILERR
                   HALT      8D
       .
       . INFILE EOF
       .                   
       INEOF       TYPE      FEOFLEN,FILEOF
                   J         L1100
       .
       . ++++++++++
       . GET THE RECORD TYPE FROM THE CONFIG RECORD.
       . THE RECORD TYPE OCCUPIES THE FIRST WORD OR THE RECORD.
       . ++++++++++
       GETRTYPE    +0
                   LA,W      INREC
                   LQ,W      SPACES
                   DEC2INT
                   J,L       GETRTYPE
       . ++++++++++
       . PSUEDO RANDOM NUMBER GENERATOR
       .
       . PARAMETERS:
       .   RANGE
       .
       . RETURNS
       .   A RANDOM NUMBER IN A. 0 <= R < RANGE
       . ++++++++++
       RANDOM      +0
                   SQ,W      R$Q
                   LA,L      RANDOM              . GET RETURN ADDR
                   LB,A      B1
                   A         1                   . BUMP PAST PARAM
                   SA,L      RANDOM$J
       .                   
                   LA,W      R$SEED,,AZERO       . SEED INTIALIZED?
                   J         RANDOM$1            . YES
                   DAYCLOCK                      . NO, LOAD WITH CURRENT TIME
                   LSA       1                   . MAKE SURE IT IS POSITIVE
                   LRSA      1
                   SA,W      R$SEED                   
       .                   
       RANDOM$1    LQ,W      R$SEED              . CALC NEW SEED
                   M,W       R$M         
                   AQ        1
                   LSQ       1                   . MAKE SURE IT IS POSITIVE
                   LRSQ      1
                   SQ,W      R$SEED
       .
                   M,W       0,B1
                   LRSAQ     29D                 . LEAVE RANDOM # IN A
                   SQ,A
                   ANQ,W     0,B1,QNEG           . CHECK WITHIN RANGE
                   J         $,,STOP
       .           
                   LQ,W      R$Q                           
       RANDOM$J    J         $                                      
       .
       R$SEED      +0
       R$M         +134775813D                   . THE MAGIC MULTIPLIER
       R$Q         +0
       .
       CFG$ERR     TYPE      CFG$LEN,CFG$MSG
                   HALT      9D
       CFG$MSG     +'INVALID LINE TYPE DETECTED IN CONFIGURATION FILE ^'
       CFG$LEN     EQU       $-CFG$MSG                   
       .
       INFILE      DTFSD     'CAVE.DAT',5
      -                      DTF$INPUT+DTF$VARREC
      -                      20D,INREC
      -                      INBFR 
      -                      INEOF 
      -                      INERR
       INREC       RES       20D
       INBFR       RES       1024D
       .
       .
       . JUMP TABLE FOR THE DIFFERENT RECORD TYPES IN THE
       . CONFIGURATION FILE
       .
       RT$JUMP     +L1100
                   +L1004
                   +L1004
                   +L1013
                   +L1020
                   +L1004
                   +L1004
       .                   
       SPACES      +'     '
       FIVE9       +99999D
       SETUP       +0
       KEYS        +1D
       LAMP        +2D
       GRATE       +3D
       ROD         +5D
       BIRD        +7D
       NUGGET      +10D
       SNAKE       +11D
       FOOD        +19D
       WATER       +20D
       AXE         +21D
       I           +0
       IKIND       +0
       JKIND       +0
       LKIND       +0
       IDWARF      +0
       IFIRST      +0
       IWEST       +0
       ILONG       +0
       IDETAL      +0
       IDARK       +0
       ILK         +0
       JJ          +0                            . J IN FTN SOURCE
       K           +0
       KK          +0
       LL          +0                            . L IN FTN SOURCE
       LLL         +0                            . LL IN FTN SOURCE
       LOLD        +0
       LOC         +0                            . CURRENT LOCATION?
       ATTACK      +0                            . # DWARVES THAT ATTACK YOU
       DTOT        +0                            . # DWARVES IN ROOM
       STICK       +0                            . # DWARVES THAT HIT YOU
       LTRUBL      +0
       JVERB       +0
       JOBJ        +0
       TWOWDS      +0
       AA          RES       1D
       WD2         RES       1D
       BB          +0
       JSPK        +0
       YEA         +0
       ITEMP       +0
       IID         +0
       JSPKT       +0                            . JSPKT [1..100]
                   +24D
                   +29D
                   +0D
                   +31D
                   +0D
                   +31D
                   +38D
                   +38D
                   +42D
                   +42D
                   +43D
                   +46D
                   +77D
                   +71D
                   +73D
                   +75D
                   RES       101D-($-JSPKT)
       IPLT        +0                            . IPLT [1..100]
                   +3D                           
                   +3D
                   +8D
                   +10D
                   +11D
                   +14D
                   +13D
                   +9D
                   +15D
                   +18D
                   +19D
                   +17D
                   +27D
                   +28D
                   +29D
                   +30D
                   +0D
                   +0D
                   +3D
                   +3D
                   RES       101D-($-IPLT)
       IFIXT       +0                            . IFIXT [1..100]
                   +0
                   +0
                   +1
                   +0
                   +0
                   +1
                   +0
                   +1
                   +1
                   +0
                   +1
                   +1
                   +0
                   +0
                   +0
                   +0
                   +0
                   +0
                   +0
                   +0
                   RES       101D-($-IFIXT)
       DTRAV       RES       21D                 . DTRAV [1..20]                    
       ODLOC       RES       11D                 . ODLOC [1..10]
       DSEEN       RES       11D                 . DSEEN [1..10] 
       DLOC        RES       11D                 . DLOC [1..10]                  
       ICHAIN      RES       101D                . ICHAIN [1..100]                                     
       TK          RES       11D                 . TK[1..10]
       KEY         RES       301D                . KEY[1..300]
       . POINTERS TO SHORT LOCATION DESCRIPTIONS                  
       STEXT       RES       301D                . STEXT [1..300]
       . POINTERS TO GAME STATE DESCRIPTIONS
       BTEXT       RES       201D                . BTEXT [1..200]
       . POINTERS TO HINTS AND EVENTS DESCRIPTIONS
       RTEXT       RES       101D                . RTEXT [1..100]                   
       . POINTERS TO LONG LOCATION DESCRIPTIONS
       LTEXT       RES       301D                . LTEXT [1..300]
       TRAVEL      RES       401D                . TRAVEL [1..400]
       . HOLDS NUMBERS OF WELL KNOWN WORDS
       KTAB        RES       201D                . KTAB [1..200]
       . HOLDS WELL KNOWN WORDS
       ATAB        RES       201D                . ATAB [1..200]
       IPLACE      RES       101D                . IPLACE [1..100]
       IFIXED      RES       101D                . IFIXED [1..100]
       COND        RES       301D                . COND [1..300]
       ABB         RES       301D                . ABB [1..300]
       IOBJ        RES       301D                . IOB [1..300]
       PROP        RES       101D                . PROP [1..100]
       .
       . AN ARRAY OF LOCATION RELATED TEXT
       .
       LLITEM      EQU       0
       LLNEXT      EQU       0                   . INDEX OF NEXT LLINE ITEM
       LLLEN       EQU       1                   . LENGTH OF CRNT LLINE ITEM
       LLTEXT      EQU       2                   . TEXT OF CRNT LLINE ITEM
       LLSIZE      EQU       17D
       LLINE       RES       601D*LLSIZE         . LLINE [1..600, 0..16]
       .
       LLCRNT      RES       1D
       .       
       FILERR      +'FILE ERROR'
       FILSTAT     +0
       CR          +'    ^'
       FELEN       EQU       $-FILERR                   
       .
       FILEOF      +'END OF FILE   ^'
       FEOFLEN     EQU       $-FILEOF
       .
       TMLERR      +'TOO MANY LINES^' 
       TMLLEN      EQU       $-TMLERR
       .
       TM3ERR      +'TOO MANY 3 RECORDS'
                   +'    ^'
       TM3LEN      EQU       $-TM3ERR                   
       .
       TMWERR      +'TOO MANY WORDS'
                   +'    ^'
       TMWLEN      EQU       $-TMWERR
       .
       CFGLOADED   +'CONFIGURATION SUCCESSUFLLY LOADED'
                   +'    ^'
       CLLEN       EQU       $-CFGLOADED
       . 
       INITDONE    +'INITIALIZATION COMPLETE'
                   +'    ^'
       IDLEN       EQU       $-INITDONE
       .
       DTHREAT     +'THERE ARE'
       DTNUM       +' '
                   +'THREATENING LITTLE DWARVES IN THE ROOM WITH YOU.'
                   +'    ^'
       DTLEN       EQU       $-DTHREAT
       .
       DKNIFE      +' '
                   +'OF THEM THROW KNIVES AT YOU!'
                   +'    ^'                   
       DKLEN       EQU       $-DKNIFE
       .
       DHIT        +' '
                   +'OF THEM HIT YOU!'
                   +'    ^'
       DHLEN       EQU       $-DHIT
       .
       GAMEOVER    +'GAME IS OVER'
                   +'    ^'
       GOLEN       EQU       $-GAMEOVER
       .
       ERR5        +'ERROR 5'
                   +'    ^'
       E5LEN       EQU       $-ERR5
       .
       ERR6        +'ERROR 6'
                   +'    ^'
       E6LEN       EQU       $-ERR6
       .
       NONO        +'NO NO'
                   +'    ^'
       NNLEN       EQU       $-NONO
       .
       OOPS        +'OOPS!'
                   +'    ^'
       OLEN        EQU       $-OOPS                   
       .
       WHAT        +'      WHAT?'
                   +'    ^'
       WLEN        EQU       $-WHAT
       .                                                                                                               
       WHAT2       +'           WHAT?'
                   +'    ^'
       W2LEN       EQU       $-WHAT2
       .
       SEENO       +'I SEE NO  '
       SEEWHAT     +'     '
                   +' HERE.'
                   +'    ^'
       SNLEN       EQU       $-SEENO
       .                   
       SEENO2      +'I SEE NO  '
       SEEWHAT2    +'          '
                   +' HERE.'
                   +'    ^'
       SN2LEN      EQU       $-SEENO2
       .
       WHATDO      +'WHAT DO YOU WANT TO DO WITH THE '
       WHATWHAT    +'     '
                   +'?   ^'
       WDLEN       EQU       $-WHATDO                   
       .                                      
       WHATDO2     +'WHAT DO YOU WANT TO DO WITH THE '
       WHATWHAT2   +'          '
                   +'?   ^'
       WD2LEN      EQU       $-WHATDO2                   
       .                                      
       ZERO        +0                                                                                                               
       SPCZERO     +'    0'
       SPC         +'     '
       NEG1        -1
       SENTER      +'ENTER'
       SSTREAM     +'STREA'
       SWATER      +'WATER'
       SWEST       +'WEST'
       SQUIT       +'QUIT'
                   END                                      