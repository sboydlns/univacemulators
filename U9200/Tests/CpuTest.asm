        START
        DS    CL5
*
* TEST1 - load, store and add half word 
* TEST2 - subtract half word
* TEST3 - compare half word
* TEST4 - logical instructions TM, NI, OI
* TEST5 - logical instructions CLI, MVI, AI
* TEST6 - character instructions MVC, CLC, TR, MVN
* TEST7 - character instructins NC, OC
* TEST8 - decimal instructions PACK, UNPK
* TEST9 - decimal instructions CP, AP
* TEST10 - decimal instructions SP
* TEST11 - decimal instructions MP, DP, ZAP
* TEST12 - decimal instructions MVO, ED
* TEST13 - branch and link BAL
* TEST255 - execute all tests
*        
TEST    DS    XL1
        DS    CL12        
*
* Address of first instruction executed after load (STRT)
*        
        DC    XL2'0080'
        DS    XL2
*
* Instruction executed after Clear - Start
*
        B     RSET
*                
        DS    CL102
*
* Halt and wait for operator to enter test # at address 5
*      
STRT    HPR   X'0001'  
        MVI   ALLT,0      
LOOP    CLI   TEST,1
        BEQ   TST1
        CLI   TEST,2
        BEQ   TST2
        CLI   TEST,3
        BEQ   TST3
        CLI   TEST,4
        BEQ   TST4
        CLI   TEST,5
        BEQ   TST5
        CLI   TEST,6
        BEQ   TST6
        CLI   TEST,7
        BEQ   TST7
        CLI   TEST,8
        BEQ   TST8
        CLI   TEST,9
        BEQ   TST9
        CLI   TEST,10
        BEQ   TSTA
        CLI   TEST,11
        BEQ   TSTB
        CLI   TEST,12
        BEQ   TSTC
        CLI   TEST,13
        BEQ   TSTD
        CLI   TEST,255
        BEQ   ALL
* Invalid test #       
        B     STRT
*
* Do all tests
*
ALL     MVI   ALLT,1
        MVI   TEST,1
        B     LOOP        
*
* TEST 1 - Test all half word instructions for basic functionality
*
* Load a value and make sure it loaded properly
TST1    LH    8,ONE                   
        CH    8,ONE                   
        BEQ   T11
        HPR   X'0101'                  R8 <> X'0001' - FAIL
        B     STRT
* Store a value and make sure it stored properly        
T11     STH   8,TMP1                   
        LH    9,TMP1                   
        CH    9,ONE
        BEQ   T12
        HPR   X'0102'                  R9 <> X'0001' - FAIL
        B     STRT
* Add two numbers checking all possible condition codes                
T12     LH    8,ONE
        AH    8,TWO
        BP    T13
        HPR   X'0103'                  R8 NOT POSITIVE - FAIL
        B     STRT
T13     CH    8,THRE
        BEQ   T14
        HPR   X'0104'                  R8 <> X'0003' - FAIL
        B     STRT
T14     LH    8,ONE
        AH    8,NEG1
        BZ    T15
        HPR   X'0105'                  R8 <> X'0000' - FAIL
        B     STRT
T15     LH    8,ZERO
        AH    8,NEG1
        BM    T16
        HPR   X'0106'                  R8 NOT NEGATIVE - FAIL
        B     STRT
T16     CH    8,NEG1
        BEQ   T17
        HPR   X'0107'                  R8 <> X'FFFF' - FAIL      
        B     STRT
T17     LH    8,MAXH
        AH    8,ONE
        BOV   T18
        HPR   X'0108'                  NO OVERFLOW - FAIL
        B     STRT
T18     CH    8,MINH
        BEQ   T19
        HPR   X'0109'                  R8 <> X'8000' - FAIL
        B     STRT                
T19     LH    8,MINH                   
        AH    8,NEG1
        BOV   T1A
        HPR   X'010A'                  NO OVERFLOW - FAIL
        B     STRT
T1A     CH    8,MAXH
        BEQ   T1B
        HPR   X'010B'                  R8 <> X'07FFF' - FAIL
        B     STRT
T1B     B     DONE
*
* TEST 2 - Subtract two numbers checking all possible condition codes
*                        
TST2    LH    8,TWO
        SH    8,ONE
        BP    T21
        HPR   X'0201'                  NOT POSITIVE - FAIL
        B     STRT
T21     CH    8,ONE
        BEQ   T22
        HPR   X'0202'                  NOT = X'0001' - FAIL
        B     STRT        
T22     LH    8,ONE
        SH    8,ONE
        BZ    T23
        HPR   X'0203'                  NOT = ZERO - FAIL
        B     STRT
T23     LH    8,ZERO
        SH    8,ONE
        BM    T24
        HPR   X'0204'                  NOT NEGATIVE - FAIL
T24     CH    8,NEG1
        BEQ   T25
        HPR   X'0205'                  NOT = X'FFFF' - FAIL
        B     STRT
T25     LH    8,MAXH
        SH    8,NEG1
        BOV   T26
        HPR   X'0206'                  NOT OVERFLOW - FAIL
        B     STRT
T26     CH    8,MINH
        BEQ   T27
        HPR   X'0207'                  NOT = X'8000' - FAIL
        B     STRT
T27     LH    8,MINH
        SH    8,ONE
        BOV   T28
        HPR   X'0208'                  NOT OVERFLOW - FAIL
T28     CH    8,MAXH                   
        BEQ   T29
        HPR   X'0209                   NOT = X'07FFF' - FAIL
        B     STRT
T29     LH    8,NEG8
        SH    8,NEG9
        BC    14,T2A               
        HPR   X'0209'                  OVERFLOW FAIL
T2A     B     DONE
*
* TEST 3 - Check all combinations of compare half word
*
TST3    LH    8,ONE
        CH    8,TWO
        BLT   T31
        HPR   X'0301'                  NOT < - FAIL
        B     STRT
T31     LH    8,TWO
        CH    8,ONE
        BGT   T32                      
        HPR   X'0302'                  NOT > - FAIL
        B     STRT
T32     LH    8,ONE
        CH    8,ONE
        BEQ   T33                      NOT = - FAIL
        HPR   X'0303'
        B     STRT
T33     B     DONE                
*
* TEST 4 - Check logical instructions TM, NI, OI
*
TST4    TM    XFF,X'55'
        BC    1,T41
        HPR   X'0401'                  TEST FOR ALL ONES MATCH FAILED
        B     STRT
T41     TM    ZERO,X'55'
        BC    8,T42
        HPR   X'0402'                  TEST FOR NO ONES MATCH FAILED        
        B     STRT
T42     TM    X50,X'55'
        BC    4,T43
        HPR   X'0403'                  TEST FOR SOME ONES MATCH FAILED
        B     STRT
T43     LH    8,NEG1
        STH   8,TMP1
        NI    TMP1,X'55'
        BC    4,T44
        HPR   X'0404'                  NOT SOME ONES - FAILED
        B     STRT
T44     CLI   TMP1,X'55'
        BEQ   T45
        HPR   X'0405'                  NOT X'55' - FAILED
        B     STRT
T45     NI    TMP1,X'AA'
        BC    8,T46
        HPR   X'0406'                  NOT ALL ZEROS - FAILED
        B     STRT
T46     CLI   TMP1,X'00'
        BEQ   T47
        HPR   X'0407'                  NOT = ZERO - FAILED
        B     STRT
T47     LH    8,ZERO
        STH   8,TMP1
        OI    TMP1,X'00'
        BC    8,T48
        HPR   X'0408'                  NOT ALL ZERO - FAILED
        B     STRT
T48     CLI   TMP1,X'00'
        BEQ   T49
        HPR   X'0409'                  NOT = ZERO - FAILED
        B     STRT
T49     OI    TMP1,X'55'
        BC    4,T4A
        HPR   X'040A'                  NOT SOME ONES - FAILED
        B     STRT
T4A     CLI   TMP1,X'55'
        BEQ   T4B
        HPR   X'040B'                  NOT = X'55' - FAILED
        B     STRT
T4B     B     DONE 
*
* TEST 5 - CLI, MVI, AI
*
TST5    MVI   TMP1,X'80'
        CLI   TMP1,X'80'
        BEQ   T51
        HPR   X'0501'                  NOT = X'80' FAIL                         
        B     STRT
T51     CLI   TMP1,X'7F'
        BGT   T52
        HPR   'X0502'                  NOT > - FAIL
        B     STRT
T52     CLI   TMP1,X'81'
        BLT   T53
        HPR   X'0503'                  NOT < - FAIL
        B     STRT
T53     LH    8,ZERO
        STH   8,TMP1
        AI    TMP1,X'01'
        BP    T54
        HPR   X'0504'                  NOT POSITIVE - FAIL
        B     STRT
T54     LH    8,TMP1
        CH    8,ONE
        BEQ   T55
        HPR   X'0505'                  NOT = ONE - FAIL
        B     STRT
T55     AI    TMP1,X'FF'
        BZ    T56
        HPR   X'0506'                  NOT ZERO - FAIL
        B     STRT
T56     LH    8,TMP1
        CH    8,ZERO
        BEQ   T57
        HPR   X'0507'                  NOT = ZERO - FAIL
        B     STRT
T57     AI    TMP1,X'FF'
        BM    T58
        HPR   X'0508'                  NOT NEGATIVE - FAIL
        B     STRT
T58     LH    8,TMP1
        CH    8,NEG1
        BEQ   T59
        HPR   X'0509'
        B     STRT
T59     B     DONE
*
* TEST 6 - MVC, CLC, TR, MVN
*
TST6    MVC   TMP2,ABC
        CLC   TMP2,ABC
        BEQ   T61
        HPR   X'0601'                  NOT = - FAIL
        B     STRT
T61     CLC   TMP2,BCD
        BLT   T62
        HPR   X'0602'                  NOT < - FAIL
        B     STRT
T62     CLC   BCD,TMP2
        BGT   T63
        HPR   X'0603'                  NOT > - FAIL
        B     STRT
T63     MVC   TMP2,XLT
        TR    TMP2,ABC
        CLC   TMP2,ABC
        BEQ   T64
        HPR   X'0604'                  NOT = - FAIL
        B     STRT
T64     MVC   TMP2(5),MVN1
        MVN   TMP2(5),MVN2
        CLC   TMP2(5),MVN2
        BEQ   T65
        HPR   X'0605'
        B     STRT
T65     B     DONE 
*
* TEST7 - character instruction NC, OC
*
TST7    MVC   TMP1,ZERO
        NC    TMP1,ONE
        BC    8,T71
        HPR   X'0701'                  NOT ALL ZERO - FAIL
        B     STRT
T71     CLC   TMP1,ZERO
        BEQ   T72
        HPR   X'0702'                  NOT = ZERO - FAIL
        B     STRT        
T72     MVC   TMP1,ONE
        NC    TMP1,THRE
        BC    4,T73
        HPR   X'0703'                  NOT SOME ONES - FAIL
        B     STRT
T73     CLC   TMP1,ONE
        BEQ   T74
        HPR   X'0704'                  NOT = 1 - FAIL
        B     STRT
T74     MVC   TMP1,ZERO
        OC    TMP1,ZERO
        BC    8,T75
        HPR   X'0705'                  NOT ALL ZERO - FAIL
        B     STRT  
T75     CLC   TMP1,ZERO
        BEQ   T76
        HPR   X'0706'                  NOT = ZERO - FAIL
        B     STRT
T76     MVC   TMP1,ZERO
        OC    TMP1,ONE
        BC    4,T77
        HPR   X'0707'                  NOT SOME ONES - FAIL
T77     CLC   TMP1,ONE
        BEQ   T78
        HPR   X'0708'                  NOT = 1 - FAIL
        B     STRT
T78     B     DONE
*
* TEST8 - decimal instructions PACK, UNPK
*
TST8    PACK  TMP3,ZONE
        CLC   TMP3,P3
        BEQ   T81
        HPR   X'0801'
        B     STRT
T81     PACK  TMP4,ZONE
        CLC   TMP4,P4
        BEQ   T82
        HPR   X'0802'
        B     STRT
T82     PACK  TMP5,ZONE
        CLC   TMP5,P5
        BEQ   T83
        HPR   X'0803'
        B     STRT
T83     UNPK  TMP5,P4
        CLC   TMP5,Z5
        BEQ   T84
        HPR   X'0804'
        B     STRT
T84     UNPK  TMP6,P4
        CLC   TMP6,Z6
        BEQ   T85
        HPR   X'0805'
        B     STRT
T85     UNPK  TMP7,P4
        CLC   TMP7,Z7
        BEQ   T86
        HPR   X'0806'
        B     STRT
T86     B     DONE 
*
* TEST9 - decimal instructions CP, AP
*
TST9    CP    P3,P3
        BEQ   T91
        HPR   X'0901'                  NOT = - FAIL
        B     STRT
T91     CP    P3H,P3
        BGT   T92
        HPR   X'0902'                  NOT > - FAIL
        B     STRT        
T92     CP    P3L,P3
        BLT   T93
        HPR   X'0903'                  NOT < - FAIL
        B     STRT
T93     MVC   TMP3,PONE
        AP    TMP3,PTWO
        BP    T94
        HPR   X'0904'                  NOT POSITIVE - FAIL
        B     STRT
T94     CP    TMP3,PTHR
        BEQ   T95                  
        HPR   X'0905'                  NOT = 3 - FAIL
        B     STRT
T95     MVC   TMP3,PONE
        AP    TMP3,PNG1
        BZ    T96
        HPR   X'0906'                  NOT ZERO - FAIL
        B     START
T96     MVC   TMP3,PZRO
        AP    TMP3,PNG1
        BM    T97
        HPR   X'0907'                  NOT NEGATIVE - FAIL
        B     STRT
T97     CP    TMP3,PNG1
        BEQ   T98
        HPR   X'0908'                  NOT = -1 - FAIL
        B     STRT
T98     MVC   TMP3,PMAX
        AP    TMP3,PONE
        BOV   T99
        HPR   X'0909'                  NOT OVERFLOW - FAIL
        B     STRT
T99     CP    TMP3,PZRO
        BEQ   T9A
        HPR   X'090A'                  NOT = ZERO - FAIL
        B     STRT
T9A     MVC   TMP3,PMIN
        AP    TMP3,PNG1
        BOV   T9B
        HPR   X'090B'                  NOT OVERFLOW - FAIL
        B     STRT
T9B     CP    TMP3,PZRO
        BEQ   T9C
        HPR   X'090C'                  NOT = ZERO - FAIL
        B     STRT
T9C     B     DONE 
*
* TEST10 - decimal instructions SP
*
TSTA    MVC   TMP3,PTWO
        SP    TMP3,PONE
        BP    TA1
        HPR   X'0A01'                  NOT POSITIVE - FAIL
        B     STRT
TA1     CP    TMP3,PONE
        BEQ   TA2
        HPR   X'0A02'                  NOT = 1 - FAIL
        B     STRT
TA2     MVC   TMP3,PONE
        SP    TMP3,PONE
        BZ    TA3
        HPR   X'0A03'                  NOT ZERO - FAIL
        B     STRT
TA3     MVC   TMP3,PZRO
        SP    TMP3,PONE
        BM    TA4
        HPR   X'0A04'                  NOT NEGATIVE - FAIL
        B     STRT
TA4     CP    TMP3,PNG1
        BEQ   TA5
        HPR   X'0A05'                  NOT = -1 - FAIL
        B     STRT
TA5     MVC   TMP3,PMAX
        SP    TMP3,PNG1
        BOV   TA6
        HPR   X'0A06'                  NOT OVERFLOW - FAIL
        B     STRT
TA6     CP    TMP3,PZRO
        BEQ   TA7
        HPR   X'0A07'                  NOT = ZERO - FAIL
        B     STRT
TA7     MVC   TMP3,PMIN
        SP    TMP3,PONE
        BOV   TA8
        HPR   X'0A08'                  NOT OVERFLOW - FAIL
        B     STRT
TA8     CP    TMP3,PZRO
        BEQ   TA9
        HPR   X'0A08'                  NOT = ZERO - FAIL
        B     STRT
TA9     B     DONE
*
* TEST11 - decimal instructions MP, DP, ZAP
*
TSTB    ZAP   TMP5,PTWO
        MP    TMP5,PTWO
        CP    TMP5,PFOR
        BEQ   TB1
        HPR   X'0B01'                  NOT = 4 - FAIL
        B     STRT
TB1     ZAP   TMP5,PTWO
        MP    TMP5,PNG1
        CP    TMP5,PNG2
        BEQ   TB2
        HPR   X'0B02'                  NOT = -2 - FAIL
        B     STRT        
TB2     ZAP   TMP5,PFOR
        DP    TMP5,PTWO
        CP    TMP5(2),PTWO
        BEQ   TB3
        HPR   X'0B03'                  NOT = 2 - FAIL
        B     STRT
TB3     CP    TMP5+2(2),PZRO
        BEQ   TB4
        HPR   X'0B04'                  NOT = ZERO - FAIL
        B     STRT
TB4     ZAP   TMP5,PFOR
        DP    TMP5,PNG2+2(2)
        CP    TMP5(2),PNG2+2(2)
        BEQ   TB5
        HPR   X'0B05'                  NOT = -2 - FAIL
        B     STRT
TB5     CP    TMP5+2(2),PZRO
        BEQ   TB6
        HPR   X'0B06'                  NOT = ZERO - FAIL
        B     STRT                                      
TB6     B     DONE
*
* TEST12 - decimal instructions MVO, ED
*
TSTC    ZAP   TMP5,PZRO
        MVO   TMP5,PONE
        NC    TMP5+3(1),X0F
        CP    TMP5,PTEN    
        BEQ   TC1
        HPR   X'0C01'                  NOT = - FAIL
        B     STRT
TC1     MVC   TMP8,ED1
        ED    TMP8,P4 
        BP    TC2
        HPR   X'0C02'                  NOT POSITIVE - FAIL
        B     STRT
TC2     CLC   TMP8,ERS1
        BEQ   TC3                      INCORRECT RESULT - FAIL
        HPR   X'0C03'
        B     STRT
TC3     MVC   TMP8,ED1
        ED    TMP8,P4NG
        BM    TC4
        HPR   X'0C04'                  NOT NEGATIVE - FAIL
        B     STRT
TC4     CLC   TMP8,ERS2
        BEQ   TC5
        HPR   X'0C05'                  INCORRECT RESULT - FAIL
        B     STRT
TC5     MVC   TMP8,ED1
        ED    TMP8,PZR3
        BZ    TC6                      
        HPR   X'0C06'                  NOT ZERO - FAIL
        B     STRT
TC6     CLC   TMP8,ERS3
        BEQ   TC7
        HPR   X'0C07'                  INCORRECT RESULT - FAIL
        B     STRT
TC7     MVO   P4,P4                    TEST OVERLAPPING FIELDS
        CLC   P4,P4O
        BEQ   TC8
        HPR   X'0C07'
        B     STRT        
TC8     B     DONE 
*
* TEST13 - brank and link BAL
*
TSTD    BAL   8,BAL1
        CLI   TMP1,X'55'
        BEQ   TD1
        HPR   X'0D01'
        B     STRT        
TD1     B     DONE        
        
BAL1    MVI   TMP1,X'55'
        B     0(,8)
                       
* Test successful, notify operator
DONE    CLI   ALLT,1
        BNE   STOP
        AI    TEST-1,1
        B     LOOP
STOP    HPR   X'0002'                  TEST SUCCESSFUL        
        B     STRT
*
* Test Clear - Start (reset) functionality
*
RSET    HPR   X'7FFF'
        B     STRT         

ZERO    DC    XL2'0'        
ONE     DC    XL2'1'
TWO     DC    XL2'2'
THRE    DC    XL2'3'
NEG1    DC    XL2'FFFF'
MAXH    DC    XL2'7FFF'
MINH    DC    XL2'8000'
TMP1    DS    XL2
X50     DC    XL1'50'
XFF     DC    XL1'FF'
ABC     DC    CL16'ABCDEF0123456789'
BCD     DC    CL16'BCDEF01234567899'
XLT     DC    XL16'000102030405060708090A0B0C0D0E0F'
MVN1    DC    CL5'ABCDE'
MVN2    DC    CL5'EDCBA'
TMP2    DS    CL16
ZONE    DC    CL5'12345'
TMP3    DS    XL2
TMP4    DS    XL3
TMP5    DS    XL4
TMP6    DS    XL5
TMP7    DS    XL6
TMP8    DS    CL8
P3      DC    XL2'345F'
P3H     DC    XL2'456F'
P3L     DC    XL2'234F'
P4      DC    XL3'12345F'
P4O     DC    XL3'2345FF'
P4NG    DC    XL3'12345D'
P5      DC    XL4'0012345F'      
Z5      DC    CL4'2345'
Z6      DC    CL5'12345'
Z7      DC    CL6'012345'
PONE    DC    XL2'001C'
PTWO    DC    XL2'002C'
PTHR    DC    XL2'003C'
PFOR    DC    XL4'0000004C'
PTEN    DC    XL4'0000010C'
PZRO    DC    XL2'000C'
PZR3    DC    XL3'00000C'
PNG1    DC    XL2'001D'
PNG2    DC    XL4'0000002D'
PMAX    DC    XL2'999C'
PMIN    DC    XL2'999D'
X0F     DC    XL1'0F'
ED1     DC    CL1' '
        DC    XL2'2020'
        DC    CL1','
        DC    XL3'202120'
        DC    CL1'-'
ERS1    DC    CL8' 12,345 ' 
ERS2    DC    CL8' 12,345-' 
ERS3    DC    CL8'      0 ' 
ALLT    DC    XL1'0'
NEG8    DC    XL2'8002'
NEG9    DC    XL2'8000'        
               
        END   STRT