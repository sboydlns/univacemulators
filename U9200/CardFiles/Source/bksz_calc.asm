*****************************************************************
*
*       Charlie Gibbs' optimal block size calculator.
*
*****************************************************************

* THIS PROGRAM, GIVEN A RECORD SIZE AND KEY LENGTH, WILL CALCULATE
* THE NUMBER OF BLOCKS PER TRACK, RECORDS PER TRACK, AND UNUSED BYTES
* REMAINING ON THE TRACK, FOR ALL POSSIBLE BLOCKING FACTORS.
* IF MORE THAN ONE BLOCKING FACTOR GIVES THE SAME NUMBER OF BLOCKS
* PER TRACK, ONLY THE LARGEST ONE IS PRINTED.
* INPUT CARDS HAVE THE FOLLOWING FORMAT:
*         1- 5--RECORD SIZE (1 TO THE MAIMUM ALLOWED FOR THE DEVICE)
*         7- 9--KEY LENGTH (0 TO 255)
*        11-12--DEVICE TYPE:
*              11--8411 (OR IBM 2311)
*              14--8414 (OR IBM 2314)
*              24--8424
*              25--8425 (SAME AS 8424)
* ALL FIELDS ARE CHECKED FOR NUMERIC AND VALIDITY--RECORD SIZE AND
*  KEY LENGTH MAY BE EITHER ZERO OR BLANK FILLED.  IF AN FIELD IS
*  INVALID, THE CARD INFORMATION IS PRINTED WITH APPROPRIATE
*  DIAGNOSTICS AND BYPASSED.
* ANY NUMBER OF CARDS MAY BE PROCESSED IN ONE RUN.

DBSZ    START 0

        EXTRN READ                     Reader I/O entry point
        EXTRN PRNT

        ENTRY FOF
        ENTRY RBUF        
        ENTRY REOF

        USING *,0
        USING *,1

CARD    DS    CL80                     My card buffer        
RBUF    DS    CL80                     IOCS reader buffer
CADR    DC    YL2(CARD)
CAD5    DC    YL2(CARD+5)
CAD6    DC    YL2(CARD+6)
CAD9    DC    YL2(CARD+9)
DVTA    DC    YL2(DVTB)
DVTN    DC    YL2(DVTE)
ONE     DC    YL2(1)
ELVN    DC    YL2(11)
SPC5    DC    CL5' '
HKLN    DC    CL12'KEY LENGTH ='
HNOK    DC    CL16'NO KEY'
BDDV    DC    CL16'INVALID DEVICE T'
        DC    CL3'YPE'
BDRS    DC    CL16'INVALID RECORD S'
        DC    CL3'IZE'        
BDRM    DC    CL16'RECORD WILL NOT '
        DC    CL12'FIT ON TRACK' 
BDKL    DC    CL16'INVALID KEY LENG'
        DC    CL2'TH' 
BDKM    DC    CL16'KEY LENGTH IS TO'
        DC    CL7'O LARGE'                      
P999    DC    XL3'99999C'              99999 PACKED 
P255    DC    XL2'255C'                255 PACKED 
P1      DC    XL1'1C'                  1 PACKED      
P500    DC    XL3'05000C'              5000 PACKED
        
        DS    0H
        
BEGN    OPEN  READ
        OPEN  PRNT
*******************************************************************
*
*       C O N T R O L   C A R D   P R O C E S S I N G   L O O P
*
*******************************************************************
NXTC    GET   READ,CARD                READ A CONTROL CARD
        MVI   OF,1                     RESET ALL INDICATORS
        MVI   HD1+95,C'0'
        MVI   ERRS,0
        MVC   HD1+62(5),CARD           RECORD SIZE
        CLC   CARD(5),SPC5             IS RECORD SIZE OMITTED?
        BC    8,BRSZ                   YES--ERROR
* CHECK RECORD SIE FIELD FOR VALID NUMERICS (LEADING BLANKS ACCEPTED)
        LH    8,CADR                   R8 SCANS RECORD SIZE FIELD
        MVI   LDBL,1                   SET LEADING-BLANK INDICATOR
L1      CLI   0(8),C' '                BLANK?
        BC    7,TDR                    NO--BLANKS ARE NO LONGER VALID
        CLI   LDBL,1                   ARE LEADING BLANKS STILL VALID
        BC    8,NDR                    YES--SKIP IT
        BC    15,BRSZ                  ERROR--EMBEDDED BLANK
TDR     MVI   LDBL,0                   BLANKS ARE NO LONGER ACCEPTABLE
        CLI   0(8),C'0'                VALID NUMERIC DIGIT?
        BC    4,BRSZ                   NO--ERROR
NDR     AH    8,ONE                    NEXT DIGIT
        CH    8,CAD5                   HAVE ALL DIGITS BEEN CHECKED?
        BC    4,L1                     NO--CHECK THE NEXT ONE
        PACK  RCSZ,CARD(5)             VALID NUMERIC RECORD SIZE
        MVC   HD1+61(6),ED5
        ED    HD1+61(6),RCSZ           RECORD SIZE TO HEADING LINE           
L2      CLI   HD1+62,C' '              LEFT-JUSTIFY SIGNIFICANT DIGIT
        BC    7,RCS0
        MVC   HD1+62(5),HD1+63
        BC    15,L2
RCS0    ZAP   RCSZ,RCSZ                IS RECORD SIZE ZERO?
        BC    7,GKLN                   NO--IT'S VALID SO FAR
BRSZ    OI    ERRS,1                   RECORD SIZE ZERO OR NOT NUMERIC
* VALIDATE AND OBTAIN KEY LENGTH--SIMILAR TO RECORD SIZE ROUTINE ABOVE
GKLN    MVC   HD1+69(12),HKLN 
        MVC   HD1+82(3),CARD+6         KEY LENGTH
        LH    8,CAD6
        MVI   LDBL,1                   CHECK FOR LEADING BLANKS
L3      CLI   0(8),C' '                
        BC    7,TDK
        CLI   LDBL,1
        BC    8,NDK
        BC    15,BKLN 
TDK     MVI   LDBL,0
        CLI   0(8),C'0'
        BC    4,BKLN
NDK     AH    8,ONE
        CH    8,CAD9
        BC    4,L3
        PACK  KLEN,CARD+6(3)
        ZAP   KLEN,KLEN                IS A KEY LENGTH GIVEN?
        BC    8,KLN0                   NO
        MVC   HD1+81(4),ED3
        ED    HD1+81(4),KLEN
L4      CLI   HD1+82,C' '              LEFT-JUSTIFY SIGNIFICANT DIGITS
        BC    7,DVTP
        MVC   HD1+82(3),HD1+83
        BC    15,L4
KLN0    MVC   HD1+69(16),HNOK
        BC    15,DVTP
BKLN    OI    ERRS,2                   KEY LENGTH IS NOT NUMERIC
* VALIDATE DEVICE TYPE AND SET UP DEVICE-DEPENDENT CONSTANTS
DVTP    MVC   HD1+43(2),CARD+10        DEVICE TYPE TO HEADING LINE
        LH    8,DVTA                   R8 SCANS DEVICE TYPE TABLE
DVCK    CLC   CARD+10(2),0(8)          HIT ON DEVICE TYPE?
        BC    8,GDDV                   YES--SET UP CONSTANTS                                                                                      
        AH    8,ELVN                   INCREMENT TABLE POINTER
        CH    8,DVTN                   END OF TABLE?
        BC    4,DVCK                   NO--TRY NEXT ENTRY
        MVC   LINE(19),BDDV
        OI    ERRS,X'10'               SET DEVICE TYPE ERROR BIT
        BAL   13,CLER
        CNTRL PRNT,SP,0,2
        PUT   PRNT,LINE                PRINT ERROR MESSAGE
        MVC   LINE,BLNK
        ZAP   MAX,P999                 SUPRESS RECORD SIZE CHECK
        BC    15,RSCK
GDDV    ZAP   MAX,2(3,8)               TRACK CAPACITY IN BYTES
        ZAP   FCTR,5(2,8)              GAP TOLERANCE FACTOR
        ZAP   KGAP,7(2,8)              KEY GAP LENGTH
        ZAP   DGAP,9(2,8)              ALL OTHER RECORD OVERHEAD
* PRINT ERROR MESSAGE IF RECORD SIZE IS NOT NUMERIC
RSCK    TM    ERRS,1                   IS RECORD SIZE VALID?
        BC    8,RSMX                   YES--CHECK AGAINST MAXIMUM
        MVC   LINE(19),BDRS
        BAL   13,CLER
        CNTRL PRNT,SP,0,2
        PUT   PRNT,LINE
        MVC   LINE,BLNK
        BC    15,KLCK                  BYPASS MAXIMUM RECORD SIZE CHK
* DETERMINE WHETHER AT LEAST ONE RECORD WILL FIT ON A TRACK
RSMX    CP    RCSZ,MAX                 IS RECORD SIZE TOO LARGE?
        BC    13,KLCK                  NO
        MVC   LINE(28),BDRM
        OI    ERRS,4                   SET RECORD TOO LARGE BIT
        BAL   13,CLER
        CNTRL PRNT,SP,0,2
        PUT   PRNT,LINE
        MVC   LINE,BLNK
* PRINT ERROR MESSAGE IF KEY LENGTH IS NOT NUMERIC
KLCK    TM    ERRS,2                   IS KEY LENGTH VALID?
        BC    8,KLMX                   YES        
        MVC   LINE(18),BDKL
        BAL   13,CLER
        CNTRL PRNT,SP,0,2
        PUT   PRNT,LINE
        MVC   LINE,BLNK
        BC    15,ERCK                  BYPASS MAXIMUM-KEY-LENGTH CHECK
* CHECK THAT KEY LENGTH DOES NOT EXCEED 255
KLMX    CP    KLEN,P255                IS KEY LENGTH TOO LARGE?
        BC    13,ERCK                  NO
        MVC   LINE(23),BDKM
        OI    ERRS,8                   SET KEY-LENGTH-TOO-LARGE BIT
        BAL   13,CLER
        CNTRL PRNT,SP,0,2
        PUT   PRNT,LINE
        MVC   LINE,BLNK
* END OF VALIDATION ROUTINES--IF THERE WERE ANY ERRORS, NO NOT PROCESS
ERCK    CLI   ERRS,0                   WERE THERE ANY ERRORS?
        BC    7,NXTC                   YES--READ NEXT CARD
*****************************************************************
*
*       START OF ACTUAL BLOCK SIZE CALCULATIONS
*
*****************************************************************
        ZAP   NREC,P1                  START WITH UNBLOCKED RECORDS
* START OF LOOP FOR EACH BLOCK SIZE
L5      ZAP   DL,RCSZ
        MP    DL,NREC                  DATA LENGTH (BLOCK SIZE)
        ZAP   KD,KLEN                  COMPUTE KEY-GAP-DATA LENGTH
        BC    8,NOKY                   NO KEY FIELD SPECIFIED
        AP    KD,KGAP                  ADD LENGTH OF THE KEY GAP
NOKY    AP    KD,DL                    TOTAL KEY-GAP-DATA LENGTH
        CP    KD,MAX                   HAS BLOCK BECOME TOO LARGE?
        BC    2,LSTL                   YES--WE'RE FINISHED
        ZAP   WORK,KLEN
        AP    WORK,DL                  TOTAL KEY AND DATA LENGTH                
        MP    WORK,FCTR                NUMBER OF VARIABLE GAP BYTES
        AP    WORK,P500                ROUND TO NEAREST BYTE
        MVN   WORK+13(1),WORK+15
        ZAP   L,WORK(14)
        AP    L,KD
        AP    L,DGAP                   TOTAL EFFECTIVE RECORD LENGTH
        ZAP   WORK,MAX                 NUMBER OF BYTES AVAILABLE FOR
        SP    WORK,KD                   ALL BLOCKS EXCEPT LAST ONE
        DP    WORK,L                   NUMBER OF BLOCKS EXCEPT LAST 1
        ZAP   N,WORK(13)
        AP    N,P1                     NUMBER OF BLOCKS PER TRACK
        ZAP   R,MAX                    MAXIMUM NUMBER OF BYTES PER TR
        ZAP   WORK,N                   COMPUTE THE TOTAL NUMBER OF
        SP    WORK,P1                   BYTES REQAUIRED BY ALL BLOCKS
        MP    WORK,L                    ON THE TRACK
        AP    WORK,KD
        SP    R,WORK                   NUMBER OF BYTES REMAINING ON TR
        ZAP   WORK,N
        MP    WORK,NREC                NUMBER OF RECORDS PER TRACK
        CLC   LINE,BLNK                IS THIS THE FIRST TIME THROUGH?
        BC    8,SAMN                   YES--DON'T PRINT YET
        CP    N,OLDN                   IF SMALLER BLOCKING FACTOR GA
        BC    8,SAMN                   SAME BLOCKS/TRACK, DON'T PRINT
        BAL   13,CLER
        PUT   PRNT,LINE                PRINT PREVIOUS LINE
        MVC   LINE,BLNK
SAMN    MVC   LINE+13(4),ED3
        ED    LINE+13(4),NREC          BLOCKING FACTOR
        MVC   LINE+26(6),ED5
        ED    LINE+26(6),DL+2          BLOCK SIZE
        MVC   LINE+43(4),ED3
        ED    LINE+43(4),N             NUMBER OF BLOCKS PER TRACK
        MVC   LINE+60(4),ED3
        ED    LINE+60(4),WORK+14       NUMBER OF RECORDS PER TRACK
        MVC   LINE+75(6),ED5
        ED    LINE+75(6),R             NUMBER OF BYTES REMAINING ON TR
        ZAP   OLDN,N                   SAVE NUMBER OF BLOCKS PER TRACK
        AP    NREC,P1                  INCREMENT BLOCK FACTOR
        BC    15,L5                    COMPUTE FOR NEW BLOCKING FACTOR
* THE LARGEST POSSIBLE BLOCK HAS BEEN FOUND--PRINT LAST LINE
LSTL    PUT   PRNT,LINE
        MVC   LINE,BLNK
        BC    15,NXTC                  GO FOR ANOTHER CARD
* END OF CONTROL CARD TERMINATE RUN
REOF    CLOSE READ
        CLOSE PRNT
        MSG   X'1FFF'                                
*
* PAGE OVERFLOW ROUTINE
*
CLER    CLI   OF,1
        BC    7,0(,13)
        AI    HD1+94,1                 INCREMENT ONE-DIGIT PAGE NUMBER
        CNTRL PRNT,SK,7
        CNTRL PRNT,SP,0,2
        PUT   PRNT,HD1
        MVI   OF,0
        CLI   ERRS,0                   IS THIS AN ERROR MESSAGE PRINT?
        BC    7,0(,13)                 YES--DON'T PRINT HD3 OR HD3
        PUT   PRNT,HD2
        CNTRL PRNT,SP,0,2
        PUT   PRNT,HD3
        BC    15,0(,13)
* PAGE OVERFLOW DETECTION
FOF     MVI   OF,1
        BC    15,0(,14)
*******************************************************************
*
*       S T O R A G E
*
*******************************************************************
HD1     DS    0CL132                   FIRST HEADING LINE
        DC    CL16'DISC BLOCK SIZE '
        DC    CL16'CALCULATION'                
        DC    CL16'DEVICE = 84'
        DC    CL16'RECORD SIZE ='
        DC    CL16' '
        DC    CL9' '
        DC    CL16'PAGE'
        DC    CL16' '
        DC    CL11' '
HD2     DS    0CL132                   SECOND HEADING LINE
        DC    CL12' '
        DC    CL16'BLOCKING'
        DC    CL14'BLOCK'
        DC    CL16'BLOCKS'
        DC    CL1' '
        DC    CL16'RECORDS'
        DC    CL2' '
        DC    CL16'BYTES'
        DC    CL16' '
        DC    CL16' '
        DC    CL7' '
HD3     DS    0CL132                   THIRD HEADING LINE
        DC    CL13' '
        DC    CL15'FACTOR'
        DC    CL13'SIZE'
        DC    CL16'PER TRACK'
        DC    CL1' '                
        DC    CL16'PER TRACK'
        DC    CL1' '                
        DC    CL16'REMAINING'
        DC    CL16' '
        DC    CL16' '
        DC    CL9' '
BLNK    DC    CL1' '                   MUST PRECEED LINE
LINE    DS    0CL132                   PRINT LINE IMAGE
        DC    CL16' '                         
        DC    CL16' '                      
        DC    CL16' '                      
        DC    CL16' '                      
        DC    CL16' '                      
        DC    CL16' '                      
        DC    CL16' '                      
        DC    CL16' '                      
        DC    CL4' '
ED3     DC    XL4'40202120'
ED5     DC    XL6'402020202120'
OF      DS    CL1                      PAGE OVERFLOW INDICATOR
ERRS    DS    CL1                      ERROR INDICATORS
LDBL    DS    CL1                      LEADING BLANK CONTROL
RCSZ    DS    CL3                      RECORD SIZE
KLEN    DS    CL2                      KEY LENGTH
DL      DS    CL5                      DISC DATA LENGTH (BLOCK SIZE)
KD      DS    CL3                      KEY-GAP-DATA LENGTH
NREC    DS    CL2                      BLOCKING FACTOR
WORK    DS    CL16                     WORK AREA
L       DS    CL3                      TOTAL EFFECTIVE RECORD LENGTH
FCTR    DS    CL2                      VARIABLE GAP TOLERANCE FACTOR
MAX     DS    CL3                      TRACK CAPACITY IN BYTES
N       DS    CL2                      NUMBER OF BLOCKS PER TRACK
OLDN    DS    CL2                      PREVIOUS VALUE OF N
R       DS    CL3                      BYTES REMAINING ON TRACK
KGAP    DS    CL2                      KEY GAP LENGTH
DGAP    DS    CL2                      TOTAL OTHER RECORD OVERHEAD
* DEVICE-DEPENDENT CONSTANT TABLE--11 BYTES PER ENTRY
*       0- 1--DEVICE TYPE CODE AS READ FROM CARD
*       2- 4--TRACK CAPACITY IN BYTES
*       5- 6--VARIABLE GAP TOLERANCE FACTOR X 10,000
*       7- 8--KEY GAP LENGTH
*       9-10--TOTAL OTHER RECORD OVERHEAD
DVTB    EQU   *,11                    
        DC    CL2'11'                  8411
        DC    XL3'03625C'
        DC    XL2'500C'
        DC    XL2'020C'
        DC    XL2'061C'
        DC    CL2'14'                  8414
        DC    XL3'07294C'
        DC    XL2'435C'
        DC    XL2'045C'
        DC    XL2'101C'
        DC    CL2'24'                  8424
        DC    XL3'07294C'
        DC    XL2'476C'
        DC    XL2'045C'
        DC    XL2'101C'
        DC    CL2'25'                  8425--SAME AS 8424
        DC    XL3'07294C'
        DC    XL2'476C'
        DC    XL2'045C'
        DC    XL2'101C'
DVTE    EQU   *        
                                
        END   BEGN