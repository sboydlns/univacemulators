TT      START 0

        USING *,0
        USING *,1
	
        EXTRN PRNT
        ENTRY FOF

BCW     EQU   X'54'                    ADDRESS OF CHANNEL 5 BCW
DEV     EQU   X'A8'                    MUX CHAN 5 DEV 0 ADDRESS

TST     EQU   *	
        OPEN  PRNT

        BAL   14,PW                    PRINT THE WELCOME MSG
*
*       DISPLAY HALT CODE 1 AND WAIT FOR OPERATOR TO ENTER TEST #
*        
LOOP    MSG   X'0001',REPLY
TEST    EQU   LOOP+7
*       MAKE SURE TEST # IS VALID
        CLI   TEST,0                   TEST # = 0
        BC    8,IT                     YES, DISPLAY ERROR
        CLI   TEST,16                  TEST # > 16?               
        BC    2,IT                     YES, DISPLAY ERROR
*       GET THE ADDRESS OF THE TEST ROUTINE        
        MVC   THW+1(1),TEST            GET THE TEST #
        LH    9,THW                    MULTIPLY BY 2
        AH    9,THW
        AH    9,JTA                    ADD ADDR OF JUMP TABLE
        LH    9,0(,9)                  GET ADDR OF TEST RTN FROM TABLE
*       ECHO THE TEST # TO THE PRINTER        
        MVI   TLN,C' '
        CLI   TEST,9                   IF TEST # > 9 THEN
        BC    13,L1
        MVI   TLN,C'1'                 SET FIRST DIGIT TO 1
        LH    8,THW                    DECR TEST # BY 10
        SH    8,HW10
        STH   8,THW
        MVC   TEST(1),THW+1
L1      EQU   *
        MVC   TLN+1(1),TEST            SET THE (POSSIBLY) SECOND DIGIT
        OI    TLN+1,X'F0'
        CNTRL PRNT,SP,0,2
        BAL   14,BCL
        MVC   PBFR(18),TL
        PUT   PRNT,PBFR
        CNTRL PRNT,SP,0,1 
*       EXECUTE THE TEST 
        BC    15,0(,9)                 GO DO THE TEST
        
        CLOSE PRNT
        
DONE    EQU   *        
        MSG   X'1FFF'
        BC    15,DONE
*
*       WRITE THE WELCOME MESSAGE TO THE PRINTER
*
PW      EQU   *
        STH   14,PWR+2                 SAVE RTN ADDR
        CNTRL PRNT,SK,7                FORM FEED
        BAL   14,BCL                   CLEAR PRINT BUFFER
        CNTRL PRNT,SP,0,2              PRINT W1 WITH BLANK LINE AFTER
        MVC   PBFR(35),W1
        PUT   PRNT,PBFR
        CNTRL PRNT,SP,0,1              PRINT AVAILABLE TEST LIST
        BAL   14,BCL
        MVC   PBFR(20),W2
        PUT   PRNT,PBFR
        BAL   14,BCL
        MVC   PBFR(10),W3
        PUT   PRNT,PBFR
        BAL   14,BCL
        MVC   PBFR(11),W4
        PUT   PRNT,PBFR
        BAL   14,BCL
        MVC   PBFR(10),W5
        PUT   PRNT,PBFR
        BAL   14,BCL
        MVC   PBFR(11),W6
        PUT   PRNT,PBFR
        BAL   14,BCL
        MVC   PBFR(19),W7
        PUT   PRNT,PBFR
        BAL   14,BCL
        MVC   PBFR(25),W8
        PUT   PRNT,PBFR
        BAL   14,BCL
        MVC   PBFR(24),W9
        PUT   PRNT,PBFR
        BAL   14,BCL
        MVC   PBFR(26),W10
        PUT   PRNT,PBFR
        BAL   14,BCL
        MVC   PBFR(25),W11
        PUT   PRNT,PBFR
        BAL   14,BCL
        MVC   PBFR(20),W12
        PUT   PRNT,PBFR
        BAL   14,BCL
        MVC   PBFR(26),W13
        PUT   PRNT,PBFR
        BAL   14,BCL
        MVC   PBFR(14),W14
        PUT   PRNT,PBFR
        BAL   14,BCL
        MVC   PBFR(12),W15
        PUT   PRNT,PBFR
        BAL   14,BCL
        MVC   PBFR(22),W16
        PUT   PRNT,PBFR
        BAL   14,BCL
        MVC   PBFR(20),W17
        PUT   PRNT,PBFR
        BAL   14,BCL
        MVC   PBFR(11),W18
        PUT   PRNT,PBFR
        CNTRL PRNT,SP,1,2
        BAL   14,BCL
        MVC   PBFR(85),W99
        PUT   PRNT,PBFR
        CNTRL PRNT,SP,0,1
PWR     BC    15,0                     MODIFIED WITH RTN ADDR ABOVE
*
*       CLEAR THE PRINTER BUFFER TO SPACES
*
BCL     EQU   *
        MVI   PBFR,C' '
        MVC   PBFR+1(131),PBFR
        BC    15,0(,14)
*
*       CLEAR THE BCW TO ZEROS
*
BCWC    EQU   *
        MVI   BCW,0
        MVI   BCW+1,0
        MVC   BCW+2(2),TBA
        BC    15,0(,14)        
*
*       PRINT INVALID TEST # ERROR
*
IT      EQU   *
        BAL   14,BCL
        MVC   PBFR(14),ITE
        PUT   PRNT,PBFR
        BC    15,LOOP
*
*       FORM OVERFLOW HANDLER
*
FOF     EQU   *
        STH   14,FOFR+2                SAVE RETURN ADDRESS
        CNTRL PRNT,SK,7                PAGE EJECT
FOFR    BC    15,0                     RETURN TO ORIGINAL CALLER
*
*       'TEST' COMMAND
*
T1      EQU   *
        BAL   14,BCL
        MVC   PBFR(20),T1L
        PUT   PRNT,PBFR
        BAL   13,WAIT                  WAIT FOR I/O TO COMPLETE
        BC    7,PCS                    ERROR
        BAL   14,BCWC                  CLEAR THE BCW
        XIOF  0,DEV                    ISSUE TEST COMMAND
        BC    7,PCS                    ERROR
        BAL   13,PS                    DUMP STATUS BYTE
        BC    15,LOOP        
*
*       'SENSE' COMMAND
*        
T2      EQU   *
        BAL   14,BCL
        MVC   PBFR(21),T2L
        PUT   PRNT,PBFR
        BAL   13,WAIT                  WAIT FOR I/O TO COMPLETE
        BC    7,PCS                    ERROR
        BAL   14,BCWC                  CLEAR THE BCW
        XIOF  X'04',DEV                ISSUE SENSE COMMAND
        BC    7,PCS                    ERROR
        BAL   13,WAIT                  WAIT FOR COMMAND TO COMPLETE
        BC    7,PCS                    ERROR
        BAL   13,PS                    PRINT THE STATUS BYTE
        BAL   13,PN                    DUMP SENSE BYTES
        BC    15,LOOP        
*
*       'READ FORWARD' COMMAND
*
T3      EQU   *
        BAL   14,BCL
        MVC   PBFR(20),T3L
        PUT   PRNT,PBFR
        BAL   13,WAIT                  WAIT FOR I/O TO COMPLETE
        BC    7,PCS                    ERROR
        BAL   14,BCWC                  CLEAR THE BCW
        OI    BCW,1                    SET BLOCK SIZE TO 256
        XIOF  X'02',DEV                ISSUE READ COMMAND
        BC    7,PCS                    ERROR
        BAL   13,WAIT                  WAIT FOR COMMAND TO COMPLETE
        BC    7,PCS                    ERROR
        TM    ST,X'01'                 EXCEPTION SET?
        BC    14,T3D                   NO, ALL OK
        BAL   14,BCL                   PROBABLY EOF
        MVC   PBFR(3),EOF
        PUT   PRNT,PBFR
        BC    15,LOOP
T3D     EQU   *
        MVC   PBFR,TBFR                DUMP 1ST 132 BYTES OF BUFFER
        PUT   PRNT,PBFR
        BC    15,LOOP        
*
*       'WRITE' COMMAND
*
T4      EQU   *
        BAL   14,BCL
        MVC   PBFR(21),T4L
        PUT   PRNT,PBFR
*       FILL A 256 BYTE BLOCK WITH DATA
        MVC   TBFR(1),BN+1
        MVC   TBFR+1(255),TBFR
*       BUMP THE CHARACTER USED TO FILL THE BUFFER        
        AI    BN,1
        CLI   BN+1,C'9'                > 9?
        BC    13,T4A                   NO
        MVI   BN+1,C'1'                YES, RESET TO 1
T4A     EQU *
        BAL   13,WAIT                  WAIT FOR I/O TO COMPLETE
        BAL   14,BCWC                  CLEAR THE BCW
        OI    BCW,1                    SET LENGTH TO 256 BYTES
        XIOF  X'01',DEV                ISSUE THE WRITE COMMAND
        BC    7,PCS                    ERROR
        BAL   13,WAIT                  WAIT FOR COMMAND TO COMPLETE
        BC    7,PCS                    ERROR
        BC    15,LOOP
*
*       'READ BACKWARD' COMMAND
*
T5      EQU   *
        BAL   14,BCL
        MVC   PBFR(29),T5L
        PUT   PRNT,PBFR
        BAL   13,WAIT                  WAIT FOR I/O TO COMPLETE
        BC    7,PCS                    ERROR
        BAL   14,BCWC                  CLEAR THE BCW
        OI    BCW,1                    SET BLOCK SIZE TO 256
        OI    BCW,X'40'                SET REVERSE DIRECTION BIT
        MVC   BCW+2(2),TBE             SET HIGH BUFFER ADDRESS
        XIOF  X'0C',DEV                ISSUE READ BACKWARD COMMAND
        BC    7,PCS                    ERROR
        BAL   13,WAIT                  WAIT FOR COMMAND TO COMPLETE
        BC    7,PCS                    ERROR
        TM    ST,X'01'                 EXCEPTION SET?
        BC    14,T5D                   NO, ALL OK
        BAL   14,BCL                   PROBABLY EOF
        MVC   PBFR(3),EOF
        PUT   PRNT,PBFR
        BC    15,LOOP
T5D     EQU   *
        MVC   PBFR,TBFR                DUMP 1ST 132 BYTES OF BUFFER
        PUT   PRNT,PBFR
        BC    15,LOOP        
*
*       'FORWARD SPACE BLOCK' COMMAND
*
T6      EQU   *
        BAL   14,BCL
        MVC   PBFR(35),T6L
        PUT   PRNT,PBFR
        BAL   13,WAIT                  WAIT FOR I/O TO COMPLETE
        BC    7,PCS                    ERROR
        BAL   14,BCWC                  CLEAR THE BCW
        XIOF  X'37',DEV                ISSUE FORWARD SPACE COMMAND
        BC    7,PCS                    ERROR
        BAL   13,WAIT                  WAIT FOR COMMAND TO COMPLETE
        BC    7,PCS                    ERROR
        TM    ST,X'01'                 EXCEPTION SET?
        BC    14,LOOP                  NO, ALL OK
        BAL   14,BCL                   PROBABLY EOF
        MVC   PBFR(3),EOF
        PUT   PRNT,PBFR
        BC    15,LOOP
*
*       'FORWARD SPACE FILE' COMMAND
*
T7      EQU   *
        BAL   14,BCL
        MVC   PBFR(34),T7L
        PUT   PRNT,PBFR
        BAL   13,WAIT                  WAIT FOR I/O TO COMPLETE
        BC    7,PCS                    ERROR
        BAL   14,BCWC                  CLEAR THE BCW
        XIOF  X'3F',DEV                ISSUE FORWARD SPACE COMMAND
        BC    7,PCS                    ERROR
        BAL   13,WAIT                  WAIT FOR COMMAND TO COMPLETE
        BC    7,PCS                    ERROR
        TM    ST,X'01'                 EXCEPTION SET?
        BC    14,LOOP                  NO, ALL OK
        BAL   14,BCL                   PROBABLY EOF
        MVC   PBFR(3),EOF
        PUT   PRNT,PBFR
        BC    15,LOOP
*
*       'BACK SPACE BLOCK" COMMAND
*
T8      EQU   *
        BAL   14,BCL
        MVC   PBFR(32),T8L
        PUT   PRNT,PBFR
        BAL   13,WAIT                  WAIT FOR I/O TO COMPLETE
        BC    7,PCS                    ERROR
        BAL   14,BCWC                  CLEAR THE BCW
        XIOF  X'27',DEV                ISSUE BACK SPACE COMMAND
        BC    7,PCS                    ERROR
        BAL   13,WAIT                  WAIT FOR COMMAND TO COMPLETE
        BC    7,PCS                    ERROR
        TM    ST,X'01'                 EXCEPTION SET?
        BC    14,LOOP                  NO, ALL OK
        BAL   14,BCL                   PROBABLY EOF
        MVC   PBFR(3),EOF
        PUT   PRNT,PBFR
        BC    15,LOOP
*
*       'BACK SPACE FILE' COMMAND
*
T9      EQU   *
        BAL   14,BCL
        MVC   PBFR(31),T9L
        PUT   PRNT,PBFR
        BAL   13,WAIT                  WAIT FOR I/O TO COMPLETE
        BC    7,PCS                    ERROR
        BAL   14,BCWC                  CLEAR THE BCW
        XIOF  X'2F',DEV                ISSUE BACK SPACE COMMAND
        BC    7,PCS                    ERROR
        BAL   13,WAIT                  WAIT FOR COMMAND TO COMPLETE
        BC    7,PCS                    ERROR
        TM    ST,X'01'                 EXCEPTION SET?
        BC    14,LOOP                  NO, ALL OK
        BAL   14,BCL                   PROBABLY EOF
        MVC   PBFR(3),EOF
        PUT   PRNT,PBFR
        BC    15,LOOP
*
*       'INHIBIT STATUS' COMMAND
*
T10     EQU   *
        BAL   14,BCL
        MVC   PBFR(22),T10L
        PUT   PRNT,PBFR
        BAL   13,WAIT                  WAIT FOR I/O TO COMPLETE
        BC    7,PCS                    ERROR
        BAL   14,BCWC                  CLEAR THE BCW
        XIOF  X'10',DEV                ISSUE INHIBIT STATUS COMMAND
        BC    7,PCS                    ERROR
        BAL   13,WAIT                  WAIT FOR COMMAND TO COMPLETE
        BC    7,PCS                    ERROR
        BC    15,LOOP
*
*       'RESET INHIBIT STATUS' COMMAND
*
T11     EQU   *
        BAL   14,BCL
        MVC   PBFR(28),T11L
        PUT   PRNT,PBFR
        BAL   13,WAIT                  WAIT FOR I/O TO COMPLETE
        BAL   14,BCWC                  CLEAR THE BCW
        XIOF  X'20',DEV                ISSUE RESET INHIBIT STATUS CMD
        BC    7,PCS                    ERROR
        BAL   13,WAIT                  WAIT FOR COMMAND TO COMPLETE
        BC    7,PCS                    ERROR
        BC    15,LOOP
*
*       'MODE SET' COMMAND
*
T12     EQU   *
        BAL   14,BCL
        MVC   PBFR(16),T12L
        PUT   PRNT,PBFR
        BAL   13,WAIT                  WAIT FOR I/O TO COMPLETE
        BC    7,PCS                    ERROR
        BAL   14,BCWC                  CLEAR THE BCW
        XIOF  X'03',DEV                ISSUE MODE SET
        BC    7,PCS                    ERROR
        BAL   13,WAIT                  WAIT FOR COMMAND TO COMPLETE
        BC    7,PCS                    ERROR
        BC    15,LOOP
*
*       'REWIND' COMMAND
*
T13     EQU   *
        BAL   14,BCL
        MVC   PBFR(14),T13L
        PUT   PRNT,PBFR
        BAL   13,WAIT                  WAIT FOR I/O TO COMPLETE
        BC    7,PCS                    ERROR
        BAL   14,BCWC                  CLEAR THE BCW
        XIOF  X'07',DEV                ISSUE REWIND
        BC    7,PCS                    ERROR
        BAL   13,WAIT                  WAIT FOR COMMAND TO COMPLETE
        BC    7,PCS                    ERROR
        BC    15,LOOP
*
*       'REWIND WITH LOCK' COMMAND
*
T14     EQU   *
        BAL   14,BCL
        MVC   PBFR(24),T14L
        PUT   PRNT,PBFR
        BAL   13,WAIT                  WAIT FOR I/O TO COMPLETE
        BC    7,PCS                    ERROR
        BAL   14,BCWC                  CLEAR THE BCW
        XIOF  X'0F',DEV                ISSUE WRITE TAPE MARK
        BC    7,PCS                    ERROR
        BAL   13,WAIT                  WAIT FOR COMMAND TO COMPLETE
        BC    7,PCS                    ERROR
        BC    15,LOOP
*
*       'WRITE TAPE MARK' COMMAND
*
T15     EQU   *
        BAL   14,BCL
        MVC   PBFR(22),T15L
        PUT   PRNT,PBFR
        BAL   13,WAIT                  WAIT FOR I/O TO COMPLETE
        BC    7,PCS                    ERROR
        BAL   14,BCWC                  CLEAR THE BCW
        XIOF  X'1F',DEV                ISSUE WRITE TAPE MARK
        BC    7,PCS                    ERROR
        BAL   13,WAIT                  WAIT FOR I/O TO COMPLETE
        BC    7,PCS                    ERROR
        BC    15,LOOP
*
*       'ERASE' COMMAND
*
T16     EQU   *
        BAL   14,BCL
        MVC   PBFR(13),T16L
        PUT   PRNT,PBFR
        BAL   13,WAIT                  WAIT FOR I/O TO COMPLETE
        BC    7,PCS                    ERROR
        BAL   14,BCWC                  CLEAR THE BCW
        XIOF  X'17',DEV                ISSUE ERASE COMMAND
        BC    7,PCS                    ERROR
        BAL   13,WAIT                  WAIT FOR I/O TO COMPLETE
        BC    7,PCS                    ERROR
        BC    15,LOOP
*
*       WAIT FOR TAPE TO BE NOT BUSY
*
WAIT    EQU   *
        TIO   ST,DEV
        BC    2,WAIT                   BUSY, KEEP WAITING
        BC    15,0(,13)
*
*       PRINT CONDITION CODE
*
PC      EQU   *
        BC    8,POK
        BC    4,PREJ
        BC    2,PBSY
        BAL   14,BCL
        MVC   PBFR(14),PCC3
        BC    15,PC1
POK     EQU   *
        BAL   14,BCL
        MVC   PBFR(16),PCC0
        BC    15,PC1
PREJ    EQU   *
        BAL   14,BCL
        MVC   PBFR(16),PCC1
        BC    15,PC1       
PBSY    EQU   *
        BAL   14,BCL
        MVC   PBFR(4),PCC2
PC1     EQU   *
        PUT   PRNT,PBFR        
        BC    15,0(,13)        
*
*       PRINT STATUS CODE
*
PS      EQU   *
        BAL   14,BCL
        MVC   PBFR(91),STAT
        TM    ST,X'80'
        BC    1,PS1
        MVI   PBFR,C' '
        MVC   PBFR+1(9),PBFR
PS1     EQU   *
        TM    ST,X'40'
        BC    1,PS2
        MVI   PBFR+10,C' '
        MVC   PBFR+11(15),PBFR+10
PS2     TM    ST,X'20'
        BC    1,PS3
        MVI   PBFR+26,C' '
        MVC   PBFR+27(16),PBFR+26
PS3     EQU   *
        TM    ST,X'10'
        BC    1,PS4
        MVI   PBFR+43,C' '
        MVC   PBFR+44(4),PBFR+43
PS4     EQU   *
        TM    ST,X'08'
        BC    1,PS5
        MVI   PBFR+48,C' '
        MVC   PBFR+49(11),PBFR+48
PS5     EQU   *
        TM    ST,X'04'
        BC    1,PS6
        MVI   PBFR+60,C' '
        MVC   PBFR+61(10),PBFR+60
PS6     EQU   *
        TM    ST,X'02'
        BC    1,PS7
        MVI   PBFR+71,C' '
        MVC   PBFR+72(10),PBFR+71
PS7     TM    ST,X'01'
        BC    1,PS8
        MVI   PBFR+82,C' '
        MVC   PBFR+83(8),PBFR+82
PS8     EQU   *  
        PUT   PRNT,PBFR                                                                
        BC    15,0(,13)
*
*       PRINT CONDITION CODE & STATUS
*
PCS     EQU   *
        BAL   13,PC
        TIO   ST,DEV                   GET MOST RECENT STATUS
        BAL   13,PS
        BC    15,LOOP
*
*       PRINT SENSE BYTES
*
PN      EQU   *
        BAL   14,BCL
        MVC   PBFR(126),SB0
        TM    TBFR,X'80'
        BC    1,PN01
        MVI   PBFR,C' '
        MVC   PBFR+1(16),PBFR
PN01    EQU   *        
        TM    TBFR,X'40'
        BC    1,PN02
        MVI   PBFR+17,C' '
        MVC   PBFR+18(21),PBFR+17
PN02    EQU   *        
        TM    TBFR,X'20'
        BC    1,PN03
        MVI   PBFR+39,C' '
        MVC   PBFR+40(13),PBFR+39
PN03    EQU   *        
        TM    TBFR,X'10'
        BC    1,PN04
        MVI   PBFR+53,C' '
        MVC   PBFR+54(15),PBFR+53
PN04    EQU   *        
        TM    TBFR,X'08'
        BC    1,PN05
        MVI   PBFR+69,C' '
        MVC   PBFR+70(10),PBFR+69
PN05    EQU   *        
        TM    TBFR,X'04'
        BC    1,PN06
        MVI   PBFR+80,C' '
        MVC   PBFR+81(9),PBFR+80
PN06    EQU   *        
        TM    TBFR,X'02'
        BC    1,PN07
        MVI   PBFR+90,C' '
        MVC   PBFR+91(15),PBFR+90
PN07    EQU   *        
        TM    TBFR,X'01'
        BC    1,PN08
        MVI   PBFR+106,C' '
        MVC   PBFR+107(19),PBFR+106
PN08    EQU   *        
        PUT   PRNT,PBFR
        
        BAL   14,BCL
        MVC   PBFR(69),SB1
        TM    TBFR+1,X'80'
        BC    1,PN11
        MVI   PBFR,C' '
        MVC   PBFR+1(5),PBFR
PN11    EQU   *        
        TM    TBFR+1,X'40'
        BC    1,PN12
        MVI   PBFR+6,C' '
        MVC   PBFR+7(8),PBFR+6
PN12    EQU   *        
        TM    TBFR+1,X'20'
        BC    1,PN13
        MVI   PBFR+15,C' '
        MVC   PBFR+16(8),PBFR+15
PN13    EQU   *        
        TM    TBFR+1,X'10'
        BC    1,PN14
        MVI   PBFR+24,C' '
        MVC   PBFR+25(7),PBFR+24
PN14    EQU   *        
        TM    TBFR+1,X'08'
        BC    1,PN15
        MVI   PBFR+32,C' '
        MVC   PBFR+33(10),PBFR+32
PN15    EQU   *        
        TM    TBFR+1,X'04'
        BC    1,PN16
        MVI   PBFR+43,C' '
        MVC   PBFR+44(8),PBFR+43
PN16    EQU   *        
        TM    TBFR+1,X'02'
        BC    1,PN17
        MVI   PBFR+52,C' '
        MVC   PBFR+53(12),PBFR+52
PN17    EQU   *        
        TM    TBFR+1,X'01'
        BC    1,PN18
        MVI   PBFR+65,C' '
        MVC   PBFR+66(3),PBFR+65
PN18    EQU   *        
        PUT   PRNT,PBFR
        
        BAL   14,BCL    
        MVC   PBFR(65),SB3
        TM    TBFR+3,X'80'
        BC    1,PN31
        MVI   PBFR,C' '
        MVC   PBFR+1(13),PBFR
PN31    EQU   *
        TM    TBFR+3,X'40'
        BC    1,PN32
        MVI   PBFR+14,C' '
        MVC   PBFR+15(8),PBFR+14
PN32    EQU   *
        TM    TBFR+3,X'20'
        BC    1,PN33
        MVI   PBFR+23,C' '
        MVC   PBFR+24(4),PBFR+23
PN33    EQU   *
        TM    TBFR+3,X'10'
        BC    1,PN34
        MVI   PBFR+28,C' '
        MVC   PBFR+29(14),PBFR+28
PN34    EQU   *
        TM    TBFR+3,X'08'
        BC    1,PN35
        MVI   PBFR+43,C' '
        MVC   PBFR+44(14),PBFR+43
PN35    EQU   *
        TM    TBFR+3,X'02'
        BC    1,PN36
        MVI   PBFR+58,C' '
        MVC   PBFR+59(6),PBFR+58
PN36    EQU   *
        PUT   PRNT,PBFR
        
        BAL   14,BCL
        MVC   PBFR(45),SB4
        TM    TBFR+4,X'80'
        BC    1,PN41
        MVI   PBFR,C' '
        MVC   PBFR+1(13),PBFR
PN41    EQU   *                
        TM    TBFR+4,X'40'
        BC    1,PN42
        MVI   PBFR+14,C' '
        MVC   PBFR+15(14),PBFR+14
PN42    EQU   *                
        TM    TBFR+4,X'04'
        BC    1,PN43
        MVI   PBFR+29,C' '
        MVC   PBFR+30(5),PBFR+29
PN43    EQU   *                
        TM    TBFR+4,X'02'
        BC    1,PN44
        MVI   PBFR+35,C' '
        MVC   PBFR+36(9),PBFR+35
PN44    EQU   *                
        PUT   PRNT,PBFR
            
        BC    15,0(,13)                
*
*       JUMP TABLE FOR TESTS
*
JT      DC    YL2(LOOP)
        DC    YL2(T1)		
        DC    YL2(T2)		
        DC    YL2(T3)		
        DC    YL2(T4)		
        DC    YL2(T5)		
        DC    YL2(T6)		
        DC    YL2(T7)		
        DC    YL2(T8)		
        DC    YL2(T9)
        DC    YL2(T10)
        DC    YL2(T11)
        DC    YL2(T12)
        DC    YL2(T13)
        DC    YL2(T14)
        DC    YL2(T15)
        DC    YL2(T16)
JTA     DC    YL2(JT)       

THW     DC    YL2(0)
HW10    DC    YL2(10)
ST      DS    CL2
BN      DC    CL2' 1'

PBFR    DS	CL132

ITE     DC    C'INVALID TEST #'

PCC0    DC    C'COMMAND ACCEPTED'
PCC1    DC    C'COMMAND REJECTED'
PCC2    DC    C'BUSY'
PCC3    DC    C'INVALID DEVICE'

W1      DC    C'UNIVAC 9200/9300'
        DC    C' EMULATOR TAPE T'
        DC    C'EST'
W2      DC    C'AVAILABLE TESTS '
        DC    C'ARE:'
W3      DC    C'  1 - TEST'
W4      DC    C'  2 - SENSE'
W5      DC    C'  3 - READ'
W6      DC    C'  4 - WRITE'
W7      DC    C'  5 - READ BACKW'
        DC    C'ARD'
W8      DC    C'  6 - FORWARD SP'
        DC    C'ACE BLOCK'
W9      DC    C'  7 - FORWARD SP'
        DC    C'ACE FILE'
W10     DC    C'  8 - BACKWARD S'
        DC    C'PACE BLOCK'
W11     DC    C'  9 - BACKWARD S'
        DC    C'PACE FILE'
W12     DC    C' 10 - INHIBIT ST'
        DC    C'ATUS'
W13     DC    C' 11 - RESET INHI'
        DC    C'BIT STATUS'
W14     DC    C' 12 - MODE SET'                        
W15     DC    C' 13 - REWIND'
W16     DC    C' 14 - REWIND WIT'
        DC    C'H LOCK'
W17     DC    C' 15 - WRITE TAPE'
        DC    C'MARK' 
W18     DC    C' 16 - ERASE'       
W99     DC    C'ENTER THE TEST #'
        DC    C' INTO MEMORY LOC'
        DC    C'ATION 4 USING TH'
        DC    C'E DATA ENTRY SWI'
        DC    C'TCHES AND PRESS '
        DC    C'START'

TL      DC    C'TEST # '
TLN     DS    CL2
        DC    C' SELECTED'

T1L     DC    C'ISSUING TEST COM'
        DC    C'MAND'
T2L     DC    C'ISSUING SENSE CO'
        DC    C'MMAND'
T3L     DC    C'ISSUING READ COM'
        DC    C'MAND'
T4L     DC    C'ISSUING WRITE CO'
        DC    C'MMAND'
T5L     DC    C'ISSUING READ BAC'
        DC    C'KWARD COMMAND'
T6L     DC    C'ISSUING FORWARD '
        DC    C'SPACE BLOCK COMM'
        DC    C'AND'                                       
T7L     DC    C'ISSUING FORWARD '
        DC    C'SPACE FILE COMMA'
        DC    C'ND'
T8L     DC    C'ISSUING BACK SPA'
        DC    C'CE BLOCK COMMAND'                                               
T9L     DC    C'ISSUING BACK SPA'
        DC    C'CE FILE COMMAND'                                               
T10L    DC    C'ISSUING INHIBIT '
        DC    C'STATUS'                                               
T11L    DC    C'ISSUING RESET IN'
        DC    C'HIBIT STATUS'                                               
T12L    DC    C'ISSUING MODE SET'
T13L    DC    C'ISSUING REWIND'
T14L    DC    C'ISSUING REWIND W'
        DC    C'ITH LOCK' 
T15L    DC    C'ISSUING WRITE TA'
        DC    C'PEMARK'        
T16L    DC    C'ISSUING ERASE'
        
STAT    DC    C'ATTENTION-STATUS'
        DC    C' MODIFIER-CONTRO'
        DC    C'L UNIT END-BUSY-'
        DC    C'CHANNEL END-DEVI'
        DC    C'CE END-UNIT CHEC'
        DC    C'K-EXCEPTION'

SB0     DC    C'INVALID FUNCTION'
        DC    C'-INTERVENTION RE'
        DC    C'QUIRED-BUS OUT C'
        DC    C'HECK-EQUIPMENT C'
        DC    C'HECK-DATA CHECK-'
        DC    C'DATA LATE-WORD C'
        DC    C'OUNT ZERO-DATA C'
        DC    C'ONVERTER CHECK'
SB1     DC    C'NOISE-STATUS A-S'
        DC    C'TATUS B-7 TRACK-'
        DC    C'LOAD POINT-TAPE '
        DC    C'END-FILE PROTECT'
        DC    C'-BUSY'
SB2     DC    C' '
SB3     DC    C'VP READ ERROR-LP'
        DC    C' ERROR-SKEW-CRC '
        DC    C'READ ERROR-VP WR'
        DC    C'ITE ERROR-REVERS'
        DC    C'E'
SB4     DC    C'RUNAWAY CHECK-MO'
        DC    C'VEMENT ERROR-STA'
        DC    C'LL-TAPE ERROR' 
        
EOF     DC    CL3'EOF'                                                                                             
                
TBA     DC    YL2(TBFR) 
TBE     DC    YL2(TBFR+255)               
TBFR    EQU   *
        ORG   *+8192
                
        END   TST