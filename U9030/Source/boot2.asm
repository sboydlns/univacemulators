0000:         START
0000: STRT    EQU   *

              USING *,R12

0000:         LR    R12,R2                       LOAD BASE REG.
0002:         LA    6,X'10A'(,R12)               CHANGE IOST NEW PSW ADDR.
0006:         ST    6,X'002C'
000A:         MVI   X'0028',0                    CLR INT. ENB. BITS IN IOST NEW PSW
000E:         LA    1,X'2B8'(,R12)               CHANGE PGM. EXCP. NEW PSW ADDR
0012:         ST    1,X'004C'
0016:         BC    15,X'076'(,R12)              GET STARTED
     *
001A:         LTE   R1,R1                        TEST R1
001C:         BC    8,X'066'(,R12)               R1 = 0
0020:         STM   R6,R9,X'AF4'(,R12)
0024:         LH    6,X'BCA'(,R12)
0028:         ST    6,X'070'(,R12)
002C:         LR    R7,R1
002E:         M     6,X'070'(,R12)
0032:         SR    R7,R1
0034:         LA    7,X'0001'(R7)
0038:         XR    R6,R6
003A:         D     6,X'B48'(,R12)
003E:         LR    R1,R6
0040:         ST    0,X'070'(,R12)
0044:         LH    7,X'BCA'(,R12)
0048:         BCTR  R7,R0
004A:         XR    R6,R6
004C:         D     6,X'070'(,R12)
0050:         AR    R1,R7
0052:         LTE   R1,R1
0054:         BC    2,X'05C'(,R12)
0058:         L     1,X'B48'(,R12)
005C:         STC   1,X'074'(,R12)
0060:         LM    R6,R9,X'AF4'(,R12)
0064:         BCR   R15,R15
     * COME HERE IF R1 = 0
0066:         LH    1,X'BCA'(,R12)               GET RECORD #?
006A:         BC    15,X'05C'(,R12)
     *
006E:         DC    XL8'0000000000010000'
0076:         ST    5,X'B20'(,R12)               SAVE MEM. SIZE
007A:         ST    3,X'B2C'(,R12)               SAVE BOOT DVC. ADDR.
007E:         CLI   X'B2E'(R12),3                CHANNEL = 3?
0082:         BC    7,X'08A'(,R12)               YES
0086:         MVI   X'B24'(R12),255
008A:         BAL   14,X'2C4'(,R12)              REQUEST ALTERNATE SUPER NAME
008E:         BAL   14,X'282'(,R12)              DETERMINE # BYTES / STORAGE KEY?
0092:         BAL   14,X'33E'(,R12)              READ VOL1
0096:         BAL   14,X'3D2'(,R12)              READ FMT4
009A:         BAL   14,X'3FC'(,R12)              SAVE FMT4 INFO
009E:         BAL   14,X'44C'(,R12)              FIND $Y$TRAN
00A2:         BAL   14,X'4DA'(,R12)              SAVE START CYL/TRK
00A6:         BAL   14,X'586'(,R12)              READ FMT2
00AA:         BAL   14,X'540'(,R12)              GET FIRST PART2 EXTENT INFO
00AE:         BAL   14,X'4EC'(,R12)              MAKE $Y$TRANA FILE TO FIND
00B2:         BAL   14,X'44C'(,R12)              FIND $Y$TRANA
00B6:         BAL   14,X'502'(,R12)              MAKE SURE IT WAS FOUND
00BA:         BAL   14,X'516'(,R12)              MAKE $Y$LOAD FILE TO FIND
00BE:         BAL   14,X'44C'(,R12)              FIND $Y$LOD
00C2:         BAL   14,X'52E'(,R12)              GET FMT2 DISK ADDRESS
00C6:         BAL   14,X'586'(,R12)              READ FMT2
00CA:         BAL   14,X'5D8'(,R12)              FIND SUPER IN $Y$LOD
00CE:         BAL   14,X'6EE'(,R12)
00D2:         CLI   X'C27'(R12),255
00D6:         BC    7,X'0E2'(,R12)
00DA:         BAL   14,X'838'(,R12)
00DE:         BC    15,X'0E6'(,R12)
00E2:         BAL   14,X'8CA'(,R12)
00E6:         BAL   14,X'96C'(,R12)
00EA:         XR    R1,R1
00EC:         L     2,X'BB4'(,R12)
00F0:         L     3,X'AF0'(,R12)
00F4:         L     4,X'B20'(,R12)
00F8:         LA    7,X'001E'
00FC:         SR    R4,R7
00FE:         LA    6,X'A42'(,R12)
0102:         MVC   X'000'(30,R4),X'6000'
0108:         BCR   R15,R4                       GOTO SUPERVISOR START ADDR.
     * IOST INTERRUPT HANDLER
010A:         OI    X'0000',128                  RESET I/O STATUS TABLE
010E:         OI    X'0004',128
0112:         OI    X'0008',128
0116:         TM    X'00E2',128                  ATTENTION?
011A:         BC    1,X'132'(,R12)               YES
011E:         TM    X'00E0',7                    CHANNEL ZERO?
0122:         BC    8,X'12C'(,R12)               YES
0126:         BC    15,X'136'(,R12)
012A:         BCR   R15,R15
012C:         MVI   X'B27'(R12),0
0130:         BCR   R15,R13                      RETURN TO CALLER
0132:         LPSW  X'0020',0                    RETURN FROM INTERRUPT
0136:         LA    1,X'00E0'                    POINT TO BCSW
013A:         TM    X'002'(R1),2                 UNIT CHECK?
013E:         BC    1,X'192'(,R12)               YES
0142:         TM    X'002'(R1),12                CHANNEL END / DEVICE END?
0146:         BC    1,X'14E'(,R12)               YES
014A:         BC    15,X'142'(,R12)
014E:         MVI   X'127'(R12),240              MAKE NOP A BRANCH
0152:         BC    15,X'12C'(,R12)
0156:         HPR   X'600'(R3),54
015A:         HPR   X'700'(R3),55                RETRY COUNT EXCEEDED STOP
015E:         HPR   X'800'(R3),56
0162:         HPR   X'900'(R3),57
0166:         HPR   X'A00'(R3),58
016A:         HPR   X'B00'(R3),59                BAD FMT4 STOP
016E:         HPR   X'C00'(R3),60                BLOCKS/TRACK = 0 STOP
0172:         HPR   X'D00'(R3),61                $Y$TRAN NOT FOUND STOP
0176:         HPR   X'E00'(R3),62                $Y$LOD NOT FOUND STOP
017A:         HPR   X'F00'(R3),63                $Y$LOD PARTN TABLE ERROR STOP
017E:         HPR   X'000'(R4),64
0182:         HPR   X'100'(R4),65                FMT2 TRK/CYL = 0 STOP
0186:         HPR   X'200'(R4),66
018A:         HPR   X'300'(R4),67
     * WAIT FOR INTERRUPT
018E:         BC    15,X'18E'(,R12)
     *
0192:         MVC   X'0200'(4),X'00A0'           SAVE CAW
0198:         MVC   X'0214'(16),X'00F0'          SAVE IDA BCW
019E:         LR    R0,R13                       SAVE RETURN ADDR
01A0:         CLI   X'B24'(R12),255              IDA DISK
01A4:         BC    8,X'26A'(,R12)               YES
01A8:         LA    1,X'AE8'(,R12)               POINT TO CAW
01AC:         ST    1,X'00A0'
01B0:         MVI   X'127'(R12),0                MAKE BRANCH A NOP
01B4:         LA    15,X'1BC'(,R12)              SET NEW RETURN ADDR
01B8:         BC    15,X'25E'(,R12)              
     * COME HERE AFTER SENSE COMMAND
01BC:         TM    X'CE7'(R12),8                NO RECORD FOUND?                            
01C0:         BC    1,X'27A'(,R12)               YES
01C4:         CLI   X'B24'(R12),255              IDA DISK?
01C8:         BC    8,X'204'(,R12)               YES
01CC:         TM    X'CE6'(R12),2                SENSE WHAT?
01D0:         BC    1,X'1EC'(,R12)               YES
01D4:         AI    X'B26'(R12),1                BUMP RETRY COUNT
01D8:         CLI   X'B27'(R12),100              256 RETRIES?
01DC:         BC    8,X'15A'(,R12)               YES
01E0:         MVI   X'127'(R12),240              MAKE NOP A BRANCH
01E4:         BC    15,X'250'(,R12)              CONTINUE
01E8:         BC    15,X'15A'(,R12)
01EC:         LA    15,X'1FC'(,R12)              SET NEW RETURN ADDR              
01F0:         LA    1,X'B68'(,R12)               POINT TO CAW
01F4:         ST    1,X'00A0'
01F8:         BC    15,X'25E'(,R12)              GO ISSUE I/O
01FC:         MVI   X'127'(R12),240
0200:         BC    15,X'250'(,R12)
0204:         TM    X'CE8'(R12),8                FLAG BYTE MISCOMPARE?
0208:         BC    1,X'212'(,R12)               YES
020C:         BC    15,X'250'(,R12)              NO
0210:         DC    XL2'0000'
0212:         MVI   X'00F0',14                   SET READ ID COMMAND
0216:         MVI   X'00FB',128                  SET HEAD ADDR = 1
021A:         LA    2,X'D00'(,R12)               SET BUFFER ADDR
021E:         STH   2,X'00F2'
0222:         LA    15,X'22A'(,R12)              SET NEW RETURN ADDR.
0226:         BC    15,X'25E'(,R12)              GO ISSUE I/O
     * COME HERE AFTER READ ID
022A:         MVC   X'210'(2,R12),X'00FC'        SAVE CYL # FROM BCW
0230:         MVC   X'CF6'(2,R12),X'D01'(R12)    GET CYL # FROM READ ID BFR
0236:         LA    2,X'CF4'(,R12)               BUILD A BCW
023A:         BAL   4,X'366'(,R12)
023E:         MVC   X'00FC'(2),X'210'(R12)       RESTORE OLD CYL # TO BCW
0244:         MVI   X'00FB',64                   SET TRACK CONDITION TO X'40'
0248:         MVI   X'127'(R12),240              MAKE NOP A BRANCH
024C:         BC    15,X'25C'(,R12)              GO ISSUE I/O
     * RETRY LAST I/O
0250:         MVC   X'00A0'(4),X'0200'           RESTORE SAVED CAW
0256:         MVC   X'00F0'(16),X'0214'          RESTORE SAVE BCW
025C:         LR    R13,R0                       RESTORE SAVE RTN ADDR.
     * ISSUE I/O 
025E:         SSM   X'B1C'(R12),0                ENABLE INTERUPTS
0262:         SIO   X'000'(R3),0                 ISSUE I/O
0266:         BC    15,X'266'(,R12)              WAIT FOR INTERRUPT
026A:         MVC   X'00F0'(4),X'AE8'(R12)       SET SENSE BCW
0270:         XC    X'00F8'(2),X'00F8'           
0276:         BC    15,X'1B0'(,R12)              CONTINUE
027A:         MVI   X'C6F'(R12),255              SET NO RECORD FOUND FLAG
027E:         BC    15,X'14E'(,R12)
     *
0282:         CLI   X'00A4',2                    MACHINE ID = 2?
0286:         BC    7,X'28E'(,R12)               NO
028A:         MVI   X'C50'(R12),64
     * I THINK THIS IS TRYING TO DETERMINE THE SIZE OF A MEMORY BLOCK
     * DEFINED BY A SINGLE STORAGE KEY. A BLOCK CAN BE 512, 1024 OR 2048
     * BYTES DEPENDING ON INSTALLED MEMORY. THE RESULT SHOULD BE 1, 2 OR
     * 3 DEPENDING ON THE SIZE OF THE MEMORY BLOCKS.
028E:         XR    R8,R8
0290:         LA    4,X'0010'                    
0294:         LH    2,X'C56'(,R12)
0298:         SSK   R4,R2
029A:         LA    4,X'0020'                    
029E:         LA    2,X'0200'(R2)
02A2:         SSK   R4,R2
02A4:         LA    4,X'0030'
02A8:         LA    2,X'0400'(R2)
02AC:         SSK   R4,R2
02AE:         LH    2,X'C56'(,R12)
02B2:         ISK   R8,R2
02B4:         SRL   R8,X'0004'
     * PROGRAM EXCEPTION INTERRUPT HANDLER
02B8:         STC   8,X'C51'(,R12)
02BC:         ST    14,X'0044'                   SAVE R14 TO OLD PSW
02C0:         LPSW  X'0040',0                    RETURN FROM INTERRUPT
     * SEND "IPL TO LOAD STANDARD SUPERVISOR ..." TO CONSOLE
02C4:         XR    R6,R6
02C6:         MVC   X'0100'(8),X'A88'(R12)       COPY BCW TO LOW MEM.
02CC:         SSM   X'B1C'(R12),0                ENABLE IOST INTERRUPT
02D0:         SIO   X'000'(R6),0                 ISSUE I/O
02D4:         BAL   13,X'18E'(,R12)              WAIT FOR INTERRUPT              
     * READ FROM CONSOLE
02D8:         XR    R6,R6
02DA:         MVC   X'0100'(8),X'A90'(R12)       COPY BCW TO LOW MEM.
02E0:         SSM   X'B1C'(R12),0                ENABLE IOST INTERRUPT
02E4:         SIO   X'000'(R6),0                 ISSUE I/O
02E8:         BAL   13,X'18E'(,R12)              WAIT FOR INTERRUPT
     *
02EC:         CLI   X'C1F'(R12),109              1ST CHAR UNDERSCORE?
02F0:         BC    8,X'330'(,R12)               YES, NOTHING ENTERED
02F4:         CLI   X'C1F'(R12),64               1ST CHAR SPACE?
02F8:         BC    8,X'166'(,R12)               YES, HALT
02FC:         LA    6,X'C48'(,R12)               OVERWRITE "00000000" WITH
0300:         LA    7,X'C1F'(,R12)               CHARACTERS ENTERED.
0304:         LA    8,X'0006'
0308:         MVC   X'000'(1,R6),X'000'(R7)
030E:         LA    6,X'0001'(R6)
0312:         LA    7,X'0001'(R7)
0316:         CLI   X'000'(R7),109               UNDERSCORE?
031A:         BC    8,X'32A'(,R12)               YES, DONE
031E:         CLI   X'000'(R7),64                SPACE?
0322:         BC    8,X'32A'(,R12)               YES, DONE
0326:         BCT   8,X'308'(,R12)               LOOP
032A:         MVC   X'C28'(8,R12),X'C48'(R12)    OVERWRITE SY$STD00 WITH NEW
0330:         CLI   X'C26'(R12),195
0334:         BC    7,X'33C'(,R12)
0338:         MVI   X'C27'(R12),255
033C:         BCR   R15,R14
     * END OF SUBROUTINE X'2C4'
     *
     * READ THE VTOC VOL1 LABEL TO X'E78'.
033E:         CLI   X'B24'(R12),255
0342:         BC    7,X'350'(,R12)
0346:         MVC   X'00F0'(16),X'B30'(R12)      COPY BCW TO READ C0/H0/R3
034C:         BC    15,X'358'(,R12)
0350:         LA    1,X'A98'(,R12)
0354:         ST    1,X'00A0'
0358:         SSM   X'B1C'(R12),0                ENABLE IOST INTERRUPT                
035C:         SIO   X'000'(R3),0                 READ VOL1
0360:         BAL   13,X'18E'(,R12)              WAIT FOR INTERRUPT
0364:         BCR   R15,R14
     * BUILD IDA BCW. PTR TO DISK ADDR. IN R2 (00CCHHR FORMAT)
0366:         STM   R6,R8,X'AF4'(,R12)           SAVE REGS
036A:         MVC   X'B3C'(2,R12),X'002'(R2)     COPY DISK ADDR TO BCW     
0370:         MVC   X'B3A'(1,R12),X'005'(R2)
0376:         MVC   X'B3E'(1,R12),X'006'(R2)
037C:         LA    6,X'D50'(,R12)               
0380:         ST    6,X'B30'(,R12)
0384:         MVI   X'B30'(R12),2                SET READ COMMAND
0388:         L     6,X'B04'(,R12)
038C:         ST    6,X'B34'(,R12)
0390:         MVC   X'00F0'(16),X'B30'(R12)      MOVE BCW TO LOW MEM
0396:         LH    7,X'B2A'(,R12)               GET OLD CYL #
039A:         LH    8,X'B3C'(,R12)               GET NEW CYL # 
039E:         N     7,X'B08'(,R12)
03A2:         N     8,X'B08'(,R12)               
03A6:         STH   8,X'B2A'(,R12)               MAKE OLD = NEW
03AA:         CR    R7,R8                        OLD = NEW?
03AC:         BC    8,X'3C0'(,R12)               YES
03B0:         BC    2,X'3C6'(,R12)               OLD > NEW
03B4:         SR    R8,R7                        CALC. SEEK DIFF.
03B6:         STH   8,X'00F8'                    SET SEEK DIFF MAGNITUDE
03BA:         LM    R6,R8,X'AF4'(,R12)           RESTORE REGS.
03BE:         BCR   R15,R4                       RETURN
03C0:         XR    R8,R8                        SEEK DIFF = 0
03C2:         BC    15,X'3B6'(,R12)
03C6:         SR    R7,R8                        CALC SEEK DIFF
03C8:         OI    X'00F6',32                   SET NEG. DIRECTION BIT
03CC:         LR    R8,R7
03CE:         BC    15,X'3B6'(,R12)
     * READ VTOC FMT4 LABEL TO X'E50'.
03D2:         MVC   X'CF6'(5,R12),X'E87'(R12)    GET DISK ADDR OF FMT4 LABEL
03D8:         CLI   X'B24'(R12),255
03DC:         BC    7,X'3EE'(,R12)
03E0:         LA    2,X'CF4'(,R12)               BUILD BCW               
03E4:         BAL   4,X'366'(,R12)
03E8:         MVC   X'00F1'(3),X'B40'(R12)       COPY BFR ADDR TO BCW
03EE:         SSM   X'B1C'(R12),0
03F2:         SIO   X'000'(R3),0
03F6:         BAL   13,X'18E'(,R12)              WAIT FOR INTERRUPT
03FA:         BCR   R15,R14
     * SAVE FMT4 INFO
03FC:         CLI   X'E7C'(R12),244              FMT4 ID = 4?
0400:         BC    8,X'408'(,R12)               YES
0404:         BC    15,X'16A'(,R12)              NO, OOPS!
0408:         LA    8,X'E50'(,R12)
040C:         CLC   X'03E'(4,R8),X'B50'(R12)     DVC SIZE = 203 CYL, 20 TRACKS?
0412:         BC    8,X'438'(,R12)               YES
0416:         MVC   X'B4E'(2,R12),X'040'(R8)     GET # TRACKS
041C:         XR    R6,R6
041E:         XR    R7,R7
0420:         IC    7,X'04B'(,R8)                GET BLOCKS / TRACK
0424:         CR    R6,R7                        EQUAL 0?
0426:         BC    8,X'16E'(,R12)               YES, OOPS!
042A:         D     6,X'B54'(,R12)               
042E:         ST    7,X'B44'(,R12)
0432:         MVC   X'B4B'(1,R12),X'04B'(R8)     COPY BLOCKS / TRACK
0438:         MVC   X'C5C'(10,R12),X'069'(R8)    GET VTOC EXTENT INFO
043E:         MVC   X'C66'(5,R12),X'02D'(R8)     GET LAST ACTIVE FMT1 CCHHR
0444:         MVC   X'C6B'(4,R12),X'03E'(R8)     GET DEVICE SIZE
044A:         BCR   R15,R14                      RETURN
     * SEARCH FOR $Y$TRAN IN VTOC
044C:         MVC   X'CF6'(4,R12),X'C5E'(R12)    GET EXTENT LOWER LIMIT
0452:         LA    1,X'AB8'(,R12)               POINT TO CAW
0456:         ST    1,X'00A0'
045A:         MVI   X'CFA'(R12),0                CLEAR RECORD #                
045E:         MVI   X'C6F'(R12),0
0462:         CLI   X'B24'(R12),255              IDA DISK?
0466:         BC    7,X'48A'(,R12)               NO
046A:         LA    2,X'CF4'(,R12)               POINT TO EXTENT START CCHH
046E:         BAL   4,X'366'(,R12)               BUILD THE BCW
0472:         MVC   X'E50'(8,R12),X'C79'(R12)    COPY $Y$TRAN TO BUFFER
0478:         MVC   X'00F1'(3),X'B40'(R12)       COPY BFR ADDRESS TO BCW
047E:         MVI   X'00F0',9                    SET COMMAND TO SEARCH/EQUAL
0482:         LH    2,X'AD6'(,R12)               SET SEARCH SINGLE TRACK/LENGTH 8
0486:         STH   2,X'00F6'
048A:         SSM   X'B1C'(R12),0                ENABLE INTERRUPTS
048E:         SIO   X'000'(R3),0                 EXECUTE SEARCH COMMAND
0492:         BAL   13,X'18E'(,R12)              WAIT FOR INTERRUPT
0496:         CLI   X'C6F'(R12),0                WAS RECORD FOUND?
049A:         BC    8,X'4D8'(,R12)               YES
049E:         CLC   X'C62'(4,R12),X'CF6'(R12)    CRNT CYL/TRK = LAST CYL/TRK?
04A4:         BC    7,X'4B0'(,R12)               NO
04A8:         BC    0,X'4D8'(,R12)
04AC:         BC    15,X'172'(,R12)              $Y$TRAN NOT FOUND, HALT
04B0:         AI    X'CF8'(R12),1                BUMP TRACK #
04B4:         CLC   X'CF8'(2,R12),X'C6D'(R12)    MAX TRACK?
04BA:         BC    2,X'4C2'(,R12)               YES
04BE:         BC    15,X'452'(,R12)              NO, TRY AGAIN
04C2:         AI    X'CF6'(R12),1                BUMP CYL #
04C6:         CLC   X'CF6'(2,R12),X'C6B'(R12)    MAX CYL?
04CC:         BC    2,X'172'(,R12)               YES, HALT
04D0:         MVI   X'CF9'(R12),0                SET TRACK # TO ZER0
04D4:         BC    15,X'44C'(,R12)              TRY AGAIN
04D8:         BCR   R15,R14                      RETURN TO CALLER
     * SAVE FILE EXTENT START CYL/TRK
04DA:         LA    8,X'E50'(,R12)               POINT TO BUFFER
04DE:         MVC   X'CEC'(2,R12),X'06B'(R8)     SAVE EXTENT LOW CYL.
04E4:         MVC   X'CEE'(2,R12),X'06D'(R8)     SAVE EXTENT LOW TRK.
04EA:         BCR   R15,R14                      RETURN 
     * MAKE $Y$TRANA FILE TO FIND
04EC:         MVI   X'AD7'(R12),8
04F0:         MVC   X'AD1'(3,R12),X'C42'(R12)
04F6:         MVC   X'C79'(8,R12),X'C3A'(R12)    MAKE $Y$TRANA FILE TO FIND
04FC:         MVI   X'4A9'(R12),240              MAKE NOP A BRANCH
0500:         BCR   R15,R14
     *
0502:         MVI   X'4A9'(R12),0                MAKE BRANCH A NOP
0506:         CLI   X'C6F'(R12),255              FILE NOT FOUND?
050A:         BC    8,X'514'(,R12)               YES, IT WAS NOT
050E:         MVC   X'C46'(2,R12),X'06B'(R8)
0514:         BCR   R15,R14                      RETURN
     * MAKE $Y$LOD FILE TO FIND
0516:         MVI   X'AD7'(R12),6
051A:         MVC   X'AD1'(3,R12),X'C37'(R12)
0520:         MVC   X'C79'(6,R12),X'C31'(R12)    MAKE $Y$LOD FILE TO FIND
0526:         MVC   X'172'(4,R12),X'176'(R12)    CHANGE HPR FOR $Y$LOD
052C:         BCR   R15,R14
     * GET FMT2 DISK ADDRESS
052E:         LA    8,X'E50'(,R12)               POINT TO DISK BUFFER
0532:         MVC   X'C70'(5,R12),X'087'(R8)     GET FMT2 DISK ADDRESS
0538:         MVC   X'C72'(1,R12),X'C74'(R12)
053E:         BCR   R15,R14
     * GET FIRST PARTITION 2 EXTENT INFO.
0540:         LA    8,X'E50'(,R12)               POINT TO FMT2 BUFFER
0544:         LA    9,X'030'(,R8)                POINT TO EXTENT TABLE
0548:         TM    X'000'(R9),64                EXTENT BELONG TO PARTITION 2?
054C:         BC    1,X'558'(,R12)               YES
0550:         LA    9,X'0004'(R9)                BUMP TABLE PTR
0554:         BC    15,X'548'(,R12)              LOOP
0558:         LH    7,X'0000'(R9)                EXTENT INFO TO R7
055C:         N     7,X'BBC'(,R12)               ISLOATE RELATIVE TRACK #
0560:         XR    R6,R6                        R6 = 0
0562:         MVC   X'B28'(2,R12),X'080'(R8)     GET TRACKS / CYLINDER
0568:         LH    9,X'B28'(,R12)               TRK/CYL = 0?
056C:         CH    9,X'BC8'(,R12)
0570:         BC    8,X'182'(,R12)               YES, HALT
0574:         ST    9,X'BC0'(,R12)               DIVIDE TRACK # BY TRK/CYL              
0578:         D     6,X'BC0'(,R12)
057C:         STH   6,X'CF2'(,R12)               SAVE ABS. TRACK #
0580:         STH   7,X'CF0'(,R12)               SAVE ABS. CYL #
0584:         BCR   R15,R14                      RETURN
     * READ FMT2 LABEL
0586:         LA    8,X'E50'(,R12)               POINT TO FMT1 BUFFER
058A:         MVC   X'CF6'(5,R12),X'087'(R8)     GET FMT2 DISK ADDRESS
0590:         LA    1,X'A98'(,R12)               POINT TO CAW
0594:         ST    1,X'00A0'
0598:         LA    1,X'E50'(,R12)                              
059C:         ST    1,X'AB0'(,R12)
05A0:         MVI   X'AB0'(R12),14
05A4:         MVI   X'AB7'(R12),140
05A8:         CLI   X'B24'(R12),255              IDA DISK?
05AC:         BC    7,X'5C2'(,R12)               NO
05B0:         LA    2,X'CF4'(,R12)               BUILD BCW
05B4:         BAL   4,X'366'(,R12)
05B8:         MVC   X'00F1'(3),X'B40'(R12)       MOVE NEW BFR ADDR TO BCW
05BE:         MVI   X'00F0',2                    MAKE A READ COMMAND
05C2:         SSM   X'B1C'(R12),0                ENABLE INTERRUPTS
05C6:         SIO   X'000'(R3),0                 READ FMT2 LABEL
05CA:         BAL   13,X'18E'(,R12)              WAIT FOR INTERRUPT
05CE:         MVI   X'AB0'(R12),6
05D2:         MVI   X'AB7'(R12),96
05D6:         BCR   R15,R14                      RETURN
     * IT LOOKS LIKE THIS BIT HERE IS SEARCHING $Y$LOD FOR SUPERVISOR
     * LOAD MODULE.
05D8:         BC    15,X'63A'(,R12)
05DC:         MVC   X'CF6'(4,R12),X'BC4'(R12)    GET CYL/TRK ADDRESS
05E2:         MVC   X'CFA'(1,R12),X'074'(R12)    GET RECORD #
05E8:         LA    1,X'A68'(,R12)
05EC:         ST    1,X'00A0'
05F0:         CLI   X'B24'(R12),255              IDA DISK?
05F4:         BC    7,X'600'(,R12)               NO
05F8:         LA    2,X'CF4'(,R12)               BUILD BCW
05FC:         BAL   4,X'366'(,R12)
0600:         SSM   X'B1C'(R12),0                READ A BLOCK
0604:         SIO   X'000'(R3),0
0608:         BAL   13,X'18E'(,R12)              WAIT FOR INTERRUPT
060C:         LA    1,X'D55'(,R12)               POINT TO BUFFER
0610:         LA    2,X'0013'
0614:         CLC   X'C28'(9,R12),X'000'(R1)     MODULE NAME MATCH REQ. SUPER?
061A:         BC    8,X'632'(,R12)               YES
061E:         CLI   X'008'(R1),161               END OF FILE?
0622:         BC    8,X'17A'(,R12)               YES, HALT
0626:         LA    1,X'000D'(R1)                BUMP BFR POINTER
062A:         BCT   2,X'614'(,R12)               LOOP
062E:         BC    15,X'5D8'(,R12)              NEXT BLOCK
0632:         MVC   X'B58'(13,R12),X'000'(R1)    SAVE DIRECTORY INFO
0638:         BCR   R15,R14                      RETURN
     *
063A:         LH    7,X'BCC'(,R12)
063E:         L     9,X'BB8'(,R12)               VALUE OF B88 CHANGES
0642:         BCR   R15,R9
     *
0644:         LA    8,X'E50'(,R12)               POINT TO FMT2 BUFFER
0648:         MVC   X'B28'(2,R12),X'080'(R8)     GET TRKS/CYL
064E:         LH    9,X'B28'(,R12)               TRKS/CYL = 0?
0652:         CH    9,X'BC8'(,R12)
0656:         BC    8,X'182'(,R12)               YES, HALT
065A:         ST    9,X'BC0'(,R12)               SAVE TRKS/CYL
065E:         LA    10,X'030'(,R8)               POINT TO EXTENT TABLE
0662:         LH    11,X'02E'(,R8)               GET BLOCKS/TRK                            
0666:         LA    11,X'0001'(R11)              BUMP BY 1
066A:         LH    5,X'032'(,R8)                GET # TRACKS 1ST EXTENT
066E:         TM    X'000'(R10),32               CRNT EXTENT FOR PARTN 1?
0672:         BC    1,X'67A'(,R12)               YES
0676:         BC    15,X'17A'(,R12)              NO, HALT
067A:         LH    7,X'0000'(R10)               GET CRNT EXTENT INFO
067E:         N     7,X'BBC'(,R12)               ISOLATE REL. TRACK #
0682:         STH   7,X'BCC'(,R12)
0686:         XR    R6,R6                        DIVIDE BY BLKS/TRK
0688:         D     6,X'BC0'(,R12)
068C:         STH   6,X'BC6'(,R12)               SAVE ABS. TRACK #
0690:         STH   7,X'BC4'(,R12)               SAVE ABS. CYL #
0694:         AI    X'BCA'(R12),1                
0698:         TM    X'086'(R8),16                TEST UNDOCUMENTED FLAG BIT
069C:         BC    1,X'6A6'(,R12)               IT'S SET
06A0:         XR    R1,R1                        CLEAR R1
06A2:         BC    15,X'6AE'(,R12)              SKIP FOLLOWING
     * NO FRIGGING CLUE WHAT THIS IS DOING. IT IS UNDOCUMENTED.
06A6:         L     1,X'01C'(,R8)                GET LACE FACTOR PARTN #4???
06AA:         L     0,X'020'(,R8)                GET EOD ID PARTN #4???
     *
06AE:         BAL   15,X'01A'(,R12)
06B2:         LA    9,X'694'(,R12)
06B6:         ST    9,X'BB8'(,R12)
06BA:         BCT   11,X'6EA'(,R12)
06BE:         AI    X'BCC'(R12),1
06C2:         MVC   X'BCA'(2,R12),X'BC8'(R12)
06C8:         LH    11,X'02E'(,R8)
06CC:         LA    11,X'0001'(R11)
06D0:         LA    9,X'686'(,R12)
06D4:         ST    9,X'BB8'(,R12)
06D8:         BCT   5,X'63A'(,R12)
06DC:         LA    10,X'0004'(R10)
06E0:         MVC   X'BCA'(2,R12),X'BC8'(R12)
06E6:         BC    15,X'662'(,R12)
06EA:         BC    15,X'5DC'(,R12)
     *
06EE:         MVC   X'B10'(4,R12),X'B60'(R12)    CLEAR TO SPACES                    
06F4:         L     7,X'B10'(,R12)
06F8:         N     7,X'B18'(,R12)
06FC:         XR    R6,R6
06FE:         LA    8,X'E50'(,R12)
0702:         LH    4,X'02E'(,R8)
0706:         ST    4,X'B14'(,R12)
070A:         D     6,X'B14'(,R12)
070E:         LTE   R6,R6
0710:         BC    7,X'718'(,R12)
0714:         LR    R6,R4
0716:         BCTR  R7,R0
0718:         STH   6,X'BCA'(,R12)
071C:         LA    9,X'030'(,R8)
0720:         TM    X'000'(R9),64
0724:         BC    1,X'73C'(,R12)
0728:         LA    9,X'0004'(R9)
072C:         BC    15,X'720'(,R12)
0730:         SH    7,X'0002'(R9)
0734:         LA    9,X'0004'(R9)
0738:         BC    15,X'73C'(,R12)
073C:         CH    7,X'0002'(R9)
0740:         BC    11,X'730'(,R12)
0744:         AH    7,X'0000'(R9)
0748:         N     7,X'BBC'(,R12)
074C:         XR    R6,R6
074E:         D     6,X'BC0'(,R12)
0752:         STH   6,X'BC6'(,R12)
0756:         STH   7,X'BC4'(,R12)
075A:         TM    X'086'(R8),32
075E:         BC    1,X'768'(,R12)
0762:         XR    R1,R1
0764:         BC    15,X'770'(,R12)
0768:         L     1,X'024'(,R8)
076C:         L     0,X'028'(,R8)
0770:         BAL   15,X'01A'(,R12)
0774:         MVC   X'CF6'(4,R12),X'BC4'(R12)
077A:         MVC   X'CFA'(1,R12),X'074'(R12)
0780:         CLI   X'B24'(R12),255
0784:         BC    7,X'790'(,R12)
0788:         LA    2,X'CF4'(,R12)
078C:         BAL   4,X'366'(,R12)
0790:         SSM   X'B1C'(R12),0
0794:         SIO   X'000'(R3),0
0798:         BAL   13,X'18E'(,R12)
079C:         CLC   X'D50'(3,R12),X'B61'(R12)
07A2:         BC    7,X'17E'(,R12)
07A6:         MVC   X'BB3'(1,R12),X'D53'(R12)
07AC:         L     7,X'B60'(,R12)
07B0:         N     7,X'B18'(,R12)
07B4:         LA    7,X'0001'(R7)
07B8:         ST    7,X'B60'(,R12)
07BC:         L     9,X'BA8'(,R12)
07C0:         XR    R6,R6
07C2:         IC    6,X'B64'(,R12)
07C6:         MVI   X'B64'(R12),5
07CA:         AR    R9,R6
07CC:         CLI   X'001'(R9),144
07D0:         BC    8,X'7E8'(,R12)
07D4:         CLI   X'001'(R9),18
07D8:         BC    8,X'800'(,R12)
07DC:         CLI   X'001'(R9),19
07E0:         BC    8,X'830'(,R12)
07E4:         BC    15,X'7EE'(,R12)
07E8:         MVC   X'AF0'(4,R12),X'009'(R9)
07EE:         XR    R4,R4
07F0:         IC    4,X'0000'(R9)
07F4:         LA    4,X'0002'(R4)
07F8:         AR    R9,R4
07FA:         AR    R6,R4
07FC:         BC    15,X'824'(,R12)
0800:         MVC   X'B0C'(4,R12),X'005'(R9)
0806:         L     7,X'BAC'(,R12)
080A:         A     7,X'B0C'(,R12)
080E:         XR    R4,R4
0810:         IC    4,X'0003'(R9)
0814:         EX    4,X'832'(,R12)
0818:         IC    4,X'0000'(R9)
081C:         LA    4,X'0002'(R4)
0820:         AR    R9,R4
0822:         AR    R6,R4
0824:         C     6,X'BB0'(,R12)
0828:         BC    4,X'7CC'(,R12)
082C:         BC    15,X'6EE'(,R12)
0830:         BCR   R15,R14
0832:         MVC   X'000'(1,R7),X'009'(R9)
0838:         XR    R9,R9
083A:         LA    5,X'0009'
083E:         LA    6,X'EDC'(,R12)
0842:         LA    7,X'0010'
0846:         BAL   11,X'8AA'(,R12)
084A:         MVC   X'000'(14,R6),X'2000'
0850:         LA    2,X'000E'(R2)
0854:         LA    6,X'000E'(R6)
0858:         BCT   4,X'860'(,R12)
085C:         BAL   11,X'8AA'(,R12)
0860:         BCT   7,X'84A'(,R12)
0864:         BC    0,X'87E'(,R12)
0868:         MVC   X'CF6'(4,R12),X'CEC'(R12)
086E:         MVI   X'CFA'(R12),1
0872:         LA    1,X'B88'(,R12)
0876:         ST    1,X'00A0'
087A:         MVI   X'865'(R12),240
087E:         BAL   15,X'906'(,R12)
0882:         BCT   5,X'88A'(,R12)
0886:         BC    15,X'186'(,R12)
088A:         LA    6,X'EDC'(,R12)
088E:         LA    7,X'0010'
0892:         XR    R10,R10
0894:         LCS   R9,X'000'(,R6)
0898:         LA    6,X'EDC'(,R12)
089C:         BC    8,X'84A'(,R12)
08A0:         BC    4,X'8A8'(,R12)
08A4:         BC    15,X'186'(,R12)
08A8:         BCR   R15,R14
08AA:         L     3,X'B04'(,R12)
08AE:         LA    4,X'0005'
08B2:         MVC   X'0110'(16),X'A60'(R12)
08B8:         LA    2,X'D00'(,R12)
08BC:         SSM   X'B1C'(R12),0
08C0:         SIO   X'000'(R3),0
08C4:         BAL   13,X'18E'(,R12)
08C8:         BCR   R15,R11
08CA:         MVC   X'CF6'(4,R12),X'CEC'(R12)
08D0:         LA    5,X'0009'
08D4:         MVI   X'CFA'(R12),1
08D8:         LA    1,X'A68'(,R12)
08DC:         ST    1,X'00A0'
08E0:         XR    R9,R9
08E2:         BAL   15,X'906'(,R12)
08E6:         LA    6,X'D50'(,R12)
08EA:         XR    R10,R10
08EC:         LCS   R9,X'000'(,R6)
08F0:         BCT   5,X'8F8'(,R12)
08F4:         BC    15,X'186'(,R12)
08F8:         BC    8,X'8E2'(,R12)
08FC:         BC    4,X'904'(,R12)
0900:         BC    15,X'186'(,R12)
0904:         BCR   R15,R14
0906:         CLI   X'B24'(R12),255
090A:         BC    7,X'926'(,R12)
090E:         STM   R2,R4,X'960'(,R12)
0912:         LA    2,X'CF4'(,R12)
0916:         BAL   4,X'366'(,R12)
091A:         LM    R2,R4,X'960'(,R12)
091E:         CLI   X'C27'(R12),255
0922:         BC    8,X'94E'(,R12)
0926:         SSM   X'B1C'(R12),0
092A:         ST    15,X'AF4'(,R12)
092E:         L     3,X'B2C'(,R12)
0932:         SIO   X'000'(R3),0
0936:         BAL   13,X'18E'(,R12)
093A:         XR    R1,R1
093C:         IC    1,X'CFA'(,R12)
0940:         LA    1,X'0001'(R1)
0944:         STC   1,X'CFA'(,R12)
0948:         L     15,X'AF4'(,R12)
094C:         BCR   R15,R15
094E:         LA    1,X'EDC'(,R12)
0952:         ST    1,X'00F0'
0956:         MVI   X'00F0',5
095A:         BC    15,X'926'(,R12)
095E:         DC    XL14'0000000000000000000000000000'
096C:         L     1,X'BB4'(,R12)
0970:         MVC   X'0A4'(4,R1),X'00A4'
0976:         AH    1,X'0006'(R1)
097A:         MVC   X'010'(4,R1),X'B44'(R12)
0980:         MVC   X'014'(4,R1),X'B48'(R12)
0986:         MVC   X'018'(4,R1),X'B4C'(R12)
098C:         MVC   X'004'(4,R1),X'CEC'(R12)
0992:         MVC   X'064'(6,R1),X'C28'(R12)
0998:         OC    X'03A'(2,R1),X'C50'(R12)
099E:         OC    X'03A'(2,R1),X'C51'(R12)
09A4:         MVC   X'008'(4,R1),X'CF0'(R12)
09AA:         LH    2,X'C46'(,R12)
09AE:         LTE   R2,R2
09B0:         BC    8,X'9B8'(,R12)
09B4:         SH    2,X'CEC'(,R12)
09B8:         STH   2,X'00C'(,R1)
09BC:         MVC   X'024'(4,R1),X'C70'(R12)
09C2:         L     2,X'AF0'(,R12)
09C6:         LA    2,X'00FF'(R2)
09CA:         SRL   R2,X'0008'
09CE:         STH   2,X'084'(,R1)
09D2:         STH   2,X'086'(,R1)
09D6:         L     2,X'B20'(,R12)
09DA:         SRL   R2,X'0008'
09DE:         STH   2,X'080'(,R1)
09E2:         L     2,X'BB4'(,R12)
09E6:         A     2,X'04C'(,R1)
09EA:         CLI   X'C26'(R12),211
09EE:         BC    8,X'A34'(,R12)
09F2:         L     3,X'048'(,R1)
09F6:         CLC   X'002'(2,R2),X'B2E'(R12)
09FC:         BC    8,X'A0C'(,R12)
0A00:         LA    2,X'0020'(R2)
0A04:         BCT   3,X'9F6'(,R12)
0A08:         BC    15,X'18A'(,R12)
0A0C:         OI    X'004'(R2),1
0A10:         S     2,X'BB4'(,R12)
0A14:         STH   2,X'028'(,R1)
0A18:         A     2,X'BB4'(,R12)
0A1C:         LH    2,X'012'(,R2)
0A20:         LH    1,X'B2A'(,R12)
0A24:         A     2,X'BB4'(,R12)
0A28:         STH   1,X'012'(,R2)
0A2C:         MVC   X'00C'(2,R2),X'C5E'(R12)
0A32:         BCR   R15,R14
     *
0A34:         MVI   X'A59'(R12),2
0A38:         MVC   X'028'(2,R1),X'B2E'(R12)
0A3E:         BC    15,X'A32'(,R12)
0A42:         MVC   X'000'(100,R1),X'000'(R2)
0A48:         LA    1,X'0064'(R1)
0A4C:         LA    2,X'0064'(R2)
0A50:         CR    R1,R3
0A52:         BC    4,X'0000'
0A56:         LH    15,X'0008'
0A5A:         BCR   R15,R15
0A5C:         HPR   X'0000',32
0A60:         DC    XL2'0200'
0A62:         LPR   R15,R0
0A64:         DC    XL4'00000050'
0A68:         BCR   R0,R0
0A6A:         LPR   R14,R4
0A6C:         STD   0,X'0006'
0A70:         LNER  R0,R0
0A72:         LPR   R14,R6
0A74:         STD   0,X'0005'
0A78:         SSK   R0,R0
0A7A:         DC    XL6'0E6000000000'
0A80:         BCTR  R0,R0
0A82:         LNR   R4,R0
0A84:         LDPR  R0,R0
0A86:         DC    XL2'0100'
0A88:         DC    XL8'41000FBE0000004A'        CONSOLE BCW (WRITE TRANSLATE)
0A90:         DC    XL8'020010080000000F'        CONSOLE BCW (READ)
0A98:         BCR   R0,R0
0A9A:         LPR   R14,R4
0A9C:         STD   0,X'0006'
0AA0:         LNER  R0,R0
0AA2:         LPR   R14,R6
0AA4:         STD   0,X'0005'
0AA8:         SSK   R0,R0
0AAA:         DC    XL6'0E9000000000'
0AB0:         BCTR  R0,R0
0AB2:         LTE   R6,R12
0AB4:         LDPR  R0,R0
0AB6:         DC    XL2'0060'
0AB8:         BCR   R0,R0
0ABA:         LPR   R14,R4
0ABC:         STD   0,X'0006'
0AC0:         CER   R0,R0
0AC2:         LPR   R14,R6
0AC4:         STD   0,X'0005'
0AC8:         SSK   R0,R0
0ACA:         DC    XL6'0EB000000000'
0AD0:         CDR   R0,R0
0AD2:         LPR   R6,R9
0AD4:         STD   0,X'0008'
0AD8:         SSK   R0,R0
0ADA:         DC    XL6'0EC000000000'
0AE0:         BCTR  R0,R0
0AE2:         LTE   R6,R12
0AE4:         LDPR  R0,R0
0AE6:         DC    XL2'0060'
0AE8:         DC    XL4'040010D6'                IDA SENSE COMMAND BCW
0AEC:         DC    XL16'00000005000000000000000000000000'
0AFC:         DC    XL14'0000000000000000000000010000'
0B0A:         STR   R15,R15
0B0C:         DC    XL12'00000000000000000000000000'
0B18:         DC    XL4'FFFFFF'
0B1C:         STH   0,X'0000'
0B20:         DC    XL16'00000000000000000000000000000000'
0B30:         DC    XL16'02001268000000010000000080000300' IDA BCW (READ C0/H0/R3)
0B40:         DC    XL4'00124000'                          ADDR OF DISK BFR
0B44:         DC    XL16'00000005000000140000001400CB0014'
0B54:         DC    XL4'00000004'
0B58:         STH   4,X'040'(,R4)
0B5C:         STH   4,X'040'(,R4)
0B60:         DC    CL4'    '
0B64:         STH   0,X'0000'
0B68:         BCR   R0,R0
0B6A:         LPR   R14,R4
0B6C:         STD   0,X'0006'
0B70:         CER   R0,R0
0B72:         LPR   R14,R6
0B74:         STD   0,X'0005'
0B78:         SSK   R0,R0
0B7A:         DC    XL6'0F6000000000'
0B80:         OR    R0,R0
0B82:         LPR   R14,R6
0B84:         LDPR  R0,R0
0B86:         DC    XL2'0005'
0B88:         BCR   R0,R0
0B8A:         LPR   R14,R4
0B8C:         STD   0,X'0006'
0B90:         LNER  R0,R0
0B92:         LPR   R14,R6
0B94:         STD   0,X'0005'
0B98:         SSK   R0,R0
0B9A:         DC    XL6'0F8000000000'
0BA0:         BALR  R0,R0
0BA2:         LTE   R12,R12
0BA4:         LDPR  R0,R0
0BA6:         DC    XL4'01000000'
0BAA:         LNR   R4,R0
0BAC:         DC    XL2'0000'
0BAE:         LCR   R8,R8
0BB0:         DC    XL6'000000000000'
0BB6:         LCR   R8,R8
0BB8:         DC    XL4'00000A34'
0BBC:         DC    XL2'0000'
0BBE:         SLR   R15,R15
0BC0:         DC    XL14'0000000100000000000000000000'
     * IPL TO LOAD STANDARD SUPERVISOR UNLESS NEW NAME KEYED
0BCE:         DC    XL2'2785'                    ESC e CURSOR HOME
0BD0:         DC    XL2'27D4'                    ESC M ERASE DISPLAY
0BD2:         DC    XL1'0F'                      SI    END PROTECTED
OBD3:         DC    CL13'IPL TO LOAD S'
0BE0:         DC    CL16'TANDARD SUPERVIS'
0BF0:         DC    CL16'OR UNLESS NEW NA'
0C00:         DC    CL10'ME KEYED  '
0C0A:         DC    XL1'1E'                      RS (SOE)
0C0B:         DC    CL8'6D6D6D6D6D6D6B6D'
0C13:         DC    XL5'270B40E70F'              ESC VT X Y SI CURSOR POSN.
     * _______,_
0C18:         DC    XL8'000000000000006D'
0C20:         DC    XL8'6D6D6D6D6D6B6D00'
0C28:         DC    CL8'SY$STD00'                DFLT SUPER NAME
     *
0C30:         DC    XL1'90'
0C31:         DC    CL6'$Y$LOD'
0C37:         DC    XL3'001021'
0C3A:         DC    CL8'$Y$TRANA'
     *
0C42:         DC    XL14'00102A000000F0F0F0F0F0F0F0F0'
0C50:         DC    XL16'0000000200042000280007FF40404040'
0C60:         DC    XL16'40404040404040404040404040404000'
0C70:         DC    XL9'404040404040404040'
0C79:         DC    CL8'$Y$TRAN '
     *
0C81:         DC    XL15'000000000000000000000000000000'
0C90:         DC    XL16'00000000000000000000000000000000'
0CA0:         DC    XL16'00000000000000000000000000000000'
0CB0:         DC    XL16'00000000000000000000000000000000'
0CC0:         DC    XL16'00000000000000000000000000000000'
0CD0:         DC    XL16'00000000000000000000000000000000'
0CE0:         DC    XL16'00000000000040404040400000000000'
0CF0:         DC    XL16'00000000000000000000030000000000'
