0000:         START
0000: STRT    EQU   *

0000:         DC    XL16'80000000800000008000000000000000'      3 WORD STATUS TABLE?
0010:         DC    XL4'00030003'	               IOSTCW0
0014:         DC    XL4'00000000'	               IOSTCW1
0018:         DC    XL4'00000000'	               IOSTCW2
001C:         DC    XL4'00000000'	               RESERVED
0020:         DC    XL4'0000000000000000'	               IOST OLD PSW
0028:         DC    XL8'C000000080000100'	               IOST NEW PSW
0030:         DC    XL8'0000000000000000'	               MACHINE CHECK OLD PSW
0038:         DC    XL8'000000008000012C'	               MACHINE CHECK NEW PSW
0040:         DC    XL8'0000000000000000'	               PGM EXCP OLD PSW
0048:         DC    XL8'00000000800001A2'	               PGM EXCP NEW PSW
0050:         DC    XL8'0000000000000000'	               SVC OLD PSW
0058:         DC    XL8'0000000080000130'	               SVC NEW PSW
0060:         DC    XL8'0000000000000000'	               INTV. TIMER OLD PSW
0068          DC    XL8'0000000080000134'	               INTV. TIMER NEW PSW
0070:         DC    XL16'D0F84780D2704110B8D24550D29E9640'      RESERVED
0080:         DC    XL8'0000000000000000'	               MONITOR OLD PSW
0088:         DC    XL8'0000000080000138' 	               MONITOR NEW PSW
0090:         DC    XL16'4550D2DED20DD128B647D205D137D26A'      RESERVED
00A0:         DC    XL4'000001B8'	               CAW
00A4:         DC    XL4'4550D2DE'	               MM LL RR 00
00A8:         DC    XL8'D20CD137B6E54550'	               RESERVED
00B0:         DC    XL16'00000000000000000000000000000000'      REL REG 0-3
00C0:         DC    XL16'00000000000000000000000000000000'      REL REG 5-7  
00D0:         DC    XL16'9260D10AD248D10BD10A4550D2DE4830'      RESERVED
00E0:         DC    XL16'D0564120D7804140000841F000F74110'      IOST BCSW
00F0:         DC    XL4'02000000'	               INT. DISK BCW0 (READ COMMAND)
00F4:         DC    XL4'00000015'	               BCW1 (READ 21 SECTORS)
00F8:         DC    XL4'00000000'	               BCW2 (HEAD 0)
00FC:         DC    XL4'80000400'	               BCW3 (CYL 0, REC 4) 
     *
     * IOST INTERRUPT ENTRY POINT
     *
0100:         TM    X'00E2',12	               CHECK I/O STATUS IN BCSW
0104:         BC    14,X'0140'	               ERROR
0108:         BC    0,X'01E4'
010C:         MVI   X'0109',240	               CHG NOP TO UNCONDITIONAL BRANCH
0110:         TM    X'0000',6	               SELECTOR #2?
0114:         BC    1,X'015A'	               YES
0118:         TM    X'0000',4	               SELECTOR #1?
011C:         BC    1,X'015A'	               YES
0120:         TM    X'0000',3	               IDA?
0124:         BC    1,X'015A'	               YES
     * ILLEGAL CHANNEL HALT
0128:         HPR   X'3500',53
     * MACHINE CHECK INTERRUPT
012C:         HPR   X'3100',49
     * SUPERVISOR CALL INTERRUPT
0130:         HPR   X'3200',50
     * INTERVAL TIMER INTERRUPT
0134:         HPR   X'3300',51
     * MONITOR INTERRUPT
0138:         HPR   X'3400',52
     * PROGRAM EXCEPTION HALT
013C:         HPR   X'3600',54
     * DISK I/O ERROR
0140:         HPR   X'3700',55
     * GET DEVICE ADDR & RESET I/O STATUS TABLE
0144:         NI    X'0000',15	               CLEAR MS 4 BITS OF STATUS			
0148:         LH    3,X'0000'	               GET CHANNEL & DEVICE ADDR IN R3
014C:         OI    X'0000',128	               RESET I/O STATUS TABLE
0150:         OI    X'0004',128
0154:         OI    X'0008',128
0158:         BCR   15,R7
     * BASIC ERROR CHECKING, OK
015A:         BAL   7,X'0144'
015E:         LA    2,X'0200'	               INIT MEMORY POINTER
0162:         XR    R8,R8		               CLEAR REGS 8-15
0164:         XR    R9,R9
0166:         XR    R10,R10
0168:         XR    R11,R11
016A:         XR    R12,R12
016C:         XR    R13,R13
016E:         XR    R14,R14
0170:         XR    R15,R15
     * AS NEARLY AS I CAN TELL THIS IS CHECKING TO SEE HOW
     * MUCH MEMORY IS INSTALLED.
0172:         STM   R8,R15,0(R2)	               CLEAR MEMORY
0176:         LA    2,X'020'(R2)	               BUMP ADDRESS
017A:         BC    15,X'0172'                                  LOOP UNTIL WE GET A PROGRAM EXCEPTION
     *
017E:         LA    4,X'0020'	               CALC. LAST INSTALLED MEMORY ADDR.
0182:         SR    R2,R4
0184:         LR    R5,R2		               PUT IT IN R5
0186:         LA    2,X'03F0'                                   SET BCW/CCW ADDRESS TO X'3F0'
018A:         ST    2,X'000C'
018E:         MVC   X'01D1'(3),X'000D'
0194:         MVC   X'00F1'(3),X'000D'
019A:         SIO   X'3000',0	               ISSUE READ COMMAND
019E:         BC    15,X'019E'	               WAIT FOR INTERRUPT
     * PROGRAM EXCEPTION INTERRUPT
01A2:         MVI   X'017B',0	               MAKE BRANCH A NOP
01A6:         TM    X'0043',5	               CHECK PGM. EXCP. INTERRUPT CODE
01AA:         BC    14,X'013C'	               ADDRESS CHECK? NO, OOPS!
01AE:         LPSW  X'0040',0	               YES, RETURN TO INST AFTER THE ERROR
     * THERE IS A CCW CHAIN BURIED IN THIS CRUFT SOMEWHERE, I JUST NEED TO WINKLE IT OUT
01B2:         DC    XL6'000000000000'
01B8:         DC    XL8'070001D840000006'	               SEL. CCW (SEEK)
01C0:         DC    XL8'310001DA40000005' 	
01C8:         DC    XL8'080001C000000000'
01D0:         DC    XL8'0600000C00000E29'	               READ - ADDR SET TO X'3F0' IN CODE
01D8:         DC    XL8'0000000000080100'	
01E0:         DC    XL2'0000'
01E2:         DC    XL2'0000'
     * CONTINUE AFTER READ COMMAND
01E4:         LA    2,X'03F0'
01E8:         BAL   7,X'0144'	               RESET I/O STATUS TABLE
01EC:         BCR   15,R2                                       JUMP TO X'03F0'
01EE:         LA    1,X'480'(,R13)
01F2:         BCR   R0,R0
01F4:         SVC   R2,R6
01F6:         LA    4,X'780'(,R13)
01FA:         LA    3,X'000B'
01FE:         BAL   5,X'5D6'(,R14)

              END