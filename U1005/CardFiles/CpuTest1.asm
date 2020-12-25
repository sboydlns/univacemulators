          BEG                                                TST1
          ORG 0373
          * SHIFT TEST BUFFERS, MUST BE ALIGNED
          * ON A ROW BOUNDARY
      SBR +31 12345678901234567890123456789012
      SBL +31 12345678901234567890123456789012
          * 
      L10 +10 1234567890              
      L21 +21 123456789012345678901
      WRK +31
      CTR +05 05011
      C12 +5  00012
      CMI +5  0001K                    MINUS 12
      CBG +10 123456789-               BIG NEGATIVE NUMBER 
            
          STA
          * EXECUTE TESTS
      GO  JR  T01
          JR  T02
          JR  T03
          JR  T04
          JR  T05
          JR  T06
          JR  T07
          JR  T08
          JR  T09
          JR  T10
          JR  T11
          *
          XF   HLT
          *
          * LOAD / STORE ASCENDING AR1 TESTS
      T01 JX  R01
          LA1 L10,10                   LEN = REG LEN
          SA1 WRK,10                  
          LA1 L10,15                   LEN > REG LEN
          SA1 WRK,15
          LA1 L10,5                    LEN < REG LEN
          SA1 WRK,5
          * LOAD ASCENDING AR2 TESTS
          LA2 L21,21                   LEN = REG LEN                  
          SA2 WRK,21                   
          LA2 L21,30                   LEN > REG LEN
          SA2 WRK,30                   
          LA2 L21,10                   LEN < REG LEN
          SA2 WRK,10                   
      R01 J   $                        RETURN TO CALLER
          *
          * LOAD DESCENDING AR1 TESTS
      T02 JX  R02
          LD1 L10,10                   LEN = REG LEN                  
          SD1 WRK,10
          LD1 L10,15                   LEN > REG LEN
          SD1 WRK,15
          LD1 L10,5                    LEN < REG LEN
          SD1 WRK,5
          * LOAD DESCENDING AR2 TESTS
          LD2 L21,21                   LEN = REG LEN                  
          SD2 WRK,21
          LD2 L21,30                   LEN > REG LEN
          SD2 WRK,30
          LD2 L21,10                   LEN < REG LEN
          SD2 WRK,10
      R02 J   $                        RETURN TO CALLER
          *
          * LOAD PRINT / STORE PRINT TESTS
      T03 JX  R03
          LPR L10,31
          SPR WRK,31
      R03 J   $                        RETURN TO CALLER
          *
          * CLEAR TESTS
      T04 JX  R04
          CLR WRK,31
      R04 J   $                        RETURN TO CALLER
          *
          * COMPARE ALPHA TESTS
      T05 JX  R05
          LA1 L10,5                    GET 12345 IN LSB OF A1
          CA1 L10,5                    TEST FOR EQUALITY
          JEA N01
          XF   HLT
      N01 CA1 L10+1,5                  TEST FOR INEQUALITY
          JUA N02
          XF   HLT
      N02 CA1 L10,4                    TEST FOR INEQUALITY
          JUA N03
          XF   HLT
      N03 CA1 L10,6                    TEST FOR INEQUALITY
          JUA R05
          XF   HLT
      R05 J   $                        RETURN TO CALLER
          *
          * COMPARE NUMERIC TESTS
      T06 JX  R06
          LA1 L10,5                    GET 12345 IN LSB OF A1
          CN1 L10,5                    TEST FOR EQUALITY
          JE  N05
          XF   HLT
      N05 CN1 L10+1,5                  TEST FOR LESS THAN
          JL  N06
          XF   HLT
      N06 CN1 L10+9,1                  TEST FOR GREATER THAN
          JG  R06
          XF   HLT
      R06 J   $                        RETURN TO CALLER
          *
          * COUNTER TESTS
      T07 JX  R07
      N07 IC  CTR                      LOOP 5 TIMES
          JL  N07
      R07 J   $                        RETURN TO CALLER   
          *
          * SHIFT TESTS
      T08 JX  R08
          SHL SBL,31 5
          SHR SBR,31 5
      R08 J   $                        RETURN TO CALLER
          *
          * ADD / SUBTRACT TESTS
      T09 JX  R09
          JR  AS1                      INIT OPERANDS
          AM1 WRK,5                    ADD 12 TO 12345
          JR  AS2                      INIT OPERANDS
          AM2 WRK,5                    ADD 12 TO 12345
          JR  AS1                      INIT OPERANDS
          AR1 WRK,5                    ADD 12 TO 12345
          JR  AS2                      INIT OPERANDS
          AR2 WRK,5                    ADD 12 TO 12345
          JR  AS1                      INIT OPERANDS
          SM1 WRK,5                    SUBTRACT 12 FROM 12345
          JR  AS2                      INIT OPERANDS
          SM2 WRK,5                    SUBTRACT 12 FROM 12345
          JR  AS1                      INIT OPERANDS
          SR1 WRK,5                    SUBTRACT 12345 FROM 12
          JR  AS2                      INIT OPERANDS
          SR2 WRK,5                    SUBTRACT 12345 FROM 12
      R09 J   $                        RETURN TO CALLER 
          XF   HLT
          *
          * MULTIPLY / DIVIDE TESTS
      T10 JX  R10
          JR  AS1                      INIT OPERANDS
          MUL WRK,5                    MULTIPLY 12 BY 12345
          JR  AS2                      INIT OPERANDS
          DIV WRK,5                    DIVIDE 12 BY 12345
      R10 J   $                        RETURN TO CALLER
          * 
          * ODD BALL LOAD / STORE TESTS
      T11 JX  R11
          LA2 C12,5                    GET 00012 INTO AR2
          SZS WRK,5                    ZERO SUPPRESS
          LWS C12,5                    GET 00012<SPACE> INTO AR2
          LWS CMI,5                    GET 00012- INTO AR2
          LN1 CMI,5                    GET 00012 INTO AR1
          LN2 CMI,5                    GET 00012 INTO AR2
          LA2 CBG,10                   GET 123456789- INTO AR2
          SED WRK,16                   STORE 12,345,678.90<SPACE>  
      R11 J   $                        RETURN TO CALLER
          *
      AS1 JX  A1R                      ARITHMETIC SET UP FOR AR1
          LD1 L10,5                    GET 12345 INTO WRK
          SD1 WRK,5  
          LA1 C12,5                    GET 00012 INTO AR1
      A1R J   $                        RETURN TO CALLER
          * 
      AS2 JX  A2R                      ARITHMETIC SET UP FOR AR2
          LD2 L10,5                    GET 12345 INTO WRK
          SD2 WRK,5  
          LA2 C12,5                    GET 00012 INTO AR2
      A2R J   $                        RETURN TO CALLER
      
          END GO