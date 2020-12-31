                   START     BEGIN
       .
                   ER$DEF
       .                   
       BEGIN       DPL       BCD1
                   DPS       OP1
                   DPL       BCD2
                   DPS       OP2
                   LA,W      PLUS
                   SA,W      OP
                   DPL       BCD1
                   DA        BCD2
                   DPS       RSLT
                   TYPE      MLEN,MSG
                   DT        40
                   J         D6ISZERO
                   TYPE      D6LEN,D6ZERO                   
       D6ISZERO    DT        100
                   J         D7ISZERO
                   TYPE      D7LEN,D7ZERO                   
       D7ISZERO    DT        200
                   J         D8ISZERO
                   TYPE      D8LEN,D8ZERO                   
       D8ISZERO    DT        400
                   J         D9ISZERO
                   TYPE      D9LEN,D9ZERO                   
       D9ISZERO    DT        1000
                   J         D10ISZERO
                   TYPE      D10LEN,D10ZERO                   
       D10ISZERO   DT        4
                   J         NOTPOS
                   TYPE      IPLEN,ISPOS
       NOTPOS      .                   
       .
                   DPS       OP1
                   DPL       BCD2
                   DPS       OP2
                   LA,W      MINUS
                   SA,W      OP
                   DPL       OP1                   
                   DAN       BCD2
                   DPS       RSLT
                   TYPE      MLEN,MSG
       .
                   DPL       BCD1
                   DPS       OP1
                   DPL       BCD2
                   DPS       OP2
                   LA,W      MINUS
                   SA,W      OP
                   DPL       BCD1
                   DAN       BCD2
                   DPS       RSLT
                   TYPE      MLEN,MSG
                   DT        20
                   J         NOTNEG
                   TYPE      INLEN,ISNEG
       NOTNEG      .
       .
                   LA,W      COMP9
                   SA,W      OP1
                   LA,W      COMP9+1
                   SA,W      OP1+1
                   LA,W      COMP9+2
                   SA,W      OP1+2
                   DPL       RSLT
                   DPS       OP2
                   DN        1
                   DPS       RSLT
                   TYPE      MLEN,MSG
       .                   
                   LA,W      COMP10
                   SA,W      OP1
                   LA,W      COMP10+1
                   SA,W      OP1+1
                   LA,W      COMP10+2
                   SA,W      OP1+2
                   DPL       RSLT
                   DPS       OP2
                   DN        2
                   DPS       RSLT
                   TYPE      MLEN,MSG
       .
                   LA,W      CVTLOW
                   SA,W      OP1
                   LA,W      CVTLOW+1
                   SA,W      OP1+1
                   LA,W      CVTLOW+2
                   SA,W      OP1+2
                   DPL       C12345
                   DPS       OP2
                   DPL       ZERO
                   DCL       BCD3
                   SQ,A
                   INT2DEC
                   DPS       RSLT
                   TYPE     MLEN,MSG
       .                   
                   LA,W      CVTHIGH
                   SA,W      OP1
                   LA,W      CVTHIGH+1
                   SA,W      OP1+1
                   LA,W      CVTHIGH+2
                   SA,W      OP1+2
                   DPL       C12345
                   DPS       OP2
                   DPL       ZERO
                   DCU       BCD4
                   SQ,A
                   INT2DEC
                   DPS       RSLT
                   TYPE     MLEN,MSG
       . TEST CARRY                   
                   DPL       BCD5
                   DPS       OP1
                   DPL       BCD6
                   DPS       OP2
                   LA,W      PLUS
                   SA,W      OP
                   DPL       BCD5
                   DA        BCD6
                   DPS       RSLT
                   TYPE      MLEN,MSG
       .           
                   DT        1
                   J         NOCARRY
                   TYPE      CRYLEN,CARRY
       NOCARRY     DT        10
                   J         NOTZERO
                   TYPE      IZLEN,ISZERO
       NOTZERO     .
       .
                   DPL       BCD0
                   DPS       OP1
                   DPL       BCD6
                   DPS       OP2
                   LA,W      PLUSC
                   SA,W      OP
                   DPL       BCD0
                   DAC       BCD6
                   DPS       RSLT
                   TYPE     MLEN,MSG
       .                   
                   HALT      
       
       BCD0        DLD       0I
       BCD1        DLD       1234567I
       BCD2        DLD       9876543I
       BCD3        +'    1'
                   +'    2'
                   +'    3'
                   +'    4'
                   +'    5'
       BCD4        +1,0
                   +2,0
                   +3,0
                   +4,0
                   +5,0
       BCD5        DLD       9999999999I
       BCD6        DLD       1I                   
       ZERO        DLD       0
       ONE         +1
       TWO         +2 
       .
       MSG         EQU       $
       OP1         RES       2D
       OP          RES       1D
       OP2         RES       2D
                   +'  =  '
       RSLT        RES       2D
       RSLT2       +'          '                   
                   +'    ^'
       MLEN        EQU       $-MSG                                     
       .
       PLUS        +'  +  '
       PLUSC       +' +C  '
       MINUS       +'  -  '
       COMP9       +'NINES COMP     '
       COMP10      +'TENS COMP      '
       CVTLOW      +'CONVERT LOW    '
       CVTHIGH     +'CONVERT HIGH   '
       C12345      +'12345     '
       CARRY       +'CARRY' 
                   +'    ^'
       CRYLEN      EQU       $-CARRY                   
       ISZERO      +'IS ZERO' 
                   +'    ^'
       IZLEN       EQU       $-ISZERO 
       D6ZERO      +'DIGIT 6 NOT ZERO'
                   +'    ^'
       D6LEN       EQU       $-D6ZERO                                    
       D7ZERO      +'DIGIT 7 NOT ZERO'
                   +'    ^'
       D7LEN       EQU       $-D7ZERO                                    
       D8ZERO      +'DIGIT 8 NOT ZERO'
                   +'    ^'
       D8LEN       EQU       $-D8ZERO                                    
       D9ZERO      +'DIGIT 9 NOT ZERO'
                   +'    ^'
       D9LEN       EQU       $-D9ZERO                                    
       D10ZERO      +'DIGIT 10 NOT ZERO'
                   +'    ^'
       D10LEN       EQU       $-D10ZERO 
       ISPOS       +'IS POSITIVE'
                   +'    ^'
       IPLEN       EQU       $-ISPOS                                                      
       ISNEG       +'IS NEGATIVE'
                   +'    ^'
       INLEN       EQU       $-ISNEG                                                      
       .       
                   END                   