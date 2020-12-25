                   START     LABEL1
       . A NONSENSE PROGRAM TO TEST THE ASSEMBLER'S ABILITY
       . TO PARSE A PROGRAM.
       .
                   EDEF      LABEL1,WORD1,XXXXX
                   XREF      XREF1,XREF2
       .                   
       LABEL1      LA        1,B1,APOS           . LOAD OCTAL 1
                   LQ        1D                  . LOAD DECIMAL 1
                   LA        -1
                   LA        '12'
                   LA        UTAG1                
                   LAQ       :1D,1D;
                   SA,W      WORD
                   SQ,L      WORD+1
                   LA,W      WORD1
                   LA,W      XXXXX
                   LA,W      XREF1+1
                   LA,W      XREF2
       WORD*       +0
       WORD1       +'ABCDEF'
       WORD2       123  
       WORD3       -1
                   +1,-2
                   -2,1
                   1,2,3
                   1,2,3,4,5
                   +XREF1
                   +XREF1,XREF2                   
       WORD4       EQU       WORD+1
                   DLD       6.0
                   DLD       -6.0
                   DLD       0.09375
                   DLD       -0.09375
                   DLD       123456I
                   DLD       -123456I
       UTAG1       UTAG      1,2
                   UTAG      -1,-2
                   RES       140-$
       FORM1       FORM      8D,8D,8D,6D                   
       . WORD5       FORM1     1,2,3,4
       .
                   LB        B2,2
                   R         77777,,ADDB
                   AN        1,B2,ANEG
                   J         $+2
                   SB,CPW    B2,WORD
                   LA        QUEUES
       .           
       QUEUES      QCB,1,2,3 f1a,f1b f2 f3a,f3b
                   QCB1
                   IOREQ          
       .
       XREF1       +0                            
       .
                   END                                               