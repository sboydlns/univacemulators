001       BEG                                                TST1
002       CRD                                                
002       *CARD BUFFER DEFINITION                            
004   C01 -   1
00401 C01 -   5
005   C02 -   10
006   C03 -   20
007       *PRINTER BUFFER DEFINITION
008       PRT
009   P01 -   1
010   P02 -   10
011   P03 -   20
012       *LITERALS
013       ORG 0373
014   LT1 +27 1005 ASSEMBLER TEST PROGRAM
015   CT1 +5  200101           
016       *DEFINE A VALUE THAT CAN BE SHIFTED. IT MUST
017       *END ON A ROW BOUNDARY. I.E. COL = 31.
018       ORG 0404
019   SH1 +31 1234567890123456789012345678901
020       STA
021   GO  LA1 C01,10
022       LA2 C02,5
023       SHL SH1,31 3
024       IC  CT1
025       XF   REA
026       XF   PR1
027       XF   PUN
028       XFC U)))
029       CCA C01,1 A 
030       J   L01
03010     J   L02
031   L01 J   GO
032       END GO
              