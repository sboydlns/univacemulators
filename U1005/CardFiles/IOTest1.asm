          BEG                                                TST1
          CRD
      CBF -   1
          PRT
      PBF -   1
          PCH
      UBF -   1    
          ORG 0373
      EOF +2  /*
            
          STA
          * EXECUTE TESTS
      GO  JR  T01
          XF   HLT

      T01 JX  R01
          XF   REA
          LA1 EOF,2            CHECK FOR LAST CARD
          CA1 CBF,2 
          JEA R01              EOF FOUND, WE'RE DONE
          LPR CBF,80           PRINT THE RECORD JUST READ
          XF   PR1            
          J   T01
      R01 J   $
            
          END GO