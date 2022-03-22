// JOB LIBS
// DVC 20 // LFD PRNTR
// DVC 50 // VOL LNS001 // LBL LNSMAC // LFD D0
// EXEC LIBS
/$
         FIL   D0=D0
         DEL   D0,P,DALGN
         ELE   D0,P,DALGN
&L       PROC  &P,1
DALGN    NAME  
&L       SRL   &P(1),3
         LA    &P(1),1(&P(1))
         SLL   &P(1),3
         END
         EOD
         COP.D D0,P,DALGN
/*
/&          
// FIN