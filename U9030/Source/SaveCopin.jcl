// JOB LIBS
// DVC 20 // LFD PRNTR
// DVC 50 // VOL REL042 // LBL $Y$JCS // LFD D0
// EXEC LIBS
/$
         FIL   D0=D0
         DEL   D0,S,LNSCOPIN
         ELE   D0,S,LNSCOPIN
// JOB LNSCOPIN,,10000
// OPTION SCAN,SUB
// IF ('&F.X' NE 'X')FOK
//F JSET LNSSRC
//FOK NOP
// DVC 20 // LFD PRNTR
// DVCVOL LNS001
// LBL &F // LFD D0
// EXEC LIBS
/$
         FIL   D0=D0
         DEL   D0,S,&M
         ELE   D0,S,&M
// CR
         EOD         
         COP.D D0,S,&M
/*
/&
         EOD
         COP.D D0,S,LNSCOPIN
/*
/&          
// FIN