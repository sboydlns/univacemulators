// JOB LIBS
// DVC 20 // LFD PRNTR
// DVC 50 // VOL REL042 // LBL $Y$JCS // LFD D0
// EXEC LIBS
/$
         FIL   D0=D0
         DEL   D0,S,PREP18
         ELE   D0,S,PREP18
// JOB PREP18
// OPTION SCAN,SUB
// DVC 20 // LFD PRNTR
// DVC 67,&D
// VOL &V.(NOV) // LFD DISKIN
// EXEC DSKPRP
/$
   SERNR=&V,IPLDK=N,PREPT=F,TRCON=N
VOL1
INSERT   NONE
/*
/&
         EOD
         COP.D D0,S,PREP18
/*
/&          
// FIN