// JOB LIBS
// DVC 20 // LFD PRNTR
// DVC 50 // VOL REL042 // LBL $Y$JCS // LFD D0
// EXEC LIBS
/$
         FIL   D0=D0
         DEL   D0,S,LNSPACK
         ELE   D0,S,LNSPACK
// JOB LNSPACK
// DVC 20   // LFD PRNTR
// DVCVOL LNS001
// LBL LNSOBJ // LFD D0
// DVCVOL LNS001
// LBL LNSLOD // LFD D1
// DVCVOL LNS001
// LBL LNSSRC // LFD D2
// DVCVOL LNS001
// LBL LNSMAC // LFD D3
// EXEC LIBS
/$
         FIL   D0=D0,D1=D1,D2=D2,D3=D3
         PAC   D0
         COP   D0
         PAC   D1
         COP   D1
         PAC   D2
         COP   D2
         PAC   D3
         COP   D3
/*
/&
         EOD
         COP.D D0,S,LNSPACK
/*
/&          
// FIN