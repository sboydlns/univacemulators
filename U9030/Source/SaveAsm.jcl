// JOB LIBS
// DVC 20 // LFD PRNTR
// DVC 50 // VOL REL042 // LBL $Y$JCS // LFD D0
// EXEC LIBS
/$
         FIL   D0=D0
         DEL   D0,S,LNSASM
         ELE   D0,S,LNSASM
// JOB LNSASM,,10000
// OPTION SCAN,SUB
// DVC 20 // LFD PRNTR
// DVCVOL LNS001
// LBL LNSOBJ // LFD OBJ
// DVCVOL LNS001
// LBL LNSLOD // LFD LOD
// WORK1
// WORK2
// EXEC ASM
// PARAM LST=(NC)
// PARAM OUT=OBJ
/$
// CR
/*
// SKIP DONE,11111111
// OPTION SUB
// WORK1
// EXEC LNKEDT
// PARAM OUT=LOD
/$
     LOADM &M
     INCLUDE &M,OBJ
/*     
//DONE NOP
/&
         EOD
         COP.D D0,S,LNSASM
/*
/&          
// FIN