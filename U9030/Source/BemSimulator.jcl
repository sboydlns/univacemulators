// JOB BEMSIM,,8000,8000,2
// DVC 20 // LFD PRNTR
// OPTION JOBDUMP
// DVCVOL REL042
// LBL $Y$SRC // LFD $Y$SRC
// DVCVOL REL042
// LBL $Y$MAC // LFD $Y$MAC
// DVCVOL LNS001
// LBL LNSSRC // LFD SRC
// DVCVOL LNS001
// LBL LNSMAC // LFD MAC
// DVCVOL LNS001
// LBL LNSLOD // LFD LOD
// WORK1
// WORK2
// EXEC BEMSIM,LOD
/$
* NETWORK NAME
EDT
* ALLOWED TERMINALS
T001
T002
* USERS
USER GUES,0,GUES
USER SBOY,0,SBOY
/*
/&
// FIN