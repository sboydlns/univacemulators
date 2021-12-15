// JOB IMS,,10000,10000,4
// OPR ' <<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> '
// OPR ' OS/3 IMS/90 SINGLE-THREAD EXECUTION '
// OPR ' <<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> '
// OPTION  SYSDUMP
// DVC 20 // LFD PRNTR
// DVC 50 // VOL REL042 // LBL TCIDTF // LFD TCIDTF,,INIT
// DVC 50 // VOL REL042 // LBL AUDCONF // LFD AUDCONF
// DVC 50 // VOL REL042 // LBL CONDATA  // LFD CONDATA
// DVC 50 // VOL REL042 // LBL NAMEREC6  // LFD  NAMEREC
// DVC 50 // VOL REL042 // LBL CUSTOMER // LFD CUSTFIL
// EXEC  VSBST6,$Y$LOD
/&
// FIN
