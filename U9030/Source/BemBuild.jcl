// JOB BEMBUILD,,10000,10000
// DVC 20 // LFD PRNTR
// DVCVOL LNS001
// LBL LNSLOD // LFD LOD
// DVCVOL LNS001
// LBL LNSOBJ // LFD OBJ
//BADCMD ASM   IN=(LNS001,LNSSRC),                                     X
//1            LIN=(LNS001,LNSMAC),                                    X
//2            OUT=(LNS001,LNSOBJ),                                    X
//3            LST=(NC)
// WORK1
// EXEC LNKEDT
// PARAM OUT=LOD
/$
     LOADM BADCMD
     INCLUDE BADCMD,OBJ
/*     
// NOP
// NOP
//BEMDEL ASM   IN=(LNS001,LNSSRC),                                     X
//1            LIN=(LNS001,LNSMAC),                                    X
//2            OUT=(LNS001,LNSOBJ),                                    X
//3            LST=(NC)
// WORK1
// EXEC LNKEDT
// PARAM OUT=LOD
/$
     LOADM BEMDEL
     INCLUDE BEMDEL,OBJ
/*     
// NOP
// NOP
//BEMDSP ASM   IN=(LNS001,LNSSRC),                                     X
//1            LIN=(LNS001,LNSMAC),                                    X
//2            OUT=(LNS001,LNSOBJ),                                    X
//3            LST=(NC)
// WORK1
// EXEC LNKEDT
// PARAM OUT=LOD
/$
     LOADM BEMDSP
     INCLUDE BEMDSP,OBJ
/*     
// NOP
// NOP
//BEMFST ASM   IN=(LNS001,LNSSRC),                                     X
//1            LIN=(LNS001,LNSMAC),                                    X
//2            OUT=(LNS001,LNSOBJ),                                    X
//3            LST=(NC)
// WORK1
// EXEC LNKEDT
// PARAM OUT=LOD
/$
     LOADM BEMFST
     INCLUDE BEMFST,OBJ
/*     
// NOP
// NOP
//BEMHLP ASM   IN=(LNS001,LNSSRC),                                     X
//1            LIN=(LNS001,LNSMAC),                                    X
//2            OUT=(LNS001,LNSOBJ),                                    X
//3            LST=(NC)
// WORK1
// EXEC LNKEDT
// PARAM OUT=LOD
/$
     LOADM BEMHLP
     INCLUDE BEMHLP,OBJ
/*     
// NOP
// NOP
//BEMLGF ASM   IN=(LNS001,LNSSRC),                                     X
//1            LIN=(LNS001,LNSMAC),                                    X
//2            OUT=(LNS001,LNSOBJ),                                    X
//3            LST=(NC)
// WORK1
// EXEC LNKEDT
// PARAM OUT=LOD
/$
     LOADM BEMLGF
     INCLUDE BEMLGF,OBJ
/*     
// NOP
// NOP
//BEMLGN ASM   IN=(LNS001,LNSSRC),                                     X
//1            LIN=(LNS001,LNSMAC),                                    X
//2            OUT=(LNS001,LNSOBJ),                                    X
//3            LST=(NC)
// WORK1
// EXEC LNKEDT
// PARAM OUT=LOD
/$
     LOADM BEMLGN
     INCLUDE BEMLGN,OBJ
/*     
// NOP
// NOP
//BEMPRT ASM   IN=(LNS001,LNSSRC),                                     X
//1            LIN=(LNS001,LNSMAC),                                    X
//2            OUT=(LNS001,LNSOBJ),                                    X
//3            LST=(NC)
// WORK1
// EXEC LNKEDT
// PARAM OUT=LOD
/$
     LOADM BEMPRT
     INCLUDE BEMPRT,OBJ
/*     
// NOP
// NOP
//BEMRDY ASM   IN=(LNS001,LNSSRC),                                     X
//1            LIN=(LNS001,LNSMAC),                                    X
//2            OUT=(LNS001,LNSOBJ),                                    X
//3            LST=(NC)
// WORK1
// EXEC LNKEDT
// PARAM OUT=LOD
/$
     LOADM BEMRDY
     INCLUDE BEMRDY,OBJ
/*     
// NOP
// NOP
//BEMSTA ASM   IN=(LNS001,LNSSRC),                                     X
//1            LIN=(LNS001,LNSMAC),                                    X
//2            OUT=(LNS001,LNSOBJ),                                    X
//3            LST=(NC)
// WORK1
// EXEC LNKEDT
// PARAM OUT=LOD
/$
     LOADM BEMSTA
     INCLUDE BEMSTA,OBJ
/*     
// NOP
// NOP
//BEMVTC ASM   IN=(LNS001,LNSSRC),                                     X
//1            LIN=(LNS001,LNSMAC),                                    X
//2            OUT=(LNS001,LNSOBJ),                                    X
//3            LST=(NC)
// WORK1
// EXEC LNKEDT
// PARAM OUT=LOD
/$
     LOADM BEMVTC
     INCLUDE BEMVTC,OBJ
/*     
// NOP
// NOP
//BEMPRNTR ASM IN=(LNS001,LNSSRC),                                     X
//1            LIN=(LNS001,LNSMAC),                                    X
//2            OUT=(LNS001,LNSOBJ),                                    X
//3            LST=(NC)
// NOP
// NOP
//BEMLIB ASM   IN=(LNS001,LNSSRC),                                     X
//1            LIN=(LNS001,LNSMAC),                                    X
//2            OUT=(LNS001,LNSOBJ),                                    X
//3            LST=(NC)
// NOP
// NOP
//BEMSIM ASM   IN=(LNS001,LNSSRC),                                     X
//1            LIN=(LNS001,LNSMAC),                                    X
//2            OUT=(LNS001,LNSOBJ),                                    X
//3            LST=(NC)
// NOP
// NOP
// WORK1
// EXEC LNKEDT
// PARAM OUT=LOD
// PARAM NOAUTO
/$
     LOADM BEMSIM
     INCLUDE BEMSIM,OBJ
     INCLUDE BEMLIB,OBJ
     INCLUDE BEMPRNTR,OBJ
     INCLUDE PR$IOE,$Y$OBJ
/*     
/&
// FIN