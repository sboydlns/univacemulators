// JOB PROCS
// DVC 20 // LFD PRNTR
// WORK1
// WORK2
// EXEC ASM
// PARAM LST=(NC)
/$
PROCS    START

         BALR  12,0
         USING *,12
       
         XDSECTS CPIOCP,MCT,MSGPRE
         SUPEQU
       
         OPR   (1),(0)
         OPR   (1),60
         OPR   MSG
         OPR   MSG,30,TCSERR                 
         OPR   MSG,,,REPLY,RESP,60
       
         STXIT PC
         STXIT AB
         STXIT IT
         STXIT OC
       
         STXIT PC,ENTPT,SA
         STXIT AB,ENTPT,SA
         STXIT IT,ENTPT,SA
         STXIT OC,ENTPT,SA,RESP,60
       
         CCRCALL MSG
         CCRCALL MSG,IRL=NO
         CCRCALL MSG,IRL=YES
         CCRCALL (1)
         CCRCALL (1),IRL=NO
         CCRCALL (1),IRL=YES
       
         LNEREL LNE1
         LNEREQ LNE1
         MOPEN  NET6,ERRET=TCSERR,TCSADDR=TCS
         NETREL NET6
         MREAD  TTT001
         MWRITE TTT002
         CYIELD
         CAWAKE
         
         SETIME 100,WAIT,M
         SETIME 100,WAIT,S
         SETIME 100,,S
         SETIME (1) 
       
TCSERR   EQU   *      
ENTPT    EQU   *
SA       EQU   *
TCSCMP   EQU   *
INOTE    EQU   *
       
MSG      DC    CL60'TEST'
RESP     DC    CL60' '
TCS      MTABLE TERMS=2,                                               X
               CCA=NET6,                                               X
               COMPL=TCSCMP,                                           X
               ERROR=TCSERR,                                           X
               INOTICE=INOTE                                      

         END
/*
/&
// FIN              