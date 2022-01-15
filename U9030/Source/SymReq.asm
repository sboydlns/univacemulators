SYMREQ   START
*
* THIS WAS AN ATTEMPT TO USE SVC 65 TO START A JOB IN A WAY SIMILAR
* TO WHAT OS/3 DOES WHEN YOU ENTER AN 'RV' COMMAND. IT DOESN'T WORK
* AND I HAVEN'T BOTHERED TO TRACE THROUGH THE SUPERVISOR TO FIGURE OUT
* WHY.
*
         PRINT NOGEN
         SUPEQU
*         
         BALR  R2,0
         USING *,R2
*         
         CNOP  0,4
         SVC   65
*         
         EOJ
*         

         DC    A(SQPKT)         
         DC    2F'0'
SQPKT    DS    XL(QS$SRBL)             ALLOC SYM REQ PACKET 
*
         ORG   SQPKT                   POPULATE PKT WITH 'RV SYSDUMP'
         DC    CL2'RV'
         DC    CL6'JL$$RU'
         DC    H'0'
         DC    XL1'0'
         DC    XL1'0'
         DC    XL1'4'
         DC    XL1'3'
         DC    XL1'40'
         DC    XL1'0'
         DC    H'78'
         DC    XL1'0'
         DC    XL3'000003'
         DC    H'0'
         DC    F'0'
         DC    CL28'SYSDUMPO'
*         
         ORG   SQPKT+QS$SRBL
*         
         END   SYMREQ
// FIN         