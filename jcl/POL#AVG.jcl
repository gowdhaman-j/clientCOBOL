//SUMANG1 JOB ,
// MSGCLASS=H,MSGLEVEL=(1,1),TIME=NOLIMIT,REGION=0M,COND=(16,LT)
//        JCLLIB ORDER=(CUST.V50.PROCLIB)
//*
//*
//STPPRE  EXEC PGM=IEFBR14
//DD1     DD DSN=SUMANG.CUSTPOL.WCA.OUTFILE,DISP=(MOD,DELETE,DELETE),
//           DCB=(LRECL=30,RECFM=FB),
//           UNIT=SYSDA,SPACE=(TRK,(1,1),RLSE)
//*
//TEST001 EXEC PGM=IKJEFT01
//STEPLIB DD DSN=SUMANG.DBBW.LOAD,DISP=SHR
//        DD DSN=DSN.V11R1M0.SDSNLOAD,DISP=SHR
//INFILE   DD DSN=SUMANG.CUSTPOL.WCA.INFILE,DISP=SHR
//OUTFILE  DD DSN=SUMANG.CUSTPOL.WCA.OUTFILE,
//            DISP=(,CATLG,DELETE),DCB=(LRECL=30,RECFM=FB),
//            UNIT=SYSDA,SPACE=(TRK,(1,1),RLSE)
//SYSTSIN  DD *
    DSN SYSTEM(DB2C)
      RUN PROGRAM(GET1AVG)-
      PLAN(NEWCPL)-
      LIBRARY('SUMANG.DBBW.LOAD')
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SYSTSPRT DD SYSOUT=*
//*