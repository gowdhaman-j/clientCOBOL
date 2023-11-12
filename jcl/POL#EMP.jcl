//<HLQ>1 JOB ,
// MSGCLASS=H,MSGLEVEL=(1,1),TIME=NOLIMIT,REGION=0M,COND=(16,LT)
//        JCLLIB ORDER=(CUST.V50.PROCLIB)
//*
//* This program utilizes the tables from Genapp.
//******************************************************************
//*                                                                *
//* LICENSED MATERIALS - PROPERTY OF IBM                           *
//*                                                                *
//* "RESTRICTED MATERIALS OF IBM"                                  *
//*                                                                *
//* (C) COPYRIGHT IBM CORP. 2021       ALL RIGHTS RESERVED         *
//*                                                                *
//* US GOVERNMENT USERS RESTRICTED RIGHTS - USE, DUPLICATION,      *
//* OR DISCLOSURE RESTRICTED BY GSA ADP SCHEDULE                   *
//* CONTRACT WITH IBM CORPORATION                                  *
//*                                                                *
//******************************************************************
//*
//STPPRE  EXEC PGM=IEFBR14
//DD1     DD DSN=<HLQ>.CUSTPOL.OUTFILE,DISP=(MOD,DELETE,DELETE),
//           DCB=(LRECL=30,RECFM=FB),
//           UNIT=SYSDA,SPACE=(TRK,(1,1),RLSE)
//*
//STP0000 EXEC PGM=IKJEFT01,COND=(0,NE)
//STEPLIB DD DSN=<HLQ>.DBBZUNIT.LOAD,DISP=SHR
//        DD DSN=DSN.V11R1M0.SDSNLOAD,DISP=SHR
//INFILE   DD DSN=<HLQ>.CUSTPOL.INFILE,DISP=SHR
//OUTFILE  DD DSN=<HLQ>.CUSTPOL.OUTFILE,
//            DISP=(,CATLG,DELETE),DCB=(LRECL=30,RECFM=FB),
//            UNIT=SYSDA,SPACE=(TRK,(1,1),RLSE)
//SYSTSIN  DD *
    DSN SYSTEM(DB2C)
      RUN PROGRAM(CALCAVG)-
      PLAN(NEWCPL)-
      LIBRARY('<HLQ>.DBBZUNIT.LOAD')
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SYSTSPRT DD SYSOUT=*