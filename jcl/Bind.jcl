//NCBIND JOB ,                                                          JOB04417
// MSGCLASS=H,MSGLEVEL=(1,1),TIME=(,4),REGION=144M,COND=(16,LT)
//PROCS JCLLIB ORDER=(CUST.V50.PROCLIB)
//*
// SET HLQ=SUMANG
// SET DB2HLQ=DSN.V12R1M0
//*
//BIND    EXEC PGM=IKJEFT01,DYNAMNBR=20
//STEPLIB  DD  DSN=&DB2HLQ..SDSNLOAD,DISP=SHR
//DBRMLIB  DD  DSN=SUMANG.DBBW.DBRM,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSTSPRT DD  SYSOUT=*
//SYSUDUMP DD  SYSOUT=*
//SYSIN  DD *
/*
//SYSTSIN DD *
DSN SYSTEM(DB2C)
BIND PACKAGE (CUST0W)                                    -
     ISO(CS)                                              -
     CURRENTDATA(NO)                                      -
     MEMBER(GETAAVG)                                      -
     DEGREE(1)                                            -
     DYNAMICRULES(BIND)                                   -
     ACTION (REPLACE)                                     -
     EXPLAIN(NO)                                          -
     QUALIFIER(GENASA14)                                   -
     ENABLE(BATCH)                                   -
     REL(DEALLOCATE)                                      -
     VALIDATE(BIND)
BIND PLAN (NEWCPL)                                       -
     PKLIST(*.CUST0W.*)                                  -
     CURRENTDATA(NO)                                      -
     ISO(CS)                                              -
     ACTION (REPLACE)                                        -
     QUALIFIER(GENASA14)                                   -
     REL(DEALLOCATE)                                      -
     ACQUIRE(USE)                                         -
     RETAIN                                               -
     NOREOPT(VARS)                                        -
     VALIDATE(BIND)
RUN  PROGRAM(DSNTIAD) PLAN(DSNTIA12) -
        LIB('DSN12C94.RUNLIB.LOAD')
END
/*