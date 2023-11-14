       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. XFOOT.                                               00020000
       AUTHOR.                                                          00030000
       DATE-WRITTEN.  SEPT, 2001                                        00040000
       DATE-COMPILED.                                                   00050000
      ******************************************************************00060000
      *                                                                 00070000
      ******************************************************************00080000
       ENVIRONMENT DIVISION.                                            00090000
       CONFIGURATION SECTION.                                           00100000
       SPECIAL-NAMES.  C01 IS TOP-OF-FORM.                              00110000
       INPUT-OUTPUT SECTION.                                            00120000
       FILE-CONTROL.                                                    00130000
            SELECT MEDICAL-FILE       ASSIGN TO UT-S-INPUT1.            00140000
       DATA DIVISION.                                                   00160000
       FILE SECTION.                                                    00170000
       FD  MEDICAL-FILE                                                 00180000
           BLOCK CONTAINS 0 RECORDS                                     00190000
           RECORDING MODE IS V                                          00200000
           LABEL RECORDS ARE STANDARD.                                  00210000
       01  MEDICAL-RECORD.                                              00220000
           05 MEDICAL-HEADER.                                           00230000
              10 MEDICAL-ID        PIC  X(4).                           00240000
              10 FILLER            PIC  X(1481).                        00250000
           05 MEDICAL-LINE-COUNT   PIC S9(2)    COMP.                   00260000
           05 MEDICAL-OTHER-DATA   PIC X(221).                          00270000
           05 MEDICAL-LINE OCCURS 1 TO 15 TIMES                         00280000
                           DEPENDING ON MEDICAL-LINE-COUNT.             00290000
              10 MEDICAL-LINE-DATA PIC X(698).                          00300000
                                                                        00370000
       WORKING-STORAGE SECTION.                                         00380000
       01  WS-END-MED-FILE          PIC X VALUE ZEROS.                  00380100
           88  END-OF-MED-FILE            VALUE '1'.                    00380200
       01  WS-END-REV-FILE          PIC X VALUE ZEROS.                  00380300
           88  END-OF-REV-FILE            VALUE '1'.                    00380400
       01  WS-NUMERIC-CLAIM-88      PIC X VALUE ZEROS.                  00380500
           88  WS-NUMERIC-CLAIM           VALUE '0'.                    00380600
           88  WS-NON-NUMERIC-CLAIM       VALUE '1'.                    00380700
                                                                        00380800
       01  WS-DATA-AREA.                                                00381000
           05   WS-TMP-CALC-AREA    PIC S9(09).                         00382000
           05   WS-TMP-DIFFERENCE   PIC S9(09).                         00382100
           05   WS-CNT              PIC 9(05).                          00382200
           05   WS-CNT1             PIC 9(05).                          00382300
           05   WS-SUB              PIC 9(03).                          00383000
                                                                        00383100
           05   WS-TST-DTA.                                             00383200
               15   WS-TST-NC-AMT              PIC 9(9)V99.             00383400
               15   WS-TST-BASE-DED-AMT        PIC 9(9)V99.             00383500
               15   WS-TST-MM-DED-AMT          PIC 9(9)V99.             00383700
               15   WS-TST-A-COINS-BASE-LN     PIC 9(9)V99.             00383900
               15   WS-TST-A-COINS-MM-LN       PIC 9(9)V99.             00384100
               15   WS-TST-BASE-COIN-PEN-AMT   PIC 9(9)V99.             00384300
               15   WS-TST-MM-COIN-PEN-AMT     PIC 9(9)V99.             00384500
               15   WS-TST-BASE-AMT            PIC 9(9)V99.             00384700
               15   WS-TST-MM-AMT              PIC 9(9)V99.             00384900
               15   WS-TST-A-NEGOT-DISC        PIC 9(9)V99.             00385100
                                                                        00385200
           05   WS-DSP-DTA.                                             00385300
               15   WS-DSP-CHARGE              PIC ZZZZZ9.99.           00386000
               15   WS-DSP-DIFFERENCE          PIC -----9.99.           00387000
               15   WS-DSP-NC-AMT              PIC ZZZZZ9.99.           00388000
               15   WS-DSP-BASE-DED-AMT        PIC ZZZZZ9.99.           00389000
               15   WS-DSP-MM-DED-AMT          PIC ZZZZZ9.99.           00389100
               15   WS-DSP-A-COINS-BASE-LN     PIC ZZZZZ9.99.           00389200
               15   WS-DSP-A-COINS-MM-LN       PIC ZZZZZ9.99.           00389300
               15   WS-DSP-BASE-COIN-PEN-AMT   PIC ZZZZZ9.99.           00389400
               15   WS-DSP-MM-COIN-PEN-AMT     PIC ZZZZZ9.99.           00389500
               15   WS-DSP-BASE-AMT            PIC ZZZZZ9.99.           00389600
               15   WS-DSP-MM-AMT              PIC ZZZZZ9.99.           00389700
               15   WS-DSP-A-NEGOT-DISC        PIC ZZZZZ9.99.           00389800
                                                                        00389900
           05   WS-DSP-HDR.                                             00390000
               15   WS-FILLER       PIC X(11) VALUE '           '.      00390100
               15   WS-FILLER       PIC X(09) VALUE '*-CHARGE*'.        00390200
               15   WS-FILLER       PIC X(09) VALUE '*--DIFF-*'.        00390300
               15   WS-FILLER       PIC X(09) VALUE '*-NC AMT*'.        00390400
               15   WS-FILLER       PIC X(09) VALUE '*-DED BA*'.        00390500
               15   WS-FILLER       PIC X(09) VALUE '*-DED MM*'.        00390600
               15   WS-FILLER       PIC X(09) VALUE '*-COI BS*'.        00390700
               15   WS-FILLER       PIC X(09) VALUE '*-COI MM*'.        00390800
               15   WS-FILLER       PIC X(09) VALUE '*-PEN BS*'.        00390900
               15   WS-FILLER       PIC X(09) VALUE '*-PEN MM*'.        00391000
               15   WS-FILLER       PIC X(09) VALUE '*-AMT BS*'.        00391100
               15   WS-FILLER       PIC X(09) VALUE '*-AMT MM*'.        00391200
               15   WS-FILLER       PIC X(09) VALUE '*-NG DIS*'.        00391300
       COPY CHPMED.                                                     00430000
                                                                        00450000
       PROCEDURE DIVISION.                                              00470000
       01000-MAINLINE.                                                  00471000
      ******************************************************************00480000
      *    OPEN  INPUT MEDICAL-FILE, REVENUE-FILE                       00490000
      *    CLOSE MEDICAL-FILE, REVENUE-FILE.                            00491000
      ******************************************************************00501000
           OPEN INPUT MEDICAL-FILE                                      00530000
           PERFORM                                                      00560000
              UNTIL END-OF-MED-FILE                                     00560100
                                                                        00560200
                READ   MEDICAL-FILE                                     00561000
                  AT END                                                00562000
                     SET END-OF-MED-FILE TO TRUE                        00563000
                                                                        00563100
                  NOT AT END                                            00563200
                     ADD 1 TO WS-CNT                                    00563300
                     ADD 1 TO WS-CNT1                                   00563400
                     IF WS-CNT1 = 20                                    00563500
                        MOVE 1 TO WS-CNT1                               00563600
                        DISPLAY   WS-DSP-HDR                            00563700
                     END-IF                                             00563800
                                                                        00563900
                     MOVE MEDICAL-RECORD TO PAYM-PAYMENT-RECORD         00564000
                                                                        00564100
                     EVALUATE TRUE                                      00564200
                        WHEN WS-CNT > 70                                00564300
                             SET END-OF-MED-FILE TO TRUE                00564400
                                                                        00564500
                        WHEN MEDICAL-ID NOT = 'PAYM'                    00564600
      *                      DISPLAY 'M11.R', WS-CNT                    00564700
      *                              ' IS NOT A PAYM: ID = ' MEDICAL-ID 00564800
                             CONTINUE                                   00564900
                        WHEN PAYM-VOID-ORIG                             00565000
                        WHEN PAYM-VOID-SUBSEQ                           00565100
      *                      DISPLAY 'M12.R', WS-CNT ' IS A VOID'       00565300
                             CONTINUE                                   00565400
                                                                        00565500
                        WHEN PAYM-OVR-CD(1) = 'P'                       00565600
      *                      DISPLAY 'M13.R', WS-CNT ' IS PENDED'       00565700
                             CONTINUE                                   00565800
                                                                        00566000
                        WHEN OTHER                                      00566100
                             PERFORM 20000-XFOOT-THE-CLAIM              00566200
                     END-EVALUATE                                       00566300
                END-READ                                                00566400
                                                                        00567000
           END-PERFORM                                                  00570100
                                                                        00571000
           CLOSE MEDICAL-FILE                                           00590000
           GOBACK.                                                      00600000
                                                                        00601000
                                                                        00610000
       20000-XFOOT-THE-CLAIM.                                           00920000
      ******************************************************************00930000
      *                                                                 00940000
      *    PROFESSIONAL/SEC PROF/SEC FAC                                01242000
      *                                                                 01243000
      *    A_TOT_CHG =                                                  01260000
      *      A_NON_COVD                                                 01270000
      *    + A_DEDUCT_BASE/MM                                           01271000
      *    + A_COINS_BASE/MM                                            01280000
      *    + A_PEN_COIN_BASE/MM                                         01280100
      *    + A_COPAY_BASE/MM                                            01280200
      *    + A_PAID_BASE/MM                                             01280300
      *                                                                 01280400
      *    + COB-SAVNGS                                                 01280500
      *                                                                 01280600
      *    + A_PROF_DISC                                                01301000
      *    + A_NEGT_DISC                                                01302000
      *                                                                 01303000
      ******************************************************************01304000
           PERFORM                                                      01304100
           VARYING WS-SUB   FROM 1 BY 1                                 01304200
           UNTIL   WS-SUB > PAYM-LINE-COUNT                             01304300
                                                                        01304400
              PERFORM 70000-CHK-4-NUMERIC                               01304500
              IF WS-NUMERIC-CLAIM                                       01304600
                 COMPUTE WS-TMP-CALC-AREA                               01309000
                    =   PAYM-NC-AMT             (WS-SUB)                01310100
                      + PAYM-BASE-DED-AMT       (WS-SUB)                01310300
                      + PAYM-MM-DED-AMT         (WS-SUB)                01310400
                      + PAYM-A-COINS-BASE-LN    (WS-SUB)                01330000
                      + PAYM-A-COINS-MM-LN      (WS-SUB)                01331000
                      + PAYM-BASE-COIN-PEN-AMT  (WS-SUB)                01340000
                      + PAYM-MM-COIN-PEN-AMT    (WS-SUB)                01341000
                      + PAYM-BASE-AMT           (WS-SUB)                01350000
                      + PAYM-MM-AMT             (WS-SUB)                01351000
                      + PAYM-A-NEGOT-DISC       (WS-SUB)                01360000
                 END-COMPUTE                                            01360100
                                                                        01360200
                 COMPUTE WS-TMP-DIFFERENCE                              01360300
                    =   PAYM-CHARGE   (WS-SUB)                          01360400
                      - WS-TMP-CALC-AREA                                01360500
                 END-COMPUTE                                            01360600
                                                                        01360700
                 IF WS-TMP-DIFFERENCE NOT = 0                           01360800
                    PERFORM 30000-FMT-N-DISP                            01360900
                 END-IF                                                 01361000
                                                                        01361100
              END-IF                                                    01361200
                                                                        01361300
           END-PERFORM.                                                 01362000
                                                                        01362100
                                                                        01362200
       30000-FMT-N-DISP.                                                01362300
      ******************************************************************01362400
      *                                                                 01362500
      ******************************************************************01362600
           MOVE PAYM-CHARGE            (WS-SUB)                         01363000
             TO WS-DSP-CHARGE                                           01364000
                                                                        01365500
           MOVE PAYM-NC-AMT            (WS-SUB)                         01366000
             TO WS-DSP-NC-AMT                                           01366100
           MOVE PAYM-BASE-DED-AMT      (WS-SUB)                         01367000
             TO WS-DSP-BASE-DED-AMT                                     01367100
           MOVE PAYM-MM-DED-AMT        (WS-SUB)                         01368000
             TO WS-DSP-MM-DED-AMT                                       01368100
           MOVE PAYM-A-COINS-BASE-LN   (WS-SUB)                         01369000
             TO WS-DSP-A-COINS-BASE-LN                                  01369100
           MOVE PAYM-A-COINS-MM-LN     (WS-SUB)                         01370000
             TO WS-DSP-A-COINS-MM-LN                                    01371000
           MOVE PAYM-BASE-COIN-PEN-AMT (WS-SUB)                         01380000
             TO WS-DSP-BASE-COIN-PEN-AMT                                01381000
           MOVE PAYM-MM-COIN-PEN-AMT   (WS-SUB)                         01390000
             TO WS-DSP-MM-COIN-PEN-AMT                                  01391000
           MOVE PAYM-BASE-AMT          (WS-SUB)                         01400000
             TO WS-DSP-BASE-AMT                                         01401000
           MOVE PAYM-MM-AMT            (WS-SUB)                         01410000
             TO WS-DSP-MM-AMT                                           01411000
           MOVE PAYM-A-NEGOT-DISC      (WS-SUB)                         01420000
             TO WS-DSP-A-NEGOT-DISC.                                    01420100
                                                                        01420200
           DISPLAY 'M30.R', WS-CNT, ' ', WS-DSP-DTA.                    01428100
                                                                        01428200
      *          , ' ,'  WS-DSP-CHARGE                                  01429000
      *          , ' ,', WS-DSP-DIFFERENCE                              01440000
      *          , ' ,', WS-DSP-NC-AMT                                  01460000
      *          , ' ,', WS-DSP-BASE-DED-AMT                            01470000
      *          , ' ,', WS-DSP-MM-DED-AMT                              01480000
      *          , ' ,', WS-DSP-A-COINS-BASE-LN                         01490000
      *          , ' ,', WS-DSP-A-COINS-MM-LN                           01500000
      *          , ' ,', WS-DSP-BASE-COIN-PEN-AMT                       01510000
      *          , ' ,', WS-DSP-MM-COIN-PEN-AMT                         01520000
      *          , ' ,', WS-DSP-BASE-AMT                                01530000
      *          , ' ,', WS-DSP-MM-AMT                                  01540000
      *          , ' ,', WS-DSP-A-NEGOT-DISC.                           01550000
                                                                        01560000
                                                                        01710000
       70000-CHK-4-NUMERIC.                                             01720000
      ******************************************************************01721000
      *                                                                 01722000
      ******************************************************************01723000
           SET WS-NUMERIC-CLAIM      TO TRUE                            01731000
                                                                        01731100
           MOVE PAYM-NC-AMT            (WS-SUB)                         01732000
             TO WS-TST-NC-AMT                                           01732100
           MOVE PAYM-BASE-DED-AMT      (WS-SUB)                         01732200
             TO WS-TST-BASE-DED-AMT                                     01732300
           MOVE PAYM-MM-DED-AMT        (WS-SUB)                         01732400
             TO WS-TST-MM-DED-AMT                                       01732500
           MOVE PAYM-A-COINS-BASE-LN   (WS-SUB)                         01732600
             TO WS-TST-A-COINS-BASE-LN                                  01732700
           MOVE PAYM-A-COINS-MM-LN     (WS-SUB)                         01732800
             TO WS-TST-A-COINS-MM-LN                                    01732900
           MOVE PAYM-BASE-COIN-PEN-AMT (WS-SUB)                         01733000
             TO WS-TST-BASE-COIN-PEN-AMT                                01733100
           MOVE PAYM-MM-COIN-PEN-AMT   (WS-SUB)                         01733200
             TO WS-TST-MM-COIN-PEN-AMT                                  01733300
           MOVE PAYM-BASE-AMT          (WS-SUB)                         01733400
             TO WS-TST-BASE-AMT                                         01733500
           MOVE PAYM-MM-AMT            (WS-SUB)                         01733600
             TO WS-TST-MM-AMT                                           01733700
           MOVE PAYM-A-NEGOT-DISC      (WS-SUB)                         01733800
             TO WS-TST-A-NEGOT-DISC                                     01733900
                                                                        01734000
           EVALUATE TRUE                                                01735000
               WHEN WS-TST-NC-AMT             IS NOT NUMERIC            01750000
                    SET WS-NON-NUMERIC-CLAIM  TO TRUE                   01751000
                    DISPLAY 'M70.R', WS-CNT,    'NOT NUMERIC'           01752000
               WHEN WS-TST-BASE-DED-AMT       IS NOT NUMERIC            01760000
                    SET WS-NON-NUMERIC-CLAIM  TO TRUE                   01761000
                    DISPLAY 'M71.R', WS-CNT,    'NOT NUMERIC'           01762000
               WHEN WS-TST-MM-DED-AMT         IS NOT NUMERIC            01770000
                    SET WS-NON-NUMERIC-CLAIM  TO TRUE                   01771000
                    DISPLAY 'M72.R', WS-CNT,    'NOT NUMERIC'           01772000
               WHEN WS-TST-A-COINS-BASE-LN    IS NOT NUMERIC            01780000
                    SET WS-NON-NUMERIC-CLAIM  TO TRUE                   01781000
                    DISPLAY 'M73.R', WS-CNT,    'NOT NUMERIC'           01782000
               WHEN WS-TST-A-COINS-MM-LN      IS NOT NUMERIC            01790000
                    SET WS-NON-NUMERIC-CLAIM  TO TRUE                   01791000
                    DISPLAY 'M74.R', WS-CNT,    'NOT NUMERIC'           01792000
               WHEN WS-TST-BASE-COIN-PEN-AMT  IS NOT NUMERIC            01800000
                    SET WS-NON-NUMERIC-CLAIM  TO TRUE                   01801000
                    DISPLAY 'M75.R', WS-CNT,    'NOT NUMERIC'           01802000
               WHEN WS-TST-MM-COIN-PEN-AMT    IS NOT NUMERIC            01810000
                    SET WS-NON-NUMERIC-CLAIM  TO TRUE                   01811000
                    DISPLAY 'M76.R', WS-CNT,    'NOT NUMERIC'           01812000
               WHEN WS-TST-BASE-AMT           IS NOT NUMERIC            01820000
                    SET WS-NON-NUMERIC-CLAIM  TO TRUE                   01821000
                    DISPLAY 'M77.R', WS-CNT,    'NOT NUMERIC'           01822000
               WHEN WS-TST-MM-AMT             IS NOT NUMERIC            01830000
                    SET WS-NON-NUMERIC-CLAIM  TO TRUE                   01831000
                    DISPLAY 'M78.R', WS-CNT,    'NOT NUMERIC'           01832000
               WHEN WS-TST-A-NEGOT-DISC       IS NOT NUMERIC            01840000
                    SET WS-NON-NUMERIC-CLAIM  TO TRUE                   01841000
                    DISPLAY 'M79.R', WS-CNT,    'NOT NUMERIC'           01842000
           END-EVALUATE.                                                01850000
  