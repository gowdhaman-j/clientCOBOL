000100 IDENTIFICATION DIVISION.                                               00
000200 PROGRAM-ID.    CALLDYNA.                                               00
000300 AUTHOR. R THORNTON                                                     00
000400 REMARKS. USED TO TEST CALLS TO DYNALLOC SUBROUTINE.                    00
000500 ENVIRONMENT DIVISION.                                                  00
000600 CONFIGURATION SECTION.                                                 00
000700 INPUT-OUTPUT SECTION.                                                  00
000800 FILE-CONTROL.                                                          00
000900     SELECT INPUT-FILE ASSIGN TO UT-S-INPUT1.                           00
001000     SELECT OUTPUT-FILE ASSIGN TO UT-S-OUTPUT1.                         00
001100 DATA DIVISION.                                                         00
001200 FILE SECTION.                                                          00
001300                                                                        00
001400 FD INPUT-FILE                                                          00
001500     RECORD CONTAINS 80 CHARACTERS                                      00
001600     RECORDING MODE IS F                                                00
001700     BLOCK CONTAINS 0 RECORDS                                           00
001800     LABEL RECORD IS STANDARD                                           00
001900     DATA RECORD IS INPUT-RECORD.                                       00
002000                                                                        00
002100 01  INPUT-RECORD.                                                      00
002200     05  FILLER              PIC X(80).                                 00
002300                                                                        00
002400 FD OUTPUT-FILE                                                         00
002500     RECORD CONTAINS 80 CHARACTERS                                      00
002600     RECORDING MODE IS F                                                00
002700     BLOCK CONTAINS 0 RECORDS                                           00
002800     LABEL RECORD IS STANDARD                                           00
002900     DATA RECORD IS INPUT-RECORD.                                       00
003000                                                                        00
003100 01  OUTPUT-RECORD.                                                     00
003200     05  FILLER              PIC X(80).                                 00
003300                                                                        00
003400 WORKING-STORAGE SECTION.                                               00
003500 77  FILLER PIC X(36)  VALUE                                            00
003600     'CALLDYNA WORKING STORAGE BEGINS HERE'.                            00
003700                                                                        00
003800 01  MISCELLANEOUS-AREAS.                                               00
003900     05 EOF-SWITCH               PIC X VALUE 'N'.                       00
004000        88 END-OF-INPUT          VALUE 'Y'.                             00
004100        88 MORE-INPUT            VALUE 'N'.                             00
004200                                                                        00
004300 01  ALLOCATE-DATA.                                                     00
004400     05 DDNAME                   PIC X(8).                              00
004500     05 DSNAME                   PIC X(44).                             00
004600     05 RELATIVE-GENERATION      PIC X(8).                              00
004700     05 CURRENT-STATUS           PIC X.                                 00
004800        88 STATUS-OLD            VALUE 'O'.                             00
004900        88 STATUS-MOD            VALUE 'M'.                             00
005000        88 STATUS-NEW            VALUE 'N'.                             00
005100        88 STATUS-SHR            VALUE 'S'.                             00
005200     05 NORMAL-DISPOSITION       PIC X.                                 00
005300        88 NORM-UNCATLG          VALUE 'U'.                             00
005400        88 NORM-CATLG            VALUE 'C'.                             00
005500        88 NORM-DELETE           VALUE 'D'.                             00
005600        88 NORM-KEEP             VALUE 'K'.                             00
005700     05 CONDITIONAL-DISPOSITION  PIC X.                                 00
005800        88 COND-UNCATLG          VALUE 'U'.                             00
005900        88 COND-CATLG            VALUE 'C'.                             00
006000        88 COND-DELETE           VALUE 'D'.                             00
006100        88 COND-KEEP             VALUE 'K'.                             00
006200     05 UNIT-NAME                PIC X(8).                              00
006300     05 UNIT-COUNT               PIC S9(4) COMP.                        00
006400     05 VOLUME-SERIAL            PIC X(6).                              00
006500     05 LABEL-TYPE               PIC X.                                 00
006600        88 NO-LABELS             VALUE 'N'.                             00
006700        88 STANDARD-LABELS       VALUE 'S'.                             00
006800        88 BYPASS-LABELS         VALUE 'B'.                             00
006900     05 DATASET-SEQUENCE         PIC S9(4) COMP.                        00
007000     05 FREE-CLOSE               PIC X.                                 00
007100        88 FREE-WHEN-CLOSED      VALUE 'F'.                             00
007200     05 RETENTION-PERIOD-DAYS    PIC S9(4) COMP.                        00
007300     05 RECORDING-MODE           PIC X.                                 00
007400        88 FIXED-RECORDS         VALUE 'F'.                             00
007500        88 VARIABLE-RECORDS      VALUE 'V'.                             00
007600        88 UNDEFINED-RECORDS     VALUE 'U'.                             00
007700        88 FIXED-STANDARD        VALUE 'S'.                             00
007800     05 BLOCKING                 PIC X.                                 00
007900        88 BLOCKED-RECORDS       VALUE 'B'.                             00
008000     05 CONTROL-CHARACTERS       PIC X.                                 00
008100        88 ASA-CONTROL-CHAR      VALUE 'A'.                             00
008200     05 LOGICAL-RECORD-LENGTH    PIC S9(4) COMP.                        00
008300     05 BLOCK-LENGTH             PIC S9(4) COMP.                        00
008400     05 TAPE-DENSITY             PIC X.                                 00
008500        88 800-BPI               VALUE '8'.                             00
008600        88 1600-BPI              VALUE '1'.                             00
008700        88 6250-BPI              VALUE '6'.                             00
008800     05 NUMBER-BUFFERS           PIC S9(4) COMP.                        00
008900     05 KEY-LENGTH               PIC S9(4) COMP.                        00
009000     05 DATASET-ORGANIZATION     PIC X.                                 00
009100        88 VSAM-DATASET          VALUE 'V'.                             00
009200        88 PARTITIONED-DATASET   VALUE 'P'.                             00
009300        88 DIRECT-DATASET        VALUE 'D'.                             00
009400        88 PHYSICAL-SEQUENTIAL   VALUE 'S'.                             00
009500     05 SPACE-TYPE               PIC X.                                 00
009600        88 CYLINDER-REQUEST      VALUE 'C'.                             00
009700        88 TRACK-REQUEST         VALUE 'T'.                             00
009800     05 PRIMARY-SPACE-AMOUNT     PIC S9(4) COMP.                        00
009900     05 SECONDARY-SPACE-AMOUNT   PIC S9(4) COMP.                        00
010000     05 RELEASE-SPACE            PIC X.                                 00
010100        88 RELEASE-UNUSED        VALUE 'R'.                             00
010200     05 NBR-DIRECTORY-BLOCKS     PIC S9(4) COMP.                        00
010300     05 EXPIRATION-DATE-YYDDD    PIC X(5).                              00
010400                                                                        00
010500 01  ALLOCATION-RESULT.                                                 00
010600     05 DYNALLOC-REQUEST         PIC X.                                 00
010700        88 ALLOCATION-REQUEST    VALUE 'A'.                             00
010800        88 VOLSER-REQUEST        VALUE 'V'.                             00
010900     05 DYNALLOC-RETURN-CODE     PIC XX.                                00
011000        88 SUCCESSFUL-ALLOCATION VALUE '00'.                            00
011100        88 ENVIRONMENT-ERROR     VALUE '04'.                            00
011200        88 VALIDATION-DENIAL     VALUE '08'.                            00
011300        88 PARAMETER-ERROR       VALUE '12'.                            00
011400     05 ERROR-REASON-CODE.                                              00
011500        10 CLASS-7-CODE              PIC X.                             00
011600        10 ERROR-CLASS               PIC X.                             00
011700           88 UNAVAILABLE-RESOURCE   VALUE '2'.                         00
011800           88 INVALID-PARAMETER-LIST VALUE '3'.                         00
011900           88 ERROR-IN-ENVIRONMENT   VALUE '4'.                         00
012000           88 SYSTEM-ROUTINE-ERROR   VALUE '7'.                         00
012100        10 SPECIFIC-ERROR-CODE       PIC XX.                            00
012200     05 INFORMATION-REASON           PIC X(4).                          00
012300                                                                        00
012400 PROCEDURE DIVISION.                                                    00
012500                                                                        00
012600 A100-EXECUTIVE-CONTROL.                                                00
012700     PERFORM A100-INITIALIZATION.                                       00
012800     PERFORM B100-MAINLINE-PROCESSING UNTIL END-OF-INPUT.               00
012900     PERFORM Z100-END-OF-PROCESSING.                                    00
013000     GOBACK.                                                            00
013100                                                                        00
013200 A100-INITIALIZATION.                                                   00
013300     MOVE LOW-VALUES TO ALLOCATE-DATA.                                  00
013400     MOVE 'OUTPUT1' TO DDNAME.                                          00
013500     MOVE 'USER02.GDGTEST.DATA' TO DSNAME.                              00
013600     MOVE '+1' TO RELATIVE-GENERATION.                                  00
013700     MOVE 'N' TO CURRENT-STATUS.                                        00
013800     MOVE 'C' TO NORMAL-DISPOSITION.                                    00
013900     MOVE 'D' TO CONDITIONAL-DISPOSITION.                               00
014000     MOVE 'DISK' TO UNIT-NAME.                                          00
014100     MOVE 'EMP810' TO VOLUME-SERIAL.                                    00
014200     MOVE 'S' TO DATASET-ORGANIZATION.                                  00
014300     MOVE 'T' TO SPACE-TYPE.                                            00
014400     MOVE 5 TO PRIMARY-SPACE-AMOUNT.                                    00
014500     MOVE 1 TO SECONDARY-SPACE-AMOUNT.                                  00
014600     MOVE 'R' TO RELEASE-SPACE.                                         00
014700     MOVE 'F' TO RECORDING-MODE.                                        00
014800     MOVE 'B' TO BLOCKING.                                              00
014900     MOVE 80 TO LOGICAL-RECORD-LENGTH.                                  00
015000     MOVE 23440 TO BLOCK-LENGTH.                                        00
015100     CALL 'DYNALLOC' USING ALLOCATE-DATA, ALLOCATION-RESULT.            00
015200     IF SUCCESSFUL-ALLOCATION                                           00
015300         NEXT SENTENCE                                                  00
015400     ELSE DISPLAY 'UNABLE TO ALLOCATE OUTPUT1, RETURN CODE '            00
015500                   DYNALLOC-RETURN-CODE ' ERROR REASON CODE '           00
015600                   ERROR-REASON-CODE,                                   00
015700          CALL 'COBABEND'.                                              00
015800     OPEN INPUT INPUT-FILE OUTPUT OUTPUT-FILE.                          00
015900                                                                        00
016000 B100-MAINLINE-PROCESSING.                                              00
016100     PERFORM C100-READ-FILE.                                            00
016200     IF MORE-INPUT PERFORM D100-WRITE-FILE.                             00
016300                                                                        00
016400 C100-READ-FILE.                                                        00
016500     READ INPUT-FILE AT END MOVE 'Y' TO EOF-SWITCH.                     00
016600                                                                        00
016700 D100-WRITE-FILE.                                                       00
016800     WRITE OUTPUT-RECORD FROM INPUT-RECORD.                             00
016900                                                                        00
017000 Z100-END-OF-PROCESSING.                                                00
017100     CLOSE INPUT-FILE OUTPUT-FILE.                                      00
  