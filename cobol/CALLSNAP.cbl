000100 IDENTIFICATION DIVISION.                                               00
000200 PROGRAM-ID.    CALLSNAP.                                               00
000300 AUTHOR. R THORNTON                                                     00
000400*REMARKS. USED TO TEST CALLS TO SNAPDUMP.                               00
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
003600     'CALLSNAP WORKING STORAGE BEGINS HERE'.                            00
003700                                                                        00
003800 01  MISCELLANEOUS-AREAS.                                               00
003810     05  INPUT-RECORDS-READ      PIC S9(5) COMP-3 VALUE +0.               
003820     05  OUTPUT-RECORDS-WRITTEN  PIC S9(5) COMP-3 VALUE +0.               
003900     05 EOF-SWITCH               PIC X VALUE 'N'.                       00
004000        88 END-OF-INPUT          VALUE 'Y'.                             00
004100        88 MORE-INPUT            VALUE 'N'.                             00
004200                                                                        00
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
015800     OPEN INPUT INPUT-FILE OUTPUT OUTPUT-FILE.                          00
015900                                                                        00
016000 B100-MAINLINE-PROCESSING.                                              00
016100     PERFORM C100-READ-FILE.                                            00
016200     IF MORE-INPUT PERFORM D100-WRITE-FILE.                             00
016210     IF INPUT-RECORDS-READ = 25                                           
016220         CALL 'SNAPDUMP'.                                                 
016300                                                                        00
016400 C100-READ-FILE.                                                        00
016500     READ INPUT-FILE AT END MOVE 'Y' TO EOF-SWITCH.                     00
016510     ADD 1 TO INPUT-RECORDS-READ.                                         
016600                                                                        00
016700 D100-WRITE-FILE.                                                       00
016800     WRITE OUTPUT-RECORD FROM INPUT-RECORD.                             00
016810     ADD 1 TO OUTPUT-RECORDS-WRITTEN.                                     
016900                                                                        00
017000 Z100-END-OF-PROCESSING.                                                00
017010     DISPLAY INPUT-RECORDS-READ ' INPUT RECORDS READ'.                    
017020     DISPLAY OUTPUT-RECORDS-WRITTEN 'OUTPUT RECORDS WRITTEN'.             
017100     CLOSE INPUT-FILE OUTPUT-FILE.                                      00
  