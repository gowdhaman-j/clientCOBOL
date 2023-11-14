000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID.    ALLDYNCB.                                                 
000300*AUTHOR. R THORNTON                                                       
000400*REMARKS. USED TO TEST CALLS TO ALLOCDYN SUBROUTINE.                      
000500 ENVIRONMENT DIVISION.                                                    
000600 CONFIGURATION SECTION.                                                   
000700 INPUT-OUTPUT SECTION.                                                    
000800 FILE-CONTROL.                                                            
000900     SELECT INPUT-FILE ASSIGN TO INPUT1.                                  
001000     SELECT PRINT-FILE ASSIGN TO PRINT1.                                  
001010     SELECT OUTPUT-FILE ASSIGN TO OUTPUT1.                                
001100 DATA DIVISION.                                                           
001200 FILE SECTION.                                                            
001300                                                                          
001400 FD INPUT-FILE                                                            
001500     RECORD CONTAINS 80 CHARACTERS                                        
001600     RECORDING MODE IS F                                                  
001700     BLOCK CONTAINS 0 RECORDS                                             
001800     LABEL RECORD IS STANDARD                                             
001900     DATA RECORD IS INPUT-RECORD.                                         
002000                                                                          
002100 01  INPUT-RECORD.                                                        
002200     05  FILLER              PIC X(80).                                   
002300                                                                          
002400 FD PRINT-FILE                                                            
002500     RECORD CONTAINS 80 CHARACTERS                                        
002600     RECORDING MODE IS F                                                  
002700     BLOCK CONTAINS 0 RECORDS                                             
002800     LABEL RECORD IS STANDARD                                             
002900     DATA RECORD IS INPUT-RECORD.                                         
003000                                                                          
003100 01  PRINT-RECORD.                                                        
003200     05  FILLER              PIC X(80).                                   
003300                                                                          
003310                                                                          
003320 FD OUTPUT-FILE                                                           
003330     RECORD CONTAINS 83 CHARACTERS                                        
003340     RECORDING MODE IS F                                                  
003350     BLOCK CONTAINS 0 RECORDS                                             
003360     LABEL RECORD IS STANDARD                                             
003370     DATA RECORD IS OUTPUT-RECORD.                                        
003380                                                                          
003390 01  OUTPUT-RECORD.                                                       
003391     05  FILLER              PIC X(80).                                   
003392                                                                          
003400 WORKING-STORAGE SECTION.                                                 
003500 77  FILLER PIC X(36)  VALUE                                              
003600     'ALLDYNCB WORKING STORAGE BEGINS HERE'.                              
003700                                                                          
003800 01  MISCELLANEOUS-AREAS.                                                 
003900     05 EOF-SWITCH               PIC X VALUE 'N'.                         
004000        88 END-OF-INPUT          VALUE 'Y'.                               
004100        88 MORE-INPUT            VALUE 'N'.                               
004200                                                                          
004300                                                                          
004400 01  ALLOCATE-DATA.                                                       
004500     05  DDNAME                      PIC X(8).                            
004600     05  DSNAME-DUMMY-SYSOUT         PIC X(44).                           
004700     05  MEMBER-NAME-GENERATION      PIC X(8).                            
004800     05  CURRENT-STATUS              PIC S9(4) COMP.                      
004900         88  STATUS-OLD                VALUE +1.                          
005000         88  STATUS-MOD                VALUE +2.                          
005100         88  STATUS-NEW                VALUE +4.                          
005200         88  STATUS-SHR                VALUE +8.                          
005300     05  NORMAL-DISPOSITION          PIC S9(4) COMP.                      
005400         88  NORM-UNCATLG              VALUE +1.                          
005500         88  NORM-CATLG                VALUE +2.                          
005600         88  NORM-DELETE               VALUE +4.                          
005700         88  NORM-KEEP                 VALUE +8.                          
005800     05  CONDITIONAL-DISPOSITION     PIC S9(4) COMP.                      
005900         88  COND-UNCATLG              VALUE +1.                          
006000         88  COND-CATLG                VALUE +2.                          
006100         88  COND-DELETE               VALUE +4.                          
006200         88  COND-KEEP                 VALUE +8.                          
006300     05  UNIT-NAME                   PIC X(8).                            
006400     05  UNIT-COUNT                  PIC S9(4) COMP.                      
006500     05  VOLUME-SERIAL               PIC X(6)                             
006600                                       OCCURS 6 TIMES.                    
006700     05  LABEL-TYPE                  PIC S9(4) COMP.                      
006800         88  NO-LABELS                 VALUE +1.                          
006900         88  STANDARD-LABELS           VALUE +2.                          
007000         88  BYPASS-LABELS             VALUE +16.                         
007100     05  DATASET-SEQUENCE            PIC S9(4) COMP.                      
007200     05  FREE-EQU-CLOSE-INDIC        PIC X.                               
007300     05  RETENTION-PERIOD-DAYS       PIC S9(4) COMP.                      
007400     05  RECORDING-MODE-SUM          PIC S9(4) COMP.                      
007500         88  F-FIXED-RECORDS           VALUE +128.                        
007600         88  V-VARIABLE-RECORDS        VALUE +64.                         
007700         88  U-UNDEFINED-RECORDS       VALUE +192.                        
007800         88  D-OR-T-ADD                VALUE +32.                         
007900         88  B-BLOCKED-RECORDS-ADD     VALUE +16.                         
008000         88  S-STANDARD-SPANNED-ADD    VALUE +8.                          
008100         88  A-ASA-PRINT-CONTROL-ADD   VALUE +4.                          
008200         88  M-MACHINE-CODE-CONTROL-ADD VALUE +2.                         
008300     05  LOGICAL-RECORD-LENGTH       PIC S9(4) COMP.                      
008400     05  BLOCK-LENGTH                PIC S9(4) COMP.                      
008500     05  TAPE-DENSITY                PIC S9(4) COMP.                      
008600         88  200-BPI                   VALUE +3.                          
008700         88 556-BPI                    VALUE +67.                         
008800         88 800-BPI                    VALUE +131.                        
008900         88 1600-BPI                   VALUE +195.                        
009000         88 6250-BPI                   VALUE +211.                        
009100     05  NUMBER-BUFFERS              PIC S9(4) COMP.                      
009200     05  KEY-LENGTH                  PIC S9(4) COMP.                      
009300     05  DATASET-ORGANIZATION        PIC S9(4) COMP.                      
009400         88 VSAM-DATASET               VALUE +8.                          
009500         88 GRAPHICS-GS                VALUE +128.                        
009600         88 PARTITIONED-DATASET        VALUE +512.                        
009700         88 PARTITIONED-UNMOVABLE      VALUE +768.                        
009800         88 DIRECT-DATASET             VALUE +8192.                       
009900         88 DIRECT-UNMOVABLE           VALUE +8448.                       
010000*        88 PHYSICAL-SEQUENTIAL        VALUE +16384.                      
010100*        88 PHYSICAL-UNMOVABLE         VALUE +16640.                      
010200     05  SPACE-TYPE                  PIC X.                               
010300         88 CYLINDER-REQUEST           VALUE 'C'.                         
010400         88 TRACK-REQUEST              VALUE 'T'.                         
010500     05  PRIMARY-SPACE-AMOUNT        PIC S9(8) COMP.                      
010600     05  SECONDARY-SPACE-AMOUNT      PIC S9(8) COMP.                      
010700     05  RELEASE-SPACE-INDIC         PIC X.                               
010800     05  NBR-DIRECTORY-BLOCKS        PIC S9(8) COMP.                      
010900     05  EXPIRATION-DATE-CENTURY     PIC X(2).                            
011000     05  EXPIRATION-DATE-YYDDD       PIC X(5).                            
011100     05  AVG-BLKSIZE-FOR-SPACE       PIC S9(8) COMP.                      
011200     05  SPACE-FORMAT-TYPE           PIC S9(4) COMP.                      
011300         88  ALX-SPACE                 VALUE +2.                          
011400         88  MXIG-SPACE                VALUE +4.                          
011500         88  CONTIG-SPACE              VALUE +8.                          
011600     05  SPACE-ROUNDING-INDIC        PIC X.                               
011700     05  PRIVATE-VOLUME-INDIC        PIC X.                               
011800     05  VOLUME-SEQUENCE-NUMBER      PIC S9(4) COMP.                      
011900     05  VOLUME-COUNT                PIC S9(4) COMP.                      
012000     05  DSNAME-FOR-VOL-REF          PIC X(44).                           
012100     05  PARALLEL-MOUNT-INDIC        PIC X.                               
012200     05  SYSOUT-PROGRAM-NAME         PIC X(8).                            
012300     05  SYSOUT-FORM-NUMBER          PIC X(4).                            
012400     05  SYSOUT-OUTLIM               PIC S9(8) COMP.                      
012500     05  NUMBER-SYSOUT-COPIES        PIC S9(4) COMP.                      
012600     05  PASSWORD-PROTECTION         PIC S9(4) COMP.                      
012700         88  PASSWORD-ALL-REFERENCES   VALUE +16.                         
012800         88  NO-PASSWORD-TO-READ       VALUE +48.                         
012900     05  INPUT-OUTPUT-ONLY           PIC S9(4) COMP.                      
013000         88  INPUT-ONLY                VALUE +64.                         
013100         88  OUTPUT-ONLY               VALUE +128.                        
013200     05  FCB-IMAGE-NAME              PIC X(4).                            
013300     05  FCB-ALIGN-VERIFY            PIC S9(4) COMP.                      
013400         88  CHECK-FCB-ALIGN           VALUE +8.                          
013500         88  VERIFY-FCB                VALUE +4.                          
013600     05  USE-TSO-TERMINAL-INDIC      PIC X.                               
013700     05  UCS-IMAGE-NAME              PIC X(4).                            
013800     05  FOLD-MODE-INDIC             PIC X.                               
013900     05  CHAIN-MOUNT-VERIFY-INDIC    PIC X.                               
014000     05  DCB-MODEL-DSNAME            PIC X(44).                           
014100     05  DCB-DDNAME-BACKREF          PIC X(26).                           
014200     05  ERROR-OPTION                PIC S9(4) COMP.                      
014300         88  EROPT-EQU-T               VALUE +16.                         
014400         88  EROPT-EQU-ABE             VALUE +32.                         
014500         88  EROPT-EQU-SKP             VALUE +48.                         
014600         88  EROPT-EQU-ACC             VALUE +128.                        
014700     05  NCP-READ-WRITE-BEFORE-CHECK PIC S9(4) COMP.                      
014800     05  OPTION-CODES-SUM            PIC S9(4) COMP.                      
014900         88  OPTCD-EQU-R-OR-J          VALUE +1.                          
015000         88  OPTCD-EQU-T               VALUE +2.                          
015100         88  OPTCD-EQU-Z               VALUE +4.                          
015200         88  OPTCD-EQU-A-OR-Q          VALUE +8.                          
015300         88  OPTCD-EQU-F-OR-H-OR-O     VALUE +16.                         
015400         88  OPTCD-EQU-C-OR-E          VALUE +32.                         
015500         88  OPTCD-EQU-B-OR-U          VALUE +64.                         
015600         88  OPTCD-EQU-W               VALUE +128.                        
015700     05  PRINTER-SPACING              PIC S9(4) COMP.                     
015800         88  NO-SPACED-PRINT            VALUE +1.                         
015900         88  SINGLE-SPACED-PRINT        VALUE +9.                         
016000         88  DOUBLE-SPACED-PRINT        VALUE +17.                        
016100         88  TRIPLE-SPACED-PRINT        VALUE +25.                        
016200     05  TRTCH-7-TRACK-TAPE           PIC S9(4) COMP.                     
016300         88  NON-COMPACTION             VALUE +4.                         
016400         88  COMPACTION                 VALUE +8.                         
016500         88  TRTCH-EQU-C                VALUE +19.                        
016600         88  TRTCH-EQU-E                VALUE +35.                        
016700         88  TRTCH-EQU-ET               VALUE +43.                        
016800         88  TRTCH-EQU-T                VALUE +59.                        
016900     05  PASSWORD-VALUE               PIC X(32).                          
017000     05  DIAGNOSTIC-TRACE-INDIC       PIC X.                              
017100     05  SYSOUT-DEST-NODE-NAME        PIC X(8).                           
017200     05  SYSOUT-HOLD-QUEUE-INDIC      PIC X.                              
017300     05  SYSOUT-DEST-USERID           PIC X(8).                           
017400     05  SYSOUT-BURSTER-STACKER       PIC S9(4) COMP.                     
017500         88  BURST-EQU-YES              VALUE +2.                         
017600         88  BURST-EQU-NO               VALUE +4.                         
017700     05  CHARACTER-ARRANGEMENT-TABLE  PIC X(4)                            
017800                                        OCCURS 4 TIMES.                   
017900     05  SYSOUT-COPY-GROUP-VALUE      PIC S9(4) COMP                      
018000                                        OCCURS 8 TIMES.                   
018100     05  SYSOUT-FLASH-OVERLAY-NAME    PIC X(4).                           
018200     05  SYSOUT-FLASH-OVERLAY-COUNT   PIC S9(4) COMP.                     
018300     05  COPY-MODIFY-MODULE-NAME      PIC X(4).                           
018400     05  COPY-MODULE-TRC              PIC S9(4) COMP.                     
018500         88  FIRST-CHAR-ARR-TABLE       VALUE +0.                         
018600         88  SECOND-CHAR-ARR-TABLE      VALUE +1.                         
018700         88  THIRD-CHAR-ARR-TABLE       VALUE +2.                         
018800         88  FOURTH-CHAR-ARR-TABLE      VALUE +3.                         
018900     05  DEFER-MOUNT-INDIC            PIC X.                              
019000     05  OUTPUT-STATEMENT-REFERENCE   PIC X(26)                           
019100                                        OCCURS 8 TIMES.                   
019200     05  CNTL-STATEMENT-REFERENCE     PIC X(26).                          
019300     05  SMS-STORCLAS-NAME            PIC X(8).                           
019400     05  SMS-MGMTCLAS-NAME            PIC X(8).                           
019500     05  SMS-DATACLAS-NAME            PIC X(8).                           
019600     05  SMS-RECORD-ORGANIZATION      PIC S9(4) COMP.                     
019700         88  VSAM-KSDS                  VALUE +128.                       
019800         88  VSAM-ESDS                  VALUE +64.                        
019900         88  VSAM-RRDS                  VALUE +32.                        
020000         88  VSAM-LINEAR-SPACE          VALUE +16.                        
020100     05  SMS-KEY-OFFSET               PIC S9(4) COMP.                     
020200     05  SMS-REFDD                    PIC X(26).                          
020300     05  SMS-LIKE-DSNAME              PIC X(44).                          
020400     05  SMS-AVERAGE-RECORD-UNITS     PIC S9(4) COMP.                     
020500         88  IN-SINGLE-RECORDS          VALUE +128.                       
020600         88  IN-THOUSAND-RECORD-UNITS   VALUE +64.                        
020700         88  IN-MILLION-RECORD-UNITS    VALUE +32.                        
020800     05  SMS-DSNTYPE                  PIC S9(4) COMP.                     
020900         88  PDSE-LIBRARY               VALUE +128.                       
021000         88  OLD-STYLE-PDS              VALUE +64.                        
021100                                                                          
021200                                                                          
021300 01  ALLOCATION-RESULT.                                                   
021400     05  ALLOCDYN-REQUEST             PIC X.                              
021500         88  ALLOCATION-REQUEST         VALUE 'A'.                        
021600         88  VOLSER-REQUEST             VALUE 'V'.                        
021700     05  ALLOCDYN-RETURN-CODE         PIC XX.                             
021800         88  SUCCESSFUL-ALLOCATION      VALUE '00'.                       
021900         88  ENVIRONMENT-ERROR          VALUE '04'.                       
022000         88  VALIDATION-DENIAL          VALUE '08'.                       
022100         88  PARAMETER-ERROR            VALUE '12'.                       
022200     05  ERROR-REASON-CODE.                                               
022300         10  CLASS-7-CODE             PIC X.                              
022400         10  ERROR-CLASS              PIC X.                              
022500             88  UNAVAILABLE-RESOURCE   VALUE '2'.                        
022600             88  INVALID-PARAMETER-LIST VALUE '3'.                        
022700             88  ERROR-IN-ENVIRONMENT   VALUE '4'.                        
022800             88  SYSTEM-ROUTINE-ERROR   VALUE '7'.                        
022900         10  SPECIFIC-ERROR-CODE      PIC XX.                             
023000     05  ERROR-REASON-MESSAGE         PIC X(71).                          
023100     05  INFORMATION-REASON           PIC X(4).                           
023200     05  INFO-REASON-MESSAGE          PIC X(71).                          
023300                                                                          
023400                                                                          
023500 PROCEDURE DIVISION.                                                      
023600                                                                          
023700 A100-EXECUTIVE-CONTROL.                                                  
023800     PERFORM A100-INITIALIZATION.                                         
023900     PERFORM B100-MAINLINE-PROCESSING UNTIL END-OF-INPUT.                 
024000     PERFORM Z100-END-OF-PROCESSING.                                      
024100     GOBACK.                                                              
024200                                                                          
024300 A100-INITIALIZATION.                                                     
024410      MOVE LOW-VALUES TO ALLOCATE-DATA.                                   
024420      MOVE 'OUTPUT1' TO DDNAME.                                           
024421      MOVE 'PROFIT.YZZ8100.SUMCYCLE.YEAR1909' TO                          
024422           DSNAME-DUMMY-SYSOUT.                                           
024450      MOVE +4 TO CURRENT-STATUS.                                          
024460      MOVE +2 TO NORMAL-DISPOSITION.                                      
024470      MOVE +4 TO CONDITIONAL-DISPOSITION.                                 
024472      MOVE +12    TO VOLUME-COUNT.                                        
024480      MOVE 'CART' TO UNIT-NAME.                                           
024481      MOVE +2557 TO RETENTION-PERIOD-DAYS.                                
024482      MOVE +8 TO TRTCH-7-TRACK-TAPE.                                      
024483      MOVE +144 TO RECORDING-MODE-SUM.                                    
024484      MOVE +83  TO LOGICAL-RECORD-LENGTH.                                 
024485      MOVE +32702 TO BLOCK-LENGTH.                                        
026000      MOVE 'A' TO ALLOCDYN-REQUEST.                                       
026200      CALL 'ALLOCDYN' USING ALLOCATE-DATA, ALLOCATION-RESULT.             
026300      IF SUCCESSFUL-ALLOCATION                                            
026400         NEXT SENTENCE                                                    
026500      ELSE DISPLAY 'UNABLE TO ALLOCATE OUTPUT1: RETURN CODE='             
026600                   ALLOCDYN-RETURN-CODE                                   
026700          DISPLAY '   ERROR-REASON-CODE=' ERROR-REASON-CODE               
026800                   ': ' ERROR-REASON-MESSAGE                              
026900          DISPLAY '   INFORMATION-REASON=' INFORMATION-REASON             
027000                   ': ' INFO-REASON-MESSAGE                               
027100          CALL 'COBABEND'.                                                
027200     OPEN INPUT INPUT-FILE OUTPUT PRINT-FILE OUTPUT-FILE.                 
027300                                                                          
027400 B100-MAINLINE-PROCESSING.                                                
027500     PERFORM C100-READ-FILE.                                              
027600     IF MORE-INPUT PERFORM D100-WRITE-FILE.                               
027700                                                                          
027800 C100-READ-FILE.                                                          
027900     READ INPUT-FILE AT END MOVE 'Y' TO EOF-SWITCH.                       
028000                                                                          
028100 D100-WRITE-FILE.                                                         
028200     WRITE PRINT-RECORD FROM INPUT-RECORD.                                
028210     WRITE OUTPUT-RECORD FROM INPUT-RECORD.                               
028300                                                                          
028400 Z100-END-OF-PROCESSING.                                                  
028500     CLOSE INPUT-FILE PRINT-FILE OUTPUT-FILE.                             
  