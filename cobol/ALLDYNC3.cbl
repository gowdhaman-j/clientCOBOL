000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID.    ALLDYNC3.                                                 
000300*AUTHOR. R THORNTON                                                       
000400*REMARKS. USED TO TEST CALLS TO ALLOCDYN SUBROUTINE FOR SYSOUT=*          
000600 ENVIRONMENT DIVISION.                                                    
000700 CONFIGURATION SECTION.                                                   
000800 INPUT-OUTPUT SECTION.                                                    
000900 FILE-CONTROL.                                                            
001000     SELECT INPUT-FILE ASSIGN TO INPUT1.                                  
001100     SELECT OUTPUT-FILE ASSIGN TO OUTPUT1.                                
001200 DATA DIVISION.                                                           
001300 FILE SECTION.                                                            
001400                                                                          
001500 FD INPUT-FILE                                                            
001600     RECORD CONTAINS 80 CHARACTERS                                        
001700     RECORDING MODE IS F                                                  
001800     BLOCK CONTAINS 0 RECORDS                                             
001900     LABEL RECORD IS STANDARD                                             
002000     DATA RECORD IS INPUT-RECORD.                                         
002100                                                                          
002200 01  INPUT-RECORD.                                                        
002300     05  FILLER              PIC X(80).                                   
002400                                                                          
002500 FD OUTPUT-FILE                                                           
002600     RECORD CONTAINS 80 CHARACTERS                                        
002700     RECORDING MODE IS F                                                  
002800     BLOCK CONTAINS 0 RECORDS                                             
002900     LABEL RECORD IS STANDARD                                             
003000     DATA RECORD IS OUTPUT-RECORD.                                        
003100                                                                          
003200 01  OUTPUT-RECORD.                                                       
003300     05  FILLER              PIC X(80).                                   
003400                                                                          
003500 WORKING-STORAGE SECTION.                                                 
003600 77  FILLER PIC X(36)  VALUE                                              
003700     "ALLDYNC3 WORKING STORAGE BEGINS HERE".                              
003800                                                                          
003900 01  MISCELLANEOUS-AREAS.                                                 
004000     05 EOF-SWITCH               PIC X VALUE "N".                         
004100        88 END-OF-INPUT          VALUE "Y".                               
004200        88 MORE-INPUT            VALUE "N".                               
004300                                                                          
004400                                                                          
004500 01  ALLOCATE-DATA.                                                       
004600     05  DDNAME                      PIC X(8).                            
004700     05  DSNAME-DUMMY-SYSOUT         PIC X(44).                           
004800     05  MEMBER-NAME-GENERATION      PIC X(8).                            
004900     05  CURRENT-STATUS              PIC S9(4) COMP.                      
005000         88  STATUS-OLD                VALUE +1.                          
005100         88  STATUS-MOD                VALUE +2.                          
005200         88  STATUS-NEW                VALUE +4.                          
005300         88  STATUS-SHR                VALUE +8.                          
005400     05  NORMAL-DISPOSITION          PIC S9(4) COMP.                      
005500         88  NORM-UNCATLG              VALUE +1.                          
005600         88  NORM-CATLG                VALUE +2.                          
005700         88  NORM-DELETE               VALUE +4.                          
005800         88  NORM-KEEP                 VALUE +8.                          
005900     05  CONDITIONAL-DISPOSITION     PIC S9(4) COMP.                      
006000         88  COND-UNCATLG              VALUE +1.                          
006100         88  COND-CATLG                VALUE +2.                          
006200         88  COND-DELETE               VALUE +4.                          
006300         88  COND-KEEP                 VALUE +8.                          
006410     05  UNIT-NAME                   PIC X(8).                            
006500     05  UNIT-COUNT                  PIC S9(4) COMP.                      
006600     05  VOLUME-SERIAL               PIC X(6)                             
006700                                       OCCURS 6 TIMES.                    
006800     05  LABEL-TYPE                  PIC S9(4) COMP.                      
006900         88  NO-LABELS                 VALUE +1.                          
007000         88  STANDARD-LABELS           VALUE +2.                          
007100         88  BYPASS-LABELS             VALUE +16.                         
007200     05  DATASET-SEQUENCE            PIC S9(4) COMP.                      
007300     05  FREE-EQU-CLOSE-INDIC        PIC X.                               
007400     05  RETENTION-PERIOD-DAYS       PIC S9(4) COMP.                      
007510     05  RECORDING-MODE-SUM          PIC S9(4) COMP.                      
007600         88  F-FIXED-RECORDS           VALUE +128.                        
007700         88  V-VARIABLE-RECORDS        VALUE +64.                         
007800         88  U-UNDEFINED-RECORDS       VALUE +192.                        
007900         88  D-OR-T-ADD                VALUE +32.                         
008000         88  B-BLOCKED-RECORDS-ADD     VALUE +16.                         
008100         88  S-STANDARD-SPANNED-ADD    VALUE +8.                          
008200         88  A-ASA-PRINT-CONTROL-ADD   VALUE +4.                          
008300         88  M-MACHINE-CODE-CONTROL-ADD VALUE +2.                         
008510     05  BLOCK-LENGTH                PIC S9(4) COMP.                      
008600     05  TAPE-DENSITY                PIC S9(4) COMP.                      
008700         88  200-BPI                   VALUE +3.                          
008800         88 556-BPI                    VALUE +67.                         
008900         88 800-BPI                    VALUE +131.                        
009000         88 1600-BPI                   VALUE +195.                        
009100         88 6250-BPI                   VALUE +211.                        
009200     05  NUMBER-BUFFERS              PIC S9(4) COMP.                      
009300     05  KEY-LENGTH                  PIC S9(4) COMP.                      
009400     05  DATASET-ORGANIZATION        PIC S9(4) COMP.                      
009500         88 VSAM-DATASET               VALUE +8.                          
009600         88 GRAPHICS-GS                VALUE +128.                        
009700         88 PARTITIONED-DATASET        VALUE +512.                        
009800         88 PARTITIONED-UNMOVABLE      VALUE +768.                        
009900         88 DIRECT-DATASET             VALUE +8192.                       
010000         88 DIRECT-UNMOVABLE           VALUE +8448.                       
010100     05  LOGICAL-RECORD-LENGTH       PIC S9(4) COMP.                      
010510     05  SPACE-TYPE                  PIC X.                               
010520         88 CYLINDER-REQUEST           VALUE "C".                         
010530         88 TRACK-REQUEST              VALUE "T".                         
010710     05  PRIMARY-SPACE-AMOUNT        PIC S9(8) COMP.                      
010720     05  SECONDARY-SPACE-AMOUNT      PIC S9(8) COMP.                      
010800     05  RELEASE-SPACE-INDIC         PIC X.                               
010900     05  NBR-DIRECTORY-BLOCKS        PIC S9(8) COMP.                      
011000     05  EXPIRATION-DATE-CENTURY     PIC X(2).                            
011100     05  EXPIRATION-DATE-YYDDD       PIC X(5).                            
011200     05  AVG-BLKSIZE-FOR-SPACE       PIC S9(8) COMP.                      
011300     05  SPACE-FORMAT-TYPE           PIC S9(4) COMP.                      
011400         88  ALX-SPACE                 VALUE +2.                          
011500         88  MXIG-SPACE                VALUE +4.                          
011600         88  CONTIG-SPACE              VALUE +8.                          
011700     05  SPACE-ROUNDING-INDIC        PIC X.                               
011800     05  PRIVATE-VOLUME-INDIC        PIC X.                               
011900     05  VOLUME-SEQUENCE-NUMBER      PIC S9(4) COMP.                      
012000     05  VOLUME-COUNT                PIC S9(4) COMP.                      
012100     05  DSNAME-FOR-VOL-REF          PIC X(44).                           
012200     05  PARALLEL-MOUNT-INDIC        PIC X.                               
012300     05  SYSOUT-PROGRAM-NAME         PIC X(8).                            
012400     05  SYSOUT-FORM-NUMBER          PIC X(4).                            
012500     05  SYSOUT-OUTLIM               PIC S9(8) COMP.                      
012600     05  NUMBER-SYSOUT-COPIES        PIC S9(4) COMP.                      
012700     05  PASSWORD-PROTECTION         PIC S9(4) COMP.                      
012800         88  PASSWORD-ALL-REFERENCES   VALUE +16.                         
012900         88  NO-PASSWORD-TO-READ       VALUE +48.                         
013000     05  INPUT-OUTPUT-ONLY           PIC S9(4) COMP.                      
013100         88  INPUT-ONLY                VALUE +64.                         
013200         88  OUTPUT-ONLY               VALUE +128.                        
013300     05  FCB-IMAGE-NAME              PIC X(4).                            
013400     05  FCB-ALIGN-VERIFY            PIC S9(4) COMP.                      
013500         88  CHECK-FCB-ALIGN           VALUE +8.                          
013600         88  VERIFY-FCB                VALUE +4.                          
013700     05  USE-TSO-TERMINAL-INDIC      PIC X.                               
013800     05  UCS-IMAGE-NAME              PIC X(4).                            
013900     05  FOLD-MODE-INDIC             PIC X.                               
014000     05  CHAIN-MOUNT-VERIFY-INDIC    PIC X.                               
014100     05  DCB-MODEL-DSNAME            PIC X(44).                           
014200     05  DCB-DDNAME-BACKREF          PIC X(26).                           
014300     05  ERROR-OPTION                PIC S9(4) COMP.                      
014400         88  EROPT-EQU-T               VALUE +16.                         
014500         88  EROPT-EQU-ABE             VALUE +32.                         
014600         88  EROPT-EQU-SKP             VALUE +48.                         
014700         88  EROPT-EQU-ACC             VALUE +128.                        
014800     05  NCP-READ-WRITE-BEFORE-CHECK PIC S9(4) COMP.                      
014900     05  OPTION-CODES-SUM            PIC S9(4) COMP.                      
015000         88  OPTCD-EQU-R-OR-J          VALUE +1.                          
015100         88  OPTCD-EQU-T               VALUE +2.                          
015200         88  OPTCD-EQU-Z               VALUE +4.                          
015300         88  OPTCD-EQU-A-OR-Q          VALUE +8.                          
015400         88  OPTCD-EQU-F-OR-H-OR-O     VALUE +16.                         
015500         88  OPTCD-EQU-C-OR-E          VALUE +32.                         
015600         88  OPTCD-EQU-B-OR-U          VALUE +64.                         
015700         88  OPTCD-EQU-W               VALUE +128.                        
015800     05  PRINTER-SPACING              PIC S9(4) COMP.                     
015900         88  NO-SPACED-PRINT            VALUE +1.                         
016000         88  SINGLE-SPACED-PRINT        VALUE +9.                         
016100         88  DOUBLE-SPACED-PRINT        VALUE +17.                        
016200         88  TRIPLE-SPACED-PRINT        VALUE +25.                        
016300     05  TRTCH-7-TRACK-TAPE           PIC S9(4) COMP.                     
016400         88  NON-COMPACTION             VALUE +4.                         
016500         88  COMPACTION                 VALUE +8.                         
016600         88  TRTCH-EQU-C                VALUE +19.                        
016700         88  TRTCH-EQU-E                VALUE +35.                        
016800         88  TRTCH-EQU-ET               VALUE +43.                        
016900         88  TRTCH-EQU-T                VALUE +59.                        
017000     05  PASSWORD-VALUE               PIC X(32).                          
017100     05  DIAGNOSTIC-TRACE-INDIC       PIC X.                              
017200     05  SYSOUT-DEST-NODE-NAME        PIC X(8).                           
017300     05  SYSOUT-HOLD-QUEUE-INDIC      PIC X.                              
017400     05  SYSOUT-DEST-USERID           PIC X(8).                           
017500     05  SYSOUT-BURSTER-STACKER       PIC S9(4) COMP.                     
017600         88  BURST-EQU-YES              VALUE +2.                         
017700         88  BURST-EQU-NO               VALUE +4.                         
017800     05  CHARACTER-ARRANGEMENT-TABLE  PIC X(4)                            
017900                                        OCCURS 4 TIMES.                   
018000     05  SYSOUT-COPY-GROUP-VALUE      PIC S9(4) COMP                      
018100                                        OCCURS 8 TIMES.                   
018200     05  SYSOUT-FLASH-OVERLAY-NAME    PIC X(4).                           
018300     05  SYSOUT-FLASH-OVERLAY-COUNT   PIC S9(4) COMP.                     
018400     05  COPY-MODIFY-MODULE-NAME      PIC X(4).                           
018500     05  COPY-MODULE-TRC              PIC S9(4) COMP.                     
018600         88  FIRST-CHAR-ARR-TABLE       VALUE +0.                         
018700         88  SECOND-CHAR-ARR-TABLE      VALUE +1.                         
018800         88  THIRD-CHAR-ARR-TABLE       VALUE +2.                         
018900         88  FOURTH-CHAR-ARR-TABLE      VALUE +3.                         
019000     05  DEFER-MOUNT-INDIC            PIC X.                              
019100     05  OUTPUT-STATEMENT-REFERENCE   PIC X(26)                           
019200                                        OCCURS 8 TIMES.                   
019300     05  CNTL-STATEMENT-REFERENCE     PIC X(26).                          
019400     05  SMS-STORCLAS-NAME            PIC X(8).                           
019500     05  SMS-MGMTCLAS-NAME            PIC X(8).                           
019600     05  SMS-DATACLAS-NAME            PIC X(8).                           
019700     05  SMS-RECORD-ORGANIZATION      PIC S9(4) COMP.                     
019800         88  VSAM-KSDS                  VALUE +128.                       
019900         88  VSAM-ESDS                  VALUE +64.                        
020000         88  VSAM-RRDS                  VALUE +32.                        
020100         88  VSAM-LINEAR-SPACE          VALUE +16.                        
020200     05  SMS-KEY-OFFSET               PIC S9(4) COMP.                     
020300     05  SMS-REFDD                    PIC X(26).                          
020400     05  SMS-LIKE-DSNAME              PIC X(44).                          
020500     05  SMS-AVERAGE-RECORD-UNITS     PIC S9(4) COMP.                     
020600         88  IN-SINGLE-RECORDS          VALUE +128.                       
020700         88  IN-THOUSAND-RECORD-UNITS   VALUE +64.                        
020800         88  IN-MILLION-RECORD-UNITS    VALUE +32.                        
020900     05  SMS-DSNTYPE                  PIC S9(4) COMP.                     
021000         88  PDSE-LIBRARY               VALUE +128.                       
021100         88  OLD-STYLE-PDS              VALUE +64.                        
021200                                                                          
021300                                                                          
021400 01  ALLOCATION-RESULT.                                                   
021500     05  ALLOCDYN-REQUEST             PIC X.                              
021600         88  ALLOCATION-REQUEST         VALUE "A".                        
021700         88  VOLSER-REQUEST             VALUE "V".                        
021800     05  ALLOCDYN-RETURN-CODE         PIC XX.                             
021900         88  SUCCESSFUL-ALLOCATION      VALUE "00".                       
022000         88  ENVIRONMENT-ERROR          VALUE "04".                       
022100         88  VALIDATION-DENIAL          VALUE "08".                       
022200         88  PARAMETER-ERROR            VALUE "12".                       
022300     05  ERROR-REASON-CODE.                                               
022400         10  CLASS-7-CODE             PIC X.                              
022500         10  ERROR-CLASS              PIC X.                              
022600             88  UNAVAILABLE-RESOURCE   VALUE "2".                        
022700             88  INVALID-PARAMETER-LIST VALUE "3".                        
022800             88  ERROR-IN-ENVIRONMENT   VALUE "4".                        
022900             88  SYSTEM-ROUTINE-ERROR   VALUE "7".                        
023000         10  SPECIFIC-ERROR-CODE      PIC XX.                             
023100     05  ERROR-REASON-MESSAGE         PIC X(71).                          
023200     05  INFORMATION-REASON           PIC X(4).                           
023300     05  INFO-REASON-MESSAGE          PIC X(71).                          
023400                                                                          
023500                                                                          
023600 PROCEDURE DIVISION.                                                      
023700                                                                          
023800 A100-EXECUTIVE-CONTROL.                                                  
023900     PERFORM A100-INITIALIZATION.                                         
024000     PERFORM B100-MAINLINE-PROCESSING UNTIL END-OF-INPUT.                 
024100     PERFORM Z100-END-OF-PROCESSING.                                      
024200     GOBACK.                                                              
024300                                                                          
024400 A100-INITIALIZATION.                                                     
024500     MOVE LOW-VALUES TO ALLOCATE-DATA.                                    
024501     MOVE "OUTPUT1" TO DDNAME.                                            
024502     MOVE "SYSOUT=*" TO DSNAME-DUMMY-SYSOUT.                              
025000     MOVE "A" TO ALLOCDYN-REQUEST.                                        
036600     OPEN INPUT INPUT-FILE OUTPUT OUTPUT-FILE.                            
036700                                                                          
036800 B100-MAINLINE-PROCESSING.                                                
036900     PERFORM C100-READ-FILE.                                              
037000     IF MORE-INPUT PERFORM D100-WRITE-FILE.                               
037100                                                                          
037200 C100-READ-FILE.                                                          
037300     READ INPUT-FILE AT END MOVE "Y" TO EOF-SWITCH.                       
037310     MOVE INPUT-RECORD TO OUTPUT-RECORD.                                  
037400                                                                          
037500 D100-WRITE-FILE.                                                         
037510     DISPLAY "OUTREC= " OUTPUT-RECORD.                                    
037600     WRITE OUTPUT-RECORD.                                                 
037700                                                                          
037800 Z100-END-OF-PROCESSING.                                                  
037900     CLOSE INPUT-FILE OUTPUT-FILE.                                        
