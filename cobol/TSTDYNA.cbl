000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID.    TSTDYNA.                                                  
000300 AUTHOR. R THORNTON                                                       
000400 REMARKS. UTILITY PROGRAM TO TEST THE DYNALLOC SUBROUTINE.                
000500 ENVIRONMENT DIVISION.                                                    
000600 CONFIGURATION SECTION.                                                   
000700 INPUT-OUTPUT SECTION.                                                    
000800 FILE-CONTROL.                                                            
000900     SELECT OUTPUT-FILE ASSIGN TO UT-S-OUTPUT1.                           
001000 DATA DIVISION.                                                           
001100 FILE SECTION.                                                            
001200 FD  OUTPUT-FILE                                                          
001300     RECORDING MODE IS F                                                  
001400     RECORD CONTAINS 80 CHARACTERS                                        
001500     BLOCK CONTAINS 0 RECORDS                                             
001600     LABEL RECORDS ARE STANDARD                                           
001700     DATA RECORD IS OUTPUT-RECORD.                                        
001800 01  OUTPUT-RECORD               PIC X(80).                               
001900                                                                          
002000 WORKING-STORAGE SECTION.                                                 
002100 77  FILLER PIC X(36)  VALUE                                              
002200     'TSTDYNA WORKING STORAGE BEGINS HERE'.                               
002300                                                                          
002400 01  ALLOCATE-DATA.                                                       
002500     05 DDNAME                   PIC X(8).                                
002600     05 DATASET-NAME             PIC X(44).                               
002700     05 MEMBER-NAME              PIC X(8).                                
002800     05 CURRENT-STATUS           PIC X.                                   
002900        88 STATUS-OLD            VALUE 'O'.                               
003000        88 STATUS-MOD            VALUE 'M'.                               
003100        88 STATUS-NEW            VALUE 'N'.                               
003200        88 STATUS-SHR            VALUE 'S'.                               
003300     05 NORMAL-DISPOSITION       PIC X.                                   
003400        88 NORM-UNCATLG          VALUE 'U'.                               
003500        88 NORM-CATLG            VALUE 'C'.                               
003600        88 NORM-DELETE           VALUE 'D'.                               
003700        88 NORM-KEEP             VALUE 'K'.                               
003800     05 CONDITIONAL-DISPOSITION  PIC X.                                   
003900        88 COND-UNCATLG          VALUE 'U'.                               
004000        88 COND-CATLG            VALUE 'C'.                               
004100        88 COND-DELETE           VALUE 'D'.                               
004200        88 COND-KEEP             VALUE 'K'.                               
004300     05 UNIT-NAME                PIC X(8).                                
004400     05 UNIT-COUNT               PIC S9(4) COMP.                          
004500     05 VOLUME-SERIAL            PIC X(6).                                
004600     05 LABEL-TYPE               PIC X.                                   
004700        88 NO-LABELS             VALUE 'N'.                               
004800        88 STANDARD-LABELS       VALUE 'S'.                               
004900        88 BYPASS-LABELS         VALUE 'B'.                               
005000     05 DATASET-SEQUENCE         PIC S9(4) COMP.                          
005100     05 FREE-CLOSE               PIC X.                                   
005200        88 FREE-WHEN-CLOSED      VALUE 'F'.                               
005300     05 RETENTION-PERIOD-DAYS    PIC S9(4) COMP.                          
005400     05 RECORDING-MODE           PIC X.                                   
005500        88 FIXED-RECORDS         VALUE 'F'.                               
005600        88 VARIABLE-RECORDS      VALUE 'V'.                               
005700        88 UNDEFINED-RECORDS     VALUE 'U'.                               
005800        88 FIXED-STANDARD        VALUE 'S'.                               
005900     05 BLOCKING                 PIC X.                                   
006000        88 BLOCKED-RECORDS       VALUE 'B'.                               
006100     05 CONTROL-CHARACTERS       PIC X.                                   
006200        88 ASA-CONTROL-CHAR      VALUE 'A'.                               
006300     05 LOGICAL-RECORD-LENGTH    PIC S9(4) COMP.                          
006400     05 BLOCK-LENGTH             PIC S9(4) COMP.                          
006500     05 TAPE-DENSITY             PIC X.                                   
006600        88 800-BPI               VALUE '8'.                               
006700        88 1600-BPI              VALUE '1'.                               
006800        88 6250-BPI              VALUE '6'.                               
006900     05 NUMBER-BUFFERS           PIC S9(4) COMP.                          
007000     05 KEY-LENGTH               PIC S9(4) COMP.                          
007100     05 DATASET-ORGANIZATION     PIC X.                                   
007200        88 VSAM-DATASET          VALUE 'V'.                               
007300        88 PARTITIONED-DATASET   VALUE 'P'.                               
007400        88 DIRECT-DATASET        VALUE 'D'.                               
007500        88 PHYSICAL-SEQUENTIAL   VALUE 'S'.                               
007600     05 SPACE-TYPE               PIC X.                                   
007700        88 CYLINDER-REQUEST      VALUE 'C'.                               
007800        88 TRACK-REQUEST         VALUE 'T'.                               
007900     05 PRIMARY-SPACE-AMOUNT     PIC S9(4) COMP.                          
008000     05 SECONDARY-SPACE-AMOUNT   PIC S9(4) COMP.                          
008100     05 RELEASE-SPACE            PIC X.                                   
008200        88 RELEASE-UNUSED        VALUE 'R'.                               
008300     05 NBR-DIRECTORY-BLOCKS     PIC S9(4) COMP.                          
008400     05 EXPIRATION-DATE-YYDDD    PIC X(5).                                
008500                                                                          
008600 01  ALLOCATION-RESULT.                                                   
008700     05 DYNALLOC-REQUEST         PIC X.                                   
008800        88 ALLOCATION-REQUEST    VALUE 'A'.                               
008900        88 VOLSER-REQUEST        VALUE 'V'.                               
009000     05 DYNALLOC-RETURN-CODE     PIC XX.                                  
009100        88 SUCCESSFUL-ALLOCATION VALUE '00'.                              
009200        88 ENVIRONMENT-ERROR     VALUE '04'.                              
009300        88 VALIDATION-DENIAL     VALUE '08'.                              
009400        88 PARAMETER-ERROR       VALUE '12'.                              
009500     05 ERROR-REASON-CODE.                                                
009600        10 CLASS-7-CODE              PIC X.                               
009700        10 ERROR-CLASS               PIC X.                               
009800           88 UNAVAILABLE-RESOURCE   VALUE '2'.                           
009900           88 INVALID-PARAMETER-LIST VALUE '3'.                           
010000           88 ERROR-IN-ENVIRONMENT   VALUE '4'.                           
010100           88 SYSTEM-ROUTINE-ERROR   VALUE '7'.                           
010200        10 SPECIFIC-ERROR-CODE       PIC XX.                              
010300     05 INFORMATION-REASON           PIC X(4).                            
010400                                                                          
010500 PROCEDURE DIVISION.                                                      
010600                                                                          
010700 A100-EXECUTIVE-CONTROL.                                                  
010800     PERFORM A200-ALLOCATE-THE-FILE THRU A200-EXIT.                       
010900     PERFORM B100-MAINLINE-PROCESSING.                                    
011000     PERFORM Z100-END-OF-PROCESSING.                                      
011100     GOBACK.                                                              
011200                                                                          
011300 A200-ALLOCATE-THE-FILE.                                                  
011400      MOVE LOW-VALUES TO ALLOCATE-DATA.                                   
011500      MOVE 'OUTPUT1' TO DDNAME.                                           
011600      MOVE 'HCS.PROD.DICK001' TO DATASET-NAME.                            
011700      MOVE '+1' TO MEMBER-NAME.                                           
011800      MOVE 'N' TO CURRENT-STATUS.                                         
011900      MOVE 'C' TO NORMAL-DISPOSITION.                                     
012000      MOVE 'D' TO CONDITIONAL-DISPOSITION.                                
012100      MOVE 'DISK' TO UNIT-NAME.                                           
012200      MOVE 'F' TO RECORDING-MODE.                                         
012300      MOVE 'B' TO BLOCKING.                                               
012400      MOVE 80 TO LOGICAL-RECORD-LENGTH.                                   
012500      MOVE 23440 TO BLOCK-LENGTH.                                         
012600      MOVE 'S' TO DATASET-ORGANIZATION.                                   
012700      MOVE 'C' TO SPACE-TYPE.                                             
012800      MOVE 'R' TO RELEASE-SPACE.                                          
012900      MOVE 5 TO PRIMARY-SPACE-AMOUNT.                                     
013000      MOVE 2 TO SECONDARY-SPACE-AMOUNT.                                   
013100      CALL 'DYNALLOC' USING ALLOCATE-DATA, ALLOCATION-RESULT.             
013200      IF SUCCESSFUL-ALLOCATION OPEN OUTPUT OUTPUT-FILE                    
013300         ELSE DISPLAY 'UNABLE TO ALLOCATE OUTPUT1, RETURN CODE '          
013400                      DYNALLOC-RETURN-CODE ' ERROR REASON CODE '          
013500                      ERROR-REASON-CODE,                                  
013600                      CALL 'COBABEND'.                                    
013700 A200-EXIT. EXIT.                                                         
013800                                                                          
013900 B100-MAINLINE-PROCESSING.                                                
014000     MOVE 'RECORD 1' TO OUTPUT-RECORD.                                    
014100     WRITE OUTPUT-RECORD.                                                 
014200     MOVE 'RECORD 2' TO OUTPUT-RECORD.                                    
014300     WRITE OUTPUT-RECORD.                                                 
014400     MOVE 'RECORD 3' TO OUTPUT-RECORD.                                    
014500     WRITE OUTPUT-RECORD.                                                 
014600     MOVE 'RECORD 4' TO OUTPUT-RECORD.                                    
014700     WRITE OUTPUT-RECORD.                                                 
014800     MOVE 'RECORD 5' TO OUTPUT-RECORD.                                    
014900     WRITE OUTPUT-RECORD.                                                 
015000     MOVE 'RECORD 6' TO OUTPUT-RECORD.                                    
015100     WRITE OUTPUT-RECORD.                                                 
015200     MOVE 'RECORD 7' TO OUTPUT-RECORD.                                    
015300     WRITE OUTPUT-RECORD.                                                 
015400     MOVE 'RECORD 8' TO OUTPUT-RECORD.                                    
015500     WRITE OUTPUT-RECORD.                                                 
015600     MOVE 'RECORD 9' TO OUTPUT-RECORD.                                    
015700     WRITE OUTPUT-RECORD.                                                 
015800 B100-EXIT. EXIT.                                                         
015900                                                                          
016000 Z100-END-OF-PROCESSING.                                                  
016100     CLOSE OUTPUT-FILE.                                                   
016200 Z100-EXIT. EXIT.                                                         
  