000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID. VSAMVERB.                                                    
000300 AUTHOR. R THORNTON.                                                      
000400 DATE-WRITTEN.  OCT 1992.                                                 
000500 DATE-COMPILED.                                                           
000600*****************************************************************         
000700*REMARKS. CONTAINS COBOL VSAM PROCESSING CODE.                  *         
000800*****************************************************************         
000900 ENVIRONMENT DIVISION.                                                    
001000 INPUT-OUTPUT SECTION.                                                    
001100*                                                                         
001200 FILE-CONTROL.                                                            
001300     SELECT VSAM-FILE ASSIGN TO VSAM01                                    
001400            ORGANIZATION IS INDEXED                                       
001500            ACCESS IS DYNAMIC                                             
001600            RECORD KEY IS VSAM-KEY                                        
001700            FILE STATUS IS FILE-STATUS.                                   
001800*                                                                         
001900 DATA DIVISION.                                                           
002000 FILE SECTION.                                                            
002100 FD  VSAM-FILE.                                                           
002200 01  VSAM-RECORD.                                                         
002300     05  VSAM-KEY                     PIC X(8).                           
002400     05  VSAM-DATA                    PIC X(72).                          
002500*                                                                         
002600 WORKING-STORAGE SECTION.                                                 
002700 77  FILLER                           PIC X(44)                           
002800       VALUE 'COB2SUBR WORKING STORAGE SECTION STARTS HERE'.              
002900*                                                                         
003000 01  SWITCHES.                                                            
003100     05  FILE-STATUS                  PIC XX.                             
003200         88  SUCCESSFUL-COMPLETION    VALUE '00'.                         
003300         88  NO-RECORD-FOUND          VALUE '23'.                         
003400*                                                                         
003500 01  COUNTERS.                                                            
003600     05  TOTAL-READ                   PIC S9(9) COMP-3 VALUE +0.          
003700*                                                                         
003800 LINKAGE SECTION.                                                         
003900 01  REQUEST-CODE                     PIC X.                              
004000     88  OPEN-REQUEST                 VALUE 'O'.                          
004100     88  READ-REQUEST                 VALUE 'R'.                          
004200     88  CLOSE-REQUEST                VALUE 'C'.                          
004300*                                                                         
004400 01  RECORD-RETURN-AREA               PIC X(80).                          
004500*                                                                         
004600 01  RECORD-KEY-ARGUMENT              PIC X(8).                           
004700*                                                                         
004800 01  COMPLETION-CODE                  PIC X.                              
004900     88  SUCCESSFUL-OPERATION         VALUE 'Y'.                          
005000     88  RECORD-NOT-FOUND             VALUE 'N'.                          
005100*                                                                         
005200 PROCEDURE DIVISION USING REQUEST-CODE,                                   
005300                          COMPLETION-CODE,                                
005400                          RECORD-RETURN-AREA,                             
005500                          RECORD-KEY-ARGUMENT.                            
005600*                                                                         
005700 A000-MAINLINE.                                                           
005800     PERFORM B010-INITIALIZE THRU B010-EXIT.                              
005900     PERFORM C010-PROCESS THRU C010-EXIT.                                 
006000     DELETE VSAM-FILE RECORD.                                             
006100     EXIT PROGRAM.                                                        
006200*                                                                         
006300 B010-INITIALIZE.                                                         
006400     MOVE 'Y' TO COMPLETION-CODE.                                         
006500 B010-EXIT.                                                               
006600     EXIT.                                                                
006700*                                                                         
006800 C010-PROCESS.                                                            
006900     IF READ-REQUEST                                                      
007000         PERFORM J010-READ-FILE THRU J010-EXIT                            
007100         REWRITE VSAM-RECORD FROM RECORD-RETURN-AREA                      
007200     ELSE IF OPEN-REQUEST                                                 
007300         PERFORM K010-OPEN-FILE THRU K010-EXIT                            
007400     ELSE IF CLOSE-REQUEST                                                
007500         PERFORM Y010-CLOSE-FILE THRU Y010-EXIT                           
007600     ELSE                                                                 
007700         DISPLAY 'COB2SUBR: INVALID REQUEST CODE FROM CALLER'             
007800         CALL 'COBABEND'.                                                 
007900 C010-EXIT.                                                               
008000     EXIT.                                                                
008100*                                                                         
008200 J010-READ-FILE.                                                          
008300     MOVE RECORD-KEY-ARGUMENT TO VSAM-KEY.                                
008400     READ VSAM-FILE RECORD INTO RECORD-RETURN-AREA.                       
008500     IF SUCCESSFUL-COMPLETION                                             
008600         ADD 1 TO TOTAL-READ                                              
008700     ELSE IF NO-RECORD-FOUND                                              
008800         MOVE 'N' TO COMPLETION-CODE                                      
008900     ELSE                                                                 
009000         DISPLAY 'COB2SUBR: READ FAILED FOR VSAM01 FILE'                  
009100         PERFORM Z010-ERRORS THRU Z010-EXIT.                              
009200 J010-EXIT.                                                               
009300     EXIT.                                                                
009400*                                                                         
009500 K010-OPEN-FILE.                                                          
009600     OPEN I-O VSAM-FILE.                                                  
009700     IF SUCCESSFUL-COMPLETION                                             
009800         NEXT SENTENCE                                                    
009900     ELSE                                                                 
010000         DISPLAY 'COB2SUBR: OPEN FAILED FOR VSAM01 FILE'                  
010100         PERFORM Z010-ERRORS THRU Z010-EXIT.                              
010200     START VSAM-FILE.                                                     
010300 K010-EXIT.                                                               
010400     EXIT.                                                                
010500*                                                                         
010600 Y010-CLOSE-FILE.                                                         
010700     CLOSE VSAM-FILE.                                                     
010800     IF SUCCESSFUL-COMPLETION                                             
010900         NEXT SENTENCE                                                    
011000     ELSE                                                                 
011100         DISPLAY 'COB2SUBR: CLOSE FAILED FOR VSAM01 FILE'                 
011200         PERFORM Z010-ERRORS THRU Z010-EXIT.                              
011300 Y010-EXIT.                                                               
011400     EXIT.                                                                
011500*                                                                         
011600 Z010-ERRORS.                                                             
011700     DISPLAY 'COB2SUBR: VSAM01 FILE STATUS IS: ' FILE-STATUS.             
011800         CALL 'COBABEND'.                                                 
011900 Z010-EXIT.                                                               
012000     EXIT.                                                                
