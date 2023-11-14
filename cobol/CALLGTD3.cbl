000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID.    CALLGTD3.                                                 
000300 AUTHOR. R THORNTON                                                       
000400*TEST BUCKET TO EXERCISE THE GETDATE3 SUBROUTINE. SENDS                   
000500*ALL DATES IN A YEAR TO GETDATE3 BY WRITING SUCCESSIVELY                  
000600*HIGHER DATES TO THE DATE FILE, THEN CALLING GETDATE3. EACH               
000700*OF THE GETDATE FORMATS IS EXERCISED AND THE RESULTING DATE               
000800*RETURNED IS PRINTED.                                                     
001100 ENVIRONMENT DIVISION.                                                    
001200 CONFIGURATION SECTION.                                                   
001300 INPUT-OUTPUT SECTION.                                                    
001400 FILE-CONTROL.                                                            
001500     SELECT DATE-FILE ASSIGN TO DATE2.                                    
001600     SELECT PRINT-FILE ASSIGN TO PRINT1.                                  
001700 DATA DIVISION.                                                           
001800 FILE SECTION.                                                            
001900 FD  DATE-FILE                                                            
002000     RECORDING MODE IS F                                                  
002100     LABEL RECORDS ARE STANDARD.                                          
002200 01  DATE-RECORD             PIC X(80).                                 10
002300 FD  PRINT-FILE                                                           
002400     RECORDING MODE IS F                                                  
002500     BLOCK CONTAINS 0 RECORDS                                             
002600     LABEL RECORDS ARE STANDARD.                                          
002700 01  PRINT-RECORD.                                                      10
002900     05  FILLER              PIC X(4).                                    
003000     05  PRINT-DATE          PIC X(8).                                    
003100     05  FILLER              PIC X.                                       
003200     05  PRINT-REQUEST       PIC X.                                       
003300     05  FILLER              PIC X.                                       
003400     05  PRINT-GETDATE       PIC X(18).                                   
003500     05  FILLER              PIC X(47).                                   
003800 WORKING-STORAGE SECTION.                                                 
003900 77  FILLER PIC X(36)  VALUE                                              
004000     'CALLGTD3 WORKING STORAGE BEGINS HERE'.                              
004100 01  WS-MISCELLANEOUS.                                                    
004200     05  ABOVE-BELOW-INDICATOR PIC X VALUE SPACE.                         
004210     05  WS-END-SWITCH       PIC X VALUE 'Y'.                             
004300         88  MORE-DATES      VALUE 'Y'.                                   
004400         88  NO-MORE-DATES   VALUE 'N'.                                   
004500     05  WS-DATE-RECORD.                                                  
004600         10  FILLER          PICTURE X(6) VALUE ' DATE='.                 
004700         10  WS-DATE.                                                     
004800             15  WS-MONTH    PIC 99 VALUE 01.                             
004900             15  FILLER      PIC X VALUE '/'.                             
005000             15  WS-DAY      PIC 99 VALUE 00.                             
005100             15  FILLER      PIC X VALUE '/'.                             
005200             15  WS-YEAR     PIC 99 VALUE 92.                             
005300     05  WS-GETDATE2-RETURN  PIC X(18).                                   
005400     05  WS-GETDATE-RETURN   PIC X(18).                                   
005500     05  WS-FORMAT-REQUEST   PIC X.                                       
005600                                                                          
005700 PROCEDURE DIVISION.                                                      
005800                                                                          
005900 A100-EXECUTIVE-CONTROL.                                                  
006000     PERFORM A100-INITIALIZATION.                                         
006100     PERFORM B100-MAINLINE-PROCESSING                                     
006200         UNTIL NO-MORE-DATES.                                             
006300     PERFORM Z100-END-OF-PROCESSING.                                      
006400     GOBACK.                                                              
006500                                                                          
006600 A100-INITIALIZATION.                                                     
006700     OPEN OUTPUT PRINT-FILE.                                              
006710     CALL 'WHEREAMI' USING ABOVE-BELOW-INDICATOR.                         
006800                                                                          
006900 B100-MAINLINE-PROCESSING.                                                
007000     PERFORM C100-INITIALIZE-DATE-FILE.                                   
007100     IF MORE-DATES                                                        
007200         PERFORM D100-EXERCISE-GETDATE3.                                  
007300                                                                          
007400 C100-INITIALIZE-DATE-FILE.                                               
007500     PERFORM C200-UPDATE-DATE.                                            
007600     IF MORE-DATES                                                        
007700         OPEN OUTPUT DATE-FILE                                            
007800         WRITE DATE-RECORD FROM WS-DATE-RECORD                            
007900         CLOSE DATE-FILE.                                                 
008000                                                                          
008100 C200-UPDATE-DATE.                                                        
008200     IF WS-DATE = '12/31/92'                                              
008300         MOVE 'N' TO WS-END-SWITCH.                                       
008400     ADD 1 TO WS-DAY.                                                     
008500     IF WS-DAY > 31                                                       
008600         ADD 1 TO WS-MONTH                                                
008700         MOVE 01 TO WS-DAY.                                               
008800                                                                          
008900 D100-EXERCISE-GETDATE3.                                                  
009000     MOVE 'A' TO WS-FORMAT-REQUEST.                                       
009100     PERFORM D200-CALL-GETDATE3.                                          
009200     MOVE 'S' TO WS-FORMAT-REQUEST.                                       
009300     PERFORM D200-CALL-GETDATE3.                                          
009400     MOVE 'Y' TO WS-FORMAT-REQUEST.                                       
009500     PERFORM D200-CALL-GETDATE3.                                          
009600     MOVE 'D' TO WS-FORMAT-REQUEST.                                       
009700     PERFORM D200-CALL-GETDATE3.                                          
009800     MOVE SPACES TO PRINT-RECORD.                                         
009900     PERFORM E100-PRINT-A-LINE.                                           
010000                                                                          
010100 D200-CALL-GETDATE3.                                                      
010200     MOVE ALL 'A' TO WS-GETDATE-RETURN.                                   
010300     MOVE WS-GETDATE-RETURN TO WS-GETDATE2-RETURN.                        
010400     CALL 'GETDATE3' USING WS-GETDATE-RETURN,                             
010500                          WS-FORMAT-REQUEST.                              
010800     PERFORM D300-PRINT-RESULTS.                                          
010900                                                                          
011000 D300-PRINT-RESULTS.                                                      
011100     MOVE SPACES TO PRINT-RECORD.                                         
011200     MOVE WS-FORMAT-REQUEST TO PRINT-REQUEST.                             
011300     MOVE WS-DATE TO PRINT-DATE.                                          
011400     MOVE WS-GETDATE-RETURN TO PRINT-GETDATE.                             
012000     PERFORM E100-PRINT-A-LINE.                                           
012100                                                                          
012200 E100-PRINT-A-LINE.                                                       
012300     WRITE PRINT-RECORD.                                                  
012400                                                                          
012500 Z100-END-OF-PROCESSING.                                                  
012600     CLOSE PRINT-FILE.                                                    
