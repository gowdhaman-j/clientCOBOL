000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID.    PRTHEX.                                                   
000300 AUTHOR. R THORNTON                                                       
000400*REMARKS. UTILITY PRTHEX PRINTS MEMORY IN HEX.                            
000500 ENVIRONMENT DIVISION.                                                    
000600 CONFIGURATION SECTION.                                                   
000700 INPUT-OUTPUT SECTION.                                                    
000800 FILE-CONTROL.                                                            
000900     SELECT PRINT-FILE ASSIGN TO UT-S-PRINT1.                             
001000 DATA DIVISION.                                                           
001100 FILE SECTION.                                                            
001200                                                                          
001300 FD PRINT-FILE                                                            
001400     RECORD CONTAINS 80 CHARACTERS                                        
001500     RECORDING MODE IS F                                                  
001600     BLOCK CONTAINS 0 RECORDS                                             
001700     LABEL RECORD IS STANDARD                                             
001800     DATA RECORD IS INPUT-RECORD.                                         
001900                                                                          
002000 01  PRINT-RECORD.                                                        
002100     05  PRINT-NUMBER              PIC XX.                                
002200     05  FILLER                    PIC X(78).                             
002300                                                                          
002400 WORKING-STORAGE SECTION.                                                 
002500 77  FILLER PIC X(36)  VALUE                                              
002600     'PRTHEX WORKING STORAGE BEGINS HERE'.                                
002700 01  MISCELLANY.                                                          
002800     05  QUOTIENT                PIC S9(4) COMP.                          
002900     05  INCOMING-BINARY-NUMBER  PIC S9(4) COMP.                          
003000     05  FILLER REDEFINES INCOMING-BINARY-NUMBER.                         
003100         10 FILLER               PIC X.                                   
003200         10 INCOMING-BINARY-BYTE PIC X.                                   
003300                                                                          
003400     05  HALFWORD                PIC S9(4) COMP.                          
003500     05  FILLER REDEFINES HALFWORD.                                       
003600         10 FILLER               PIC X.                                   
003700         10 HEX-BYTE             PIC X.                                   
003800                                                                          
003900     05  CONVERTED-NUMBER.                                                
004000         10  ZONE-DIGIT          PIC X.                                   
004100         10  NUMERIC-DIGIT       PIC X.                                   
004200                                                                          
004300 PROCEDURE DIVISION.                                                      
004400                                                                          
004500     OPEN OUTPUT PRINT-FILE.                                              
004600 MAINLINE.                                                                
004700     MOVE +0 TO INCOMING-BINARY-NUMBER.                                   
004800     PERFORM CONVERT-AND-PRINT 256 TIMES.                                 
004900     CLOSE PRINT-FILE.                                                    
005000     GOBACK.                                                              
005100 CONVERT-AND-PRINT.                                                       
005200     MOVE +0 TO HALFWORD.                                                 
005300     MOVE INCOMING-BINARY-BYTE TO HEX-BYTE.                               
005400     DIVIDE HALFWORD BY 16 GIVING QUOTIENT REMAINDER HALFWORD.            
005500     IF HALFWORD > 9                                                      
005600         ADD 183 TO HALFWORD                                              
005700     ELSE                                                                 
005800         ADD 240 TO HALFWORD.                                             
005900     MOVE HEX-BYTE TO NUMERIC-DIGIT.                                      
006000     MOVE +0 TO HALFWORD.                                                 
006100     MOVE INCOMING-BINARY-BYTE TO HEX-BYTE.                               
006200     DIVIDE HALFWORD BY 16 GIVING HALFWORD.                               
006300     IF HALFWORD > 9                                                      
006400         ADD 183 TO HALFWORD                                              
006500     ELSE                                                                 
006600         ADD 240 TO HALFWORD.                                             
006700     MOVE HEX-BYTE TO ZONE-DIGIT.                                         
006800     MOVE CONVERTED-NUMBER TO PRINT-NUMBER.                               
006900     WRITE PRINT-RECORD.                                                  
007000     ADD 1 TO INCOMING-BINARY-NUMBER.                                     

