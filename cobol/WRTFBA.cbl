000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID.    WRTFBA.                                                   
000300 AUTHOR. R THORNTON                                                       
000400*REMARKS. TEST BUCKET FOR WRITING HEX 5A AS CARRIAGE CONTROL TO           
000410*         A RECFM=FBA FILE.                                               
000500 ENVIRONMENT DIVISION.                                                    
000600 CONFIGURATION SECTION.                                                   
000700 INPUT-OUTPUT SECTION.                                                    
000800 FILE-CONTROL.                                                            
000900*                                                                         
001000     SELECT PRINT-FILE ASSIGN TO UT-S-PRINT1.                             
001100*                                                                         
001200 DATA DIVISION.                                                           
001300*                                                                         
001400 FILE SECTION.                                                            
001500*                                                                         
001600 FD  PRINT-FILE                                                           
001700     RECORD CONTAINS 133 CHARACTERS                                       
001800     RECORDING MODE IS F                                                  
001900     BLOCK CONTAINS 0 RECORDS                                             
002000     LABEL RECORD IS STANDARD                                             
002100     DATA RECORDS ARE PRINT-RECORD.                                       
002200*                                                                         
002300 01  PRINT-RECORD.                                                        
002310     05 CARRIAGE-CONTROL     PIC X.                                       
002320     05 PRINT-DATA           PIC X(132).                                  
002400*                                                                         
002500 WORKING-STORAGE SECTION.                                                 
002600                                                                          
002700 77  FILLER PIC X(36)  VALUE                                              
002800     'WRTFBA WORKING STORAGE BEGINS HERE'.                                
002801 77  COUNTER                 PIC S9(3) COMP-3 VALUE ZERO.                 
002810 01  PRINT-AFP5A-RECORD.                                                  
002820     05 CARRIAGE-CONTROL     PIC X VALUE X'5A'.                           
002830     05 FILLER               PIC X(29) VALUE                              
002840                             'THIS IS AFP-5A RECORD NUMBER '.             
002850     05 AFP5A-NUMBER         PIC 999.                                     
002900     05 FILLER               PIC X(100) VALUE SPACES.                     
002910 01  PRINT-BLANK-RECORD.                                                  
002920     05 CARRIAGE-CONTROL     PIC X VALUE ' '.                             
002930     05 FILLER               PIC X(28) VALUE                              
002940                             'THIS IS BLANK RECORD NUMBER '.              
002950     05 BLANK-NUMBER         PIC 999.                                     
002960     05 FILLER               PIC X(101) VALUE SPACES.                     
002970 01  PRINT-ONE-RECORD.                                                    
002980     05 CARRIAGE-CONTROL     PIC X VALUE '1'.                             
002990     05 FILLER               PIC X(28) VALUE                              
002991                             'THIS IS ONE RECORD NUMBER '.                
002992     05 ONE-NUMBER           PIC 999.                                     
002993     05 FILLER               PIC X(101) VALUE SPACES.                     
002994 01  PRINT-ZERO-RECORD.                                                   
002995     05 CARRIAGE-CONTROL     PIC X VALUE '0'.                             
002996     05 FILLER               PIC X(27) VALUE                              
002997                             'THIS IS ZERO RECORD NUMBER '.               
002998     05 ZERO-NUMBER          PIC 999.                                     
002999     05 FILLER               PIC X(102) VALUE SPACES.                     
003000 01  PRINT-DASH-RECORD.                                                   
003001     05 CARRIAGE-CONTROL     PIC X VALUE '-'.                             
003002     05 FILLER               PIC X(27) VALUE                              
003003                             'THIS IS DASH RECORD NUMBER '.               
003004     05 DASH-NUMBER          PIC 999.                                     
003005     05 FILLER               PIC X(102) VALUE SPACES.                     
003006 01  PRINT-PLUS-RECORD.                                                   
003007     05 CARRIAGE-CONTROL     PIC X VALUE '+'.                             
003008     05 FILLER               PIC X(27) VALUE                              
003009                             'THIS IS PLUS RECORD NUMBER '.               
003010     05 PLUS-NUMBER          PIC 999.                                     
003011     05 FILLER               PIC X(102) VALUE SPACES.                     
003020 PROCEDURE DIVISION.                                                      
003100     OPEN OUTPUT PRINT-FILE.                                              
003200 MAINLINE.                                                                
003300     PERFORM WRITE-OUTPUT VARYING COUNTER FROM 1 BY 1 UNTIL               
003400          COUNTER IS GREATER THAN 5.                                      
003500     CLOSE PRINT-FILE.                                                    
003600     GOBACK.                                                              
003700 WRITE-OUTPUT.                                                            
003800     MOVE COUNTER TO AFP5A-NUMBER, BLANK-NUMBER, ONE-NUMBER,              
003900                     ZERO-NUMBER, DASH-NUMBER, PLUS-NUMBER.               
004000     WRITE PRINT-RECORD FROM PRINT-AFP5A-RECORD.                          
004100     WRITE PRINT-RECORD FROM PRINT-BLANK-RECORD.                          
004200     WRITE PRINT-RECORD FROM PRINT-ONE-RECORD.                            
004300     WRITE PRINT-RECORD FROM PRINT-ZERO-RECORD.                           
004400     WRITE PRINT-RECORD FROM PRINT-DASH-RECORD.                           
004500     WRITE PRINT-RECORD FROM PRINT-PLUS-RECORD.                           
  