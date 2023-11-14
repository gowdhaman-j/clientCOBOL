000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID.    SKELETON.                                                 
000300 AUTHOR. R THORNTON                                                       
000400*REMARKS. TEST PROGRAM TO ANALYZE CONTINUATION LINE USAGE.                
000500 ENVIRONMENT DIVISION.                                                    
000600 CONFIGURATION SECTION.                                                   
000700 INPUT-OUTPUT SECTION.                                                    
000800 FILE-CONTROL.                                                            
000900 DATA DIVISION.                                                           
001000 FILE SECTION.                                                            
001100 WORKING-STORAGE SECTION.                                                 
001200 77  FILLER PIC X(72)  VALUE 'CONTINU WORKING STORAGE BEGINS HERE'        
001300     .                                                                    
001400 01  GENERAL-AREAS.                                                       
001500     05  CONTINUED-WITH-DASH PIC X(72) VALUE 'THIS IS A CONTINUED         
001600-    'NON-NUMERIC LITERAL WITH A DASH IN COLUMN 7.'.                      
001700     05  PERIOD-IN-COL-72                        PIC S9(8) COMP-3.        
001800     05  PERIOD-BEFORE-COL-72                   PIC S9(8) COMP-3.         
002300                                                                          
002400 PROCEDURE DIVISION.                                                      
002500                                                                          
002600 A100-EXECUTIVE-CONTROL.                                                  
002700     PERFORM                                                              
002800     D100-DISPLAY                                                         
002900     THRU                                                                 
003000     D100-EXIT.                                                           
003100     GOBACK.                                                              
003200 D100-DISPLAY.                                                            
003300     DISPLAY CONTINUED-WITH-DASH.                                         
003400 D100-EXIT. EXIT.                                                         
  