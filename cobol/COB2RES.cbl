000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID.    COB2RES.                                                  
000300*AUTHOR. R THORNTON                                                       
000400*REMARKS. SUB-PROGRAM CALLED BY ASSEMBLER PROGRAM SPOUTST.                
000500 ENVIRONMENT DIVISION.                                                    
000600 CONFIGURATION SECTION.                                                   
000700 INPUT-OUTPUT SECTION.                                                    
000710 FILE-CONTROL.                                                            
000800 DATA DIVISION.                                                           
000810 FILE SECTION.                                                            
000900 WORKING-STORAGE SECTION.                                                 
001000 77  FILLER PIC X(36)  VALUE                                              
001100     'COB2RES WORKING STORAGE BEGINS HERE'.                               
001400 LINKAGE SECTION.                                                         
001500 01  ONE-BYTE                PIC X.                                       
001600                                                                          
001700 PROCEDURE DIVISION USING ONE-BYTE.                                       
001800                                                                          
001900 A100-EXECUTIVE-CONTROL.                                                  
002000     PERFORM A100-INITIALIZATION.                                         
002100     PERFORM B100-MAINLINE-PROCESSING.                                    
002200     PERFORM Z100-END-OF-PROCESSING.                                      
002300     GOBACK.                                                              
002400                                                                          
002500 A100-INITIALIZATION.                                                     
002600     MOVE '1' TO ONE-BYTE.                                                
002700                                                                          
002800 B100-MAINLINE-PROCESSING.                                                
002900     MOVE '2' TO ONE-BYTE.                                                
003000                                                                          
003100 Z100-END-OF-PROCESSING.                                                  
003200     MOVE '3' TO ONE-BYTE.                                                
  