000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID.    WRTVRBL.                                                  
000300 AUTHOR. R THORNTON                                                       
000400*REMARKS. TEST BUCKET FOR WRITING VARIABLE LENGTH RECORDS.                
000500 ENVIRONMENT DIVISION.                                                    
000600 CONFIGURATION SECTION.                                                   
000700 INPUT-OUTPUT SECTION.                                                    
000800 FILE-CONTROL.                                                            
000900*                                                                         
001000     SELECT OUTPUT-FILE ASSIGN TO UT-S-OUTPUT1.                           
001100*                                                                         
001200 DATA DIVISION.                                                           
001300*                                                                         
001400 FILE SECTION.                                                            
001500*                                                                         
001600 FD  OUTPUT-FILE                                                          
001700     RECORD CONTAINS 80 CHARACTERS                                        
001800     RECORDING MODE IS V                                                  
001900     BLOCK CONTAINS 0 RECORDS                                             
002000     LABEL RECORD IS STANDARD                                             
002100     DATA RECORDS ARE OUTPUT-REC1.                                        
002200*                                                                         
002300 01  OUTPUT-REC1             PIC X(26).                                   
002310 01  OUTPUT-REC2             PIC X(10).                                   
002400*                                                                         
002500 WORKING-STORAGE SECTION.                                                 
002600                                                                          
002700 77  FILLER PIC X(36)  VALUE                                              
002800     'WRTVRBL WORKING STORAGE BEGINS HERE'.                               
002900                                                                          
003000 PROCEDURE DIVISION.                                                      
003100     OPEN INPUT OUTPUT-FILE.                                              
003200 MAINLINE.                                                                
003300     READ OUTPUT-FILE AT END GO TO EOJ.                                   
003400     CALL 'DKH0215P' USING OUTPUT-REC1.                                   
003500     GO TO MAINLINE.                                                      
003600 EOJ.                                                                     
003700     GOBACK.                                                              
  