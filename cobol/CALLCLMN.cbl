000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID.    CALLCLMN.                                                 
000300 AUTHOR. R THORNTON                                                       
000400*REMARKS. TEST BUCKET FOR THE CLISTMON PROGRAM                            
000500 ENVIRONMENT DIVISION.                                                    
000600 CONFIGURATION SECTION.                                                   
000700 INPUT-OUTPUT SECTION.                                                    
000710 FILE-CONTROL.                                                            
000800 DATA DIVISION.                                                           
000810 FILE SECTION.                                                            
000900 WORKING-STORAGE SECTION.                                                 
001000 77  FILLER PIC X(36)  VALUE                                              
001100     'CALLCLMN WORKING STORAGE BEGINS HERE'.                              
001200 77  MYNAME PIC X(8) VALUE 'CALLCLMN'.                                    
001700 PROCEDURE DIVISION.                                                      
002900     CALL 'CLISTMON' USING MYNAME.                                        
003200     GOBACK.                                                              
  