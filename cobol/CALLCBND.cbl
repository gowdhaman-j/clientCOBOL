000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID.    CALLCBND.                                                 
000300 AUTHOR. R THORNTON                                                       
000400*REMARKS. TEST BUCKET TO TEST CALLS TO COBABEND.                          
000500 ENVIRONMENT DIVISION.                                                    
000600 CONFIGURATION SECTION.                                                   
000700 INPUT-OUTPUT SECTION.                                                    
000701 FILE-CONTROL.                                                            
000702     SELECT PRINT-FILE ASSIGN TO PRINT1.                                  
000800 DATA DIVISION.                                                           
000810 FILE SECTION.                                                            
000820 FD  PRINT-FILE                                                           
000830     BLOCK CONTAINS 0 RECORDS                                             
000840     RECORDING MODE IS F                                                  
000850     LABEL RECORDS ARE STANDARD                                           
000860     RECORD CONTAINS 80 CHARACTERS                                        
000870     DATA RECORD IS PRINT-LINE.                                           
000880 01  PRINT-LINE                 PIC X(80).                                
000890                                                                          
000900 WORKING-STORAGE SECTION.                                                 
001000 77  FILLER PIC X(36)  VALUE                                              
001100     'CALLCBND WORKING STORAGE BEGINS HERE'.                              
001200 01  MISC-AREAS.                                                          
001300     05  COBABEND          PIC X(8) VALUE 'COBABEND'.                     
001700 PROCEDURE DIVISION.                                                      
002900     OPEN OUTPUT PRINT-FILE.                                              
003000     CALL COBABEND.                                                       
003200     GOBACK.                                                              
  