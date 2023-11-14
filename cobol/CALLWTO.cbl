000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID.    CALLWTO.                                                  
000300 AUTHOR. R THORNTON                                                       
000400*REMARKS. CALL THE WTOPGMR PROGRAM TO TEST ITS OPERATION.                 
000500 ENVIRONMENT DIVISION.                                                    
000600 CONFIGURATION SECTION.                                                   
000700 INPUT-OUTPUT SECTION.                                                    
000800 DATA DIVISION.                                                           
000900                                                                          
000910 WORKING-STORAGE SECTION.                                                 
001000 77  FILLER PIC X(36)  VALUE                                              
001100     'CALLWTO WORKING STORAGE BEGINS HERE'.                               
001200 77  SUB                     PIC S9(4) COMP VALUE +0.                     
001300 01  WTO-TEXT.                                                            
001301     05  MSG-TEXT-LEFT       PIC X(57) VALUE                              
001302     'THIS IS THE TEXT AREA FOR A 72-BYTE MESSAGE TO BE DISPLAY'.         
001303     05  MSG-TEXT-RIGHT      PIC X(15) VALUE                              
001304     'ED BY WTOPGMR!!'.                                                   
001600                                                                          
001700 PROCEDURE DIVISION.                                                      
001800                                                                          
001900     CALL 'WTOPGMR' USING WTO-TEXT.                                       
002300     GOBACK.                                                              
  