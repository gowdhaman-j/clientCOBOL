000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID.    CALLCRVL.                                                 
000300 AUTHOR. R THORNTON                                                       
000400*REMARKS. TEST BUCKET TO TEST THE CURRVOL SUBROUTINE.                     
000500 ENVIRONMENT DIVISION.                                                    
000600 CONFIGURATION SECTION.                                                   
000700 INPUT-OUTPUT SECTION.                                                    
000701 FILE-CONTROL.                                                            
000702     SELECT IN-FILE ASSIGN TO INPUT1.                                     
000800 DATA DIVISION.                                                           
000810 FILE SECTION.                                                            
000820 FD  IN-FILE                                                              
000830     BLOCK CONTAINS 0 RECORDS                                             
000840     RECORDING MODE IS F                                                  
000850     LABEL RECORDS ARE STANDARD                                           
000860     RECORD CONTAINS 80 CHARACTERS                                        
000870     DATA RECORD IS IN-RECORD.                                            
000880 01  IN-RECORD                  PIC X(80).                                
000890                                                                          
000900 WORKING-STORAGE SECTION.                                                 
001000 77  FILLER PIC X(36)  VALUE                                              
001100     'CALLCRVL WORKING STORAGE BEGINS HERE'.                              
001200 77  VOLUME-SERIAL           PIC X(6) VALUE SPACES.                       
001700 PROCEDURE DIVISION.                                                      
002900     OPEN INPUT IN-FILE.                                                  
003000     CALL 'CURRVOL' USING IN-FILE, VOLUME-SERIAL.                         
003100     IF VOLUME-SERIAL = HIGH-VALUES                                       
003110         DISPLAY 'CURRVOL RETURNED HIGH-VALUES'                           
003120     ELSE                                                                 
003130         DISPLAY 'CURRVOL RETURNED VOLUME SERIAL: '                       
003140                 VOLUME-SERIAL.                                           
003200     GOBACK.                                                              
  