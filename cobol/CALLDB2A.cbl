000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID.    CALLDB2A.                                                 
000300 AUTHOR. R THORNTON                                                       
000400*REMARKS. USED AS A COBOL II PROGRAM TO CALL DB2ABEND FROM ABOVE          
000410*         THE 16 MEGABYTE LINE.                                           
000500 ENVIRONMENT DIVISION.                                                    
000600 CONFIGURATION SECTION.                                                   
000700 INPUT-OUTPUT SECTION.                                                    
000710 FILE-CONTROL.                                                            
000800 DATA DIVISION.                                                           
000810 FILE SECTION.                                                            
000900 WORKING-STORAGE SECTION.                                                 
001000 77  FILLER PIC X(36)  VALUE                                              
001100     'CALLDB2A WORKING STORAGE BEGINS HERE'.                              
001200 77  WHEREAMI-RETURN-BYTE    PIC X VALUE '?'.                             
001700 PROCEDURE DIVISION.                                                      
002900     CALL 'WHEREAMI' USING WHEREAMI-RETURN-BYTE.                          
003000     DISPLAY 'WHEREAMI-RETURN-BYTE = ' WHEREAMI-RETURN-BYTE.              
003100     CALL 'DB2ABEND'.                                                     
003200     GOBACK.                                                              
  