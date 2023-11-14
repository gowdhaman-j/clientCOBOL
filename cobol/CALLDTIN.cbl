000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID.    CALLDTIN.                                                 
000300 AUTHOR. R THORNTON                                                       
000400*REMARKS. USED AS A VS COBOL PROGRAM TO CALL THE DATEINFO PROGRAM.        
000500 ENVIRONMENT DIVISION.                                                    
000600 CONFIGURATION SECTION.                                                   
000700 INPUT-OUTPUT SECTION.                                                    
000710 FILE-CONTROL.                                                            
000800 DATA DIVISION.                                                           
000810 FILE SECTION.                                                            
000900 WORKING-STORAGE SECTION.                                                 
001000 77  FILLER PIC X(36)  VALUE                                              
001100     'CALLDTIN WORKING STORAGE BEGINS HERE'.                              
001200 77  JULDATE PIC X(8) VALUE 'J2000015'.                                   
001300 77  GREGDATE PIC X(9) VALUE 'G12251935'.                                 
001700 PROCEDURE DIVISION.                                                      
002900     CALL 'DATEINFO' USING JULDATE.                                       
003000     CALL 'DATEINFO' USING GREGDATE.                                      
003200     GOBACK.                                                              
  