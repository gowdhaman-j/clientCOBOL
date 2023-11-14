000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID.    CALLABOV.                                                 
000300 AUTHOR. R THORNTON                                                       
000400*REMARKS. USED AS A COBOL II PROGRAM TO CALL WHEREAMI FROM ABOVE          
000410*         THE 16 MEGABYTE LINE.                                           
000500 ENVIRONMENT DIVISION.                                                    
000600 CONFIGURATION SECTION.                                                   
000700 INPUT-OUTPUT SECTION.                                                    
000710 FILE-CONTROL.                                                            
000800 DATA DIVISION.                                                           
000810 FILE SECTION.                                                            
000900 WORKING-STORAGE SECTION.                                                 
001000 77  FILLER PIC X(36)  VALUE                                              
001100     'CALLABV WORKING STORAGE BEGINS HERE'.                               
001200 77  WHEREAMI-RETURN-BYTE    PIC X VALUE '?'.                             
001300 77  BINARY-FIELD PIC S9(4) BINARY VALUE 234.                             
001310 77  BINARY-FULLWORD PIC S9(9) BINARY VALUE 234.                          
001320 77  BINARY-DOUBLEWORD PIC S9(18) BINARY VALUE 234.                       
001400 77  DECIMAL-FIELD PIC S9(5)V99 COMP-3 VALUE +567.89.                     
001500 77  ZONED-FIELD PIC S9(3)V9 VALUE -61.6.                                 
001600 77  RESULT-FIELD PIC ZZZ,ZZZ,ZZ9.99.                                     
001610 77  GETJOBN-NAME PIC X(8) VALUE 'GETJOBN'.                               
001620 77  JOB-NAME PIC X(8).                                                   
001700 PROCEDURE DIVISION.                                                      
001800     IF BINARY-FIELD IS POSITIVE ADD 1 TO BINARY-FIELD.                   
001900     IF DECIMAL-FIELD IS NEGATIVE SUBTRACT 1 FROM DECIMAL-FIELD.          
002000     IF ZONED-FIELD IS NUMERIC ADD 1 TO ZONED-FIELD.                      
002100     COMPUTE RESULT-FIELD = (ZONED-FIELD ** 2) + DECIMAL-FIELD            
002200         - BINARY-FIELD.                                                  
002900*    CALL 'WHEREAMI' USING WHEREAMI-RETURN-BYTE.                          
003000*    DISPLAY 'WHEREAMI-RETURN-BYTE = ' WHEREAMI-RETURN-BYTE.              
003100     CALL GETJOBN-NAME USING JOB-NAME.                                    
003110     DISPLAY 'BACK FROM GETJOBN WITH JOB NAME = ' JOB-NAME.               
003200     GOBACK.                                                              
  