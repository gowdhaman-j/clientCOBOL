000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID.    CALLJBIN.                                                 
000300 AUTHOR. R THORNTON                                                       
000400*REMARKS. USED TO TEST CALLS TO THE JOBINFO SUBROUTINE                    
000500 ENVIRONMENT DIVISION.                                                    
000600 CONFIGURATION SECTION.                                                   
000700 INPUT-OUTPUT SECTION.                                                    
000800 FILE-CONTROL.                                                            
000900     SELECT PRINT-FILE ASSIGN TO PRINT1.                                  
001000 DATA DIVISION.                                                           
001100 FILE SECTION.                                                            
001200 FD  PRINT-FILE                                                           
001300     BLOCK CONTAINS 0 RECORDS                                             
001400     RECORD CONTAINS 133 CHARACTERS                                       
001500     LABEL RECORDS ARE STANDARD                                           
001600     RECORDING MODE IS F                                                  
001700     DATA RECORD IS PRINT-LINE.                                           
001800 01  PRINT-LINE.                                                          
001900     05  PRINT-CONTROL                  PIC X.                            
002000     05  PRINT-JOB-NAME                 PIC X(8).                         
002100     05  FILLER                         PIC XX.                           
002200     05  PRINT-JOB-NUMBER               PIC X(8).                         
002300     05  FILLER                         PIC X(114).                       
002400 WORKING-STORAGE SECTION.                                                 
002500 77  FILLER PIC X(36)  VALUE                                              
002600     "CALLJBIN WORKING STORAGE BEGINS HERE".                              
002700 01  JOB-INFO-AREA.                                                       
002800     05  JOB-NAME                      PIC X(8).                          
002900     05  JOB-NUMBER                    PIC X(8).                          
003000 PROCEDURE DIVISION.                                                      
003100     OPEN OUTPUT PRINT-FILE.                                              
003200     MOVE SPACES TO PRINT-LINE.                                           
003300     CALL "JOBINFO" USING JOB-INFO-AREA.                                  
003400     MOVE JOB-NAME TO PRINT-JOB-NAME.                                     
003500     MOVE JOB-NUMBER TO PRINT-JOB-NUMBER.                                 
003600     WRITE PRINT-LINE AFTER ADVANCING 1 LINE.                             
003700     CLOSE PRINT-FILE.                                                    
003800     GOBACK.                                                              
  