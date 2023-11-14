000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID.    CALLGTVL.                                                 
000300 AUTHOR. R THORNTON                                                       
000400*REMARKS. TEST BUCKET FOR CALLING GETVOLS SUBROUTINE.                     
000500 ENVIRONMENT DIVISION.                                                    
000600 CONFIGURATION SECTION.                                                   
000700 INPUT-OUTPUT SECTION.                                                    
000800 FILE-CONTROL.                                                            
000900*                                                                         
001000     SELECT INPUT-FILE ASSIGN TO UT-S-INPUT1.                             
001100     SELECT PRINT-FILE  ASSIGN TO UT-S-PRINT1.                            
001200*                                                                         
001300 DATA DIVISION.                                                           
001400*                                                                         
001500 FILE SECTION.                                                            
001600*                                                                         
001700 FD  INPUT-FILE                                                           
001800     RECORD CONTAINS 80 CHARACTERS                                        
001900     RECORDING MODE IS F                                                  
002000     BLOCK CONTAINS 0 RECORDS                                             
002100     LABEL RECORD IS STANDARD                                             
002200     DATA RECORD IS SOURCE-RECORD.                                        
002300*                                                                         
002400 01  SOURCE-RECORD           PIC X(80).                                   
002500*                                                                         
002600 FD  PRINT-FILE                                                           
002700     RECORD CONTAINS 133 CHARACTERS                                       
002800     RECORDING MODE IS F                                                  
002900     BLOCK CONTAINS 0 RECORDS                                             
003000     LABEL RECORD IS STANDARD                                             
003100     DATA RECORD IS PRINT-RECORD.                                         
003200*                                                                         
003300 01  PRINT-RECORD.                                                        
003400     05  CARRIAGE-CONTROL    PIC X.                                       
003500     05  PRINT-DATA          PIC X(132).                                  
003600                                                                          
003700 WORKING-STORAGE SECTION.                                                 
003800                                                                          
003900 77  FILLER PIC X(36)  VALUE                                              
004000     'CALLGTVL WORKING STORAGE BEGINS HERE'.                              
004100                                                                          
004200 01  MISCELLANEOUS-AREAS.                                                 
004300     05 VOLSER-TABLE.                                                     
004400        10 VOLUME-SERIAL     PIC X(6) OCCURS 5 TIMES.                     
004500                                                                          
004600 PROCEDURE DIVISION.                                                      
004700                                                                          
004800     OPEN OUTPUT PRINT-FILE.                                              
004900     CALL 'GETVOLS' USING INPUT-FILE, VOLSER-TABLE.                       
005000     MOVE '1: GETVOLS CALLED FOR INPUT1 BEFORE OPEN:'                     
005100         TO PRINT-DATA.                                                   
005200     WRITE PRINT-RECORD AFTER ADVANCING 1 LINE.                           
005300     MOVE VOLSER-TABLE TO PRINT-DATA.                                     
005400     WRITE PRINT-RECORD AFTER ADVANCING 1 LINE.                           
005500     MOVE SPACES TO PRINT-DATA.                                           
005600     WRITE PRINT-RECORD AFTER ADVANCING 1 LINE.                           
005700     OPEN INPUT INPUT-FILE.                                               
005800     CALL 'GETVOLS' USING INPUT-FILE, VOLSER-TABLE.                       
005900     MOVE '1: GETVOLS CALLED FOR INPUT1 AFTER OPEN:'                      
006000         TO PRINT-DATA.                                                   
006100     WRITE PRINT-RECORD AFTER ADVANCING 1 LINE.                           
006200     MOVE VOLSER-TABLE TO PRINT-DATA.                                     
006300     WRITE PRINT-RECORD AFTER ADVANCING 1 LINE.                           
006400     MOVE SPACES TO PRINT-DATA.                                           
006500     WRITE PRINT-RECORD AFTER ADVANCING 1 LINE.                           
006600     CALL 'GETVOLS' USING PRINT-FILE, VOLSER-TABLE.                       
006700     MOVE '1: GETVOLS CALLED FOR PRINT1 AFTER OPEN:'                      
006800         TO PRINT-DATA.                                                   
006900     WRITE PRINT-RECORD AFTER ADVANCING 1 LINE.                           
007000     MOVE VOLSER-TABLE TO PRINT-DATA.                                     
007100     WRITE PRINT-RECORD AFTER ADVANCING 1 LINE.                           
007200     MOVE SPACES TO PRINT-DATA.                                           
007300     WRITE PRINT-RECORD AFTER ADVANCING 1 LINE.                           
007400     GOBACK.                                                              
  