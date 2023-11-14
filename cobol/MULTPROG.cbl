000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID. MULTPROG.                                                    
000300 ENVIRONMENT DIVISION.                                                    
000400 INPUT-OUTPUT SECTION.                                                    
000500*                                                                         
000600 FILE-CONTROL.                                                            
000700     SELECT IN-FILE  ASSIGN TO UT-S-DDTEXTS.                              
000800     SELECT OUT-FILE ASSIGN TO UT-S-DDPRINT.                              
000900 DATA DIVISION.                                                           
001000 FILE SECTION.                                                            
001100*                                                                         
001200 FD   IN-FILE                                                             
001300      RECORDING MODE IS F                                                 
001400      RECORD CONTAINS 80 CHARACTERS                                       
001500      BLOCK CONTAINS 0 RECORDS                                            
001600      LABEL RECORDS ARE STANDARD                                          
001700      DATA RECORD IS IN-RECORD.                                           
001800 01   IN-RECORD            PIC A(80).                                     
001900*                                                                         
002000 FD   OUT-FILE                                                            
002100      RECORDING MODE IS F                                                 
002200      RECORD CONTAINS 80 CHARACTERS                                       
002300      BLOCK CONTAINS 0 RECORDS                                            
002400      LABEL RECORDS ARE STANDARD                                          
002500      DATA RECORD IS OUT-RECORD.                                          
002600 01   OUT-RECORD           PIC X(80).                                     
002700*                                                                         
002800 WORKING-STORAGE SECTION.                                                 
002900 77   TOTAL-COUNT          PIC 999   VALUE IS 0.                          
003000 77   PASS-CODE            PIC 9.                                         
003100 01   WORK-RECORD.                                                        
003200      02  NAME             PIC A(10) VALUE IS SPACES.                     
003300      02  FILLER           PIC A(5)  VALUE IS SPACES.                     
003400      02  MONTH            PIC A(5)  VALUE IS SPACES.                     
003500      02  FILLER           PIC A(60) VALUE IS SPACES.                     
003600*                                                                         
003700 PROCEDURE DIVISION.                                                      
003800 BEGIN.                                                                   
003900      OPEN INPUT IN-FILE                                                  
004000           OUTPUT OUT-FILE.                                               
004100 READ-WRITE.                                                              
004200      READ IN-FILE INTO WORK-RECORD                                       
004300           AT END GO TO FINISH.                                           
004400      WRITE OUT-RECORD FROM WORK-RECORD.                                  
004500 BIRTH-MONTH.                                                             
004600      IF MONTH IS EQUAL TO 'APRIL'                                        
004700           MOVE 1 TO PASS-CODE,                                           
004800      ELSE                                                                
004900           MOVE 2 TO PASS-CODE.                                           
005000 CALL-TWOPROG.                                                            
005100      CALL 'TWOPROG' USING PASS-CODE.                                     
005200      GO TO READ-WRITE.                                                   
005300 FINISH.                                                                  
005400      MOVE 3 TO PASS-CODE.                                                
005500      CALL 'TWOPROG' USING PASS-CODE, TOTAL-COUNT.                        
005600      MOVE SPACES TO OUT-RECORD.                                          
005700      WRITE OUT-RECORD FROM TOTAL-COUNT.                                  
005800      CLOSE IN-FILE.                                                      
005900      CLOSE OUT-FILE.                                                     
006000      STOP RUN.                                                           

