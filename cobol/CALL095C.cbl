000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID.    CALL095C.                                                 
000300 AUTHOR. R THORNTON                                                       
000310**********************************************************************    
000400*REMARKS. CALL095C IS A TEST DRIVER FOR THE IEFSD095 BLOCK LETTER    *    
000410*         SUBROUTINE. AS WRITTEN, A CHARACTER STRING CONTAINING      *    
000411*         "BC0THORX" IS SENT TO THE IEFSD095 SUBROUTINE 9 TIMES,     *    
000412*         TO OBTAIN ALL 9 LINES THAT COMPRISE THE BLOCK LETTER EQUIV-*    
000413*         ALENT. ALL 9 LINES ARE PRINTED TO THE PRINT1 FILE AS THEY  *    
000414*         ARE OBTAINED FROM IEFSD095, SO THAT THE PRINT1 FILE        *    
000415*         SHOULD CONTAIN THE BLOCK LETTER RESULT.                    *    
000416*                                                                    *    
000417*         A NUMBER OF TESTS CAN BE MADE BY COMPILING THIS DRIVER     *    
000418*         UNDER EITHER COBOL II OR COBOL/390; COMPILING WITH DYNAM   *    
000419*         OR NODYNAM, AND LINKEDITING WITH AMODE=31,RMODE=ANY OR     *    
000420*         WITH AMODE=24,RMODE=24. A TOTAL OF 8 TESTS CAN BE MADE     *    
000421*         USING THESE OPTIONS.                                       *    
000425*                                                                    *    
000426*                   *** TEST JCL FOR THIS PROGRAM ***                *    
000427* //CALL095C EXEC  PGM=CALL095C                                      *    
000428* //STEPLIB  DD    DISP=SHR,DSN=.............                        *    
000429* //SYSUDUMP DD    SYSOUT=*                                          *    
000430* //PRINT1   DD    SYSOUT=*                                          *    
000440**********************************************************************    
000500 ENVIRONMENT DIVISION.                                                    
000600 CONFIGURATION SECTION.                                                   
000700 INPUT-OUTPUT SECTION.                                                    
000710 FILE-CONTROL.                                                            
000740     SELECT PRINT-FILE ASSIGN TO PRINT1.                                  
000800 DATA DIVISION.                                                           
000810 FILE SECTION.                                                            
000889 FD  PRINT-FILE                                                           
000890     BLOCK CONTAINS 0 RECORDS                                             
000891     RECORD CONTAINS 133 CHARACTERS                                       
000892     RECORDING MODE IS F                                                  
000893     LABEL RECORDS ARE STANDARD.                                          
000894 01  PRINT-RECORD.                                                      10
000895   05  CARRIAGE-CONTROL-BYTE PIC X.                                     20
000896   05  PRINT-DATA            PIC X(132).                                20
000900                                                                          
000910 WORKING-STORAGE SECTION.                                                 
001000 77  FILLER PIC X(36)  VALUE                                              
001100     'CALL095C WORKING STORAGE BEGINS HERE'.                              
001200 01  IEFSD095-PARAMETERS.                                                 
001300     05  IEFSD095-STRING           PIC X(8).                              
001400     05  IEFSD095-LINE-NUMBER      PIC S9(8) COMP.                        
001500     05  IEFSD095-RETURN-AREA      PIC X(133) VALUE SPACES.               
001510     05  IEFSD095-STRING-LENGTH    PIC S9(8) COMP.                        
001600                                                                          
001700 PROCEDURE DIVISION.                                                      
001800                                                                          
001900 A100-EXECUTIVE-CONTROL.                                                  
002000     PERFORM A100-INITIALIZATION.                                         
002100     PERFORM B100-MAINLINE-PROCESSING.                                    
002200     PERFORM Z100-END-OF-PROCESSING.                                      
002300     GOBACK.                                                              
002400                                                                          
002500 A100-INITIALIZATION.                                                     
002600     OPEN OUTPUT PRINT-FILE.                                              
002700                                                                          
002800 B100-MAINLINE-PROCESSING.                                                
002900     MOVE 'BC0THORX' TO IEFSD095-STRING.                                  
003000     MOVE +8 TO IEFSD095-STRING-LENGTH.                                   
003001     PERFORM C100-CALL-IEFSD095                                           
003002        VARYING IEFSD095-LINE-NUMBER FROM 1 BY 1 UNTIL                    
003003        IEFSD095-LINE-NUMBER > 12.                                        
003096                                                                          
003097 C100-CALL-IEFSD095.                                                      
003098     MOVE SPACES TO IEFSD095-RETURN-AREA.                                 
003099     CALL 'IEFSD095' USING IEFSD095-STRING,                               
003100                           IEFSD095-LINE-NUMBER,                          
003101                           IEFSD095-RETURN-AREA,                          
003102                           IEFSD095-STRING-LENGTH.                        
003103     MOVE IEFSD095-RETURN-AREA TO PRINT-DATA.                             
003104     PERFORM Y100-PRINT-A-LINE.                                           
003105                                                                          
003106 Y100-PRINT-A-LINE.                                                       
003107     WRITE PRINT-RECORD AFTER ADVANCING 1 LINE.                           
003108     MOVE SPACES TO PRINT-RECORD.                                         
003109                                                                          
003110 Z100-END-OF-PROCESSING.                                                  
003300     CLOSE PRINT-FILE.                                                    
  