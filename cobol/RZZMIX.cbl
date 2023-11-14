000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID.             RZZMIX.                                          
000300 AUTHOR.                 WADE DONAHUE.                                    
000400 DATE-WRITTEN.           OCTOBER 1990.                                    
000500* THIS PROGRAM IS USED TO TEST THE CALLREXX SUBROUTINE.                   
000600 DATE-COMPILED.                                                           
000700 ENVIRONMENT DIVISION.                                                    
000800 CONFIGURATION SECTION.                                                   
000900 INPUT-OUTPUT SECTION.                                                    
001000 DATA DIVISION.                                                           
001100 WORKING-STORAGE SECTION.                                                 
001200 01  GENERAL-WORK-AREAS.                                                  
001300   05  WS-RET-CODE PIC S9(04) COMP VALUE +0.                              
001400 01  REXX-PARM.                                                           
001500   05  REXX-LEN    PIC S9(04) COMP VALUE +19.                             
001600   05  REXX-EXEC   PIC X(09) VALUE 'MIXABC'.                              
001700   05  REXX-ARG    PIC X(50) VALUE 'ABCDEFGHIJ'.                          
001800 01  RETURN-ARG    PIC X(50) VALUE SPACES.                                
001900 PROCEDURE DIVISION.                                                      
002000       CALL 'CALLREXX' USING REXX-PARM, RETURN-ARG                        
002100       PERFORM REPORT-RESULTS THRU RRXIT.                                 
002200       MOVE 'MIX123' TO REXX-EXEC                                         
002300       MOVE '1234567890' TO REXX-ARG.                                     
002400       CALL 'CALLREXX' USING REXX-PARM, RETURN-ARG                        
002500       PERFORM REPORT-RESULTS THRU RRXIT.                                 
002600       MOVE 'MIXABC' TO REXX-EXEC                                         
002700       MOVE 'ABCDEFGHIJ' TO REXX-ARG.                                     
002800       CALL 'CALLREXX' USING REXX-PARM, RETURN-ARG                        
002900       PERFORM REPORT-RESULTS THRU RRXIT.                                 
003000       CALL 'CALLREXX' USING REXX-PARM, RETURN-ARG                        
003100       PERFORM REPORT-RESULTS THRU RRXIT.                                 
003200       MOVE 'MIX123' TO REXX-EXEC                                         
003300       MOVE '1234567890' TO REXX-ARG.                                     
003400       CALL 'CALLREXX' USING REXX-PARM, RETURN-ARG                        
003500       PERFORM REPORT-RESULTS THRU RRXIT.                                 
003600       CALL 'COBABEND'.                                                   
003700       GOBACK.                                                            
003800 REPORT-RESULTS.                                                          
003900     MOVE RETURN-CODE TO WS-RET-CODE.                                     
004000     DISPLAY 'RETURN CODE = ' WS-RET-CODE.                                
004100     DISPLAY 'REXX ' REXX-ARG.                                            
004200     DISPLAY 'RET  ' RETURN-ARG.                                          
004300     MOVE SPACES TO RETURN-ARG.                                           
004400     DISPLAY RETURN-ARG.                                                  
004500 RRXIT. EXIT.                                                             

