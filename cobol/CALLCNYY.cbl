000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID.    CALLCNYY.                                                 
000300 AUTHOR. R THORNTON                                                       
000400*REMARKS. TEST BUCKET FOR THE CENTURYY SUBROUTINE.                        
000500 ENVIRONMENT DIVISION.                                                    
000600 CONFIGURATION SECTION.                                                   
000700 INPUT-OUTPUT SECTION.                                                    
000710 FILE-CONTROL.                                                            
000730     SELECT PRINT-FILE ASSIGN TO PRINT1.                                  
000800 DATA DIVISION.                                                           
000810 FILE SECTION.                                                            
000880 FD  PRINT-FILE                                                           
000881     RECORDING MODE IS F                                                  
000890     LABEL RECORDS ARE STANDARD.                                          
000891 01  PRINT-RECORD            PIC X(80).                                 10
000900                                                                          
000910 WORKING-STORAGE SECTION.                                                 
001000 77  FILLER PIC X(36)  VALUE                                              
001100     'CALLCNYY WORKING STORAGE BEGINS HERE'.                              
001110                                                                          
001200 01  MISCELLANEOUS-DATA.                                                  
001210     05  WHERE-BYTE                PIC X.                                 
001220         88  ABOVE-THE-LINE        VALUE 'A'.                             
001230         88  BELOW-THE-LINE        VALUE 'B'.                             
001300     05  FULL-DATE.                                                       
001400         10  YEAR-NBR              PIC 9(4).                              
001500         10  MONTH-NBR             PIC 99.                                
001510         10  DAY-NBR               PIC 99.                                
001520         10  HOUR-NBR              PIC 99.                                
001530         10  MINUTE-NBR            PIC 99.                                
001540         10  SECOND-NBR            PIC 99V9(6).                           
001541                                                                          
001550     05  ABOVE-MSG                 PIC X(80) VALUE                        
001551                         'RUNNING ABOVE THE LINE'.                        
001552     05  BELOW-MSG                 PIC X(80) VALUE                        
001553                         'RUNNING BELOW THE LINE'.                        
001554     05  BAD-WHERE-MSG             PIC X(80) VALUE                        
001555                         'INVALID RETURN FROM WHEREAMI'.                  
001556                                                                          
001557 01  PRINT-CENTURYY.                                                      
001558     05  FILLER                    PIC X VALUE SPACE.                     
001561     05  FILLER                    PIC X VALUE '/'.                       
001570     05  PRINT-MONTH-NBR           PIC 99.                                
001571     05  FILLER                    PIC X VALUE '/'.                       
001572     05  PRINT-YEAR-NBR            PIC 9(4).                              
001580     05  PRINT-DAY-NBR             PIC 99.                                
001581     05  FILLER                    PIC X VALUE ' '.                       
001590     05  PRINT-HOUR-NBR            PIC 99.                                
001591     05  FILLER                    PIC X VALUE ':'.                       
001592     05  PRINT-MINUTE-NBR          PIC 99.                                
001593     05  FILLER                    PIC X VALUE ':'.                       
001594     05  PRINT-SECOND-NBR          PIC 99.9(6).                           
001600                                                                          
001700 PROCEDURE DIVISION.                                                      
001800                                                                          
001900 A100-EXECUTIVE-CONTROL.                                                  
002000     OPEN OUTPUT PRINT-FILE.                                              
002001     CALL 'WHEREAMI' USING WHERE-BYTE.                                    
002002     IF ABOVE-THE-LINE                                                    
002003         WRITE PRINT-RECORD FROM ABOVE-MSG                                
002004     ELSE                                                                 
002005         IF BELOW-THE-LINE                                                
002006             WRITE PRINT-RECORD FROM BELOW-MSG                            
002007         ELSE                                                             
002008             WRITE PRINT-RECORD FROM BAD-WHERE-MSG.                       
002010     CALL 'CENTURYY' USING FULL-DATE.                                     
002100     MOVE YEAR-NBR TO PRINT-YEAR-NBR.                                     
002200     MOVE MONTH-NBR TO PRINT-MONTH-NBR.                                   
002210     MOVE DAY-NBR TO PRINT-DAY-NBR.                                       
002220     MOVE HOUR-NBR TO PRINT-HOUR-NBR.                                     
002230     MOVE MINUTE-NBR TO PRINT-MINUTE-NBR.                                 
002240     MOVE SECOND-NBR TO PRINT-SECOND-NBR.                                 
002250     WRITE PRINT-RECORD FROM PRINT-CENTURYY.                              
002260     CLOSE PRINT-FILE.                                                    
002300     GOBACK.                                                              
  