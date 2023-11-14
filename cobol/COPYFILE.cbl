000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID.    COPYFILE.                                                 
000300 AUTHOR. R THORNTON                                                       
000400*REMARKS. PROGRAM TO COPY INPUT1 TO OUTPUT1 USING FILES WITH              
000410*         NO BLOCK CONTAINS CLAUSE IN ORDER TO TEST A REPORTED            
000420*         EXECUTION TIME FAILURE.                                         
000500 ENVIRONMENT DIVISION.                                                    
000600 CONFIGURATION SECTION.                                                   
000700 INPUT-OUTPUT SECTION.                                                    
000710 FILE-CONTROL.                                                            
000720     SELECT INPUT-FILE ASSIGN TO INPUT1.                                  
000730     SELECT OUTPUT-FILE ASSIGN TO OUTPUT1.                                
000800 DATA DIVISION.                                                           
000810 FILE SECTION.                                                            
000820 FD  INPUT-FILE                                                           
000821     RECORDING MODE IS F                                                  
000830     LABEL RECORDS ARE STANDARD.                                          
000840 01  INPUT-RECORD.                                                      10
000850   05  FIELD-1               PIC XX.                                    20
000860   05  FIELD-2               PIC X(70).                                 30
000870   05  FILLER                PIC X(8).                                  40
000880 FD  OUTPUT-FILE                                                          
000881     RECORDING MODE IS F                                                  
000890     LABEL RECORDS ARE STANDARD.                                          
000891 01  OUTPUT-RECORD.                                                     10
000892   05  OUT-FIELD-1           PIC XX.                                    20
000893   05  OUT-FIELD-2           PIC X(70).                                 30
000894   05  FILLER                PIC X(8).                                  40
000900 WORKING-STORAGE SECTION.                                                 
001000 77  FILLER PIC X(36)  VALUE                                              
001100     'COPYFILE WORKING STORAGE BEGINS HERE'.                              
001101                                                                          
001110 01  MISCELLANEOUS-AREAS.                                                 
001200     05  EOF-INPUT-SWITCH    PIC X VALUE 'N'.                             
001300         88  MORE-RECORDS    VALUE 'N'.                                   
001400         88  EOF-INPUT       VALUE 'Y'.                                   
001600                                                                          
001700 PROCEDURE DIVISION.                                                      
001800                                                                          
001900 A100-EXECUTIVE-CONTROL.                                                  
002000     PERFORM A100-INITIALIZATION.                                         
002100     PERFORM B100-MAINLINE-PROCESSING UNTIL EOF-INPUT.                    
002200     PERFORM Z100-END-OF-PROCESSING.                                      
002300     GOBACK.                                                              
002400                                                                          
002500 A100-INITIALIZATION.                                                     
002600     OPEN INPUT INPUT-FILE                                                
002610          OUTPUT OUTPUT-FILE.                                             
002700                                                                          
002800 B100-MAINLINE-PROCESSING.                                                
002900     PERFORM C100-READ-INPUT.                                             
003000     IF MORE-RECORDS                                                      
003010         PERFORM D100-WRITE-OUTPUT.                                       
003020                                                                          
003030 C100-READ-INPUT.                                                         
003040     IF EOF-INPUT                                                         
003050         NEXT SENTENCE                                                    
003060     ELSE                                                                 
003070         READ INPUT-FILE                                                  
003080             AT END MOVE 'Y' TO EOF-INPUT-SWITCH.                         
003090                                                                          
003091 D100-WRITE-OUTPUT.                                                       
003092     WRITE OUTPUT-RECORD FROM INPUT-RECORD.                               
003093                                                                          
003100 Z100-END-OF-PROCESSING.                                                  
003200     CLOSE INPUT-FILE, OUTPUT-FILE.                                       

