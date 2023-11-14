000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID.    SUBANAL.                                                  
000300 AUTHOR. R THORNTON                                                       
000400*REMARKS. SUBANAL IS USED TO TEST THE ASSEMBLER PROBRAM PULLGRPS,         
000410*         WHICH CALLS THIS PROGRAM DYNAMICALLY.                           
000420*         EACH TIME THIS PROGRAM IS CALLED, IT PRINTS THE CONTENT         
000430*         OF THE AREA PASSED BY PULLGRPS.                                 
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
000894 01  PRINT-LINE.                                                        10
000895   05  CARRIAGE-CONTROL-BYTE   PIC X.                                   20
000896   05  PRINT-DATA              PIC X(132).                              20
000900                                                                          
000910 WORKING-STORAGE SECTION.                                                 
001000 77  FILLER PIC X(36)  VALUE                                              
001100     'SUBANAL WORKING STORAGE BEGINS HERE'.                               
001200 77  SUB                     PIC S9(4) COMP VALUE +0.                     
001300                                                                          
001301 01  SWITCHES.                                                            
001302     05  FIRST-TIME-SWITCH     PIC X VALUE 'Y'.                           
001303         88  FIRST-TIME              VALUE 'Y'.                           
001310                                                                          
001320 01  WS-PRINT-LINE1.                                                      
001330     05  FILLER                PIC X(8) VALUE ' DSNAME='.                 
001340     05  PRT-DSNAME            PIC X(44).                                 
001350     05  FILLER                PIC X(9) VALUE '  MEMBER='.                
001360     05  PRT-MEMBER            PIC X(8).                                  
001370     05  FILLER                PIC X(64) VALUE SPACES.                    
001380                                                                          
001381 01  WS-PRINT-LINE2.                                                      
001382     05  FILLER                PIC X(19) VALUE                            
001383                                         ' STRING-FOUND-FLAG='.           
001384     05  PRT-MTCH-STRING-FLAG  PIC X.                                     
001385     05  FILLER                PIC X(14) VALUE '  MTCH-STRING='.          
001386     05  PRT-MTCH-STRING       PIC X(72).                                 
001387     05  FILLER                PIC X(27) VALUE SPACES.                    
001388                                                                          
001389 01  WS-PRINT-LINE3.                                                      
001390     05  FILLER                PIC X(16) VALUE                            
001391                                         ' END-FOUND-FLAG='.              
001392     05  PRT-END-FOUND-FLAG    PIC X.                                     
001393     05  FILLER                PIC X(24) VALUE                            
001394                                    '  NUMBER-LINES-IN-GROUP='.           
001395     05  PRT-NBR-LINES         PIC 999.                                   
001396     05  FILLER                PIC X(89) VALUE SPACES.                    
001397                                                                          
001398 01  WS-PRINT-LINE4.                                                      
001399     05  FILLER                PIC X(16) VALUE                            
001400                                         '    LINE-NUMBER='.              
001401     05  PRT-LINE-NUMBER       PIC 999.                                   
001402     05  FILLER                PIC X(12) VALUE '  LINE-DATA='.            
001404     05  PRT-GROUP-LINE        PIC X(80).                                 
001405     05  FILLER                PIC X(22) VALUE SPACES.                    
001406                                                                          
001426                                                                          
001430 LINKAGE SECTION.                                                         
001500 01  PASSAREA.                                                            
001510     05  LKG-DSNAME            PIC X(44).                                 
001520     05  LKG-MEMBER            PIC X(8).                                  
001530     05  LKG-MTCH-STRING       PIC X(72).                                 
001540     05  LKG-MTCH-STRING-FLAG  PIC X.                                     
001550         88  MATCH-STRING-FOUND VALUE 'Y'.                                
001560     05  LKG-END-FOUND-FLAG    PIC X.                                     
001570         88  NORMAL-STATEMENT-END-FOUND VALUE 'Y'.                        
001580     05  LKG-NBR-LINES         PIC S9(3) COMP-3.                          
001590     05  LKG-GROUP-LINE        PIC X(80)                                  
001591                               OCCURS 100 TIMES.                          
001600                                                                          
001700 PROCEDURE DIVISION USING PASSAREA.                                       
001800 A100-INITIALIZATION.                                                     
001900     IF FIRST-TIME                                                        
002000        MOVE 'N' TO FIRST-TIME-SWITCH,                                    
002610        OPEN OUTPUT PRINT-FILE.                                           
002700                                                                          
002710 B100-PROCESSING.                                                         
002800     MOVE LKG-DSNAME TO PRT-DSNAME.                                       
002900     MOVE LKG-MEMBER TO PRT-MEMBER.                                       
003000     WRITE PRINT-LINE FROM WS-PRINT-LINE1                                 
003010           AFTER ADVANCING 3 LINES.                                       
003030     MOVE LKG-MTCH-STRING-FLAG TO PRT-MTCH-STRING-FLAG.                   
003040     MOVE LKG-MTCH-STRING TO PRT-MTCH-STRING.                             
003050     WRITE PRINT-LINE FROM WS-PRINT-LINE2                                 
003060           AFTER ADVANCING 1 LINE.                                        
003080     MOVE LKG-MTCH-STRING-FLAG TO PRT-MTCH-STRING-FLAG.                   
003090     MOVE LKG-MTCH-STRING TO PRT-MTCH-STRING.                             
003091     WRITE PRINT-LINE FROM WS-PRINT-LINE3                                 
003092           AFTER ADVANCING 1 LINE.                                        
003094     PERFORM L100-PRINT-GROUP-LINE                                        
003095        VARYING SUB FROM 1 BY 1                                           
003096           UNTIL SUB > LKG-NBR-LINES.                                     
003098     GOBACK.                                                              
003099                                                                          
003100 L100-PRINT-GROUP-LINE.                                                   
003101     MOVE SUB TO PRT-LINE-NUMBER.                                         
003102     MOVE LKG-GROUP-LINE (SUB) TO PRT-GROUP-LINE.                         
003103     WRITE PRINT-LINE FROM WS-PRINT-LINE4                                 
003110           AFTER ADVANCING 1 LINE.                                        
  