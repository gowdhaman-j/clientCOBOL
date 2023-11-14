000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID.    CALLGTVM.                                                 
000300 AUTHOR. R THORNTON                                                       
000400*REMARKS. CALLTHE GETVOLUM SUBROUTINE.                                    
000500 ENVIRONMENT DIVISION.                                                    
000600 CONFIGURATION SECTION.                                                   
000700 INPUT-OUTPUT SECTION.                                                    
000710 FILE-CONTROL.                                                            
000720     SELECT INPUT-FILE ASSIGN TO INPUT1.                                  
000730     SELECT OUTPUT-FILE ASSIGN TO OUTPUT1.                                
000740     SELECT PRINT-FILE ASSIGN TO PRINT1.                                  
000800 DATA DIVISION.                                                           
000810 FILE SECTION.                                                            
000820 FD  INPUT-FILE                                                           
000821     BLOCK CONTAINS 0 RECORDS                                             
000822     RECORD CONTAINS 80 CHARACTERS                                        
000823     RECORDING MODE IS F                                                  
000830     LABEL RECORDS ARE STANDARD.                                          
000840 01  INPUT-RECORD.                                                      10
000850   05  FIELD-1               PIC XX.                                    20
000860   05  FIELD-2               PIC X(14).                                 30
000861   05  FIELD-3               PIC Z(5).                                  30
000870   05  FILLER                PIC X(59).                                 40
000874                                                                          
000875 FD  OUTPUT-FILE                                                          
000876     BLOCK CONTAINS 0 RECORDS                                             
000877     RECORD CONTAINS 80 CHARACTERS                                        
000878     RECORDING MODE IS F                                                  
000879     LABEL RECORDS ARE STANDARD.                                          
000880 01  OUTPUT-RECORD.                                                     10
000881   05  OUT-COUNT             PIC X(10).                                 20
000884   05  FILLER                PIC X(70).                                 40
000888                                                                          
000889 FD  PRINT-FILE                                                           
000890     BLOCK CONTAINS 0 RECORDS                                             
000891     RECORD CONTAINS 133 CHARACTERS                                       
000892     RECORDING MODE IS F                                                  
000893     LABEL RECORDS ARE STANDARD.                                          
000894                                                                          
000895 01  PRINT-LINE.                                                          
000896     05  CARRIAGE-CONTROL-BYTE PIC X.                                   20
000897     05  PRT-COUNT-MSG         PIC X(13).                                 
000898     05  FILLER                PIC X.                                     
000899     05  PRT-COUNTER           PIC X(10).                                 
000900     05  FILLER                PIC X(108).                                
000901                                                                          
000902 01  PRINT-VOLSER.                                                      10
000903     05  FILLER                PIC X.                                   20
000904     05  PRT-DSNAME            PIC X(8).                                20
000905     05  FILLER                PIC X.                                   30
000906     05  PRT-MSG               PIC X(16).                               30
000907     05  FILLER                PIC X.                                   40
000908     05  PRT-VOLSER            PIC X(6).                                  
000909     05  FILLER                PIC X.                                   40
000910     05  PRT-MSG2              PIC X(15).                                 
000911     05  FILLER                PIC X.                                   40
000912     05  PRT-VOLCHG-COUNT      PIC X(10).                                 
000913     05  FILLER                PIC X(73).                                 
000914                                                                          
000920 WORKING-STORAGE SECTION.                                                 
001000 77  FILLER PIC X(36)  VALUE                                              
001100     'CALLGTVM WORKING STORAGE BEGINS HERE'.                              
001300 01  GENERAL-AREAS.                                                       
001302     05  END-OF-INPUT-SWITCH PIC X VALUE 'N'.                             
001303         88  END-OF-INPUT-DATA  VALUE IS 'Y'.                             
001304         88  MORE-DATA-TO-PROCESS VALUE IS 'N'.                           
001305     05  COUNTER             PIC S9(10) COMP-3.                           
001306     05  VOLUME-SERIAL-NO    PIC X(6).                                    
001307     05  INPUT1-VOLSER       PIC X(6).                                    
001308     05  OUTPUT1-VOLSER      PIC X(6).                                    
001309     05  PRINT1-VOLSER       PIC X(6).                                    
001320                                                                          
001400 LINKAGE SECTION.                                                         
001500 01  PARM-FIELD.                                                          
001510     05  PARM-LENGTH         PIC S9(4) COMP.                              
001520     05  PARM-DATA           PIC 9(10).                                   
001600                                                                          
001700 PROCEDURE DIVISION USING PARM-FIELD.                                     
001800                                                                          
001900 A100-EXECUTIVE-CONTROL.                                                  
002000     PERFORM A100-INITIALIZATION.                                         
002100     PERFORM B100-MAINLINE-PROCESSING UNTIL END-OF-INPUT-DATA.            
002200     PERFORM Z100-END-OF-PROCESSING.                                      
002300     GOBACK.                                                              
002400                                                                          
002500 A100-INITIALIZATION.                                                     
002501     OPEN INPUT INPUT-FILE, OUTPUT OUTPUT-FILE, PRINT-FILE.               
002503     MOVE SPACES TO PRINT-LINE.                                           
002504     MOVE SPACES TO OUTPUT-RECORD.                                        
002510     IF PARM-LENGTH = 10                                                  
002520         AND PARM-DATA IS NUMERIC,                                        
002530             MOVE PARM-DATA TO COUNTER                                    
002540     ELSE                                                                 
002550        DISPLAY 'INVALID 10-DIGIT PARM FIELD'                             
002560        CALL 'COBABEND'.                                                  
002570     MOVE COUNTER TO PRT-COUNTER.                                         
002580     MOVE 'PARM COUNT IS' TO PRT-COUNT-MSG.                               
002590     PERFORM Y100-PRINT-A-LINE THRU Y100-EXIT.                            
002611     CALL 'GETVOLUM' USING INPUT-FILE, INPUT1-VOLSER.                     
002630     MOVE 'INPUT1' TO PRT-DSNAME.                                         
002640     MOVE 'VOLUME SERIAL IS' TO PRT-MSG.                                  
002641     MOVE INPUT1-VOLSER TO PRT-VOLSER.                                    
002650     PERFORM Y100-PRINT-A-LINE THRU Y100-EXIT.                            
002660     CALL 'GETVOLUM' USING OUTPUT-FILE, OUTPUT1-VOLSER.                   
002662     MOVE 'OUTPUT1' TO PRT-DSNAME.                                        
002663     MOVE 'VOLUME SERIAL IS' TO PRT-MSG.                                  
002664     MOVE OUTPUT1-VOLSER TO PRT-VOLSER.                                   
002665     PERFORM Y100-PRINT-A-LINE THRU Y100-EXIT.                            
002670     CALL 'GETVOLUM' USING PRINT-FILE, PRINT1-VOLSER.                     
002690     MOVE 'PRINT1' TO PRT-DSNAME.                                         
002691     MOVE 'VOLUME SERIAL IS' TO PRT-MSG.                                  
002692     MOVE PRINT1-VOLSER TO PRT-VOLSER.                                    
002693     PERFORM Y100-PRINT-A-LINE THRU Y100-EXIT.                            
002694     PERFORM Y100-PRINT-A-LINE THRU Y100-EXIT.                            
002700                                                                          
002800 B100-MAINLINE-PROCESSING.                                                
002900     PERFORM C100-READ-INPUT-FILE THRU C100-EXIT.                         
002910     IF MORE-DATA-TO-PROCESS                                              
002920         PERFORM E100-WRITE-OUTPUT-FILE THRU E100-EXIT.                   
003010                                                                          
003020 C100-READ-INPUT-FILE.                                                    
003030     READ INPUT-FILE                                                      
003040         AT END MOVE 'Y' TO END-OF-INPUT-SWITCH.                          
003041     IF MORE-DATA-TO-PROCESS                                              
003042         CALL 'GETVOLUM' USING INPUT-FILE, VOLUME-SERIAL-NO               
003043             IF VOLUME-SERIAL-NO = INPUT1-VOLSER                          
003044                 NEXT SENTENCE                                            
003045             ELSE                                                         
003046                 MOVE VOLUME-SERIAL-NO TO INPUT1-VOLSER                   
003047                 MOVE 'INPUT1' TO PRT-DSNAME                              
003048                 MOVE 'VOLUME SERIAL IS' TO PRT-MSG                       
003049                 MOVE INPUT1-VOLSER TO PRT-VOLSER                         
003050                 MOVE 'RECORD COUNT IS' TO PRT-MSG2                       
003051                 MOVE COUNTER TO PRT-VOLCHG-COUNT                         
003052                 PERFORM Y100-PRINT-A-LINE THRU Y100-EXIT.                
003053 C100-EXIT. EXIT.                                                         
003060                                                                          
003092 E100-WRITE-OUTPUT-FILE.                                                  
003093     SUBTRACT 1 FROM COUNTER.                                             
003094     MOVE COUNTER TO OUT-COUNT.                                           
003095     WRITE OUTPUT-RECORD.                                                 
003096     MOVE SPACES TO OUTPUT-RECORD.                                        
003098     CALL 'GETVOLUM' USING OUTPUT-FILE, VOLUME-SERIAL-NO.                 
003099     IF VOLUME-SERIAL-NO = OUTPUT1-VOLSER                                 
003100         NEXT SENTENCE                                                    
003101     ELSE                                                                 
003102         MOVE VOLUME-SERIAL-NO TO OUTPUT1-VOLSER                          
003103         MOVE 'OUTPUT1' TO PRT-DSNAME                                     
003104         MOVE 'VOLUME SERIAL IS' TO PRT-MSG                               
003105         MOVE OUTPUT1-VOLSER TO PRT-VOLSER                                
003106         MOVE 'RECORD COUNT IS' TO PRT-MSG2                               
003107         MOVE COUNTER TO PRT-VOLCHG-COUNT                                 
003108         PERFORM Y100-PRINT-A-LINE THRU Y100-EXIT                         
003109         PERFORM Y100-PRINT-A-LINE THRU Y100-EXIT.                        
003110 E100-EXIT. EXIT.                                                         
003111                                                                          
003112 Y100-PRINT-A-LINE.                                                       
003113     WRITE PRINT-LINE AFTER ADVANCING 1.                                  
003114     MOVE SPACES TO PRINT-LINE.                                           
003115 Y100-EXIT. EXIT.                                                         
003116                                                                          
003117 Z100-END-OF-PROCESSING.                                                  
003118     PERFORM E100-WRITE-OUTPUT-FILE THRU E100-EXIT                        
003119         UNTIL COUNTER < 1.                                               
003120     PERFORM Y100-PRINT-A-LINE THRU Y100-EXIT.                            
003121     PERFORM Y100-PRINT-A-LINE THRU Y100-EXIT.                            
003122     MOVE COUNTER TO PRT-COUNTER.                                         
003123     MOVE ' END COUNT IS' TO PRT-COUNT-MSG.                               
003124     PERFORM Y100-PRINT-A-LINE THRU Y100-EXIT.                            
003130     CALL 'GETVOLUM' USING INPUT-FILE, INPUT1-VOLSER.                     
003150     MOVE 'INPUT1' TO PRT-DSNAME.                                         
003160     MOVE 'VOLUME SERIAL IS' TO PRT-MSG.                                  
003170     MOVE INPUT1-VOLSER TO PRT-VOLSER.                                    
003180     PERFORM Y100-PRINT-A-LINE THRU Y100-EXIT.                            
003190     CALL 'GETVOLUM' USING OUTPUT-FILE, OUTPUT1-VOLSER.                   
003210     MOVE 'OUTPUT1' TO PRT-DSNAME.                                        
003220     MOVE 'VOLUME SERIAL IS' TO PRT-MSG.                                  
003230     MOVE OUTPUT1-VOLSER TO PRT-VOLSER.                                   
003240     PERFORM Y100-PRINT-A-LINE THRU Y100-EXIT.                            
003250     CALL 'GETVOLUM' USING PRINT-FILE, PRINT1-VOLSER.                     
003270     MOVE 'PRINT1' TO PRT-DSNAME.                                         
003280     MOVE 'VOLUME SERIAL IS' TO PRT-MSG.                                  
003290     MOVE PRINT1-VOLSER TO PRT-VOLSER.                                    
003291     PERFORM Y100-PRINT-A-LINE THRU Y100-EXIT.                            
003300     CLOSE INPUT-FILE,                                                    
003400           OUTPUT-FILE,                                                   
003500           PRINT-FILE.                                                    

