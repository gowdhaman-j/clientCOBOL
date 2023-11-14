000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID.    CHKPARMS.                                                 
000300 AUTHOR. R THORNTON                                                       
000400*REMARKS. CHKPARMS IS A DO-NEARLY-NOTHING PROGRAM FOR TESTING PARM        
000410*         OPTIONS SUCH AS RPTSTG(ON) AND RPTOPTS(ON)                      
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
000881   05  OUT-FLD-1             PIC XX.                                    20
000882   05  OUT-FLD-2             PIC X(14).                                 30
000883   05  OUT-FLD-3             PIC Z(5).                                  30
000884   05  FILLER                PIC X(59).                                 40
000888                                                                          
000889 FD  PRINT-FILE                                                           
000890     BLOCK CONTAINS 0 RECORDS                                             
000891     RECORD CONTAINS 133 CHARACTERS                                       
000892     RECORDING MODE IS F                                                  
000893     LABEL RECORDS ARE STANDARD.                                          
000894 01  PRINT-RECORD.                                                      10
000895   05  CARRIAGE-CONTROL-BYTE PIC X.                                     20
000896   05  PRT-FIELD-1           PIC XX.                                    20
000897   05  PRT-FIELD-2           PIC X(9).                                  30
000898   05  PRT-COUNTER           PIC ZZ,ZZZ,ZZ9.                            30
000899   05  FILLER                PIC X(1).                                  40
000900   05  PRT-MESSAGE           PIC X(110).                                40
000901                                                                          
000910 WORKING-STORAGE SECTION.                                                 
001000 77  FILLER PIC X(36)  VALUE                                              
001100     'CHKPARMS WORKING STORAGE BEGINS HERE'.                              
001200 77  SUB                     PIC S9(4) COMP VALUE +0.                     
001300 01  GENERAL-AREAS.                                                       
001301     05  LINE-SPACING        PIC 9 VALUE 1.                               
001302     05  END-OF-INPUT-SWITCH PIC X VALUE 'N'.                             
001303         88  END-OF-INPUT-DATA  VALUE IS 'Y'.                             
001304         88  MORE-DATA-TO-PROCESS VALUE IS 'N'.                           
001305     05  COUNTER             PIC S9(8) COMP-3 VALUE +0.                   
001310                                                                          
001400 LINKAGE SECTION.                                                         
001500 01  PARM-FIELD.                                                          
001510     05  PARM-LENGTH         PIC S9(4) COMP.                              
001520     05  PARM-DATA           PIC X(104).                                  
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
002600     OPEN INPUT INPUT-FILE,                                               
002610          OUTPUT OUTPUT-FILE, PRINT-FILE.                                 
002650     MOVE SPACES TO PRINT-RECORD.                                         
002660     MOVE PARM-LENGTH TO PRT-COUNTER.                                     
002670     MOVE PARM-DATA TO PRT-MESSAGE.                                       
002680     PERFORM Y100-PRINT-A-LINE THRU Y100-EXIT.                            
002700                                                                          
002800 B100-MAINLINE-PROCESSING.                                                
002900     PERFORM C100-READ-INPUT-FILE THRU C100-EXIT.                         
002910     IF MORE-DATA-TO-PROCESS                                              
003000        PERFORM D100-PROCESS-THE-RECORD THRU D100-EXIT.                   
003001     PERFORM E100-WRITE-OUTPUT-FILE THRU E100-EXIT.                       
003010                                                                          
003020 C100-READ-INPUT-FILE.                                                    
003030     READ INPUT-FILE                                                      
003040         AT END MOVE 'Y' TO END-OF-INPUT-SWITCH.                          
003050 C100-EXIT. EXIT.                                                         
003060                                                                          
003070 D100-PROCESS-THE-RECORD.                                                 
003080     ADD 1 TO COUNTER.                                                    
003090 D100-EXIT. EXIT.                                                         
003091                                                                          
003092 E100-WRITE-OUTPUT-FILE.                                                  
003093     WRITE OUTPUT-RECORD FROM INPUT-RECORD.                               
003095 E100-EXIT. EXIT.                                                         
003096                                                                          
003097 Y100-PRINT-A-LINE.                                                       
003098     WRITE PRINT-RECORD AFTER ADVANCING LINE-SPACING.                     
003099 Y100-EXIT. EXIT.                                                         
003100                                                                          
003101 Z100-END-OF-PROCESSING.                                                  
003102     MOVE SPACES TO PRINT-RECORD.                                         
003103     MOVE COUNTER TO PRT-COUNTER.                                         
003104     MOVE 'RECORDS COPIED.' TO PRT-MESSAGE.                               
003120     PERFORM Y100-PRINT-A-LINE THRU Y100-EXIT.                            
003300     CLOSE INPUT-FILE,                                                    
003400           OUTPUT-FILE,                                                   
003500           PRINT-FILE.                                                    

