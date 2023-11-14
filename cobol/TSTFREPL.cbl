000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID.    TSTFREPL.                                                 
000300 AUTHOR. R THORNTON                                                       
000400*REMARKS. TEST BUCKET FOR THE FREEPOOL SUBROUTINE.                        
000500 ENVIRONMENT DIVISION.                                                    
000600 CONFIGURATION SECTION.                                                   
000700 INPUT-OUTPUT SECTION.                                                    
000800 FILE-CONTROL.                                                            
000900     SELECT INPUT-FILE ASSIGN TO INPUT1.                                  
001000     SELECT OUTPUT-FILE ASSIGN TO OUTPUT1.                                
001100     SELECT PRINT-FILE ASSIGN TO PRINT1.                                  
001200 DATA DIVISION.                                                           
001300 FILE SECTION.                                                            
001400 FD  INPUT-FILE                                                           
001500     BLOCK CONTAINS 0 RECORDS                                             
001600     RECORD CONTAINS 80 CHARACTERS                                        
001700     RECORDING MODE IS F                                                  
001800     LABEL RECORDS ARE STANDARD.                                          
001900 01  INPUT-RECORD.                                                      10
002000   05  FIELD-1               PIC XX.                                    20
002100   05  FIELD-2               PIC X(14).                                 30
002200   05  FIELD-3               PIC Z(5).                                  30
002300   05  FILLER                PIC X(59).                                 40
002400 01  REC-DEFN-2.                                                          
002500   05  DEF-2-FLD1            PIC X(55).                                   
002600   05  FILLER                PIC X(25).                                   
002700                                                                          
002800 FD  OUTPUT-FILE                                                          
002900     BLOCK CONTAINS 0 RECORDS                                             
003000     RECORD CONTAINS 80 CHARACTERS                                        
003100     RECORDING MODE IS F                                                  
003200     LABEL RECORDS ARE STANDARD.                                          
003300 01  OUTPUT-RECORD.                                                     10
003400   05  OUT-FLD-1             PIC XX.                                    20
003500   05  OUT-FLD-2             PIC X(14).                                 30
003600   05  OUT-FLD-3             PIC Z(5).                                  30
003700   05  FILLER                PIC X(59).                                 40
003800                                                                          
003900 FD  PRINT-FILE                                                           
004000     BLOCK CONTAINS 0 RECORDS                                             
004100     RECORD CONTAINS 133 CHARACTERS                                       
004200     RECORDING MODE IS F                                                  
004300     LABEL RECORDS ARE STANDARD.                                          
004400 01  PRINT-RECORD.                                                      10
004500   05  CARRIAGE-CONTROL-BYTE PIC X.                                     20
004600   05  PRT-FIELD-1           PIC XX.                                    20
004700   05  PRT-FIELD-2           PIC X(9).                                  30
004800   05  PRT-COUNTER           PIC ZZ,ZZZ,ZZ9.                            30
004900   05  FILLER                PIC X(111).                                40
005000                                                                          
005100 WORKING-STORAGE SECTION.                                                 
005200 77  FILLER PIC X(36)  VALUE                                              
005300     'TSTFREPL WORKING STORAGE BEGINS HERE'.                              
005400 77  SUB                     PIC S9(4) COMP VALUE +0.                     
005500 01  GENERAL-AREAS.                                                       
005600     05  FREEPOOL-CODE       PIC X VALUE 'F'.                             
005700     05  LOCATE-CODE         PIC X VALUE 'L'.                             
005800     05  INPUT-DDNAME        PIC X(8) VALUE 'INPUT1  '.                   
005900     05  LINE-SPACING        PIC 9 VALUE 1.                               
006000     05  END-OF-INPUT-SWITCH PIC X VALUE 'N'.                             
006100         88  END-OF-INPUT-DATA  VALUE IS 'Y'.                             
006200         88  MORE-DATA-TO-PROCESS VALUE IS 'N'.                           
006300     05  COUNTER             PIC S9(8) COMP-3 VALUE +0.                   
006400                                                                          
006500 LINKAGE SECTION.                                                         
006600 01  PARM-FIELD.                                                          
006700     05  PARM-LENGTH         PIC S9(4) COMP.                              
006800     05  PARM-DATA           PIC X(104).                                  
006900                                                                          
007000 PROCEDURE DIVISION USING PARM-FIELD.                                     
007100                                                                          
007200 A100-EXECUTIVE-CONTROL.                                                  
007300     PERFORM A100-INITIALIZATION.                                         
007400     PERFORM B100-MAINLINE-PROCESSING UNTIL END-OF-INPUT-DATA.            
007500     PERFORM Z100-END-OF-PROCESSING.                                      
007700     PERFORM B100-MAINLINE-PROCESSING UNTIL END-OF-INPUT-DATA.            
007800     PERFORM Z100-END-OF-PROCESSING.                                      
007900     GOBACK.                                                              
008000                                                                          
008100 A100-INITIALIZATION.                                                     
008200     OPEN INPUT INPUT-FILE,                                               
008300          OUTPUT OUTPUT-FILE, PRINT-FILE.                                 
008400     CALL 'FREEPOOL' USING LOCATE-CODE, INPUT-DDNAME.                     
008500                                                                          
008600 B100-MAINLINE-PROCESSING.                                                
008700     PERFORM C100-READ-INPUT-FILE THRU C100-EXIT.                         
008800     IF MORE-DATA-TO-PROCESS                                              
008900        PERFORM D100-PROCESS-THE-RECORD THRU D100-EXIT.                   
009000     PERFORM E100-WRITE-OUTPUT-FILE THRU E100-EXIT.                       
009100                                                                          
009200 C100-READ-INPUT-FILE.                                                    
009300     READ INPUT-FILE                                                      
009400         AT END MOVE 'Y' TO END-OF-INPUT-SWITCH.                          
009500 C100-EXIT. EXIT.                                                         
009600                                                                          
009700 D100-PROCESS-THE-RECORD.                                                 
009800     ADD 1 TO COUNTER.                                                    
009900 D100-EXIT. EXIT.                                                         
010000                                                                          
010100 E100-WRITE-OUTPUT-FILE.                                                  
010200     WRITE OUTPUT-RECORD.                                                 
010300 E100-EXIT. EXIT.                                                         
010400                                                                          
010500 Y100-PRINT-A-LINE.                                                       
010600     WRITE PRINT-RECORD AFTER ADVANCING LINE-SPACING.                     
010700 Y100-EXIT. EXIT.                                                         
010800                                                                          
010900 Z100-END-OF-PROCESSING.                                                  
011000     MOVE COUNTER TO PRT-COUNTER.                                         
011100     PERFORM Y100-PRINT-A-LINE THRU Y100-EXIT.                            
011200     ADD 1 TO SUB.                                                        
011300     IF SUB = 1                                                           
011400         CLOSE INPUT-FILE                                                 
011500         CALL 'FREEPOOL' USING FREEPOOL-CODE                              
011600         OPEN INPUT INPUT-FILE                                            
011700     ELSE                                                                 
011800         CLOSE INPUT-FILE                                                 
011900               OUTPUT-FILE                                                
012000               PRINT-FILE.                                                

