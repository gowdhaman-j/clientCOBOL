000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID.    CNV32BIT.                                                 
000300 AUTHOR. R THORNTON                                                       
000400*REMARKS. CONVERTS A CHARACTER-FORM DECIMAL NUMBER TO A BASE-32           
000500*         VALUE SO THAT IT CAN BE PRINTED, YET FIT IN A SMALLER           
000600*         FIELD.                                                          
000700 ENVIRONMENT DIVISION.                                                    
000800 CONFIGURATION SECTION.                                                   
000900 INPUT-OUTPUT SECTION.                                                    
001000 FILE-CONTROL.                                                            
001100     SELECT INPUT-FILE ASSIGN TO INPUT1.                                  
001200     SELECT PRINT-FILE ASSIGN TO PRINT1.                                  
001300 DATA DIVISION.                                                           
001400 FILE SECTION.                                                            
001500 FD  INPUT-FILE                                                           
001600     BLOCK CONTAINS 0 RECORDS                                             
001700     RECORD CONTAINS 80 CHARACTERS                                        
001800     RECORDING MODE IS F                                                  
001900     LABEL RECORDS ARE STANDARD.                                          
002000 01  INPUT-RECORD.                                                      10
002100   05  NUMBER-LENGTH         PIC 9.                                     20
002200   05  FILLER                PIC X.                                     30
002300   05  DECIMAL-NUMBER-6      PIC 9(6).                                  30
002400   05  FILLER                REDEFINES DECIMAL-NUMBER-6.                  
002500       10  DECIMAL-NUMBER-3  PIC 9(3).                                    
002600       10  FILLER            PIC X(3).                                    
002700   05  FILLER                PIC X(72).                                 40
002800                                                                          
002900 FD  PRINT-FILE                                                           
003000     BLOCK CONTAINS 0 RECORDS                                             
003100     RECORD CONTAINS 133 CHARACTERS                                       
003200     RECORDING MODE IS F                                                  
003300     LABEL RECORDS ARE STANDARD.                                          
003400 01  PRINT-RECORD.                                                      10
003500   05  CARRIAGE-CONTROL-BYTE PIC X.                                     20
003600   05  PRT-LENGTH            PIC 99.                                    20
003700   05  FILLER                PIC X.                                     30
003800   05  PRT-NUMBER-6          PIC X(6).                                  30
003900   05  FILLER                REDEFINES PRT-NUMBER-6.                      
004000       10  PRT-NUMBER-3      PIC X(3).                                    
004100       10  FILLER            PIC X(3).                                    
004200   05  FILLER                PIC X.                                     40
004300   05  PRT-32BIT-4           PIC X(4).                                  40
004400   05  FILLER                REDEFINES PRT-32BIT-4.                       
004500       10 PRT-32BIT-2        PIC XX.                                      
004600       10 FILLER             PIC XX.                                      
004700   05  FILLER                PIC X(118).                                40
004800                                                                          
004900 WORKING-STORAGE SECTION.                                                 
005000 77  FILLER PIC X(36)  VALUE                                              
005100     "CNV32BIT WORKING STORAGE BEGINS HERE".                              
005200 01  GENERAL-AREAS.                                                       
005300     05  END-OF-INPUT-SWITCH PIC X VALUE "N".                             
005400         88  END-OF-INPUT-DATA     VALUE "Y".                             
005500     05  I                   PIC S9(4) COMP VALUE +0.                     
005600     05  J                   PIC S9(4) COMP VALUE +0.                     
005700     05  32BIT-TABLE         PIC X(32) VALUE                              
005800         "0123456789ABCDEFGHIJKLMNOPQRSTUV".                              
005810     05  32BIT-TABLE-DIGIT   REDEFINES 32BIT-TABLE                        
005820                             PIC X OCCURS 32 TIMES.                       
005900     05  WORK-NUMBER         PIC S9(7) COMP-3.                            
006000     05  32BIT-NUMBER-4      PIC X(4).                                    
006100     05  FILLER              REDEFINES 32BIT-NUMBER-4.                    
006200         10 32BIT-DIGIT      PIC X OCCURS 4 TIMES.                        
006300     05  FILLER              REDEFINES 32BIT-NUMBER-4.                    
006400         10 FILLER           PIC XX.                                      
006500         10 32BIT-NUMBER-2   PIC XX.                                      
006600                                                                          
006700 PROCEDURE DIVISION.                                                      
006800                                                                          
006900 A100-EXECUTIVE-CONTROL.                                                  
007000     PERFORM A100-INITIALIZATION.                                         
007100     PERFORM B100-MAINLINE-PROCESSING UNTIL END-OF-INPUT-DATA.            
007200     PERFORM Z100-END-OF-PROCESSING.                                      
007300     GOBACK.                                                              
007400                                                                          
007500 A100-INITIALIZATION.                                                     
007510     MOVE SPACES TO PRINT-RECORD.                                         
007600     OPEN INPUT INPUT-FILE,                                               
007700          OUTPUT PRINT-FILE.                                              
007800     PERFORM C100-READ-INPUT-FILE THRU C100-EXIT.                         
007900                                                                          
008000 B100-MAINLINE-PROCESSING.                                                
008100     IF NUMBER-LENGTH = 6                                                 
008200       MOVE DECIMAL-NUMBER-6 TO WORK-NUMBER                               
008300     ELSE                                                                 
008400       IF NUMBER-LENGTH = 3                                               
008500         MOVE DECIMAL-NUMBER-3 TO WORK-NUMBER                             
008600       ELSE                                                               
008700         DISPLAY "FOLLOWING RECORD INVALID, SKIPPED"                      
008800         DISPLAY INPUT-RECORD                                             
008810         PERFORM C100-READ-INPUT-FILE THRU C100-EXIT                      
008900         GO TO B100-EXIT.                                                 
009000     PERFORM D100-32BIT-CONVERSION VARYING I FROM 4 BY -1                 
009100         UNTIL I = 0.                                                     
009200     MOVE NUMBER-LENGTH TO PRT-LENGTH.                                    
009300     IF NUMBER-LENGTH = 6                                                 
009400         MOVE DECIMAL-NUMBER-6 TO PRT-NUMBER-6                            
009500         MOVE 32BIT-NUMBER-4 TO PRT-32BIT-4                               
009600     ELSE                                                                 
009700         MOVE DECIMAL-NUMBER-3 TO PRT-NUMBER-3                            
009800         MOVE 32BIT-NUMBER-2 TO PRT-32BIT-2.                              
009900     PERFORM Y100-PRINT-A-LINE THRU Y100-EXIT.                            
010100     PERFORM C100-READ-INPUT-FILE THRU C100-EXIT.                         
010110 B100-EXIT. EXIT.                                                         
010200                                                                          
010300 C100-READ-INPUT-FILE.                                                    
010400     READ INPUT-FILE                                                      
010500         AT END MOVE "Y" TO END-OF-INPUT-SWITCH.                          
010600 C100-EXIT. EXIT.                                                         
010700                                                                          
010800 D100-32BIT-CONVERSION.                                                   
010900     DIVIDE WORK-NUMBER BY 32 GIVING WORK-NUMBER                          
011000         REMAINDER J.                                                     
011010     ADD 1 TO J.                                                          
011100     MOVE 32BIT-TABLE-DIGIT (J) TO 32BIT-DIGIT (I).                       
011200 D100-EXIT. EXIT.                                                         
011300                                                                          
011400 Y100-PRINT-A-LINE.                                                       
011500     WRITE PRINT-RECORD AFTER ADVANCING 1 LINE.                           
011510     MOVE SPACES TO PRINT-RECORD.                                         
011600 Y100-EXIT. EXIT.                                                         
011700                                                                          
011800 Z100-END-OF-PROCESSING.                                                  
011900     CLOSE INPUT-FILE,                                                    
012000           PRINT-FILE.                                                    
