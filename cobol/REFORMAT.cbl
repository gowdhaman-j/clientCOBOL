000100 ID DIVISION.                                                     09/18/77
000200 PROGRAM-ID. REFORMAT.                                            REFORMAT
000300 DATE-COMPILED.                                                      LV001
000400 AUTHOR. R THORNTON                                                       
000500*********************************************************                 
000600* THIS PROGRAM USES A FIXED LENGTH 80-BYTE FILE OF INPUT*                 
000700* RECORDS TO PRODUCE A FIXED LENGTH OUTPUT FILE WHOSE   *                 
000800* RECORDS MAY BE FROM 81 TO 300 BYTES IN LENGTH, AS     *                 
000900* SPECIFIED IN THE EXEC STATEMENT PARM FIELD. INPUT     *                 
001000* RECORDS ARE READ AND CONCATENATED TO FORM THE OUTPUT  *                 
001100* RECORDS. BECAUSE COBOL REQUIRES LENGTH TO BE SPECIFIED*                 
001200* AT THE FD LEVEL, AN ASSEMBLER SUBROUTINE CALLED       *                 
001300* WRITEKL IS CALLED TO WRITE THE RECORDS USING THE LRECL*                 
001400* SPECIFIED IN THE OUTPUT1 DD STATEMENT.                *                 
001500*                                                       *                 
001600* THE EXEC STATEMENT PARM FIELD CONTAINS A 12-CHARACTER *                 
001700* STRING: BYTES 1-8 ARE THE JOB NAME, WHICH IS PRINTED  *                 
001800* IN THE HEADINGS OF THE OUTPUT2 AND OUTPUT3 REPORT     *                 
001900* FILES. BYTES 9-11 ARE EXPECTED TO BE THE LRECL FOR THE*                 
002000* OUTPUT1 FILE, HOWEVER THIS IS NEVER USED AFTER TESTING*                 
002100* FOR A NUMERIC VALUE, AND THE ACTUAL LRECL FOR THE     *                 
002200* OUTPUT1 FILE IS TAKEN FROM THE OUTPUT1 DD STATEMENT.  *                 
002300* BYTE 12 OF THE PARM SPECIFIES THE NUMBER OF INPUT1    *                 
002400* RECORDS REQUIRED TO BUILD AN OUTPUT1 RECORD. AN       *                 
002500* EXAMPLE OF THE EXEC STATEMENT PARM IS:                *                 
002600*     PARM=DVC8910Y1933                                 *                 
002700* WHICH SHOWS JOBNAME IS DVC8910Y, OUTPUT1 LRECL IS     *                 
002800* EXPECTED TO BE 193 BYTES, AND 3 INPUT1 RECORDS ARE    *                 
002900* REQUIRED TO BUILD 1 OUTPUT1 RECORD.                   *                 
003000*                                                       *                 
003100* DURING THE CONVERSION TO COBOL390, SEVERAL POTENTIAL  *                 
003200* PROBLEMS WERE NOTED, BUT LEFT AS-IS:                  *                 
003300* 1. EXEC STATEMENT PARM FIELD LRECL FIELD IS MISLEADING*                 
003400*    SINCE IT IS NOT USED, AND MAY LEAD THE USER TO     *                 
003500*    BELIEVE THE OUTPUT1 FILE LRECL IS THAT LENGTH, WHEN*                 
003600*    ONLY THE OUTPUT1 DD STATEMENT LRECL CONTROLS THIS. *                 
003700* 2. THE EXEC STATEMENT PARM FIELD NUMBER OF INPUT      *                 
003800*    RECORDS FIELD IS ONE BYTE THAT IS TESTED FOR NUM-  *                 
003900*    ERICS, BUT NOT FOR RANGE. SINCE THE OUTPUT RECORD  *                 
004000*    AREA IS HARD-CODED IN WORKING-STORAGE AS 300 BYTES,*                 
004100*    ANY VALUE OVER 3 WILL RESULT IN OVERLAY OF THE     *                 
004200*    FOLLOWING DATA, WHICH WOULD RESULT IN ABEND OR     *                 
004300*    INCORRECT OUTPUT.                                  *                 
004400* 3. OUTPUT RECORD FORMATTING SEEMS INEFFICIENT, AS IT  *                 
004500*    INVOLVES BYTE-BY-BYTE MOVES IN A PERFORM LOOP,     *                 
004600*    WHEN SIMPLE 80-BYTE MOVES WOULD SUFFICE IF THE     *                 
004700*    RECORD AREAS ARE PROPERLY DEFINED.                 *                 
004800* 4. NO ERROR INDICATION IS RETURNED BY THE WRITEKL SUB-*                 
004900*    ROUTINE, SO THIS PROGRAM HAS NO WAY TO TELL WHETHER*                 
005000*    THE CALL WAS SUCCESSFUL. IN FACT, IF THE REQUEST   *                 
005100*    CODE SENT IS OTHER THAN "C" OR "W", WRITEKL DOES   *                 
005200*    NOTHING, AND RETURNS AS THOUGH IT HAD ACCOMPLISHED *                 
005300*    THE REQUESTED TASK.                                *                 
005400*                                                       *                 
005500*********************************************************                 
005600*              *** MAINTENANCE ***                      *                 
005700*********************************************************                 
005800* ADDED DOCUMENTATION COMMENTS AND UPGRADED TO COBOL390 *                 
005900* IN OCTOBER, 2000. NOTE THAT THIS CONVERSION REQUIRES  *                 
006000* THAT THE EXECUTION-TIME STEPLIB CONTAIN CEE.SCEERUN.  *                 
006100* ALSO, SYS1.COB2LIB MUST BE REMOVED FROM STEPLIB IF IT *                 
006200* IS CURRENTLY USED.                                    *                 
006300*********************************************************                 
006400 ENVIRONMENT DIVISION.                                                    
006500 INPUT-OUTPUT SECTION.                                                    
006600 FILE-CONTROL.                                                            
006700     SELECT KEY-DISC-INPUT               ASSIGN TO UT-S-INPUT1.           
006800     SELECT CONTROL-MSGS                 ASSIGN TO UT-S-OUTPUT2.          
006900     SELECT KEY-DISC-OPERATOR-MSGS       ASSIGN TO UT-S-OUTPUT3.          
007000 DATA DIVISION.                                                           
007100 FILE SECTION.                                                            
007200 FD  KEY-DISC-INPUT                                                       
007300     RECORDING MODE IS F                                                  
007400     LABEL RECORDS ARE STANDARD                                           
007500     RECORD CONTAINS 80 CHARACTERS                                        
007600     BLOCK CONTAINS 0 RECORDS.                                            
007700 01  KEY-DISC-INPUT-RECORD.                                               
007800     05  KEY-DISC-INPUT-REC            PIC   X OCCURS  80  TIMES.         
007900 FD  CONTROL-MSGS                                                         
008000     RECORDING MODE IS F                                                  
008100     LABEL RECORDS ARE STANDARD                                           
008200     RECORD CONTAINS 133 CHARACTERS                                       
008300     BLOCK CONTAINS 0 RECORDS.                                            
008400 01  CONTROL-MSGS-REC.                                                    
008500     05  MSG-CC1                       PIC   9.                           
008600     05  FILLER                        PIC   X(132).                      
008700 FD  KEY-DISC-OPERATOR-MSGS                                               
008800     RECORDING MODE IS F                                                  
008900     LABEL RECORDS ARE STANDARD                                           
009000     RECORD CONTAINS 133 CHARACTERS                                       
009100     BLOCK CONTAINS 0 RECORDS.                                            
009200 01  KEY-DISC-OPERATOR-MSGS-REC.                                          
009300     05  MSG-CC2                       PIC   9.                           
009400     05  FILLER                        PIC   X(132).                      
009500 WORKING-STORAGE SECTION.                                                 
009600 77  FILLER                        PIC  X(36)   VALUE                     
009700         'REFORMAT WORKING STORAGE BEGINS HERE'.                          
009800 77  COMPILE-DATE                 PIC  X(20)   VALUE SPACES.              
009900 01  ACCEPT-DATE.                                                         
010000     05 ACCEPT-YEAR               PIC XX.                                 
010100     05 ACCEPT-MONTH              PIC XX.                                 
010200     05 ACCEPT-DAY                PIC XX.                                 
010300 01  COUNTERS.                                                            
010400     05  PAGE-NO                  PIC  S9(3)   VALUE ZERO COMP-3.         
010500     05  INDX                     PIC  S9(5)   VALUE ZERO COMP.           
010600     05  INDX2                    PIC  S9(5)   VALUE ZERO COMP.           
010700     05  BYTE-MAX                 PIC  S9(5)   VALUE ZERO COMP.           
010800     05  INPUT-REC-COUNT          PIC  S9(5)   VALUE ZERO COMP-3.         
010900     05  OUTPUT-REC-COUNT         PIC  S9(5)   VALUE ZERO COMP-3.         
011000 01  KEY-DISC-OUTPUT-RECORD.                                              
011100     05  KEY-DISC-OUTPUT-REC      PIC  X  OCCURS  300  TIMES.             
011200 01  ERROR-MESSAGES.                                                      
011300     05  ERROR-MSG1               PIC  X(59)   VALUE                      
011400           'INVALID OUTPUT RECORD LENGTH IN PARM FIELD --- JOB CAN        
011500-        'CELED'.                                                         
011600     05  ERROR-MSG2               PIC  X(59)   VALUE                      
011700           'INVALID NUMBER OF INPUT RECS IN PARM FIELD --- JOB CAN        
011800-        'CELED'.                                                         
011900 01  HEADING-1.                                                           
012000     05  FILLER                   PIC  X      VALUE '1'.                  
012100     05  FILLER                   PIC  X(15)  VALUE                       
012200         'CURRENT DATE = '.                                               
012300     05  PRINT-DATE.                                                      
012400         10 PRINT-MONTH           PIC XX.                                 
012500         10 FILLER                PIC X VALUE '/'.                        
012600         10 PRINT-DAY             PIC XX.                                 
012700         10 FILLER                PIC X VALUE '/'.                        
012800         10 PRINT-YEAR            PIC XX.                                 
012900     05  FILLER                   PIC  X(42)  VALUE SPACES.               
013000     05  FILLER                   PIC  X(12)  VALUE                       
013100         'RJE REFORMAT'.                                                  
013200     05  FILLER                   PIC  X(44)  VALUE SPACES.               
013300     05  FILLER                   PIC  X(8)   VALUE                       
013400         'PAGE NO '.                                                      
013500     05  PRINT-PAGE-NO            PIC  ZZ9.                               
013600 01  MESSAGE-LINE.                                                        
013700     05  FILLER                   PIC  X      VALUE '2'.                  
013800     05  FILLER                   PIC  X(9)   VALUE '$$$$$    '.          
013900     05  PRINT-PROGRAM-NAME       PIC  X(8)   VALUE SPACES.               
014000     05  FILLER                   PIC  X(30)  VALUE                       
014100         ' HAS COMPLETED PROCESSING --- '.                                
014200     05  PRINT-INPUT-REC-COUNT    PIC  ZZZZ9.                             
014300     05  FILLER                   PIC  X(24)  VALUE                       
014400         ' INPUT RECORDS READ     '.                                      
014500     05  PRINT-OUTPUT-REC-COUNT   PIC  ZZZZ9.                             
014600     05  FILLER                   PIC  X(24)  VALUE                       
014700         ' OUTPUT RECORDS WRITTEN '.                                      
014800     05  FILLER                   PIC  X(27)  VALUE SPACES.               
014900 01  SWITCHES.                                                            
015000     05  EOF-INPUT                PIC  X      VALUE SPACE.                
015100         88  NO-MORE-INPUT    VALUE '1'.                                  
015200     05  IOOP                     PIC  X      VALUE SPACE.                
015300 01  ERROR-LINE.                                                          
015400     05  FILLER                   PIC  X       VALUE '-'.                 
015500     05  PRINT-ERR-MSG            PIC  X(59)   VALUE SPACES.              
015600     05  FILLER                   PIC  X(73)   VALUE SPACES.              
015700*                                                                         
015800 LINKAGE SECTION.                                                         
015900 01  JCL-PARAMETERS.                                                      
016000     05  FILLER                   PIC  9(2)    COMP.                      
016100     05  JCL-PROGRAM-NAME         PIC  X(8).                              
016200     05  JCL-OUTPUT-REC-LENGTH    PIC  9(3).                              
016300     05  JCL-NO-INPUT-RECS        PIC  9.                                 
016400*                                                                         
016500 PROCEDURE DIVISION USING JCL-PARAMETERS.                                 
016600     MOVE WHEN-COMPILED TO COMPILE-DATE.                                  
016700     OPEN INPUT  KEY-DISC-INPUT                                           
016800          OUTPUT CONTROL-MSGS                                             
016900                 KEY-DISC-OPERATOR-MSGS.                                  
017000     ACCEPT ACCEPT-DATE FROM DATE.                                        
017100     MOVE ACCEPT-YEAR TO PRINT-YEAR.                                      
017200     MOVE ACCEPT-MONTH TO PRINT-MONTH.                                    
017300     MOVE ACCEPT-DAY TO PRINT-DAY.                                        
017400     PERFORM T100-PRINT-HEADINGS.                                         
017500     IF JCL-OUTPUT-REC-LENGTH NUMERIC                                     
017600     THEN NEXT SENTENCE                                                   
017700     ELSE                                                                 
017800         MOVE ERROR-MSG1 TO PRINT-ERR-MSG                                 
017900         PERFORM Z100-ERROR-ROUTINE                                       
018000         STOP RUN.                                                        
018100     IF JCL-NO-INPUT-RECS NUMERIC                                         
018200     THEN NEXT SENTENCE                                                   
018300     ELSE                                                                 
018400         MOVE ERROR-MSG2 TO PRINT-ERR-MSG                                 
018500         PERFORM Z100-ERROR-ROUTINE                                       
018600         STOP RUN.                                                        
018700     PERFORM A100-CREATE-KEY-DISC-RECS UNTIL NO-MORE-INPUT.               
018800     PERFORM S100-CONTROL-MESSAGE-FORMATING.                              
018900     CLOSE KEY-DISC-INPUT                                                 
019000           CONTROL-MSGS                                                   
019100           KEY-DISC-OPERATOR-MSGS.                                        
019200     MOVE 'C' TO IOOP.                                                    
019300     CALL 'WRITEKL' USING IOOP, KEY-DISC-OUTPUT-RECORD.                   
019400     STOP RUN.                                                            
019500*                                                                         
019600 A100-CREATE-KEY-DISC-RECS.                                               
019700     MOVE ZERO TO BYTE-MAX.                                               
019800     PERFORM D100-READ-INPUT JCL-NO-INPUT-RECS TIMES.                     
019900     IF NO-MORE-INPUT                                                     
020000     THEN NEXT SENTENCE                                                   
020100     ELSE                                                                 
020200         PERFORM G100-WRITE-OUTPUT.                                       
020300*                                                                         
020400 D100-READ-INPUT.                                                         
020500     IF NO-MORE-INPUT                                                     
020600     THEN NEXT SENTENCE                                                   
020700     ELSE                                                                 
020800         READ KEY-DISC-INPUT AT END MOVE '1' TO EOF-INPUT.                
020900     IF NO-MORE-INPUT                                                     
021000     THEN NEXT SENTENCE                                                   
021100     ELSE                                                                 
021200         ADD 1 TO INPUT-REC-COUNT                                         
021300         PERFORM F100-CREATE-OUTPUT-REC                                   
021400            VARYING INDX FROM 1 BY 1 UNTIL INDX IS GREATER THAN 80        
021500         ADD 80 TO BYTE-MAX.                                              
021600*                                                                         
021700 F100-CREATE-OUTPUT-REC.                                                  
021800     ADD INDX TO BYTE-MAX GIVING INDX2.                                   
021900     MOVE KEY-DISC-INPUT-REC (INDX) TO                                    
022000        KEY-DISC-OUTPUT-REC (INDX2).                                      
022100*                                                                         
022200 G100-WRITE-OUTPUT.                                                       
022300     MOVE 'W' TO IOOP.                                                    
022400     CALL 'WRITEKL' USING IOOP, KEY-DISC-OUTPUT-RECORD.                   
022500     ADD 1 TO OUTPUT-REC-COUNT.                                           
022600     PERFORM H100-FILL-OUTPUT-WITH-SPACES VARYING INDX FROM               
022700         1 BY 1 UNTIL INDX IS GREATER THAN 300.                           
022800*                                                                         
022900 H100-FILL-OUTPUT-WITH-SPACES.                                            
023000     MOVE SPACE TO KEY-DISC-OUTPUT-REC (INDX).                            
023100*                                                                         
023200 S100-CONTROL-MESSAGE-FORMATING.                                          
023300     MOVE JCL-PROGRAM-NAME TO PRINT-PROGRAM-NAME.                         
023400     MOVE INPUT-REC-COUNT TO PRINT-INPUT-REC-COUNT.                       
023500     MOVE OUTPUT-REC-COUNT TO PRINT-OUTPUT-REC-COUNT.                     
023600     WRITE CONTROL-MSGS-REC FROM MESSAGE-LINE                             
023700         AFTER ADVANCING MSG-CC1.                                         
023800     WRITE KEY-DISC-OPERATOR-MSGS-REC FROM MESSAGE-LINE                   
023900         AFTER ADVANCING MSG-CC2.                                         
024000*                                                                         
024100 T100-PRINT-HEADINGS.                                                     
024200     ADD 1 TO PAGE-NO.                                                    
024300     MOVE PAGE-NO TO PRINT-PAGE-NO.                                       
024400     WRITE CONTROL-MSGS-REC FROM HEADING-1                                
024500         AFTER ADVANCING MSG-CC1.                                         
024600*                                                                         
024700 Z100-ERROR-ROUTINE.                                                      
024800     WRITE CONTROL-MSGS-REC FROM ERROR-LINE                               
024900         AFTER ADVANCING MSG-CC1.                                         
025000     WRITE KEY-DISC-OPERATOR-MSGS-REC FROM ERROR-LINE                     
025100         AFTER ADVANCING MSG-CC2.                                         

