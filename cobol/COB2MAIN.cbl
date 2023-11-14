000020 CBL APOST                                                                
000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID. COB2MAIN.                                                    
000300 AUTHOR. R THORNTON.                                                      
000400 DATE-WRITTEN.  OCT 1992.                                                 
000500 DATE-COMPILED.                                                           
000510*****************************************************************         
000600*REMARKS. USED AS A COBOL II MAINLINE PROGRAM IN TESTING MIXED  *         
000610*         RUN UNITS CONTAINING BOTH VS COBOL AND COBOL II       *         
000620*         COMPONENTS. TWO SUBROUTINES ARE CALLED: SUBR2 AND     *         
000621*         SUBR1 (COBOL II AND VS COBOL, RESPECTIVELY).          *         
000630*****************************************************************         
000700 ENVIRONMENT DIVISION.                                                    
000800 INPUT-OUTPUT SECTION.                                                    
000810*                                                                         
000900 FILE-CONTROL.                                                            
001000     SELECT INFILE   ASSIGN TO UT-S-INPUT1.                               
001100     SELECT PRINT    ASSIGN TO UT-S-PRINT1.                               
001110*                                                                         
001200 DATA DIVISION.                                                           
001300 FILE SECTION.                                                            
001310*                                                                         
001400 FD  INFILE                                                               
001500     LABEL RECORDS ARE STANDARD                                           
001600     RECORDING MODE IS F                                                  
001700     RECORD CONTAINS 80 CHARACTERS                                        
001800     BLOCK CONTAINS 0 CHARACTERS.                                         
001900 01  INFILE-RECORD.                                                       
001910     05  IN-KEY                     PIC X(8).                             
001920     05  IN-DATA                    PIC X(72).                            
001930*                                                                         
002100 FD  PRINT                                                                
002200     LABEL RECORDS ARE OMITTED                                            
002300     RECORD CONTAINS 133 CHARACTERS                                       
002400     RECORDING MODE IS F                                                  
002500     BLOCK CONTAINS 0 CHARACTERS                                          
002600     DATA RECORD IS PRINT-REC.                                            
002610*                                                                         
002700 01  PRINT-REC.                                                           
002800     05  CARR-CTL                    PIC X(1).                            
002900     05  PRINT-RECORD.                                                    
002910         10  SUBR-ID                 PIC X(4).                            
002911         10  FILLER                  PIC X.                               
002912         10  PRINT-KEY               PIC X(8).                            
002920         10  PRINT-DATA              PIC X(72).                           
002930         10  FILLER                  PIC X(47).                           
002940*                                                                         
003100 WORKING-STORAGE SECTION.                                                 
003200 77  FILLER          PIC X(44)                                            
003300       VALUE 'COB2MAIN WORKING STORAGE SECTION STARTS HERE'.              
003310*                                                                         
003500 01  SWITCHES.                                                            
003600     05  END-OF-INPUT-SW              PIC X  VALUE 'N'.                   
003700         88  MORE-INFILE-RECORDS      VALUE 'N'.                          
003800         88  END-OF-INPUT             VALUE 'Y'.                          
003900*                                                                         
003901 01  SUBROUTINE-PARAMETER-AREAS.                                          
003910     05  REQUEST-CODE                     PIC X.                          
003920         88  OPEN-REQUEST                 VALUE 'O'.                      
003930         88  READ-REQUEST                 VALUE 'R'.                      
003940         88  CLOSE-REQUEST                VALUE 'C'.                      
003960     05  RECORD-RETURN-AREA.                                              
003970         10  RETURN-KEY                   PIC X(8).                       
003971         10  RETURN-DATA.                                                 
003972             15  RETURN-FIRST-FIELD       PIC X(7).                       
003973             15  RETURN-FIRST-FIELD9                                      
003974                 REDEFINES RETURN-FIRST-FIELD PIC S9(7).                  
003975             15  RETURN-REST-OF-DATA      PIC X(65).                      
003980     05  RECORD-KEY-ARGUMENT              PIC X(8).                       
003991     05  COMPLETION-CODE                  PIC X VALUE 'Y'.                
003992         88  SUCCESSFUL-OPERATION         VALUE 'Y'.                      
003993         88  RECORD-NOT-FOUND             VALUE 'N'.                      
004003*                                                                         
004010 01  MISCELLANEOUS-FIELDS.                                                
004100     05  LINE-NUMBER           PIC S999 COMP-3 VALUE +0.                  
004200     05  PAGE-NUMBER           PIC S999 COMP-3 VALUE +0.                  
004201     05  ANSWER1               PIC ZZ,ZZ9.999- BLANK ZERO.                
004202     05  ANSWER2               PIC ZZ,ZZ9.999- BLANK ZERO.                
004203     05  BINARY-NUM            PIC S9(4) COMP VALUE +403.                 
004204     05  PACKED-NUM            PIC S99V9 COMP-3 VALUE -72.6.              
004205     05  ZONED-NUM             PIC S9(5)V99 VALUE +105.92.                
004206     05  ZONED-BINRCV          PIC S9(3)V9 VALUE +0.                      
004207     05  PACKED-BINRCV         PIC S9(5)V9(4) COMP-3 VALUE +0.            
004208     05  ZONED-PKRCV           PIC S9V999 VALUE +0.                       
004209     05  BINARY-PKRCV          PIC S9(7) COMP VALUE +0.                   
004210     05  PACKED-ZONRCV         PIC S9999V9 COMP-3 VALUE +0.               
004211     05  BINARY-ZONRCV         PIC S9(7)V9(7) COMP VALUE +0.              
004220*                                                                         
004400 01  COUNTERS.                                                            
004500     05  TOTAL-READ                   PIC S9(9) COMP-3 VALUE +0.          
004600     05  TOTAL-CNT                    PIC S9(9) COMP-3 VALUE +0.          
004610*                                                                         
004800 01  HEAD-LINE-1.                                                         
004900     05  FILLER                       PIC X(25)                           
005000                                 VALUE '1PROGRAM NAME: COB2MAIN'.         
005100     05  FILLER                       PIC X(10)                           
005200                                      VALUE 'RUN DATE: '.                 
005310     05  HEADER-RUN-DATE              PIC 99/99/99.                       
005320*    05  HEADER-RUN-DATE              PIC X(8) VALUE SPACE.               
005400     05  FILLER                       PIC X(8) VALUE SPACE.               
005500     05  FILLER                       PIC X(30)                           
005600                          VALUE '    REPORT HEADING LINE 1     '.         
005700     05  FILLER                       PIC X(43) VALUE SPACE.              
005800     05  FILLER                       PIC X(6)                            
005900                                      VALUE 'PAGE: '.                     
006000     05  HEADER-PAGE-NUMBER           PIC ZZ9.                            
006010*                                                                         
006300 01  DETAIL-REC.                                                          
006400     05  DETAIL-CC                    PIC X VALUE SPACE.                  
006500     05  FILLER                       PIC X(17)                           
006600                                      VALUE '                 '.          
006700     05  DETAIL-TOTAL-CNT             PIC ZZZ,ZZZ,ZZ9.                    
006800     05  FILLER                       PIC X(104) VALUE SPACE.             
006810*                                                                         
007000 01  TOTAL-LINE-1.                                                        
007100     05  FILLER                       PIC X(13) VALUE '-'.                
007200     05  FILLER                       PIC X(21)                           
007300                                     VALUE 'TOTAL RECORDS READ:'.         
007400     05  FILLER                       PIC X(7) VALUE SPACES.              
007500     05  TOTAL-READ-I                 PIC ZZZ,ZZZ,Z99.                    
007510*                                                                         
007800 PROCEDURE DIVISION.                                                      
007810*                                                                         
008000 A000-MAINLINE.                                                           
008100     PERFORM B010-INITIALIZATION THRU B010-EXIT.                          
008200     PERFORM C010-PROCESS THRU C010-EXIT UNTIL END-OF-INPUT.              
008300     PERFORM Z030-END-OF-JOB THRU Z030-EXIT.                              
008400     GOBACK.                                                              
008410*                                                                         
008600 B010-INITIALIZATION.                                                     
008610     MOVE 'O' TO REQUEST-CODE.                                            
008620     CALL 'COBVSUBR' USING REQUEST-CODE, COMPLETION-CODE.                 
008621     CALL 'COB2SUBR' USING REQUEST-CODE, COMPLETION-CODE.                 
008660     MOVE 'R' TO REQUEST-CODE.                                            
008800     OPEN INPUT  INFILE, OUTPUT PRINT.                                    
008900     ACCEPT HEADER-RUN-DATE FROM DATE.                                    
008910*    MOVE CURRENT-DATE TO HEADER-RUN-DATE.                                
009000     PERFORM Q010-PRINT-HEADING THRU Q010-EXIT.                           
009100 B010-EXIT.                                                               
009200     EXIT.                                                                
009210*                                                                         
009400 C010-PROCESS.                                                            
009500     PERFORM E010-READ-INFILE THRU E010-EXIT.                             
009600     IF END-OF-INPUT GO TO C010-EXIT.                                     
009700     MOVE SPACES TO PRINT-RECORD.                                         
009701     MOVE IN-KEY TO PRINT-KEY.                                            
009800     MOVE IN-KEY TO RECORD-KEY-ARGUMENT.                                  
009803     CALL 'COBVSUBR' USING REQUEST-CODE,                                  
009804                           COMPLETION-CODE,                               
009805                           RECORD-RETURN-AREA,                            
009806                           RECORD-KEY-ARGUMENT.                           
009807     IF SUCCESSFUL-OPERATION                                              
009808         MOVE 'COBV' TO SUBR-ID                                           
009809         MOVE RECORD-RETURN-AREA TO PRINT-DATA                            
009810     ELSE                                                                 
009820         MOVE 'V-RECORD FOR THIS KEY NOT FOUND' TO PRINT-DATA.            
009842     PERFORM M010-PRINT-LINE THRU M010-EXIT.                              
009843     IF RETURN-FIRST-FIELD IS NUMERIC                                     
009844         COMPUTE ANSWER1 = (RETURN-FIRST-FIELD9 - BINARY-NUM) *           
009845            (PACKED-NUM / ZONED-NUM).                                     
009846     CALL 'COB2SUBR' USING REQUEST-CODE,                                  
009847                           COMPLETION-CODE,                               
009848                           RECORD-RETURN-AREA,                            
009849                           RECORD-KEY-ARGUMENT.                           
009850     IF SUCCESSFUL-OPERATION                                              
009851         MOVE 'COB2' TO SUBR-ID                                           
009852         MOVE RECORD-RETURN-AREA TO PRINT-DATA                            
009853     ELSE                                                                 
009854         MOVE '2-RECORD FOR THIS KEY NOT FOUND' TO PRINT-DATA.            
009860     PERFORM M010-PRINT-LINE THRU M010-EXIT.                              
009861     IF RETURN-FIRST-FIELD IS NUMERIC                                     
009870         COMPUTE ANSWER2 = (RETURN-FIRST-FIELD9 + BINARY-NUM) *           
009880            (PACKED-NUM / ZONED-NUM)                                      
009890     ELSE                                                                 
009891         MOVE BINARY-NUM TO ZONED-BINRCV                                  
009892         MOVE BINARY-NUM TO PACKED-BINRCV                                 
009893         MOVE PACKED-NUM TO ZONED-PKRCV                                   
009894         MOVE PACKED-NUM TO BINARY-PKRCV                                  
009895         MOVE ZONED-NUM TO PACKED-ZONRCV                                  
009896         MOVE ZONED-NUM TO BINARY-ZONRCV.                                 
009900 C010-EXIT.                                                               
010000     EXIT.                                                                
010710*                                                                         
010900 E010-READ-INFILE.                                                        
011000     READ INFILE                                                          
011100         AT END MOVE 'Y' TO END-OF-INPUT-SW.                              
011200     IF MORE-INFILE-RECORDS                                               
011300         ADD 1 TO TOTAL-READ.                                             
011400 E010-EXIT.                                                               
011500     EXIT.                                                                
011510*                                                                         
011600 G010-WRITE-COUNTS.                                                       
011700     PERFORM K010-PRINT-DETAIL THRU K010-EXIT.                            
011800     MOVE TOTAL-READ TO TOTAL-READ-I.                                     
011900     MOVE TOTAL-LINE-1 TO PRINT-REC.                                      
012000     PERFORM M010-PRINT-LINE THRU M010-EXIT.                              
012100 G010-EXIT.                                                               
012200     EXIT.                                                                
012210*                                                                         
012500 K010-PRINT-DETAIL.                                                       
012600     MOVE TOTAL-CNT TO DETAIL-TOTAL-CNT.                                  
012700     MOVE DETAIL-REC TO PRINT-REC.                                        
012800     PERFORM M010-PRINT-LINE THRU M010-EXIT.                              
012900 K010-EXIT.                                                               
013000     EXIT.                                                                
013010*                                                                         
013300 M010-PRINT-LINE.                                                         
013400     WRITE PRINT-REC.                                                     
013600     ADD 1 TO LINE-NUMBER.                                                
013700     MOVE SPACES TO PRINT-REC.                                            
013800     IF LINE-NUMBER > 56                                                  
013900         PERFORM Q010-PRINT-HEADING THRU Q010-EXIT.                       
014000 M010-EXIT.                                                               
014100     EXIT.                                                                
014110*                                                                         
014300 Q010-PRINT-HEADING.                                                      
014400     ADD 1 TO PAGE-NUMBER.                                                
014500     MOVE PAGE-NUMBER TO HEADER-PAGE-NUMBER.                              
014600     MOVE HEAD-LINE-1 TO PRINT-REC.                                       
014700     WRITE PRINT-REC.                                                     
014900     MOVE 2 TO LINE-NUMBER.                                               
014910     MOVE SPACES TO PRINT-REC.                                            
015000     MOVE '0' TO CARR-CTL.                                                
015100 Q010-EXIT.                                                               
015200     EXIT.                                                                
015300*                                                                         
015400 Z030-END-OF-JOB.                                                         
015500     PERFORM G010-WRITE-COUNTS THRU G010-EXIT.                            
015600     CLOSE INFILE, PRINT.                                                 
015800     MOVE 'C' TO REQUEST-CODE.                                            
016000     CALL 'COBVSUBR' USING REQUEST-CODE, COMPLETION-CODE.                 
016010     CALL 'COB2SUBR' USING REQUEST-CODE, COMPLETION-CODE.                 
016020     DISPLAY '************************************************'.          
016030     DISPLAY '*     GOOD COMPLETION. ANSWER1 IS: ' ANSWER1 ' *'.          
016040     DISPLAY '*     GOOD COMPLETION. ANSWER2 IS: ' ANSWER2 ' *'.          
016050     DISPLAY '************************************************'.          
016300 Z030-EXIT.                                                               
016400     EXIT.                                                                
  