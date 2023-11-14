000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID.    FILLBLOW.                                                 
000300 AUTHOR. R THORNTON                                                       
000400*REMARKS. UTILITY PROGRAM TO USE UP ALL THE MEMORY BELOW THE LINE         
000410*         AS AN AID IN TESTING NEW VERSIONS OF COBOL.                     
000500 ENVIRONMENT DIVISION.                                                    
000600 CONFIGURATION SECTION.                                                   
000700 INPUT-OUTPUT SECTION.                                                    
000710 FILE-CONTROL.                                                            
000720     SELECT INPUT-FIL1 ASSIGN TO INPUT1.                                  
000721     SELECT INPUT-FIL2 ASSIGN TO INPUT2.                                  
000722     SELECT INPUT-FIL3 ASSIGN TO INPUT3.                                  
000723     SELECT INPUT-FIL4 ASSIGN TO INPUT4.                                  
000724     SELECT INPUT-FIL5 ASSIGN TO INPUT5.                                  
000725     SELECT INPUT-FIL6 ASSIGN TO INPUT6.                                  
000726     SELECT INPUT-FIL7 ASSIGN TO INPUT7.                                  
000727     SELECT INPUT-FIL8 ASSIGN TO INPUT8.                                  
000728     SELECT INPUT-FIL9 ASSIGN TO INPUT9.                                  
000730     SELECT PRINT-FIL1 ASSIGN TO PRINT1.                                  
000740     SELECT PRINT-FIL2 ASSIGN TO PRINT2.                                  
000750     SELECT PRINT-FIL3 ASSIGN TO PRINT3.                                  
000760     SELECT PRINT-FIL4 ASSIGN TO PRINT4.                                  
000770     SELECT PRINT-FIL5 ASSIGN TO PRINT5.                                  
000780     SELECT PRINT-FIL6 ASSIGN TO PRINT6.                                  
000790     SELECT PRINT-FIL7 ASSIGN TO PRINT7.                                  
000791     SELECT PRINT-FIL8 ASSIGN TO PRINT8.                                  
000792     SELECT PRINT-FIL9 ASSIGN TO PRINT9.                                  
000793     SELECT OUTPUT-FIL1 ASSIGN TO OUTPUT1.                                
000794     SELECT OUTPUT-FIL2 ASSIGN TO OUTPUT2.                                
000795     SELECT OUTPUT-FIL3 ASSIGN TO OUTPUT3.                                
000796     SELECT OUTPUT-FIL4 ASSIGN TO OUTPUT4.                                
000797     SELECT OUTPUT-FIL5 ASSIGN TO OUTPUT5.                                
000798     SELECT OUTPUT-FIL6 ASSIGN TO OUTPUT6.                                
000799     SELECT OUTPUT-FIL7 ASSIGN TO OUTPUT7.                                
000800     SELECT OUTPUT-FIL8 ASSIGN TO OUTPUT8.                                
000801     SELECT OUTPUT-FIL9 ASSIGN TO OUTPUT9.                                
000802 DATA DIVISION.                                                           
000810 FILE SECTION.                                                            
000820 FD  INPUT-FIL1                                                           
000821     RECORDING MODE IS F                                                  
000822     BLOCK CONTAINS 0 RECORDS                                             
000830     LABEL RECORDS ARE STANDARD.                                          
000840 01  INPUT-RECORD1           PIC X(80).                                 10
000841 FD  INPUT-FIL2                                                           
000842     RECORDING MODE IS F                                                  
000843     BLOCK CONTAINS 0 RECORDS                                             
000844     LABEL RECORDS ARE STANDARD.                                          
000845 01  INPUT-RECORD2           PIC X(80).                                 10
000846 FD  INPUT-FIL3                                                           
000847     RECORDING MODE IS F                                                  
000848     BLOCK CONTAINS 0 RECORDS                                             
000849     LABEL RECORDS ARE STANDARD.                                          
000850 01  INPUT-RECORD3           PIC X(80).                                 10
000851 FD  INPUT-FIL4                                                           
000852     RECORDING MODE IS F                                                  
000853     BLOCK CONTAINS 0 RECORDS                                             
000854     LABEL RECORDS ARE STANDARD.                                          
000855 01  INPUT-RECORD4           PIC X(80).                                 10
000856 FD  INPUT-FIL5                                                           
000857     RECORDING MODE IS F                                                  
000858     BLOCK CONTAINS 0 RECORDS                                             
000859     LABEL RECORDS ARE STANDARD.                                          
000860 01  INPUT-RECORD5           PIC X(80).                                 10
000861 FD  INPUT-FIL6                                                           
000862     RECORDING MODE IS F                                                  
000863     BLOCK CONTAINS 0 RECORDS                                             
000864     LABEL RECORDS ARE STANDARD.                                          
000865 01  INPUT-RECORD6           PIC X(80).                                 10
000866 FD  INPUT-FIL7                                                           
000867     RECORDING MODE IS F                                                  
000868     BLOCK CONTAINS 0 RECORDS                                             
000869     LABEL RECORDS ARE STANDARD.                                          
000870 01  INPUT-RECORD7           PIC X(80).                                 10
000871 FD  INPUT-FIL8                                                           
000872     RECORDING MODE IS F                                                  
000873     BLOCK CONTAINS 0 RECORDS                                             
000874     LABEL RECORDS ARE STANDARD.                                          
000875 01  INPUT-RECORD8           PIC X(80).                                 10
000876 FD  INPUT-FIL9                                                           
000877     RECORDING MODE IS F                                                  
000878     BLOCK CONTAINS 0 RECORDS                                             
000879     LABEL RECORDS ARE STANDARD.                                          
000880 01  INPUT-RECORD9           PIC X(80).                                 10
000881 FD  PRINT-FIL1                                                           
000882     RECORDING MODE IS F                                                  
000883     BLOCK CONTAINS 0 RECORDS                                             
000890     LABEL RECORDS ARE STANDARD.                                          
000891 01  PRINT-RECORD1           PIC X(133).                                10
000892 FD  PRINT-FIL2                                                           
000893     RECORDING MODE IS F                                                  
000894     BLOCK CONTAINS 0 RECORDS                                             
000895     LABEL RECORDS ARE STANDARD.                                          
000896 01  PRINT-RECORD2           PIC X(133).                                10
000897 FD  PRINT-FIL3                                                           
000898     RECORDING MODE IS F                                                  
000899     BLOCK CONTAINS 0 RECORDS                                             
000900     LABEL RECORDS ARE STANDARD.                                          
000901 01  PRINT-RECORD3           PIC X(133).                                10
000902 FD  PRINT-FIL4                                                           
000903     RECORDING MODE IS F                                                  
000904     BLOCK CONTAINS 0 RECORDS                                             
000905     LABEL RECORDS ARE STANDARD.                                          
000906 01  PRINT-RECORD4           PIC X(133).                                10
000907 FD  PRINT-FIL5                                                           
000908     RECORDING MODE IS F                                                  
000909     BLOCK CONTAINS 0 RECORDS                                             
000910     LABEL RECORDS ARE STANDARD.                                          
000911 01  PRINT-RECORD5           PIC X(133).                                10
000912 FD  PRINT-FIL6                                                           
000913     RECORDING MODE IS F                                                  
000914     BLOCK CONTAINS 0 RECORDS                                             
000915     LABEL RECORDS ARE STANDARD.                                          
000916 01  PRINT-RECORD6           PIC X(133).                                10
000917 FD  PRINT-FIL7                                                           
000918     RECORDING MODE IS F                                                  
000919     BLOCK CONTAINS 0 RECORDS                                             
000920     LABEL RECORDS ARE STANDARD.                                          
000921 01  PRINT-RECORD7           PIC X(133).                                10
000922 FD  PRINT-FIL8                                                           
000923     RECORDING MODE IS F                                                  
000924     BLOCK CONTAINS 0 RECORDS                                             
000925     LABEL RECORDS ARE STANDARD.                                          
000926 01  PRINT-RECORD8           PIC X(133).                                10
000927 FD  PRINT-FIL9                                                           
000928     RECORDING MODE IS F                                                  
000929     BLOCK CONTAINS 0 RECORDS                                             
000930     LABEL RECORDS ARE STANDARD.                                          
000931 01  PRINT-RECORD9           PIC X(133).                                10
000932 FD  OUTPUT-FIL1                                                          
000933     RECORDING MODE IS F                                                  
000934     BLOCK CONTAINS 0 RECORDS                                             
000935     LABEL RECORDS ARE STANDARD.                                          
000936 01  OUTPUT-RECORD1           PIC X(80).                                10
000937 FD  OUTPUT-FIL2                                                          
000938     RECORDING MODE IS F                                                  
000939     BLOCK CONTAINS 0 RECORDS                                             
000940     LABEL RECORDS ARE STANDARD.                                          
000941 01  OUTPUT-RECORD2           PIC X(80).                                10
000942 FD  OUTPUT-FIL3                                                          
000943     RECORDING MODE IS F                                                  
000944     BLOCK CONTAINS 0 RECORDS                                             
000945     LABEL RECORDS ARE STANDARD.                                          
000946 01  OUTPUT-RECORD3           PIC X(80).                                10
000947 FD  OUTPUT-FIL4                                                          
000948     RECORDING MODE IS F                                                  
000949     BLOCK CONTAINS 0 RECORDS                                             
000950     LABEL RECORDS ARE STANDARD.                                          
000951 01  OUTPUT-RECORD4           PIC X(80).                                10
000952 FD  OUTPUT-FIL5                                                          
000953     RECORDING MODE IS F                                                  
000954     BLOCK CONTAINS 0 RECORDS                                             
000955     LABEL RECORDS ARE STANDARD.                                          
000956 01  OUTPUT-RECORD5           PIC X(80).                                10
000957 FD  OUTPUT-FIL6                                                          
000958     RECORDING MODE IS F                                                  
000959     BLOCK CONTAINS 0 RECORDS                                             
000960     LABEL RECORDS ARE STANDARD.                                          
000961 01  OUTPUT-RECORD6           PIC X(80).                                10
000962 FD  OUTPUT-FIL7                                                          
000963     RECORDING MODE IS F                                                  
000964     BLOCK CONTAINS 0 RECORDS                                             
000965     LABEL RECORDS ARE STANDARD.                                          
000966 01  OUTPUT-RECORD7           PIC X(80).                                10
000967 FD  OUTPUT-FIL8                                                          
000968     RECORDING MODE IS F                                                  
000969     BLOCK CONTAINS 0 RECORDS                                             
000970     LABEL RECORDS ARE STANDARD.                                          
000971 01  OUTPUT-RECORD8           PIC X(80).                                10
000972 FD  OUTPUT-FIL9                                                          
000973     RECORDING MODE IS F                                                  
000974     BLOCK CONTAINS 0 RECORDS                                             
000975     LABEL RECORDS ARE STANDARD.                                          
000976 01  OUTPUT-RECORD9           PIC X(80).                                10
000977                                                                          
000980 WORKING-STORAGE SECTION.                                                 
001000 77  FILLER PIC X(36)  VALUE                                              
001100     'FILLBLOW WORKING STORAGE BEGINS HERE'.                              
001200 77  GETMAIN-CALLS         PIC S9(9) COMP-3 VALUE +0.                     
001600                                                                          
001601 01  NUMBER-CALLS.                                                        
001602     05  FILLER            PIC X(19) VALUE ' CALLED GETMAIN SUB'.         
001603     05  FILLER            PIC X(18) VALUE 'ROUTINE TO GET 9K '.          
001604     05  PRINT-CALLS       PIC ZZZ,ZZZ,ZZZ.                               
001605     05  FILLER            PIC X(32) VALUE ' TIMES'.                      
001607                                                                          
001608 01  SWITCHES-AREA.                                                       
001609     05  FILE1-EOF-SWITCH          PIC X VALUE 'N'.                       
001610         88  FILE1-EOF             VALUE 'Y'.                             
001611     05  FILE2-EOF-SWITCH          PIC X VALUE 'N'.                       
001612         88  FILE2-EOF             VALUE 'Y'.                             
001613     05  FILE3-EOF-SWITCH          PIC X VALUE 'N'.                       
001614         88  FILE3-EOF             VALUE 'Y'.                             
001615     05  FILE4-EOF-SWITCH          PIC X VALUE 'N'.                       
001616         88  FILE4-EOF             VALUE 'Y'.                             
001617     05  FILE5-EOF-SWITCH          PIC X VALUE 'N'.                       
001618         88  FILE5-EOF             VALUE 'Y'.                             
001619     05  FILE6-EOF-SWITCH          PIC X VALUE 'N'.                       
001620         88  FILE6-EOF             VALUE 'Y'.                             
001621     05  FILE7-EOF-SWITCH          PIC X VALUE 'N'.                       
001622         88  FILE7-EOF             VALUE 'Y'.                             
001623     05  FILE8-EOF-SWITCH          PIC X VALUE 'N'.                       
001624         88  FILE8-EOF             VALUE 'Y'.                             
001625     05  FILE9-EOF-SWITCH          PIC X VALUE 'N'.                       
001626         88  FILE9-EOF             VALUE 'Y'.                             
001627                                                                          
001628 01  HEADER-LINE.                                                         
001629     05  FILLER           PIC X(16) VALUE '1THIS IS LINE 1 '.             
001630     05  FILLER           PIC X(19) VALUE 'FOR PRINT FILE NBR '.          
001631     05  HEADER-ID        PIC X.                                          
001632     05  FILLER           PIC X(44) VALUE SPACES.                         
001633                                                                          
001634 01  OUT-RECORD.                                                          
001635     05  FILLER           PIC X(16) VALUE ' THIS IS RECORD '.             
001636     05  FILLER           PIC X(15) VALUE 'NUMBER ONE FOR '.              
001637     05  FILLER           PIC X(16) VALUE 'OUTPUT FILE NBR '.             
001638     05  OUT-FILE-NBR     PIC X.                                          
001639     05  FILLER           PIC X(32) VALUE SPACES.                         
001640                                                                          
001641 01  TABLE1.                                                              
001642     05  ENTRY1              OCCURS 180 TIMES INDEXED BY X1.              
001643         10 FIELD1           PIC X(80).                                   
001644 01  TABLE2.                                                              
001650     05  ENTRY2              OCCURS 180 TIMES INDEXED BY X2.              
001660         10 FIELD2           PIC X(80).                                   
001670 01  TABLE3.                                                              
001680     05  ENTRY3              OCCURS 180 TIMES INDEXED BY X3.              
001690         10 FIELD3           PIC X(80).                                   
001691 01  TABLE4.                                                              
001692     05  ENTRY4              OCCURS 180 TIMES INDEXED BY X4.              
001693         10 FIELD4           PIC X(80).                                   
001694 01  TABLE5.                                                              
001695     05  ENTRY5              OCCURS 180 TIMES INDEXED BY X5.              
001696         10 FIELD5           PIC X(80).                                   
001697 01  TABLE6.                                                              
001698     05  ENTRY6              OCCURS 180 TIMES INDEXED BY X6.              
001699         10 FIELD6           PIC X(80).                                   
001700 01  TABLE7.                                                              
001701     05  ENTRY7              OCCURS 180 TIMES INDEXED BY X7.              
001702         10 FIELD7           PIC X(80).                                   
001703 01  TABLE8.                                                              
001704     05  ENTRY8              OCCURS 180 TIMES INDEXED BY X8.              
001705         10 FIELD8           PIC X(80).                                   
001706 01  TABLE9.                                                              
001707     05  ENTRY9              OCCURS 180 TIMES INDEXED BY X9.              
001708         10 FIELD9           PIC X(80).                                   
001709                                                                          
001710 PROCEDURE DIVISION.                                                      
001800                                                                          
002000     PERFORM A100-INITIALIZATION.                                         
002100     PERFORM B100-MAINLINE-PROCESSING.                                    
002200     PERFORM Z100-END-OF-PROCESSING.                                      
002300     GOBACK.                                                              
002400                                                                          
002500 A100-INITIALIZATION.                                                     
002600     OPEN INPUT INPUT-FIL1, INPUT-FIL2, INPUT-FIL3,                       
002610                INPUT-FIL4, INPUT-FIL5, INPUT-FIL6,                       
002611                INPUT-FIL7, INPUT-FIL8, INPUT-FIL9,                       
002630         OUTPUT OUTPUT-FIL1, OUTPUT-FIL2, OUTPUT-FIL3,                    
002631                OUTPUT-FIL4, OUTPUT-FIL5, OUTPUT-FIL6,                    
002640                OUTPUT-FIL7, OUTPUT-FIL8, OUTPUT-FIL9,                    
002660                PRINT-FIL1, PRINT-FIL2, PRINT-FIL3,                       
002670                PRINT-FIL4, PRINT-FIL5, PRINT-FIL6,                       
002671                PRINT-FIL7, PRINT-FIL8, PRINT-FIL9.                       
002700     PERFORM A101-LOAD-TABLE1 UNTIL FILE1-EOF OR X1 > 179.                
002710     PERFORM A102-LOAD-TABLE2 UNTIL FILE2-EOF OR X2 > 179.                
002720     PERFORM A103-LOAD-TABLE3 UNTIL FILE3-EOF OR X3 > 179.                
002730     PERFORM A104-LOAD-TABLE4 UNTIL FILE4-EOF OR X4 > 179.                
002740     PERFORM A105-LOAD-TABLE5 UNTIL FILE5-EOF OR X5 > 179.                
002750     PERFORM A106-LOAD-TABLE6 UNTIL FILE6-EOF OR X6 > 179.                
002760     PERFORM A107-LOAD-TABLE7 UNTIL FILE7-EOF OR X7 > 179.                
002770     PERFORM A108-LOAD-TABLE8 UNTIL FILE8-EOF OR X8 > 179.                
002780     PERFORM A109-LOAD-TABLE9 UNTIL FILE9-EOF OR X9 > 179.                
002781     PERFORM A201-PRINT-HEADER1.                                          
002782     PERFORM A202-PRINT-HEADER2.                                          
002783     PERFORM A203-PRINT-HEADER3.                                          
002784     PERFORM A204-PRINT-HEADER4.                                          
002785     PERFORM A205-PRINT-HEADER5.                                          
002786     PERFORM A206-PRINT-HEADER6.                                          
002787     PERFORM A207-PRINT-HEADER7.                                          
002788     PERFORM A208-PRINT-HEADER8.                                          
002789     PERFORM A209-PRINT-HEADER9.                                          
002790     PERFORM A301-WRITE-OUTPUT1.                                          
002791     PERFORM A302-WRITE-OUTPUT2.                                          
002792     PERFORM A303-WRITE-OUTPUT3.                                          
002793     PERFORM A304-WRITE-OUTPUT4.                                          
002794     PERFORM A305-WRITE-OUTPUT5.                                          
002795     PERFORM A306-WRITE-OUTPUT6.                                          
002796     PERFORM A307-WRITE-OUTPUT7.                                          
002797     PERFORM A308-WRITE-OUTPUT8.                                          
002798     PERFORM A309-WRITE-OUTPUT9.                                          
002803                                                                          
002804 A101-LOAD-TABLE1.                                                        
002810     READ INPUT-FIL1                                                      
002900         AT END                                                           
003000             MOVE 'Y' TO FILE1-EOF-SWITCH.                                
003100     IF FILE1-EOF                                                         
003200         MOVE HIGH-VALUES TO FIELD1 (X1)                                  
003300     ELSE                                                                 
003400         MOVE INPUT-RECORD1 TO FIELD1 (X1)                                
003500         SET X1 UP BY 1.                                                  
003600                                                                          
003700 A102-LOAD-TABLE2.                                                        
003800     READ INPUT-FIL2                                                      
003900         AT END                                                           
004000             MOVE 'Y' TO FILE2-EOF-SWITCH.                                
004100     IF FILE2-EOF                                                         
004200         MOVE HIGH-VALUES TO FIELD2 (X2)                                  
004300     ELSE                                                                 
004400         MOVE INPUT-RECORD2 TO FIELD2 (X2)                                
004500         SET X2 UP BY 1.                                                  
004510                                                                          
004520 A103-LOAD-TABLE3.                                                        
004530     READ INPUT-FIL3                                                      
004540         AT END                                                           
004550             MOVE 'Y' TO FILE3-EOF-SWITCH.                                
004551     IF FILE3-EOF                                                         
004552         MOVE HIGH-VALUES TO FIELD3 (X3)                                  
004553     ELSE                                                                 
004554         MOVE INPUT-RECORD3 TO FIELD3 (X3)                                
004555         SET X3 UP BY 1.                                                  
004556                                                                          
004557 A104-LOAD-TABLE4.                                                        
004558     READ INPUT-FIL4                                                      
004559         AT END                                                           
004560             MOVE 'Y' TO FILE4-EOF-SWITCH.                                
004561     IF FILE4-EOF                                                         
004562         MOVE HIGH-VALUES TO FIELD4 (X4)                                  
004563     ELSE                                                                 
004564         MOVE INPUT-RECORD4 TO FIELD4 (X4)                                
004565         SET X4 UP BY 1.                                                  
004566                                                                          
004567 A105-LOAD-TABLE5.                                                        
004568     READ INPUT-FIL5                                                      
004569         AT END                                                           
004570             MOVE 'Y' TO FILE5-EOF-SWITCH.                                
004571     IF FILE5-EOF                                                         
004572         MOVE HIGH-VALUES TO FIELD5 (X5)                                  
004573     ELSE                                                                 
004574         MOVE INPUT-RECORD5 TO FIELD5 (X5)                                
004575         SET X5 UP BY 1.                                                  
004576                                                                          
004577 A106-LOAD-TABLE6.                                                        
004578     READ INPUT-FIL6                                                      
004579         AT END                                                           
004580             MOVE 'Y' TO FILE6-EOF-SWITCH.                                
004581     IF FILE6-EOF                                                         
004582         MOVE HIGH-VALUES TO FIELD6 (X6)                                  
004583     ELSE                                                                 
004584         MOVE INPUT-RECORD6 TO FIELD6 (X6)                                
004585         SET X6 UP BY 1.                                                  
004586                                                                          
004587 A107-LOAD-TABLE7.                                                        
004588     READ INPUT-FIL7                                                      
004589         AT END                                                           
004590             MOVE 'Y' TO FILE7-EOF-SWITCH.                                
004591     IF FILE7-EOF                                                         
004592         MOVE HIGH-VALUES TO FIELD7 (X7)                                  
004593     ELSE                                                                 
004594         MOVE INPUT-RECORD7 TO FIELD7 (X7)                                
004595         SET X7 UP BY 1.                                                  
004596                                                                          
004597 A108-LOAD-TABLE8.                                                        
004598     READ INPUT-FIL8                                                      
004599         AT END                                                           
004600             MOVE 'Y' TO FILE8-EOF-SWITCH.                                
004601     IF FILE8-EOF                                                         
004602         MOVE HIGH-VALUES TO FIELD8 (X8)                                  
004603     ELSE                                                                 
004604         MOVE INPUT-RECORD8 TO FIELD8 (X8)                                
004605         SET X8 UP BY 1.                                                  
004606                                                                          
004607 A109-LOAD-TABLE9.                                                        
004608     READ INPUT-FIL9                                                      
004609         AT END                                                           
004610             MOVE 'Y' TO FILE9-EOF-SWITCH.                                
004611     IF FILE9-EOF                                                         
004612         MOVE HIGH-VALUES TO FIELD9 (X9)                                  
004613     ELSE                                                                 
004614         MOVE INPUT-RECORD9 TO FIELD9 (X9)                                
004615         SET X9 UP BY 1.                                                  
004616                                                                          
004617 A201-PRINT-HEADER1.                                                      
004618     MOVE '1' TO HEADER-ID.                                               
004619     WRITE PRINT-RECORD1 FROM HEADER-LINE AFTER 1.                        
004620                                                                          
004621 A202-PRINT-HEADER2.                                                      
004622     MOVE '2' TO HEADER-ID.                                               
004623     WRITE PRINT-RECORD2 FROM HEADER-LINE AFTER 1.                        
004624                                                                          
004625 A203-PRINT-HEADER3.                                                      
004626     MOVE '3' TO HEADER-ID.                                               
004627     WRITE PRINT-RECORD3 FROM HEADER-LINE AFTER 1.                        
004628                                                                          
004629 A204-PRINT-HEADER4.                                                      
004630     MOVE '4' TO HEADER-ID.                                               
004631     WRITE PRINT-RECORD4 FROM HEADER-LINE AFTER 1.                        
004632                                                                          
004633 A205-PRINT-HEADER5.                                                      
004634     MOVE '5' TO HEADER-ID.                                               
004635     WRITE PRINT-RECORD5 FROM HEADER-LINE AFTER 1.                        
004636                                                                          
004637 A206-PRINT-HEADER6.                                                      
004638     MOVE '6' TO HEADER-ID.                                               
004639     WRITE PRINT-RECORD6 FROM HEADER-LINE AFTER 1.                        
004640                                                                          
004641 A207-PRINT-HEADER7.                                                      
004642     MOVE '7' TO HEADER-ID.                                               
004643     WRITE PRINT-RECORD7 FROM HEADER-LINE AFTER 1.                        
004644                                                                          
004645 A208-PRINT-HEADER8.                                                      
004646     MOVE '8' TO HEADER-ID.                                               
004647     WRITE PRINT-RECORD8 FROM HEADER-LINE AFTER 1.                        
004648                                                                          
004649 A209-PRINT-HEADER9.                                                      
004650     MOVE '9' TO HEADER-ID.                                               
004651     WRITE PRINT-RECORD9 FROM HEADER-LINE AFTER 1.                        
004652                                                                          
004653 A301-WRITE-OUTPUT1.                                                      
004654     MOVE '1' TO OUT-FILE-NBR.                                            
004655     WRITE OUTPUT-RECORD1 FROM OUT-RECORD.                                
004656                                                                          
004657 A302-WRITE-OUTPUT2.                                                      
004658     MOVE '2' TO OUT-FILE-NBR.                                            
004659     WRITE OUTPUT-RECORD2 FROM OUT-RECORD.                                
004660                                                                          
004661 A303-WRITE-OUTPUT3.                                                      
004662     MOVE '3' TO OUT-FILE-NBR.                                            
004663     WRITE OUTPUT-RECORD3 FROM OUT-RECORD.                                
004664                                                                          
004665 A304-WRITE-OUTPUT4.                                                      
004666     MOVE '4' TO OUT-FILE-NBR.                                            
004667     WRITE OUTPUT-RECORD4 FROM OUT-RECORD.                                
004668                                                                          
004669 A305-WRITE-OUTPUT5.                                                      
004670     MOVE '5' TO OUT-FILE-NBR.                                            
004671     WRITE OUTPUT-RECORD5 FROM OUT-RECORD.                                
004672                                                                          
004673 A306-WRITE-OUTPUT6.                                                      
004674     MOVE '6' TO OUT-FILE-NBR.                                            
004675     WRITE OUTPUT-RECORD6 FROM OUT-RECORD.                                
004676                                                                          
004677 A307-WRITE-OUTPUT7.                                                      
004678     MOVE '7' TO OUT-FILE-NBR.                                            
004679     WRITE OUTPUT-RECORD7 FROM OUT-RECORD.                                
004680                                                                          
004681 A308-WRITE-OUTPUT8.                                                      
004682     MOVE '8' TO OUT-FILE-NBR.                                            
004683     WRITE OUTPUT-RECORD8 FROM OUT-RECORD.                                
004684                                                                          
004685 A309-WRITE-OUTPUT9.                                                      
004686     MOVE '9' TO OUT-FILE-NBR.                                            
004687     WRITE OUTPUT-RECORD9 FROM OUT-RECORD.                                
004688                                                                          
008117 B100-MAINLINE-PROCESSING.                                                
008118     CALL 'GETMAIN'.                                                      
008119     ADD 1 TO GETMAIN-CALLS.                                              
008120     MOVE GETMAIN-CALLS TO PRINT-CALLS                                    
008121     WRITE PRINT-RECORD1 FROM NUMBER-CALLS AFTER 1.                       
008122     GO TO B100-MAINLINE-PROCESSING.                                      
008129                                                                          
008130 Z100-END-OF-PROCESSING.                                                  
008131     CLOSE INPUT-FIL1, INPUT-FIL2, INPUT-FIL3,                            
008132           INPUT-FIL4, INPUT-FIL5, INPUT-FIL6,                            
008133           INPUT-FIL7, INPUT-FIL8, INPUT-FIL9,                            
008134           OUTPUT-FIL1, OUTPUT-FIL2, OUTPUT-FIL3,                         
008135           OUTPUT-FIL4, OUTPUT-FIL5, OUTPUT-FIL6,                         
008136           OUTPUT-FIL7, OUTPUT-FIL8, OUTPUT-FIL9,                         
008137           PRINT-FIL1, PRINT-FIL2, PRINT-FIL3,                            
008140           PRINT-FIL4, PRINT-FIL5, PRINT-FIL6,                            
008200           PRINT-FIL7, PRINT-FIL8, PRINT-FIL9.                            

