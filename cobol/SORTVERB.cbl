000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID.    SORTVERB.                                                 
000300 AUTHOR.        R THORNTON                                                
000400 DATE-WRITTEN.  DEC 2000.                                                 
000500 DATE-COMPILED.                                                           
000600******************************************************************        
000700*REMARKS: THIS PROGRAM CONTAINS SAMPLE COBOL INTERNAL SORT CODE. *        
000800******************************************************************        
000900 ENVIRONMENT DIVISION.                                                    
001000 CONFIGURATION SECTION.                                                   
001100 INPUT-OUTPUT SECTION.                                                    
001200 FILE-CONTROL.                                                            
001300     SELECT SORT-FILE          ASSIGN   TO  SORTWK01.                     
001400     SELECT INPUT-FILE         ASSIGN   TO  INPUT1.                       
001500     SELECT SORTED-FILE        ASSIGN   TO  OUTPUT1.                      
001600 DATA DIVISION.                                                           
001700 FILE SECTION.                                                            
001800 SD  SORT-FILE                                                            
001900     DATA RECORD IS SORT-RECORD.                                          
002000 01  SORT-RECORD.                                                         
002100     05  SD-MEMBER-NAME           PIC X(8).                               
002200     05  SD-REST                  PIC X(72).                              
002300 FD  INPUT-FILE                                                           
002400     DATA RECORD IS INPUT-RECORD                                          
002500     RECORD CONTAINS 80 CHARACTERS                                        
002600     BLOCK CONTAINS 0 RECORDS                                             
002700     LABEL RECORDS ARE STANDARD                                           
002800     RECORDING MODE IS F.                                                 
002900 01  INPUT-RECORD.                                                        
003000     05  IN-MEMBER-NAME           PIC X(8).                               
003100     05  IN-REST                  PIC X(72).                              
003200 FD  SORTED-FILE                                                          
003300     DATA RECORD IS SORTED-RECORD                                         
003400     RECORD CONTAINS 80 CHARACTERS                                        
003500     BLOCK CONTAINS 0 RECORDS                                             
003600     LABEL RECORDS ARE STANDARD                                           
003700     RECORDING MODE IS F.                                                 
003800 01  SORTED-RECORD.                                                       
003900     05  OUT-MEMBER-NAME          PIC X(8).                               
004000     05  OUT-REST                 PIC X(72).                              
004100 WORKING-STORAGE SECTION.                                                 
004200 77  FILLER                        PIC X(36) VALUE                        
004300     'COBLSORT WORKING STORAGE STARTS HERE'.                              
004400 01  SWITCHES.                                                            
004500     05  INPUT-EOF-SWITCH         PIC X VALUE 'N'.                        
004600         88  INPUT-EOF            VALUE 'Y'.                              
004700     05  SORT-EOF-SWITCH          PIC X VALUE 'N'.                        
004800         88  SORT-EOF             VALUE 'Y'.                              
004900                                                                          
005000 PROCEDURE DIVISION.                                                      
005100     OPEN INPUT INPUT-FILE, OUTPUT SORTED-FILE.                           
005200     SORT SORT-FILE ASCENDING KEY SD-MEMBER-NAME                          
005300         INPUT PROCEDURE IS 100-READ-INPUT                                
005400         OUTPUT PROCEDURE IS 200-WRITE-OUTPUT.                            
005500     CLOSE INPUT-FILE, SORTED-FILE.                                       
005600     GOBACK.                                                              
005700                                                                          
005800 100-READ-INPUT SECTION.                                                  
005900     PERFORM 1000-READ-INPUT-FILE THRU 100-EXIT                           
006000         UNTIL INPUT-EOF.                                                 
006100                                                                          
006200 200-WRITE-OUTPUT SECTION.                                                
006300     PERFORM 2000-WRITE-SORTED-FILE THRU 200-EXIT                         
006400         UNTIL SORT-EOF.                                                  
006500                                                                          
006600 999-PROCEDURES SECTION.                                                  
006700 1000-READ-INPUT-FILE.                                                    
006800     READ INPUT-FILE                                                      
006900         AT END                                                           
007000             MOVE 'Y' TO INPUT-EOF-SWITCH                                 
007100             GO TO 100-EXIT.                                              
007200     PERFORM 110-BUILD-SORT-RECORD THRU 110-EXIT                          
007300     GO TO 1000-READ-INPUT-FILE.                                          
007400 100-EXIT. EXIT.                                                          
007500                                                                          
007600 110-BUILD-SORT-RECORD.                                                   
007700     MOVE INPUT-RECORD TO SORT-RECORD.                                    
007800     RELEASE SORT-RECORD.                                                 
007900 110-EXIT. EXIT.                                                          
008000                                                                          
008100 2000-WRITE-SORTED-FILE.                                                  
008200     RETURN SORT-FILE                                                     
008300         AT END                                                           
008400             MOVE 'Y' TO SORT-EOF-SWITCH                                  
008500             GO TO 200-EXIT.                                              
008600     WRITE SORTED-RECORD FROM SORT-RECORD.                                
008700     GO TO 2000-WRITE-SORTED-FILE.                                        
008800 200-EXIT. EXIT.                                                          
  