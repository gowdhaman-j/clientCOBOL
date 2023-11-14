000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID.         CALLRNMF                                             
000300 AUTHOR.             R THORNTON                                           
000400*****************************************************************         
000500*REMARKS. PROGRAM TO TEST SUBROUTINE RANDMCTF.                            
000600*****************************************************************         
000700 ENVIRONMENT DIVISION.                                                    
000800 CONFIGURATION SECTION.                                                   
000900 INPUT-OUTPUT SECTION.                                                    
001000 FILE-CONTROL.                                                            
001100     SELECT REQUEST-FILE ASSIGN TO REQUESTS.                              
001200     SELECT OUTPUT-FILE ASSIGN TO MCTFOUT.                                
001300 DATA DIVISION.                                                           
001400 FILE SECTION.                                                            
001500 FD  REQUEST-FILE                                                         
001600     BLOCK CONTAINS 0 RECORDS                                             
001700     RECORD CONTAINS 80 CHARACTERS                                        
001800     RECORDING MODE IS F                                                  
001900     LABEL RECORDS ARE STANDARD.                                          
002000 01  REQUEST-RECORD.                                                    10
002100     05  REQUEST-CODE            PIC X(1).                              20
002110     05  REQUEST-KEY             PIC X(15).                             20
002200     05  FILLER                  PIC X(64).                             40
002300 FD  OUTPUT-FILE                                                          
002400     BLOCK CONTAINS 0 RECORDS                                             
002500     RECORD CONTAINS 244 CHARACTERS                                       
002600     RECORDING MODE IS F                                                  
002700     LABEL RECORDS ARE STANDARD.                                          
002800 01  OUTPUT-RECORD.                                                     10
002900     05  OUT-REQUEST-CODE-SENT   PIC X(1).                              20
002910     05  OUT-REQUEST-KEY-SENT    PIC X(15).                             20
002920     05  OUT-REQUEST-CODE-BACK   PIC X(1).                              20
002930     05  OUT-REQUEST-KEY-BACK    PIC X(15).                             20
003300     05  OUT-MCTF-RECORD-BACK    PIC X(212).                            30
003400 WORKING-STORAGE SECTION.                                                 
003500 01  MISCELLANEOUS-WORK-FIELDS.                                           
003600     05  REQUEST-FILE-SWITCH     PIC X VALUE 'N'.                         
003700         88 MORE-KEYS            VALUE 'N'.                               
003800         88 END-OF-JOB           VALUE 'Y'.                               
005100 01  RANDMCTF-FIELDS.                                                     
005200     05  RANDMCTF-IOOP           PIC X.                                   
005300     05  RANDMCTF-KEY-FIELD      PIC X(15).                               
005800 01  RANDMCTF-RETURNED-RECORD    PIC X(212).                              
006000                                                                          
006600 PROCEDURE DIVISION.                                                      
006700 A100-EXECUTIVE-PROCESSING.                                               
006800     PERFORM A200-HOUSEKEEPING THRU A200-EXIT.                            
006900     PERFORM B100-MAINLINE UNTIL END-OF-JOB.                              
007000     PERFORM Z100-END-OF-JOB THRU Z100-EXIT.                              
007100     GOBACK.                                                              
007110                                                                          
007200 A200-HOUSEKEEPING.                                                       
007300     OPEN INPUT REQUEST-FILE, OUTPUT OUTPUT-FILE.                         
007600 A200-EXIT. EXIT.                                                         
007700                                                                          
007800 B100-MAINLINE.                                                           
007900     PERFORM C100-READ-REQUEST-RECORD THRU C100-EXIT.                     
008000     IF MORE-KEYS                                                         
008010         MOVE REQUEST-CODE TO RANDMCTF-IOOP                               
008020         MOVE REQUEST-KEY TO RANDMCTF-KEY-FIELD                           
008100         PERFORM D100-CALL-RANDMCTF THRU D100-EXIT                        
008200         PERFORM E100-WRITE-OUTPUT THRU E100-EXIT.                        
008300 B100-EXIT. EXIT.                                                         
008400                                                                          
008500 C100-READ-REQUEST-RECORD.                                                
008600     READ REQUEST-FILE AT END MOVE 'Y' TO REQUEST-FILE-SWITCH.            
008700 C100-EXIT. EXIT.                                                         
008800                                                                          
008900 D100-CALL-RANDMCTF.                                                      
009000     MOVE RANDMCTF-IOOP TO OUT-REQUEST-CODE-SENT.                         
009100     MOVE RANDMCTF-KEY-FIELD TO OUT-REQUEST-KEY-SENT.                     
009200     MOVE SPACES TO RANDMCTF-RETURNED-RECORD.                             
009300     CALL 'RANDMCTF' USING RANDMCTF-IOOP,                                 
009400                           RANDMCTF-KEY-FIELD,                            
009500                           RANDMCTF-RETURNED-RECORD.                      
009510     MOVE RANDMCTF-IOOP TO OUT-REQUEST-CODE-BACK.                         
009520     MOVE RANDMCTF-KEY-FIELD TO OUT-REQUEST-KEY-BACK.                     
009530     MOVE RANDMCTF-RETURNED-RECORD TO OUT-MCTF-RECORD-BACK.               
009600 D100-EXIT. EXIT.                                                         
009700                                                                          
009800 E100-WRITE-OUTPUT.                                                       
011500     WRITE OUTPUT-RECORD.                                                 
011600 E100-EXIT. EXIT.                                                         
011700                                                                          
011800 Z100-END-OF-JOB.                                                         
011900     CLOSE REQUEST-FILE, OUTPUT-FILE.                                     
012000 Z100-EXIT. EXIT.                                                         
  