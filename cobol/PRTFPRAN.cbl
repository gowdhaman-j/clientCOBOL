000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID.  PRTFPRAN.                                                   
000300 REMARKS. PRINT SEQUENCE OF RANDOM NUMBERS BASED ON CONTROL CARDS:        
000400           (A) THE CONTROL CARD CONTAINS A SEED VALUE IN COLUMNS          
000500               1-9, RIGHT-JUSTIFIED, AND THE NUMBER OF RANDOM OUTPUT      
000600               VALUES TO BE PRINTED IN COLUMNS 11-15, RIGHT JUSTIFIED.    
000700           (B) THE PRINTED OUTPUT CONTAINS THE ORIGINAL CONTROL           
000800               CARD FOLLOWED BY THE RANDOM NUMBERS GENERATED, ONE PER     
000900               PRINT LINE.                                                
001000 ENVIRONMENT DIVISION.                                                    
001100 CONFIGURATION SECTION.                                                   
001200 INPUT-OUTPUT SECTION.                                                    
001300 FILE-CONTROL.                                                            
001400     SELECT CONTROL-CARD ASSIGN TO UT-S-READER1.                          
001500     SELECT PRINT-OUT ASSIGN TO UT-S-PRINT1.                              
001600 DATA DIVISION.                                                           
001700 FILE SECTION.                                                            
001800 FD  CONTROL-CARD                                                         
001900       LABEL RECORDS ARE STANDARD                                         
002000       RECORD CONTAINS 80 CHARACTERS                                      
002100       BLOCK CONTAINS 0 RECORDS                                           
002200       DATA RECORD IS CARD-IMAGE.                                         
002300 01  CARD-IMAGE.                                                          
002400     05  RANDOM-SEED             PIC 9(9).                                
002500     05  FILLER                  PIC X.                                   
002600     05  NUMBER-VALUES           PIC 9(5).                                
002700     05  FILLER                  PIC X(65).                               
002800 FD  PRINT-OUT                                                            
002900       LABEL RECORDS ARE STANDARD                                         
003000       RECORD CONTAINS 80 CHARACTERS                                      
003100       BLOCK CONTAINS 0 RECORDS                                           
003200       DATA RECORD IS PRINT-LINE.                                         
003300 01  PRINT-LINE.                                                          
003400     05  PRINT-LITERAL      PIC X(9).                                     
003500     05  PRINT-NUMBER       PIC .9(15).                                   
003600     05  FILLER             PIC X(55).                                    
003700 WORKING-STORAGE SECTION.                                                 
003800 77  RANDOM-VALUE            COMP-2.                                      
003900 77  LOOP-CTR                PIC S9(5) COMP-3.                            
004000 01  MISC-WORK.                                                           
004100     05  PRINTABLE-FP        PIC -9.9(15)E-99.                            
004200     05  FILLER REDEFINES PRINTABLE-FP.                                   
004300         10  FP-MANTISSA-FIELD.                                           
004400             15  FP-SIGN1    PIC X(1).                                    
004500             15  FP-HI-DIGIT PIC 9(1).                                    
004600             15  FP-DECIMAL  PIC X(1).                                    
004700             15  FP-FRACTION PIC V9(15).                                  
004800         10  FP-EXPONENT-FIELD.                                           
004900             15  FP-E        PIC X(1).                                    
005000             15  FP-SIGN2    PIC X(1).                                    
005100             15  FP-EXPONENT PIC 9(2).                                    
005200 PROCEDURE DIVISION.                                                      
005300     OPEN INPUT CONTROL-CARD, OUTPUT PRINT-OUT.                           
005400     READ CONTROL-CARD                                                    
005500         AT END DISPLAY 'MISSING CONTROL CARD'                            
005600         CLOSE CONTROL-CARD, PRINT-OUT                                    
005700         GOBACK.                                                          
005800     IF RANDOM-SEED IS NUMERIC AND NUMBER-VALUES IS NUMERIC,              
005900         MOVE RANDOM-SEED TO RANDOM-VALUE                                 
006000     ELSE                                                                 
006100         DISPLAY 'INVALID CONTROL CARD'                                   
006200         CLOSE CONTROL-CARD, PRINT-OUT                                    
006300         GOBACK.                                                          
006400     PERFORM COMPUTE-RANDOM VARYING LOOP-CTR FROM 1 BY 1                  
006500         UNTIL LOOP-CTR IS GREATER THAN NUMBER-VALUES.                    
006600     CLOSE CONTROL-CARD, PRINT-OUT.                                       
006700     GOBACK.                                                              
006800 COMPUTE-RANDOM.                                                          
006900     COMPUTE RANDOM-VALUE =                                               
007000         (RANDOM-VALUE / ((2 ** 31) - 1)) * (7 ** 5).                     
007100     MOVE RANDOM-VALUE TO PRINTABLE-FP                                    
007200     MOVE SPACES TO PRINT-LINE.                                           
007300     MOVE 'RANDOM = ' TO PRINT-LITERAL.                                   
007400     IF FP-FRACTION IS ZERO                                               
007500         ADD 1 TO RANDOM-SEED                                             
007600         MOVE RANDOM-SEED TO RANDOM-VALUE                                 
007700         SUBTRACT 1 FROM LOOP-CTR                                         
007800     ELSE                                                                 
007900         MOVE FP-FRACTION TO PRINT-NUMBER                                 
008000         WRITE PRINT-LINE.                                                
  