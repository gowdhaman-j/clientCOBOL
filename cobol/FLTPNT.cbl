000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID.  FLTPNT.                                                     
000300 REMARKS. USED TO INVESTIGATE COBOL'S USE OF FLOATING POINT. SEVERAL      
000400          SIGNIFICANT COBOL CODING RULES ARE:                             
000500          (1) INTERNAL FLOATING POINT FIELDS:                             
000600              (A) DO NOT CODE A PIC CLAUSE.                               
000700              (B) FOR SHORT PRECISION (4-BYTE) FIELDS USE COMP-1.         
000800              (C) FOR LONG PRECISION (8-BYTE) FIELDS USE COMP-2.          
000900              (D) VALUE CLAUSES THE LITERAL MUST BE OF THE FORM           
001000                  SMM.MMESXX, WHERE:                                      
001100                  S IS A SIGN, + OR - AND IS REQUIRED BOTH PLACES.        
001200                  M IS A DIGIT IN THE MANTISSA (16 DIGITS MAX)            
001300                  . IS A DECIMAL POINT, AND IS REQUIRED.                  
001400                  E IS THE LETTER E, IDENTIFYING THE EXPONENT, AND        
001500                    IS REQUIRED.                                          
001600                  XX IS THE EXPONENT VALUE BETWEEN -79 AND +76.           
001700              (E) MINIMUM VALUE IS +5.41E-79.                             
001800              (F) MAXIMUM VALUE IS +.719E+76.                             
001900          (2) EXTERNAL FLOATING POINT FIELDS:                             
002000              (A) USAGE DISPLAY MUST BE CODED OR IMPLIED                  
002100              (B) NO VALUE CLAUSE MAY BE CODED                            
002200              (C) PIC CHARACTER STRING MUST BE OF THE FORM                
002300                  S99D99ESXX, WHERE                                       
002400                  S IS A SIGN INDICATOR, + SPECIFIES THAT "+"             
002500                    PRECEDES POSITIVE VALUES. IF - IS USED,               
002600                    A SPACE PRECEDES POSITIVE VALUES. FOR BOTH,           
002700                    A "-" PRECEDES NEGATIVE VALUES. REQUIRED.             
002800                  9 IS A DIGIT IN THE MANTISSA (16 DIGITS MAX)            
002900                  D IS AN ACTUAL DECIMAL POINT (.) OR IMPLIED             
003000                    DECIMAL POINT (V), AND IS REQUIRED.                   
003100                  E IS THE LETTER E, IDENTIFYING THE EXPONENT             
003200                  XX REPRESENTS THE EXPONENT VALUE AND MUST BE            
003300                    TWO DIGITS.                                           
003400 ENVIRONMENT DIVISION.                                                    
003500 CONFIGURATION SECTION.                                                   
003600 INPUT-OUTPUT SECTION.                                                    
003700 FILE-CONTROL.                                                            
003800     SELECT PRINT-OUT ASSIGN TO UT-S-PRINT1.                              
003900 DATA DIVISION.                                                           
004000 FILE SECTION.                                                            
004100 FD  PRINT-OUT                                                            
004200       LABEL RECORDS ARE STANDARD                                         
004300       RECORD CONTAINS 133 CHARACTERS                                     
004400       DATA RECORD IS PRINT-LINE.                                         
004500 01  PRINT-LINE.                                                          
004600     05  PRINT-CC                PIC X.                                   
004700     05  PRINT-DATA              PIC X(132).                              
004800 WORKING-STORAGE SECTION.                                                 
004900 77  MINFP                  COMP-2 VALUE +5.41E-79.                       
005000 77  MAXFP                  COMP-2 VALUE +.719E+76.                       
005100 77  MULNBR                 COMP-2 VALUE +1.7E+71.                        
005200 77  DIVNBR                 COMP-2 VALUE +2147483647.E+0.                 
005300 77  SEEDNBR                COMP-2 VALUE +1.111E+0.                       
005400 77  FPANSWER               COMP-2 VALUE +0.E+0.                          
005500 77  FP231-1                COMP-2.                                       
005600 77  FP9                    COMP-2.                                       
005700 77  FP1E9                  COMP-2.                                       
005800 77  FP2700                 COMP-2.                                       
005900*                                                                         
006000 01  PRINT-MINFP.                                                         
006100     05 FILLER              PIC X(12) VALUE ' MINFP =    '.               
006200     05 PRT-MINFP           PIC -9.9(15)E-99.                             
006300 01  PRINT-MAXFP.                                                         
006400     05 FILLER              PIC X(12) VALUE ' MAXFP =    '.               
006500     05 PRT-MAXFP           PIC -9.9(15)E-99.                             
006600 01  PRINT-MULNBR.                                                        
006700     05 FILLER              PIC X(12) VALUE ' MULNBR =   '.               
006800     05 PRT-MULNBR          PIC -9.9(15)E-99.                             
006900 01  PRINT-DIVNBR.                                                        
007000     05 FILLER              PIC X(12) VALUE ' DIVNBR =   '.               
007100     05 PRT-DIVNBR          PIC -9.9(15)E-99.                             
007200 01  PRINT-SEEDNBR.                                                       
007300     05 FILLER              PIC X(12) VALUE ' SEEDNBR =  '.               
007400     05 PRT-SEEDNBR         PIC -9.9(15)E-99.                             
007500 01  PRINT-FPANSWER.                                                      
007600     05 FILLER              PIC X(12) VALUE ' FPANSWER = '.               
007700     05 PRT-FPANSWER        PIC -9.9(15)E-99.                             
007800 01  PRINT-FP231-1.                                                       
007900     05 FILLER              PIC X(12) VALUE ' FP231-1 =  '.               
008000     05 PRT-FP231-1         PIC -9.9(15)E-99.                             
008100 01  PRINT-FP9.                                                           
008200     05 FILLER              PIC X(12) VALUE ' FP9 =      '.               
008300     05 PRT-FP9             PIC -9.9(15)E-99.                             
008400 01  PRINT-FP1E9.                                                         
008500     05 FILLER              PIC X(12) VALUE ' FP1E9 =    '.               
008600     05 PRT-FP1E9           PIC -9.9(15)E-99.                             
008700 01  PRINT-FP2700.                                                        
008800     05 FILLER              PIC X(12) VALUE ' FP2700 =   '.               
008900     05 PRT-FP2700          PIC -9.9(15)E-99.                             
009000                                                                          
009100 PROCEDURE DIVISION.                                                      
009200     OPEN OUTPUT PRINT-OUT.                                               
009300     MOVE MINFP TO PRT-MINFP.                                             
009400     MOVE MAXFP TO PRT-MAXFP.                                             
009500     MOVE MULNBR TO PRT-MULNBR.                                           
009600     MOVE DIVNBR TO PRT-DIVNBR.                                           
009700     MOVE SEEDNBR TO PRT-SEEDNBR.                                         
009800     COMPUTE FPANSWER = (SEEDNBR / DIVNBR) * MULNBR.                      
009900     MOVE FPANSWER TO PRT-FPANSWER.                                       
010000     COMPUTE FP231-1 = (2 ** 31) - 1.                                     
010100     MOVE FP231-1 TO PRT-FP231-1.                                         
010200     MOVE +9 TO FP9.                                                      
010300     MOVE FP9 TO PRT-FP9.                                                 
010400     MOVE +1.0E+9 TO FP1E9.                                               
010500     MOVE FP1E9 TO PRT-FP1E9.                                             
010600     MOVE 2700 TO FP2700.                                                 
010700     MOVE FP2700 TO PRT-FP2700.                                           
010800     WRITE PRINT-LINE FROM PRINT-MINFP.                                   
010900     WRITE PRINT-LINE FROM PRINT-MAXFP.                                   
011000     WRITE PRINT-LINE FROM PRINT-MULNBR.                                  
011100     WRITE PRINT-LINE FROM PRINT-DIVNBR.                                  
011200     WRITE PRINT-LINE FROM PRINT-SEEDNBR.                                 
011300     WRITE PRINT-LINE FROM PRINT-FPANSWER.                                
011400     WRITE PRINT-LINE FROM PRINT-FP231-1.                                 
011500     WRITE PRINT-LINE FROM PRINT-FP9.                                     
011600     WRITE PRINT-LINE FROM PRINT-FP1E9.                                   
011700     WRITE PRINT-LINE FROM PRINT-FP2700.                                  
011800     CLOSE PRINT-OUT.                                                     
011900     GOBACK.                                                              

