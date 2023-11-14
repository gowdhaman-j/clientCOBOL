000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID.  CTLBLOX2.                                                   
000300 AUTHOR.  R THORNTON.                                                     
000400 INSTALLATION.  BLUE CROSS AND BLUE SHIELD OF VIRGINIA.                   
000500 DATE-WRITTEN.  NOV, 1987.                                                
000600 DATE-COMPILED.                                                           
000700****************************************************************          
000800* TEST COBOL'S ABILITY TO ACCESS STORAGE IN UNUSUAL LOCATIONS, *          
000900* IN ORDER TO OBTAIN INFORMATION FROM CONTROL BLOCKS WITHOUT   *          
001000* HAVING TO RESORT TO USE OF ASSEMBLY LANGUAGE SUBROUTINES.    *          
001100*                                                              *          
001200* THIS VERSION ACCESSES THE CVT VIA A (COBOL) SUBROUTINE CALLED*          
001300* READCVT, RATHER THAN BY MOVING THE FIELD BYTE-BY-BYTE AS IS  *          
001400* DONE IN THE PROGRAM CTRLBLOX.                                *          
001500*                                                              *          
001600* THE FOLLOWING CHARACTERISTICS OF IBM COBOL ARE USED FOR THIS *          
001700* TECHNIQUE:                                                   *          
001800*                                                              *          
001900*   1. IN LINKAGE SECTION, BLL CELLS FOR EACH RECORD ARE SET TO*          
002000*      ZERO, AND ARE CHANGED ONLY WHEN ACCEPTED BY THE USING   *          
002100*      PARAMETER ON THE PROCEDURE DIVISION, OR AN ENTRY STATE- *          
002200*      MENT. THE RECORD MEMORY-BYTES IS NEVER RECEIVED, AND    *          
002300*      WILL THEREFORE ALWAYS BE LOCATED AT ABSOLUTE LOCATION   *          
002400*      ZERO, THE BEGINNING OF MEMORY.                          *          
002500*                                                              *          
002600*   2. SUBSCRIPT VALUES ARE NOT CHECKED FOR OUT-OF-RANGE VALUES*          
002700*      IN NORMAL OPERATION, THEREFORE ANY VALUE AT ALL CAN BE  *          
002800*      USED IN THE INDEX BYTE-NBR. BECAUSE OF THIS, ANY BYTE IN*          
002900*      MEMORY CAN BE ACCESSED BY PUTTING ITS ADDRESS IN THE    *          
003000*      INDEX FIELD BYTE-NBR, AND ACCESSING IT BY REFERRING TO  *          
003100*      MEMORY-BYTE (BYTE-NBR).                                 *          
003200*                                                              *          
003300*   3. SINCE COBOL'S SUBSCRIPT/INDEXING FACILITY REFERS TO THE *          
003400*      OCCURRENCE NUMBER OF A FIELD RELATIVE TO 1, RATHER THAN *          
003500*      TO 0 AS IS NORMALLY GIVEN IN CONTROL BLOCK BOOKS, THE   *          
003600*      SUBSCRIPT/INDEX VALUES MUST BE 1 GREATER THAN GIVEN IN  *          
003700*      THE CONTROL BLOCK BOOKS. (TO ACCESS A FIELD AT OFFSET   *          
003800*      67 RELATIVE TO ZERO, THE SUBSCRIPT OR INDEX MUST SPECIFY*          
003900*      68).                                                    *          
004000****************************************************************          
004100 ENVIRONMENT DIVISION.                                                    
004200 INPUT-OUTPUT SECTION.                                                    
004300 FILE-CONTROL.                                                            
004400     SELECT PRINT-OUT ASSIGN TO UT-S-PRINT1.                              
004500 DATA DIVISION.                                                           
004600 FILE SECTION.                                                            
004700 FD  PRINT-OUT                                                            
004800       LABEL RECORDS ARE STANDARD                                         
004900       RECORD CONTAINS 80 CHARACTERS                                      
005000       BLOCK CONTAINS 0 RECORDS                                           
005100       DATA RECORD IS PRINT-LINE.                                         
005200 01  PRINT-LINE.                                                          
005300     05  PRINT-DATE         PIC 99/999.                                   
005400     05  FILLER             PIC X(74).                                    
005500 WORKING-STORAGE SECTION.                                                 
005600 77  FILLER                     PIC X(36)  VALUE                          
005700     'CTLBLOX2 WORKING STORAGE BEGINS HERE'.                              
005800 01  MISCELLANEOUS-WORK.                                                  
005900     05  CVTADR                 PIC S9(8) COMP.                           
006000     05  POINTER-WORK           PIC S9(8) COMP.                           
006100     05  FILLER REDEFINES POINTER-WORK.                                   
006200         10  POINTER-HIGH-BYTE  PIC X.                                    
006300         10  POINTER-BYTE2      PIC X.                                    
006400         10  POINTER-BYTE3      PIC X.                                    
006500         10  POINTER-BYTE4      PIC X.                                    
006600     05  DATE-PACKED            PIC S9(7) COMP-3.                         
006700 LINKAGE SECTION.                                                         
006800 01  MEMORY-BYTES.                                                        
006900     05  MEMORY-BYTE            PIC X OCCURS 4096 TIMES                   
007000                                INDEXED BY BYTE-NBR.                      
007100 PROCEDURE DIVISION.                                                      
007200****************************************************************          
007300* INITIALIZE                                                   *          
007400****************************************************************          
007500     OPEN OUTPUT PRINT-OUT.                                               
007600     MOVE SPACES TO PRINT-LINE.                                           
007700****************************************************************          
007800* GET THE ADDRESS OF THE CVT INTO CVTADR. THE CVT ADDRESS IS   *          
007900* LOCATED AT ABSOLUTE MEMORY ADDRESSES 17-19 (DECIMAL).        *          
008000****************************************************************          
008100     MOVE LOW-VALUES TO POINTER-HIGH-BYTE.                                
008200     MOVE MEMORY-BYTE (18) TO POINTER-BYTE2.                              
008300     MOVE MEMORY-BYTE (19) TO POINTER-BYTE3.                              
008400     MOVE MEMORY-BYTE (20) TO POINTER-BYTE4.                              
008500     MOVE POINTER-WORK TO CVTADR.                                         
008600****************************************************************          
008700* GET THE SYSTEM DATE FROM THE CVT. THE CVT ADDRESS IS PASSED  *          
008800* TO THE SUBPROGRAM READCVT, WHICH RETURNS SYSTEM DATE FIELD IN*          
008900* THE DATE-PACKED FIELD. NOTE THAT THE CVT ADDRESS MUST BE     *          
009000* ADVANCED BY 1 TO ADJUST FOR COBOL'S CONVERSION FROM AN       *          
009100* OCCURRENCE NUMBER (CVTADR) TO AN OFFSET (BYTE-NBR).          *          
009200****************************************************************          
009300     SET BYTE-NBR TO CVTADR.                                              
009400     SET BYTE-NBR UP BY 1.                                                
009500     CALL 'READCVT' USING MEMORY-BYTE (BYTE-NBR),                         
009600                          DATE-PACKED.                                    
009700****************************************************************          
009800* PRINT THE SYSTEM DATE.                                       *          
009900****************************************************************          
010000     MOVE DATE-PACKED TO PRINT-DATE.                                      
010100     WRITE PRINT-LINE.                                                    
010200     CLOSE PRINT-OUT.                                                     
010300     GOBACK.                                                              
  