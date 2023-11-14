000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID.  SETUPBLL.                                                   
000300 AUTHOR.  R THORNTON.                                                     
000400 INSTALLATION.  BLUE CROSS AND BLUE SHIELD OF VIRGINIA.                   
000500 DATE-WRITTEN.  NOV, 1987.                                                
000600 DATE-COMPILED.                                                           
000700****************************************************************          
000800* TEST COBOL'S ABILITY TO ACCESS STORAGE IN UNUSUAL LOCATIONS, *          
000900* IN ORDER TO OBTAIN INFORMATION FROM CONTROL BLOCKS WITHOUT   *          
001000* HAVING TO RESORT TO USE OF ASSEMBLY LANGUAGE SUBROUTINES.    *          
001100*                                                              *          
001200* THIS SUBROUTINE IS CALLED BY A COBOL PROGRAM THAT DESIRES ITS*          
001300* LINKAGE SECTION BLL CELLS TO BE SET TO APPROPRIATE VALUES SO *          
001400* THAT IT CAN DIRECTLY ADDRESS SYSTEM CONTROL BLOCKS AND/OR ITS*          
001500* OWN FILE DESCRIPTIONS, ETC. THE CALLING PROGRAM PASSES:      *          
001600*   1. THE PARAGRAPH NAME WHICH IMMEDIATELY PRECEDES THE CALL  *          
001700*      TO THIS SUBROUTINE. THERE CAN BE NO OTHER STATEMENTS    *          
001800*      BETWEEN THE PARAGRAPH NAME AND THE CALL STATEMENT!      *          
001900*   2. THE NAME OF A GROUP ITEM WHICH MUST CONTAIN A SET OF    *          
002000*      8-CHARACTER FIELDS DEFINING THE DESIRED CONTENT OF THE  *          
002100*      CORRESPONDING BLL CELL. (CVT, ASCB, OUCB, TCB, OR FD).  *          
002200*      WHEN FD IS SPECIFIED, THE VALUE IS SET TO THE VALUE OF  *          
002300*      THE PARAMETER ON THA CALL STATEMENT AFTER THE LAST BLL  *          
002400*      CELL. THE LAST OF THESE FIELDS MUST CONTAIN ALL BLANKS. *          
002500*      THERE MUST BE OF ONE THESE FIELDS FOR EACH BLL PARAMETER*          
002600*      PASSED IN 3. BELOW, PLUS A BLANK ONE AT THE END.        *          
002700*   3. THE 3RD THROUGH NTH PARAMETERS ARE THE NAMES OF 01 LEVEL*          
002800*      RECORDS DEFINED IN THE LINKAGE SECTION THAT ARE TO BE   *          
002900*      INITIALIZED BY THIS SUBROUTINE. THERE MUST BE ONE LESS  *          
003000*      OF THESE THAN THERE ARE FIELDS IN THE GROUP ITEM ABOVE. *          
003100*   4. IF FD IS CODED IN ANY OF THE FIELDS IN 2. ABOVE, THERE  *          
003200*      MUST BE A PARAMETER FOLLOWING THE BLL CELLS WHICH HAS   *          
003300*      THE VALUE TO BE PLACED IN THE BLL CELL (USUALLY THIS    *          
003400*      WILL BE AN FD NAME).                                    *          
003500*                                                              *          
003600*                                                              *          
003700****************************************************************          
003800 ENVIRONMENT DIVISION.                                                    
003900 DATA DIVISION.                                                           
004000 WORKING-STORAGE SECTION.                                                 
004100 77  FILLER                     PIC X(36)  VALUE                          
004200     'SETUPBLL WORKING STORAGE BEGINS HERE'.                              
004300 01  MISCELLANEOUS-WORK.                                                  
004400     05  BLL-NUMBER             PIC S9(3) COMP-3 VALUE +1.                
004500     05  FD-NUMBER              PIC S9(3) COMP-3 VALUE +1.                
004600     05  POINTER-WORK           PIC S9(8) COMP.                           
004700     05  FILLER REDEFINES POINTER-WORK.                                   
004800         10  POINTER-HIGH-BYTE  PIC X.                                    
004900         10  POINTER-BYTE2      PIC X.                                    
005000         10  POINTER-BYTE3      PIC X.                                    
005100         10  POINTER-BYTE4      PIC X.                                    
005200 LINKAGE SECTION.                                                         
005300 01  MEMORY-BYTES.                                                        
005400     05  MEMORY-BYTE            PIC X OCCURS 4096 TIMES.                  
005500 01  REQUEST-AREA.                                                        
005600     05  BLL-VALUE              PIC X(8) OCCURS 10 TIMES.                 
005700 01  CALL-STATEMENT-AREA.                                                 
005800     05  PREFIX-INSTRUCTIONS    PIC X(32).                                
005900     05  PARAMETER-ADDRESS      OCCURS 10 TIMES                           
006000                                PIC S9(8) COMP.                           
006100 PROCEDURE DIVISION USING REQUEST-AREA, CALL-STATEMENT-AREA.              
006200****************************************************************          
006300* GET THE ADDRESS OF THE CVT INTO CVTADR. THE CVT ADDRESS IS   *          
006400* LOCATED AT ABSOLUTE MEMORY ADDRESSES 17-19 (DECIMAL).        *          
006500****************************************************************          
006600     MOVE LOW-VALUES TO POINTER-HIGH-BYTE.                                
006700     MOVE MEMORY-BYTE (18) TO POINTER-BYTE2.                              
006800     MOVE MEMORY-BYTE (19) TO POINTER-BYTE3.                              
006900     MOVE MEMORY-BYTE (20) TO POINTER-BYTE4.                              
007000     MOVE POINTER-WORK TO PARAMETER-ADDRESS (BLL-NUMBER).                 
007100     GOBACK.                                                              
  