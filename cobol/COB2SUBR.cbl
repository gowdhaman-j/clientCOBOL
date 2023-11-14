000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID. COB2SUBR.                                                    
000300 AUTHOR. R THORNTON.                                                      
000400 DATE-WRITTEN.  OCT 1992.                                                 
000500 DATE-COMPILED.                                                           
000510*****************************************************************         
000600*REMARKS. USED AS A COBOL II SUBROUTINE IN A RUN UNIT CONTAINING*         
000610*         A MIXTURE OF VS COBOL AND COBOL II PROGRAMS. CALLER   *         
000620*         PASSES A REQUEST CODE FOR OPEN, READ, OR CLOSE, AND   *         
000621*         A RETURN CODE FIELD. FOR READ REQUESTS, THE KEY OF THE*         
000622*         DESIRED RECORD AND AN AREA TO CONTAIN THE RECORD ARE  *         
000623*         ALSO PASSED. IF ANY UNUSUAL CONDITION OCCURS, A       *         
000624*         MESSAGE IS WRITTEN AND COBABEND IS CALLED.            *         
000630*****************************************************************         
000700 ENVIRONMENT DIVISION.                                                    
000800 INPUT-OUTPUT SECTION.                                                    
000810*                                                                         
000900 FILE-CONTROL.                                                            
001000     SELECT VSAM-FILE ASSIGN TO VSAM01                                    
001100            ORGANIZATION IS INDEXED                                       
001110            ACCESS IS RANDOM                                              
001120            RECORD KEY IS VSAM-KEY                                        
001130            FILE STATUS IS FILE-STATUS.                                   
001140*                                                                         
001200 DATA DIVISION.                                                           
001300 FILE SECTION.                                                            
001400 FD  VSAM-FILE                                                            
001500     LABEL RECORDS ARE STANDARD.                                          
001900 01  VSAM-RECORD.                                                         
001910     05  VSAM-KEY                     PIC X(8).                           
001920     05  VSAM-DATA                    PIC X(72).                          
003000*                                                                         
003100 WORKING-STORAGE SECTION.                                                 
003200 77  FILLER                           PIC X(44)                           
003300       VALUE 'COB2SUBR WORKING STORAGE SECTION STARTS HERE'.              
003400*                                                                         
003500 01  SWITCHES.                                                            
004010     05  FILE-STATUS                  PIC XX.                             
004030         88  SUCCESSFUL-COMPLETION    VALUE '00'.                         
004050         88  NO-RECORD-FOUND          VALUE '23'.                         
004300*                                                                         
004400 01  COUNTERS.                                                            
004500     05  TOTAL-READ                   PIC S9(9) COMP-3 VALUE +0.          
004700*                                                                         
004800 LINKAGE SECTION.                                                         
004900 01  REQUEST-CODE                     PIC X.                              
005000     88  OPEN-REQUEST                 VALUE 'O'.                          
005100     88  READ-REQUEST                 VALUE 'R'.                          
005200     88  CLOSE-REQUEST                VALUE 'C'.                          
005300*                                                                         
007710 01  RECORD-RETURN-AREA               PIC X(80).                          
007720*                                                                         
007730 01  RECORD-KEY-ARGUMENT              PIC X(8).                           
007740*                                                                         
007750 01  COMPLETION-CODE                  PIC X.                              
007760     88  SUCCESSFUL-OPERATION         VALUE 'Y'.                          
007770     88  RECORD-NOT-FOUND             VALUE 'N'.                          
007780*                                                                         
007800 PROCEDURE DIVISION USING REQUEST-CODE,                                   
007801                          COMPLETION-CODE,                                
007810                          RECORD-RETURN-AREA,                             
007820                          RECORD-KEY-ARGUMENT.                            
007900*                                                                         
008000 A000-MAINLINE.                                                           
008100     PERFORM B010-INITIALIZE THRU B010-EXIT.                              
008200     PERFORM C010-PROCESS THRU C010-EXIT.                                 
008400     GOBACK.                                                              
008410*                                                                         
008420 B010-INITIALIZE.                                                         
008430     MOVE 'Y' TO COMPLETION-CODE.                                         
008440 B010-EXIT.                                                               
008450     EXIT.                                                                
008500*                                                                         
008510 C010-PROCESS.                                                            
008511     IF READ-REQUEST                                                      
008512         PERFORM J010-READ-FILE THRU J010-EXIT                            
008513     ELSE IF OPEN-REQUEST                                                 
008514         PERFORM K010-OPEN-FILE THRU K010-EXIT                            
008515     ELSE IF CLOSE-REQUEST                                                
008516         PERFORM Y010-CLOSE-FILE THRU Y010-EXIT                           
008517     ELSE                                                                 
008518         DISPLAY 'COB2SUBR: INVALID REQUEST CODE FROM CALLER'             
008519         CALL 'COBABEND'.                                                 
008520 C010-EXIT.                                                               
008530     EXIT.                                                                
010800*                                                                         
010900 J010-READ-FILE.                                                          
010910     MOVE RECORD-KEY-ARGUMENT TO VSAM-KEY.                                
011000     READ VSAM-FILE RECORD INTO RECORD-RETURN-AREA.                       
011310     IF SUCCESSFUL-COMPLETION                                             
011311         ADD 1 TO TOTAL-READ                                              
011330     ELSE IF NO-RECORD-FOUND                                              
011331         MOVE 'N' TO COMPLETION-CODE                                      
011332     ELSE                                                                 
011340         DISPLAY 'COB2SUBR: READ FAILED FOR VSAM01 FILE'                  
011341         PERFORM Z010-ERRORS THRU Z010-EXIT.                              
011400 J010-EXIT.                                                               
011500     EXIT.                                                                
011510*                                                                         
011520 K010-OPEN-FILE.                                                          
011530     OPEN INPUT VSAM-FILE.                                                
011540     IF SUCCESSFUL-COMPLETION                                             
011550         NEXT SENTENCE                                                    
011560     ELSE                                                                 
011570         DISPLAY 'COB2SUBR: OPEN FAILED FOR VSAM01 FILE'                  
011571         PERFORM Z010-ERRORS THRU Z010-EXIT.                              
011590 K010-EXIT.                                                               
011591     EXIT.                                                                
011600*                                                                         
011620 Y010-CLOSE-FILE.                                                         
011630     CLOSE VSAM-FILE.                                                     
011631     IF SUCCESSFUL-COMPLETION                                             
011632         NEXT SENTENCE                                                    
011633     ELSE                                                                 
011634         DISPLAY 'COB2SUBR: CLOSE FAILED FOR VSAM01 FILE'                 
011635         PERFORM Z010-ERRORS THRU Z010-EXIT.                              
011640 Y010-EXIT.                                                               
011650     EXIT.                                                                
011660*                                                                         
011700 Z010-ERRORS.                                                             
011800     DISPLAY 'COB2SUBR: VSAM01 FILE STATUS IS: ' FILE-STATUS.             
011810         CALL 'COBABEND'.                                                 
012000 Z010-EXIT.                                                               
012100     EXIT.                                                                

