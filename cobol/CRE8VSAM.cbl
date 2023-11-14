000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID. CRE8VSAM.                                                    
000300 AUTHOR. R THORNTON.                                                      
000400 DATE-WRITTEN.  OCT 1992.                                                 
000500 DATE-COMPILED.                                                           
000510*****************************************************************         
000600*REMARKS. WRITES DATA TO AN EMPTY VSAM DATASET.                 *         
000630*****************************************************************         
000700 ENVIRONMENT DIVISION.                                                    
000800 INPUT-OUTPUT SECTION.                                                    
000810*                                                                         
000900 FILE-CONTROL.                                                            
001000     SELECT VSAM-FILE ASSIGN TO VSAM01                                    
001100            ORGANIZATION IS INDEXED                                       
001110            ACCESS IS SEQUENTIAL                                          
001120            RECORD KEY IS VSAM-KEY                                        
001130            FILE STATUS IS FILE-STATUS.                                   
001131     SELECT SEQNTL-FILE ASSIGN TO SEQNTL01.                               
001140*                                                                         
001200 DATA DIVISION.                                                           
001300 FILE SECTION.                                                            
001310*                                                                         
001400 FD  VSAM-FILE                                                            
001500     LABEL RECORDS ARE STANDARD.                                          
001900 01  VSAM-RECORD.                                                         
001910     05  VSAM-KEY                     PIC X(8).                           
001920     05  VSAM-DATA                    PIC X(72).                          
001921*                                                                         
001930 FD  SEQNTL-FILE                                                          
001931     BLOCK CONTAINS 0 RECORDS                                             
001940     LABEL RECORDS ARE STANDARD.                                          
001950 01  SEQNTL-RECORD.                                                       
001960     05  SEQNTL-KEY                   PIC X(8).                           
001970     05  SEQNTL-DATA                  PIC X(72).                          
003000*                                                                         
003100 WORKING-STORAGE SECTION.                                                 
003200 77  FILLER                           PIC X(44)                           
003300       VALUE 'CRE8VSAM WORKING STORAGE SECTION STARTS HERE'.              
003400*                                                                         
003500 01  SWITCHES.                                                            
004010     05  FILE-STATUS                  PIC XX.                             
004030         88  SUCCESSFUL-COMPLETION    VALUE '00'.                         
004050         88  NO-RECORD-FOUND          VALUE '23'.                         
004060     05  EOF-SWITCH                   PIC X  VALUE 'N'.                   
004070         88  NO-MORE-RECORDS          VALUE 'Y'.                          
004090         88  MORE-RECORDS             VALUE 'N'.                          
004300*                                                                         
004400 01  COUNTERS.                                                            
004500     05  TOTAL-READ                   PIC S9(9) COMP-3 VALUE +0.          
007780*                                                                         
007800 PROCEDURE DIVISION.                                                      
007900*                                                                         
008000 A000-MAINLINE.                                                           
008100     PERFORM B010-INITIALIZE THRU B010-EXIT.                              
008200     PERFORM C010-PROCESS THRU C010-EXIT                                  
008300         UNTIL NO-MORE-RECORDS.                                           
008310     PERFORM Y010-TERMINATE THRU Y010-EXIT.                               
008400     GOBACK.                                                              
008410*                                                                         
008420 B010-INITIALIZE.                                                         
008430     OPEN INPUT SEQNTL-FILE, OUTPUT VSAM-FILE.                            
008440 B010-EXIT.                                                               
008450     EXIT.                                                                
008500*                                                                         
008510 C010-PROCESS.                                                            
008511     PERFORM J010-READ-FILE THRU J010-EXIT.                               
008512     IF MORE-RECORDS                                                      
008514         MOVE SEQNTL-RECORD TO VSAM-RECORD                                
008515         PERFORM K010-WRITE-FILE THRU K010-EXIT.                          
008524 C010-EXIT.                                                               
008530     EXIT.                                                                
010800*                                                                         
010900 J010-READ-FILE.                                                          
010901     READ SEQNTL-FILE  AT END MOVE 'Y' TO EOF-SWITCH.                     
010902     IF MORE-RECORDS ADD 1 TO TOTAL-READ.                                 
011400 J010-EXIT.                                                               
011500     EXIT.                                                                
011510*                                                                         
011520 K010-WRITE-FILE.                                                         
011530     WRITE VSAM-RECORD.                                                   
011540     IF SUCCESSFUL-COMPLETION                                             
011550         NEXT SENTENCE                                                    
011560     ELSE                                                                 
011570         DISPLAY 'CRE8VSAM: WRITE FAILED FOR VSAM01 FILE'                 
011571         PERFORM Z010-ERRORS THRU Z010-EXIT.                              
011590 K010-EXIT.                                                               
011591     EXIT.                                                                
011600*                                                                         
011620 Y010-TERMINATE.                                                          
011621     DISPLAY TOTAL-READ ' RECORDS PROCESSED'.                             
011630     CLOSE VSAM-FILE, SEQNTL-FILE.                                        
011631     IF SUCCESSFUL-COMPLETION                                             
011632         NEXT SENTENCE                                                    
011633     ELSE                                                                 
011634         DISPLAY 'CRE8VSAM: CLOSE FAILED FOR VSAM01 FILE'                 
011635         PERFORM Z010-ERRORS THRU Z010-EXIT.                              
011640 Y010-EXIT.                                                               
011650     EXIT.                                                                
011660*                                                                         
011700 Z010-ERRORS.                                                             
011800     DISPLAY 'CRE8VSAM: VSAM01 FILE STATUS IS: ' FILE-STATUS.             
011810         CALL 'COBABEND'.                                                 
012000 Z010-EXIT.                                                               
012100     EXIT.                                                                
  