000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID. CALLGRPC.                                                    
000300 ENVIRONMENT DIVISION.                                                    
000400 INPUT-OUTPUT SECTION.                                                    
000500*                                                                         
000600 FILE-CONTROL.                                                            
000700 DATA DIVISION.                                                           
000800 FILE SECTION.                                                            
000900*                                                                         
001000 WORKING-STORAGE SECTION.                                                 
001100 77   FILLER         PIC X(35) VALUE 'START OF CALLGRPC W.S.'.            
001200 77   IOOP           PIC X.                                               
001300 77   GROUPNBR       PIC 9(8) COMP-3.                                     
001400 77   STAT-BYT       PIC X.                                               
001500 77   I              PIC S99 COMP.                                        
001600*                                                                         
001700 01   GRP-MAST             COPY WSGRPMST.                                 
001800*                                                                         
001900 PROCEDURE DIVISION.                                                      
002000*                                                                         
002100 START-CALLGRPC.                                                          
002200      MOVE 'O' TO IOOP.                                                   
002300      MOVE ZEROS TO GROUPNBR.                                             
002400      CALL 'READGRP' USING IOOP, GRP-MAST, GROUPNBR, STAT-BYT.            
002500      IF STAT-BYT = '0'                                                   
002600          NEXT SENTENCE                                                   
002700      ELSE                                                                
002800          DISPLAY 'ERROR OPENING GRP-MAST',                               
002900          DISPLAY 'STATUS CODE IS ' STAT-BYT,                             
003000          CALL 'COBABEND'.                                                
003100      MOVE 'N' TO IOOP.                                                   
003200      MOVE ZEROS TO GROUPNBR.                                             
003300      PERFORM 1000-READ-SEQ THRU 1000-EXIT                                
003400          VARYING I FROM 1 BY 1 UNTIL I > 5.                              
003500      MOVE 'C' TO IOOP.                                                   
003600      MOVE ZEROS TO GROUPNBR.                                             
003700      CALL 'READGRP' USING IOOP, GRP-MAST, GROUPNBR, STAT-BYT.            
003800      IF STAT-BYT = '0'                                                   
003900          NEXT SENTENCE                                                   
004000      ELSE                                                                
004100          DISPLAY 'ERROR CLOSING GRP-MAST',                               
004200          DISPLAY 'STATUS CODE IS ' STAT-BYT,                             
004300          CALL 'COBABEND'.                                                
004400      GOBACK.                                                             
004500 1000-READ-SEQ.                                                           
004600      CALL 'READGRP' USING IOOP, GRP-MAST, GROUPNBR, STAT-BYT.            
004700      IF STAT-BYT = '0'                                                   
004800          NEXT SENTENCE                                                   
004900      ELSE                                                                
005000          DISPLAY 'ERROR CLOSING GRP-MAST',                               
005100          DISPLAY 'STATUS CODE IS ' STAT-BYT,                             
005200          CALL 'COBABEND'.                                                
005300          DISPLAY 'READ RECORD NUMBER ' I.                                
005400          DISPLAY 'GRP-NAME IS ' GRP-NAME.                                
005500 1000-EXIT. EXIT.                                                         

