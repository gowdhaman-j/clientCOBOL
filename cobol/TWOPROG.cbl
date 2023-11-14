000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID. TWOPROG.                                                     
000300 ENVIRONMENT DIVISION.                                                    
000400 INPUT-OUTPUT SECTION.                                                    
000500*                                                                         
000600 FILE-CONTROL.                                                            
000700 DATA DIVISION.                                                           
000800 FILE SECTION.                                                            
000900*                                                                         
001000 WORKING-STORAGE SECTION.                                                 
001100 77   FILLER         PIC X(35) VALUE 'START OF TWOPROG W.S.'.             
001200*                                                                         
001300 LINKAGE SECTION.                                                         
001400 01   PASS-CODE            PIC 9.                                         
001500 01   TOTAL-COUNT          PIC 999.                                       
001600*                                                                         
001700 PROCEDURE DIVISION USING PASS-CODE, TOTAL-COUNT.                         
001800*                                                                         
001900 START-TWOPROG.                                                           
002000      IF PASS-CODE = 1                                                    
002100           MOVE 7 TO PASS-CODE                                            
002200      ELSE                                                                
002300           IF PASS-CODE = 2                                               
002400               MOVE 8 TO PASS-CODE                                        
002500           ELSE                                                           
002600               MOVE 9 TO PASS-CODE,                                       
002700               SUBTRACT 30 FROM TOTAL-COUNT.                              
002800      GOBACK.                                                             
  