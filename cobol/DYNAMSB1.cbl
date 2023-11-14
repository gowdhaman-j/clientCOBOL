000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID. DYNAMSB1.                                                    
000300 ENVIRONMENT DIVISION.                                                    
000400 INPUT-OUTPUT SECTION.                                                    
000500*                                                                         
000600 FILE-CONTROL.                                                            
000700 DATA DIVISION.                                                           
000800 FILE SECTION.                                                            
000900*                                                                         
001000 WORKING-STORAGE SECTION.                                                 
001100 77   FILLER         PIC X(35) VALUE 'START OF DYNAMSB1 W.S.'.            
001200 01   TOTAL-COUNT          PIC S9(5) COMP-3 VALUE ZERO.                   
001300*                                                                         
001400 LINKAGE SECTION.                                                         
001500 01   PASS-BACK            PIC S9(5) COMP-3.                              
001600*                                                                         
001700 PROCEDURE DIVISION USING PASS-BACK.                                      
001800*                                                                         
001900 START-DYNAMSB1.                                                          
002000      ADD 1 TO TOTAL-COUNT.                                               
002100      MOVE TOTAL-COUNT TO PASS-BACK.                                      
002200      GOBACK.                                                             
  