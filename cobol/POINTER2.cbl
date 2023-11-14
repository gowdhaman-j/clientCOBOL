000100 ID DIVISION.                                                             
000200 PROGRAM-ID. POINTER2.                                                    
000300 ENVIRONMENT DIVISION.                                                    
000400 INPUT-OUTPUT SECTION.                                                    
000500 FILE-CONTROL.                                                            
000600 DATA DIVISION.                                                           
000700 FILE SECTION.                                                            
000800 WORKING-STORAGE SECTION.                                                 
000900 01  CVT-ADDRESS-POINTER.                                                 
001000     05  CVT-ADDR-POINTER        POINTER.                                 
001100 LINKAGE SECTION.                                                         
001200 01  CVT-POINTER                 POINTER.                                 
001300 01  CVT.                                                                 
001400     05  FILLER                  PIC X(296).                              
001500     05  JESCT-POINTER           POINTER.                                 
001600 01  JESCT.                                                               
001700     05  FILLER                  PIC X(24).                               
001800     05  JESSSCVT-POINTER        POINTER.                                 
001900 01  SSCVT.                                                               
002000     05  EYE-CATCHER             PIC X(4).                                
002100     05  NEXT-SSCVT              POINTER.                                 
002200     05  SUBSYSTEM-NAME          PIC X(4).                                
002300 PROCEDURE DIVISION.                                                      
002400     MOVE X'00000010' TO CVT-ADDRESS-POINTER.                             
002500     SET ADDRESS OF CVT-POINTER TO CVT-ADDR-POINTER.                      
002600     SET ADDRESS OF CVT TO CVT-POINTER.                                   
002700     SET ADDRESS OF JESCT TO JESCT-POINTER.                               
002800     SET ADDRESS OF SSCVT TO JESSSCVT-POINTER.                            
002900     DISPLAY SUBSYSTEM-NAME.                                              
003000     PERFORM UNTIL NEXT-SSCVT = NULL                                      
003100         SET ADDRESS OF SSCVT TO NEXT-SSCVT                               
003200         DISPLAY SUBSYSTEM-NAME                                           
003300     END-PERFORM.                                                         
003400     GOBACK.                                                              
  