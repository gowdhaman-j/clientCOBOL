000100 ID DIVISION.                                                             
000200 PROGRAM-ID. POINTER3.                                                    
000300 ENVIRONMENT DIVISION.                                                    
000400 INPUT-OUTPUT SECTION.                                                    
000500 FILE-CONTROL.                                                            
000600 DATA DIVISION.                                                           
000700 FILE SECTION.                                                            
000800 WORKING-STORAGE SECTION.                                                 
000900 01  TCB-ADDRESS-POINTER.                                                 
001000     05  TCB-ADDR-POINTER        POINTER.                                 
001100 01  TIOT-SEG-POINT.                                                      
001200     05  TIOT-SEG-POINTER        POINTER.                                 
001300     05  TIOT-SEG-PNT            REDEFINES TIOT-SEG-POINTER               
001400                                 PIC S9(9) COMP.                          
001500 01  JFCB-POINT.                                                          
001600     05  JFCB-POINTER            POINTER.                                 
001700     05  JFCB-POINT-RED          REDEFINES JFCB-POINTER.                  
001800         10  FILLER              PIC X.                                   
001900         10  JFCB-LOW-3          PIC X(3).                                
002000 LINKAGE SECTION.                                                         
002100 01  TCB-POINTER                 POINTER.                                 
002200 01  TCB.                                                                 
002300     05  FILLER                  PIC X(12).                               
002400     05  TIOT-POINTER            POINTER.                                 
002500 01  TIOT-START                  PIC X(24).                               
002600 01  TIOT-SEG.                                                            
002700     05  TIO-LEN                 PIC X.                                   
002800     05  FILLER                  PIC X(3).                                
002900     05  DD-NAME                 PIC X(8).                                
003000     05  JFCB-ADDR               PIC X(3).                                
003100 01  JFCB.                                                                
003200     05  FILLER                  PIC X(16).                               
003300     05  DS-NAME                 PIC X(44).                               
003400 PROCEDURE DIVISION.                                                      
003500     MOVE LOW-VALUES TO JFCB-POINT.                                       
003600     MOVE X'0000021C' TO TCB-ADDRESS-POINTER.                             
003700     SET ADDRESS OF TCB-POINTER TO TCB-ADDR-POINTER.                      
003800     SET ADDRESS OF TCB TO TCB-POINTER.                                   
003900     SET ADDRESS OF TIOT-START TO TIOT-POINTER.                           
004000     SET TIOT-SEG-POINTER TO TIOT-POINTER                                 
004100     ADD 24 TO TIOT-SEG-PNT.                                              
004200     SET ADDRESS OF TIOT-SEG TO TIOT-SEG-POINTER.                         
004300     PERFORM UNTIL TIO-LEN = LOW-VALUES                                   
004400         DISPLAY DD-NAME                                                  
004500         MOVE JFCB-ADDR TO JFCB-LOW-3                                     
004600         SET ADDRESS OF JFCB TO JFCB-POINTER                              
004700         DISPLAY DS-NAME                                                  
004800         ADD 20 TO TIOT-SEG-PNT                                           
004900         SET ADDRESS OF TIOT-SEG TO TIOT-SEG-POINTER                      
005000     END-PERFORM.                                                         
005100     GOBACK.                                                              

