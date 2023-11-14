000100 ID DIVISION.                                                             
000200 PROGRAM-ID. GETJOBN.                                                     
000201*****************************************************************         
000210*   GETJOBN LOCATES THE JOB NAME FOLLOWING THE PATH FROM THE    *       00
000220*   NEW/OLD TCB POINTERS AT ADDRESS X'21C' TO THE TCB FOR THIS  *       00
000230*   TASK AND FROM THERE TO THE TIOT. THE JOBNAME IS MOVED FROM  *       00
000240*   THE TIOT TO AN 8-BYTE FIELD PASSED BY THE CALLER.           *       00
000250*****************************************************************         
000300 ENVIRONMENT DIVISION.                                                    
000400 INPUT-OUTPUT SECTION.                                                    
000500 FILE-CONTROL.                                                            
000600 DATA DIVISION.                                                           
000700 FILE SECTION.                                                            
000800 WORKING-STORAGE SECTION.                                                 
000900 01  WS-POINTER.                                                          
001000     05  LOCATION-ZERO           POINTER.                                 
001020*                                                                         
001100 LINKAGE SECTION.                                                         
001200 01  JOBNAME-FIELD               PIC X(8).                                
001201 01  PSA.                                                                 
001202     05  FILLER                  PIC X(16).                               
001203     05  FLCCVT                  POINTER.                                 
001204 01  CVT.                                                                 
001205     05  CVTTCBP                 POINTER.                                 
001300 01  NEW-OLD-POINTERS.                                                    
001301     05  NEW-TCB-POINTER         POINTER.                                 
001302     05  OLD-TCB-POINTER         POINTER.                                 
001310 01  TCB.                                                                 
001400     05  FILLER                  PIC X(12).                               
001500     05  TCBTIOT                 POINTER.                                 
001600 01  TIOT.                                                                
001700     05  JOB-NAME                PIC X(8).                                
001800 PROCEDURE DIVISION USING JOBNAME-FIELD.                                  
001900     MOVE X'00000000' TO WS-POINTER.                                      
001901     SET ADDRESS OF PSA TO LOCATION-ZERO.                                 
001910     SET ADDRESS OF CVT TO FLCCVT.                                        
002000     SET ADDRESS OF NEW-OLD-POINTERS TO CVTTCBP.                          
002100     SET ADDRESS OF TCB TO NEW-TCB-POINTER.                               
002200     SET ADDRESS OF TIOT TO TCBTIOT.                                      
002300     MOVE JOB-NAME TO JOBNAME-FIELD.                                      
002400     GOBACK.                                                              

