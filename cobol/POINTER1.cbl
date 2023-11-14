000100 ID DIVISION.                                                             
000200 PROGRAM-ID. POINTER1.                                                    
000210**********************************************************************    
000220* FOLLOWING ARE VALID POINTER STATEMENTS:                            *    
000230* MOVE X'        ' TO REDEFINED-POINTER-ITEM.  (OR AS GROUP NAME)    *    
000240* MOVE LOW-VALUES TO REDEFINED-POINTER-ITEM.   (OR AS GROUP NAME)    *    
000250* SET PTR-DATA-ITEM1 TO PTR-DATA-ITEM2                               *    
000260* SET PTR-DATA-ITEM TO ADDRESS OF LINKAGE-SECTION-01.                *    
000270* SET PTR-DATA-ITEM TO NULLS.                                        *    
000280* SET ADDRESS OF LINKAGE-SECTION-01 TO POINTER-DATA-ITEM.            *    
000290* SET ADDRESS OF LINGAGE-SECTION-01 TO ADDRESS OF LINKAGE-SECTION-01 *    
000291* SET ADDRESS OF LINKAGE-SECTION-01 TO NULLS.                        *    
000292*                                                                    *    
000293* NOTICE THAT ADDRESS OF IS ONLY VALID FOR LINKAGE SECTION 01 OR 77. *    
000294**********************************************************************    
000300 ENVIRONMENT DIVISION.                                                    
000400 INPUT-OUTPUT SECTION.                                                    
000500 FILE-CONTROL.                                                            
000600 DATA DIVISION.                                                           
000700 FILE SECTION.                                                            
000800 WORKING-STORAGE SECTION.                                                 
000900 01  TCB-ADDRESS-POINTER.                                                 
001000     05  TCB-ADDR-POINTER        POINTER.                                 
001100 LINKAGE SECTION.                                                         
001200 01  TCB-POINTER                 POINTER.                                 
001300 01  TCB.                                                                 
001400     05  FILLER                  PIC X(12).                               
001500     05  TIOT-POINTER            POINTER.                                 
001600 01  TIOT.                                                                
001700     05  JOB-NAME                PIC X(8).                                
001800 PROCEDURE DIVISION.                                                      
001900     MOVE X'0000021C' TO TCB-ADDRESS-POINTER.                             
002000     SET ADDRESS OF TCB-POINTER TO TCB-ADDR-POINTER.                      
002100     SET ADDRESS OF TCB TO TCB-POINTER.                                   
002200     SET ADDRESS OF TIOT TO TIOT-POINTER.                                 
002300     DISPLAY JOB-NAME.                                                    
002400     GOBACK.                                                              
  