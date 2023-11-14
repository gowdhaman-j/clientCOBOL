000110 IDENTIFICATION DIVISION.                                               00
000200 PROGRAM-ID.    CALLCBLT.                                               00
000300 AUTHOR. R THORNTON                                                     00
000400*REMARKS. TEST BUCKET FOR CALLING CBLTBCDP.                             00
000500 ENVIRONMENT DIVISION.                                                  00
000600 CONFIGURATION SECTION.                                                 00
000700 INPUT-OUTPUT SECTION.                                                  00
000710 FILE-CONTROL.                                                            
000720     SELECT INPUT-FILE ASSIGN TO UT-S-INPUT1.                             
000730     SELECT PRINT-FILE ASSIGN TO UT-S-PRINT1.                             
000800*                                                                       00
000900 DATA DIVISION.                                                         00
000910 FILE SECTION.                                                            
000920*                                                                         
000930 FD  INPUT-FILE                                                           
000940     RECORD CONTAINS 80 CHARACTERS                                        
000950     RECORDING MODE IS F                                                  
000960     BLOCK CONTAINS 0 RECORDS                                             
000970     LABEL RECORD IS STANDARD                                             
000980     DATA RECORD IS CONTROL-RECORD.                                       
000990*                                                                         
000991 01  CONTROL-RECORD.                                                      
000992     05  CONTROL-FUNCTION-CODE     PIC XX.                                
000993     05  FILLER                    PIC X.                                 
000994     05  CONTROL-RELATION-CODE     PIC XX.                                
000995     05  FILLER                    PIC X.                                 
000996     05  CONTROL-KEY-VALUE         PIC X(28).                             
000997     05  FILLER                    PIC X.                                 
000998     05  CONTROL-RELATION-CODE2    PIC XX.                                
000999     05  FILLER                    PIC X.                                 
001000     05  CONTROL-KEY-VALUE2        PIC X(28).                             
001001     05  FILLER                    PIC X(14).                             
001008*                                                                         
001009 FD  PRINT-FILE                                                           
001010     RECORD CONTAINS 80 CHARACTERS                                        
001011     RECORDING MODE IS F                                                  
001012     BLOCK CONTAINS 0 RECORDS                                             
001013     LABEL RECORD IS STANDARD                                             
001014     DATA RECORD IS PRINT-RECORD.                                         
001015*                                                                         
001016 01  PRINT-RECORD.                                                        
001017     05  FILLER                    PIC X(5).                              
001018     05  CARR-CTL                  PIC X.                                 
001019     05  TAB1.                                                            
001020         10  FILLER                PIC X(5).                              
001021         10  TAB2.                                                        
001022             15 FILLER             PIC X(5).                              
001023             15 TAB3               PIC X(63).                             
001024     05  FILLER                    PIC X.                                 
001025*                                                                         
001030 WORKING-STORAGE SECTION.                                               00
001040*                                                                         
001200 77  FILLER PIC X(36)  VALUE                                            00
001300     'CALLCBLT WORKING STORAGE BEGINS HERE'.                            00
001400*                                                                         
001500 01  MISCELLANEOUS-AREAS.                                               00
001600     05 BC-DBDANCHOR        PIC S9(5) COMP.                             00
001610     05 IMS-STATUS-CODE     PIC XX VALUE '  '.                          00
001620     05 CBLTBCDP-FUNCTION   PIC X(4) VALUE 'GU'.                          
001630     05 BC-DBDNAME          PIC X(8) VALUE 'BCMAST  '.                  00
001640     05 BC-RECORD-AREA      PIC X(500).                                   
001650     05 INPUT-SWITCH        PIC X VALUE 'N'.                              
001660        88 MORE-INPUT       VALUE 'N'.                                    
001661        88 END-OF-INPUT     VALUE 'Y'.                                    
001670*                                                                         
001710 COPY HP10PCB.                                                            
001720*                                                                         
001730 COPY HP10.                                                               
001740*                                                                         
001810 COPY IMSSFUN0.                                                           
001821*                                                                         
001830 01  SSA-HP10R.                                                           
001840     05 SGN-HP10R            PIC X(8) VALUE 'HP10R   '.                   
001850     05 CCA-HP10R            PIC X VALUE '*'.                             
001860     05 CCF-HP10R            PIC X VALUE '-'.                             
001870     05 BP-HP10R             PIC X VALUE '('.                             
001880     05 FLN-HP10R            PIC X(8) VALUE 'NULLKEY0'.                   
001890     05 RO-HP10R             PIC X(2) VALUE '=>'.                         
001891     05 VALUE1-HP10R         PIC X(28) VALUE SPACES.                      
001892     05 RO-AMPERSAND         PIC X VALUE ')'.                             
001893     05 FLN-HP10R            PIC X(8) VALUE 'NULLKEY0'.                   
001894     05 RO2-HP10R            PIC X(2) VALUE '=<'.                         
001895     05 VALUE2-HP10R         PIC X(28) VALUE SPACES.                      
001896     05 EP-HP10R             PIC X VALUE ')'.                             
001897*                                                                         
001900 PROCEDURE DIVISION.                                                    00
002000*                                                                         
002100 A100-EXECUTIVE-CONTROL.                                                00
002101     PERFORM B100-INITIALIZATION.                                         
002102     PERFORM C100-MAINLINE                                                
002103         UNTIL END-OF-INPUT.                                              
002104     PERFORM Z100-TERMINATION.                                            
002105     GOBACK.                                                            00
002106*                                                                         
002107 B100-INITIALIZATION.                                                     
002108     MOVE 'OP' TO CBLTBCDP-FUNCTION.                                      
002109     CALL 'CBLTBCDP' USING CBLTBCDP-FUNCTION,                           00
002110                           BC-DBDNAME,                                  00
002111                           HP10R,                                       00
002112                           BC-RECORD-AREA,                                
002113                           BC-DBDANCHOR.                                  
002114     OPEN INPUT INPUT-FILE, OUTPUT PRINT-FILE.                            
002115*                                                                         
002120 C100-MAINLINE.                                                           
002121     PERFORM P100-READ-INPUT-FILE.                                        
002122     IF MORE-INPUT                                                        
002123         PERFORM H100-CALL-CBLTBCDP                                       
002124         PERFORM L100-FORMAT-PRINTOUT.                                    
002125*                                                                         
002126 H100-CALL-CBLTBCDP.                                                      
002127     MOVE CONTROL-FUNCTION-CODE TO CBLTBCDP-FUNCTION                      
002128     MOVE CONTROL-RELATION-CODE TO RO-HP10R                               
002129     MOVE CONTROL-KEY-VALUE TO VALUE1-HP10R                               
002130     MOVE CONTROL-RELATION-CODE2 TO RO2-HP10R                             
002131     MOVE CONTROL-KEY-VALUE2 TO VALUE2-HP10R                              
002140     IF CONTROL-RELATION-CODE2 IS EQUAL TO SPACES                         
002150         MOVE ')' TO RO-AMPERSAND                                         
002160     ELSE                                                                 
002170         MOVE '&' TO RO-AMPERSAND.                                        
002200     CALL 'CBLTBCDP' USING CBLTBCDP-FUNCTION,                           00
002300                     PCB-HP10,                                          00
002400                     HP10R,                                             00
002410                     SSA-HP10R,                                           
002420                     BC-DBDANCHOR.                                        
002430     MOVE STC-HP10 TO IMS-STATUS-CODE.                                    
002431*                                                                         
002432 L100-FORMAT-PRINTOUT.                                                    
002433     MOVE ' CONTROL CARD READ WAS: ' TO PRINT-RECORD.                     
002434     PERFORM T100-PRINT-A-LINE.                                           
002435     MOVE SPACES TO PRINT-RECORD.                                         
002436     MOVE CONTROL-RECORD TO TAB1.                                         
002437     PERFORM T100-PRINT-A-LINE.                                           
002438     MOVE 'SSA CONTENT USED WAS: ' TO TAB1.                               
002439     PERFORM T100-PRINT-A-LINE.                                           
002440     MOVE SPACES TO PRINT-RECORD.                                         
002441     MOVE SSA-HP10R TO TAB2.                                              
002442     PERFORM T100-PRINT-A-LINE.                                           
002443     MOVE 'STATUS CODE FROM CBLTBCDP IS: ' TO TAB1.                       
002444     PERFORM T100-PRINT-A-LINE.                                           
002445     MOVE SPACES TO PRINT-RECORD.                                         
002446     MOVE STC-HP10 TO TAB2.                                               
002447     PERFORM T100-PRINT-A-LINE.                                           
002448     MOVE 'FIRST 74 BYTES OF RECORD RETURNED IS: ' TO TAB1.               
002449     PERFORM T100-PRINT-A-LINE.                                           
002450     MOVE SPACES TO PRINT-RECORD.                                         
002451     MOVE HP10R TO TAB2.                                                  
002452     PERFORM T100-PRINT-A-LINE.                                           
002453     MOVE SPACES TO PRINT-RECORD.                                         
002455     PERFORM T100-PRINT-A-LINE.                                           
002456*                                                                         
002457 P100-READ-INPUT-FILE.                                                    
002458     READ INPUT-FILE                                                      
002459         AT END MOVE 'Y' TO INPUT-SWITCH.                                 
002460*                                                                         
002461 T100-PRINT-A-LINE.                                                       
002462     WRITE PRINT-RECORD AFTER ADVANCING 1 LINE.                           
002463*                                                                         
002464 Z100-TERMINATION.                                                        
002470     CLOSE INPUT-FILE, PRINT-FILE.                                        
  