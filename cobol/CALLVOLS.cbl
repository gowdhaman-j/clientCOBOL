000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID.    CALLVOLS.                                                 
000300 AUTHOR. R THORNTON                                                       
000400*REMARKS. TESTS CALLS TO THE DSNVOLS SUBROUTINE.                          
000500 ENVIRONMENT DIVISION.                                                    
000600 CONFIGURATION SECTION.                                                   
000700 INPUT-OUTPUT SECTION.                                                    
000710 FILE-CONTROL.                                                            
000720     SELECT INPUT-FILE ASSIGN TO INPUT1.                                  
000740     SELECT PRINT-FILE ASSIGN TO PRINT1.                                  
000800 DATA DIVISION.                                                           
000810 FILE SECTION.                                                            
000820 FD  INPUT-FILE                                                           
000821     BLOCK CONTAINS 0 RECORDS                                             
000822     RECORD CONTAINS 80 CHARACTERS                                        
000823     RECORDING MODE IS F                                                  
000830     LABEL RECORDS ARE STANDARD.                                          
000840 01  INPUT-RECORD.                                                      10
000850   05  DSNAME                PIC X(44).                                 20
000860   05  FILLER                PIC X(36).                                 30
000874                                                                          
000889 FD  PRINT-FILE                                                           
000890     BLOCK CONTAINS 0 RECORDS                                             
000891     RECORD CONTAINS 133 CHARACTERS                                       
000892     RECORDING MODE IS F                                                  
000893     LABEL RECORDS ARE STANDARD.                                          
000894 01  PRINT-RECORD.                                                      10
000895   05  LINE-SPACING          PIC X.                                     20
000896   05  PRT-TEXT              PIC X(132).                                20
000900                                                                          
000910 WORKING-STORAGE SECTION.                                                 
001000 77  FILLER PIC X(36)  VALUE                                              
001100     'CALLVOLS WORKING STORAGE BEGINS HERE'.                              
001200                                                                          
001201 01  SWITCHES.                                                            
001210     05  END-OF-INPUT-SWITCH PIC X VALUE 'N'.                             
001220         88  END-OF-INPUT-DATA     VALUE 'Y'.                             
001221         88 MORE-DATA-TO-PROCESS   VALUE 'N'.                             
001230                                                                          
001300 01  VOLSER-RETURN-AREA.                                                  
001301     05  FILLER              PIC X(6).                                    
001302         88  DSNVOLS-ERROR   VALUE HIGH-VALUES.                           
001303     05  IKJEHCIR-RET-CODE   PIC S9(8) COMP.                              
001304     05  LOCATE-RET-CODE     PIC X.                                       
001305     05  ERROR-DESCRIP       PIC X(120).                                  
001306     05  FILLER              PIC X(1405).                                 
001307 01  VOLSER-TABLE REDEFINES VOLSER-RETURN-AREA.                           
001308     05  VOLSER              PIC X(6)                                     
001309                             OCCURS 256 TIMES                             
001310                             INDEXED BY VOLNO.                            
001311         88  NO-MORE-VOLSERS VALUE HIGH-VALUES.                           
001320                                                                          
001700 PROCEDURE DIVISION.                                                      
001800                                                                          
001900 A100-EXECUTIVE-CONTROL.                                                  
001910     OPEN INPUT INPUT-FILE, OUTPUT PRINT-FILE.                            
002100     PERFORM B100-MAINLINE-PROCESSING UNTIL END-OF-INPUT-DATA.            
002200     PERFORM Z100-END-OF-PROCESSING.                                      
002300     GOBACK.                                                              
002400                                                                          
002800 B100-MAINLINE-PROCESSING.                                                
002900     PERFORM C100-READ-INPUT-FILE THRU C100-EXIT.                         
002910     IF MORE-DATA-TO-PROCESS                                              
003000        PERFORM D100-PROCESS-THE-RECORD THRU D100-EXIT.                   
003010                                                                          
003020 C100-READ-INPUT-FILE.                                                    
003030     READ INPUT-FILE                                                      
003040         AT END MOVE 'Y' TO END-OF-INPUT-SWITCH.                          
003050 C100-EXIT. EXIT.                                                         
003051                                                                          
003052 D100-PROCESS-THE-RECORD.                                                 
003053     CALL 'DSNVOLS' USING DSNAME, VOLSER-RETURN-AREA.                     
003054     IF DSNVOLS-ERROR                                                     
003055         PERFORM E100-DSNVOLS-ERROR THRU E100-EXIT                        
003056     ELSE                                                                 
003057         PERFORM F100-DSNVOLS-SUCCESSFUL THRU F100-EXIT.                  
003059 D100-EXIT. EXIT.                                                         
003060                                                                          
003061 E100-DSNVOLS-ERROR.                                                      
003062     MOVE DSNAME TO PRT-TEXT.                                             
003063     PERFORM Y100-PRINT-A-LINE THRU Y100-EXIT.                            
003064     MOVE 'DSNVOLS RETURNED ERROR MESSAGE: ' TO PRT-TEXT.                 
003065     PERFORM Y100-PRINT-A-LINE THRU Y100-EXIT.                            
003066     MOVE ERROR-DESCRIP TO PRT-TEXT.                                      
003067     PERFORM Y100-PRINT-A-LINE THRU Y100-EXIT.                            
003068     PERFORM Y100-PRINT-A-LINE THRU Y100-EXIT.                            
003069 E100-EXIT. EXIT.                                                         
003070                                                                          
003071 F100-DSNVOLS-SUCCESSFUL.                                                 
003072     MOVE DSNAME TO PRT-TEXT.                                             
003073     PERFORM Y100-PRINT-A-LINE THRU Y100-EXIT.                            
003074     SET VOLNO TO 1.                                                      
003075     PERFORM G100-PRINT-VOLSER VARYING VOLNO FROM 1 BY 1                  
003076         UNTIL NO-MORE-VOLSERS (VOLNO).                                   
003077     PERFORM Y100-PRINT-A-LINE THRU Y100-EXIT.                            
003078 F100-EXIT. EXIT.                                                         
003079                                                                          
003080 G100-PRINT-VOLSER.                                                       
003081     MOVE VOLSER (VOLNO) TO PRT-TEXT.                                     
003082     IF NO-MORE-VOLSERS (VOLNO)                                           
003083         NEXT SENTENCE                                                    
003084     ELSE                                                                 
003085         PERFORM Y100-PRINT-A-LINE THRU Y100-EXIT.                        
003086 G100-EXIT. EXIT.                                                         
003090                                                                          
003097 Y100-PRINT-A-LINE.                                                       
003098     WRITE PRINT-RECORD AFTER ADVANCING 1 LINE.                           
003099     MOVE SPACES TO PRT-TEXT.                                             
003100 Y100-EXIT. EXIT.                                                         
003101                                                                          
003102 Z100-END-OF-PROCESSING.                                                  
003300     CLOSE INPUT-FILE,                                                    
003500           PRINT-FILE.                                                    
