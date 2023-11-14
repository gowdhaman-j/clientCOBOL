000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID.    ABEND.                                                    
000300 AUTHOR. R THORNTON'r PROGRAM                                             
000310 DATE-WRITTEN. MAY, 2000.                                                 
000320 DATE-COMPILED.                                                           
000400*REMARKS. THE ABEND PROGRAM CREATES AN ABEND USING NATIVE COBOL           
000410*         PROGRAM STATEMENTS.                                             
000500 ENVIRONMENT DIVISION.                                                    
000600 CONFIGURATION SECTION.                                                   
000700 INPUT-OUTPUT SECTION.                                                    
000710 FILE-CONTROL.                                                            
000800 DATA DIVISION.                                                           
000810 FILE SECTION.                                                            
000910 WORKING-STORAGE SECTION.                                                 
001000 77  FILLER PIC X(36)  VALUE                                              
001100     'ABEND WORKING STORAGE BEGINS HERE'.                                 
001300 01  GENERAL-AREAS.                                                       
001305     05  HEX-FF              PIC X VALUE X'FF'.                           
001306     05  BAD-PACKED REDEFINES HEX-FF PIC S9 COMP-3.                       
001307     05  ZERO-DIVISOR        PIC S9 COMP-3 VALUE +0.                      
001308     05  DIVIDEND            PIC S9(5) COMP-3 VALUE +287.                 
001310     05  ILBO-ABEND-CODE     PIC S9(4) COMP VALUE +654.                   
001311     05  CEE3-ABEND-CODE     PIC S9(8) COMP VALUE +987.                   
001312     05  CEE3-CLEANUP-CODE   PIC S9(8) COMP VALUE +0.                     
001313                                                                          
001314 LINKAGE SECTION.                                                         
001315 01  PARM-FIELD.                                                          
001316     05  PARM-LENGTH         PIC S9(4) COMP.                              
001317     05  PARM-DATA           PIC X(4).                                    
001320                                                                          
001700 PROCEDURE DIVISION USING PARM-FIELD.                                     
001710     IF PARM-DATA = 'S0C7' GO TO S0C7-ABEND.                              
001800     IF PARM-DATA = 'S0CB' GO TO S0CB-ABEND.                              
001801     IF PARM-DATA = 'CEE3' GO TO CEE3-ABEND.                              
001802     IF PARM-DATA = 'ILBO' GO TO ILBO-ABEND.                              
001810     DISPLAY 'INVALID PARM ABEND, USE S0C7, S0CB, CEE3, OR ILBO'.         
001811 ILBO-ABEND.                                                              
001820     CALL 'ILBOABN0' USING ILBO-ABEND-CODE.                               
001830                                                                          
001840 CEE3-ABEND.                                                              
001850     CALL 'CEE3ABD' USING CEE3-ABEND-CODE, CEE3-CLEANUP-CODE.             
001860                                                                          
001900 S0C7-ABEND.                                                              
001910     DISPLAY 'ADDING 1 TO A HEX FF'.                                      
002000     ADD 1 TO BAD-PACKED.                                                 
002100     GOBACK.                                                              
002400                                                                          
002500 S0CB-ABEND.                                                              
002600     DISPLAY 'DIVIDING BY ZERO'.                                          
002601     DIVIDE ZERO-DIVISOR INTO DIVIDEND.                                   
002620     GOBACK.                                                              

