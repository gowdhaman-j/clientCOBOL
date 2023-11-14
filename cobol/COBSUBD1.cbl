000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID.    COBSUBD1.                                                 
000300 AUTHOR. R THORNTON                                                       
000400*REMARKS. COBSUBD1 IS USED TO TEST VARIOUS STATIC AND DYNAMIC             
000411*         CALLS IN AN ENCLAVE THAT HAS AN ASSEMBLER MAIN PROGRAM          
000420*         WITH NO CALL TO ILBOSTP0 NOR TO IGZERRE.                        
000430*         THIS DYNAMICALLY CALLED SUBROUTINE RECEIVES THREE               
000431*         PARAMETERS, (1) AN 8-CHARACTER STRING CONTAINING                
000440*         'COBSUBD1', (2) A 2-BYTE PACKED DECIMAL NUMBER, AND             
000450*         (3) A BINARY HALFWORD.                                          
000460*         A DISPLAY IS DONE TO SHOW THE CONTENTOF THE PARAMETERS          
000470*         RECEIVED.                                                       
000480*         CALLS ARE MADE TO COBSUBS2 AND COBSUBD2                         
000500 ENVIRONMENT DIVISION.                                                    
000600 CONFIGURATION SECTION.                                                   
000700 INPUT-OUTPUT SECTION.                                                    
000710 FILE-CONTROL.                                                            
000800 DATA DIVISION.                                                           
000900                                                                          
000910 WORKING-STORAGE SECTION.                                                 
001000 77  FILLER PIC X(36)  VALUE                                              
001100     'COBSUBD1 WORKING STORAGE BEGINS HERE'.                              
001110 01  MISC-AREAS.                                                          
001200     05  SUBRTN-NAME-S       PIC X(8) VALUE 'COBSUBS2'.                   
001300     05  SUBRTN-NAME-D       PIC X(8) VALUE 'COBSUBD2'.                   
001301     05  PACK200             PIC S9(3) COMP-3 VALUE +200.                 
001302     05  BIN200              PIC S9(4) COMP VALUE +200.                   
001310                                                                          
001400 LINKAGE SECTION.                                                         
001500 01  PARM-STRING             PIC X(8).                                    
001510 01  PARM-PACKED             PIC S9(3) COMP-3.                            
001520 01  PARM-BINARY             PIC S9(4) COMP.                              
001600                                                                          
001700 PROCEDURE DIVISION USING PARM-STRING                                     
001710                          PARM-PACKED                                     
001720                          PARM-BINARY.                                    
002800 MAINLINE-PROCESSING.                                                     
002900     DISPLAY 'COBSUBD1 RECEIVED: PARM1='                                  
002910             PARM-STRING ', PARM2='                                       
003000             PARM-PACKED, ', PARM3='                                      
003001             PARM-BINARY.                                                 
003002     CALL 'COBSUBS2' USING SUBRTN-NAME-S, PACK200, BIN200.                
003003     CALL SUBRTN-NAME-D USING SUBRTN-NAME-D, PACK200, BIN200.             
003010     GOBACK.                                                              
