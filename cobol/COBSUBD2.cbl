000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID.    COBSUBD2.                                                 
000300 AUTHOR. R THORNTON                                                       
000310*REMARKS. COBSUBD2 IS USED TO TEST VARIOUS STATIC AND DYNAMIC             
000320*         CALLS IN AN ENCLAVE THAT HAS AN ASSEMBLER MAIN PROGRAM          
000330*         WITH NO CALL TO ILBOSTP0 NOR TO IGZERRE.                        
000340*         THIS DYNAMICALLY CALLED SUBROUTINE RECEIVES THREE               
000350*         PARAMETERS, (1) AN 8-CHARACTER STRING CONTAINING                
000360*         'COBSUBD1', (2) A 2-BYTE PACKED DECIMAL NUMBER, AND             
000370*         (3) A BINARY HALFWORD.                                          
000380*         A DISPLAY IS DONE TO SHOW THE CONTENT OF THE PARAMETERS         
000390*         RECEIVED.                                                       
000500 ENVIRONMENT DIVISION.                                                    
000600 CONFIGURATION SECTION.                                                   
000700 INPUT-OUTPUT SECTION.                                                    
000710 FILE-CONTROL.                                                            
000800 DATA DIVISION.                                                           
000900                                                                          
000910 WORKING-STORAGE SECTION.                                                 
001000 77  FILLER PIC X(36)  VALUE                                              
001100     'COBSUBD2 WORKING STORAGE BEGINS HERE'.                              
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
002900     DISPLAY 'COBSUBD2 RECEIVED: PARM1='                                  
002910             PARM-STRING ', PARM2='                                       
003000             PARM-PACKED, ', PARM3='                                      
003001             PARM-BINARY.                                                 
003010     GOBACK.                                                              
  