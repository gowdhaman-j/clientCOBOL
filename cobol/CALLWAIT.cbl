000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID.    CALLWAIT.                                                 
000300 AUTHOR. R THORNTON                                                       
000400*REMARKS. USED TO TEST CALLS TO THE WAITASEC SUBROUTINE.                  
000500 ENVIRONMENT DIVISION.                                                    
000600 CONFIGURATION SECTION.                                                   
000700 INPUT-OUTPUT SECTION.                                                    
000710 FILE-CONTROL.                                                            
000800 DATA DIVISION.                                                           
000810 FILE SECTION.                                                            
000900 WORKING-STORAGE SECTION.                                                 
001000 01 WAIT-TIME.                                                            
001100    05 WAIT-HOURS            PIC 99 VALUE 00.                             
001200    05 WAIT-MINUTES          PIC 99 VALUE 00.                             
001300    05 WAIT-SECONDS          PIC 99 VALUE 05.                             
001400    05 WAIT-HUNDREDTHS       PIC 99 VALUE 00.                             
001500 01 START-TIME.                                                           
001600    05 START-HOURS           PIC 99 VALUE 00.                             
001601    05 START-MINUTES         PIC 99 VALUE 00.                             
001602    05 START-SECONDS         PIC 99 VALUE 05.                             
001603    05 START-HUNDREDTHS      PIC 99 VALUE 00.                             
001604 01 END-TIME.                                                             
001605    05 END-HOURS             PIC 99 VALUE 00.                             
001606    05 END-MINUTES           PIC 99 VALUE 00.                             
001607    05 END-SECONDS           PIC 99 VALUE 05.                             
001608    05 END-HUNDREDTHS        PIC 99 VALUE 00.                             
001610 01 MESSAGE-1.                                                            
001620    05  FILLER               PIC X(17) VALUE "ISSUED WAIT FOR: ".         
001630    05  HOURS1               PIC 99.                                      
001631    05  FILLER               PIC X VALUE ":".                             
001632    05  MINUTES1             PIC 99.                                      
001633    05  FILLER               PIC X VALUE ":".                             
001634    05  SECONDS1             PIC 99.                                      
001635    05  FILLER               PIC X VALUE ".".                             
001636    05  HUNDREDTHS1          PIC 99.                                      
001640 01 MESSAGE-2.                                                            
001650    05  FILLER               PIC X(17) VALUE "WAIT STARTED AT: ".         
001651    05  HOURS2               PIC 99.                                      
001652    05  FILLER               PIC X VALUE ":".                             
001653    05  MINUTES2             PIC 99.                                      
001654    05  FILLER               PIC X VALUE ":".                             
001655    05  SECONDS2             PIC 99.                                      
001656    05  FILLER               PIC X VALUE ".".                             
001657    05  HUNDREDTHS2          PIC 99.                                      
001670 01 MESSAGE-3.                                                            
001680    05  FILLER               PIC X(15) VALUE "WAIT ENDED AT: ".           
001681    05  HOURS3               PIC 99.                                      
001682    05  FILLER               PIC X VALUE ":".                             
001683    05  MINUTES3             PIC 99.                                      
001684    05  FILLER               PIC X VALUE ":".                             
001685    05  SECONDS3             PIC 99.                                      
001686    05  FILLER               PIC X VALUE ".".                             
001687    05  HUNDREDTHS3          PIC 99.                                      
001691                                                                          
001692 LINKAGE SECTION.                                                         
001693 01  PARM-FIELD.                                                          
001694     05 PARM-LENGTH          PIC S9(4) COMP.                              
001695     05 PARM-DATA            PIC 9(8).                                    
001696                                                                          
001700 PROCEDURE DIVISION USING PARM-FIELD.                                     
001710     MOVE PARM-DATA TO WAIT-TIME.                                         
001800     ACCEPT START-TIME FROM TIME.                                         
002900     CALL "WAITASEC" USING WAIT-TIME.                                     
002910     ACCEPT END-TIME FROM TIME.                                           
002920     MOVE WAIT-HOURS TO HOURS1.                                           
002930     MOVE WAIT-MINUTES TO MINUTES1.                                       
002940     MOVE WAIT-SECONDS TO SECONDS1.                                       
002950     MOVE WAIT-HUNDREDTHS TO HUNDREDTHS1.                                 
002960     MOVE START-HOURS TO HOURS2.                                          
002970     MOVE START-MINUTES TO MINUTES2.                                      
002980     MOVE START-SECONDS TO SECONDS2.                                      
002990     MOVE START-HUNDREDTHS TO HUNDREDTHS2.                                
003000     MOVE END-HOURS TO HOURS3.                                            
003100     MOVE END-MINUTES TO MINUTES3.                                        
003101     MOVE END-SECONDS TO SECONDS3.                                        
003102     MOVE WAIT-HUNDREDTHS TO HUNDREDTHS3.                                 
003110     DISPLAY MESSAGE-1.                                                   
003130     DISPLAY MESSAGE-2.                                                   
003150     DISPLAY MESSAGE-3.                                                   
003200     GOBACK.                                                              
  