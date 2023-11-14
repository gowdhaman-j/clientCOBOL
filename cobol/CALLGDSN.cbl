000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID.    CALLGDCB.                                                 
000300 AUTHOR. R THORNTON                                                       
000400*REMARKS. USED TO CALL THE GETDCB SUBROUTINE FOR TESTING                  
000500 ENVIRONMENT DIVISION.                                                    
000600 CONFIGURATION SECTION.                                                   
000700 INPUT-OUTPUT SECTION.                                                    
000710 FILE-CONTROL.                                                            
000720     SELECT SAMPLE-FILE ASSIGN TO INPUT1.                                 
000800 DATA DIVISION.                                                           
000810 FILE SECTION.                                                            
000820 FD  SAMPLE-FILE                                                          
000830     BLOCK CONTAINS 0 RECORDS                                             
000840     LABEL RECORDS ARE STANDARD                                           
000850     RECORDING MODE IS F                                                  
000860     DATA RECORD IS FIXED-RECORD                                          
000870     RECORD CONTAINS 80 CHARACTERS.                                       
000880                                                                          
000890 01  FIXED-RECORD.                                                        
000891     05 DATA                               PIC X(80).                     
000892                                                                          
000900 WORKING-STORAGE SECTION.                                                 
000910                                                                          
001000 01  GETDCB-PARAMETERS.                                                 00
001100     05  GETDCB-STATUS         PIC X.                                     
001200         88 GETDCB-SUCCESSFUL  VALUE ' '.                                 
001300         88 DSORG-NOT-PS       VALUE 'X'.                                 
001400         88 FILE-NOT-OPEN      VALUE 'N'.                                 
001500     05  DATASET-NAME          PIC X(44).                                 
001600     05  VOLUME-SERIAL         PIC X(6)                                   
001610                               OCCURS 5 TIMES.                            
001620     05  RECORDING-FORMAT      PIC X(4).                                00
001630     05  LOGICAL-RECORD-LENGTH PIC 9(9).                                00
001640     05  BLOCK-SIZE            PIC 9(9).                                00
001650                                                                          
001700 PROCEDURE DIVISION.                                                      
006000     OPEN INPUT SAMPLE-FILE,                                            00
006100          OUTPUT PRINT-FILE.                                              
006400     CALL 'GETDCB' USING SAMPLE-FILE,                                   00
006500                         GETDCB-PARAMETERS.                             00
006600     GOBACK.                                                              

