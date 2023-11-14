000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID.    CALLMLCK.                                                 
000300 AUTHOR. R THORNTON                                                       
000400*REMARKS. TEST BUCKET FOR THE MLNKCSUM SUBROUTINE.                        
000500 ENVIRONMENT DIVISION.                                                    
000600 CONFIGURATION SECTION.                                                   
000700 INPUT-OUTPUT SECTION.                                                    
000800 FILE-CONTROL.                                                            
000900     SELECT DATA-FILE ASSIGN TO INPUT1.                                   
000910     SELECT PRINT-FILE ASSIGN TO PRINT1.                                  
001000 DATA DIVISION.                                                           
001100 FILE SECTION.                                                            
001200 FD  DATA-FILE                                                            
001300     RECORDING MODE IS F                                                  
001400     LABEL RECORDS ARE STANDARD.                                          
001500 01  INPUT-RECORD.                                                      10
001510     05  DATA-AREA-LENGTH        PIC X(002).                              
001520     05  DATA-AREA               PIC X(078).                              
001600                                                                          
001610 FD  PRINT-FILE                                                           
001620     RECORDING MODE IS F                                                  
001630     LABEL RECORDS ARE STANDARD.                                          
001640 01  PRINT-RECORD            PIC X(80).                                 10
001650                                                                          
001700 WORKING-STORAGE SECTION.                                                 
001800 77  FILLER PIC X(36)  VALUE                                              
001900     "CALLMLCK WORKING STORAGE BEGINS HERE".                              
002000                                                                          
002100 01  MISCELLANEOUS-DATA.                                                  
002110     05  DATA-AREA-BINARY-LENGTH PIC S9(004) COMP.                        
002120                                                                          
002130 01  DATA-AREA-CHECKSUM-RECORD.                                           
002140     05  FILLER                  PIC X(011) VALUE "CHECKSUM = ".          
002150     05  DATA-AREA-CHECKSUM      PIC X(002).                              
002160     05  FILLER                  PIC X(067) VALUE SPACES.                 
002170                                                                          
002180 01  LONG-CHECK.                                                          
002190     05  LONG-DATA-AREA.                                                  
002200         10  FILL00  PIC X(16) VALUE "0201899999999000".                  
002210         10  FILL01  PIC X(16) VALUE "0000008999999988".                  
002300         10  FILL02  PIC X(16) VALUE "9999999809502271".                  
002400         10  FILL03  PIC X(16) VALUE "43521           ".                  
002500         10  FILL04  PIC X(16) VALUE "                ".                  
002600         10  FILL05  PIC X(16) VALUE "             000".                  
002700         10  FILL06  PIC X(16) VALUE "0000000000      ".                  
002800         10  FILL07  PIC X(16) VALUE "      0000000000".                  
002900         10  FILL08  PIC X(16) VALUE "000000      0000".                  
003000         10  FILL09  PIC X(16) VALUE "000             ".                  
003100         10  FILL0A  PIC X(16) VALUE "                ".                  
003200         10  FILL0B  PIC X(16) VALUE "      00        ".                  
003300         10  FILL0C  PIC X(16) VALUE "                ".                  
003400         10  FILL0D  PIC X(16) VALUE "      0000000000".                  
003500         10  FILL0E  PIC X(16) VALUE "0000            ".                  
003600         10  FILL0F  PIC X(14) VALUE "              ".                    
003610     05  LONG-DATA-LENGTH     PIC S9(4) COMP VALUE +252.                  
003620     05  LONG-DATA-CHECKSUM   PIC XX.                                     
005300                                                                          
005400 PROCEDURE DIVISION.                                                      
005700     OPEN OUTPUT PRINT-FILE                                               
005710          INPUT  DATA-FILE.                                               
005711                                                                          
005720 MAINLINE-ROUTINE.                                                        
005730     READ DATA-FILE                                                       
005731         AT END GO TO END-OF-JOB.                                         
005740     MOVE INPUT-RECORD TO PRINT-RECORD.                                   
005750     WRITE PRINT-RECORD.                                                  
005760     MOVE DATA-AREA-LENGTH TO DATA-AREA-BINARY-LENGTH.                    
005770     CALL "MLNKCSUM" USING DATA-AREA,                                     
005780                           DATA-AREA-BINARY-LENGTH                        
005790                           DATA-AREA-CHECKSUM.                            
005791     MOVE DATA-AREA-CHECKSUM-RECORD TO PRINT-RECORD.                      
005793     WRITE PRINT-RECORD.                                                  
005794     GO TO MAINLINE-ROUTINE.                                              
005795                                                                          
005796 END-OF-JOB.                                                              
005797     CALL "MLNKCSUM" USING LONG-DATA-AREA,                                
005799                           LONG-DATA-LENGTH,                              
005900                           DATA-AREA-CHECKSUM.                            
006000     MOVE DATA-AREA-CHECKSUM-RECORD TO PRINT-RECORD.                      
006100     WRITE PRINT-RECORD.                                                  
007400     CLOSE PRINT-FILE.                                                    
007410     CLOSE DATA-FILE.                                                     
007500     GOBACK.                                                              
  