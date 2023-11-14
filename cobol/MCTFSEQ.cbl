000010 IDENTIFICATION DIVISION.                                                 
000020     SKIP1                                                                
000030 PROGRAM-ID. MCTFSEQ.                                                     
000040     SKIP1                                                                
      *TRANSLATED BY MHTRAN-2  9/06/94                                          
000050 AUTHOR. JACK E. BOWEN, JR.                                               
000060 INSTALLATION. BLUE CROSS AND BLUE SHIELD                                 
000070 DATE-WRITTEN. AUGUST 29,1973                                             
000080 DATE-COMPILED.                                                           
Y2000+*+*************************************************************** Y2000+  
Y2000+*+  EDITED BY CGA/ARC RENOVATOR RENOALL V6.00 ON 1998-3-25 11:45  Y2000+  
Y2000+*+*************************************************************** Y2000+  
000090     SKIP2                                                                
000100*REMARKS.                                                                 
000110*        THIS SUBROUTINE READS THE MASTER CODE TABLE FILE                 
000120*        SEQUENTIALLY BEGINNING AT ANY POINT IN THE FILE.                 
000130                                                                          
000140*        FUNCTIONS - (ACCEPTABLE VALUES FOR F-R-CODE IN THE               
000150*                     CALL STATEMENT.)                                    
000160                                                                          
000170*          O     OPENS FILE                                               
000180*          S     INITIATES START VERB                                     
000190*          R     READS THE RECORD CURRENTLY POINTED TO                    
000200*                IN THE FILE                                              
000210*          C     CLOSES THE FILE                                          
000220*        RETURN CODES - (CONTENTS OF F-R-CODE ON RETURN                   
000230*                        FROM THE SUBROUTINE.)                            
000240                                                                          
000250*          0     FUNCTION WAS SUCCESSFUL                                  
000260*          1     INVALID KEY                                              
000270*          2     INVALID FUNCTION                                         
000280*          3     READ ATTEMPTED BEFORE FILE WAS OPENED                    
000290*          4     I O ERROR                                                
000300*          6     START VERB ATTEMPTED BEFORE FILE WAS                     
000310*                OPENED                                                   
000320*          7     END OF FILE WAS ENCOUNTERED                              
000330                                                                          
000340*                                                                         
000350*  INCIDENT 83-00315 - IMPLEMENTED 05/20/83 BY IWANNA ATKINS              
000360*     CHANGED FILE ACCESS FOR MASTER CODE TABLE FILE TO REMOVE            
000370*     USAGE OF THE ISAM INTERFACE WHICH CAUSED AN 03B ABEND               
000380*     WHICH RESULTED WHEN FILE WHICH IS VSAM NEEDED TO BE VERIFIED        
000390*                                                                         
000400                                                                          
000410     SKIP2                                                                
000420 ENVIRONMENT DIVISION.                                                    
000430 INPUT-OUTPUT SECTION.                                                    
000440 FILE-CONTROL.                                                            
000450*    SELECT MCTF-IN  ASSIGN TO UT-I-MCTFSEQ                               
000460     SELECT MCTF-IN  ASSIGN TO DA-MCTFSEQ                                 
000470                     ACCESS IS SEQUENTIAL                                 
000480                     ORGANIZATION IS INDEXED                              
000490*                    NOMINAL KEY IS MCTR-NOMINAL-KEY                      
000500                     RECORD KEY IS MCTRI-RECORD-KEY.                      
000510     SKIP2                                                                
000520 DATA DIVISION.                                                           
000530 FILE SECTION.                                                            
000540 FD  MCTF-IN                                                              
000550     RECORD CONTAINS 211 CHARACTERS                                       
000560     LABEL RECORDS ARE STANDARD                                           
000570     DATA RECORD IS MCTR-IN.                                              
000580 01  MCTR-IN.                                                             
000590     05  FILLER                      PIC X(001).                          
000600     05  MCTRI-RECORD-KEY            PIC X(015).                          
000610     05  FILLER                      PIC X(195).                          
000620     SKIP2                                                                
000630 WORKING-STORAGE SECTION.                                                 
000640 77  FILLER                          PIC X(35)                            
000650         VALUE  'MCTFSEQ WORKING STORAGE BEGINS HERE'.                    
000660*01  MCTR-NOMINAL-KEY                PIC X(015).                          
000670 01  WS-SWITCHES.                                                         
000680     05  OPEN-SW                     PIC X(001).                          
000690     88  OPENED   VALUE '1'.                                              
000700     05  START-SW                    PIC X(001).                          
000710     88  STARTED   VALUE  '1'.                                            
000720 01  WS-MCTR-IN,  VALUE SPACE.                                            
000730     05  MI-STATUS-CODE              PIC X(001).                          
000740     05  MI-RECORD-KEY.                                                   
000750         10  MI-TABLE-ID             PIC X(003).                          
000760         10  MI-CODE-ID              PIC X(012).                          
000770     05  MI-DATA.                                                         
000780         10  MI-CURR-EFF-DATE        PIC X(006).                          
000790         10  MI-PREV-EFF-DATE        PIC X(006).                          
000800         10  MI-TABLE-DATA           PIC X(183).                          
000810     SKIP2                                                                
000820 LINKAGE SECTION.                                                         
000830 01  SUBPGM-PARAMETERS.                                                   
000840     05  F-R-CODE                    PIC X(001).                          
000850     05  MCTF-KEY                    PIC X(015).                          
000860     05  MCTF-DATA                   PIC X(195).                          
000870     EJECT                                                                
000880 PROCEDURE DIVISION  USING SUBPGM-PARAMETERS.                             
000890     SKIP2                                                                
000900 DECLARATIVES.                                                            
000910 USER-IO-ERROR-RTN SECTION.                                               
000920     USE AFTER ERROR PROCEDURE ON MCTF-IN.                                
000930     GO TO I-O-ERROR.                                                     
000940 END DECLARATIVES.                                                        
000950     SKIP2                                                                
000960     IF F-R-CODE NOT = 'R' THEN GO TO NOT-R.                              
000970     IF NOT OPENED THEN MOVE '3' TO F-R-CODE                              
000980                        GO TO GOBACK-RTN.                                 
000990     READ MCTF-IN INTO WS-MCTR-IN AT END GO TO E-O-F.                     
001000     MOVE MI-RECORD-KEY TO MCTF-KEY.                                      
001010     MOVE MI-DATA TO MCTF-DATA.                                           
001020     MOVE '0' TO F-R-CODE.                                                
001030     GO TO GOBACK-RTN.                                                    
001040     SKIP3                                                                
001050 NOT-R.                                                                   
001060     IF F-R-CODE NOT = 'S' THEN GO TO NOT-S.                              
001070     IF NOT OPENED THEN MOVE '6' TO F-R-CODE                              
001080                        GO TO GOBACK-RTN.                                 
001090*    MOVE MCTF-KEY TO MCTR-NOMINAL-KEY.                                   
001100     MOVE MCTF-KEY TO MCTRI-RECORD-KEY.                                   
001110     START MCTF-IN INVALID KEY GO TO MCTF-BAD-READ.                       
001120     MOVE '0' TO F-R-CODE.                                                
001130     GO TO GOBACK-RTN.                                                    
001140 NOT-S.                                                                   
001150     IF F-R-CODE = 'O' THEN   OPEN INPUT MCTF-IN                          
001160                              MOVE '1' TO OPEN-SW                         
001170                              MOVE '0' TO F-R-CODE                        
001180                              GO TO GOBACK-RTN.                           
001190     IF F-R-CODE = 'C' THEN CLOSE MCTF-IN                                 
001200                            MOVE '0' TO OPEN-SW                           
001210                            MOVE '0' TO F-R-CODE                          
001220                            GO TO GOBACK-RTN.                             
001230     MOVE '2' TO F-R-CODE.                                                
001240 GOBACK-RTN.                                                              
001250     GOBACK.                                                              
001260     EJECT                                                                
001270 MCTF-BAD-READ.                                                           
001280     MOVE '1' TO F-R-CODE.                                                
001290     GOBACK.                                                              
001300 MCTF-BAD-READ-EXIT.                                                      
001310     SKIP3                                                                
001320 I-O-ERROR.                                                               
001330     MOVE '4' TO F-R-CODE.                                                
001340     GOBACK.                                                              
001350 I-O-ERROR-EXIT.                                                          
001360     SKIP3                                                                
001370 E-O-F.                                                                   
001380     MOVE '7' TO F-R-CODE.                                                
001390     GOBACK.                                                              
001400 E-O-F-EXIT.                                                              
  