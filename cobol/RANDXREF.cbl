000010 IDENTIFICATION DIVISION.                                                 
000020 PROGRAM-ID. RANDXREF.                                                    
000030 AUTHOR. RICHARD WOMBLE.                                                  
000040 DATE-WRITTEN. JULY, 1983.                                                
000050 DATE-COMPILED.                                                           
000060 REMARKS.                                                                 
000070        PROJECT 8300510 IMPLEMENTED 09/09/83.                             
000080        THIS SUBROUTINE  WILL OPEN, READ AND CLOSE THE BLUE               
000090        CROSS BLUE SHIELD CROSS REFERENCE FILE.                           
000100                                                                          
000110        PROJECT 8601799 IMPLEMENTED 04/24/87 BY ANGELA HERRING.           
000120        THE PROGRAM WAS MODIFIED SO THAT IT CAN NOW OPEN THE              
000130        CROSS REFERENCE FILE FOR INQUIRY ONLY.  A NEW PARAMETER           
000140        (I) WAS ADDED TO DO THIS.                                         
000150                                                                          
000180        ACCESS IS RANDOM.                                                 
000250                                                                          
000260         FUNCTIONS - (ACCEPTABLE VALUES FOR IOOP CODE IN THE              
000270                      CALL STATEMENT.)                                    
000280           O     OPENS FILE                                               
000290           C     CLOSES THE FILE                                          
000300           R     READS THE FILE RANDOMLY                                  
000310           D     DELETES THE RECORD                                       
000330           A     WRITES THE RECORD RANDOMLY (FOR ADDS)                    
000340           I     OPENS FILE FOR INQUIRY (READ ONLY)               8601799 
000350                                                                          
000360                                                                          
000370         RETURN CODES - VALUES RETURNED IN IOOP                           
000380           O     GOOD OPEN                                                
000390           C     GOOD CLOSE                                               
000400           R     GOOD READ                                                
000410           D     GOOD DELETE                                              
000430           A     GOOD WRITE                                               
000440                                                                          
000450         RETURN CODE - VALUES RETURNED IN ERR CODE                        
000460           N     RECORD NOT FOUND                                         
000470          ' '    GOOD ENTRY                                               
000480           E     INVALID KEY STRUCTURE                                    
000490           I     INVALID IOOP                                             
000500           D     DUPLICATE KEY (FOR ADDS).                                
Y2000+*+*************************************************************** Y2000+  
Y2000+*+  EDITED BY CGA/ARC RENOVATOR RENOALL V6.05 ON 1998-5-21 18:18  Y2000+  
Y2000+*+*************************************************************** Y2000+  
000510                                                                          
000511* 945026  PROGRAM RE-COMPILED AND RE-LINKED BY ANN JOHNSTON 09/01/94      
000512*         FOR NCALL. WAS PICKING UP OLD VSCOB SUBROUTINES NEED COB2       
000513*         SUBROUTINES.  THIS WAS CAUSING PROBLEMS WITH INQUIRIES.         
000514*                                                                         
000520                                                                          
000530 ENVIRONMENT DIVISION.                                                    
000540 INPUT-OUTPUT SECTION.                                                    
000550 FILE-CONTROL.                                                            
000560     SELECT CROSS-REFERENCE  ASSIGN TO UT-MMXREF                          
000570         ACCESS IS RANDOM                                                 
000580         ORGANIZATION IS INDEXED                                          
000590         RECORD KEY IS CROSS-REFERENCE-KEY                                
000600         FILE STATUS IS VSAM-FILE-STATUS.                                 
000610                                                                          
000620 DATA DIVISION.                                                           
000630 FILE SECTION.                                                            
000640 FD  CROSS-REFERENCE                                                      
000650     LABEL RECORDS ARE STANDARD                                           
000660     DATA RECORDS IS CROSS-REF-REC.                                       
000670 01  CROSS-REF-REC  COPY BCBSXREF.                                        
000680                                                                          
000690 WORKING-STORAGE SECTION.                                                 
000700 77  FILLER                          PIC X(36)                            
000710         VALUE  'RANDXREF WORKING STORAGE BEGINS HERE'.                   
000720 77  COMPILE-DATE                    PIC X(20).                           
000730 01  VSAM-ERROR-DATA.                                                     
000740     05 VSAM-FILE-STATUS             PIC XX    VALUE 'ZZ'.                
000750     05 VSAM-ERROR-MSG               PIC X(45).                           
000760     05 MSG-02                       PIC X(45) VALUE                      
000770        'DUPLICATE KEY AND DUPLICATES SPECIFIED'.                         
000780     05 MSG-10                       PIC X(45) VALUE                      
000790        'AT END, NO NEXT LOGICAL RECORD'.                                 
000800     05 MSG-20                       PIC X(45) VALUE                      
000810        'INVALID KEY'.                                                    
000820     05 MSG-21                       PIC X(45) VALUE                      
000830        'INVALID KEY, SEQUENCE ERROR'.                                    
000840     05 MSG-22                       PIC X(45) VALUE                      
000850        'DUPLICATE KEY AND NO DUPLICATES ALLOWED'.                        
000860     05 MSG-23                       PIC X(45) VALUE                      
000870        'NO RECORD FOUND'.                                                
000880     05 MSG-24                       PIC X(45) VALUE                      
000890        'BOUNDARY VIOLATION INDEXED OR RELATIVE FILE'.                    
000900     05 MSG-30                       PIC X(45) VALUE                      
000910        'DATA CHECK, PARITY CHECK, TRANSMISSION ERROR'.                   
000920     05 MSG-34                       PIC X(45) VALUE                      
000930        'BOUNDARY VIOLATION SEQUENTIAL FILE'.                             
000940     05 MSG-90                       PIC X(45) VALUE                      
000950        'NO FURTHER INFORMATION'.                                         
000960     05 MSG-91                       PIC X(45) VALUE                      
000970        'PASSWORD FAILURE'.                                               
000980     05 MSG-92                       PIC X(45) VALUE                      
000990        'LOGIC ERROR'.                                                    
001000     05 MSG-93                       PIC X(45) VALUE                      
001010        'RESOURCE NOT AVAILABLE'.                                         
001020     05 MSG-94                       PIC X(45) VALUE                      
001030        'NO CURRENT REC POINTER FOR SEQ REQUEST'.                         
001040     05 MSG-95                       PIC X(45) VALUE                      
001050        'INVALID OR INCOMPLETE FILE INFORMATION'.                         
001060     05 MSG-96                       PIC X(45) VALUE                      
001070        'NO FILE IDENTIFICATION'.                                         
001080     05 MSG-97                       PIC X(45) VALUE                      
001090        'OPEN SUCCESSFUL, FILE INTEGRITY VERIFIED'.                       
001100 LINKAGE SECTION.                                                         
001110 01  PARM-IOOP                   PIC X(1).                                
001120 01  PARM-ERR                    PIC X(1).                                
001150 01  PARM-XREF-DATA              PIC X(30).                               
001160 01  PARM-XREF-KEY               PIC X(18).                               
001170                                                                          
001180                                                                          
001190                                                                          
001200                                                                          
001210 PROCEDURE DIVISION  USING PARM-IOOP PARM-ERR PARM-XREF-DATA              
001220       PARM-XREF-KEY.                                                     
001230     MOVE WHEN-COMPILED TO COMPILE-DATE.                                  
001240*****IF PARM-IOOP = 'O' OR 'C' OR 'R' OR 'D' OR 'A'***************8601799 
001241     IF PARM-IOOP = 'O' OR 'C' OR 'R' OR 'D' OR 'A' OR 'I'        8601799 
001250        NEXT SENTENCE                                                     
001260     ELSE                                                                 
001270        MOVE ZERO TO VSAM-FILE-STATUS                                     
001280        MOVE 'I' TO PARM-ERR.                                             
001290     IF PARM-IOOP = 'O'                                                   
001300        PERFORM A100-OPEN-FILE THRU A100-EXIT.                            
001310     IF PARM-IOOP = 'C'                                                   
001320        PERFORM A200-CLOSE-FILE THRU A200-EXIT.                           
001330     IF PARM-IOOP = 'R'                                                   
001340        PERFORM A300-READ-REC THRU A300-EXIT.                             
001350     IF PARM-IOOP = 'D'                                                   
001360        PERFORM A400-DELETE-REC THRU A400-EXIT.                           
001390     IF PARM-IOOP = 'A'                                                   
001400        PERFORM A500-WRITE-REC THRU A500-EXIT.                            
001401     IF PARM-IOOP = 'I'                                           8601799 
001402        PERFORM A600-INQUIRY THRU A600-EXIT.                      8601799 
001410     IF VSAM-FILE-STATUS = ZERO                                           
001420        NEXT SENTENCE                                                     
001430     ELSE                                                                 
001440        DISPLAY 'VSAM ERROR CODE...' VSAM-FILE-STATUS                     
001450        DISPLAY 'CODE EXPLANATION..' VSAM-ERROR-MSG                       
001460        DISPLAY ' '                                                       
001470        CALL 'COBABEND'.                                                  
001480     GOBACK.                                                              
001490 A100-OPEN-FILE SECTION.                                                  
001500     OPEN I-O CROSS-REFERENCE.                                            
001510     IF VSAM-FILE-STATUS = ZERO                                           
001520        GO TO A100-EXIT.                                                  
001530     IF VSAM-FILE-STATUS = '97'                                           
001540        MOVE ZERO TO VSAM-FILE-STATUS                                     
001550        GO TO A100-EXIT.                                                  
001560     PERFORM A9999-VSAM-ERROR-RTN THRU A9999-VSAM-EXIT.                   
001570 A100-EXIT.                                                               
001580     EXIT.                                                                
001590     SKIP3                                                                
001600 A200-CLOSE-FILE SECTION.                                                 
001610     CLOSE CROSS-REFERENCE.                                               
001620     IF VSAM-FILE-STATUS = ZERO                                           
001630        GO TO A200-EXIT.                                                  
001640     PERFORM A9999-VSAM-ERROR-RTN THRU A9999-VSAM-EXIT.                   
001650 A200-EXIT.                                                               
001660     EXIT.                                                                
001670     SKIP3                                                                
001680 A300-READ-REC SECTION.                                                   
001690     MOVE ' ' TO PARM-ERR.                                                
001700     MOVE PARM-XREF-KEY TO CROSS-REFERENCE-KEY.                           
001710     READ CROSS-REFERENCE.                                                
001720     IF VSAM-FILE-STATUS = ZERO                                           
001730        MOVE CROSS-REF-REC TO PARM-XREF-DATA                              
001740        GO TO A300-EXIT.                                                  
001750     IF VSAM-FILE-STATUS = '23'                                           
001760        MOVE 'N' TO PARM-ERR                                              
001770        MOVE ZERO TO VSAM-FILE-STATUS                                     
001780        GO TO A300-EXIT.                                                  
001790     IF VSAM-FILE-STATUS = '20'                                           
001800        MOVE 'E' TO PARM-ERR                                              
001810        MOVE ZERO TO VSAM-FILE-STATUS                                     
001820        GO TO A300-EXIT.                                                  
001830     PERFORM A9999-VSAM-ERROR-RTN THRU A9999-VSAM-EXIT.                   
001840 A300-EXIT.                                                               
001850     EXIT.                                                                
001860     SKIP3                                                                
001870 A400-DELETE-REC SECTION.                                                 
001880     MOVE ' ' TO PARM-ERR.                                                
001890     MOVE PARM-XREF-KEY TO CROSS-REFERENCE-KEY.                           
001900     DELETE CROSS-REFERENCE RECORD.                                       
001910     IF VSAM-FILE-STATUS = ZERO                                           
001920        GO TO A400-EXIT.                                                  
001930     IF VSAM-FILE-STATUS = '23'                                           
001940        MOVE 'N' TO PARM-ERR                                              
001950        MOVE ZERO TO VSAM-FILE-STATUS                                     
001960        GO TO A400-EXIT.                                                  
001970     IF VSAM-FILE-STATUS = '20'                                           
001980        MOVE 'E' TO PARM-ERR                                              
001990        MOVE ZERO TO VSAM-FILE-STATUS                                     
002000        GO TO A400-EXIT.                                                  
002010     PERFORM A9999-VSAM-ERROR-RTN THRU A9999-VSAM-EXIT.                   
002020 A400-EXIT.                                                               
002030     EXIT.                                                                
002040     SKIP3                                                                
002240 A500-WRITE-REC SECTION.                                                  
002250     MOVE ' ' TO PARM-ERR.                                                
002260     MOVE PARM-XREF-KEY TO CROSS-REFERENCE-KEY.                           
002270     READ CROSS-REFERENCE.                                                
002280     IF VSAM-FILE-STATUS = '23'                                           
002290        PERFORM A505-WRITE-REC THRU A505-EXIT                             
002300     ELSE                                                                 
002310        MOVE 'D' TO PARM-ERR                                              
002320        MOVE ZERO TO VSAM-FILE-STATUS.                                    
002330 A500-EXIT.                                                               
002340     EXIT.                                                                
002350 A505-WRITE-REC.                                                          
002360     MOVE ' ' TO PARM-ERR.                                                
002370     MOVE PARM-XREF-KEY TO CROSS-REFERENCE-KEY.                           
002380     MOVE PARM-XREF-DATA TO CROSS-REF-REC.                                
002390     WRITE CROSS-REF-REC.                                                 
002400     IF VSAM-FILE-STATUS = ZEROS OR '02'                                  
002410         MOVE ZERO TO VSAM-FILE-STATUS                                    
002420         GO TO A505-EXIT.                                                 
002430     IF VSAM-FILE-STATUS = '20'                                           
002440         MOVE 'E' TO PARM-ERR                                             
002450         MOVE ZERO TO VSAM-FILE-STATUS                                    
002460         GO TO A505-EXIT.                                                 
002470     IF VSAM-FILE-STATUS = '22'                                           
002480         MOVE 'D' TO PARM-ERR                                             
002490         MOVE ZERO TO VSAM-FILE-STATUS                                    
002500         GO TO A505-EXIT.                                                 
002510     IF VSAM-FILE-STATUS = '23'                                           
002520         MOVE 'N' TO PARM-ERR                                             
002530         MOVE ZERO TO VSAM-FILE-STATUS                                    
002540         GO TO A505-EXIT.                                                 
002550     PERFORM A9999-VSAM-ERROR-RTN THRU A9999-VSAM-EXIT.                   
002560 A505-EXIT.                                                               
002570     EXIT.                                                                
002571                                                                          
002572 A600-INQUIRY.                                                    8601799 
002573     OPEN INPUT CROSS-REFERENCE.                                  8601799 
002574     IF VSAM-FILE-STATUS = ZERO                                   8601799 
002575        GO TO A600-EXIT                                           8601799 
002576     ELSE                                                         8601799 
002577        IF VSAM-FILE-STATUS = '97'                                8601799 
002578           MOVE ZERO TO VSAM-FILE-STATUS                          8601799 
002579           GO TO A600-EXIT.                                       8601799 
002580     PERFORM A9999-VSAM-ERROR-RTN THRU A9999-VSAM-EXIT.           8601799 
002581 A600-EXIT.                                                       8601799 
002582     EXIT.                                                        8601799 
002584     SKIP3                                                                
002590 A9999-VSAM-ERROR-RTN SECTION.                                            
002600     IF VSAM-FILE-STATUS = '02'                                           
002610        MOVE MSG-02 TO VSAM-ERROR-MSG                                     
002620        GO TO A9999-VSAM-EXIT.                                            
002630     IF VSAM-FILE-STATUS = '10'                                           
002640        MOVE MSG-10 TO VSAM-ERROR-MSG                                     
002650        GO TO A9999-VSAM-EXIT.                                            
002660     IF VSAM-FILE-STATUS = '20'                                           
002670        MOVE MSG-20 TO VSAM-ERROR-MSG                                     
002680        GO TO A9999-VSAM-EXIT.                                            
002690     IF VSAM-FILE-STATUS = '21'                                           
002700        MOVE MSG-21 TO VSAM-ERROR-MSG                                     
002710        GO TO A9999-VSAM-EXIT.                                            
002720     IF VSAM-FILE-STATUS = '22'                                           
002730        MOVE MSG-22 TO VSAM-ERROR-MSG                                     
002740        GO TO A9999-VSAM-EXIT.                                            
002750     IF VSAM-FILE-STATUS = '23'                                           
002760        MOVE MSG-23 TO VSAM-ERROR-MSG                                     
002770        GO TO A9999-VSAM-EXIT.                                            
002780     IF VSAM-FILE-STATUS = '24'                                           
002790        MOVE MSG-24 TO VSAM-ERROR-MSG                                     
002800        GO TO A9999-VSAM-EXIT.                                            
002810     IF VSAM-FILE-STATUS = '30'                                           
002820        MOVE MSG-30 TO VSAM-ERROR-MSG                                     
002830        GO TO A9999-VSAM-EXIT.                                            
002840     IF VSAM-FILE-STATUS = '34'                                           
002850        MOVE MSG-34 TO VSAM-ERROR-MSG                                     
002860        GO TO A9999-VSAM-EXIT.                                            
002870     IF VSAM-FILE-STATUS = '90'                                           
002880        MOVE MSG-90 TO VSAM-ERROR-MSG                                     
002890        GO TO A9999-VSAM-EXIT.                                            
002900     IF VSAM-FILE-STATUS = '91'                                           
002910        MOVE MSG-91 TO VSAM-ERROR-MSG                                     
002920        GO TO A9999-VSAM-EXIT.                                            
002930     IF VSAM-FILE-STATUS = '92'                                           
002940        MOVE MSG-92 TO VSAM-ERROR-MSG                                     
002950        GO TO A9999-VSAM-EXIT.                                            
002960     IF VSAM-FILE-STATUS = '93'                                           
002970        MOVE MSG-93 TO VSAM-ERROR-MSG                                     
002980        GO TO A9999-VSAM-EXIT.                                            
002990     IF VSAM-FILE-STATUS = '94'                                           
003000        MOVE MSG-94 TO VSAM-ERROR-MSG                                     
003010        GO TO A9999-VSAM-EXIT.                                            
003020     IF VSAM-FILE-STATUS = '95'                                           
003030        MOVE MSG-95 TO VSAM-ERROR-MSG                                     
003040        GO TO A9999-VSAM-EXIT.                                            
003050     IF VSAM-FILE-STATUS = '96'                                           
003060        MOVE MSG-96 TO VSAM-ERROR-MSG                                     
003070        GO TO A9999-VSAM-EXIT.                                            
003080     IF VSAM-FILE-STATUS = '97'                                           
003090        MOVE MSG-97 TO VSAM-ERROR-MSG                                     
003100        GO TO A9999-VSAM-EXIT.                                            
003110 A9999-VSAM-EXIT.                                                         
003120     EXIT.                                                                
  