000100 ID DIVISION.                                                             
000200 PROGRAM-ID. BCBSEXTR                                                     
000210**************************************************************            
000300*    READ THE MACHINE FILE INPUT AND WRITE A FILE OF LOAD    *            
000400*    MODULES THAT MUST BE RELINKED BEFORE THEY WILL RUN UNDER*            
000410*    LEMVS. HIS LIST IS FOR ALL LOAD MODULES CONTAINING      *            
000420*    BOTH COBOL RES AND COBOL NORES MODULES.                 *            
000421*    SPP.EDGE.PRTFOLIO.SOURCE(MRRECCOB).                     *            
000422*    THE MACHINE RECORD CONTAINS COMMON FIELDS FROM MRMEM TO *            
000423*    MRDATLKD. FIELDS BETWEEN MRC2DATA AND MRC2AWO ARE FOR   *            
000424*    TYPES C2 AND C3. FIELDS BETWEEN MROCSYMD AND MROCSYSO   *            
000425*    ARE FOR TYPES VS AND V4. FIELDS MRCOBSM AND MRCOBDC ARE *            
000426*    COMMON TO ALL COBOLS.                                   *            
000430**************************************************************            
000500 ENVIRONMENT DIVISION.                                                    
000600 CONFIGURATION SECTION.                                                   
000700 SOURCE-COMPUTER. IBM-370.                                                
000800 OBJECT-COMPUTER. IBM-370.                                                
000900 INPUT-OUTPUT SECTION.                                                    
001000 FILE-CONTROL.                                                            
001100     SELECT MACHINE-FILE ASSIGN TO UT-S-MACHINE                           
001200         FILE STATUS MACHINE-FILE-STATUS.                                 
001300     SELECT REPORT-FILE ASSIGN TO UT-S-REPORT                             
001400         FILE STATUS REPORT-FILE-STATUS.                                  
001500 DATA DIVISION.                                                           
001600 FILE SECTION.                                                            
001700 FD  MACHINE-FILE                                                         
001800     BLOCK CONTAINS 0 RECORDS                                             
001900     RECORD CONTAINS 200 CHARACTERS                                       
002000     RECORDING F.                                                         
002100 01  MACHINER.                                                            
002110     05 CSECT-DATA.                                                       
002120        10 MEMBER-NAME               PIC X(8).                            
002130        10 MEMBER-AMODE              PIC X(1).                            
002140           88 MEMBER-AMODE-24           VALUE '2'.                        
002150           88 MEMBER-AMODE-31           VALUE '3'.                        
002160           88 MEMBER-AMODE-ANY          VALUE 'A'.                        
002170        10 MEMBER-RMODE              PIC X(1).                            
002180           88 MEMBER-RMODE-24           VALUE '2'.                        
002190           88 MEMBER-RMODE-31           VALUE '3'.                        
002191           88 MEMBER-RMODE-ANY          VALUE 'A'.                        
002192        10 CSECTNAME                 PIC X(8).                            
002193        10 COMPILER-ID               PIC X(10).                           
002194        10 COMPILER-VVMM             PIC X(04).                           
002195        10 COMPILE-DATE-YYDDD        PIC 9(05).                           
002196        10 CSECT-AMODE               PIC X.                               
002197           88 CSECT-AMODE-24            VALUE '2'.                        
002198           88 CSECT-AMODE-31            VALUE '3'.                        
002199           88 CSECT-AMODE-ANY           VALUE 'A'.                        
002200           88 CSECT-AMODE-PREXA         VALUE ' '.                        
002201        10 CSECT-RMODE               PIC X.                               
002202           88 CSECT-RMODE-24            VALUE '2'.                        
002203           88 CSECT-RMODE-ANY           VALUE 'A'.                        
002204        10 CSECT-TYPE                PIC  X(02).                          
002205           88 ASSEMBLER-PROGRAM         VALUE 'AR'.                       
002206           88 BASIC-PROGRAM             VALUE 'BA'.                       
002207           88 DEBUG-TOOL                VALUE 'BG'.                       
002208           88 BERKELEY-SOCKET           VALUE 'BS'.                       
002209           88 CICS-INTERFACE            VALUE 'CC'.                       
002210           88 C370-RUNTIME-ROUTINE      VALUE 'CE'.                       
002211           88 C370-PROGRAM              VALUE 'CI'.                       
002212           88 LATTICE-C-PROGRAM         VALUE 'CL'.                       
002213           88 COMMON-SECTION            VALUE 'CM'.                       
002214           88 COBOL-RUNTIME-ROUTINE     VALUE 'CR'.                       
002215           88 SAS-C-PROGRAM             VALUE 'CS'.                       
002216           88 C-CPLUSPLUS-FOR-OS390     VALUE 'CP'.                       
002217           88 COBOL-II-PROGRAM          VALUE 'C2'.                       
002218           88 COBOL-370-390-PROGRAM     VALUE 'C3'.                       
002219           88 DCE-ROUTINE               VALUE 'DC'.                       
002220           88 DB2-INTERFACE             VALUE 'DI'.                       
002221           88 EASYTRIEVE-PLUS-PROGRAM   VALUE 'EZ'.                       
002222           88 FORTRAN-G-G1-PROGRAM      VALUE 'FG'.                       
002223           88 FORTRAN-H-PROGRAM         VALUE 'FH'.                       
002224           88 FORTRAN-RUNTIME-ROUTINE   VALUE 'FR'.                       
002225           88 VS-FORTRAN-PROGRAM        VALUE 'FV'.                       
002226           88 IMS-INTERFACE             VALUE 'II'.                       
002227           88 ISPF-INTERFACE            VALUE 'IS'.                       
002228           88 LE-RUNTIME-ROUTINE        VALUE 'LE'.                       
002229           88 MARK-V-PROGRAM            VALUE 'MV'.                       
002230           88 NOT-COMPLETELY-ANALYZED   VALUE 'NA'.                       
002231           88 UNRECOGNIZED-CSECT-TYPE   VALUE 'NR'.                       
002232           88 VA-PLI-FOR-OS390-PROGRAM  VALUE 'PL'.                       
002233           88 PLI-FOR-MVS-AND-VM-PROGRAM VALUE 'PM'.                      
002234           88 PLI-RUNTIME-ROUTINE       VALUE 'PR'.                       
002235           88 VS-PASCAL-PROGRAM         VALUE 'PV'.                       
002236           88 PLI-OPTIMIZER-V1-PROGRAM  VALUE 'P1'.                       
002237           88 PLI-OPTIMIZER-V2-PROGRAM  VALUE 'P2'.                       
002238           88 IBM-REXX-PROGRAM          VALUE 'RE'.                       
002239           88 OLD-LANGUAGE-RUNTIME-ROUTINE VALUE 'RE'.                    
002240           88 SNAPAID-INTERFACE         VALUE 'SA'.                       
002241           88 COBOL-SAMPLE-PROGRAM      VALUE 'SC'.                       
002242           88 USER-RECOGNIZED-ASSEMBLER VALUE 'UR'.                       
002243           88 VERY-OLD-COBOL            VALUE 'VO'.                       
002244           88 OSVS-COBOL-V2REL3         VALUE 'VS'.                       
002245           88 OSVS-COBOL-V2REL4         VALUE 'V4'.                       
002246        10 DATE-TIME-FORMAT          PIC X.                               
002247           88 SVC-PATTERN-DATE-TIME     VALUE '1'.                        
002248           88 STCK-DATE-TIME            VALUE '2'.                        
002249           88 BOTH-SVC-AND-STCK         VALUE '3'.                        
002250           88 COBOL-ACCEPT-FROM-FORMAT  VALUE '4'.                        
002251           88 COBOL-ACCEPT-OTHER-FORMAT VALUE '9'.                        
002252        10 LOAD-MODULE-ATTRIBUTES    PIC X.                               
002253           88 REUSABLE-MODULE           VALUE '1'.                        
002254           88 RENT-NOT-REUS-MODULE      VALUE '2'.                        
002255           88 RENT-AND-REUS-MODULE      VALUE '3'.                        
002256           88 OVERLAY-DATA-SUSPECT      VALUE '8'.                        
002257           88 MODULE-NOT-REUSABLE       VALUE ' '.                        
002258        10 PRIMARY-ENTRY-POINT-NAME  PIC X(8).                            
002259        10 CSECT-SIZE-IN-DECIMAL     PIC 9(08).                           
002260        10 SLASH-U-PARM-DATA         PIC X(04).                           
002261        10 LAST-LINKEDIT-DATE-YYDDD  PIC 9(05).                           
002262     05 COBOL-II-AND-COBOL-370-FIELDS.                                    
002263        10 COBOL-II-COMPILER-VERSION PIC X(06).                           
002264        10 COBOL-II-LEVEL            PIC X(04).                           
002265        10 COBOL-II-DATE-COMPILED    PIC X(9).                            
002266        10 COBOL-II-TIME-COMPILED    PIC X(8).                            
002267        10 COBOL-II-ADV-INDICATOR    PIC X(1).                            
002268           88 COBOL-II-ADV              VALUE '1'.                        
002269           88 COBOL-II-NOADV            VALUE '0'.                        
002270        10 COBOL-II-QUOTE-APOST-INDICATOR PIC X.                          
002271           88 COBOL-II-QUOTE            VALUE '0'.                        
002272           88 COBOL-II-APOST            VALUE '1'.                        
002273        10 COBOL-II-DATA-INDICATOR   PIC X.                               
002274           88 COBOL-II-DATA-24          VALUE '0'.                        
002275           88 COBOL-II-DATA-31          VALUE '1'.                        
002276        10 COBOL-II-DECK-INDICATOR   PIC X.                               
002277           88 COBOL-II-NODECK           VALUE '0'.                        
002278           88 COBOL-II-DECK             VALUE '1'.                        
002279        10 COBOL-II-DUMP-INDICATOR   PIC X.                               
002280           88 COBOL-II-NODUMP           VALUE '0'.                        
002281           88 COBOL-II-DUMP             VALUE '1'.                        
002282        10 COBOL-II-DYNAM-INDICATOR  PIC X.                               
002283           88 COBOL-II-NODYNAM          VALUE '0'.                        
002284           88 COBOL-II-DYNAM            VALUE '1'.                        
002285        10 COBOL-II-FASTSRT-INDICATOR PIC X.                              
002286           88 COBOL-II-NOFASTSRT        VALUE '0'.                        
002287           88 COBOL-II-FASTSRT          VALUE '1'.                        
002288        10 COBOL-II-FDUMP-INDICATOR  PIC X.                               
002289           88 COBOL-II-NOFDUMP          VALUE '0'.                        
002290           88 COBOL-II-FDUMP            VALUE '1'.                        
002291        10 COBOL-II-LIB-INDICATOR    PIC X.                               
002292           88 COBOL-II-NOLIB            VALUE '0'.                        
002293           88 COBOL-II-LIB              VALUE '1'.                        
002294        10 COBOL-II-LIST-INDICATOR   PIC X.                               
002295           88 COBOL-II-NOLIST           VALUE '0'.                        
002296           88 COBOL-II-LIST             VALUE '1'.                        
002297        10 COBOL-II-MAP-INDICATOR    PIC X.                               
002298           88 COBOL-II-NOMAP            VALUE '0'.                        
002299           88 COBOL-II-MAP              VALUE '1'.                        
002300        10 COBOL-II-NUM-INDICATOR    PIC X.                               
002301           88 COBOL-II-NONUM            VALUE '0'.                        
002302           88 COBOL-II-NUM              VALUE '1'.                        
002303        10 COBOL-II-OBJ-INDICATOR    PIC X.                               
002304           88 COBOL-II-NOOBJ            VALUE '0'.                        
002305           88 COBOL-II-OBJ              VALUE '1'.                        
002306        10 COBOL-II-OFFSET-INDICATOR  PIC X.                              
002307           88 COBOL-II-NOOFFSET         VALUE '0'.                        
002308           88 COBOL-II-OFFSET           VALUE '1'.                        
002309        10 COBOL-II-OPTIMIZ-INDICATOR PIC X.                              
002310           88 COBOL-II-NOOPTIMIZ        VALUE '0'.                        
002311           88 COBOL-II-OPTIMIZ          VALUE '1'.                        
002312        10 COBOL-II-DD-INDICATOR     PIC X.                               
002313           88 COBOL-II-DD               VALUE '0'.                        
002314           88 COBOL-II-OUTDD            VALUE '1'.                        
002315        10 COBOL-II-PFD-INDICATOR    PIC X.                               
002316           88 COBOL-II-NOPFDSGN         VALUE '0'.                        
002317           88 COBOL-II-PFDSGN           VALUE '1'.                        
002318           88 COBOL-390-NUMNPFD          VALUE '0'.                       
002319           88 COBOL-390-NUMPFD           VALUE '1'.                       
002320           88 COBOL-390-NUMMIG           VALUE '2'.                       
002321        10 COBOL-II-RENT-INDICATOR   PIC X.                               
002322           88 COBOL-II-NORENT           VALUE '0'.                        
002323           88 COBOL-II-RENT             VALUE '1'.                        
002324        10 COBOL-II-RES-INDICATOR    PIC X.                               
002325           88 COBOL-II-NORES            VALUE '0'.                        
002326           88 COBOL-II-RES              VALUE '1'.                        
002327        10 COBOL-II-SEQ-INDICATOR    PIC X.                               
002328           88 COBOL-II-NOSEQ            VALUE '0'.                        
002329           88 COBOL-II-SEQ              VALUE '1'.                        
002330        10 COBOL-II-SIZE-INDICATOR   PIC X.                               
002331           88 COBOL-II-SIZE-NBR         VALUE '0'.                        
002332           88 COBOL-II-SIZE-MAX         VALUE '1'.                        
002333        10 COBOL-II-SOURCE-INDICATOR PIC X.                               
002334           88 COBOL-II-NOSOURCE         VALUE '0'.                        
002335           88 COBOL-II-SOURCE           VALUE '1'.                        
002336        10 COBOL-II-SSRANGE-INDICATOR PIC X.                              
002337           88 COBOL-II-NOSSRANGE        VALUE '0'.                        
002338           88 COBOL-II-SSRANGE          VALUE '1'.                        
002339        10 COBOL-II-TERM-INDICATOR   PIC X.                               
002340           88 COBOL-II-NOTERM           VALUE '0'.                        
002341           88 COBOL-II-TERM             VALUE '1'.                        
002342        10 COBOL-II-TEST-INDICATOR   PIC X.                               
002343           88 COBOL-II-NOTEST           VALUE '0'.                        
002344           88 COBOL-II-TEST             VALUE '1'.                        
002345        10 COBOL-II-TRUNC-INDICATOR  PIC X.                               
002346           88 COBOL-II-NOTRUNC          VALUE '0'.                        
002347           88 COBOL-II-TRUNC            VALUE '1'.                        
002348           88 COBOL-390-TRUNCOPT        VALUE '0'.                        
002349           88 COBOL-390-TRUNC-STD       VALUE '1'.                        
002350           88 COBOL-390-TRUNC-BIN       VALUE '1'.                        
002351        10 COBOL-II-WORD-INDICATOR   PIC X.                               
002352           88 COBOL-II-WORD-DEFAULT     VALUE '0'.                        
002353           88 COBOL-II-WORD-SPECIFIED   VALUE '1'.                        
002354        10 COBOL-II-VBREF-INDICATOR  PIC X.                               
002355           88 COBOL-II-NOVBREF          VALUE '0'.                        
002356           88 COBOL-II-VBREF            VALUE '1'.                        
002357        10 COBOL-II-XREF-INDICATOR   PIC X.                               
002358           88 COBOL-II-NOXREF           VALUE '0'.                        
002359           88 COBOL-II-XREF             VALUE '1'.                        
002360        10 COBOL-II-ZWB-INDICATOR    PIC X.                               
002361           88 COBOL-II-NOZWB            VALUE '0'.                        
002362           88 COBOL-II-ZWB              VALUE '1'.                        
002363        10 COBOL-II-CMPR2-INDICATOR  PIC X.                               
002364           88 COBOL-II-NOCMPR2          VALUE '0'.                        
002365           88 COBOL-II-CMPR2            VALUE '1'.                        
002366        10 COBOL-II-NAME-INDICATOR   PIC X.                               
002367           88 COBOL-II-NONAME           VALUE '0'.                        
002368           88 COBOL-II-NAME             VALUE '1'.                        
002369        10 COBOL-II-NMCL-INDICATOR   PIC X.                               
002370           88 COBOL-II-NMCLPRI          VALUE '0'.                        
002371           88 COBOL-II-NMCLALT          VALUE '1'.                        
002372        10 COBOL-II-DBCS-INDICATOR   PIC X.                               
002373           88 COBOL-II-NODBCS           VALUE '0'.                        
002374           88 COBOL-II-DBCS             VALUE '1'.                        
002375        10 COBOL-II-AWO-INDICATOR    PIC X.                               
002376           88 COBOL-II-NOAWO            VALUE '0'.                        
002377           88 COBOL-II-AWO              VALUE '1'.                        
002378     05 COBOL-370-AND-390-FIELDS.                                         
002379        10 COBOL-390-EVENTS-INDICATOR PIC X.                              
002380           88 COBOL-390-NOEVENTS         VALUE '0'.                       
002381           88 COBOL-390-EVENTS           VALUE '1'.                       
002382        10 COBOL-390-CURRENCY-INDICATOR PIC X.                            
002383           88 COBOL-39-NOCURRENCY       VALUE '0'.                        
002384           88 COBOL-39-CURRENCY         VALUE '1'.                        
002385        10 COBOL-390-RMODE-INDICATOR PIC X.                               
002386           88 COBOL-39-RMODE-24         VALUE '0'.                        
002387           88 COBOL-39-RMODE-ANY        VALUE '1'.                        
002388        10 FILLER                    PIC X(4).                            
002389     05 ALL-COBOL-SORTMERGE-INDICATOR PIC X.                              
002390        88 SORT-OR-MERGE-FOUND          VALUE '1'.                        
002391        88 NO-SORT-OR-MERGE-FOUND       VALUE '0'.                        
002392     05 ALL-COBOL-DYNAM-CALL-FND-INDIC PIC X.                             
002393        88 NO-DYNAMIC-CALLS-FOUND       VALUE '0'.                        
002394        88 CALL-LITERAL-FORCED-DYNAMIC VALUE '1'.                         
002395        88 CALL-DATANAME                VALUE '2'.                        
002396        88 CALL-LIT-FRCD-AND-CALL-DATANM VALUE '3'.                       
002397        88 CAPEX-OPT-CALL-ADDR          VALUE '4'.                        
002398        88 CAPEX-CALL-LIT-FORCED-DYNAM VALUE '5'.                         
002399        88 CAPEX-OPT-CALL-DATANAME      VALUE '6'.                        
002400        88 CAPEX-CALL-LIT-FRCD-CALL-DTNM VALUE '7'.                       
002401   05 VERSION-4-AND-VS-COBOL-FIELDS.                                      
002402        10 VSCOBOL-SYMDUMP-INDICATOR PIC X.                               
002403           88 VSCOBOL-NOSYMDUMP         VALUE '0'.                        
002404           88 VSCOBOL-SYMDUMP           VALUE '1'.                        
002405        10 VSCOBOL-FLOW-INDICATOR    PIC X.                               
002406           88 VSCOBOL-NOFLOW            VALUE '0'.                        
002407           88 VSCOBOL-FLOW              VALUE '1'.                        
002408        10 VSCOBOL-STATE-INDICATOR   PIC X.                               
002409           88 VSCOBOL-NOSTATE           VALUE '0'.                        
002410           88 VSCOBOL-STATE             VALUE '1'.                        
002411        10 VSCOBOL-TEST-INDICATOR    PIC X.                               
002412           88 VSCOBOL-NOTEST            VALUE '0'.                        
002413           88 VSCOBOL-TEST              VALUE '1'.                        
002414        10 VSCOBOL-RES-INDICATOR     PIC X.                               
002415           88 VSCOBOL-NORES             VALUE '0'.                        
002416           88 VSCOBOL-RES               VALUE '1'.                        
002417        10 VSCOBOL-ENDJOB-INDICATOR  PIC X.                               
002418           88 VSCOBOL-NOENDJOB          VALUE '0'.                        
002419           88 VSCOBOL-ENDJOB            VALUE '1'.                        
002420        10 VSCOBOL-OBJ370-INDICATOR  PIC X.                               
002421           88 VSCOBOL-OBJ360            VALUE '0'.                        
002422           88 VSCOBOL-OBJ370            VALUE '1'.                        
002423        10 VSCOBOL-OPTIMIZED-INDICATOR PIC X.                             
002424           88 VSCOBOL-NOOPT             VALUE '0'.                        
002425           88 VSCOBOL-OPT               VALUE '1'.                        
002426        10 VSCOBOL-CAPEX-OPT-INDICATOR PIC X.                             
002427           88 VSCOBOL-NOCAPEX-OPT       VALUE '0'.                        
002428           88 VSCOBOL-CAPEX-OPT         VALUE '1'.                        
002429        10 VSCOBOL-CAPEX-DTECT-INDICATOR PIC X.                           
002430           88 VSCOBOL-NOCAPEX-DTECT     VALUE '0'.                        
002431           88 VSCOBOL-CAPEX-DTECT       VALUE '1'.                        
002432        10 VSCOBOL-COUNT-INDICATOR   PIC X.                               
002433           88 VSCOBOL-NOCOUNT           VALUE '0'.                        
002434           88 VSCOBOL-COUNT             VALUE '1'.                        
002435        10 VSCOBOL-TRACE-INDICATOR   PIC X.                               
002436           88 VSCOBOL-NOTRACE           VALUE '0'.                        
002437           88 VSCOBOL-TRACE             VALUE '1'.                        
002438        10 VSCOBOL-LANGLEVEL-INDICATOR PIC X.                             
002439           88 VSCOBOL-LANGLEVEL-1       VALUE '1'.                        
002440           88 VSCOBOL-LANGLEVEL-2       VALUE '2'.                        
002441        10 VSCOBOL-LAST-SYSOU-DDNM-CHAR PIC X.                            
002442        10 FILLER                    PIC X(3).                            
002443     05 PLI-V2-FIELDS.                                                    
002444        10 PLI-V2-MAIN-INDICATOR     PIC X.                               
002445           88 PLI-V2-PROC-OPTIONS-MAIN  VALUE '1'.                        
002446           88 PLI-V2-NO-PROC-OPTIONS-MAIN VALUE '0'.                      
002447        10 PLI-V2-REORDER-INDICATOR  PIC X.                               
002448           88 PLI-V2-NOREORDER          VALUE '0'.                        
002449           88 PLI-V2-REORDER            VALUE '1'.                        
002450        10 PLI-V2-EXECOPS-INDICATOR  PIC X.                               
002451           88 PLI-V2-NOEXECOPS          VALUE '0'.                        
002452           88 PLI-V2-EXECOPS            VALUE '1'.                        
002453        10 PLI-V2-CMPAT-INDICATOR    PIC X.                               
002454           88 PLI-V2-CMPAT-V1           VALUE '1'.                        
002455           88 PLI-V2-CMPAT-V2           VALUE '2'.                        
002456        10 PLI-V2-GRAPHIC-INDICATOR  PIC X.                               
002457           88 PLI-V2-NOGRAPHIC          VALUE '0'.                        
002458           88 PLI-V2-GRAPHIC            VALUE '1'.                        
002459        10 PLI-V2-OPTIMIZE-INDICATOR PIC X.                               
002460           88 PLI-V2-NOOPTIMIZE         VALUE '0'.                        
002461           88 PLI-V2-OPTIMIZE           VALUE '1'.                        
002462        10 PLI-V2-INTERUPT-INDICATOR PIC X.                               
002463           88 PLI-V2-NOINTERUPT         VALUE '0'.                        
002464           88 PLI-V2-INTERUPT           VALUE '1'.                        
002465        10 PLI-V2-FLOW-INDICATOR     PIC X.                               
002466           88 PLI-V2-NOFLOW             VALUE '0'.                        
002467           88 PLI-V2-FLOW               VALUE '1'.                        
002468        10 PLI-V2-COUNT-INDICATOR    PIC X.                               
002469           88 PLI-V2-NOCOUNT            VALUE '0'.                        
002470           88 PLI-V2-COUNT              VALUE '1'.                        
002471        10 PLI-V2-TEST-INDICATOR     PIC X.                               
002472           88 PLI-V2-NOTEST             VALUE '0'.                        
002473           88 PLI-V2-TEST               VALUE '1'.                        
002474        10 PLI-V2-SYSTEM-INDICATOR   PIC X.                               
002475           88 PLI-V2-SYSTEM-CMS         VALUE '1'.                        
002476           88 PLI-V2-SYSTEM-CMSTP       VALUE '2'.                        
002477           88 PLI-V2-SYSTEM-MVS         VALUE '3'.                        
002478           88 PLI-V2-SYSTEM-TSO         VALUE '4'.                        
002479           88 PLI-V2-SYSTEM-CICS        VALUE '5'.                        
002480           88 PLI-V2-SYSTEM-IMS         VALUE '6'.                        
002481        10 PLI-V2-CSECT-INDICATOR    PIC X.                               
002482           88 PLI-V2-RUNTIME-CSECT      VALUE '1'.                        
002483           88 PLI-V2-EXTERNAL-DATA-CSECT VALUE '2'.                       
002484           88 PLI-V2-STATIC-DATA-CSECT  VALUE '3'.                        
002485           88 PLI-V2-CODE-CSECT         VALUE '4'.                        
002486     05 FILLER                       PIC X(28).                           
002487     05 WINDOW-INDICATOR             PIC X(4).                            
002488                                                                          
002489 FD  REPORT-FILE                                                          
002490     BLOCK CONTAINS 0 RECORDS                                             
002500     RECORD CONTAINS 133 CHARACTERS                                       
002600     RECORDING MODE F.                                                    
002700 01  REPORT-RECORD.                                                       
002800     05  FILLER                      PIC X.                               
002900     05  RPT-MEMBER-NAME             PIC X(8).                            
003000     05  FILLER                      PIC X.                               
003100     05  RPT-DSNAME                  PIC X(33).                           
003200     05  FILLER                      PIC X.                               
003300     05  RPT-ANY-REASON.                                                  
003400         10  RPT-RNR                 PIC X(9).                            
003500         10  FILLER                  PIC X.                               
003600         10  RPT-PLI                 PIC X(4).                            
003700         10  FILLER                  PIC X.                               
003800         10  RPT-C370                PIC X(4).                            
003900         10  FILLER                  PIC X.                               
004000         10  RPT-ILBO                PIC X(8).                            
004100         10  FILLER                  PIC X.                               
004200         10  RPT-STPRRE              PIC X(8).                            
004300         10  FILLER                  PIC X.                               
004400         10  RPT-TUNE                PIC X(8).                            
004500         10  FILLER                  PIC X(43).                           
004600                                                                          
004700 WORKING-STORAGE SECTION.                                                 
004800 01  MACHINE-FILE-STATUS.                                                 
004900     05  MACHINE-FILE-STATUS-BYTE1   PIC 9 VALUE 0.                       
005000         88  MACHINE-EOF VALUE 1.                                         
005100     05  SECOND-BYTE PIC X.                                               
005200 01  REPORT-FILE-STATUS.                                                  
005300     05  REPORT-FILE-STATUS-BYTE1    PIC 9 VALUE 0.                       
005400     05  REPORT-FILE-STATUS-BYTE2    PIC X.                               
005500 01  STATISTICAL-DATA.                                                    
005600     05  MACHINE-RECORDS-READ        PIC S9(8) COMP VALUE +0.             
005700 01  CURRENT-MODULE                  PIC X(8) VALUE SPACES.               
005800 01  CURRENT-USRDAT                  PIC X(4) VALUE SPACES.               
005900 01  ATTRIBUTES-OF-MODULE.                                                
006000     05  COBOL-CODE-COUNT            PIC S9(8) COMP VALUE +0.             
006100     05  COBOL-CODE-FOUND-SWITCH     PIC X(1) VALUE '0'.                  
006200         88 COBOL-CODE-IN-MODULE     VALUE '1'.                           
006300     05  RES-CODE-FOUND-SWITCH       PIC X(1) VALUE '0'.                  
006400         88 RES-CODE-FOUND-IN-MODULE VALUE '1'.                           
006500     05  RES-CODE-COUNT              PIC S9(8) COMP VALUE +0.             
006600     05  NORES-CODE-FOUND-SWITCH     PIC X(1) VALUE '0'.                  
006700         88 NORES-CODE-FOUND-IN-MODULE VALUE '1'.                         
006800     05  NORES-CODE-COUNT            PIC S9(8) COMP VALUE +0.             
006900 01  DSNAMES-ARRAY.                                                       
007000     05  FILLER PIC X(4) VALUE 'EOLL'.                                    
007100     05  FILLER PIC X(33) VALUE 'EMVSP.OUTPUT.LOADLIB'.                   
007300     05  FILLER PIC X(4) VALUE 'IMSQ'.                                    
007400     05  FILLER PIC X(33) VALUE 'EMVSP.IMSVS.PGMLIB'.                     
007600     05  FILLER PIC X(4) VALUE 'KX01'.                                    
007700     05  FILLER PIC X(33) VALUE  'EMVSP.CICS.LOADLIB'.                    
007800     05  FILLER PIC X(4) VALUE 'KX02'.                                    
007900     05  FILLER PIC X(33) VALUE  'EMVSQ.CICS.LOADLIB'.                    
008000     05  FILLER PIC X(4) VALUE 'KX03'.                                    
008100     05  FILLER PIC X(33) VALUE  'EMVSP.HCS.LOADLIB'.                     
008200     05  FILLER PIC X(4) VALUE 'KX04'.                                    
008300     05  FILLER PIC X(33) VALUE  'ECC.PRD.MDARNDP.LINKLIB'.               
008400     05  FILLER PIC X(4) VALUE 'KX05'.                                    
008500     05  FILLER PIC X(33) VALUE                                           
008510         'ECC.PRD.MDDCTI.V20000.LINKLIB'.                                 
008600     05  FILLER PIC X(4) VALUE 'KX06'.                                    
008700     05  FILLER PIC X(33) VALUE                                           
008710         'ECC.PRD.MDDOAI.V20000.PTF.LINKLIB'.                             
008800     05  FILLER PIC X(4) VALUE 'KX07'.                                    
008900     05  FILLER PIC X(33) VALUE  'CICS.PROD.USERLOAD'.                    
009000     05  FILLER PIC X(4) VALUE 'KX08'.                                    
009100     05  FILLER PIC X(33) VALUE  'CICS.TEST.USER.LOADLIB'.                
009200     05  FILLER PIC X(4) VALUE 'KX09'.                                    
009300     05  FILLER PIC X(33) VALUE  'CICS.MAINT.USER.LOADLIB'.               
009400     05  FILLER PIC X(4) VALUE 'KX10'.                                    
009500     05  FILLER PIC X(33) VALUE  'CICS.MAINT.OMNIDESK.LOADLIB'.           
009600     05  FILLER PIC X(4) VALUE 'KX11'.                                    
009700     05  FILLER PIC X(33) VALUE  'CICS.PROD.COINSERV.LOADLIB'.            
009800     05  FILLER PIC X(4) VALUE 'KX12'.                                    
009900     05  FILLER PIC X(33) VALUE  'DBEXCEL.PRD.ALT.LOADLIB'.               
010000     05  FILLER PIC X(4) VALUE 'KX13'.                                    
010100     05  FILLER PIC X(33) VALUE  'DRG.PRD.CCEAP981.LOADLIBA'.             
010200     05  FILLER PIC X(4) VALUE 'KX14'.                                    
010300     05  FILLER PIC X(33) VALUE  'DRG.PRD.CCEAP991.LOADLIBA'.             
010400     05  FILLER PIC X(4) VALUE 'KX15'.                                    
010500     05  FILLER PIC X(33) VALUE  'DRG.PRD.CCE981.LOADLIBH'.               
010600     05  FILLER PIC X(4) VALUE 'KX16'.                                    
010700     05  FILLER PIC X(33) VALUE  'DRG.PRD.CCE982.LOADLIBA'.               
010800     05  FILLER PIC X(4) VALUE 'KX17'.                                    
010900     05  FILLER PIC X(33) VALUE  'DRG.PRD.CCE982.LOADLIBH'.               
011000     05  FILLER PIC X(4) VALUE 'KX18'.                                    
011100     05  FILLER PIC X(33) VALUE  'DRG.PRD.CCE991.LOADLIBH'.               
011200     05  FILLER PIC X(4) VALUE 'KX19'.                                    
011300     05  FILLER PIC X(33) VALUE  'GIBC.PRODCICS'.                         
011400     05  FILLER PIC X(4) VALUE 'KX20'.                                    
011500     05  FILLER PIC X(33) VALUE 'INTERQ.PROD.COB2.LOAD'.                  
011600     05  FILLER PIC X(4) VALUE HIGH-VALUES.                               
011700     05  FILLER PIC X(33) VALUE                                           
011800         '????UNKNOWN.LOADLIB?????         '.                             
011900                                                                          
012000 01  DSNAME-TABLE REDEFINES DSNAMES-ARRAY.                                
012100     05  DSNAME-TABLE-ENTRY               OCCURS 23 TIMES                 
012200                                          INDEXED BY I.                   
012300         10  TBL-ARG                      PIC X(4).                       
012400         10  TBL-DSNAME                   PIC X(33).                      
012500                                                                          
012600 PROCEDURE DIVISION.                                                      
012700 0000-EXECUTIVE-CONTROL.                                                  
012800     PERFORM 9900-INITIALIZATION.                                         
012900     PERFORM 1000-MAINLINE UNTIL MACHINE-EOF.                             
013000     PERFORM 9990-END-OF-JOB.                                             
013100     GOBACK.                                                              
013200                                                                          
013300 1000-MAINLINE.                                                           
013400      READ MACHINE-FILE                                                   
013500         AT END PERFORM 8000-END-OF-FILE.                                 
013600      IF MACHINE-EOF                                                      
013700          NEXT SENTENCE                                                   
013800      ELSE                                                                
013900          ADD +1 TO MACHINE-RECORDS-READ                                  
014000          PERFORM 2000-CHECK-MODULE-CHANGE                                
014100          PERFORM 3000-PROCESS-THIS-CSECT.                                
014200                                                                          
014300 2000-CHECK-MODULE-CHANGE.                                                
014400      IF (MRMEM NOT EQUAL CURRENT-MODULE) OR                              
014500          (MRUSRDAT NOT EQUAL CURRENT-USRDAT)                             
014600              PERFORM 7000-MODULE-ANALYSIS                                
014700              PERFORM 6000-INITIALIZE-NEW-MODULE.                         
014800                                                                          
014900 3000-PROCESS-THIS-CSECT.                                                 
015000      IF MRTYPE = 'VO'                                                    
015100          MOVE '1' TO NORES-CODE-FOUND-SWITCH                             
015200          ADD +1 TO NORES-CODE-COUNT                                      
015300          MOVE '1' TO COBOL-CODE-FOUND-SWITCH                             
015400          ADD +1 TO   COBOL-CODE-COUNT.                                   
015500      IF MRTYPE = 'V4'                                                    
015600          MOVE '1' TO COBOL-CODE-FOUND-SWITCH                             
015700          ADD +1 TO   COBOL-CODE-COUNT                                    
015800          IF MROCRES EQUAL '1'                                            
015900              MOVE '1' TO RES-CODE-FOUND-SWITCH                           
016000              ADD +1 TO RES-CODE-COUNT                                    
016100          ELSE                                                            
016200              MOVE '1' TO NORES-CODE-FOUND-SWITCH                         
016300              ADD +1 TO NORES-CODE-COUNT.                                 
016400      IF MRTYPE = 'VS'                                                    
016500          MOVE '1' TO COBOL-CODE-FOUND-SWITCH                             
016600          ADD +1 TO   COBOL-CODE-COUNT                                    
016700          IF MROCRES EQUAL '1'                                            
016800              MOVE '1' TO RES-CODE-FOUND-SWITCH                           
016900              ADD +1 TO RES-CODE-COUNT                                    
017000          ELSE                                                            
017100              MOVE '1' TO NORES-CODE-FOUND-SWITCH                         
017200              ADD +1 TO NORES-CODE-COUNT.                                 
017300      IF MRTYPE = 'C2'                                                    
017400          MOVE '1' TO COBOL-CODE-FOUND-SWITCH                             
017500          ADD +1 TO   COBOL-CODE-COUNT                                    
017600          IF MRC2RES EQUAL '1'                                            
017700              MOVE '1' TO RES-CODE-FOUND-SWITCH                           
017800              ADD +1 TO RES-CODE-COUNT                                    
017900          ELSE                                                            
018000              MOVE '1' TO NORES-CODE-FOUND-SWITCH                         
018100              ADD +1 TO NORES-CODE-COUNT.                                 
018200      IF MRTYPE = 'C3'                                                    
018300          MOVE '1' TO RES-CODE-FOUND-SWITCH                               
018400          ADD +1 TO RES-CODE-COUNT                                        
018500          MOVE '1' TO COBOL-CODE-FOUND-SWITCH                             
018600          ADD +1 TO   COBOL-CODE-COUNT.                                   
018700      IF MRTYPE = 'PM' OR 'P1' OR 'P2' OR 'PR'                            
018800          MOVE 'PL/1' TO RPT-PLI.                                         
018900      IF MRTYPE = 'CS' OR 'CE' OR 'C1' OR 'CL'                            
019000          MOVE 'C-LANG' TO RPT-C370.                                      
019100      IF MRCSECT = 'ILBOD01' OR 'ILBODBE' OR 'ILBOPRM'                    
019200          OR 'ILBOSND' OR 'ILBOSTN' OR 'ILBOTC2'                          
019300              MOVE MRCSECT TO RPT-ILBO.                                   
019400      IF MRCSECT = 'ILBOSTP0' OR 'IGZERRE'                                
019500              MOVE MRCSECT TO RPT-STPRRE.                                 
019600      IF MRCSECT = 'IGZETUN'                                              
019700              MOVE MRCSECT TO RPT-TUNE.                                   
019800      IF MRCSECT = 'PLICALLA' OR 'PLICALLB'                               
019900              MOVE 'PL/1' TO RPT-PLI.                                     
020000                                                                          
020100 6000-INITIALIZE-NEW-MODULE.                                              
020200     MOVE MRMEM TO CURRENT-MODULE                                         
020300     MOVE MRUSRDAT TO CURRENT-USRDAT                                      
020400     MOVE '0' TO COBOL-CODE-FOUND-SWITCH                                  
020500     MOVE '0' TO NORES-CODE-FOUND-SWITCH                                  
020600     MOVE +0  TO NORES-CODE-COUNT.                                        
020700     MOVE '0' TO RES-CODE-FOUND-SWITCH                                    
020800     MOVE +0  TO RES-CODE-COUNT.                                          
020900     MOVE +0  TO COBOL-CODE-COUNT.                                        
021000                                                                          
021100 7000-MODULE-ANALYSIS.                                                    
021200     IF COBOL-CODE-IN-MODULE                                              
021300         AND NORES-CODE-FOUND-IN-MODULE                                   
021400         AND RES-CODE-FOUND-IN-MODULE                                     
021500             MOVE 'RES&NORES' TO RPT-RNR.                                 
021600     IF RPT-ANY-REASON = ALL SPACES                                       
021700         NEXT SENTENCE                                                    
021800     ELSE                                                                 
021900         MOVE CURRENT-MODULE TO RPT-MEMBER-NAME                           
022000         PERFORM VARYING I FROM 1 BY 1                                    
022100             UNTIL TBL-ARG(I) = CURRENT-USRDAT                            
022200             OR TBL-ARG(I) = HIGH-VALUES                                  
022300         END-PERFORM                                                      
022400         MOVE TBL-DSNAME(I) TO RPT-DSNAME                                 
022500         WRITE REPORT-RECORD                                              
022600         MOVE SPACES TO REPORT-RECORD.                                    
022700                                                                          
022800 8000-END-OF-FILE.                                                        
022900      IF MACHINE-RECORDS-READ EQUAL ZERO                                  
023000          DISPLAY 'NULL MACHINE INPUT FILE ENCOUNTERED - INVALID'         
023100          CALL 'ILBOABN0'                                                 
023200      ELSE                                                                
023300          IF (MACHINE-FILE-STATUS-BYTE1 NOT EQUAL 0)                      
023400              AND (MACHINE-FILE-STATUS-BYTE1 NOT EQUAL 1)                 
023500     DISPLAY 'UNEXPECTED FILE STATUS AFTER MACHINE FILE READ ='           
023600                      MACHINE-FILE-STATUS                                 
023700                  CALL 'ILBOABN0'.                                        
023800                                                                          
023900 9900-INITIALIZATION.                                                     
024000     OPEN INPUT MACHINE-FILE.                                             
024100     IF (MACHINE-FILE-STATUS-BYTE1 NOT EQUAL 0)                           
024200         AND (MACHINE-FILE-STATUS-BYTE1 NOT EQUAL 1)                      
024300           DISPLAY 'UNEXPECTED FILE STATUS AFTER MACHINE OPEN = '         
024400                 MACHINE-FILE-STATUS                                      
024500             CALL 'ILBOABN0'.                                             
024600     OPEN OUTPUT REPORT-FILE.                                             
024700     IF ( REPORT-FILE-STATUS-BYTE1 NOT EQUAL 0 )                          
024800       DISPLAY 'UNEXPECTED FILE STATUS AFTER REPORT FILE OPEN = '         
024900             REPORT-FILE-STATUS                                           
025000         CALL 'ILBOABN0'.                                                 
025100     MOVE SPACES TO REPORT-RECORD.                                        
025200                                                                          
025300 9990-END-OF-JOB.                                                         
025400      PERFORM 7000-MODULE-ANALYSIS                                        
025500     CLOSE MACHINE-FILE.                                                  
025600     CLOSE REPORT-FILE.                                                   
  