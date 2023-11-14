000100 ID DIVISION.                                                             
000200 PROGRAM-ID. CMPRMACH                                                     
000300**************************************************************            
000400*    READ THE MACHINE FILES CREATED BY EDGE RUNS AGAINST     *            
000500*    TWO LIBRARIES AND COMPARES THEM TO IDENTIFY THE DIFF-   *            
000600*    ERENCES BETWEEN THE LIBRARIES.                          *            
000700*    1) THE OLD FILE INPUT IS THE EDGE MACHINE FILE FROM A   *            
000800*       LOAD LIBRARY CONTAINING PROGRAMS LINKED WITH THE OLD *            
000900*       COBOL II LIBRARY, SYS1.COB2LIB.                      *            
001000*    2) THE NEW FILE INPUT IS THE EDGE MACHINE FILE FROM A   *            
001100*       LOAD LIBRARY CONTAINING THE SAME PROGRAMS AS IN THE  *            
001200*       OLD FILE AFTER THEY HAVE BEEN RELINKED WITH THE L.E. *            
001300*       LIBRARY, CEE.SCEELKED.                               *            
001400**************************************************************            
001500 ENVIRONMENT DIVISION.                                                    
001600 CONFIGURATION SECTION.                                                   
001700 SOURCE-COMPUTER. IBM-370.                                                
001800 OBJECT-COMPUTER. IBM-370.                                                
001900 INPUT-OUTPUT SECTION.                                                    
002000 FILE-CONTROL.                                                            
002100     SELECT OLD-FILE ASSIGN TO UT-S-OLD.                                  
002300     SELECT NEW-FILE ASSIGN TO UT-S-NEW.                                  
002500     SELECT REPORT-FILE ASSIGN TO UT-S-REPORT.                            
002700 DATA DIVISION.                                                           
002800 FILE SECTION.                                                            
002900 FD  OLD-FILE                                                             
003000     BLOCK CONTAINS 0 RECORDS                                             
003100     RECORD CONTAINS 200 CHARACTERS                                       
003200     RECORDING F.                                                         
003300 01  OLD-RECORD.                                                          
003400     05 CSECT-DATA.                                                       
003500        10 MEMBER-NAME               PIC X(8).                            
003600        10 MEMBER-AMODE              PIC X(1).                            
003700           88 MEMBER-AMODE-24           VALUE '2'.                        
003800           88 MEMBER-AMODE-31           VALUE '3'.                        
003900           88 MEMBER-AMODE-ANY          VALUE 'A'.                        
004000        10 MEMBER-RMODE              PIC X(1).                            
004100           88 MEMBER-RMODE-24           VALUE '2'.                        
004200           88 MEMBER-RMODE-31           VALUE '3'.                        
004300           88 MEMBER-RMODE-ANY          VALUE 'A'.                        
004400        10 CSECTNAME                 PIC X(8).                            
004500        10 COMPILER-ID               PIC X(10).                           
004600        10 COMPILER-VVMM             PIC X(04).                           
004700        10 COMPILE-DATE-YYDDD        PIC 9(05).                           
004800        10 CSECT-AMODE               PIC X.                               
004900           88 CSECT-AMODE-24            VALUE '2'.                        
005000           88 CSECT-AMODE-31            VALUE '3'.                        
005100           88 CSECT-AMODE-ANY           VALUE 'A'.                        
005200           88 CSECT-AMODE-PREXA         VALUE ' '.                        
005300        10 CSECT-RMODE               PIC X.                               
005400           88 CSECT-RMODE-24            VALUE '2'.                        
005500           88 CSECT-RMODE-ANY           VALUE 'A'.                        
005600        10 CSECT-TYPE                PIC  X(02).                          
005700           88 ASSEMBLER-PROGRAM         VALUE 'AR'.                       
005800           88 BASIC-PROGRAM             VALUE 'BA'.                       
005900           88 DEBUG-TOOL                VALUE 'BG'.                       
006000           88 BERKELEY-SOCKET           VALUE 'BS'.                       
006100           88 CICS-INTERFACE            VALUE 'CC'.                       
006200           88 C370-RUNTIME-ROUTINE      VALUE 'CE'.                       
006300           88 C370-PROGRAM              VALUE 'CI'.                       
006400           88 LATTICE-C-PROGRAM         VALUE 'CL'.                       
006500           88 COMMON-SECTION            VALUE 'CM'.                       
006600           88 COBOL-RUNTIME-ROUTINE     VALUE 'CR'.                       
006700           88 SAS-C-PROGRAM             VALUE 'CS'.                       
006800           88 C-CPLUSPLUS-FOR-OS390     VALUE 'CP'.                       
006900           88 COBOL-II-PROGRAM          VALUE 'C2'.                       
007000           88 COBOL-370-390-PROGRAM     VALUE 'C3'.                       
007100           88 DCE-ROUTINE               VALUE 'DC'.                       
007200           88 DB2-INTERFACE             VALUE 'DI'.                       
007300           88 EASYTRIEVE-PLUS-PROGRAM   VALUE 'EZ'.                       
007400           88 FORTRAN-G-G1-PROGRAM      VALUE 'FG'.                       
007500           88 FORTRAN-H-PROGRAM         VALUE 'FH'.                       
007600           88 FORTRAN-RUNTIME-ROUTINE   VALUE 'FR'.                       
007700           88 VS-FORTRAN-PROGRAM        VALUE 'FV'.                       
007800           88 IMS-INTERFACE             VALUE 'II'.                       
007900           88 ISPF-INTERFACE            VALUE 'IS'.                       
008000           88 LE-RUNTIME-ROUTINE        VALUE 'LE'.                       
008100           88 MARK-V-PROGRAM            VALUE 'MV'.                       
008200           88 NOT-COMPLETELY-ANALYZED   VALUE 'NA'.                       
008300           88 UNRECOGNIZED-CSECT-TYPE   VALUE 'NR'.                       
008400           88 VA-PLI-FOR-OS390-PROGRAM  VALUE 'PL'.                       
008500           88 PLI-FOR-MVS-AND-VM-PROGRAM VALUE 'PM'.                      
008600           88 PLI-RUNTIME-ROUTINE       VALUE 'PR'.                       
008700           88 VS-PASCAL-PROGRAM         VALUE 'PV'.                       
008800           88 PLI-OPTIMIZER-V1-PROGRAM  VALUE 'P1'.                       
008900           88 PLI-OPTIMIZER-V2-PROGRAM  VALUE 'P2'.                       
009000           88 IBM-REXX-PROGRAM          VALUE 'RE'.                       
009100           88 OLD-LANGUAGE-RUNTIME-ROUTINE VALUE 'RE'.                    
009200           88 SNAPAID-INTERFACE         VALUE 'SA'.                       
009300           88 COBOL-SAMPLE-PROGRAM      VALUE 'SC'.                       
009400           88 USER-RECOGNIZED-ASSEMBLER VALUE 'UR'.                       
009500           88 VERY-OLD-COBOL            VALUE 'VO'.                       
009600           88 OSVS-COBOL-V2REL3         VALUE 'VS'.                       
009700           88 OSVS-COBOL-V2REL4         VALUE 'V4'.                       
009800        10 DATE-TIME-FORMAT          PIC X.                               
009900           88 SVC-PATTERN-DATE-TIME     VALUE '1'.                        
010000           88 STCK-DATE-TIME            VALUE '2'.                        
010100           88 BOTH-SVC-AND-STCK         VALUE '3'.                        
010200           88 COBOL-ACCEPT-FROM-FORMAT  VALUE '4'.                        
010300           88 COBOL-ACCEPT-OTHER-FORMAT VALUE '9'.                        
010400        10 LOAD-MODULE-ATTRIBUTES    PIC X.                               
010500           88 REUSABLE-MODULE           VALUE '1'.                        
010600           88 RENT-NOT-REUS-MODULE      VALUE '2'.                        
010700           88 RENT-AND-REUS-MODULE      VALUE '3'.                        
010800           88 OVERLAY-DATA-SUSPECT      VALUE '8'.                        
010900           88 MODULE-NOT-REUSABLE       VALUE ' '.                        
011000        10 PRIMARY-ENTRY-POINT-NAME  PIC X(8).                            
011100        10 CSECT-SIZE-IN-DECIMAL     PIC 9(08).                           
011200        10 SLASH-U-PARM-DATA         PIC X(04).                           
011300        10 LAST-LINKEDIT-DATE-YYDDD  PIC 9(05).                           
011400     05 COBOL-II-AND-COBOL-370-FIELDS.                                    
011500        10 COBOL-II-COMPILER-VERSION PIC X(06).                           
011600        10 COBOL-II-LEVEL            PIC X(04).                           
011700        10 COBOL-II-DATE-COMPILED    PIC X(9).                            
011800        10 COBOL-II-TIME-COMPILED    PIC X(8).                            
011900        10 COBOL-II-ADV-INDICATOR    PIC X(1).                            
012000           88 COBOL-II-ADV              VALUE '1'.                        
012100           88 COBOL-II-NOADV            VALUE '0'.                        
012200        10 COBOL-II-QUOTE-APOST-INDICATOR PIC X.                          
012300           88 COBOL-II-QUOTE            VALUE '0'.                        
012400           88 COBOL-II-APOST            VALUE '1'.                        
012500        10 COBOL-II-DATA-INDICATOR   PIC X.                               
012600           88 COBOL-II-DATA-24          VALUE '0'.                        
012700           88 COBOL-II-DATA-31          VALUE '1'.                        
012800        10 COBOL-II-DECK-INDICATOR   PIC X.                               
012900           88 COBOL-II-NODECK           VALUE '0'.                        
013000           88 COBOL-II-DECK             VALUE '1'.                        
013100        10 COBOL-II-DUMP-INDICATOR   PIC X.                               
013200           88 COBOL-II-NODUMP           VALUE '0'.                        
013300           88 COBOL-II-DUMP             VALUE '1'.                        
013400        10 COBOL-II-DYNAM-INDICATOR  PIC X.                               
013500           88 COBOL-II-NODYNAM          VALUE '0'.                        
013600           88 COBOL-II-DYNAM            VALUE '1'.                        
013700        10 COBOL-II-FASTSRT-INDICATOR PIC X.                              
013800           88 COBOL-II-NOFASTSRT        VALUE '0'.                        
013900           88 COBOL-II-FASTSRT          VALUE '1'.                        
014000        10 COBOL-II-FDUMP-INDICATOR  PIC X.                               
014100           88 COBOL-II-NOFDUMP          VALUE '0'.                        
014200           88 COBOL-II-FDUMP            VALUE '1'.                        
014300        10 COBOL-II-LIB-INDICATOR    PIC X.                               
014400           88 COBOL-II-NOLIB            VALUE '0'.                        
014500           88 COBOL-II-LIB              VALUE '1'.                        
014600        10 COBOL-II-LIST-INDICATOR   PIC X.                               
014700           88 COBOL-II-NOLIST           VALUE '0'.                        
014800           88 COBOL-II-LIST             VALUE '1'.                        
014900        10 COBOL-II-MAP-INDICATOR    PIC X.                               
015000           88 COBOL-II-NOMAP            VALUE '0'.                        
015100           88 COBOL-II-MAP              VALUE '1'.                        
015200        10 COBOL-II-NUM-INDICATOR    PIC X.                               
015300           88 COBOL-II-NONUM            VALUE '0'.                        
015400           88 COBOL-II-NUM              VALUE '1'.                        
015500        10 COBOL-II-OBJ-INDICATOR    PIC X.                               
015600           88 COBOL-II-NOOBJ            VALUE '0'.                        
015700           88 COBOL-II-OBJ              VALUE '1'.                        
015800        10 COBOL-II-OFFSET-INDICATOR  PIC X.                              
015900           88 COBOL-II-NOOFFSET         VALUE '0'.                        
016000           88 COBOL-II-OFFSET           VALUE '1'.                        
016100        10 COBOL-II-OPTIMIZ-INDICATOR PIC X.                              
016200           88 COBOL-II-NOOPTIMIZ        VALUE '0'.                        
016300           88 COBOL-II-OPTIMIZ          VALUE '1'.                        
016400        10 COBOL-II-DD-INDICATOR     PIC X.                               
016500           88 COBOL-II-DD               VALUE '0'.                        
016600           88 COBOL-II-OUTDD            VALUE '1'.                        
016700        10 COBOL-II-PFD-INDICATOR    PIC X.                               
016800           88 COBOL-II-NOPFDSGN         VALUE '0'.                        
016900           88 COBOL-II-PFDSGN           VALUE '1'.                        
017000           88 COBOL-390-NUMNPFD          VALUE '0'.                       
017100           88 COBOL-390-NUMPFD           VALUE '1'.                       
017200           88 COBOL-390-NUMMIG           VALUE '2'.                       
017300        10 COBOL-II-RENT-INDICATOR   PIC X.                               
017400           88 COBOL-II-NORENT           VALUE '0'.                        
017500           88 COBOL-II-RENT             VALUE '1'.                        
017600        10 COBOL-II-RES-INDICATOR    PIC X.                               
017700           88 COBOL-II-NORES            VALUE '0'.                        
017800           88 COBOL-II-RES              VALUE '1'.                        
017900        10 COBOL-II-SEQ-INDICATOR    PIC X.                               
018000           88 COBOL-II-NOSEQ            VALUE '0'.                        
018100           88 COBOL-II-SEQ              VALUE '1'.                        
018200        10 COBOL-II-SIZE-INDICATOR   PIC X.                               
018300           88 COBOL-II-SIZE-NBR         VALUE '0'.                        
018400           88 COBOL-II-SIZE-MAX         VALUE '1'.                        
018500        10 COBOL-II-SOURCE-INDICATOR PIC X.                               
018600           88 COBOL-II-NOSOURCE         VALUE '0'.                        
018700           88 COBOL-II-SOURCE           VALUE '1'.                        
018800        10 COBOL-II-SSRANGE-INDICATOR PIC X.                              
018900           88 COBOL-II-NOSSRANGE        VALUE '0'.                        
019000           88 COBOL-II-SSRANGE          VALUE '1'.                        
019100        10 COBOL-II-TERM-INDICATOR   PIC X.                               
019200           88 COBOL-II-NOTERM           VALUE '0'.                        
019300           88 COBOL-II-TERM             VALUE '1'.                        
019400        10 COBOL-II-TEST-INDICATOR   PIC X.                               
019500           88 COBOL-II-NOTEST           VALUE '0'.                        
019600           88 COBOL-II-TEST             VALUE '1'.                        
019700        10 COBOL-II-TRUNC-INDICATOR  PIC X.                               
019800           88 COBOL-II-NOTRUNC          VALUE '0'.                        
019900           88 COBOL-II-TRUNC            VALUE '1'.                        
020000           88 COBOL-390-TRUNCOPT        VALUE '0'.                        
020100           88 COBOL-390-TRUNC-STD       VALUE '1'.                        
020200           88 COBOL-390-TRUNC-BIN       VALUE '1'.                        
020300        10 COBOL-II-WORD-INDICATOR   PIC X.                               
020400           88 COBOL-II-WORD-DEFAULT     VALUE '0'.                        
020500           88 COBOL-II-WORD-SPECIFIED   VALUE '1'.                        
020600        10 COBOL-II-VBREF-INDICATOR  PIC X.                               
020700           88 COBOL-II-NOVBREF          VALUE '0'.                        
020800           88 COBOL-II-VBREF            VALUE '1'.                        
020900        10 COBOL-II-XREF-INDICATOR   PIC X.                               
021000           88 COBOL-II-NOXREF           VALUE '0'.                        
021100           88 COBOL-II-XREF             VALUE '1'.                        
021200        10 COBOL-II-ZWB-INDICATOR    PIC X.                               
021300           88 COBOL-II-NOZWB            VALUE '0'.                        
021400           88 COBOL-II-ZWB              VALUE '1'.                        
021500        10 COBOL-II-CMPR2-INDICATOR  PIC X.                               
021600           88 COBOL-II-NOCMPR2          VALUE '0'.                        
021700           88 COBOL-II-CMPR2            VALUE '1'.                        
021800        10 COBOL-II-NAME-INDICATOR   PIC X.                               
021900           88 COBOL-II-NONAME           VALUE '0'.                        
022000           88 COBOL-II-NAME             VALUE '1'.                        
022100        10 COBOL-II-NMCL-INDICATOR   PIC X.                               
022200           88 COBOL-II-NMCLPRI          VALUE '0'.                        
022300           88 COBOL-II-NMCLALT          VALUE '1'.                        
022400        10 COBOL-II-DBCS-INDICATOR   PIC X.                               
022500           88 COBOL-II-NODBCS           VALUE '0'.                        
022600           88 COBOL-II-DBCS             VALUE '1'.                        
022700        10 COBOL-II-AWO-INDICATOR    PIC X.                               
022800           88 COBOL-II-NOAWO            VALUE '0'.                        
022900           88 COBOL-II-AWO              VALUE '1'.                        
023000     05 COBOL-370-AND-390-FIELDS.                                         
023100        10 COBOL-390-EVENTS-INDICATOR PIC X.                              
023200           88 COBOL-390-NOEVENTS         VALUE '0'.                       
023300           88 COBOL-390-EVENTS           VALUE '1'.                       
023400        10 COBOL-390-CURRENCY-INDICATOR PIC X.                            
023500           88 COBOL-39-NOCURRENCY       VALUE '0'.                        
023600           88 COBOL-39-CURRENCY         VALUE '1'.                        
023700        10 COBOL-390-RMODE-INDICATOR PIC X.                               
023800           88 COBOL-39-RMODE-24         VALUE '0'.                        
023900           88 COBOL-39-RMODE-ANY        VALUE '1'.                        
024000        10 FILLER                    PIC X(4).                            
024100     05 ALL-COBOL-SORTMERGE-INDICATOR PIC X.                              
024200        88 SORT-OR-MERGE-FOUND          VALUE '1'.                        
024300        88 NO-SORT-OR-MERGE-FOUND       VALUE '0'.                        
024400     05 ALL-COBOL-DYNAM-CALL-FND-INDIC PIC X.                             
024500        88 NO-DYNAMIC-CALLS-FOUND       VALUE '0'.                        
024600        88 CALL-LITERAL-FORCED-DYNAMIC VALUE '1'.                         
024700        88 CALL-DATANAME                VALUE '2'.                        
024800        88 CALL-LIT-FRCD-AND-CALL-DATANM VALUE '3'.                       
024900        88 CAPEX-OPT-CALL-ADDR          VALUE '4'.                        
025000        88 CAPEX-CALL-LIT-FORCED-DYNAM VALUE '5'.                         
025100        88 CAPEX-OPT-CALL-DATANAME      VALUE '6'.                        
025200        88 CAPEX-CALL-LIT-FRCD-CALL-DTNM VALUE '7'.                       
025300   05 VERSION-4-AND-VS-COBOL-FIELDS.                                      
025400        10 VSCOBOL-SYMDUMP-INDICATOR PIC X.                               
025500           88 VSCOBOL-NOSYMDUMP         VALUE '0'.                        
025600           88 VSCOBOL-SYMDUMP           VALUE '1'.                        
025700        10 VSCOBOL-FLOW-INDICATOR    PIC X.                               
025800           88 VSCOBOL-NOFLOW            VALUE '0'.                        
025900           88 VSCOBOL-FLOW              VALUE '1'.                        
026000        10 VSCOBOL-STATE-INDICATOR   PIC X.                               
026100           88 VSCOBOL-NOSTATE           VALUE '0'.                        
026200           88 VSCOBOL-STATE             VALUE '1'.                        
026300        10 VSCOBOL-TEST-INDICATOR    PIC X.                               
026400           88 VSCOBOL-NOTEST            VALUE '0'.                        
026500           88 VSCOBOL-TEST              VALUE '1'.                        
026600        10 VSCOBOL-RES-INDICATOR     PIC X.                               
026700           88 VSCOBOL-NORES             VALUE '0'.                        
026800           88 VSCOBOL-RES               VALUE '1'.                        
026900        10 VSCOBOL-ENDJOB-INDICATOR  PIC X.                               
027000           88 VSCOBOL-NOENDJOB          VALUE '0'.                        
027100           88 VSCOBOL-ENDJOB            VALUE '1'.                        
027200        10 VSCOBOL-OBJ370-INDICATOR  PIC X.                               
027300           88 VSCOBOL-OBJ360            VALUE '0'.                        
027400           88 VSCOBOL-OBJ370            VALUE '1'.                        
027500        10 VSCOBOL-OPTIMIZED-INDICATOR PIC X.                             
027600           88 VSCOBOL-NOOPT             VALUE '0'.                        
027700           88 VSCOBOL-OPT               VALUE '1'.                        
027800        10 VSCOBOL-CAPEX-OPT-INDICATOR PIC X.                             
027900           88 VSCOBOL-NOCAPEX-OPT       VALUE '0'.                        
028000           88 VSCOBOL-CAPEX-OPT         VALUE '1'.                        
028100        10 VSCOBOL-CAPEX-DTECT-INDICATOR PIC X.                           
028200           88 VSCOBOL-NOCAPEX-DTECT     VALUE '0'.                        
028300           88 VSCOBOL-CAPEX-DTECT       VALUE '1'.                        
028400        10 VSCOBOL-COUNT-INDICATOR   PIC X.                               
028500           88 VSCOBOL-NOCOUNT           VALUE '0'.                        
028600           88 VSCOBOL-COUNT             VALUE '1'.                        
028700        10 VSCOBOL-TRACE-INDICATOR   PIC X.                               
028800           88 VSCOBOL-NOTRACE           VALUE '0'.                        
028900           88 VSCOBOL-TRACE             VALUE '1'.                        
029000        10 VSCOBOL-LANGLEVEL-INDICATOR PIC X.                             
029100           88 VSCOBOL-LANGLEVEL-1       VALUE '1'.                        
029200           88 VSCOBOL-LANGLEVEL-2       VALUE '2'.                        
029300        10 VSCOBOL-LAST-SYSOU-DDNM-CHAR PIC X.                            
029400        10 FILLER                    PIC X(3).                            
029500     05 PLI-V2-FIELDS.                                                    
029600        10 PLI-V2-MAIN-INDICATOR     PIC X.                               
029700           88 PLI-V2-PROC-OPTIONS-MAIN  VALUE '1'.                        
029800           88 PLI-V2-NO-PROC-OPTIONS-MAIN VALUE '0'.                      
029900        10 PLI-V2-REORDER-INDICATOR  PIC X.                               
030000           88 PLI-V2-NOREORDER          VALUE '0'.                        
030100           88 PLI-V2-REORDER            VALUE '1'.                        
030200        10 PLI-V2-EXECOPS-INDICATOR  PIC X.                               
030300           88 PLI-V2-NOEXECOPS          VALUE '0'.                        
030400           88 PLI-V2-EXECOPS            VALUE '1'.                        
030500        10 PLI-V2-CMPAT-INDICATOR    PIC X.                               
030600           88 PLI-V2-CMPAT-V1           VALUE '1'.                        
030700           88 PLI-V2-CMPAT-V2           VALUE '2'.                        
030800        10 PLI-V2-GRAPHIC-INDICATOR  PIC X.                               
030900           88 PLI-V2-NOGRAPHIC          VALUE '0'.                        
031000           88 PLI-V2-GRAPHIC            VALUE '1'.                        
031100        10 PLI-V2-OPTIMIZE-INDICATOR PIC X.                               
031200           88 PLI-V2-NOOPTIMIZE         VALUE '0'.                        
031300           88 PLI-V2-OPTIMIZE           VALUE '1'.                        
031400        10 PLI-V2-INTERUPT-INDICATOR PIC X.                               
031500           88 PLI-V2-NOINTERUPT         VALUE '0'.                        
031600           88 PLI-V2-INTERUPT           VALUE '1'.                        
031700        10 PLI-V2-FLOW-INDICATOR     PIC X.                               
031800           88 PLI-V2-NOFLOW             VALUE '0'.                        
031900           88 PLI-V2-FLOW               VALUE '1'.                        
032000        10 PLI-V2-COUNT-INDICATOR    PIC X.                               
032100           88 PLI-V2-NOCOUNT            VALUE '0'.                        
032200           88 PLI-V2-COUNT              VALUE '1'.                        
032300        10 PLI-V2-TEST-INDICATOR     PIC X.                               
032400           88 PLI-V2-NOTEST             VALUE '0'.                        
032500           88 PLI-V2-TEST               VALUE '1'.                        
032600        10 PLI-V2-SYSTEM-INDICATOR   PIC X.                               
032700           88 PLI-V2-SYSTEM-CMS         VALUE '1'.                        
032800           88 PLI-V2-SYSTEM-CMSTP       VALUE '2'.                        
032900           88 PLI-V2-SYSTEM-MVS         VALUE '3'.                        
033000           88 PLI-V2-SYSTEM-TSO         VALUE '4'.                        
033100           88 PLI-V2-SYSTEM-CICS        VALUE '5'.                        
033200           88 PLI-V2-SYSTEM-IMS         VALUE '6'.                        
033300        10 PLI-V2-CSECT-INDICATOR    PIC X.                               
033400           88 PLI-V2-RUNTIME-CSECT      VALUE '1'.                        
033500           88 PLI-V2-EXTERNAL-DATA-CSECT VALUE '2'.                       
033600           88 PLI-V2-STATIC-DATA-CSECT  VALUE '3'.                        
033700           88 PLI-V2-CODE-CSECT         VALUE '4'.                        
033800     05 FILLER                       PIC X(28).                           
033900     05 WINDOW-INDICATOR             PIC X(4).                            
034000                                                                          
034100 FD  NEW-FILE                                                             
034200     BLOCK CONTAINS 0 RECORDS                                             
034300     RECORD CONTAINS 200 CHARACTERS                                       
034400     RECORDING F.                                                         
034500 01  NEW-RECORD.                                                          
034600     05 CSECT-DATA.                                                       
034700        10 MEMBER-NAME               PIC X(8).                            
034800        10 MEMBER-AMODE              PIC X(1).                            
034900           88 MEMBER-AMODE-24           VALUE '2'.                        
035000           88 MEMBER-AMODE-31           VALUE '3'.                        
035100           88 MEMBER-AMODE-ANY          VALUE 'A'.                        
035200        10 MEMBER-RMODE              PIC X(1).                            
035300           88 MEMBER-RMODE-24           VALUE '2'.                        
035400           88 MEMBER-RMODE-31           VALUE '3'.                        
035500           88 MEMBER-RMODE-ANY          VALUE 'A'.                        
035600        10 CSECTNAME                 PIC X(8).                            
035700        10 COMPILER-ID               PIC X(10).                           
035800        10 COMPILER-VVMM             PIC X(04).                           
035900        10 COMPILE-DATE-YYDDD        PIC 9(05).                           
036000        10 CSECT-AMODE               PIC X.                               
036100           88 CSECT-AMODE-24            VALUE '2'.                        
036200           88 CSECT-AMODE-31            VALUE '3'.                        
036300           88 CSECT-AMODE-ANY           VALUE 'A'.                        
036400           88 CSECT-AMODE-PREXA         VALUE ' '.                        
036500        10 CSECT-RMODE               PIC X.                               
036600           88 CSECT-RMODE-24            VALUE '2'.                        
036700           88 CSECT-RMODE-ANY           VALUE 'A'.                        
036800        10 CSECT-TYPE                PIC  X(02).                          
036900           88 ASSEMBLER-PROGRAM         VALUE 'AR'.                       
037000           88 BASIC-PROGRAM             VALUE 'BA'.                       
037100           88 DEBUG-TOOL                VALUE 'BG'.                       
037200           88 BERKELEY-SOCKET           VALUE 'BS'.                       
037300           88 CICS-INTERFACE            VALUE 'CC'.                       
037400           88 C370-RUNTIME-ROUTINE      VALUE 'CE'.                       
037500           88 C370-PROGRAM              VALUE 'CI'.                       
037600           88 LATTICE-C-PROGRAM         VALUE 'CL'.                       
037700           88 COMMON-SECTION            VALUE 'CM'.                       
037800           88 COBOL-RUNTIME-ROUTINE     VALUE 'CR'.                       
037900           88 SAS-C-PROGRAM             VALUE 'CS'.                       
038000           88 C-CPLUSPLUS-FOR-OS390     VALUE 'CP'.                       
038100           88 COBOL-II-PROGRAM          VALUE 'C2'.                       
038200           88 COBOL-370-390-PROGRAM     VALUE 'C3'.                       
038300           88 DCE-ROUTINE               VALUE 'DC'.                       
038400           88 DB2-INTERFACE             VALUE 'DI'.                       
038500           88 EASYTRIEVE-PLUS-PROGRAM   VALUE 'EZ'.                       
038600           88 FORTRAN-G-G1-PROGRAM      VALUE 'FG'.                       
038700           88 FORTRAN-H-PROGRAM         VALUE 'FH'.                       
038800           88 FORTRAN-RUNTIME-ROUTINE   VALUE 'FR'.                       
038900           88 VS-FORTRAN-PROGRAM        VALUE 'FV'.                       
039000           88 IMS-INTERFACE             VALUE 'II'.                       
039100           88 ISPF-INTERFACE            VALUE 'IS'.                       
039200           88 LE-RUNTIME-ROUTINE        VALUE 'LE'.                       
039300           88 MARK-V-PROGRAM            VALUE 'MV'.                       
039400           88 NOT-COMPLETELY-ANALYZED   VALUE 'NA'.                       
039500           88 UNRECOGNIZED-CSECT-TYPE   VALUE 'NR'.                       
039600           88 VA-PLI-FOR-OS390-PROGRAM  VALUE 'PL'.                       
039700           88 PLI-FOR-MVS-AND-VM-PROGRAM VALUE 'PM'.                      
039800           88 PLI-RUNTIME-ROUTINE       VALUE 'PR'.                       
039900           88 VS-PASCAL-PROGRAM         VALUE 'PV'.                       
040000           88 PLI-OPTIMIZER-V1-PROGRAM  VALUE 'P1'.                       
040100           88 PLI-OPTIMIZER-V2-PROGRAM  VALUE 'P2'.                       
040200           88 IBM-REXX-PROGRAM          VALUE 'RE'.                       
040300           88 OLD-LANGUAGE-RUNTIME-ROUTINE VALUE 'RE'.                    
040400           88 SNAPAID-INTERFACE         VALUE 'SA'.                       
040500           88 COBOL-SAMPLE-PROGRAM      VALUE 'SC'.                       
040600           88 USER-RECOGNIZED-ASSEMBLER VALUE 'UR'.                       
040700           88 VERY-OLD-COBOL            VALUE 'VO'.                       
040800           88 OSVS-COBOL-V2REL3         VALUE 'VS'.                       
040900           88 OSVS-COBOL-V2REL4         VALUE 'V4'.                       
041000        10 DATE-TIME-FORMAT          PIC X.                               
041100           88 SVC-PATTERN-DATE-TIME     VALUE '1'.                        
041200           88 STCK-DATE-TIME            VALUE '2'.                        
041300           88 BOTH-SVC-AND-STCK         VALUE '3'.                        
041400           88 COBOL-ACCEPT-FROM-FORMAT  VALUE '4'.                        
041500           88 COBOL-ACCEPT-OTHER-FORMAT VALUE '9'.                        
041600        10 LOAD-MODULE-ATTRIBUTES    PIC X.                               
041700           88 REUSABLE-MODULE           VALUE '1'.                        
041800           88 RENT-NOT-REUS-MODULE      VALUE '2'.                        
041900           88 RENT-AND-REUS-MODULE      VALUE '3'.                        
042000           88 OVERLAY-DATA-SUSPECT      VALUE '8'.                        
042100           88 MODULE-NOT-REUSABLE       VALUE ' '.                        
042200        10 PRIMARY-ENTRY-POINT-NAME  PIC X(8).                            
042300        10 CSECT-SIZE-IN-DECIMAL     PIC 9(08).                           
042400        10 SLASH-U-PARM-DATA         PIC X(04).                           
042500        10 LAST-LINKEDIT-DATE-YYDDD  PIC 9(05).                           
042600     05 COBOL-II-AND-COBOL-370-FIELDS.                                    
042700        10 COBOL-II-COMPILER-VERSION PIC X(06).                           
042800        10 COBOL-II-LEVEL            PIC X(04).                           
042900        10 COBOL-II-DATE-COMPILED    PIC X(9).                            
043000        10 COBOL-II-TIME-COMPILED    PIC X(8).                            
043100        10 COBOL-II-ADV-INDICATOR    PIC X(1).                            
043200           88 COBOL-II-ADV              VALUE '1'.                        
043300           88 COBOL-II-NOADV            VALUE '0'.                        
043400        10 COBOL-II-QUOTE-APOST-INDICATOR PIC X.                          
043500           88 COBOL-II-QUOTE            VALUE '0'.                        
043600           88 COBOL-II-APOST            VALUE '1'.                        
043700        10 COBOL-II-DATA-INDICATOR   PIC X.                               
043800           88 COBOL-II-DATA-24          VALUE '0'.                        
043900           88 COBOL-II-DATA-31          VALUE '1'.                        
044000        10 COBOL-II-DECK-INDICATOR   PIC X.                               
044100           88 COBOL-II-NODECK           VALUE '0'.                        
044200           88 COBOL-II-DECK             VALUE '1'.                        
044300        10 COBOL-II-DUMP-INDICATOR   PIC X.                               
044400           88 COBOL-II-NODUMP           VALUE '0'.                        
044500           88 COBOL-II-DUMP             VALUE '1'.                        
044600        10 COBOL-II-DYNAM-INDICATOR  PIC X.                               
044700           88 COBOL-II-NODYNAM          VALUE '0'.                        
044800           88 COBOL-II-DYNAM            VALUE '1'.                        
044900        10 COBOL-II-FASTSRT-INDICATOR PIC X.                              
045000           88 COBOL-II-NOFASTSRT        VALUE '0'.                        
045100           88 COBOL-II-FASTSRT          VALUE '1'.                        
045200        10 COBOL-II-FDUMP-INDICATOR  PIC X.                               
045300           88 COBOL-II-NOFDUMP          VALUE '0'.                        
045400           88 COBOL-II-FDUMP            VALUE '1'.                        
045500        10 COBOL-II-LIB-INDICATOR    PIC X.                               
045600           88 COBOL-II-NOLIB            VALUE '0'.                        
045700           88 COBOL-II-LIB              VALUE '1'.                        
045800        10 COBOL-II-LIST-INDICATOR   PIC X.                               
045900           88 COBOL-II-NOLIST           VALUE '0'.                        
046000           88 COBOL-II-LIST             VALUE '1'.                        
046100        10 COBOL-II-MAP-INDICATOR    PIC X.                               
046200           88 COBOL-II-NOMAP            VALUE '0'.                        
046300           88 COBOL-II-MAP              VALUE '1'.                        
046400        10 COBOL-II-NUM-INDICATOR    PIC X.                               
046500           88 COBOL-II-NONUM            VALUE '0'.                        
046600           88 COBOL-II-NUM              VALUE '1'.                        
046700        10 COBOL-II-OBJ-INDICATOR    PIC X.                               
046800           88 COBOL-II-NOOBJ            VALUE '0'.                        
046900           88 COBOL-II-OBJ              VALUE '1'.                        
047000        10 COBOL-II-OFFSET-INDICATOR  PIC X.                              
047100           88 COBOL-II-NOOFFSET         VALUE '0'.                        
047200           88 COBOL-II-OFFSET           VALUE '1'.                        
047300        10 COBOL-II-OPTIMIZ-INDICATOR PIC X.                              
047400           88 COBOL-II-NOOPTIMIZ        VALUE '0'.                        
047500           88 COBOL-II-OPTIMIZ          VALUE '1'.                        
047600        10 COBOL-II-DD-INDICATOR     PIC X.                               
047700           88 COBOL-II-DD               VALUE '0'.                        
047800           88 COBOL-II-OUTDD            VALUE '1'.                        
047900        10 COBOL-II-PFD-INDICATOR    PIC X.                               
048000           88 COBOL-II-NOPFDSGN         VALUE '0'.                        
048100           88 COBOL-II-PFDSGN           VALUE '1'.                        
048200           88 COBOL-390-NUMNPFD          VALUE '0'.                       
048300           88 COBOL-390-NUMPFD           VALUE '1'.                       
048400           88 COBOL-390-NUMMIG           VALUE '2'.                       
048500        10 COBOL-II-RENT-INDICATOR   PIC X.                               
048600           88 COBOL-II-NORENT           VALUE '0'.                        
048700           88 COBOL-II-RENT             VALUE '1'.                        
048800        10 COBOL-II-RES-INDICATOR    PIC X.                               
048900           88 COBOL-II-NORES            VALUE '0'.                        
049000           88 COBOL-II-RES              VALUE '1'.                        
049100        10 COBOL-II-SEQ-INDICATOR    PIC X.                               
049200           88 COBOL-II-NOSEQ            VALUE '0'.                        
049300           88 COBOL-II-SEQ              VALUE '1'.                        
049400        10 COBOL-II-SIZE-INDICATOR   PIC X.                               
049500           88 COBOL-II-SIZE-NBR         VALUE '0'.                        
049600           88 COBOL-II-SIZE-MAX         VALUE '1'.                        
049700        10 COBOL-II-SOURCE-INDICATOR PIC X.                               
049800           88 COBOL-II-NOSOURCE         VALUE '0'.                        
049900           88 COBOL-II-SOURCE           VALUE '1'.                        
050000        10 COBOL-II-SSRANGE-INDICATOR PIC X.                              
050100           88 COBOL-II-NOSSRANGE        VALUE '0'.                        
050200           88 COBOL-II-SSRANGE          VALUE '1'.                        
050300        10 COBOL-II-TERM-INDICATOR   PIC X.                               
050400           88 COBOL-II-NOTERM           VALUE '0'.                        
050500           88 COBOL-II-TERM             VALUE '1'.                        
050600        10 COBOL-II-TEST-INDICATOR   PIC X.                               
050700           88 COBOL-II-NOTEST           VALUE '0'.                        
050800           88 COBOL-II-TEST             VALUE '1'.                        
050900        10 COBOL-II-TRUNC-INDICATOR  PIC X.                               
051000           88 COBOL-II-NOTRUNC          VALUE '0'.                        
051100           88 COBOL-II-TRUNC            VALUE '1'.                        
051200           88 COBOL-390-TRUNCOPT        VALUE '0'.                        
051300           88 COBOL-390-TRUNC-STD       VALUE '1'.                        
051400           88 COBOL-390-TRUNC-BIN       VALUE '1'.                        
051500        10 COBOL-II-WORD-INDICATOR   PIC X.                               
051600           88 COBOL-II-WORD-DEFAULT     VALUE '0'.                        
051700           88 COBOL-II-WORD-SPECIFIED   VALUE '1'.                        
051800        10 COBOL-II-VBREF-INDICATOR  PIC X.                               
051900           88 COBOL-II-NOVBREF          VALUE '0'.                        
052000           88 COBOL-II-VBREF            VALUE '1'.                        
052100        10 COBOL-II-XREF-INDICATOR   PIC X.                               
052200           88 COBOL-II-NOXREF           VALUE '0'.                        
052300           88 COBOL-II-XREF             VALUE '1'.                        
052400        10 COBOL-II-ZWB-INDICATOR    PIC X.                               
052500           88 COBOL-II-NOZWB            VALUE '0'.                        
052600           88 COBOL-II-ZWB              VALUE '1'.                        
052700        10 COBOL-II-CMPR2-INDICATOR  PIC X.                               
052800           88 COBOL-II-NOCMPR2          VALUE '0'.                        
052900           88 COBOL-II-CMPR2            VALUE '1'.                        
053000        10 COBOL-II-NAME-INDICATOR   PIC X.                               
053100           88 COBOL-II-NONAME           VALUE '0'.                        
053200           88 COBOL-II-NAME             VALUE '1'.                        
053300        10 COBOL-II-NMCL-INDICATOR   PIC X.                               
053400           88 COBOL-II-NMCLPRI          VALUE '0'.                        
053500           88 COBOL-II-NMCLALT          VALUE '1'.                        
053600        10 COBOL-II-DBCS-INDICATOR   PIC X.                               
053700           88 COBOL-II-NODBCS           VALUE '0'.                        
053800           88 COBOL-II-DBCS             VALUE '1'.                        
053900        10 COBOL-II-AWO-INDICATOR    PIC X.                               
054000           88 COBOL-II-NOAWO            VALUE '0'.                        
054100           88 COBOL-II-AWO              VALUE '1'.                        
054200     05 COBOL-370-AND-390-FIELDS.                                         
054300        10 COBOL-390-EVENTS-INDICATOR PIC X.                              
054400           88 COBOL-390-NOEVENTS         VALUE '0'.                       
054500           88 COBOL-390-EVENTS           VALUE '1'.                       
054600        10 COBOL-390-CURRENCY-INDICATOR PIC X.                            
054700           88 COBOL-39-NOCURRENCY       VALUE '0'.                        
054800           88 COBOL-39-CURRENCY         VALUE '1'.                        
054900        10 COBOL-390-RMODE-INDICATOR PIC X.                               
055000           88 COBOL-39-RMODE-24         VALUE '0'.                        
055100           88 COBOL-39-RMODE-ANY        VALUE '1'.                        
055200        10 FILLER                    PIC X(4).                            
055300     05 ALL-COBOL-SORTMERGE-INDICATOR PIC X.                              
055400        88 SORT-OR-MERGE-FOUND          VALUE '1'.                        
055500        88 NO-SORT-OR-MERGE-FOUND       VALUE '0'.                        
055600     05 ALL-COBOL-DYNAM-CALL-FND-INDIC PIC X.                             
055700        88 NO-DYNAMIC-CALLS-FOUND       VALUE '0'.                        
055800        88 CALL-LITERAL-FORCED-DYNAMIC VALUE '1'.                         
055900        88 CALL-DATANAME                VALUE '2'.                        
056000        88 CALL-LIT-FRCD-AND-CALL-DATANM VALUE '3'.                       
056100        88 CAPEX-OPT-CALL-ADDR          VALUE '4'.                        
056200        88 CAPEX-CALL-LIT-FORCED-DYNAM VALUE '5'.                         
056300        88 CAPEX-OPT-CALL-DATANAME      VALUE '6'.                        
056400        88 CAPEX-CALL-LIT-FRCD-CALL-DTNM VALUE '7'.                       
056500   05 VERSION-4-AND-VS-COBOL-FIELDS.                                      
056600        10 VSCOBOL-SYMDUMP-INDICATOR PIC X.                               
056700           88 VSCOBOL-NOSYMDUMP         VALUE '0'.                        
056800           88 VSCOBOL-SYMDUMP           VALUE '1'.                        
056900        10 VSCOBOL-FLOW-INDICATOR    PIC X.                               
057000           88 VSCOBOL-NOFLOW            VALUE '0'.                        
057100           88 VSCOBOL-FLOW              VALUE '1'.                        
057200        10 VSCOBOL-STATE-INDICATOR   PIC X.                               
057300           88 VSCOBOL-NOSTATE           VALUE '0'.                        
057400           88 VSCOBOL-STATE             VALUE '1'.                        
057500        10 VSCOBOL-TEST-INDICATOR    PIC X.                               
057600           88 VSCOBOL-NOTEST            VALUE '0'.                        
057700           88 VSCOBOL-TEST              VALUE '1'.                        
057800        10 VSCOBOL-RES-INDICATOR     PIC X.                               
057900           88 VSCOBOL-NORES             VALUE '0'.                        
058000           88 VSCOBOL-RES               VALUE '1'.                        
058100        10 VSCOBOL-ENDJOB-INDICATOR  PIC X.                               
058200           88 VSCOBOL-NOENDJOB          VALUE '0'.                        
058300           88 VSCOBOL-ENDJOB            VALUE '1'.                        
058400        10 VSCOBOL-OBJ370-INDICATOR  PIC X.                               
058500           88 VSCOBOL-OBJ360            VALUE '0'.                        
058600           88 VSCOBOL-OBJ370            VALUE '1'.                        
058700        10 VSCOBOL-OPTIMIZED-INDICATOR PIC X.                             
058800           88 VSCOBOL-NOOPT             VALUE '0'.                        
058900           88 VSCOBOL-OPT               VALUE '1'.                        
059000        10 VSCOBOL-CAPEX-OPT-INDICATOR PIC X.                             
059100           88 VSCOBOL-NOCAPEX-OPT       VALUE '0'.                        
059200           88 VSCOBOL-CAPEX-OPT         VALUE '1'.                        
059300        10 VSCOBOL-CAPEX-DTECT-INDICATOR PIC X.                           
059400           88 VSCOBOL-NOCAPEX-DTECT     VALUE '0'.                        
059500           88 VSCOBOL-CAPEX-DTECT       VALUE '1'.                        
059600        10 VSCOBOL-COUNT-INDICATOR   PIC X.                               
059700           88 VSCOBOL-NOCOUNT           VALUE '0'.                        
059800           88 VSCOBOL-COUNT             VALUE '1'.                        
059900        10 VSCOBOL-TRACE-INDICATOR   PIC X.                               
060000           88 VSCOBOL-NOTRACE           VALUE '0'.                        
060100           88 VSCOBOL-TRACE             VALUE '1'.                        
060200        10 VSCOBOL-LANGLEVEL-INDICATOR PIC X.                             
060300           88 VSCOBOL-LANGLEVEL-1       VALUE '1'.                        
060400           88 VSCOBOL-LANGLEVEL-2       VALUE '2'.                        
060500        10 VSCOBOL-LAST-SYSOU-DDNM-CHAR PIC X.                            
060600        10 FILLER                    PIC X(3).                            
060700     05 PLI-V2-FIELDS.                                                    
060800        10 PLI-V2-MAIN-INDICATOR     PIC X.                               
060900           88 PLI-V2-PROC-OPTIONS-MAIN  VALUE '1'.                        
061000           88 PLI-V2-NO-PROC-OPTIONS-MAIN VALUE '0'.                      
061100        10 PLI-V2-REORDER-INDICATOR  PIC X.                               
061200           88 PLI-V2-NOREORDER          VALUE '0'.                        
061300           88 PLI-V2-REORDER            VALUE '1'.                        
061400        10 PLI-V2-EXECOPS-INDICATOR  PIC X.                               
061500           88 PLI-V2-NOEXECOPS          VALUE '0'.                        
061600           88 PLI-V2-EXECOPS            VALUE '1'.                        
061700        10 PLI-V2-CMPAT-INDICATOR    PIC X.                               
061800           88 PLI-V2-CMPAT-V1           VALUE '1'.                        
061900           88 PLI-V2-CMPAT-V2           VALUE '2'.                        
062000        10 PLI-V2-GRAPHIC-INDICATOR  PIC X.                               
062100           88 PLI-V2-NOGRAPHIC          VALUE '0'.                        
062200           88 PLI-V2-GRAPHIC            VALUE '1'.                        
062300        10 PLI-V2-OPTIMIZE-INDICATOR PIC X.                               
062400           88 PLI-V2-NOOPTIMIZE         VALUE '0'.                        
062500           88 PLI-V2-OPTIMIZE           VALUE '1'.                        
062600        10 PLI-V2-INTERUPT-INDICATOR PIC X.                               
062700           88 PLI-V2-NOINTERUPT         VALUE '0'.                        
062800           88 PLI-V2-INTERUPT           VALUE '1'.                        
062900        10 PLI-V2-FLOW-INDICATOR     PIC X.                               
063000           88 PLI-V2-NOFLOW             VALUE '0'.                        
063100           88 PLI-V2-FLOW               VALUE '1'.                        
063200        10 PLI-V2-COUNT-INDICATOR    PIC X.                               
063300           88 PLI-V2-NOCOUNT            VALUE '0'.                        
063400           88 PLI-V2-COUNT              VALUE '1'.                        
063500        10 PLI-V2-TEST-INDICATOR     PIC X.                               
063600           88 PLI-V2-NOTEST             VALUE '0'.                        
063700           88 PLI-V2-TEST               VALUE '1'.                        
063800        10 PLI-V2-SYSTEM-INDICATOR   PIC X.                               
063900           88 PLI-V2-SYSTEM-CMS         VALUE '1'.                        
064000           88 PLI-V2-SYSTEM-CMSTP       VALUE '2'.                        
064100           88 PLI-V2-SYSTEM-MVS         VALUE '3'.                        
064200           88 PLI-V2-SYSTEM-TSO         VALUE '4'.                        
064300           88 PLI-V2-SYSTEM-CICS        VALUE '5'.                        
064400           88 PLI-V2-SYSTEM-IMS         VALUE '6'.                        
064500        10 PLI-V2-CSECT-INDICATOR    PIC X.                               
064600           88 PLI-V2-RUNTIME-CSECT      VALUE '1'.                        
064700           88 PLI-V2-EXTERNAL-DATA-CSECT VALUE '2'.                       
064800           88 PLI-V2-STATIC-DATA-CSECT  VALUE '3'.                        
064900           88 PLI-V2-CODE-CSECT         VALUE '4'.                        
065000     05 FILLER                       PIC X(28).                           
065100     05 WINDOW-INDICATOR             PIC X(4).                            
065200                                                                          
065300 FD  REPORT-FILE                                                          
065400     BLOCK CONTAINS 0 RECORDS                                             
065500     RECORD CONTAINS 133 CHARACTERS                                       
065600     RECORDING MODE F.                                                    
065700 01  REPORT-RECORD.                                                       
065800     05  FILLER                      PIC X.                               
065900     05  RPT-MEMBER-NAME             PIC X(8).                            
066000     05  FILLER                      PIC X.                               
066100     05  RPT-CSECT-NAME              PIC X(8).                            
066200     05  FILLER                      PIC X.                               
066300     05  RPT-MESSAGE                 PIC X(114).                          
066400                                                                          
066500 WORKING-STORAGE SECTION.                                                 
066600 01  SWITCHES-AND-INDICATORS.                                             
066610     05  OLD-FILE-STATUS            PIC X VALUE 'M'.                      
066800             88  NO-MORE-OLD-DATA          VALUE 'N'.                     
066810             88  MORE-OLD-DATA             VALUE 'M'.                     
067000     05  NEW-FILE-STATUS            PIC X VALUE 'M'.                      
067200             88  NO-MORE-NEW-DATA          VALUE 'N'.                     
067210             88  MORE-NEW-DATA             VALUE 'M'.                     
067610     05  INPUT-DATA-SWITCH           PIC X VALUE 'N'.                     
067620         88  END-OF-INPUT-DATA             VALUE 'Y'.                     
067630         88  MORE-INPUT-DATA               VALUE 'N'.                     
067640     05  GOT-OLD-DATA-FLAG           PIC X VALUE 'N'.                     
067650         88  GOT-OLD-RECORD                VALUE 'Y'.                     
067660         88  BYPASS-OLD-DATA-RECORD        VALUE 'N'.                     
067670     05  GOT-NEW-DATA-FLAG           PIC X VALUE 'N'.                     
067680         88  GOT-NEW-RECORD                VALUE 'Y'.                     
067690         88  BYPASS-NEW-DATA-RECORD        VALUE 'N'.                     
067691     05  READ-SWITCH                 PIC X VALUE 'B'.                     
067692          88  READ-BOTH-FILES              VALUE 'O'.                     
067693          88  READ-OLD-FILE                VALUE 'N'.                     
067694          88  READ-NEW-FILE                VALUE 'B'.                     
067700                                                                          
067800 PROCEDURE DIVISION.                                                      
067900 0000-EXECUTIVE-CONTROL.                                                  
068000     PERFORM 9900-INITIALIZATION.                                         
068100     PERFORM 1000-MAINLINE-PROCESSING UNTIL END-OF-INPUT-DATA.            
068200     PERFORM 9990-END-OF-JOB.                                             
068300     GOBACK.                                                              
068400                                                                          
068500 1000-MAINLINE-PROCESSING.                                                
068600      IF READ-OLD-FILE OR READ-BOTH-FILES                                 
068900          PERFORM 9000-READ-OLD-FILE                                      
068901              UNTIL GOT-OLD-RECORD OR NO-MORE-OLD-DATA                    
068910      ELSE                                                                
069000          IF READ-NEW-FILE OR READ-BOTH-FILES                             
069300          PERFORM 9001-READ-NEW-FILE                                      
069301              UNTIL GOT-NEW-RECORD OR NO-MORE-NEW-DATA                    
069310      MOVE ' ' TO READ-SWITCH.                                            
069400      IF MORE-INPUT-DATA                                                  
069500          PERFORM 2000-COMPARE-NEW-AND-OLD THRU 2000-EXIT.                
069900                                                                          
069901 2000-COMPARE-NEW-AND-OLD.                                                
069902      PERFORM 3000-CHECK-OLD-RECORD.                                      
069903      PERFORM 3001-CHECK-NEW-RECORD.                                      
069904      IF USABLE-OLD-RECORD AND USABLE-NEW-RECORD                          
069905          IF MEMBER-NAME IN OLD-RECORD <                                  
069906             MEMBER-NAME IN NEW-RECORD                                    
069907 2000-EXIT. EXIT.                                                         
069910                                                                          
070000 9000-READ-OLD-FILE.                                                      
070002      IF MORE-OLD-DATA                                                    
070010          READ OLD-FILE                                                   
070020              AT END                                                      
070100                  IF OLD-RECORDS-READ EQUAL ZERO                          
070200                  DISPLAY 'NULL OLD INPUT FILE ENCOUNTERED'               
070300                  CALL 'ILBOABN0'                                         
070310              ELSE                                                        
070320                  IF END-NEW-DATA-FILE                                    
070330                      MOVE 'Y' TO INPUT-DATA-SWITCH.                      
070340      IF MORE-OLD-DATA                                                    
070400          ADD 1 TO OLD-RECORDS-READ.                                      
072100                                                                          
072110 9001-READ-NEW-FILE.                                                      
072120      READ NEW-FILE                                                       
072130          AT END                                                          
072140              IF NEW-RECORDS-READ EQUAL ZERO                              
072150                  DISPLAY 'NULL NEW INPUT FILE ENCOUNTERED'               
072160                  CALL 'ILBOABN0'                                         
072170              ELSE                                                        
072180                  IF END-OLD-DATA-FILE                                    
072190                      MOVE 'Y' TO INPUT-DATA-SWITCH.                      
072191      IF MORE-NEW-DATA                                                    
072192          ADD 1 TO NEW-RECORDS-READ.                                      
072193                                                                          
072194                                                                          
072200 9900-INITIALIZATION.                                                     
072300     OPEN INPUT OLD-FILE, NEW-FILE,                                       
073400          OUTPUT REPORT-FILE.                                             
074000                                                                          
074100 9990-END-OF-JOB.                                                         
074300     CLOSE OLD-FILE.                                                      
074400     CLOSE NEW-FILE.                                                      
074500     CLOSE REPORT-FILE.                                                   

