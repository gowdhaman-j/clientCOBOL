000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID.    C1UEXT02.                                                 
000300 AUTHOR. R THORNTON                                                       
000400*****************************************************************         
000500*REMARKS. C1UEXT02 IS AN ENDEVOR EXIT PROGRAM THAT IS INVOKED BY*         
000600*         THE ENDEVOR INTERFACE PROGRAM, EPC1UEXT, WHICH CALLS  *         
000700*         THIS EXIT, PASSING SEVERAL ENDEVOR CONTROL BLOCKS     *         
000800*         BEFORE ENDEVOR EXECUTES ANY OF THE FOLLOWING ACTIONS: *         
000900*                ADD, UPDATE, RETRIEVE, GENERATE,               *         
001000*                MOVE, DELETE, SIGNIN, TRANSFER,                *         
001100*                ARCHIVE, AND RESTORE.                          *         
001200*     PURPOSE: THIS EXIT WILL TEST FOR OVERRIDE TO A PREVIOUS   *         
001300*         SIGNOUT. WHEN THIS OCCURS, THE ORIGINAL OWNER WILL BE *         
001400*         SENT AN EMAIL NOTIFYING THEM THAT THE SIGHOUT OVERRIDE*         
001500*         OCCURRED, AND WILL GIVE THE LOGON ID OF THE OVERRIDER.*         
001600*     NOTES: C1UEXT02 USES THE COBOL COPY BOOK SUPPLIED BY      *         
001700*         ENDEVOR AS SPP.ENDEVOR.V3R9M1.SOURCE(EXITBLKS) TO     *         
001800*         DEFINE THE CONTROL BLOCKS PASSED IN THE LINKAGE       *         
001900*         SECTION OF THIS PROGRAM.                              *         
002000*         (2) THE LOAD MODULE FOR THIS PROGRAM MUST BE LINKED   *         
002100*         WITH EPC1UEXT AS THE ENTRY POINT, AND THE LOAD        *         
002200*         MODULE MUST BE IN SPP.ENDEVOR.V3R9M1.CONLIB(C1UEXT02) *         
002300*         (3) THE C1UEXT02 EXIT MUST BE IDENTIFIED TO ENDEVOR   *         
002400*         BY INCLUDING IT IN THE C1UEXITS TABLE.                *         
002500*****************************************************************         
002600 ENVIRONMENT DIVISION.                                                    
002700 CONFIGURATION SECTION.                                                   
002800 INPUT-OUTPUT SECTION.                                                    
002900 FILE-CONTROL.                                                            
003000 DATA DIVISION.                                                           
003100 FILE SECTION.                                                            
003200 WORKING-STORAGE SECTION.                                                 
003300 77  FILLER PIC X(36)  VALUE                                              
003400     'C1UEXT02 WORKING STORAGE BEGINS HERE'.                              
003410 01  EMAIL-MESSAGE.                                                       
003420     05  CURRENT-OWNER-USERID       PIC X(8).                             
003430     05  FILLER                     PIC X(29) VALUE                       
003440                ': THIS IS TO INFORM YOU THAT '.                          
003450     05  SIGNOUT-USERID             PIC X(8).                             
003460     05  FILLER                     PIC X(41) VALUE                       
003470                ' HAS OVERRIDDEN YOUR SIGNOUT FOR ELEMENT '.              
003480     05  ELEMENT-NAME               PIC X(8).                             
003490     05  FILLER                     PIC X(16) VALUE                       
003491                ' IN ENVIRONMENT '.                                       
003492     05  ENVIRONMENT-NAME           PIC X(8).                             
003493     05  FILLER                     PIC X(9) VALUE                        
003494                ', SYSTEM '.                                              
003495     05  SYSTEM-NAME               PIC X(8).                              
003496     05  FILLER                    PIC X(12) VALUE                        
003497                ', SUBSYSTEM '.                                           
003498     05  SUBSYSTEM-NAME            PIC X(8).                              
003499     05  FILLER                    PIC X VALUE '.'.                       
003500*                                                                         
003510 LINKAGE SECTION.                                                         
003600*    COPY EXITBLKS.                                                       
003700 01  EXIT-CONTROL-BLOCK.                                          00010000
003800     05  ECB-LENGTH              PIC 9(4) COMP SYNC.              00020000
003900     05  ECB-CURRENT-VERSION     PIC 9(4) COMP SYNC.              00030000
004000     05  ECB-BLOCK-ID            PIC X(4).                        00040007
004100     05  ECB-FLAG                PIC 9(8) COMP SYNC.              00050005
004200     05  ECB-RETURN-CODE         PIC 9(8) COMP SYNC.              00060005
004300         88  RETURN-OK           VALUE 0.                         00070005
004400         88  RETURN-OK-MODS      VALUE 4.                         00080005
004500         88  RETURN-ABORT        VALUE 8.                         00090005
004600     05  ECB-ARGUMENTS           PIC 9(8) COMP SYNC.              00100005
004700     05  ECB-EXIT-NUMBER         PIC 9(8) COMP SYNC.              00110005
004800         88  C1IEXT01            VALUE 1.                         00120005
004900         88  C1IEXT02            VALUE 2.                         00130005
005000         88  C1IEXT03            VALUE 3.                         00140005
005100         88  C1IEXT04            VALUE 4.                         00150005
005200         88  C1IEXT05            VALUE 5.                         00160005
005300         88  C1IEXT06            VALUE 6.                         00170005
005400     05  ECB-USER-ID             PIC X(8).                        00180005
005500     05  ECB-TSO-BATCH-MODE      PIC X.                           00190005
005600         88  TSO                 VALUE 'T'.                       00200005
005700         88  BATCH               VALUE 'B'.                       00210005
005800     05  ECB-CALL-FLAG           PIC X.                           00220005
005900         88  INTERNAL-CALL       VALUE 'Y'.                       00230005
006000         88  ACTION-CALL         VALUE 'N'.                       00240005
006100     05  ECB-SMF-FLAG            PIC X.                           00250005
006200         88  SMF-ENABLED         VALUE 'Y'.                       00260005
006300         88  SMF-DISABLED        VALUE 'N'.                       00270005
006400     05  ECB-SMF-WRITE-FLAG      PIC X.                           00280005
006500         88  SMF-WRITE-BUFFER    VALUE 'Y'.                       00290005
006600         88  SMF-NO-WRITE-BUF    VALUE 'N'.                       00300005
006700     05  ECB-ACTION-CODE         PIC 9(8) COMP SYNC.              00310005
006800         88  ADD-ACTION          VALUE 1.                         00320005
006900         88  UPDATE-ACTION       VALUE 2.                         00330005
007000         88  RETRIEVE-ACTION     VALUE 3.                         00340005
007100         88  DELETE-ACTION       VALUE 4.                         00350005
007200         88  GENERATE-ACTION     VALUE 5.                         00360005
007300         88  DISPLAY-ACTION      VALUE 6.                         00370005
007400         88  MOVE-ACTION         VALUE 7.                         00380005
007500         88  ARCHIVE-ACTION      VALUE 8.                         00390005
007600         88  SIGNIN-ACTION       VALUE 12.                        00400000
007700         88  SO-OVERRIDE-ACTION  VALUE 13.                        00401000
007800         88  ENV-MGR-ACTION      VALUE 14.                        00410005
007900         88  PRINT-ACTION        VALUE 15.                        00420009
008000         88  TRANSFER-ACTION     VALUE 16.                        00430009
008100         88  RESTORE-ACTION      VALUE 18.                        00440005
008200     05  ECB-ACTION-NAME         PIC X(8).                        00450005
008300     05  ECB-MESSAGE-CODE        PIC X(4).                        00460005
008400     05  ECB-MESSAGE-LENGTH      PIC 9(4) COMP SYNC.              00470005
008500     05  ECB-MESSAGE-TEXT        PIC X(132).                      00480005
008600     05  ECB-CALLER-ORIGIN       PIC X(1).                                
008700         88  CALLER-ENDEVOR      VALUE 'E'.                               
008800         88  CALLER-QUICKEDIT    VALUE 'Q'.                               
008900     05  FILLER                  PIC X(1).                                
009000     05  ECB-REQUEST-POINTER     PIC 9(8) COMP SYNC.              00490005
009100     05  ECB-PANEL-TYPE          PIC 9(8) COMP SYNC.              00500005
009200         88  NO-PANEL-INFO       VALUE 0.                         00510005
009300         88  ENVIRONMENT-PANEL   VALUE 1.                         00520005
009400         88  MAIN-PANEL          VALUE 2.                         00530005
009500         88  FOREGROUND-PANEL    VALUE 3.                         00540005
009600     05  ECB-EXIT-HOLD-FIELD     PIC 9(8) COMP SYNC.              00550005
009700     05  ECB-SECURITY-FUNCTION   PIC 9(8) COMP SYNC.              00560005
009800     05  ECB-HIGH-RC             PIC 9(8) COMP SYNC.              00560005
009900*                                                                 00570000
010000*                                                                 00580000
010100*                                                                 00590000
010200 01  REQUEST-INFO-BLOCK.                                          00600000
010300     05  REQ-LENGTH              PIC 9(4) COMP SYNC.              00610000
010400     05  REQ-CURRENT-VERSION     PIC 9(4) COMP SYNC.              00620000
010500     05  REQ-BLOCK-ID            PIC X(4).                        00630007
010600     05  REQ-CCID                PIC X(12).                       00640005
010700     05  REQ-COMMENT             PIC X(40).                       00650005
010800     05  REQ-REQUEST-FLAG        PIC 9(8) COMP SYNC.              00660005
010900     05  REQ-SISO-INDICATOR      PIC X.                           00670005
011000         88  SIGNOUT-OVERRIDE    VALUE 'Y'.                       00680000
011100         88  NO-SIGNOUT-OVERRIDE VALUE 'N'.                       00690000
011200     05  REQ-COPY-INDICATOR      PIC X.                           00700005
011300         88  RETRIEVE-COPY-ONLY  VALUE 'Y'.                       00710000
011400         88  RETRIEVE-FOR-UPDATE VALUE 'N'.                       00720000
011500     05  REQ-EXPINCL-INDICATOR   PIC X.                           00730005
011600         88  EXPAND-INCLUDES     VALUE 'Y'.                       00740000
011700         88  NO-EXPAND-INCLUDES  VALUE 'N'.                       00750000
011800     05  REQ-OVERWRITE-INDICATOR PIC X.                           00760000
011900         88  WRITE-OVER-MEMBER    VALUE 'Y'.                      00770000
012000         88  NO-WRITE-OVER-MEMBER VALUE 'N'.                      00780000
012100     05  REQ-ACTION-RC           PIC 9(8) COMP SYNC.              00790005
012200     05  REQ-NEW-VERSION         PIC X(2).                        00800000
012300     05  REQ-GEN-COPYBACK        PIC X.                           00810000
012400         88  GEN-COPYBACK           VALUE 'Y'.                    00811000
012500         88  NO-GEN-COPYBACK        VALUE 'N'.                    00812000
012600     05  FILLER                  PIC X(5).                        00813000
012700     05  REQ-DELETE-AFTER        PIC X.                           00820005
012800         88  DELETE-AFTER-ACTION    VALUE 'Y'.                    00830000
012900         88  NO-DELETE-AFTER-ACTION VALUE 'N'.                    00840000
013000     05  REQ-MOVE-WITH-HISTORY   PIC X.                           00850005
013100         88  MOVE-WITH-HISTORY    VALUE 'Y'.                      00860000
013200         88  MOVE-WITHOUT-HISTORY VALUE 'N'.                      00870000
013300     05  REQ-ADD-WITH-UPDATE     PIC X.                           00880005
013400         88  ADD-WITH-UPDATE     VALUE 'Y'.                       00890000
013500         88  ADD-WITHOUT-UPDATE  VALUE 'N'.                       00900000
013600     05  REQ-BYPASS-GEN-PROC     PIC X.                           00910000
013700         88  BYPASS-GEN-PROC     VALUE 'Y'.                       00911000
013800         88  NO-BYPASS-GEN-PROC  VALUE 'N'.                       00912000
013900     05  REQ-DEL-COMPS-ONLY      PIC X.                           00913000
014000         88  DEL-COMPS-ONLY      VALUE 'Y'.                       00914000
014100         88  NO-DEL-COMPS-ONLY   VALUE 'N'.                       00915000
014200     05  REQ-SYNCHRONIZE         PIC X.                           00916000
014300         88  YES-SYNCHRONIZE     VALUE 'Y'.                       00917000
014400         88  NO-SYNCHRONIZE      VALUE 'N'.                       00918000
014500     05  REQ-IGNGEN-FAIL         PIC X.                           00919000
014600         88  IGNORE-GEN-FAIL     VALUE 'Y'.                       00919100
014700         88  NO-INGORE-GEN-FAIL  VALUE 'N'.                       00919200
014800     05  REQ-BYPASS-DEL-PROC     PIC X.                           00919300
014900         88  BYPASS-DEL-PROC     VALUE 'Y'.                       00919400
015000         88  NO-BYPASS-DEL-PROC  VALUE 'N'.                       00919500
015100     05  REQ-PROCESSOR-GROUP     PIC X(8).                        00919600
015200     05  REQ-SIGNOUT-USRID       PIC X(8).                        00919700
015300     05  REQ-SEARCH-OPTION       PIC X.                           00919300
015400         88  SEARCH-ENV-MAP      VALUE 'Y'.                       00919400
015500         88  NO-SEARCH-ENV-MAP   VALUE 'N'.                       00919500
015600     05  REQ-RETAIN-SIGNOUT-OPT  PIC X.                           00919300
015700         88  RETAIN-SIGNOUT      VALUE 'Y'.                       00919400
015800         88  NO-RETAIN-SIGNOUT   VALUE 'N'.                       00919500
015900     05  REQ-JUMP-OPTION         PIC X.                           00919300
016000         88  JUMP-IS-OK          VALUE 'Y'.                       00919400
016100         88  NO-JUMP-SPECIFIED   VALUE 'N'.                       00919500
016200     05  REQ-SIGNIN-OPTION       PIC X.                           00919300
016300         88  SIGNIN-IS-SPECIFIED VALUE 'Y'.                       00919400
016400         88  NO-SIGNIN-SPECIFIED VALUE 'N'.                       00919500
016500*                                                                 00920000
016600*                                                                 00930000
016700*                                                                 00940000
016800 01  SRC-ENVIRONMENT-BLOCK.                                       00950010
016900     05  SRC-ENV-LENGTH              PIC 9(4) COMP SYNC.          00960000
017000     05  SRC-ENV-CURRENT-VERSION     PIC 9(4) COMP SYNC.          00970000
017100     05  SRC-ENV-BLOCK-ID            PIC X(4).                    00980010
017200     05  SRC-ENV-FLAGS               PIC 9(4) COMP SYNC.          00990010
017300     05  SRC-ENV-TYPE-OF-BLOCK       PIC X.                       01000010
017400         88  SRC-INTERNAL-C1-BLOCK   VALUE 'C'.                   01010010
017500         88  SRC-EXTERNAL-ENV-BLOCK  VALUE 'E'.                   01020010
017600         88  SRC-ARCHIVE-FILE        VALUE 'A'.                   01021000
017700     05  SRC-ENV-IO-TYPE             PIC X.                       01030010
017800         88  SRC-SOURCE-LOCATION     VALUE 'I'.                   01040010
017900         88  SRC-TARGET-LOCATION     VALUE 'O'.                   01050010
018000     05  SRC-ENV-NEXT-ENV-POINTER    PIC 9(8) COMP SYNC.          01060010
018100     05  SRC-ENV-FILE-POINTER        PIC 9(8) COMP SYNC.          01070010
018200     05  SRC-ENV-SITE-NAME           PIC X.                       01080010
018300     05  SRC-ENV-STAGE-ID            PIC X.                       01090010
018400         88  SRC-STAGE-ONE           VALUE '1'.                   01100010
018500         88  SRC-STAGE-TWO           VALUE '2'.                   01110010
018600     05  SRC-ENV-STAGE-CODE          PIC X.                       01120010
018700     05  FILLER                  PIC X.                           01130005
018800     05  SRC-ENV-ELEMENT-VERSION     PIC 9(4) COMP SYNC.          01140010
018900     05  SRC-ENV-ELEMENT-LEVEL       PIC 9(4) COMP SYNC.          01150010
019000     05  SRC-ENV-ENVIRONMENT-NAME    PIC X(8).                    01160010
019100     05  SRC-ENV-STAGE-NAME          PIC X(8).                    01170010
019200     05  SRC-ENV-SYSTEM-NAME         PIC X(8).                    01180010
019300     05  SRC-ENV-SYSTEM-DATA         PIC X(8).                    01190010
019400     05  FILLER                      PIC X(8).                    01200010
019500     05  SRC-ENV-SUBSYSTEM-NAME      PIC X(8).                    01250010
019600     05  SRC-ENV-SUBSYSTEM-DATA      PIC X(8).                    01260010
019700     05  SRC-ENV-TYPE-NAME           PIC X(8).                    01270010
019800     05  SRC-ENV-TYPE-DATA           PIC X(8).                    01280010
019900     05  SRC-ENV-ELEMENT-NAME        PIC X(10).                   01290010
020000     05  FILLER                  PIC X(8).                        01300005
020100*                                                                 01310002
020200*                                                                 01320002
020300*                                                                 01330002
020400 01  SRC-ELEMENT-MASTER-INFO-BLOCK.                               01340010
020500     05  SRC-ELM-LENGTH              PIC 9(4) COMP SYNC.          01350000
020600     05  SRC-ELM-CURRENT-VERSION     PIC 9(4) COMP SYNC.          01360000
020700     05  SRC-ELM-BLOCK-ID            PIC X(4).                    01370010
020800     05  SRC-ELM-FLAGS               PIC 9(8) COMP SYNC.          01380010
020900     05  SRC-ELM-ELEMENT-VERSION     PIC 9(4) COMP SYNC.          01390010
021000     05  SRC-ELM-ELEMENT-LEVEL       PIC 9(4) COMP SYNC.          01400010
021100     05  SRC-ELM-NUMBER-INSERTS      PIC 9(8) COMP SYNC.          01410010
021200     05  SRC-ELM-NUMBER-DELETES      PIC 9(8) COMP SYNC.          01420010
021300     05  SRC-ELM-ELEMENT-NAME        PIC X(10).                   01430010
021400     05  SRC-ELM-CURRENT-CCID        PIC X(12).                   01440010
021500     05  SRC-ELM-BASE-LEVEL-NUMBER   PIC 9(4) COMP SYNC.          01450010
021600     05  SRC-ELM-BASE-TOTAL-STMTS    PIC 9(8) COMP SYNC.          01460010
021700     05  SRC-ELM-BASE-DATE           PIC 9(6).                    01470010
021800     05  SRC-ELM-BASE-TIME           PIC 9(4).                    01480010
021900     05  SRC-ELM-BASE-COMMENT        PIC X(40).                   01490010
022000     05  SRC-ELM-BASE-USERID         PIC X(8).                    01500010
022100     05  SRC-ELM-GEN-USERID          PIC X(8).                    01510010
022200     05  SRC-ELM-GEN-DATE            PIC 9(6).                    01520010
022300     05  SRC-ELM-GEN-TIME            PIC 9(4).                    01530010
022400     05  SRC-ELM-FROM-DSN            PIC X(44).                   01540010
022500     05  SRC-ELM-FROM-MEMBER         PIC X(10).                   01550010
022600     05  SRC-ELM-PROCESSOR-FLAG      PIC X.                       01560010
022700     05  FILLER                  PIC X(3).                        01570005
022800     05  SRC-ELM-PROCESSOR-LAST-DATE PIC 9(6).                    01580010
022900     05  SRC-ELM-PROCESSOR-LAST-TIME PIC 9(4).                    01590010
023000     05  SRC-ELM-PROCESSOR-USERID    PIC X(8).                    01600010
023100     05  SRC-ELM-PROCESSOR-NAME      PIC X(10).                   01610010
023200     05  SRC-ELM-PROCESSOR-RC        PIC 9(4) COMP SYNC.          01620010
023300     05  SRC-ELM-PROCESSOR-C1-RC     PIC 9(4) COMP SYNC.          01630010
023400     05  SRC-ELM-PROCESSOR-LAST-COMMENT PIC X(40).                01640010
023500     05  SRC-ELM-LEVEL-COMMENT       PIC X(40).                   01650010
023600     05  SRC-ELM-LEVEL-DATE          PIC 9(6).                    01660010
023700     05  SRC-ELM-LEVEL-TIME          PIC 9(4).                    01670010
023800     05  SRC-ELM-LEVEL-USERID        PIC X(8).                    01680010
023900     05  SRC-ELM-LAST-ACTION         PIC X(8).                    01690010
024000     05  SRC-ELM-LAST-LEVEL-TOTAL    PIC 9(8) COMP SYNC.          01700010
024100     05  SRC-ELM-MOVE-DATE           PIC 9(6).                    01710010
024200     05  SRC-ELM-MOVE-TIME           PIC 9(4).                    01720010
024300     05  SRC-ELM-MOVE-USERID         PIC X(8).                    01730010
024400     05  SRC-ELM-RETRIEVE-DATE       PIC 9(6).                    01740010
024500     05  SRC-ELM-RETRIEVE-TIME       PIC 9(4).                    01750010
024600     05  SRC-ELM-RETRIEVE-USERID     PIC X(8).                    01760010
024700     05  SRC-ELM-RETRIEVE-COMMENT    PIC X(40).                   01770010
024800     05  SRC-ELM-RETRIEVE-TO-DSN     PIC X(44).                   01780010
024900     05  SRC-ELM-RETRIEVE-TO-MEMBER  PIC X(10).                   01790010
025000     05  FILLER                  PIC X(30).                       01800005
025100     05  SRC-ELM-RET-CCID            PIC X(12).                   01801000
025200     05  SRC-ELM-GEN-CCID            PIC X(12).                   01802000
025300     05  SRC-ELM-MOD-ACTION          PIC X(8).                    01803000
025400     05  SRC-ELM-ACTION-CCID         PIC X(12).                   01804000
025500     05  SRC-ELM-ACTION-COMM         PIC X(40).                   01805000
025600     05  SRC-ELM-ACTION-USER         PIC X(8).                    01806000
025700     05  SRC-ELM-ACTION-DATE         PIC 9(6).                    01807000
025800     05  SRC-ELM-ACTIOM-TIME         PIC 9(4).                    01808000
025900     05  SRC-ELM-SIGNOUT-ID          PIC X(8).                    01809000
026000     05  SRC-ELM-LOC-FLAG            PIC X.                       01809100
026100     05  SRC-ELM-FR-ACTION           PIC X(8).                    01809200
026200     05  SRC-ELM-FR-ACT-USERID       PIC X(8).                    01809300
026300     05  SRC-ELM-FR-ACT-DATE         PIC 9(6).                    01809400
026400     05  SRC-ELM-FR-ACT-TIME         PIC 9(4).                    01809500
026500     05  SRC-ELM-FR-SITE             PIC X.                       01809600
026600     05  SRC-ELM-FR-ENV              PIC X(8).                    01809700
026700     05  SRC-ELM-FR-SYS              PIC X(8).                    01809800
026800     05  SRC-ELM-FR-SUBSYS           PIC X(8).                    01809900
026900     05  SRC-ELM-FR-ELEMENT          PIC X(10).                   01810000
027000     05  SRC-ELM-FR-TYPE             PIC X(8).                    01810100
027100     05  SRC-ELM-FR-STG-NBR          PIC X.                       01810200
027200     05  SRC-ELM-FR-VER              PIC 9(4) COMP SYNC.          01810300
027300     05  SRC-ELM-FR-LEVEL            PIC 9(4) COMP SYNC.          01810400
027400                                                                  01810500
027500* NEW ELEMENT FIELDS FOR E/MVS 3.6 FOLLOW                         01810006
027600                                                                  01810500
027700     05  SRC-ELM-UPDATE-MEMBER-NAME  PIC X(10).                   01810400
027800     05  SRC-ELM-BASE-MEMBER-NAME    PIC X(10).                   01810400
027900     05  SRC-ELM-LOWER-LEVEL-IND     PIC X(2).                    01810400
028000     05  SRC-ELM-PCT-PREV-INSERT-DEL PIC X.                       01810400
028100     05  SRC-ELM-PCT-PREV-DELS-REINS PIC X.                       01810400
028200     05  SRC-ELM-ESD-FP-NAME-IF-OBJ  PIC X(8).                    01810400
028300     05  SRC-ELM-DELTA-FORMAT        PIC X.                       01810400
028400         88  SRC-ELM-FORWARD-DELTA   VALUE 'F'.                   01100010
028500         88  SRC-ELM-REVVERSE-DELTA  VALUE 'R'.                   01110010
028600     05  SRC-ELM-PACKED-INDICATOR    PIC X.                       01810400
028700         88  SRC-ELM-BASE-NOT-PACKED VALUE 'Y'.                   01100010
028800     05  SRC-ELM-LAST-PROC-VERSION   PIC X.                       01810400
028900     05  SRC-ELM-LAST-PROC-LEVEL     PIC X.                       01810400
029000     05  SRC-ELM-RECORD-FORMAT       PIC X.                       01810400
029100     05  SRC-ELM-CONF-DELTA-MEM-ID   PIC X(8).                    01810400
029200     05  SRC-ELM-CONF-DELTA-VERSION  PIC S9(4) COMP SYNC.         01810400
029300     05  SRC-ELM-CONF-BASE-TOTAL     PIC S9(8) COMP SYNC.         01810400
029400     05  SRC-ELM-CONF-LAST-LVL-TOTAL PIC S9(8) COMP SYNC.         01810400
029500     05  SRC-ELM-CONF-BASE-LVL-NBR   PIC S9(4) COMP SYNC.         01810400
029600     05  SRC-ELM-CONF-LAST-LVL-NBR   PIC S9(4) COMP SYNC.         01810400
029700     05  SRC-ELM-INSERTS-LAST-LEVEL  PIC S9(4) COMP SYNC.         01810400
029800     05  SRC-ELM-DELETES-LAST-LEVEL  PIC S9(4) COMP SYNC.         01810400
029900     05  SRC-ELM-CONF-REGRES-INS-PCT PIC S9(4) COMP SYNC.         01810400
030000     05  SRC-ELM-CONF-REGRES-DEL-PCT PIC S9(4) COMP SYNC.         01810400
030100     05  SRC-ELM-CONF-BASE-DATE      PIC 9(6).                    01810400
030200     05  SRC-ELM-CONF-BASE-TIME      PIC 9(4).                    01810400
030300     05  SRC-ELM-CONF-LAST-LVL-DATE  PIC 9(6).                    01810400
030400     05  SRC-ELM-CONF-LAST-LVL-TIME  PIC 9(4).                    01810400
030500     05  SRC-ELM-CONF-COMP-STAT-FLAG PIC X.                       01810400
030600     05  SRC-ELM-CONF-DELTA-FORMAT   PIC X.                       01810400
030700     05  SRC-ELM-CONF-DELTA-BASE-FLG PIC X.                       01810400
030800         88  SRC-ELM-CONF-BASE-IN-DELTA VALUE 'Y'.                01100010
030900     05  SRC-ELM-LAST-PROC-PACKAGE   PIC X(16).                   01810400
031000     05  SRC-ELM-PACKAGE-TIMESTAMP1  PIC X(4).                    01810400
031100     05  SRC-ELM-PACKAGE-TIMESTAMP2  PIC X(4).                    01810400
031200     05  SRC-ELM-PACKAGE-WITH-OUTPUT PIC X(16).                   01810400
031300     05  SRC-ELM-OPACKAGE-TIMESTAMP1 PIC X(4).                    01810400
031400     05  SRC-ELM-OPACKAGE-TIMESTAMP2 PIC X(4).                    01810400
031500     05  SRC-ELM-PROCESSOR-GROUP     PIC X(8).                    01810400
031600                                                                  01810006
031700*                                                                 01810006
031800*                                                                 01820006
031900*                                                                 01830006
032000 01  SRC-FILE-CONTROL-BLOCK.                                      01840010
032100     05  SRC-FIL-LENGTH          PIC 9(4) COMP SYNC.              01850000
032200     05  SRC-FIL-CURRENT-VERSION PIC 9(4) COMP SYNC.              01860000
032300     05  SRC-FIL-BLOCK-ID        PIC X(4).                        01870010
032400     05  SRC-FIL-DATASET-NAME    PIC X(44).                       01880010
032500     05  SRC-FIL-DATASET-MEMBER  PIC X(10).                       01890010
032600     05  SRC-FIL-DDNAME          PIC X(8).                        01900010
032700     05  SRC-FIL-DATASET-TYPE    PIC X(3).                        01910010
032800     05  FILLER              PIC X(21).                           01920006
032900*                                                                 01930010
033000*                                                                 01940010
033100*                                                                 01950010
033200 01  TGT-ENVIRONMENT-BLOCK.                                       01960010
033300     05  TGT-ENV-LENGTH              PIC 9(4) COMP SYNC.          01970000
033400     05  TGT-ENV-CURRENT-VERSION     PIC 9(4) COMP SYNC.          01980000
033500     05  TGT-ENV-BLOCK-ID            PIC X(4).                    01990010
033600     05  TGT-ENV-FLAGS               PIC 9(4) COMP SYNC.          02000010
033700     05  TGT-ENV-TYPE-OF-BLOCK       PIC X.                       02010010
033800         88  TGT-INTERNAL-C1-BLOCK   VALUE 'C'.                   02020010
033900         88  TGT-EXTERNAL-ENV-BLOCK  VALUE 'E'.                   02030010
034000         88  TGT-ARCHIVE-FILE        VALUE 'A'.                   02031000
034100     05  TGT-ENV-IO-TYPE             PIC X.                       02040010
034200         88  TGT-SOURCE-LOCATION     VALUE 'I'.                   02050010
034300         88  TGT-TARGET-LOCATION     VALUE 'O'.                   02060010
034400     05  TGT-ENV-NEXT-ENV-POINTER    PIC 9(8) COMP SYNC.          02070010
034500     05  TGT-ENV-FILE-POINTER        PIC 9(8) COMP SYNC.          02080010
034600     05  TGT-ENV-SITE-NAME           PIC X.                       02090010
034700     05  TGT-ENV-STAGE-ID            PIC X.                       02100010
034800         88  TGT-STAGE-ONE           VALUE '1'.                   02110010
034900         88  TGT-STAGE-TWO           VALUE '2'.                   02120010
035000     05  TGT-ENV-STAGE-CODE          PIC X.                       02130010
035100     05  TGT-ENV-ELEMENT-CHANGES     PIC X.                               
035200         88  TGT-SOURCE-CHANGES      VALUE 'Y'.                           
035300         88  TGT-NO-SOURCE-CHANGES   VALUE 'N'.                           
035400     05  TGT-ENV-ELEMENT-VERSION     PIC 9(4) COMP SYNC.          02150010
035500     05  TGT-ENV-ELEMENT-LEVEL       PIC 9(4) COMP SYNC.          02160010
035600     05  TGT-ENV-ENVIRONMENT-NAME    PIC X(8).                    02170010
035700     05  TGT-ENV-STAGE-NAME          PIC X(8).                    02180010
035800     05  TGT-ENV-SYSTEM-NAME         PIC X(8).                    02190010
035900     05  TGT-ENV-SYSTEM-DATA         PIC X(8).                    02200010
036000     05  FILLER                      PIC X(8).                    02210010
036100     05  TGT-ENV-SUBSYSTEM-NAME      PIC X(8).                    02260010
036200     05  TGT-ENV-SUBSYSTEM-DATA      PIC X(8).                    02270010
036300     05  TGT-ENV-TYPE-NAME           PIC X(8).                    02280010
036400     05  TGT-ENV-TYPE-DATA           PIC X(8).                    02290010
036500     05  TGT-ENV-ELEMENT-NAME        PIC X(10).                   02300010
036600     05  FILLER                  PIC X(8).                        02310010
036700*                                                                 02320010
036800*                                                                 02330010
036900*                                                                 02340010
037000 01  TGT-ELEMENT-MASTER-INFO-BLOCK.                               02350010
037100     05  TGT-ELM-LENGTH              PIC 9(4) COMP SYNC.          02360000
037200     05  TGT-ELM-CURRENT-VERSION     PIC 9(4) COMP SYNC.          02370000
037300     05  TGT-ELM-BLOCK-ID            PIC X(4).                    02380010
037400     05  TGT-ELM-FLAGS               PIC 9(8) COMP SYNC.          02390010
037500     05  TGT-ELM-ELEMENT-VERSION     PIC 9(4) COMP SYNC.          02400010
037600     05  TGT-ELM-ELEMENT-LEVEL       PIC 9(4) COMP SYNC.          02410010
037700     05  TGT-ELM-NUMBER-INSERTS      PIC 9(8) COMP SYNC.          02420010
037800     05  TGT-ELM-NUMBER-DELETES      PIC 9(8) COMP SYNC.          02430010
037900     05  TGT-ELM-ELEMENT-NAME        PIC X(10).                   02440010
038000     05  TGT-ELM-CURRENT-CCID        PIC X(12).                   02450010
038100     05  TGT-ELM-BASE-LEVEL-NUMBER   PIC 9(4) COMP SYNC.          02460010
038200     05  TGT-ELM-BASE-TOTAL-STMTS    PIC 9(8) COMP SYNC.          02470010
038300     05  TGT-ELM-BASE-DATE           PIC 9(6).                    02480010
038400     05  TGT-ELM-BASE-TIME           PIC 9(4).                    02490010
038500     05  TGT-ELM-BASE-COMMENT        PIC X(40).                   02500010
038600     05  TGT-ELM-BASE-USERID         PIC X(8).                    02510010
038700     05  TGT-ELM-GEN-USERID          PIC X(8).                    02520010
038800     05  TGT-ELM-GEN-DATE            PIC 9(6).                    02530010
038900     05  TGT-ELM-GEN-TIME            PIC 9(4).                    02540010
039000     05  TGT-ELM-FROM-DSN            PIC X(44).                   02550010
039100     05  TGT-ELM-FROM-MEMBER         PIC X(10).                   02560010
039200     05  TGT-ELM-PROCESSOR-FLAG      PIC X.                       02570010
039300     05  FILLER                  PIC X(3).                        02580010
039400     05  TGT-ELM-PROCESSOR-LAST-DATE PIC 9(6).                    02590010
039500     05  TGT-ELM-PROCESSOR-LAST-TIME PIC 9(4).                    02600010
039600     05  TGT-ELM-PROCESSOR-USERID    PIC X(8).                    02610010
039700     05  TGT-ELM-PROCESSOR-NAME      PIC X(10).                   02620010
039800     05  TGT-ELM-PROCESSOR-RC        PIC 9(4) COMP SYNC.          02630010
039900     05  TGT-ELM-PROCESSOR-C1-RC     PIC 9(4) COMP SYNC.          02640010
040000     05  TGT-ELM-PROCESSOR-LAST-COMMENT PIC X(40).                02650010
040100     05  TGT-ELM-LEVEL-COMMENT       PIC X(40).                   02660010
040200     05  TGT-ELM-LEVEL-DATE          PIC 9(6).                    02670010
040300     05  TGT-ELM-LEVEL-TIME          PIC 9(4).                    02680010
040400     05  TGT-ELM-LEVEL-USERID        PIC X(8).                    02690010
040500     05  TGT-ELM-LAST-ACTION         PIC X(8).                    02700010
040600     05  TGT-ELM-LAST-LEVEL-TOTAL    PIC 9(8) COMP SYNC.          02710010
040700     05  TGT-ELM-MOVE-DATE           PIC 9(6).                    02720010
040800     05  TGT-ELM-MOVE-TIME           PIC 9(4).                    02730010
040900     05  TGT-ELM-MOVE-USERID         PIC X(8).                    02740010
041000     05  TGT-ELM-RETRIEVE-DATE       PIC 9(6).                    02750010
041100     05  TGT-ELM-RETRIEVE-TIME       PIC 9(4).                    02760010
041200     05  TGT-ELM-RETRIEVE-USERID     PIC X(8).                    02770010
041300     05  TGT-ELM-RETRIEVE-COMMENT    PIC X(40).                   02780010
041400     05  TGT-ELM-RETRIEVE-TO-DSN     PIC X(44).                   02790010
041500     05  TGT-ELM-RETRIEVE-TO-MEMBER  PIC X(10).                   02800010
041600     05  FILLER                  PIC X(30).                       02810010
041700     05  TGT-ELM-RET-CCID            PIC X(12).                   02811000
041800     05  TGT-ELM-GEN-CCID            PIC X(12).                   02812000
041900     05  TGT-ELM-MOD-ACTION          PIC X(8).                    02813000
042000     05  TGT-ELM-ACTION-CCID         PIC X(12).                   02814000
042100     05  TGT-ELM-ACTION-COMM         PIC X(40).                   02815000
042200     05  TGT-ELM-ACTION-USER         PIC X(8).                    02816000
042300     05  TGT-ELM-ACTION-DATE         PIC 9(6).                    02817000
042400     05  TGT-ELM-ACTIOM-TIME         PIC 9(4).                    02818000
042500     05  TGT-ELM-SIGNOUT-ID          PIC X(8).                    02819000
042600     05  TGT-ELM-LOC-FLAG            PIC X.                       02819100
042700     05  TGT-ELM-FR-ACTION           PIC X(8).                    02819200
042800     05  TGT-ELM-FR-ACT-USERID       PIC X(8).                    02819300
042900     05  TGT-ELM-FR-ACT-DATE         PIC 9(6).                    02819400
043000     05  TGT-ELM-FR-ACT-TIME         PIC 9(4).                    02819500
043100     05  TGT-ELM-FR-SITE             PIC X.                       02819600
043200     05  TGT-ELM-FR-ENV              PIC X(8).                    02819700
043300     05  TGT-ELM-FR-SYS              PIC X(8).                    02819800
043400     05  TGT-ELM-FR-SUBSYS           PIC X(8).                    02819900
043500     05  TGT-ELM-FR-ELEMENT          PIC X(10).                   02820000
043600     05  TGT-ELM-FR-TYPE             PIC X(8).                    02820100
043700     05  TGT-ELM-FR-STG-NBR          PIC X.                       02820200
043800     05  TGT-ELM-FR-VER              PIC 9(4) COMP SYNC.          02820300
043900     05  TGT-ELM-FR-LEVEL            PIC 9(4) COMP SYNC.          02820400
044000                                                                          
044100* NEW ELEMENT FIELDS FOR E/MVS 3.6 FOLLOW                         01810006
044200                                                                  01810500
044300     05  TGT-ELM-UPDATE-MEMBER-NAME  PIC X(10).                   01810400
044400     05  TGT-ELM-BASE-MEMBER-NAME    PIC X(10).                   01810400
044500     05  TGT-ELM-LOWER-LEVEL-IND     PIC X(2).                    01810400
044600     05  TGT-ELM-PCT-PREV-INSERT-DEL PIC X.                       01810400
044700     05  TGT-ELM-PCT-PREV-DELS-REINS PIC X.                       01810400
044800     05  TGT-ELM-ESD-FP-NAME-IF-OBJ  PIC X(8).                    01810400
044900     05  TGT-ELM-DELTA-FORMAT        PIC X.                       01810400
045000         88  TGT-ELM-FORWARD-DELTA   VALUE 'F'.                   01100010
045100         88  TGT-ELM-REVVERSE-DELTA  VALUE 'R'.                   01110010
045200     05  TGT-ELM-PACKED-INDICATOR    PIC X.                       01810400
045300         88  TGT-ELM-BASE-NOT-PACKED VALUE 'Y'.                   01100010
045400     05  TGT-ELM-LAST-PROC-VERSION   PIC X.                       01810400
045500     05  TGT-ELM-LAST-PROC-LEVEL     PIC X.                       01810400
045600     05  TGT-ELM-RECORD-FORMAT       PIC X.                       01810400
045700     05  TGT-ELM-CONF-DELTA-MEM-ID   PIC X(8).                    01810400
045800     05  TGT-ELM-CONF-DELTA-VERSION  PIC S9(4) COMP SYNC.         01810400
045900     05  TGT-ELM-CONF-BASE-TOTAL     PIC S9(8) COMP SYNC.         01810400
046000     05  TGT-ELM-CONF-LAST-LVL-TOTAL PIC S9(8) COMP SYNC.         01810400
046100     05  TGT-ELM-CONF-BASE-LVL-NBR   PIC S9(4) COMP SYNC.         01810400
046200     05  TGT-ELM-CONF-LAST-LVL-NBR   PIC S9(4) COMP SYNC.         01810400
046300     05  TGT-ELM-INSERTS-LAST-LEVEL  PIC S9(4) COMP SYNC.         01810400
046400     05  TGT-ELM-DELETES-LAST-LEVEL  PIC S9(4) COMP SYNC.         01810400
046500     05  TGT-ELM-CONF-REGRES-INS-PCT PIC S9(4) COMP SYNC.         01810400
046600     05  TGT-ELM-CONF-REGRES-DEL-PCT PIC S9(4) COMP SYNC.         01810400
046700     05  TGT-ELM-CONF-BASE-DATE      PIC 9(6).                    01810400
046800     05  TGT-ELM-CONF-BASE-TIME      PIC 9(4).                    01810400
046900     05  TGT-ELM-CONF-LAST-LVL-DATE  PIC 9(6).                    01810400
047000     05  TGT-ELM-CONF-LAST-LVL-TIME  PIC 9(4).                    01810400
047100     05  TGT-ELM-CONF-COMP-STAT-FLAG PIC X.                       01810400
047200     05  TGT-ELM-CONF-DELTA-FORMAT   PIC X.                       01810400
047300     05  TGT-ELM-CONF-DELTA-BASE-FLG PIC X.                       01810400
047400         88  TGT-ELM-CONF-BASE-IN-DELTA VALUE 'Y'.                01100010
047500     05  TGT-ELM-LAST-PROC-PACKAGE   PIC X(16).                   01810400
047600     05  TGT-ELM-PACKAGE-TIMESTAMP1  PIC X(4).                    01810400
047700     05  TGT-ELM-PACKAGE-TIMESTAMP2  PIC X(4).                    01810400
047800     05  TGT-ELM-PACKAGE-WITH-OUTPUT PIC X(16).                   01810400
047900     05  TGT-ELM-OPACKAGE-TIMESTAMP1 PIC X(4).                    01810400
048000     05  TGT-ELM-OPACKAGE-TIMESTAMP2 PIC X(4).                    01810400
048100     05  TGT-ELM-PROCESSOR-GROUP     PIC X(8).                    01810400
048200*                                                                 02820010
048300*                                                                 02830010
048400*                                                                 02840010
048500 01  TGT-FILE-CONTROL-BLOCK.                                      02850010
048600     05  TGT-FIL-LENGTH          PIC 9(4) COMP SYNC.              02860000
048700     05  TGT-FIL-CURRENT-VERSION PIC 9(4) COMP SYNC.              02870000
048800     05  TGT-FIL-BLOCK-ID        PIC X(4).                        02880010
048900     05  TGT-FIL-DATASET-NAME    PIC X(44).                       02890010
049000     05  TGT-FIL-DATASET-MEMBER  PIC X(10).                       02900010
049100     05  TGT-FIL-DDNAME          PIC X(8).                        02910010
049200     05  TGT-FIL-DATASET-TYPE    PIC X(3).                        02920010
049300     05  FILLER              PIC X(21).                           02930010
049600 PROCEDURE DIVISION USING EXIT-CONTROL-BLOCK,                             
049601                          REQUEST-INFO-BLOCK,                             
049602                          SRC-ENVIRONMENT-BLOCK,                          
049800                          SRC-ELEMENT-MASTER-INFO-BLOCK,                  
049900                          SRC-FILE-CONTROL-BLOCK,                         
050000                          TGT-ENVIRONMENT-BLOCK,                          
050100                          TGT-ELEMENT-MASTER-INFO-BLOCK,                  
050200                          TGT-FILE-CONTROL-BLOCK.                         
050210     IF RETRIEVE-ACTION AND                                               
050211        SIGNOUT-OVERRIDE AND                                              
050212        SRC-ELM-RETRIEVE-USERID NOT EQUAL TO '        ' AND             00
050213        SRC-ELM-LAST-ACTION = 'RETRIEVE'                                  
050214           MOVE SRC-ELM-SIGNOUT-ID TO CURRENT-OWNER-USERID                
050220           MOVE SRC-ELM-SIGNOUT-ID TO SIGNOUT-USERID                      
050230           MOVE SRC-ENV-ENVIRONMENT-NAME TO ENVIRONMENT-NAME              
050231           MOVE SRC-ENV-SYSTEM-NAME TO SYSTEM-NAME                        
050232           MOVE SRC-ENV-SUBSYSTEM-NAME TO SUBSYSTEM-NAME                  
050293           MOVE SRC-ENV-ELEMENT-NAME TO ELEMENT-NAME                      
050294           CALL 'SNDEMAIL' USING EMAIL-MESSAGE.                           
050320     MOVE +0 TO ECB-RETURN-CODE.                                          
050400     GOBACK.                                                              

