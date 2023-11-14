000100 ID DIVISION.                                                             
000200 PROGRAM-ID.                     STARTREK.                                
000300     SKIP1                                                                
000400 AUTHOR.                         RICK JONES.                              
000500 DATE-WRITTEN.                   JULY 1977.                               
000700*REMARKS.                                                                 
000800*                                                                         
000900*    THIS IS THE MAIN PROGRAM FOR 'STAR TREK'.                            
001000*                                                                         
001100*    THIS PROGRAM WAS CONVERTED TO ISPF FROM A CICS PROGRAM               
001200*    WRITTEN BY RICK JONES THAT APPEARED ON THE SHARE CICS                
001300*    MOD TAPE.                                                            
001400*                                                                         
001500*    IT'S DESIGN IS BASED ON 'SUPER STAR TREK' BY ROBERT LEEDOM &         
001600*    DAVID AHL, AS IT APPEARED IN 'THE BEST OF CREATIVE COMPUTING,        
001700*    VOLUME 1'.                                                           
001800*                                                                         
001900 ENVIRONMENT DIVISION.                                                    
002000 CONFIGURATION SECTION.                                                   
002100   SOURCE-COMPUTER.   IBM-370.                                            
002200   OBJECT-COMPUTER.   IBM-370.                                            
002300 INPUT-OUTPUT SECTION.                                                    
002400 FILE-CONTROL.                                                            
002500     SELECT GAME-FILE ASSIGN TO S-GAMEFILE.                               
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800 FILE SECTION.                                                            
002900 FD  GAME-FILE,                                                           
003000     LABEL RECORDS ARE STANDARD,                                          
003100     RECORD CONTAINS 425 CHARACTERS,                                      
003200     BLOCK CONTAINS 1 RECORDS,                                            
003300     DATA RECORD IS GAME-FILE-REC.                                        
003400 01  GAME-RECORD      PIC X(425).                                         
003500 WORKING-STORAGE SECTION.                                                 
003600     SKIP1                                                                
003700 01  TIME-VAR       PIC X(5).                                             
003800 01  TIME-VAR-BO REDEFINES TIME-VAR.                                      
003900     05  TIME-HOURS   PIC 99.                                             
004000     05  FILLER PIC X.                                                    
004100     05  TIME-MINS PIC 99.                                                
004200 01  TIME-VAR-NUM.                                                        
004300     05  TIME-HOURS   PIC 99.                                             
004400     05  TIME-MINS PIC 99.                                                
004500 01  TIME-VAR-NUM2    PIC 9999999  VALUE 0.                               
004600 01  CONSTANTS.                                                           
004700     12  LITERALS.                                                        
004800         16  L-INDX              PIC S9.                                  
004900         16  L-MULT              PIC S9(5)   VALUE +31621  COMP-3.        
005000         16  L-NULL-TABLE        PIC X(64)   VALUE ALL ' '.               
005100         16  L-NULL              PIC X       VALUE ' '.                   
005200         16  L-KLING             PIC X       VALUE 'K'.                   
005300         16  L-SBASE             PIC X       VALUE 'B'.                   
005400         16  L-STARS             PIC X       VALUE '*'.                   
005500         16  L-EPRISE            PIC X       VALUE 'E'.                   
005600         16  L-COLON             PIC X       VALUE ':'.                   
005700         16  L-NAV               PIC XXX     VALUE 'NAV'.                 
005800         16  L-LRS               PIC XXX     VALUE 'LRS'.                 
005900         16  L-PHA               PIC XXX     VALUE 'PHA'.                 
006000         16  L-TOR               PIC XXX     VALUE 'TOR'.                 
006100         16  L-SHE               PIC XXX     VALUE 'SHE'.                 
006200         16  L-DAM               PIC XXX     VALUE 'DAM'.                 
006300         16  L-COM               PIC XXX     VALUE 'COM'.                 
006400         16  L-END               PIC XXX     VALUE 'SUR'.                 
006500         16  L-SAV               PIC XXX     VALUE 'SAV'.                 
006600         16  L-RES               PIC XXX     VALUE 'RES'.                 
006700         16  L-LRS-TOP           PIC X(50)                                
006800                 VALUE '  ----- ----- ----- ----- ----- ----- ----        
006900-                      '- ----- '.                                        
007000         16  L-ROMAN             PIC X(21)                                
007100                 VALUE ' I    II    III   IV '.                           
007200     12  DAM-NAME-LIST.                                                   
007300         16  FILLER              PIC X(12)   VALUE 'WARP ENGINES'.        
007400         16  FILLER              PIC X(12)   VALUE 'S.R. SENSORS'.        
007500         16  FILLER              PIC X(12)   VALUE 'L.R. SENSORS'.        
007600         16  FILLER              PIC X(12)   VALUE 'PHASER CNTRL'.        
007700         16  FILLER              PIC X(12)   VALUE 'PHOTON TUBES'.        
007800         16  FILLER              PIC X(12)   VALUE 'DAMAGE CNTRL'.        
007900         16  FILLER              PIC X(12)   VALUE 'SHIELD CNTRL'.        
008000         16  FILLER              PIC X(12)   VALUE 'COMPUTER    '.        
008100     12  DAM-NAME-TABLE  REDEFINES DAM-NAME-LIST.                         
008200         16  DAM-NAME        OCCURS 8 TIMES                               
008300                                 PIC X(12).                               
008400     12  C-LIST.                                                          
008500         16  FILLER              PIC S9  VALUE -1    COMP-3.              
008600         16  FILLER              PIC S9  VALUE +0    COMP-3.              
008700         16  FILLER              PIC S9  VALUE -1    COMP-3.              
008800         16  FILLER              PIC S9  VALUE +1    COMP-3.              
008900         16  FILLER              PIC S9  VALUE +0    COMP-3.              
009000         16  FILLER              PIC S9  VALUE +1    COMP-3.              
009100         16  FILLER              PIC S9  VALUE +1    COMP-3.              
009200         16  FILLER              PIC S9  VALUE +1    COMP-3.              
009300         16  FILLER              PIC S9  VALUE +1    COMP-3.              
009400         16  FILLER              PIC S9  VALUE +0    COMP-3.              
009500         16  FILLER              PIC S9  VALUE +1    COMP-3.              
009600         16  FILLER              PIC S9  VALUE -1    COMP-3.              
009700         16  FILLER              PIC S9  VALUE +0    COMP-3.              
009800         16  FILLER              PIC S9  VALUE -1    COMP-3.              
009900         16  FILLER              PIC S9  VALUE -1    COMP-3.              
010000         16  FILLER              PIC S9  VALUE -1    COMP-3.              
010100         16  FILLER              PIC S9  VALUE -1    COMP-3.              
010200         16  FILLER              PIC S9  VALUE +0    COMP-3.              
010300     12  C-TABLE REDEFINES C-LIST.                                        
010400         16  FILLER          OCCURS 9 TIMES                               
010500                             INDEXED BY C-X1.                             
010600             20  C-VERT          PIC S9              COMP-3.              
010700             20  C-HORZ          PIC S9              COMP-3.              
010800     12  REGION-AREA.                                                     
010900         16  REGION-LIST.                                                 
011000             20  FILLER          PIC X(33)                                
011100                     VALUE 'ANTARES    SIRIUS     RIGEL      '.           
011200             20  FILLER          PIC X(33)                                
011300                     VALUE 'DENEB      PROCYON    CAPELLA    '.           
011400             20  FILLER          PIC X(33)                                
011500                     VALUE 'VEGA       BETELGEUSE CANOPUS    '.           
011600             20  FILLER          PIC X(33)                                
011700                     VALUE 'ALDEBARAN  ALTAIR     REGULUS    '.           
011800             20  FILLER          PIC X(33)                                
011900                     VALUE 'SAGITTARIUSANTARES    POLLUX     '.           
012000             20  FILLER          PIC X(11)                                
012100                     VALUE 'SPICA      '.                                 
012200         16  REGION-TABLE  REDEFINES REGION-LIST.                         
012300             20  REGION-NAME         OCCURS 16 TIMES                      
012400                                     INDEXED BY REGION-X1                 
012500                                 PIC X(11).                               
012600     12  INIT-AREA.                                                       
012700         16  INIT-TORPEDOS       PIC S999    VALUE +10   COMP-3.          
012800         16  INIT-ENERGY-EPRISE  PIC S9(5)   VALUE +3000 COMP-3.          
012900         16  INIT-ENERGY-KLING   PIC S999    VALUE +200  COMP-3.          
013000         16  MAX-KLING           PIC S9      VALUE +3    COMP-3.          
013100         16  TOR-ENERGY          PIC S999    VALUE +20   COMP-3.          
013200     SKIP1                                                                
013300 01  MISC-WORK.                                                           
013400         16  WRAPUP-CODE         PIC 9 VALUE 0.                           
013500         16  NAV-EDIT-FLAG       PIC X.                                   
013600         16  COURSE              PIC S9V99           COMP-3.              
013700         16  WARP-FACTOR         PIC S9V99           COMP-3.              
013800         16  WARP-ENERGY         PIC S9(5)           COMP-3.              
013900         16  STARDATES-USED      PIC S99V9           COMP-3.              
014000         16  EPRISE-MOVE-FLAG    PIC X.                                   
014100         16  SECTORS-MOVE-CURR   PIC S999            COMP-3.              
014200         16  SECTORS-MOVE-TOTL   PIC S999            COMP-3.              
014300         16  OBSTACLE            PIC X.                                   
014400         16  GALAXY-LIMIT-FLAG   PIC S9              COMP-3.              
014500         16  PREV-GLXY-ROW       PIC S9              COMP-3.              
014600         16  PREV-GLXY-COL       PIC S9              COMP-3.              
014700         16  QUAD-NAME-ROW       PIC S9              COMP-3.              
014800         16  QUAD-NAME-COL       PIC S9              COMP-3.              
014900         16  REGION-ONLY-FLAG    PIC S9              COMP-3.              
015000         16  QUAD-NAME-AREA.                                              
015100             20  QUAD-NAME.                                               
015200                 24  QUAD-NAME-BYTE  OCCURS 16 TIMES                      
015300                                     INDEXED BY QUAD-X1                   
015400                                 PIC X.                                   
015500         16  SHIELD-ENERGY       PIC S9(5)           COMP-3.              
015600         16  PHASER-ENERGY       PIC S9(5)           COMP-3.              
015700         16  DIST                PIC S99V999         COMP-3.              
015800         16  HIT                 PIC S9(3)           COMP-3.              
015900         16  HIT-RATIO           PIC S999V99         COMP-3.              
016000         16  COMPUTER-REQUEST    PIC S9              COMP-3.              
016100         16  DAM-SUB             PIC S9              COMP-3.              
016200         16  DAM-HDG-FLAG        PIC X.                                   
016300         16  REPAIR-EST          PIC S99V9           COMP-3.              
016400         16  DAM-DELAY           PIC S99V9           COMP-3.              
016500         16  WORK-VERT           PIC S999V99         COMP-3.              
016600         16  WORK-HORZ           PIC S999V99         COMP-3.              
016700         16  STEP-VERT           PIC S9V99           COMP-3.              
016800         16  STEP-HORZ           PIC S9V99           COMP-3.              
016900         16  TOR-ROW             PIC S9              COMP-3.              
017000         16  TOR-COL             PIC S9              COMP-3.              
017100         16  RANDOM-ROW          PIC S9              COMP-3.              
017200         16  RANDOM-COL          PIC S9              COMP-3.              
017300         16  START-TIME          PIC S9(7)           COMP-3.              
017400         16  SEED1A              PIC  9(7).                               
017500         16  SEED1B  REDEFINES SEED1A.                                    
017600             20  SEED-1          PIC 9.                                   
017700             20  SEED-2          PIC 9.                                   
017800             20  SEED-3          PIC 9.                                   
017900             20  SEED-4          PIC 9.                                   
018000             20  SEED-5          PIC 9.                                   
018100             20  SEED-6          PIC 9.                                   
018200             20  SEED-7          PIC 9.                                   
018300         16  SEED2A              PIC  9(7).                               
018400         16  SEED2B  REDEFINES SEED2A.                                    
018500             20  SEED-7          PIC 9.                                   
018600             20  SEED-6          PIC 9.                                   
018700             20  SEED-5          PIC 9.                                   
018800             20  SEED-4          PIC 9.                                   
018900             20  SEED-3          PIC 9.                                   
019000             20  SEED-2          PIC 9.                                   
019100             20  SEED-1          PIC 9.                                   
019200         16  CURPOS              PIC S9(8)                 COMP.          
019300         16  TEMP-999            PIC S999            COMP-3.              
019400         16  TEMP-999V9999       PIC S999V9999       COMP-3.              
019500         16  TEMP-9-9            PIC 9.9.                                 
019600         16  TEMP-Z9             PIC Z9.                                  
019700         16  TEMP-ZZVZ           PIC ZZ.Z.                                
019800         16  TEMP-ZZZ9           PIC ZZZ9.                                
019900     12  TS-KEY.                                                          
020000         16  TS-KEY-1-4          PIC X(4).                                
020100         16  TS-KEY-5-8          PIC X(4).                                
020200     12  C30-AREA.                                                        
020300         16  C30-MSG                 PIC XXX.                             
020400         16  C30-FLAGS.                                                   
020500             20  C30-NON-BLANK-FLAG  PIC X.                               
020600             20  C30-NEG-FLAG        PIC X.                               
020700             20  C30-DECIMAL-FLAG    PIC X.                               
020800         16  C30-DIGIT-CTR           PIC S999    COMP-3.                  
020900         16  C30-DECIMAL-CTR         PIC S999    COMP-3.                  
021000         16  C30-IN-QTY.                                                  
021100             20  C30-I-CHAR          PIC X   OCCURS 4 TIMES               
021200                                             INDEXED BY C30-IX1.          
021300         16  C30-OUT-QTY             PIC S9(4).                           
021400         16  FILLER  REDEFINES C30-OUT-QTY.                               
021500             20  C30-O-CHAR          PIC X   OCCURS 4 TIMES               
021600                                             INDEXED BY C30-OX1.          
021700         16  C30-ADJ-QTY         PIC S9(4)V999  COMP-3.                   
021800     12  MSG-LINE.                                                        
021900         16  MSG-LINE01          PIC X.                                   
022000         16  MSG-LINE02          PIC X.                                   
022100         16  MSG-LINE03          PIC X.                                   
022200         16  MSG-LINE04          PIC X.                                   
022300         16  MSG-LINE05          PIC X.                                   
022400         16  MSG-LINE06          PIC X.                                   
022500         16  MSG-LINE07          PIC X.                                   
022600         16  MSG-LINE08          PIC X.                                   
022700         16  MSG-LINE09          PIC X.                                   
022800         16  MSG-LINE10          PIC X.                                   
022900         16  MSG-LINE11          PIC X.                                   
023000         16  MSG-LINE12          PIC X.                                   
023100         16  MSG-LINE13          PIC X.                                   
023200         16  MSG-LINE14          PIC X.                                   
023300         16  MSG-LINE15          PIC X.                                   
023400         16  MSG-LINE16          PIC X.                                   
023500         16  MSG-LINE17          PIC X.                                   
023600         16  MSG-LINE18          PIC X.                                   
023700         16  MSG-LINE19          PIC X.                                   
023800         16  MSG-LINE20          PIC X.                                   
023900         16  MSG-LINE21          PIC X.                                   
024000         16  MSG-LINE22          PIC X.                                   
024100         16  MSG-LINE23          PIC X.                                   
024200         16  MSG-LINE24          PIC X.                                   
024300         16  MSG-LINE25          PIC X.                                   
024400         16  MSG-LINE26          PIC X.                                   
024500         16  MSG-LINE27          PIC X.                                   
024600         16  MSG-LINE28          PIC X.                                   
024700         16  MSG-LINE29          PIC X.                                   
024800         16  MSG-LINE30          PIC X.                                   
024900         16  MSG-LINE31          PIC X.                                   
025000         16  MSG-LINE32          PIC X.                                   
025100         16  MSG-LINE33          PIC X.                                   
025200         16  MSG-LINE34          PIC X.                                   
025300         16  MSG-LINE35          PIC X.                                   
025400         16  MSG-LINE36          PIC X.                                   
025500         16  MSG-LINE37          PIC X.                                   
025600         16  MSG-LINE38          PIC X.                                   
025700         16  MSG-LINE39          PIC X.                                   
025800         16  MSG-LINE40          PIC X.                                   
025900         16  MSG-LINE41          PIC X.                                   
026000         16  MSG-LINE42          PIC X.                                   
026100         16  MSG-LINE43          PIC X.                                   
026200         16  MSG-LINE44          PIC X.                                   
026300         16  MSG-LINE45          PIC X.                                   
026400         16  MSG-LINE46          PIC X.                                   
026500         16  MSG-LINE47          PIC X.                                   
026600         16  MSG-LINE48          PIC X.                                   
026700         16  MSG-LINE49          PIC X.                                   
026800         16  MSG-LINE50          PIC X.                                   
026900     12  LRS-LINE  REDEFINES MSG-LINE.                                    
027000         16  LRS-ROW             PIC 9.                                   
027100         16  LRS-SEP1            PIC X.                                   
027200         16  FILLER                  OCCURS 8 TIMES                       
027300                                     INDEXED BY LRS-X1.                   
027400             20  FILLER          PIC X.                                   
027500             20  LRS-SECT        PIC ZZ9.                                 
027600             20  FILLER          PIC X.                                   
027700             20  LRS-SEP         PIC X.                                   
027800     12  GAL-LINE  REDEFINES MSG-LINE.                                    
027900         16  FILLER              PIC XXX.                                 
028000         16  GAL-LEFT.                                                    
028100             20  GAL-MARG1       PIC X(4).                                
028200             20  FILLER          PIC X.                                   
028300             20  GAL-LEFT-NAME   PIC X(11).                               
028400             20  FILLER          PIC X.                                   
028500             20  GAL-MARG2       PIC X(4).                                
028600         16  FILLER              PIC XXX.                                 
028700         16  GAL-RIGHT.                                                   
028800             20  GAL-MARG3       PIC X(4).                                
028900             20  FILLER          PIC X.                                   
029000             20  GAL-RIGHT-NAME  PIC X(11).                               
029100             20  FILLER          PIC X.                                   
029200             20  GAL-MARG4       PIC X(4).                                
029300         16  FILLER              PIC XX.                                  
029400     66  MSG-LINE01-20  RENAMES MSG-LINE01 THRU MSG-LINE20.               
029500     66  MSG-LINE01-35  RENAMES MSG-LINE01 THRU MSG-LINE35.               
029600     66  MSG-LINE05-06  RENAMES MSG-LINE05 THRU MSG-LINE06.               
029700     66  MSG-LINE14-29  RENAMES MSG-LINE14 THRU MSG-LINE29.               
029800     66  MSG-LINE27-42  RENAMES MSG-LINE27 THRU MSG-LINE42.               
029900     66  MSG-LINE17-18  RENAMES MSG-LINE17 THRU MSG-LINE18.               
030000     66  MSG-LINE39-42  RENAMES MSG-LINE39 THRU MSG-LINE42.               
030100     66  MSG-LINE18-19  RENAMES MSG-LINE18 THRU MSG-LINE19.               
030200     66  MSG-LINE13-14  RENAMES MSG-LINE13 THRU MSG-LINE14.               
030300     66  MSG-LINE28-31  RENAMES MSG-LINE28 THRU MSG-LINE31.               
030400     66  MSG-LINE35-37  RENAMES MSG-LINE35 THRU MSG-LINE37.               
030500     66  MSG-LINE37-40  RENAMES MSG-LINE37 THRU MSG-LINE40.               
030600     66  MSG-LINE38-39  RENAMES MSG-LINE38 THRU MSG-LINE39.               
030700     66  MSG-LINE10-11  RENAMES MSG-LINE10 THRU MSG-LINE11.               
030800     66  MSG-LINE10-13  RENAMES MSG-LINE10 THRU MSG-LINE13.               
030900     66  MSG-LINE11-12  RENAMES MSG-LINE11 THRU MSG-LINE12.               
031000     66  MSG-LINE11-22  RENAMES MSG-LINE11 THRU MSG-LINE22.               
031100     66  MSG-LINE12-13  RENAMES MSG-LINE12 THRU MSG-LINE13.               
031200     66  MSG-LINE01-04  RENAMES MSG-LINE01 THRU MSG-LINE04.               
031300     66  MSG-LINE20-23  RENAMES MSG-LINE20 THRU MSG-LINE23.               
031400     66  MSG-LINE19-22  RENAMES MSG-LINE19 THRU MSG-LINE22.               
031500     66  MSG-LINE31-34  RENAMES MSG-LINE31 THRU MSG-LINE34.               
031600     66  MSG-LINE32-34  RENAMES MSG-LINE32 THRU MSG-LINE34.               
031700     66  MSG-LINE22-25  RENAMES MSG-LINE22 THRU MSG-LINE25.               
031800     66  MSG-LINE23-26  RENAMES MSG-LINE23 THRU MSG-LINE26.               
031900     66  MSG-LINE24-27  RENAMES MSG-LINE24 THRU MSG-LINE27.               
032000     66  MSG-LINE24-35  RENAMES MSG-LINE24 THRU MSG-LINE35.               
032100     66  MSG-LINE26-29  RENAMES MSG-LINE26 THRU MSG-LINE29.               
032200     66  MSG-LINE16-19  RENAMES MSG-LINE16 THRU MSG-LINE19.               
032300     66  MSG-LINE27-30  RENAMES MSG-LINE27 THRU MSG-LINE30.               
032400     66  MSG-LINE44-47  RENAMES MSG-LINE44 THRU MSG-LINE47.               
032500     66  MSG-LINE47-50  RENAMES MSG-LINE47 THRU MSG-LINE50.               
032600     SKIP1                                                                
032700 01  MAP1O.                                                               
032800     12  DIALOG-V7.                                                       
032900         16  M1-QUAD-NAME            PIC X(16).                           
033000     12  M1-SECTOR-TABLE.                                                 
033100         16  M1-ROW                  OCCURS 8 TIMES                       
033200                                     INDEXED BY M1-X1.                    
033300             20  M1-COL              OCCURS 8 TIMES                       
033400                                     INDEXED BY M1-X2.                    
033500                 24  FILLER      PIC X(2).                                
033600                 24  M1-DISP     PIC X.                                   
033700     12  DIALOG-V1 REDEFINES M1-SECTOR-TABLE.                             
033800         16  FILLER   PIC X(192).                                         
033900     12  DIALOG-V3.                                                       
034000         16  M1-STARDATE             PIC 9999.9.                          
034100         16  M1-CONDITION            PIC X(6).                            
034200     12  DIALOG-V4.                                                       
034300         16  M1-GLXY-ROW             PIC 9.                               
034400         16  M1-QUAD-SEP             PIC X.                               
034500         16  M1-GLXY-COL             PIC 9.                               
034600         16  M1-SECT-ROW             PIC 9.                               
034700         16  M1-SECT-SEP             PIC X.                               
034800         16  M1-SECT-COL             PIC 9.                               
034900         16  CMDI                    PIC XXX.                             
035000     12  M1-TORPEDOS             PIC Z9.                                  
035100     12  DIALOG-V5.                                                       
035200         16  FLDA     PIC 9999.                                           
035300         16  FLDB     PIC 9999.                                           
035400         16  M1-ENERGY               PIC ZZZ9.                            
035500         16  M1-SHIELD               PIC ZZZ9.                            
035600     12  DIALOG-V6.                                                       
035700         16  MESSAGEO                PIC X(53).                           
035800     SKIP1                                                                
035900 01  MAP2O.                                                               
036000     12  MLINE-AREA.                                                      
036100         16  MLINE-AREA-ROWS         OCCURS 23 TIMES                      
036200                                     INDEXED BY MLINE-X1.                 
036300             20  MLINE           PIC X(50).                               
036400     12  DIALOG-V2  REDEFINES MLINE-AREA.                                 
036500         16 FILLER  PIC X(1150).                                          
036600     SKIP1                                                                
036700   01  TEMPSTRG.                                                          
036800     12  CURR-STATUS.                                                     
036900         16  ABEND-CODE          PIC X(4).                                
037000         16  STARDATE            PIC S9(4)V9         COMP-3.              
037100         16  ENERGY              PIC S9(5)           COMP-3.              
037200         16  TORPEDOS            PIC S999            COMP-3.              
037300         16  SHIELD              PIC S9(5)           COMP-3.              
037400         16  CURR-QUAD           PIC X(16).                               
037500         16  CONDITION           PIC X(6).                                
037600         16  EPRISE-VERT         PIC S9V999.                              
037700         16  FILLER  REDEFINES EPRISE-VERT.                               
037800             20  EPRISE-ROW      PIC  9.                                  
037900             20  FILLER          PIC XXX.                                 
038000         16  EPRISE-HORZ         PIC S9V999.                              
038100         16  FILLER  REDEFINES EPRISE-HORZ.                               
038200             20  EPRISE-COL      PIC  9.                                  
038300             20  FILLER          PIC XXX.                                 
038400         16  EPRISE-GLXY-ROW     PIC S9              COMP-3.              
038500         16  EPRISE-GLXY-COL     PIC S9              COMP-3.              
038600         16  DOCKING-FLAG        PIC X.                                   
038700         16  SBASE-ROW           PIC S9              COMP-3.              
038800         16  SBASE-COL           PIC S9              COMP-3.              
038900         16  CURR-KLING          PIC S9              COMP-3.              
039000         16  CURR-SBASE          PIC S9              COMP-3.              
039100         16  CURR-STARS          PIC S9              COMP-3.              
039200         16  SBASE-TOTAL         PIC S999            COMP-3.              
039300         16  KLING-TOTAL         PIC S999            COMP-3.              
039400         16  ORIG-SBASE          PIC S999            COMP-3.              
039500         16  ORIG-KLING          PIC S999            COMP-3.              
039600         16  FIRST-STARDATE      PIC S9(5)           COMP-3.              
039700         16  ENDDATE             PIC S9(5)           COMP-3.              
039800         16  RND-MULT            PIC S9(9)           COMP-3.              
039900         16  RND                 PIC SV9(9)          COMP-3.              
040000         16  RND-WORK  REDEFINES RND                                      
040100                                 PIC S9(9)           COMP-3.              
040200     12  KLING-AREA.                                                      
040300         16  KLING-TABLE     OCCURS 3 TIMES                               
040400                             INDEXED BY KLING-X1.                         
040500             20  KLING-ROW       PIC S9              COMP-3.              
040600             20  KLING-COL       PIC S9              COMP-3.              
040700             20  KLING-ENERGY    PIC S999            COMP-3.              
040800     12  SECTOR-AREA.                                                     
040900         16  SECTOR-ROW              OCCURS 8 TIMES                       
041000                                     INDEXED BY SECTOR-X1.                
041100             20  SECTOR-COL          OCCURS 8 TIMES                       
041200                                     INDEXED BY SECTOR-X2.                
041300                 24  SECTOR-DISP PIC X.                                   
041400     12  G-AREA.                                                          
041500         16  G-ROW           OCCURS 8 TIMES                               
041600                             INDEXED BY G-X1.                             
041700             20  G-COL       OCCURS 8 TIMES                               
041800                             INDEXED BY G-X2.                             
041900                 24  G-STATUS    PIC X.                                   
042000***                      VALUES... 0=UNKNOWN, 1=KNOWN                     
042100                 24  G-TOTAL     PIC 999.                                 
042200                 24  FILLER  REDEFINES G-TOTAL.                           
042300                     28  G-KLING PIC 9.                                   
042400                     28  G-SBASE PIC 9.                                   
042500                     28  G-STARS PIC 9.                                   
042600     12  DAM-REPAIR-LIST.                                                 
042700         16  DAM-NAV             PIC S99V9           COMP-3.              
042800         16  DAM-SRS             PIC S99V9           COMP-3.              
042900         16  DAM-LRS             PIC S99V9           COMP-3.              
043000         16  DAM-PHA             PIC S99V9           COMP-3.              
043100         16  DAM-TOR             PIC S99V9           COMP-3.              
043200         16  DAM-DAM             PIC S99V9           COMP-3.              
043300         16  DAM-SHE             PIC S99V9           COMP-3.              
043400         16  DAM-COM             PIC S99V9           COMP-3.              
043500     12  DAM-REPAIR-TABLE  REDEFINES DAM-REPAIR-LIST.                     
043600         16  DAM-REPAIR      OCCURS 8 TIMES                               
043700                                 PIC S99V9           COMP-3.              
043800** ***************************************************************        
043900*                                                                *        
044000*         WORKING-STORAGE FIELDS FOR ISPF SERVICE CALLS          *        
044100*                                                                *        
044200******************************************************************        
044300 77  ISPF-ALT-NAME           PICTURE X(8)       VALUE SPACES.             
044400 77  ISPF-BUFFER             PICTURE X(80)      VALUE SPACES.             
044500*    ISPF-BUFFER FIELD LENGTH CAN BE INCREASED IF NECESSARY.              
044600 77  ISPF-BUF-LENGTH         PICTURE 9(6)  COMP VALUE ZEROES.             
044700 77  ISPF-CRP-NAME           PICTURE X(8)       VALUE SPACES.             
044800 77  ISPF-DATA               PICTURE X(80)      VALUE SPACES.             
044900*    ISPF-DATA FIELD LENGTH CAN BE INCREASED IF NECESSARY.                
045000 77  ISPF-DSNAME             PICTURE X(56)      VALUE SPACES.             
045100 77  ISPF-FIELD-NAME         PICTURE X(8)       VALUE SPACES.             
045200 77  ISPF-FILEID             PICTURE X(22)      VALUE SPACES.             
045300 77  ISPF-KEY-NAME           PICTURE X(8)       VALUE SPACES.             
045400 77  ISPF-KEYNUM-NAME        PICTURE X(8)       VALUE SPACES.             
045500 77  ISPF-LENGTH             PICTURE 9(6)  COMP VALUE ZEROES.             
045600 77  ISPF-LINE-NUMBER        PICTURE 9(6)  COMP VALUE ZEROES.             
045700 77  ISPF-MEMBER-NAME        PICTURE X(8)       VALUE SPACES.             
045800 77  ISPF-MSG-ID             PICTURE X(8)       VALUE SPACES.             
045900 77  ISPF-NAMENUM-NAME       PICTURE X(8)       VALUE SPACES.             
046000 77  ISPF-NUMBER             PICTURE S9(6) COMP VALUE ZEROES.             
046100 77  ISPF-OPTION1            PICTURE X(8)       VALUE SPACES.             
046200 77  ISPF-OPTION2            PICTURE X(8)       VALUE SPACES.             
046300 77  ISPF-PANEL-NAME         PICTURE X(8)       VALUE SPACES.             
046400 77  ISPF-PERCENT            PICTURE 9(6)  COMP VALUE ZEROES.             
046500 77  ISPF-PSWD-VALUE         PICTURE X(8)       VALUE SPACES.             
046600 77  ISPF-ROW                PICTURE 9(6)  COMP VALUE ZEROES.             
046700 77  ISPF-ROWNUM-NAME        PICTURE X(8)       VALUE SPACES.             
046800 77  ISPF-SERIAL             PICTURE X(6)       VALUE SPACES.             
046900 77  ISPF-SERVICE            PICTURE X(8)       VALUE SPACES.             
047000 77  ISPF-SKEL-NAME          PICTURE X(8)       VALUE SPACES.             
047100 77  ISPF-TABLE-NAME         PICTURE X(8)       VALUE SPACES.             
047200 77  ISPF-TYPE               PICTURE X(8)       VALUE SPACES.             
047300 77  ISPF-VAR-NAME           PICTURE X(8)       VALUE SPACES.             
047400******************************************************************        
047500*    ISPF-KEY-LIST IS USED BY THE TBCREATE SERVICE TO NAME THE   *        
047600*                 VARIABLES TO BE USED AS KEYS IN THE TABLE      *        
047700*                 BE CREATED                                     *        
047800 01  ISPF-KEY-LIST.                                                       
047900     05  ISPF-KEY-COUNT      PICTURE 9(6)  COMP VALUE ZEROES.             
048000     05  FILLER              PICTURE 9(6)  COMP VALUE ZEROES.             
048100     05  ISPF-KEY       OCCURS 50 TIMES         PICTURE X(8).             
048200*        THIS ARRAY HAS BEEN DEFINED WITH A DIMENSION OF 50.     *        
048300*                 ADJUST ACCORDING TO YOUR REQUIREMENTS.         *        
048400******************************************************************        
048500*    ISPF-LENGTH-LIST IS USED BY THE VCOPY AND VREPLACE SERVICES *        
048600*                    TO SPECIFY THE LENGTH OF THE DATA FOR THE   *        
048700*                    VARIABLES(S) IN A NAME LIST. EACH LENGTH    *        
048800*                    CORRESPONDS BY POSITION TO A VARIABLE IN    *        
048900*                    THE NAME LIST, SO THERE MUST BE AS MANY     *        
049000*                    LENGTHS AS THERE ARE NAMES IN THE LIST.     *        
049100 01  ISPF-LENGTH-LIST.                                                    
049200     05  ISPF-LENGTHS   OCCURS 50 TIMES  COMP   PICTURE 9(6)     .        
049300*        THIS ARRAY HAS BEEN DEFINED WITH A DIMENSION OF 50.     *        
049400*                 ADJUST ACCORDING TO YOUR REQUIREMENTS.         *        
049500******************************************************************        
049600*    ISPF-NAME-LIST IS USED BY MANY VARIABLE AND TABLE SERVICES  *        
049700*                  TO  SPECIFY THE NAME(S) OF VARIABLES: THESE   *        
049800*                  MAY BE DIALOG VARIABLES, NON-KEY NAME         *        
049900*                  VARIABLES FOR TABLES, OR EXTENSION TABLE      *        
050000*                  VARIABLES.                                    *        
050100 01  ISPF-NAME-LIST.                                                      
050200     05  ISPF-NAME-COUNT     PICTURE 9(6)  COMP VALUE ZEROES.             
050300     05  FILLER              PICTURE 9(6)  COMP VALUE ZEROES.             
050400     05  ISPF-NAME      OCCURS 50 TIMES         PICTURE X(8).             
050500*        THIS ARRAY HAS BEEN DEFINED WITH A DIMENSION OF 50.     *        
050600*                 ADJUST ACCORDING TO YOUR REQUIREMENTS.         *        
050700******************************************************************        
050800*    ISPF-VARIABLE-LIST IS USED BY THE VDEFINE SERVICE TO        *        
050900*                      IDENTIFY THE VARIABLE(S) WHOSE STORAGE IS *        
051000*                      TO BE USED. EACH VARIABLE HERE CORRESPONDS*        
051100*                      BY POSITION TO AN ISPF VARIABLE IN THE    *        
051200*                      NAME LIST, SO THERE MUST BE AS MANY       *        
051300*                      VARIABLE NAMES AS THERE ARE ISPF NAMES.   *        
051400 01  ISPF-VARIABLE-LIST.                                                  
051500     05  ISPF-VARIABLE  OCCURS 50 TIMES         PICTURE X(8).             
051600*        THIS ARRAY HAS BEEN DEFINED WITH A DIMENSION OF 50.     *        
051700*                 ADJUST ACCORDING TO YOUR REQUIREMENTS.         *        
051800******************************************************************        
051900*       END OF WORKING-STORAGE FIELDS FOR ISPF SERVICE CALLS     *        
052000******************************************************************        
052100     EJECT                                                                
052200 PROCEDURE DIVISION.                                                      
052300     SKIP1                                                                
052400 A10-MAIN-PREPARATIONS.                                                   
052500     MOVE  'VDEFINE' TO ISPF-SERVICE.                                     
052600     MOVE 08 TO ISPF-NAME-COUNT.                                          
052700     MOVE 'ROW1    ' TO ISPF-NAME(1).                                     
052800     MOVE 'ROW2    ' TO ISPF-NAME(2).                                     
052900     MOVE 'ROW3    ' TO ISPF-NAME(3).                                     
053000     MOVE 'ROW4    ' TO ISPF-NAME(4).                                     
053100     MOVE 'ROW5    ' TO ISPF-NAME(5).                                     
053200     MOVE 'ROW6    ' TO ISPF-NAME(6).                                     
053300     MOVE 'ROW7    ' TO ISPF-NAME(7).                                     
053400     MOVE 'ROW8    ' TO ISPF-NAME(8).                                     
053500     MOVE     'CHAR' TO ISPF-OPTION1.                                     
053600     MOVE    24 TO ISPF-LENGTH.                                           
053700     MOVE 'COPY    ' TO ISPF-OPTION2.                                     
053800     CALL 'ISPLINK' USING  ISPF-SERVICE  ISPF-NAME-LIST                   
053900                           DIALOG-V1  ISPF-OPTION1                        
054000                           ISPF-LENGTH  ISPF-OPTION2.                     
054100     IF RETURN-CODE NOT = 0                                               
054200*                              8  - VARIABLE NOT FOUND                    
054300*                              16 - DATA TRUNCATION OCCURED               
054400*                              20 - SEVERE ERROR                          
054500         NEXT SENTENCE.                                                   
054600                                                                          
054700     MOVE  'VDEFINE' TO ISPF-SERVICE.                                     
054800     MOVE 23 TO ISPF-NAME-COUNT.                                          
054900     MOVE 'MLINE01 ' TO ISPF-NAME(1).                                     
055000     MOVE 'MLINE02 ' TO ISPF-NAME(2).                                     
055100     MOVE 'MLINE03 ' TO ISPF-NAME(3).                                     
055200     MOVE 'MLINE04 ' TO ISPF-NAME(4).                                     
055300     MOVE 'MLINE05 ' TO ISPF-NAME(5).                                     
055400     MOVE 'MLINE06 ' TO ISPF-NAME(6).                                     
055500     MOVE 'MLINE07 ' TO ISPF-NAME(7).                                     
055600     MOVE 'MLINE08 ' TO ISPF-NAME(8).                                     
055700     MOVE 'MLINE09 ' TO ISPF-NAME(9).                                     
055800     MOVE 'MLINE10 ' TO ISPF-NAME(10).                                    
055900     MOVE 'MLINE11 ' TO ISPF-NAME(11).                                    
056000     MOVE 'MLINE12 ' TO ISPF-NAME(12).                                    
056100     MOVE 'MLINE13 ' TO ISPF-NAME(13).                                    
056200     MOVE 'MLINE14 ' TO ISPF-NAME(14).                                    
056300     MOVE 'MLINE15 ' TO ISPF-NAME(15).                                    
056400     MOVE 'MLINE16 ' TO ISPF-NAME(16).                                    
056500     MOVE 'MLINE17 ' TO ISPF-NAME(17).                                    
056600     MOVE 'MLINE18 ' TO ISPF-NAME(18).                                    
056700     MOVE 'MLINE19 ' TO ISPF-NAME(19).                                    
056800     MOVE 'MLINE20 ' TO ISPF-NAME(20).                                    
056900     MOVE 'MLINE21 ' TO ISPF-NAME(21).                                    
057000     MOVE 'MLINE22 ' TO ISPF-NAME(22).                                    
057100     MOVE 'MLINE23 ' TO ISPF-NAME(23).                                    
057200     MOVE     'CHAR' TO ISPF-OPTION1.                                     
057300     MOVE    50 TO ISPF-LENGTH.                                           
057400     MOVE 'COPY    ' TO ISPF-OPTION2.                                     
057500     CALL 'ISPLINK' USING  ISPF-SERVICE  ISPF-NAME-LIST                   
057600                           DIALOG-V2  ISPF-OPTION1                        
057700                           ISPF-LENGTH  ISPF-OPTION2.                     
057800     IF RETURN-CODE NOT = 0                                               
057900*                              8  - VARIABLE NOT FOUND                    
058000*                              16 - DATA TRUNCATION OCCURED               
058100*                              20 - SEVERE ERROR                          
058200         NEXT SENTENCE.                                                   
058300     MOVE  'VDEFINE' TO ISPF-SERVICE.                                     
058400     MOVE 02 TO ISPF-NAME-COUNT.                                          
058500     MOVE 'STRDTE  ' TO ISPF-NAME(1).                                     
058600     MOVE 'COND    ' TO ISPF-NAME(2).                                     
058700     MOVE     'CHAR' TO ISPF-OPTION1.                                     
058800     MOVE     6 TO ISPF-LENGTH.                                           
058900     MOVE 'COPY    ' TO ISPF-OPTION2.                                     
059000     CALL 'ISPLINK' USING  ISPF-SERVICE  ISPF-NAME-LIST                   
059100                           DIALOG-V3  ISPF-OPTION1                        
059200                           ISPF-LENGTH  ISPF-OPTION2.                     
059300     IF RETURN-CODE NOT = 0                                               
059400*                              8  - VARIABLE NOT FOUND                    
059500*                              16 - DATA TRUNCATION OCCURED               
059600*                              20 - SEVERE ERROR                          
059700         NEXT SENTENCE.                                                   
059800     MOVE  'VDEFINE' TO ISPF-SERVICE.                                     
059900     MOVE 03 TO ISPF-NAME-COUNT.                                          
060000     MOVE 'QDR     ' TO ISPF-NAME(1).                                     
060100     MOVE 'SCT     ' TO ISPF-NAME(2).                                     
060200     MOVE 'COM     ' TO ISPF-NAME(3).                                     
060300     MOVE     'CHAR' TO ISPF-OPTION1.                                     
060400     MOVE     3 TO ISPF-LENGTH.                                           
060500     MOVE 'COPY    ' TO ISPF-OPTION2.                                     
060600     CALL 'ISPLINK' USING  ISPF-SERVICE  ISPF-NAME-LIST                   
060700                           DIALOG-V4 ISPF-OPTION1                         
060800                           ISPF-LENGTH  ISPF-OPTION2.                     
060900     IF RETURN-CODE NOT = 0                                               
061000*                              8  - VARIABLE NOT FOUND                    
061100*                              16 - DATA TRUNCATION OCCURED               
061200*                              20 - SEVERE ERROR                          
061300         NEXT SENTENCE.                                                   
061400     MOVE  'VDEFINE' TO ISPF-SERVICE.                                     
061500     MOVE 04 TO ISPF-NAME-COUNT.                                          
061600     MOVE 'FLDA    ' TO ISPF-NAME(1).                                     
061700     MOVE 'FLDB    ' TO ISPF-NAME(2).                                     
061800     MOVE 'ENER    ' TO ISPF-NAME(3).                                     
061900     MOVE 'SHLD    ' TO ISPF-NAME(4).                                     
062000     MOVE     'CHAR' TO ISPF-OPTION1.                                     
062100     MOVE     4 TO ISPF-LENGTH.                                           
062200     MOVE 'COPY    ' TO ISPF-OPTION2.                                     
062300     CALL 'ISPLINK' USING  ISPF-SERVICE  ISPF-NAME-LIST                   
062400                           DIALOG-V5  ISPF-OPTION1                        
062500                           ISPF-LENGTH  ISPF-OPTION2.                     
062600     IF RETURN-CODE NOT = 0                                               
062700*                              8  - VARIABLE NOT FOUND                    
062800*                              16 - DATA TRUNCATION OCCURED               
062900*                              20 - SEVERE ERROR                          
063000         NEXT SENTENCE.                                                   
063100     MOVE  'VDEFINE' TO ISPF-SERVICE.                                     
063200     MOVE 01 TO ISPF-NAME-COUNT.                                          
063300     MOVE 'MESSAGEO' TO ISPF-NAME(1).                                     
063400     MOVE     'CHAR' TO ISPF-OPTION1.                                     
063500     MOVE    53 TO ISPF-LENGTH.                                           
063600     MOVE 'COPY    ' TO ISPF-OPTION2.                                     
063700     CALL 'ISPLINK' USING  ISPF-SERVICE  ISPF-NAME-LIST                   
063800                           DIALOG-V6 ISPF-OPTION1                         
063900                           ISPF-LENGTH  ISPF-OPTION2.                     
064000     IF RETURN-CODE NOT = 0                                               
064100*                              8  - VARIABLE NOT FOUND                    
064200*                              16 - DATA TRUNCATION OCCURED               
064300*                              20 - SEVERE ERROR                          
064400         NEXT SENTENCE.                                                   
064500     MOVE  'VDEFINE' TO ISPF-SERVICE.                                     
064600     MOVE 01 TO ISPF-NAME-COUNT.                                          
064700     MOVE 'QUADNAM ' TO ISPF-NAME(1).                                     
064800     MOVE     'CHAR' TO ISPF-OPTION1.                                     
064900     MOVE    16 TO ISPF-LENGTH.                                           
065000     MOVE 'COPY    ' TO ISPF-OPTION2.                                     
065100     CALL 'ISPLINK' USING  ISPF-SERVICE  ISPF-NAME-LIST                   
065200                           DIALOG-V7  ISPF-OPTION1                        
065300                           ISPF-LENGTH  ISPF-OPTION2.                     
065400     IF RETURN-CODE NOT = 0                                               
065500*                              8  - VARIABLE NOT FOUND                    
065600*                              16 - DATA TRUNCATION OCCURED               
065700*                              20 - SEVERE ERROR                          
065800         NEXT SENTENCE.                                                   
065900     MOVE  'VDEFINE' TO ISPF-SERVICE.                                     
066000     MOVE 01 TO ISPF-NAME-COUNT.                                          
066100     MOVE 'TP      ' TO ISPF-NAME(1).                                     
066200     MOVE     'CHAR' TO ISPF-OPTION1.                                     
066300     MOVE     2 TO ISPF-LENGTH.                                           
066400     MOVE 'COPY    ' TO ISPF-OPTION2.                                     
066500     CALL 'ISPLINK' USING  ISPF-SERVICE  ISPF-NAME-LIST                   
066600                           M1-TORPEDOS  ISPF-OPTION1                      
066700                           ISPF-LENGTH  ISPF-OPTION2.                     
066800     IF RETURN-CODE NOT = 0                                               
066900*                              8  - VARIABLE NOT FOUND                    
067000*                              16 - DATA TRUNCATION OCCURED               
067100*                              20 - SEVERE ERROR                          
067200         NEXT SENTENCE.                                                   
067300     MOVE  'VDEFINE' TO ISPF-SERVICE.                                     
067400     MOVE 01 TO ISPF-NAME-COUNT.                                          
067500     MOVE 'ZTIME   ' TO ISPF-NAME(1).                                     
067600     MOVE     'CHAR' TO ISPF-OPTION1.                                     
067700     MOVE     5 TO ISPF-LENGTH.                                           
067800     MOVE '        ' TO ISPF-OPTION2.                                     
067900     CALL 'ISPLINK' USING  ISPF-SERVICE  ISPF-NAME-LIST                   
068000                           TIME-VAR  ISPF-OPTION1                         
068100                           ISPF-LENGTH  ISPF-OPTION2.                     
068200     IF RETURN-CODE NOT = 0                                               
068300*                              8  - VARIABLE NOT FOUND                    
068400*                              16 - DATA TRUNCATION OCCURED               
068500*                              20 - SEVERE ERROR                          
068600         NEXT SENTENCE.                                                   
068700     PERFORM W10-GAME-BEGINS THRU W10-EXIT.                               
068800     PERFORM C70-SRS-COMMAND THRU C70-EXIT.                               
068900     GO TO A50-NEW-SCREENS.                                               
069000 A30-NEXT-COMMAND.                                                        
069100     MOVE +0                     TO  WRAPUP-CODE.                         
069200     MOVE +0                     TO  L-INDX.                              
069300     SET MLINE-X1 TO L-INDX.                                              
069400     IF CMDI = L-NAV                                                      
069500         PERFORM F00-NAV-COMMAND THRU F00-EXIT                            
069600         GO TO A40-ENERGY-CHECK.                                          
069700     IF CMDI = L-LRS                                                      
069800         PERFORM G00-LRS-COMMAND THRU G00-EXIT                            
069900         GO TO A40-ENERGY-CHECK.                                          
070000     IF CMDI = L-PHA                                                      
070100         PERFORM H00-PHA-COMMAND THRU H00-EXIT                            
070200         GO TO A40-ENERGY-CHECK.                                          
070300     IF CMDI = L-TOR                                                      
070400         PERFORM J00-TOR-COMMAND THRU J00-EXIT                            
070500         GO TO A40-ENERGY-CHECK.                                          
070600     IF CMDI = L-SHE                                                      
070700         PERFORM L00-SHE-COMMAND THRU L00-EXIT                            
070800         GO TO A40-ENERGY-CHECK.                                          
070900     IF CMDI = L-DAM                                                      
071000         PERFORM M00-DAM-COMMAND THRU M00-EXIT                            
071100         GO TO A40-ENERGY-CHECK.                                          
071200     IF CMDI = L-COM                                                      
071300         PERFORM K00-COM-COMMAND THRU K00-EXIT                            
071400         GO TO A40-ENERGY-CHECK.                                          
071500     IF CMDI = L-SAV                                                      
071600         PERFORM B00-SAV-COMMAND THRU B00-EXIT                            
071700         GO TO A40-ENERGY-CHECK.                                          
071800     IF CMDI = L-RES                                                      
071900         PERFORM B10-RES-COMMAND THRU B10-EXIT                            
072000         GO TO A40-ENERGY-CHECK.                                          
072100     IF CMDI = L-END                                                      
072200         MOVE +2 TO WRAPUP-CODE                                           
072300         MOVE 'YOUR LACK OF CONFIDENCE HAS DOOMED THE GALAXY.'            
072400                                 TO  MSG-LINE                             
072500         PERFORM C50-NEXT-MSG THRU C50-EXIT                               
072600         GO TO A40-ENERGY-CHECK.                                          
072700     PERFORM V70-VALID-COMMANDS THRU V70-EXIT.                            
072800 A40-ENERGY-CHECK.                                                        
072900     IF WRAPUP-CODE > +0                                                  
073000         GO TO A50-NEW-SCREENS.                                           
073100     IF ENERGY > +10                                                      
073200         GO TO A50-NEW-SCREENS.                                           
073300     IF (SHIELD + ENERGY) > +10                                           
073400         IF DAM-SHE NOT < +0                                              
073500             GO  TO A50-NEW-SCREENS.                                      
073600     IF ENERGY < +0                                                       
073700         MOVE +0                 TO  ENERGY.                              
073800     MOVE +1 TO WRAPUP-CODE.                                              
073900 A50-NEW-SCREENS.                                                         
074000     PERFORM C70-SRS-COMMAND THRU C70-EXIT.                               
074100     IF WRAPUP-CODE > +0                                                  
074200         PERFORM Z80-GAME-OVER THRU Z80-EXIT                              
074300         GO TO A50-NORESP.                                                
074400     GO TO A60-BMS-OUT-MAP1.                                              
074500 A50-NORESP.                                                              
074600 A60-BMS-OUT-MAP1.                                                        
074700 A60-NORESP.                                                              
074800 A70-BMS-OUT-MAP2.                                                        
074900     MOVE  'DISPLAY' TO ISPF-SERVICE.                                     
075000     MOVE 'TRKPMAIN' TO ISPF-PANEL-NAME.                                  
075100     MOVE '        ' TO ISPF-MSG-ID.                                      
075200     MOVE '        ' TO ISPF-FIELD-NAME.                                  
075300     CALL 'ISPLINK' USING  ISPF-SERVICE  ISPF-PANEL-NAME                  
075400                           ISPF-MSG-ID  ISPF-FIELD-NAME.                  
075500*    IF RETURN-CODE NOT = 0                                               
075600*                              8  - END OR RETURN COMMAND ENTERED         
075700*                              12 - PANEL, MESSAGE, OR CURSOR             
075800*                                   FIELD COULD NOT BE FOUND              
075900*                              16 - DATA TRUNCATION OR TRANSLATION        
076000*                                   ERROR                                 
076100*                              20 - SEVERE ERROR                          
076200**********  ERROR                                                         
076300*        MOVE 'A70-' TO ABEND-CODE,                                       
076400*        GO TO Z99-ABEND.                                                 
076500 A70-NORESP.                                                              
076600     IF WRAPUP-CODE > +0                                                  
076700         GO TO A90-FINAL-RETURN.                                          
076800     GO TO A30-NEXT-COMMAND.                                              
076900 A90-FINAL-RETURN.                                                        
077000     STOP RUN.                                                            
077100     EJECT                                                                
077200 B00-SAV-COMMAND.                                                         
077300     OPEN OUTPUT GAME-FILE.                                               
077400     WRITE GAME-RECORD FROM TEMPSTRG.                                     
077500     CLOSE GAME-FILE.                                                     
077600 B00-EXIT.                                                                
077700     EXIT.                                                                
077800                                                                          
077900 B10-RES-COMMAND.                                                         
078000     OPEN INPUT GAME-FILE.                                                
078100     READ GAME-FILE INTO TEMPSTRG.                                        
078200     CLOSE GAME-FILE.                                                     
078300 B10-EXIT.                                                                
078400     EXIT.                                                                
078500                                                                          
078600 C30-EDIT-INPUT-QTY.                                                      
078700     MOVE '   '              TO  C30-MSG.                                 
078800     MOVE 'N'                TO  C30-NON-BLANK-FLAG                       
078900                                 C30-NEG-FLAG                             
079000                                 C30-DECIMAL-FLAG.                        
079100     MOVE +0                 TO  C30-DIGIT-CTR                            
079200                                 C30-DECIMAL-CTR                          
079300                                 C30-OUT-QTY.                             
079400     MOVE -1                 TO  C30-ADJ-QTY.                             
079500     MOVE +5                     TO  L-INDX.                              
079600     SET C30-IX1 TO L-INDX.                                               
079700     MOVE +4                     TO  L-INDX.                              
079800     SET C30-OX1 TO L-INDX.                                               
079900 C30-EDIT-QTY-LOOP.                                                       
080000     SET C30-IX1 DOWN BY +1.                                              
080100     IF C30-IX1 < +1                                                      
080200         GO TO C30-ADJUST-OUT-QTY.                                        
080300     IF C30-I-CHAR (C30-IX1) = ' '                                        
080400         IF C30-NON-BLANK-FLAG = 'N'                                      
080500             GO TO C30-EDIT-QTY-LOOP                                      
080600         ELSE                                                             
080700             GO TO C30-ERR.                                               
080800     IF C30-I-CHAR (C30-IX1) = '-'                                        
080900         IF C30-NON-BLANK-FLAG = 'N'                                      
081000             MOVE 'Y' TO C30-NON-BLANK-FLAG                               
081100                         C30-NEG-FLAG                                     
081200             GO TO C30-EDIT-QTY-LOOP                                      
081300         ELSE                                                             
081400             GO TO C30-ERR.                                               
081500     IF C30-I-CHAR (C30-IX1) = '.'                                        
081600         IF C30-DECIMAL-FLAG = 'N'                                        
081700             MOVE 'Y' TO C30-NON-BLANK-FLAG                               
081800                         C30-DECIMAL-FLAG                                 
081900             MOVE C30-DIGIT-CTR TO C30-DECIMAL-CTR                        
082000             GO TO C30-EDIT-QTY-LOOP                                      
082100         ELSE                                                             
082200             GO TO C30-ERR.                                               
082300     IF C30-I-CHAR (C30-IX1) IS NOT NUMERIC                               
082400         GO TO C30-ERR.                                                   
082500     MOVE 'Y' TO C30-NON-BLANK-FLAG.                                      
082600     ADD +1 TO C30-DIGIT-CTR.                                             
082700     MOVE C30-I-CHAR (C30-IX1)  TO  C30-O-CHAR (C30-OX1).                 
082800     SET C30-OX1 DOWN BY +1.                                              
082900     GO TO C30-EDIT-QTY-LOOP.                                             
083000 C30-ADJUST-OUT-QTY.                                                      
083100     IF C30-NEG-FLAG = 'Y'                                                
083200         MULTIPLY -1 BY C30-OUT-QTY.                                      
083300     COMPUTE C30-ADJ-QTY  ROUNDED =                                       
083400         C30-OUT-QTY / (10 ** C30-DECIMAL-CTR).                           
083500     GO TO C30-EXIT.                                                      
083600 C30-ERR.                                                                 
083700     MOVE 'ERR' TO C30-MSG.                                               
083800 C30-EXIT.  EXIT.                                                         
083900     SKIP1                                                                
084000 C40-GET-RND.                                                             
084100     COMPUTE RND-WORK = RND-WORK * RND-MULT.                              
084200 C40-EXIT.  EXIT.                                                         
084300     SKIP1                                                                
084400 C50-NEXT-MSG.                                                            
084500     IF MLINE-X1 < +23                                                    
084600         SET MLINE-X1 UP BY +1                                            
084700     ELSE                                                                 
084800         MOVE '*** OVERFLOW *** OVERFLOW *** OVERFLOW ***'                
084900                                 TO  MSG-LINE.                            
085000***  I HAVE ONLY SEEN THE ABOVE 'OVERFLOW' OCCUR ONCE.                    
085100***  IT SHOULD NOT PRESENT ANY REAL INCONVIENCE.                          
085200     MOVE MSG-LINE               TO  MLINE (MLINE-X1).                    
085300 C50-EXIT.  EXIT.                                                         
085400     SKIP1                                                                
085500 C60-BLANK-MSG.                                                           
085600     MOVE SPACES                 TO  MSG-LINE.                            
085700     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
085800 C60-EXIT.  EXIT.                                                         
085900     SKIP1                                                                
086000 C70-SRS-COMMAND.                                                         
086100     MOVE ' GREEN'               TO  CONDITION.                           
086200     IF ENERGY < (0.1 * INIT-ENERGY-EPRISE)                               
086300         MOVE 'YELLOW'           TO  CONDITION.                           
086400     IF CURR-KLING > +0                                                   
086500         MOVE ' *RED*'           TO  CONDITION.                           
086600     IF DOCKING-FLAG = 'Y'                                                
086700         MOVE 'DOCKED'           TO  CONDITION.                           
086800     MOVE CURR-QUAD              TO  M1-QUAD-NAME.                        
086900     MOVE STARDATE               TO  M1-STARDATE.                         
087000     MOVE CONDITION              TO  M1-CONDITION.                        
087100     MOVE EPRISE-GLXY-ROW                     TO  M1-GLXY-ROW.            
087200     MOVE L-COLON                TO  M1-QUAD-SEP.                         
087300     MOVE EPRISE-GLXY-COL                     TO  M1-GLXY-COL.            
087400     MOVE EPRISE-ROW             TO  M1-SECT-ROW.                         
087500     MOVE L-COLON                TO  M1-SECT-SEP.                         
087600     MOVE EPRISE-COL             TO  M1-SECT-COL.                         
087700     MOVE ENERGY                 TO  M1-ENERGY.                           
087800     MOVE SHIELD                 TO  M1-SHIELD.                           
087900     MOVE TORPEDOS               TO  M1-TORPEDOS.                         
088000     IF (DAM-SRS < +0) AND (MLINE-X1 < +21)                               
088100         PERFORM C60-BLANK-MSG THRU C60-EXIT                              
088200         MOVE '*** SHORT RANGE SENSORS ARE OUT ***'                       
088300                                 TO  MSG-LINE                             
088400         PERFORM C50-NEXT-MSG THRU C50-EXIT                               
088500         PERFORM C60-BLANK-MSG THRU C60-EXIT                              
088600         GO TO C70-EXIT.                                                  
088700     MOVE +1 TO L-INDX.                                                   
088800     SET SECTOR-X1 TO L-INDX.                                             
088900     MOVE +0 TO L-INDX.                                                   
089000     SET SECTOR-X2 TO L-INDX.                                             
089100 C70-SRS-LOOP.                                                            
089200     SET SECTOR-X2 UP BY +1.                                              
089300     IF SECTOR-X2 > +8                                                    
089400         IF SECTOR-X1 < +8                                                
089500             SET SECTOR-X1 UP BY +1                                       
089600             MOVE +0 TO L-INDX                                            
089700             SET SECTOR-X2 TO L-INDX                                      
089800         ELSE                                                             
089900             GO TO C70-EXIT.                                              
090000     SET M1-X1 TO SECTOR-X1.                                              
090100     SET M1-X2 TO SECTOR-X2.                                              
090200     MOVE SECTOR-DISP (SECTOR-X1, SECTOR-X2)                              
090300                                 TO  M1-DISP (M1-X1, M1-X2).              
090400     GO TO C70-SRS-LOOP.                                                  
090500 C70-EXIT.  EXIT.                                                         
090600     EJECT                                                                
090700 F00-NAV-COMMAND.                                                         
090800     MOVE 'A'                    TO  NAV-EDIT-FLAG.                       
090900     IF FLDA = SPACES                                                     
091000         PERFORM T10-NAV-PROMPT THRU T10-EXIT                             
091100         GO TO F00-EXIT.                                                  
091200     MOVE FLDA                   TO  C30-IN-QTY.                          
091300     PERFORM C30-EDIT-INPUT-QTY THRU C30-EXIT.                            
091400     IF (C30-ADJ-QTY < +0) OR (C30-ADJ-QTY > +360)                        
091500         PERFORM T10-NAV-PROMPT THRU T10-EXIT                             
091600         GO TO F00-EXIT.                                                  
091700     COMPUTE COURSE ROUNDED = 1 + (C30-ADJ-QTY / 45).                     
091800     MOVE 'B'                    TO  NAV-EDIT-FLAG.                       
091900     IF FLDB  = SPACES                                                    
092000         PERFORM T10-NAV-PROMPT THRU T10-EXIT                             
092100         GO TO F00-EXIT.                                                  
092200     MOVE FLDB                   TO  C30-IN-QTY.                          
092300     PERFORM C30-EDIT-INPUT-QTY THRU C30-EXIT.                            
092400     IF (C30-ADJ-QTY < +0.01) OR (C30-ADJ-QTY > +8)                       
092500         PERFORM T10-NAV-PROMPT THRU T10-EXIT                             
092600         GO TO F00-EXIT.                                                  
092700*******  INSTEAD OF RESTRICTING THE WARP FACTOR DURING ENGINE             
092800*******  DAMAGE, SOME RANDOM DEVIATION CAN BE EXPECTED IN                 
092900*******  BOTH THE WARP FACTOR AND COURSE WHILE DAMAGED.                   
093000**** IF (C30-ADJ-QTY > 0.2) AND (DAM-NAV < +0)                            
093100****     PERFORM T10-NAV-PROMPT THRU T10-EXIT                             
093200****     GO TO F00-EXIT.                                                  
093300     MOVE C30-ADJ-QTY            TO  WARP-FACTOR.                         
093400     COMPUTE SECTORS-MOVE-TOTL ROUNDED = WARP-FACTOR * 8.                 
093500     COMPUTE WARP-ENERGY ROUNDED = (WARP-FACTOR * 8) + 10.                
093600     IF WARP-ENERGY NOT > ENERGY                                          
093700         GO TO F00-OKAY-TO-PROCEED.                                       
093800     MOVE 'ENGINEERING REPORTS INSUFFICIENT ENERGY AVAILABLE'             
093900                                 TO  MSG-LINE.                            
094000     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
094100     MOVE '   FOR MANEUVERING AT WARP      !'                             
094200                                 TO  MSG-LINE.                            
094300     MOVE FLDB                   TO  MSG-LINE28-31.                       
094400     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
094500     IF (WARP-ENERGY > (ENERGY + SHIELD)) OR (DAM-SHE < +0)               
094600         GO TO F00-EXIT.                                                  
094700     IF ENERGY > (0.5 * WARP-ENERGY)                                      
094800         GO TO F00-OKAY-TO-PROCEED.                                       
094900     MOVE 'DEFLECTOR CONTROL ROOM ACKNOWLEDGES       UNITS'               
095000                                 TO  MSG-LINE.                            
095100     MOVE SHIELD TO TEMP-ZZZ9.                                            
095200     MOVE TEMP-ZZZ9              TO  MSG-LINE37-40.                       
095300     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
095400     MOVE '   OF ENERGY PRESENTLY DEPLOYED TO SHIELDS.'                   
095500                                 TO  MSG-LINE.                            
095600     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
095700     MOVE 'ADJUSTING THE SHIELDS TO ZZZ9 UNITS WILL MAKE YOUR'            
095800                                 TO  MSG-LINE.                            
095900     COMPUTE TEMP-ZZZ9 = (SHIELD + ENERGY) - WARP-ENERGY.                 
096000     MOVE TEMP-ZZZ9              TO  MSG-LINE26-29.                       
096100     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
096200     MOVE '   WARP REQUEST POSSIBLE, SIR!!'                               
096300                                 TO  MSG-LINE.                            
096400     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
096500     GO TO F00-EXIT.                                                      
096600 F00-OKAY-TO-PROCEED.                                                     
096700     PERFORM Q10-KLINGONS-FIRE THRU Q10-EXIT.                             
096800     IF WRAPUP-CODE > +0                                                  
096900         GO TO F00-EXIT.                                                  
097000     PERFORM Q50-MOVE-KLINGONS THRU Q50-EXIT                              
097100         VARYING KLING-X1 FROM 1 BY 1                                     
097200         UNTIL KLING-X1 > MAX-KLING.                                      
097300     IF DAM-NAV < +0                                                      
097400         PERFORM T60-NAV-DAMAGED THRU T60-EXIT.                           
097500     IF WARP-FACTOR < +1                                                  
097600         COMPUTE STARDATES-USED ROUNDED = WARP-FACTOR                     
097700     ELSE                                                                 
097800         MOVE 1 TO STARDATES-USED.                                        
097900     PERFORM F10-MOVE-EPRISE THRU F10-EXIT.                               
098000     MOVE 'N' TO DAM-HDG-FLAG.                                            
098100     PERFORM F20-NORMAL-DAMAGE-REPAIRS THRU F20-EXIT                      
098200         VARYING DAM-SUB FROM +1 BY +1                                    
098300         UNTIL DAM-SUB > +8.                                              
098400     PERFORM C40-GET-RND THRU C40-EXIT.                                   
098500     IF RND < +0.08                                                       
098600         PERFORM R30-RANDOM-DAMAGE THRU R30-EXIT.                         
098700     IF RND > +0.85                                                       
098800         PERFORM R50-RANDOM-REPAIR THRU R50-EXIT.                         
098900     PERFORM F40-IS-EPRISE-DOCKED THRU F40-EXIT.                          
099000     IF DOCKING-FLAG = 'Y'                                                
099100         PERFORM V10-OPTIONAL-REPAIRS THRU V10-EXIT.                      
099200 F00-EXIT.  EXIT.                                                         
099300     SKIP1                                                                
099400 F10-MOVE-EPRISE.                                                         
099500     ADD STARDATES-USED       TO  STARDATE.                               
099600     SET SECTOR-X1 TO EPRISE-ROW.                                         
099700     SET SECTOR-X2 TO EPRISE-COL.                                         
099800     MOVE L-NULL TO SECTOR-DISP (SECTOR-X1, SECTOR-X2).                   
099900     IF COURSE NOT < +9                                                   
100000         SUBTRACT +8 FROM COURSE.                                         
100100     MOVE COURSE TO TEMP-999.                                             
100200     SET C-X1 TO TEMP-999.                                                
100300     COMPUTE STEP-VERT = C-VERT (C-X1)                                    
100400           + ((C-VERT (C-X1 + 1) - C-VERT (C-X1))                         
100500              * (COURSE - TEMP-999)).                                     
100600     COMPUTE STEP-HORZ = C-HORZ (C-X1)                                    
100700           + ((C-HORZ (C-X1 + 1) - C-HORZ (C-X1))                         
100800              * (COURSE - TEMP-999)).                                     
100900     MOVE EPRISE-VERT TO WORK-VERT.                                       
101000     MOVE EPRISE-HORZ TO WORK-HORZ.                                       
101100     MOVE EPRISE-GLXY-ROW TO PREV-GLXY-ROW.                               
101200     MOVE EPRISE-GLXY-COL TO PREV-GLXY-COL.                               
101300     MOVE +0                     TO  SECTORS-MOVE-CURR.                   
101400     MOVE ' ' TO EPRISE-MOVE-FLAG.                                        
101500     PERFORM F15-NUDGE-EPRISE THRU F15-EXIT                               
101600         UNTIL EPRISE-MOVE-FLAG NOT = ' '.                                
101700     IF EPRISE-MOVE-FLAG = '2'                                            
101800         PERFORM S10-EPRISE-TO-NEW-QUAD THRU S10-EXIT.                    
101900     SET SECTOR-X1 TO EPRISE-ROW.                                         
102000     SET SECTOR-X2 TO EPRISE-COL.                                         
102100     MOVE L-EPRISE TO SECTOR-DISP (SECTOR-X1, SECTOR-X2).                 
102200     PERFORM F30-ADJ-ENERGY-FOR-MOVE THRU F30-EXIT.                       
102300     IF STARDATE > ENDDATE                                                
102400         MOVE +4 TO WRAPUP-CODE.                                          
102500 F10-EXIT. EXIT.                                                          
102600     SKIP1                                                                
102700 F15-NUDGE-EPRISE.                                                        
102800     ADD +1 TO SECTORS-MOVE-CURR.                                         
102900     IF SECTORS-MOVE-CURR > SECTORS-MOVE-TOTL                             
103000         MOVE '1' TO EPRISE-MOVE-FLAG                                     
103100         GO TO F15-EXIT.                                                  
103200     ADD STEP-VERT TO EPRISE-VERT.                                        
103300     ADD STEP-HORZ TO EPRISE-HORZ.                                        
103400     IF (EPRISE-VERT < +1) OR (EPRISE-VERT NOT < +9)                      
103500     OR (EPRISE-HORZ < +1) OR (EPRISE-HORZ NOT < +9)                      
103600         MOVE '2' TO EPRISE-MOVE-FLAG                                     
103700         GO TO F15-EXIT.                                                  
103800     SET SECTOR-X1 TO EPRISE-ROW.                                         
103900     SET SECTOR-X2 TO EPRISE-COL.                                         
104000     IF SECTOR-DISP (SECTOR-X1, SECTOR-X2) = L-NULL                       
104100         GO TO F15-EXIT.                                                  
104200     MOVE SECTOR-DISP (SECTOR-X1, SECTOR-X2) TO OBSTACLE.                 
104300     SUBTRACT STEP-VERT FROM EPRISE-VERT.                                 
104400     SUBTRACT STEP-HORZ FROM EPRISE-HORZ.                                 
104500     MOVE '1' TO EPRISE-MOVE-FLAG.                                        
104600     MOVE 'WARP ENGINES SHUT DOWN AT SECTOR  :'                           
104700                                 TO  MSG-LINE.                            
104800     MOVE EPRISE-ROW                     TO  MSG-LINE34.                  
104900     MOVE EPRISE-COL                     TO  MSG-LINE36.                  
105000     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
105100     IF OBSTACLE = L-STARS                                                
105200         MOVE '  DUE TO BAD NAVIGATION.'                                  
105300                                 TO  MSG-LINE.                            
105400     IF OBSTACLE = L-KLING                                                
105500         MOVE '  DUE TO A DARING KLINGON MANEUVER.'                       
105600                                 TO  MSG-LINE.                            
105700     IF OBSTACLE = L-SBASE                                                
105800         MOVE '  TO AVOID RAMMING THE STARBASE.'                          
105900                                 TO  MSG-LINE.                            
106000     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
106100 F15-EXIT.  EXIT.                                                         
106200     SKIP1                                                                
106300 F20-NORMAL-DAMAGE-REPAIRS.                                               
106400     IF DAM-REPAIR (DAM-SUB) NOT < +0                                     
106500         GO TO F20-EXIT.                                                  
106600     ADD STARDATES-USED TO DAM-REPAIR (DAM-SUB).                          
106700     IF DAM-REPAIR (DAM-SUB) < +0                                         
106800         GO TO F20-EXIT.                                                  
106900     IF DAM-REPAIR (DAM-SUB) > +0                                         
107000         MOVE +0                 TO  DAM-REPAIR (DAM-SUB).                
107100     IF DAM-HDG-FLAG = 'N'                                                
107200         MOVE 'Y' TO DAM-HDG-FLAG                                         
107300         MOVE 'DAMAGE CONTROL REPORT:'                                    
107400                                 TO  MSG-LINE                             
107500         PERFORM C50-NEXT-MSG THRU C50-EXIT.                              
107600     MOVE '                       REPAIR COMPLETED'                       
107700                                 TO  MSG-LINE.                            
107800     MOVE DAM-NAME (DAM-SUB)    TO  MSG-LINE11-22.                        
107900     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
108000 F20-EXIT.  EXIT.                                                         
108100     SKIP1                                                                
108200 F30-ADJ-ENERGY-FOR-MOVE.                                                 
108300     COMPUTE ENERGY = ENERGY - WARP-ENERGY.                               
108400     IF ENERGY NOT < +0                                                   
108500         GO TO F30-EXIT.                                                  
108600     MOVE 'SHIELD CONTROL SUPPLIED ENERGY FOR THE MANEUVER.'              
108700                                 TO  MSG-LINE.                            
108800     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
108900     ADD ENERGY TO SHIELD.                                                
109000     MOVE +0 TO ENERGY.                                                   
109100     IF SHIELD < +0                                                       
109200         MOVE +0 TO SHIELD.                                               
109300 F30-EXIT.  EXIT.                                                         
109400     SKIP1                                                                
109500 F40-IS-EPRISE-DOCKED.                                                    
109600     SET SECTOR-X1 TO EPRISE-ROW.                                         
109700     SET SECTOR-X2 TO EPRISE-COL.                                         
109800     MOVE 'N' TO DOCKING-FLAG.                                            
109900     IF SECTOR-X1 > +1                                                    
110000         SET SECTOR-X1 DOWN BY +1                                         
110100         PERFORM F45-DOCK-CHECK THRU F45-EXIT                             
110200         SET SECTOR-X1  UP  BY +1.                                        
110300     PERFORM F45-DOCK-CHECK THRU F45-EXIT.                                
110400     IF SECTOR-X1 < +8                                                    
110500         SET SECTOR-X1  UP  BY +1                                         
110600         PERFORM F45-DOCK-CHECK THRU F45-EXIT                             
110700         SET SECTOR-X1 DOWN BY +1.                                        
110800     IF (DOCKING-FLAG = 'Y')                                              
110900     AND ((ENERGY + SHIELD) < INIT-ENERGY-EPRISE)                         
111000         MOVE INIT-ENERGY-EPRISE TO  ENERGY                               
111100         MOVE INIT-TORPEDOS      TO  TORPEDOS                             
111200         MOVE +0                 TO  SHIELD                               
111300         MOVE 'SHIELDS DROPPED FOR DOCKING PURPOSES.'                     
111400                                 TO  MSG-LINE                             
111500         PERFORM C50-NEXT-MSG THRU C50-EXIT.                              
111600 F40-EXIT.  EXIT.                                                         
111700     SKIP1                                                                
111800 F45-DOCK-CHECK.                                                          
111900     IF SECTOR-X2 > +1                                                    
112000         IF SECTOR-DISP (SECTOR-X1, SECTOR-X2 - 1) = L-SBASE              
112100             MOVE 'Y' TO DOCKING-FLAG.                                    
112200     IF SECTOR-X2 < +8                                                    
112300         IF SECTOR-DISP (SECTOR-X1, SECTOR-X2 + 1) = L-SBASE              
112400             MOVE 'Y' TO DOCKING-FLAG.                                    
112500     IF SECTOR-DISP (SECTOR-X1, SECTOR-X2) = L-SBASE                      
112600         MOVE 'Y' TO DOCKING-FLAG.                                        
112700 F45-EXIT.  EXIT.                                                         
112800     EJECT                                                                
112900 G00-LRS-COMMAND.                                                         
113000     IF DAM-LRS < +0                                                      
113100         MOVE '*** LONG RANGE SENSORS ARE INOPERABLE ***'                 
113200                                 TO  MSG-LINE                             
113300         PERFORM C50-NEXT-MSG THRU C50-EXIT                               
113400         GO TO G00-EXIT.                                                  
113500     SET G-X1 TO EPRISE-GLXY-ROW.                                         
113600     SET G-X2 TO EPRISE-GLXY-COL.                                         
113700     MOVE SPACES                 TO  MSG-LINE.                            
113800     MOVE L-LRS-TOP              TO  MSG-LINE01-20.                       
113900     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
114000     SET G-X1 DOWN BY +1.                                                 
114100     PERFORM G10-LRS-PRINT THRU G10-EXIT.                                 
114200     SET G-X1  UP  BY +1.                                                 
114300     PERFORM G10-LRS-PRINT THRU G10-EXIT.                                 
114400     SET G-X1  UP  BY +1.                                                 
114500     PERFORM G10-LRS-PRINT THRU G10-EXIT.                                 
114600     SET G-X1 DOWN BY +1.                                                 
114700 G00-EXIT.  EXIT.                                                         
114800     SKIP1                                                                
114900 G10-LRS-PRINT.                                                           
115000     MOVE SPACES TO  LRS-LINE.                                            
115100     MOVE +0 TO L-INDX.                                                   
115200     SET LRS-X1 TO L-INDX.                                                
115300     MOVE ':'    TO  LRS-SEP1                                             
115400                     LRS-SEP (LRS-X1 + 1)                                 
115500                     LRS-SEP (LRS-X1 + 2)                                 
115600                     LRS-SEP (LRS-X1 + 3).                                
115700     IF (G-X1 < +1) OR (G-X1 > +8)                                        
115800         GO TO G10-CONTINUE.                                              
115900     MOVE '1'                    TO  G-STATUS (G-X1, G-X2).               
116000     MOVE G-TOTAL (G-X1, G-X2)   TO  LRS-SECT (LRS-X1 + 2).               
116100     IF G-X2 > +1                                                         
116200         MOVE '1'                TO  G-STATUS (G-X1, G-X2 - 1)            
116300         MOVE G-TOTAL (G-X1, G-X2 - 1) TO LRS-SECT (LRS-X1 + 1).          
116400     IF G-X2 < +8                                                         
116500         MOVE '1'                TO  G-STATUS (G-X1, G-X2 + 1)            
116600         MOVE G-TOTAL (G-X1, G-X2 + 1) TO LRS-SECT (LRS-X1 + 3).          
116700 G10-CONTINUE.                                                            
116800     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
116900     MOVE L-LRS-TOP              TO  MSG-LINE01-20.                       
117000     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
117100 G10-EXIT.  EXIT.                                                         
117200     EJECT                                                                
117300 H00-PHA-COMMAND.                                                         
117400     IF CURR-KLING NOT > +0                                               
117500         MOVE 'SCIENCE OFFICER SPOCK REPORTS, SENSORS SHOW'               
117600                                 TO  MSG-LINE                             
117700         PERFORM C50-NEXT-MSG THRU C50-EXIT                               
117800         MOVE '   NO ENEMY SHIPS IN THIS QUADRANT.'                       
117900                                 TO  MSG-LINE                             
118000         PERFORM C50-NEXT-MSG THRU C50-EXIT                               
118100         GO TO H00-EXIT.                                                  
118200     IF DAM-PHA < +0                                                      
118300         MOVE '*** PHASERS ARE INOPERATIVE ***'                           
118400                                 TO  MSG-LINE                             
118500         PERFORM C50-NEXT-MSG THRU C50-EXIT                               
118600         GO TO H00-EXIT.                                                  
118700     IF DOCKING-FLAG = 'Y'                                                
118800         MOVE 'PHASERS ARE DEACTIVATED WHILE DOCKED'                      
118900                                 TO  MSG-LINE                             
119000         PERFORM C50-NEXT-MSG THRU C50-EXIT                               
119100         GO TO H00-EXIT.                                                  
119200     IF DAM-COM < +0                                                      
119300         MOVE '*** COMPUTER FAILURE HAMPERS ACCURACY ***'                 
119400                                 TO  MSG-LINE                             
119500         PERFORM C50-NEXT-MSG THRU C50-EXIT.                              
119600     IF FLDA  = SPACES                                                    
119700         PERFORM T40-PHA-PROMPT THRU T40-EXIT                             
119800         GO TO H00-EXIT.                                                  
119900     MOVE FLDA                   TO  C30-IN-QTY.                          
120000     PERFORM C30-EDIT-INPUT-QTY THRU C30-EXIT.                            
120100     IF (C30-ADJ-QTY < +1) OR (C30-ADJ-QTY > ENERGY)                      
120200         PERFORM T40-PHA-PROMPT THRU T40-EXIT                             
120300         GO TO H00-EXIT.                                                  
120400     MOVE C30-ADJ-QTY            TO  PHASER-ENERGY.                       
120500     SUBTRACT PHASER-ENERGY FROM ENERGY.                                  
120600     PERFORM Q10-KLINGONS-FIRE THRU Q10-EXIT.                             
120700     IF WRAPUP-CODE > +0                                                  
120800         GO TO H00-EXIT.                                                  
120900     MOVE 'PHASERS ARE LOCKED ON TARGET'                                  
121000                                 TO  MSG-LINE.                            
121100     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
121200     PERFORM C60-BLANK-MSG THRU C60-EXIT.                                 
121300     IF DAM-COM < +0                                                      
121400         PERFORM C40-GET-RND THRU C40-EXIT                                
121500         COMPUTE PHASER-ENERGY = PHASER-ENERGY * (RND ** 0.5).            
121600     COMPUTE PHASER-ENERGY = PHASER-ENERGY / CURR-KLING.                  
121700     MOVE +0 TO L-INDX.                                                   
121800     SET KLING-X1 TO L-INDX.                                              
121900     PERFORM H10-PHASER-1-KLINGON THRU H10-EXIT                           
122000         MAX-KLING TIMES.                                                 
122100 H00-EXIT.  EXIT.                                                         
122200     SKIP1                                                                
122300 H10-PHASER-1-KLINGON.                                                    
122400     SET KLING-X1 UP BY +1.                                               
122500     IF KLING-ENERGY (KLING-X1) NOT > +0                                  
122600         GO TO H10-EXIT.                                                  
122700     PERFORM Q30-DIST-FROM-KLINGON THRU Q30-EXIT.                         
122800     PERFORM C40-GET-RND THRU C40-EXIT.                                   
122900     COMPUTE HIT ROUNDED =                                                
123000         (PHASER-ENERGY * (1 - (0.1 * RND))) / (DIST ** 0.5).             
123100     IF HIT < (0.15 * KLING-ENERGY (KLING-X1))                            
123200         MOVE 'SENSORS SHOW NO DAMAGE TO KLINGON AT SECTOR  :'            
123300                                   TO  MSG-LINE                           
123400         MOVE KLING-ROW (KLING-X1) TO  MSG-LINE45                         
123500         MOVE KLING-COL (KLING-X1) TO  MSG-LINE47                         
123600         PERFORM C50-NEXT-MSG THRU C50-EXIT                               
123700         GO TO H10-EXIT.                                                  
123800     SUBTRACT HIT FROM KLING-ENERGY (KLING-X1).                           
123900     MOVE HIT TO TEMP-ZZZ9.                                               
124000     MOVE '     UNITS HIT ON KLINGON AT SECTOR  :'                        
124100                                 TO  MSG-LINE.                            
124200     MOVE TEMP-ZZZ9              TO  MSG-LINE01-04.                       
124300     MOVE KLING-ROW (KLING-X1)   TO  MSG-LINE37.                          
124400     MOVE KLING-COL (KLING-X1)   TO  MSG-LINE39.                          
124500     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
124600     IF KLING-ENERGY (KLING-X1) > +0                                      
124700         MOVE '     (SENSORS SHOW      UNITS REMAINING)'                  
124800                                 TO  MSG-LINE                             
124900         MOVE KLING-ENERGY (KLING-X1) TO TEMP-ZZZ9                        
125000         MOVE TEMP-ZZZ9          TO  MSG-LINE20-23                        
125100         PERFORM C50-NEXT-MSG THRU C50-EXIT                               
125200         GO TO H10-EXIT.                                                  
125300     SET SECTOR-X1 TO KLING-ROW (KLING-X1).                               
125400     SET SECTOR-X2 TO KLING-COL (KLING-X1).                               
125500     PERFORM I10-KLINGON-DESTROYED THRU I10-EXIT.                         
125600 H10-EXIT.  EXIT.                                                         
125700     SKIP3                                                                
125800 I10-KLINGON-DESTROYED.                                                   
125900***                                                                       
126000***  REQUIRES THAT 'SECTOR-X1' & 'SECTOR-X2' POINT TO THE KLINGON.        
126100***                                                                       
126200     MOVE L-NULL TO SECTOR-DISP (SECTOR-X1, SECTOR-X2).                   
126300     MOVE '     **** KLINGON DESTROYED ****'                              
126400                                 TO  MSG-LINE.                            
126500     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
126600     SET G-X1 TO EPRISE-GLXY-ROW.                                         
126700     SET G-X2 TO EPRISE-GLXY-COL.                                         
126800     SUBTRACT +1 FROM CURR-KLING                                          
126900                      KLING-TOTAL                                         
127000                      G-KLING (G-X1, G-X2).                               
127100     IF KLING-TOTAL NOT > +0                                              
127200         MOVE +5 TO WRAPUP-CODE.                                          
127300     IF CMDI = L-PHA                                                      
127400         GO TO I10-FOUND-KLING-X1.                                        
127500     MOVE +0 TO L-INDX.                                                   
127600     SET KLING-X1 TO L-INDX.                                              
127700 I10-FIND-KLING-X1.                                                       
127800     SET KLING-X1 UP BY +1.                                               
127900     IF KLING-X1 > MAX-KLING                                              
128000         GO TO I10-EXIT.                                                  
128100     IF KLING-ROW (KLING-X1) NOT = SECTOR-X1                              
128200         GO TO I10-FIND-KLING-X1.                                         
128300     IF KLING-COL (KLING-X1) NOT = SECTOR-X2                              
128400         GO TO I10-FIND-KLING-X1.                                         
128500 I10-FOUND-KLING-X1.                                                      
128600     MOVE +0 TO KLING-ENERGY (KLING-X1)                                   
128700                KLING-ROW (KLING-X1)                                      
128800                KLING-COL (KLING-X1).                                     
128900 I10-EXIT.  EXIT.                                                         
129000     EJECT                                                                
129100 J00-TOR-COMMAND.                                                         
129200     IF DAM-TOR < +0                                                      
129300         MOVE '*** PHOTON TORPEDO TUBES ARE NOT OPERATIONAL ***'          
129400                                 TO  MSG-LINE                             
129500         PERFORM C50-NEXT-MSG THRU C50-EXIT                               
129600         GO TO J00-EXIT.                                                  
129700     IF DOCKING-FLAG = 'Y'                                                
129800         MOVE 'PHOTON TUBES ARE DEACTIVATED WHILE DOCKED'                 
129900                                 TO  MSG-LINE                             
130000         PERFORM C50-NEXT-MSG THRU C50-EXIT                               
130100         GO TO J00-EXIT.                                                  
130200     IF TORPEDOS NOT > +0                                                 
130300         MOVE 'ALL PHOTON TORPEDOS HAVE BEEN EXPENDED, SIR.'              
130400                                 TO  MSG-LINE                             
130500         PERFORM C50-NEXT-MSG THRU C50-EXIT                               
130600         GO TO J00-EXIT.                                                  
130700     IF ENERGY < TOR-ENERGY                                               
130800         MOVE 'AT LEAST Z9 ENERGY UNITS REQUIRED TO FIRE TORPEDOS'        
130900                                 TO  MSG-LINE                             
131000         MOVE TOR-ENERGY TO TEMP-Z9                                       
131100         MOVE TEMP-Z9            TO  MSG-LINE10-11                        
131200         PERFORM C50-NEXT-MSG THRU C50-EXIT                               
131300         GO TO J00-EXIT.                                                  
131400     IF FLDA  = SPACES                                                    
131500         PERFORM T30-TOR-PROMPT THRU T30-EXIT                             
131600         GO TO J00-EXIT.                                                  
131700     MOVE FLDA                   TO  C30-IN-QTY.                          
131800     PERFORM C30-EDIT-INPUT-QTY THRU C30-EXIT.                            
131900     IF (C30-ADJ-QTY < +0) OR (C30-ADJ-QTY > +360)                        
132000         PERFORM T30-TOR-PROMPT THRU T30-EXIT                             
132100         GO TO J00-EXIT.                                                  
132200     COMPUTE TEMP-999 ROUNDED = C30-ADJ-QTY.                              
132300     COMPUTE COURSE ROUNDED = 1 + (TEMP-999 / 45).                        
132400     IF COURSE NOT < +9                                                   
132500         SUBTRACT +8 FROM COURSE.                                         
132600     PERFORM Q10-KLINGONS-FIRE THRU Q10-EXIT.                             
132700     IF WRAPUP-CODE > +0                                                  
132800         GO TO J00-EXIT.                                                  
132900     MOVE COURSE TO TEMP-999.                                             
133000     SET C-X1 TO TEMP-999.                                                
133100     COMPUTE STEP-VERT = C-VERT (C-X1)                                    
133200           + ((C-VERT (C-X1 + 1) - C-VERT (C-X1))                         
133300              * (COURSE - TEMP-999)).                                     
133400     COMPUTE STEP-HORZ = C-HORZ (C-X1)                                    
133500           + ((C-HORZ (C-X1 + 1) - C-HORZ (C-X1))                         
133600              * (COURSE - TEMP-999)).                                     
133700     SUBTRACT TOR-ENERGY FROM ENERGY.                                     
133800     MOVE EPRISE-ROW TO WORK-VERT.                                        
133900     MOVE EPRISE-COL TO WORK-HORZ.                                        
134000     SUBTRACT +1 FROM TORPEDOS.                                           
134100     MOVE 'TORPEDO TRACK, COURSE'                                         
134200                                 TO  MSG-LINE.                            
134300     COMPUTE TEMP-ZZZ9 ROUNDED = C30-ADJ-QTY.                             
134400     MOVE TEMP-ZZZ9              TO  MSG-LINE22-25.                       
134500     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
134600 J00-TORPEDO-LOOP.                                                        
134700     ADD STEP-VERT TO WORK-VERT.                                          
134800     ADD STEP-HORZ TO WORK-HORZ.                                          
134900     COMPUTE TOR-ROW ROUNDED = WORK-VERT.                                 
135000     COMPUTE TOR-COL ROUNDED = WORK-HORZ.                                 
135100     IF (TOR-ROW < +1) OR (TOR-ROW > +8)                                  
135200     OR (TOR-COL < +1) OR (TOR-COL > +8)                                  
135300         MOVE 'TORPEDO MISSED'                                            
135400                                 TO  MSG-LINE                             
135500         PERFORM C50-NEXT-MSG THRU C50-EXIT                               
135600         GO TO J00-TOR-WRAPUP.                                            
135700     MOVE '                 :'                                            
135800                                 TO  MSG-LINE.                            
135900     MOVE TOR-ROW                     TO  MSG-LINE17.                     
136000     MOVE TOR-COL                     TO  MSG-LINE19.                     
136100     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
136200     SET SECTOR-X1 TO TOR-ROW.                                            
136300     SET SECTOR-X2 TO TOR-COL.                                            
136400     IF SECTOR-DISP (SECTOR-X1, SECTOR-X2) = L-NULL                       
136500         PERFORM C40-GET-RND THRU C40-EXIT                                
136600         IF RND < +0.99                                                   
136700             GO TO J00-TORPEDO-LOOP                                       
136800         ELSE                                                             
136900             MOVE 'TORPEDO DETONATED PREMATURELY.'                        
137000                                 TO  MSG-LINE                             
137100             PERFORM C50-NEXT-MSG THRU C50-EXIT                           
137200             GO TO J00-TOR-WRAPUP.                                        
137300     IF SECTOR-DISP (SECTOR-X1, SECTOR-X2) = L-KLING                      
137400         PERFORM I10-KLINGON-DESTROYED THRU I10-EXIT                      
137500         GO TO J00-TOR-WRAPUP.                                            
137600     IF SECTOR-DISP (SECTOR-X1, SECTOR-X2) = L-STARS                      
137700         PERFORM X10-STAR-DESTROYED THRU X10-EXIT                         
137800         GO TO J00-TOR-WRAPUP.                                            
137900     IF SECTOR-DISP (SECTOR-X1, SECTOR-X2) NOT = L-SBASE                  
138000         GO TO J00-TORPEDO-LOOP.                                          
138100     MOVE '*** STARBASE DESTROYED ***'                                    
138200                                 TO  MSG-LINE.                            
138300     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
138400     SET G-X1 TO EPRISE-GLXY-ROW.                                         
138500     SET G-X2 TO EPRISE-GLXY-COL.                                         
138600     SUBTRACT +1 FROM CURR-SBASE                                          
138700                      SBASE-TOTAL                                         
138800                      G-SBASE (G-X1, G-X2).                               
138900     MOVE L-NULL TO SECTOR-DISP (SECTOR-X1, SECTOR-X2).                   
139000     IF SBASE-TOTAL NOT > +0                                              
139100         MOVE +6 TO WRAPUP-CODE                                           
139200         GO TO J00-EXIT.                                                  
139300     MOVE 'STARFLEET COMMAND IS REVIEWING YOUR RECORD'                    
139400                                 TO  MSG-LINE.                            
139500     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
139600     MOVE '   FOR A POSSIBLE COURT MARTIAL!!'                             
139700                                 TO  MSG-LINE.                            
139800     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
139900     MOVE 'N' TO DOCKING-FLAG.                                            
140000 J00-TOR-WRAPUP.                                                          
140100**** IF WRAPUP-CODE NOT > +0                                              
140200****     PERFORM Q10-KLINGONS-FIRE THRU Q10-EXIT.                         
140300**** PERFORM Q50-MOVE-KLINGONS THRU Q50-EXIT                              
140400****     VARYING KLING-X1 FROM +1 BY +1                                   
140500****     UNTIL   KLING-X1 > MAX-KLING.                                    
140600 J00-EXIT.  EXIT.                                                         
140700     EJECT                                                                
140800 K00-COM-COMMAND.                                                         
140900     IF DAM-COM < +0                                                      
141000         MOVE '*** COMPUTER DISABLED ***'                                 
141100                                 TO  MSG-LINE                             
141200         PERFORM C50-NEXT-MSG THRU C50-EXIT                               
141300         GO TO K00-EXIT.                                                  
141400     IF FLDA  = SPACES                                                    
141500         PERFORM T20-COM-PROMPT THRU T20-EXIT                             
141600         GO TO K00-EXIT.                                                  
141700     MOVE FLDA                   TO  C30-IN-QTY.                          
141800     PERFORM C30-EDIT-INPUT-QTY THRU C30-EXIT.                            
141900     IF C30-ADJ-QTY = (+0 OR +1 OR +2 OR +3 OR +4 OR +5)                  
142000         MOVE C30-ADJ-QTY        TO  COMPUTER-REQUEST                     
142100     ELSE                                                                 
142200         PERFORM T20-COM-PROMPT THRU T20-EXIT                             
142300         GO TO K00-EXIT.                                                  
142400     IF COMPUTER-REQUEST = +0                                             
142500         PERFORM K10-GALAXY-RECORD THRU K10-EXIT                          
142600         GO TO K00-EXIT.                                                  
142700     IF COMPUTER-REQUEST = +1                                             
142800         PERFORM K20-STATUS-REPORT THRU K20-EXIT                          
142900         GO TO K00-EXIT.                                                  
143000     IF COMPUTER-REQUEST = +2                                             
143100         PERFORM K30-GALAXY-NAMES THRU K30-EXIT                           
143200         GO TO K00-EXIT.                                                  
143300 K00-EXIT.  EXIT.                                                         
143400     SKIP1                                                                
143500 K10-GALAXY-RECORD.                                                       
143600     MOVE 'AT THIS TIME, THE GALAXY IS KNOWN TO CONTAIN:'                 
143700                                 TO  MSG-LINE.                            
143800     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
143900     MOVE '    1     2     3     4     5     6     7     8'               
144000                                 TO  MSG-LINE.                            
144100     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
144200     MOVE L-LRS-TOP              TO  MSG-LINE.                            
144300     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
144400     PERFORM K15-SCAN-SET THRU K15-EXIT                                   
144500         VARYING G-X1 FROM +1 BY +1                                       
144600         UNTIL   G-X1 > +8.                                               
144700 K10-EXIT.  EXIT.                                                         
144800     SKIP1                                                                
144900 K15-SCAN-SET.                                                            
145000     MOVE SPACES                 TO  MSG-LINE.                            
145100     SET LRS-ROW TO G-X1.                                                 
145200     MOVE ':'                    TO  LRS-SEP1.                            
145300     PERFORM K17-QUAD-DISP THRU K17-EXIT                                  
145400         VARYING G-X2 FROM +1 BY +1                                       
145500         UNTIL   G-X2 > +8.                                               
145600     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
145700     MOVE L-LRS-TOP              TO  MSG-LINE.                            
145800     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
145900 K15-EXIT.  EXIT.                                                         
146000     SKIP1                                                                
146100 K17-QUAD-DISP.                                                           
146200     SET LRS-X1 TO G-X2.                                                  
146300     MOVE ':'                    TO  LRS-SEP (LRS-X1).                    
146400     IF G-STATUS (G-X1, G-X2) = '1'                                       
146500         MOVE G-TOTAL (G-X1, G-X2) TO LRS-SECT (LRS-X1).                  
146600 K17-EXIT.  EXIT.                                                         
146700     SKIP1                                                                
146800 K20-STATUS-REPORT.                                                       
146900     MOVE '   STATUS REPORT:'                                             
147000                                 TO  MSG-LINE.                            
147100     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
147200     PERFORM C60-BLANK-MSG THRU C60-EXIT.                                 
147300     MOVE 'THERE ARE    KLINGONS LEFT MENACING THE GALAXY.'               
147400                                 TO  MSG-LINE.                            
147500     MOVE KLING-TOTAL TO TEMP-Z9.                                         
147600     MOVE TEMP-Z9                TO  MSG-LINE11-12.                       
147700     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
147800     MOVE 'YOUR MISSION MUST BE COMPLETED BY STARDATE     .'              
147900                                 TO  MSG-LINE.                            
148000     MOVE ENDDATE                TO  TEMP-ZZZ9.                           
148100     MOVE TEMP-ZZZ9              TO  MSG-LINE44-47.                       
148200     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
148300     MOVE '   THAT GIVES YOU      STARDATES !'                            
148400                                 TO  MSG-LINE.                            
148500     COMPUTE TEMP-ZZVZ = ENDDATE - STARDATE.                              
148600     MOVE TEMP-ZZVZ              TO  MSG-LINE19-22.                       
148700     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
148800     IF SBASE-TOTAL > +0                                                  
148900         MOVE 'THERE ARE    FEDERATION STARBASES IN THE GALAXY.'          
149000                                 TO  MSG-LINE                             
149100         MOVE SBASE-TOTAL TO TEMP-Z9                                      
149200         MOVE TEMP-Z9            TO  MSG-LINE11-12                        
149300     ELSE                                                                 
149400         MOVE 'YOUR STUPIDITY HAS LEFT YOU ON YOUR OWN IN'                
149500                                 TO  MSG-LINE                             
149600         PERFORM C50-NEXT-MSG THRU C50-EXIT                               
149700         MOVE '   THE GALAXY -- YOU HAVE NO STARBASES LEFT!!!'            
149800                                 TO  MSG-LINE.                            
149900     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
150000 K20-EXIT.  EXIT.                                                         
150100     SKIP1                                                                
150200 K30-GALAXY-NAMES.                                                        
150300     PERFORM K35-NAME-SET THRU K35-EXIT                                   
150400         VARYING REGION-X1 FROM +1 BY +2                                  
150500         UNTIL   REGION-X1 > +16.                                         
150600 K30-EXIT.  EXIT.                                                         
150700     SKIP1                                                                
150800 K35-NAME-SET.                                                            
150900     MOVE SPACES                 TO  MSG-LINE.                            
151000     MOVE '----'                 TO  GAL-MARG1  GAL-MARG2                 
151100                                     GAL-MARG3  GAL-MARG4.                
151200     MOVE REGION-NAME (REGION-X1)     TO  GAL-LEFT-NAME.                  
151300     MOVE REGION-NAME (REGION-X1 + 1) TO  GAL-RIGHT-NAME.                 
151400     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
151500     MOVE SPACES                 TO  MSG-LINE.                            
151600     MOVE L-ROMAN                TO  GAL-LEFT   GAL-RIGHT.                
151700     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
151800     IF REGION-X1 < +15                                                   
151900         PERFORM C60-BLANK-MSG THRU C60-EXIT.                             
152000 K35-EXIT.  EXIT.                                                         
152100     EJECT                                                                
152200 L00-SHE-COMMAND.                                                         
152300     IF DAM-SHE < +0                                                      
152400         MOVE '*** SHIELD CONTROL INOPERABLE ***'                         
152500                                 TO  MSG-LINE                             
152600         PERFORM C50-NEXT-MSG THRU C50-EXIT                               
152700         GO TO L00-BLANK-MSG.                                             
152800     IF FLDA  = SPACES                                                    
152900         PERFORM T50-SHE-PROMPT THRU T50-EXIT                             
153000         GO TO L00-EXIT.                                                  
153100     MOVE FLDA                   TO  C30-IN-QTY.                          
153200     PERFORM C30-EDIT-INPUT-QTY THRU C30-EXIT.                            
153300     IF (C30-ADJ-QTY < +0) OR (C30-ADJ-QTY > (SHIELD + ENERGY))           
153400         PERFORM T50-SHE-PROMPT THRU T50-EXIT                             
153500         GO TO L00-EXIT.                                                  
153600     MOVE C30-ADJ-QTY            TO  SHIELD-ENERGY.                       
153700     MOVE 'DEFLECTOR CONTROL ROOM REPORT:'                                
153800                                 TO  MSG-LINE.                            
153900     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
154000     IF SHIELD-ENERGY = SHIELD                                            
154100         MOVE '   (SHELDS UNCHANGED)'                                     
154200                                 TO  MSG-LINE                             
154300         PERFORM C50-NEXT-MSG THRU C50-EXIT                               
154400         GO TO L00-BLANK-MSG.                                             
154500     COMPUTE ENERGY = ENERGY + (SHIELD - SHIELD-ENERGY).                  
154600     MOVE SHIELD-ENERGY TO SHIELD.                                        
154700     MOVE '   SHIELDS NOW AT      PER YOUR COMMAND'                       
154800                                 TO  MSG-LINE.                            
154900     MOVE SHIELD TO TEMP-ZZZ9.                                            
155000     MOVE TEMP-ZZZ9              TO  MSG-LINE19-22.                       
155100     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
155200 L00-BLANK-MSG.                                                           
155300     PERFORM C60-BLANK-MSG THRU C60-EXIT.                                 
155400 L00-EXIT.  EXIT.                                                         
155500     EJECT                                                                
155600 M00-DAM-COMMAND.                                                         
155700     IF DAM-DAM < +0                                                      
155800         MOVE '*** DAMAGE CONTROL REPORT NOT AVAILABLE ***'               
155900                                 TO  MSG-LINE                             
156000         PERFORM C50-NEXT-MSG THRU C50-EXIT                               
156100     ELSE                                                                 
156200         PERFORM M10-DAM-REPORT THRU M10-EXIT.                            
156300     IF DOCKING-FLAG = 'Y'                                                
156400         PERFORM V10-OPTIONAL-REPAIRS THRU V10-EXIT.                      
156500 M00-EXIT.  EXIT.                                                         
156600     SKIP1                                                                
156700 M10-DAM-REPORT.                                                          
156800     PERFORM C60-BLANK-MSG THRU C60-EXIT.                                 
156900     MOVE 'CURRENT DAMAGES TO THE ENTERPRISE ARE AS FOLLOWS:'             
157000                                 TO  MSG-LINE.                            
157100     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
157200     PERFORM C60-BLANK-MSG THRU C60-EXIT.                                 
157300     MOVE '...DEVICE.......STARDATES TO REPAIR...'                        
157400                                 TO  MSG-LINE.                            
157500     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
157600     PERFORM C60-BLANK-MSG THRU C60-EXIT.                                 
157700     PERFORM M15-1-DEVICE-REPORT THRU M15-EXIT                            
157800         VARYING DAM-SUB FROM +1 BY +1                                    
157900         UNTIL DAM-SUB > +8.                                              
158000     PERFORM C60-BLANK-MSG THRU C60-EXIT.                                 
158100 M10-EXIT.  EXIT.                                                         
158200     SKIP1                                                                
158300 M15-1-DEVICE-REPORT.                                                     
158400     IF DAM-REPAIR (DAM-SUB) > +0                                         
158500         MOVE +0                 TO  DAM-REPAIR (DAM-SUB).                
158600     IF DAM-REPAIR (DAM-SUB) = +0                                         
158700         GO TO M15-EXIT.                                                  
158800     MOVE DAM-NAME (DAM-SUB)      TO  MSG-LINE.                           
158900     MOVE DAM-REPAIR (DAM-SUB) TO TEMP-ZZVZ.                              
159000     MOVE TEMP-ZZVZ              TO  MSG-LINE22-25.                       
159100     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
159200 M15-EXIT.  EXIT.                                                         
159300     EJECT                                                                
159400 N00-INSTRUCTIONS.                                                        
159500 N00-EXIT.  EXIT.                                                         
159600     EJECT                                                                
159700 Q10-KLINGONS-FIRE.                                                       
159800     IF CURR-KLING NOT > +0                                               
159900         GO TO Q10-EXIT.                                                  
160000     IF DOCKING-FLAG = 'Y'                                                
160100         MOVE 'STAR BASE SHIELDS PROTECT THE ENTERPRISE'                  
160200                                 TO  MSG-LINE                             
160300         PERFORM C50-NEXT-MSG THRU C50-EXIT                               
160400         GO TO Q10-EXIT.                                                  
160500     PERFORM Q15-ONE-KLINGON-FIRES THRU Q15-EXIT                          
160600         VARYING KLING-X1 FROM 1 BY 1                                     
160700         UNTIL KLING-X1 > MAX-KLING.                                      
160800 Q10-EXIT.  EXIT.                                                         
160900     SKIP1                                                                
161000 Q15-ONE-KLINGON-FIRES.                                                   
161100     IF KLING-ENERGY (KLING-X1) NOT > +0                                  
161200         GO TO Q15-EXIT.                                                  
161300     PERFORM C40-GET-RND THRU C40-EXIT.                                   
161400     PERFORM Q30-DIST-FROM-KLINGON THRU Q30-EXIT.                         
161500*******************                                                       
161600*********  THIS CALCULATION IS MY IDEA OF WHAT'S FAIR,                    
161700*********  BUT IT IS VERY EASY TO CHANGE.                                 
161800*******************                                                       
161900     COMPUTE HIT ROUNDED =                                                
162000         (KLING-ENERGY (KLING-X1) * (1 - (0.1 * RND)))                    
162100         / (DIST ** 0.5).                                                 
162200     IF SHIELD > +0                                                       
162300         COMPUTE HIT-RATIO ROUNDED = HIT / SHIELD                         
162400     ELSE                                                                 
162500         COMPUTE HIT-RATIO ROUNDED = HIT.                                 
162600     SUBTRACT HIT FROM SHIELD.                                            
162700     MOVE '     UNITS HIT ON ENTERPRISE FROM SECTOR  :'                   
162800                                 TO  MSG-LINE.                            
162900     MOVE HIT TO TEMP-ZZZ9.                                               
163000     MOVE TEMP-ZZZ9             TO  MSG-LINE01-04.                        
163100     MOVE KLING-ROW (KLING-X1)   TO  MSG-LINE42.                          
163200     MOVE KLING-COL (KLING-X1)   TO  MSG-LINE44.                          
163300     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
163400     IF SHIELD < +0                                                       
163500         MOVE +3 TO WRAPUP-CODE                                           
163600         SET KLING-X1 TO MAX-KLING                                        
163700         MOVE +0                 TO  SHIELD                               
163800         MOVE 'YOUR SHIELDS HAVE BEEN DEPLETED BY KLINGON ATTACK.'        
163900                                 TO  MSG-LINE                             
164000         PERFORM C50-NEXT-MSG THRU C50-EXIT                               
164100         GO TO Q15-BLANK-MSG.                                             
164200     MOVE '      (SHIELDS DOWN TO      UNITS)'                            
164300                                 TO  MSG-LINE.                            
164400     MOVE SHIELD TO TEMP-ZZZ9.                                            
164500     MOVE TEMP-ZZZ9             TO  MSG-LINE24-27.                        
164600     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
164700     IF HIT < +20                                                         
164800         GO TO Q15-BLANK-MSG.                                             
164900**************                                                            
165000*********  THE MINIMUM HIT-RATIO BELOW IS ALSO EASILY CHANGED.            
165100**************                                                            
165200     IF HIT-RATIO < +0.20                                                 
165300         GO TO Q15-BLANK-MSG.                                             
165400***  DAMAGE TO THE 'ENTERPRISE' MAY HAVE OCCURED.  THE HIGHER             
165500***  THE HIT-RATIO, THE MORE LIKELY TO BE DAMAGED.                        
165600     PERFORM C40-GET-RND THRU C40-EXIT.                                   
165700     IF RND > HIT-RATIO                                                   
165800         GO TO Q15-BLANK-MSG.                                             
165900     PERFORM C40-GET-RND THRU C40-EXIT.                                   
166000     COMPUTE TEMP-999V9999 = (HIT-RATIO * 4)                              
166100                            + (RND * 0.5).                                
166200     IF TEMP-999V9999 > +5                                                
166300         MOVE +5                 TO  TEMP-999V9999.                       
166400     PERFORM C40-GET-RND THRU C40-EXIT.                                   
166500     COMPUTE DAM-SUB = 1 + (RND * 8).                                     
166600     IF DAM-SUB = +2                                                      
166700         MULTIPLY 0.5 BY TEMP-999V9999 ROUNDED.                           
166800     SUBTRACT TEMP-999V9999 FROM DAM-REPAIR (DAM-SUB).                    
166900     MOVE 'DAMAGE CONTROL REPORTS              DAMAGED BY HIT'            
167000                                 TO  MSG-LINE.                            
167100     MOVE DAM-NAME (DAM-SUB)    TO  MSG-LINE24-35.                        
167200     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
167300 Q15-BLANK-MSG.                                                           
167400     PERFORM C60-BLANK-MSG THRU C60-EXIT.                                 
167500 Q15-EXIT.  EXIT.                                                         
167600     SKIP1                                                                
167700 Q30-DIST-FROM-KLINGON.                                                   
167800     COMPUTE DIST = (((KLING-ROW (KLING-X1) - EPRISE-ROW) ** 2)           
167900                   + ((KLING-COL (KLING-X1) - EPRISE-COL) ** 2))          
168000                   ** 0.5.                                                
168100     IF DIST < +1                                                         
168200         MOVE +1                 TO  DIST.                                
168300 Q30-EXIT.  EXIT.                                                         
168400     SKIP1                                                                
168500 Q50-MOVE-KLINGONS.                                                       
168600     IF KLING-ENERGY (KLING-X1) NOT > +0                                  
168700         GO TO Q50-EXIT.                                                  
168800     SET SECTOR-X1 TO KLING-ROW (KLING-X1).                               
168900     SET SECTOR-X2 TO KLING-COL (KLING-X1).                               
169000     MOVE L-NULL TO SECTOR-DISP (SECTOR-X1, SECTOR-X2).                   
169100     PERFORM R10-RANDOM-HOLE THRU R10-EXIT.                               
169200     SET SECTOR-X1 TO RANDOM-ROW.                                         
169300     SET SECTOR-X2 TO RANDOM-COL.                                         
169400     MOVE L-KLING TO SECTOR-DISP (SECTOR-X1, SECTOR-X2).                  
169500     MOVE RANDOM-ROW TO KLING-ROW (KLING-X1).                             
169600     MOVE RANDOM-COL TO KLING-COL (KLING-X1).                             
169700 Q50-EXIT.  EXIT.                                                         
169800     SKIP1                                                                
169900 R10-RANDOM-HOLE.                                                         
170000     PERFORM C40-GET-RND THRU C40-EXIT.                                   
170100     COMPUTE RANDOM-ROW = 1 + (RND * 8).                                  
170200     PERFORM C40-GET-RND THRU C40-EXIT.                                   
170300     COMPUTE RANDOM-COL = 1 + (RND * 8).                                  
170400     SET SECTOR-X1 TO RANDOM-ROW.                                         
170500     SET SECTOR-X2 TO RANDOM-COL.                                         
170600     IF SECTOR-DISP (SECTOR-X1, SECTOR-X2) NOT = L-NULL                   
170700         GO TO R10-RANDOM-HOLE.                                           
170800 R10-EXIT.  EXIT.                                                         
170900     SKIP1                                                                
171000 R30-RANDOM-DAMAGE.                                                       
171100     MOVE 'THE ENTERPRISE JUST PASSED THRU AN ION STORM.'                 
171200                                 TO  MSG-LINE.                            
171300     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
171400     PERFORM C40-GET-RND THRU C40-EXIT.                                   
171500     COMPUTE DAM-SUB = 1 + (RND * 8).                                     
171600     IF DAM-REPAIR (DAM-SUB) < +0                                         
171700         MOVE '                       DAMAGED FURTHER'                    
171800                                 TO  MSG-LINE                             
171900     ELSE                                                                 
172000         MOVE '                       DAMAGED'                            
172100                                 TO  MSG-LINE.                            
172200     MOVE DAM-NAME (DAM-SUB)    TO  MSG-LINE11-22.                        
172300     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
172400     PERFORM C40-GET-RND THRU C40-EXIT.                                   
172500     IF DAM-SUB = +2                                                      
172600         MULTIPLY 0.4 BY RND ROUNDED.                                     
172700     COMPUTE DAM-REPAIR (DAM-SUB) =                                       
172800             DAM-REPAIR (DAM-SUB) - (1 + (RND * 2)).                      
172900 R30-EXIT.  EXIT.                                                         
173000     SKIP1                                                                
173100 R50-RANDOM-REPAIR.                                                       
173200     PERFORM C40-GET-RND THRU C40-EXIT.                                   
173300     COMPUTE DAM-SUB = 1 + (RND * 8).                                     
173400     IF DAM-REPAIR (DAM-SUB) NOT < +0                                     
173500         PERFORM C40-GET-RND THRU C40-EXIT                                
173600         IF RND > +0.1                                                    
173700             GO TO R50-RANDOM-REPAIR                                      
173800         ELSE                                                             
173900             GO TO R50-EXIT.                                              
174000     MOVE 'REPAIR CREWS HAVE BEEN WORKING OVERTIME.'                      
174100                                 TO  MSG-LINE.                            
174200     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
174300     COMPUTE DAM-REPAIR (DAM-SUB) =                                       
174400             DAM-REPAIR (DAM-SUB) + (1 + (RND * 2)).                      
174500     IF DAM-REPAIR (DAM-SUB) > +0                                         
174600         MOVE +0 TO DAM-REPAIR (DAM-SUB).                                 
174700     IF DAM-REPAIR (DAM-SUB) = +0                                         
174800         MOVE '                       REPAIR COMPLETED EARLY'             
174900                                 TO  MSG-LINE                             
175000     ELSE                                                                 
175100         MOVE '                       REPAIR TIME REDUCED'                
175200                                 TO  MSG-LINE.                            
175300     MOVE DAM-NAME (DAM-SUB)    TO  MSG-LINE11-22.                        
175400     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
175500 R50-EXIT.  EXIT.                                                         
175600     SKIP1                                                                
175700 S10-EPRISE-TO-NEW-QUAD.                                                  
175800     COMPUTE WORK-VERT =                                                  
175900             WORK-VERT                                                    
176000             + (8 * EPRISE-GLXY-ROW)                                      
176100             + (SECTORS-MOVE-TOTL * STEP-VERT).                           
176200     COMPUTE WORK-HORZ =                                                  
176300             WORK-HORZ                                                    
176400             + (8 * EPRISE-GLXY-COL)                                      
176500             + (SECTORS-MOVE-TOTL * STEP-HORZ).                           
176600     COMPUTE EPRISE-GLXY-ROW = WORK-VERT / 8.                             
176700     COMPUTE EPRISE-GLXY-COL = WORK-HORZ / 8.                             
176800     COMPUTE EPRISE-VERT = WORK-VERT - (8 * EPRISE-GLXY-ROW).             
176900     COMPUTE EPRISE-HORZ = WORK-HORZ - (8 * EPRISE-GLXY-COL).             
177000     IF EPRISE-VERT < +1                                                  
177100         SUBTRACT +1 FROM EPRISE-GLXY-ROW                                 
177200         ADD +8 TO EPRISE-VERT.                                           
177300     IF EPRISE-HORZ < +1                                                  
177400         SUBTRACT +1 FROM EPRISE-GLXY-COL                                 
177500         ADD +8 TO EPRISE-HORZ.                                           
177600     MOVE +0 TO GALAXY-LIMIT-FLAG.                                        
177700     IF EPRISE-GLXY-ROW < +1                                              
177800         MOVE +1 TO GALAXY-LIMIT-FLAG                                     
177900                    EPRISE-GLXY-ROW                                       
178000                    EPRISE-VERT.                                          
178100     IF EPRISE-GLXY-ROW > +8                                              
178200         MOVE +1 TO GALAXY-LIMIT-FLAG                                     
178300         MOVE +8 TO EPRISE-GLXY-ROW                                       
178400                    EPRISE-VERT.                                          
178500     IF EPRISE-GLXY-COL < +1                                              
178600         MOVE +1 TO GALAXY-LIMIT-FLAG                                     
178700                    EPRISE-GLXY-COL                                       
178800                    EPRISE-HORZ.                                          
178900     IF EPRISE-GLXY-COL > +8                                              
179000         MOVE +1 TO GALAXY-LIMIT-FLAG                                     
179100         MOVE +8 TO EPRISE-GLXY-COL                                       
179200                    EPRISE-HORZ.                                          
179300     IF GALAXY-LIMIT-FLAG = +1                                            
179400         MOVE 'LT. UHURA REPORTS MESSAGE FROM STARFLEET COMMAND:'         
179500                                 TO  MSG-LINE                             
179600         PERFORM C50-NEXT-MSG THRU C50-EXIT                               
179700         MOVE '   PERMISSION TO ATTEMPT CROSSING OF GALACTIC'             
179800                                 TO  MSG-LINE                             
179900         PERFORM C50-NEXT-MSG THRU C50-EXIT                               
180000         MOVE '   PERIMETER IS HEREBY DENIED.  SHUT DOWN ENGINES.'        
180100                                 TO  MSG-LINE                             
180200         PERFORM C50-NEXT-MSG THRU C50-EXIT                               
180300         MOVE 'CHIEF ENGINEER SCOTT REPORTS WARP ENGINES'                 
180400                                 TO  MSG-LINE                             
180500         PERFORM C50-NEXT-MSG THRU C50-EXIT                               
180600         MOVE '   SHUT DOWN AT SECTOR  :  OF QUADRANT  :'                 
180700                                 TO  MSG-LINE                             
180800         MOVE EPRISE-ROW                 TO  MSG-LINE24                   
180900         MOVE EPRISE-COL                 TO  MSG-LINE26                   
181000         MOVE EPRISE-GLXY-ROW                 TO  MSG-LINE40              
181100         MOVE EPRISE-GLXY-COL                 TO  MSG-LINE42              
181200         PERFORM C50-NEXT-MSG THRU C50-EXIT.                              
181300     IF  (EPRISE-GLXY-ROW NOT = PREV-GLXY-ROW)                            
181400     OR  (EPRISE-GLXY-COL NOT = PREV-GLXY-COL)                            
181500         PERFORM S15-ENTER-NEW-QUADRANT THRU S15-EXIT.                    
181600 S10-EXIT.  EXIT.                                                         
181700     SKIP1                                                                
181800 S15-ENTER-NEW-QUADRANT.                                                  
181900     MOVE EPRISE-GLXY-ROW TO QUAD-NAME-ROW.                               
182000     MOVE EPRISE-GLXY-COL TO QUAD-NAME-COL.                               
182100     MOVE +0 TO CURR-KLING                                                
182200                CURR-SBASE                                                
182300                CURR-STARS                                                
182400                REGION-ONLY-FLAG.                                         
182500     PERFORM C40-GET-RND THRU C40-EXIT.                                   
182600     PERFORM S30-QUADRANT-NAME THRU S30-EXIT.                             
182700     MOVE QUAD-NAME              TO  CURR-QUAD.                           
182800     PERFORM C60-BLANK-MSG THRU C60-EXIT.                                 
182900     IF STARDATE NOT = FIRST-STARDATE                                     
183000         MOVE 'NOW ENTERING                  QUADRANT...'                 
183100                                 TO  MSG-LINE                             
183200         MOVE CURR-QUAD          TO  MSG-LINE14-29                        
183300         PERFORM C50-NEXT-MSG THRU C50-EXIT                               
183400     ELSE                                                                 
183500         MOVE 'YOUR MISSION BEGINS WITH YOUR STARSHIP LOCATED'            
183600                                 TO  MSG-LINE                             
183700         PERFORM C50-NEXT-MSG THRU C50-EXIT                               
183800         MOVE 'IN THE GALACTIC QUADRANT,'                                 
183900                                 TO  MSG-LINE                             
184000         MOVE CURR-QUAD          TO  MSG-LINE27-42                        
184100         PERFORM C50-NEXT-MSG THRU C50-EXIT.                              
184200     PERFORM C60-BLANK-MSG THRU C60-EXIT.                                 
184300     SET G-X1 TO EPRISE-GLXY-ROW.                                         
184400     SET G-X2 TO EPRISE-GLXY-COL.                                         
184500     MOVE G-KLING (G-X1, G-X2) TO CURR-KLING.                             
184600     MOVE G-SBASE (G-X1, G-X2) TO CURR-SBASE.                             
184700     MOVE G-STARS (G-X1, G-X2) TO CURR-STARS.                             
184800     MOVE '1' TO G-STATUS (G-X1, G-X2).                                   
184900     IF CURR-KLING > +0                                                   
185000         MOVE 'COMBAT AREA      CONDITION  *RED*'                         
185100                                 TO  MSG-LINE                             
185200         PERFORM C50-NEXT-MSG THRU C50-EXIT                               
185300         IF SHIELD < (INIT-ENERGY-KLING * CURR-KLING)                     
185400             PERFORM C60-BLANK-MSG THRU C60-EXIT                          
185500             MOVE ALL '*'        TO  MSG-LINE01-35                        
185600             PERFORM C50-NEXT-MSG THRU C50-EXIT                           
185700             MOVE '*** SHIELDS ARE DANGEROUSLY LOW ***'                   
185800                                 TO  MSG-LINE                             
185900             PERFORM C50-NEXT-MSG THRU C50-EXIT                           
186000             MOVE ALL '*'        TO  MSG-LINE01-35                        
186100             PERFORM C50-NEXT-MSG THRU C50-EXIT.                          
186200     MOVE +0 TO L-INDX.                                                   
186300     SET KLING-X1 TO L-INDX.                                              
186400     MOVE +0                     TO  KLING-ROW    (KLING-X1 + 1)          
186500                                     KLING-COL    (KLING-X1 + 1)          
186600                                     KLING-ENERGY (KLING-X1 + 1)          
186700                                     KLING-ROW    (KLING-X1 + 2)          
186800                                     KLING-COL    (KLING-X1 + 2)          
186900                                     KLING-ENERGY (KLING-X1 + 2)          
187000                                     KLING-ROW    (KLING-X1 + 3)          
187100                                     KLING-COL    (KLING-X1 + 3)          
187200                                     KLING-ENERGY (KLING-X1 + 3).         
187300     MOVE L-NULL-TABLE           TO  SECTOR-AREA.                         
187400     SET SECTOR-X1 TO EPRISE-ROW.                                         
187500     SET SECTOR-X2 TO EPRISE-COL.                                         
187600     MOVE L-EPRISE TO SECTOR-DISP (SECTOR-X1, SECTOR-X2).                 
187700     PERFORM S60-POSITION-1-KLINGON THRU S60-EXIT                         
187800         CURR-KLING TIMES.                                                
187900     PERFORM S70-POSITION-1-STARBASE THRU S70-EXIT                        
188000         CURR-SBASE TIMES.                                                
188100     PERFORM S80-POSITION-1-STAR THRU S80-EXIT                            
188200         CURR-STARS TIMES.                                                
188300 S15-EXIT.  EXIT.                                                         
188400     SKIP1                                                                
188500 S30-QUADRANT-NAME.                                                       
188600***  INPUT IS QUAD-NAME-ROW,QUAD-NAME-COL                                 
188700***      WHICH = EPRISE-GLXY-ROW,EPRISE-GLXY-COL.                         
188800***  OUTPUT IS QUAD-NAME                                                  
188900***  IF REGION-ONLY-FLAG=1, GIVE REGION NAME ONLY                         
189000     COMPUTE TEMP-999 = QUAD-NAME-ROW * 2.                                
189100     IF QUAD-NAME-COL < +5                                                
189200         SUBTRACT +1 FROM TEMP-999.                                       
189300     SET REGION-X1 TO TEMP-999.                                           
189400     MOVE REGION-NAME (REGION-X1) TO QUAD-NAME.                           
189500     IF REGION-ONLY-FLAG = +1                                             
189600         GO TO S30-EXIT.                                                  
189700     MOVE +5 TO L-INDX.                                                   
189800     SET QUAD-X1 TO L-INDX.                                               
189900 S30-LOOP.                                                                
190000     IF QUAD-NAME-BYTE (QUAD-X1) NOT = ' '                                
190100         SET QUAD-X1 UP BY +1                                             
190200         GO TO S30-LOOP.                                                  
190300     MOVE QUAD-NAME-COL TO TEMP-999.                                      
190400     IF QUAD-NAME-COL > +4                                                
190500         SUBTRACT +4 FROM TEMP-999.                                       
190600     MOVE 'I' TO QUAD-NAME-BYTE (QUAD-X1 + 1).                            
190700     IF TEMP-999 = +1                                                     
190800         GO TO S30-EXIT.                                                  
190900     IF TEMP-999 = +4                                                     
191000         MOVE 'V' TO QUAD-NAME-BYTE (QUAD-X1 + 2)                         
191100         GO TO S30-EXIT                                                   
191200     ELSE                                                                 
191300         MOVE 'I' TO QUAD-NAME-BYTE (QUAD-X1 + 2).                        
191400     IF TEMP-999 = +3                                                     
191500         MOVE 'I' TO QUAD-NAME-BYTE (QUAD-X1 + 3).                        
191600 S30-EXIT.  EXIT.                                                         
191700     SKIP1                                                                
191800 S60-POSITION-1-KLINGON.                                                  
191900     SET KLING-X1 UP BY +1.                                               
192000     PERFORM R10-RANDOM-HOLE THRU R10-EXIT.                               
192100     MOVE L-KLING TO SECTOR-DISP (SECTOR-X1, SECTOR-X2).                  
192200     SET KLING-ROW (KLING-X1) TO SECTOR-X1.                               
192300     SET KLING-COL (KLING-X1) TO SECTOR-X2.                               
192400     MOVE INIT-ENERGY-KLING      TO  KLING-ENERGY (KLING-X1).             
192500 S60-EXIT.  EXIT.                                                         
192600     SKIP1                                                                
192700 S70-POSITION-1-STARBASE.                                                 
192800     PERFORM R10-RANDOM-HOLE THRU R10-EXIT.                               
192900     MOVE L-SBASE TO SECTOR-DISP (SECTOR-X1, SECTOR-X2).                  
193000     SET SBASE-ROW TO SECTOR-X1.                                          
193100     SET SBASE-COL TO SECTOR-X2.                                          
193200 S70-EXIT.  EXIT.                                                         
193300     SKIP1                                                                
193400 S80-POSITION-1-STAR.                                                     
193500     PERFORM R10-RANDOM-HOLE THRU R10-EXIT.                               
193600     MOVE L-STARS TO SECTOR-DISP (SECTOR-X1, SECTOR-X2).                  
193700 S80-EXIT.  EXIT.                                                         
193800     SKIP1                                                                
193900 T10-NAV-PROMPT.                                                          
194000     IF NAV-EDIT-FLAG = 'B'                                               
194100         GO TO T10-B.                                                     
194200     MOVE 'CHIEF ENGINEER SCOTT REQUESTS A COURSE HEADING:'               
194300                                 TO  MSG-LINE.                            
194400     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
194500     MOVE '  MIN=0, MAX=360'                                              
194600                                 TO  MSG-LINE.                            
194700     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
194800 T10-B.                                                                   
194900     MOVE 'SCOTTY ALSO NEEDS A WARP FACTOR: MIN=.01, MAX=8.00'            
195000                                 TO  MSG-LINE.                            
195100**** IF DAM-NAV < +0                                                      
195200****     MOVE '0'                TO  MSG-LINE47                           
195300****     MOVE '2'                TO  MSG-LINE49                           
195400****     PERFORM C50-NEXT-MSG THRU C50-EXIT                               
195500****     MOVE '*** MAX WARP = 0.20 DUE TO DAMAGE TO WARP ENGINES'         
195600****                             TO  MSG-LINE.                            
195700     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
195800     IF NAV-EDIT-FLAG = 'A'                                               
195900         MOVE 'ENTER... COURSE (FLDA) & WARP FACTOR (FLDB)'               
196000                                 TO  MESSAGEO                             
196100     ELSE                                                                 
196200         MOVE 'ENTER... WARP FACTOR (FLDB)'                               
196300                                 TO  MESSAGEO.                            
196400 T10-EXIT.  EXIT.                                                         
196500     SKIP1                                                                
196600 T20-COM-PROMPT.                                                          
196700     MOVE 'FUNCTIONS AVAILABLE FROM THE LIBRARY-COMPUTER:'                
196800                                 TO  MSG-LINE.                            
196900     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
197000     MOVE '  0 = CUMULATIVE GALAXY SCAN'                                  
197100                                 TO  MSG-LINE.                            
197200     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
197300     MOVE '  1 = STATUS REPORT'                                           
197400                                 TO  MSG-LINE.                            
197500     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
197600     MOVE '  2 = QUADRANT NOMENCLATURE MAP'                               
197700                                 TO  MSG-LINE.                            
197800     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
197900     PERFORM C60-BLANK-MSG THRU C60-EXIT.                                 
198000     MOVE 'ENTER... COMPUTER FUNCTION (FLDA)  OR CHANGE COMMAND'          
198100                                 TO  MESSAGEO.                            
198200 T20-EXIT.  EXIT.                                                         
198300     SKIP1                                                                
198400 T30-TOR-PROMPT.                                                          
198500     MOVE 'ENSIGN CHECKOV REQUESTS A TORPEDO COURSE HEADING.'             
198600                                 TO  MSG-LINE.                            
198700     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
198800     MOVE '  MIN=0, MAX=360'                                              
198900                                 TO  MSG-LINE.                            
199000     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
199100     MOVE 'ENTER... TORPEDO COURSE (FLDA)  OR CHANGE COMMAND'             
199200                                 TO  MESSAGEO.                            
199300 T30-EXIT.  EXIT.                                                         
199400     SKIP1                                                                
199500 T40-PHA-PROMPT.                                                          
199600     MOVE 'SCIENCE OFFICER SPOCK REQUESTS AMOUNT OF CURRENT'              
199700                                 TO  MSG-LINE.                            
199800     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
199900     MOVE 'ENERGY YOU WANT FIRED BY PHASERS?  MIN=1, MAX=    '            
200000                                 TO  MSG-LINE.                            
200100     MOVE ENERGY TO TEMP-ZZZ9.                                            
200200     MOVE TEMP-ZZZ9              TO  MSG-LINE47-50.                       
200300     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
200400     MOVE 'ENTER... ENERGY TO FIRE (FLDA)  OR CHANGE COMMAND'             
200500                                 TO  MESSAGEO.                            
200600 T40-EXIT.  EXIT.                                                         
200700     SKIP1                                                                
200800 T50-SHE-PROMPT.                                                          
200900     MOVE 'SCIENCE OFFICER SPOCK REQUESTS AMOUNT OF ENERGY'               
201000                                 TO  MSG-LINE.                            
201100     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
201200     MOVE '  TO BE USED FOR SHIELDS:  MIN=0, MAX =   '                    
201300                                 TO  MSG-LINE.                            
201400     COMPUTE TEMP-ZZZ9 = ENERGY + SHIELD.                                 
201500     MOVE TEMP-ZZZ9              TO  MSG-LINE39-42.                       
201600     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
201700     MOVE 'ENTER... NEW SHIELD STRENGTH (FLDA) OR CHANGE COMMAND'         
201800                                 TO  MESSAGEO.                            
201900 T50-EXIT.  EXIT.                                                         
202000     SKIP1                                                                
202100 T60-NAV-DAMAGED.                                                         
202200     MOVE 'NAVIGATION WAS INACCURATE (WARP ENGINE DAMAGED).'              
202300                                 TO  MSG-LINE.                            
202400     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
202500     PERFORM C40-GET-RND THRU C40-EXIT.                                   
202600     COMPUTE COURSE = COURSE + ((RND - 0.5) * 2).                         
202700     IF COURSE < +1                                                       
202800         ADD +8 TO COURSE.                                                
202900     PERFORM C40-GET-RND THRU C40-EXIT.                                   
203000     COMPUTE WARP-FACTOR = WARP-FACTOR + ((RND - 0.5) * 2).               
203100     IF WARP-FACTOR < +0.125                                              
203200         MOVE +0.125 TO WARP-FACTOR.                                      
203300     IF WARP-FACTOR > +8                                                  
203400         MOVE +8 TO WARP-FACTOR.                                          
203500     COMPUTE SECTORS-MOVE-TOTL ROUNDED = WARP-FACTOR * 8.                 
203600     COMPUTE WARP-ENERGY ROUNDED = (WARP-FACTOR * 8) + 10.                
203700 T60-EXIT.  EXIT.                                                         
203800     SKIP1                                                                
203900 V10-OPTIONAL-REPAIRS.                                                    
204000     MOVE +0                     TO  REPAIR-EST.                          
204100     PERFORM V15-IS-THERE-DAMAGE THRU V15-EXIT                            
204200         VARYING DAM-SUB FROM +1 BY +1                                    
204300         UNTIL   DAM-SUB > +8.                                            
204400     IF REPAIR-EST = +0                                                   
204500         GO TO V10-EXIT.                                                  
204600     PERFORM M10-DAM-REPORT THRU M10-EXIT.                                
204700     PERFORM C60-BLANK-MSG THRU C60-EXIT.                                 
204800     PERFORM C40-GET-RND THRU C40-EXIT.                                   
204900     COMPUTE REPAIR-EST ROUNDED =                                         
205000         REPAIR-EST + (RND * 0.3).                                        
205100     IF REPAIR-EST > +1.0                                                 
205200         MOVE +1.0               TO  REPAIR-EST.                          
205300     MOVE 'TECHNICIANS STANDING BY TO REPAIR YOUR DAMAGES.'               
205400                                 TO  MSG-LINE.                            
205500     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
205600     MOVE '   ESTIMATED TIME TO REPAIR IS     STARDATES.'                 
205700                                 TO  MSG-LINE.                            
205800     MOVE REPAIR-EST TO TEMP-9-9.                                         
205900     MOVE TEMP-9-9               TO  MSG-LINE32-34.                       
206000     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
206100     MOVE 'WILL YOU AUTHORIZE THE REPAIR ORDER?  YES/NO (CMD)'            
206200                                 TO  MSG-LINE.                            
206300     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
206400     PERFORM V30-YES-OR-NO THRU V30-EXIT.                                 
206500     IF CMDI NOT = 'YES'                                                  
206600         GO TO V10-EXIT.                                                  
206700     MOVE +0                     TO  DAM-NAV  DAM-TOR                     
206800                                     DAM-SRS  DAM-DAM                     
206900                                     DAM-LRS  DAM-SHE                     
207000                                     DAM-PHA  DAM-COM.                    
207100     PERFORM C40-GET-RND THRU C40-EXIT.                                   
207200     COMPUTE DAM-DELAY ROUNDED = RND * 0.5.                               
207300     COMPUTE STARDATE = STARDATE + REPAIR-EST + DAM-DELAY.                
207400     MOVE 'ALL REPAIRS COMPLETED AS AUTHORIZED.'                          
207500                                 TO  MSG-LINE.                            
207600     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
207700     IF DAM-DELAY > +0                                                    
207800         MOVE 'THE REPAIR CREW REQUIRED AN EXTRA     STARDATES.'          
207900                                 TO  MSG-LINE                             
208000         MOVE DAM-DELAY TO TEMP-9-9                                       
208100         MOVE TEMP-9-9           TO  MSG-LINE35-37                        
208200         PERFORM C50-NEXT-MSG THRU C50-EXIT.                              
208300 V10-EXIT.  EXIT.                                                         
208400     SKIP1                                                                
208500 V15-IS-THERE-DAMAGE.                                                     
208600     IF DAM-REPAIR (DAM-SUB) < +0                                         
208700         ADD +0.2 TO REPAIR-EST.                                          
208800 V15-EXIT.  EXIT.                                                         
208900     SKIP1                                                                
209000 V30-YES-OR-NO.                                                           
209100     PERFORM C70-SRS-COMMAND THRU C70-EXIT.                               
209200     MOVE 'ENTER... YES OR NO (CMD)'                                      
209300                                 TO  MESSAGEO.                            
209400 V30-BMS-OUT-MAP1.                                                        
209500 V30-BMS-OUT-MAP2.                                                        
209600     MOVE  'DISPLAY' TO ISPF-SERVICE.                                     
209700     MOVE 'TRKPMAIN' TO ISPF-PANEL-NAME.                                  
209800     MOVE '        ' TO ISPF-MSG-ID.                                      
209900     MOVE '        ' TO ISPF-FIELD-NAME.                                  
210000     CALL 'ISPLINK' USING  ISPF-SERVICE  ISPF-PANEL-NAME                  
210100                           ISPF-MSG-ID  ISPF-FIELD-NAME.                  
210200     IF RETURN-CODE NOT = 0                                               
210300*                              8  - END OR RETURN COMMAND ENTERED         
210400*                              12 - PANEL, MESSAGE, OR CURSOR             
210500*                                   FIELD COULD NOT BE FOUND              
210600*                              16 - DATA TRUNCATION OR TRANSLATION        
210700*                                   ERROR                                 
210800*                              20 - SEVERE ERROR                          
210900**********  ERROR                                                         
211000         MOVE 'X0502-' TO ABEND-CODE,                                     
211100         GO TO Z99-ABEND.                                                 
211200 V30-BMS-IN.                                                              
211300*********                                                                 
211400     IF CMDI = SPACES                                                     
211500         GO TO V30-YES-OR-NO.                                             
211600     MOVE +0 TO L-INDX.                                                   
211700     SET MLINE-X1 TO L-INDX.                                              
211800     MOVE SPACES                 TO  MAP2O                                
211900                                     MESSAGEO.                            
212000 V30-EXIT.  EXIT.                                                         
212100     SKIP1                                                                
212200 V50-TIME-CHECK.                                                          
212300***  THIS PARAGRAPH CALL A SUB-ROUTINE WHICH CHECKS THE OPERATOR'S        
212400***  SIGN-ON VERSUS THE TIME-OF-DAY AND DETERMINES WHETHER HE             
212500***  IS ALLOWED TO PLAY AT THIS TIME.                                     
212600***  ALL RECREATIONAL PROGRAMS OBTAINED FROM ME CALL THIS                 
212700***  SUB-ROUTINE, BUT 'PCDC17' IS A SEPARATE PROGRAM PRODUCT.             
212800***DFHPC TYPE=LINK,                                              X        
212900***      PROGRAM=PCDC17                                                   
213000 V50-EXIT.  EXIT.                                                         
213100     SKIP1                                                                
213200 V70-VALID-COMMANDS.                                                      
213300     MOVE +3 TO L-INDX.                                                   
213400     SET MLINE-X1 TO L-INDX.                                              
213500     MOVE 'PLEASE ENTER ONE OF THE FOLLOWING VALID COMMANDS:'             
213600                                 TO  MLINE (MLINE-X1 + 00).               
213700     MOVE '  SAV - SAVE CURRENT GAME FOR LATER RECALL'                    
213800                                 TO  MLINE (MLINE-X1 + 02).               
213900     MOVE '  NAV - NAVIGATE (FLDA=COURSE, FLDB=WARP FACTOR)'              
214000                                 TO  MLINE (MLINE-X1 + 03).               
214100     MOVE '  LRS - LONG RANGE SENSOR SCAN'                                
214200                                 TO  MLINE (MLINE-X1 + 04).               
214300     MOVE '  PHA - FIRE PHASERS (FLDA=ENERGY)'                            
214400                                 TO  MLINE (MLINE-X1 + 05).               
214500     MOVE '  TOR - FIRE PHOTON TORPEDOS (FLDA=COURSE)'                    
214600                                 TO  MLINE (MLINE-X1 + 06).               
214700     MOVE '  SHE - SHIELD CONTROL (FLDA=NEW SHIELD LEVEL)'                
214800                                 TO  MLINE (MLINE-X1 + 07).               
214900     MOVE '  DAM - DAMAGE CONTROL REPORT'                                 
215000                                 TO  MLINE (MLINE-X1 + 08).               
215100     MOVE '  COM - LIBRARY-COMPUTER SUPPORT (FLDA..SEE BELOW)'            
215200                                 TO  MLINE (MLINE-X1 + 09).               
215300     MOVE '          0 - CUMULATIVE GALAXY SCAN'                          
215400                                 TO  MLINE (MLINE-X1 + 10).               
215500     MOVE '          1 - STATUS REPORT'                                   
215600                                 TO  MLINE (MLINE-X1 + 11).               
215700     MOVE '          2 - QUADRANT NOMENCLATURE MAP'                       
215800                                 TO  MLINE (MLINE-X1 + 12).               
215900     MOVE '  SUR - *** TO SURRENDER ***'                                  
216000                                 TO  MLINE (MLINE-X1 + 14).               
216100     MOVE '  RES - RESTORE THE LAST SAVED GAME'                           
216200                                 TO  MLINE (MLINE-X1 + 15).               
216300 V70-EXIT.  EXIT.                                                         
216400     SKIP1                                                                
216500 W10-GAME-BEGINS.                                                         
216600     MOVE L-MULT                 TO  RND-MULT.                            
216700***DFHIC TYPE=GETIME,                                            X        
216800*****    FORM=PACKED,                                            X        
216900*****    TIMADR=START-TIME                                                
217000     MOVE     'VGET' TO ISPF-SERVICE.                                     
217100     MOVE 01 TO ISPF-NAME-COUNT.                                          
217200     MOVE 'ZTIME   ' TO ISPF-NAME(1).                                     
217300     MOVE     'ASIS' TO ISPF-OPTION1.                                     
217400     CALL 'ISPLINK' USING  ISPF-SERVICE  ISPF-NAME-LIST                   
217500                           ISPF-OPTION1.                                  
217600     IF RETURN-CODE NOT = 0                                               
217700*                              8  - VARIABLE NOT FOUND                    
217800*                              16 - TRANSLATION ERROR OR DATA             
217900*                                   TRUNCATION                            
218000*                              20 - SEVERE ERROR                          
218100         DISPLAY 'VGET FOR ZTIME FAILED',                                 
218200         STOP RUN.                                                        
218300     MOVE CORR TIME-VAR-BO TO TIME-VAR-NUM.                               
218400     MOVE TIME-VAR-NUM TO SEED1A.                                         
218500     MOVE CORRESPONDING SEED1B TO SEED2B.                                 
218600     MOVE SEED2A TO RND-WORK.                                             
218700     PERFORM C40-GET-RND THRU C40-EXIT  +10 TIMES.                        
218800     COMPUTE FIRST-STARDATE = 100 * (21 + (4 * RND)).                     
218900     MOVE FIRST-STARDATE         TO  STARDATE.                            
219000     MOVE 'N'                    TO  DOCKING-FLAG.                        
219100     MOVE INIT-ENERGY-EPRISE     TO  ENERGY.                              
219200     MOVE INIT-TORPEDOS          TO  TORPEDOS.                            
219300     MOVE +0                     TO  SHIELD.                              
219400     MOVE L-NULL-TABLE           TO  SECTOR-AREA.                         
219500     PERFORM R10-RANDOM-HOLE THRU R10-EXIT.                               
219600     MOVE RANDOM-ROW             TO  EPRISE-GLXY-ROW.                     
219700     MOVE RANDOM-COL             TO  EPRISE-GLXY-COL.                     
219800     PERFORM R10-RANDOM-HOLE THRU R10-EXIT.                               
219900     MOVE RANDOM-ROW             TO  EPRISE-VERT.                         
220000     MOVE RANDOM-COL             TO  EPRISE-HORZ.                         
220100     MOVE +0  TO  DAM-NAV  DAM-SRS  DAM-LRS  DAM-PHA                      
220200                  DAM-TOR  DAM-SHE  DAM-DAM  DAM-COM.                     
220300     MOVE +0                     TO  KLING-TOTAL                          
220400                                     SBASE-TOTAL.                         
220500     PERFORM W70-INITIALIZE-GALAXY THRU W70-EXIT                          
220600         VARYING G-X1 FROM +1 BY +1                                       
220700           UNTIL G-X1 > +8                                                
220800         AFTER   G-X2 FROM +1 BY +1                                       
220900           UNTIL G-X2 > +8.                                               
221000     MOVE KLING-TOTAL                     TO  ORIG-KLING.                 
221100     PERFORM C40-GET-RND THRU C40-EXIT.                                   
221200     COMPUTE ENDDATE  ROUNDED =                                           
221300         FIRST-STARDATE + 20                                              
221400       + (ORIG-KLING * 0.5) + (RND * 2).                                  
221500     IF SBASE-TOTAL < +1                                                  
221600         PERFORM R10-RANDOM-HOLE THRU R10-EXIT                            
221700         SET G-X1                TO  RANDOM-ROW                           
221800         SET G-X2                TO  RANDOM-COL                           
221900         MOVE +1                 TO  G-SBASE (G-X1, G-X2)                 
222000                                     SBASE-TOTAL.                         
222100     MOVE SBASE-TOTAL                     TO  ORIG-SBASE.                 
222200     MOVE 'YOUR ORDERS ARE AS FOLLOWS:'                                   
222300                                 TO  MSG-LINE.                            
222400     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
222500     PERFORM C60-BLANK-MSG THRU C60-EXIT.                                 
222600     MOVE '    DESTROY THE    KLINGON WARSHIPS, WHICH HAVE'               
222700                                 TO  MSG-LINE.                            
222800     MOVE KLING-TOTAL TO TEMP-Z9.                                         
222900     MOVE TEMP-Z9                TO  MSG-LINE17-18.                       
223000     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
223100     MOVE '  INVADED THE GALAXY, BEFORE THEY CAN ATTACK THE'              
223200                                 TO  MSG-LINE.                            
223300     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
223400     MOVE '  FEDERATION HEADQUARTERS ON STARDATE     ;'                   
223500                                 TO  MSG-LINE.                            
223600     MOVE ENDDATE TO TEMP-ZZZ9.                                           
223700     MOVE TEMP-ZZZ9              TO  MSG-LINE39-42.                       
223800     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
223900     MOVE '  THIS GIVES YOU    STARDATES.'                                
224000                                 TO  MSG-LINE.                            
224100     COMPUTE TEMP-Z9 = ENDDATE - FIRST-STARDATE.                          
224200     MOVE TEMP-Z9                TO  MSG-LINE18-19.                       
224300     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
224400     MOVE '  THERE ARE    STARBASES IN THE GALAXY'                        
224500                                 TO  MSG-LINE.                            
224600     MOVE SBASE-TOTAL TO TEMP-Z9.                                         
224700     MOVE TEMP-Z9                TO  MSG-LINE13-14.                       
224800     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
224900     MOVE '  FOR RESUPPLYING YOUR SHIP.'                                  
225000                                 TO  MSG-LINE.                            
225100     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
225200     PERFORM S15-ENTER-NEW-QUADRANT THRU S15-EXIT.                        
225300     PERFORM C60-BLANK-MSG THRU C60-EXIT.                                 
225400     MOVE SPACES                 TO  CMDI.                                
225500     MOVE 'FOR DETAILED OPERATING INSTRUCTIONS, PRESS "PF1"'              
225600                                 TO  MSG-LINE.                            
225700     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
225800     MOVE '  OTHERWISE, ISSUE YOUR FIRST COMMAND.'                        
225900                                 TO  MSG-LINE.                            
226000     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
226100 W10-EXIT.  EXIT.                                                         
226200     SKIP1                                                                
226300 W70-INITIALIZE-GALAXY.                                                   
226400     PERFORM C40-GET-RND THRU C40-EXIT.                                   
226500     MOVE +0                     TO  G-KLING (G-X1, G-X2).                
226600     IF RND > 0.80                                                        
226700         ADD +1                  TO  G-KLING (G-X1, G-X2).                
226800     IF RND > 0.92                                                        
226900         ADD +1                  TO  G-KLING (G-X1, G-X2).                
227000     IF RND > 0.97                                                        
227100         ADD +1                  TO  G-KLING (G-X1, G-X2).                
227200     ADD G-KLING (G-X1, G-X2)    TO  KLING-TOTAL.                         
227300     PERFORM C40-GET-RND THRU C40-EXIT.                                   
227400     MOVE +0                     TO  G-SBASE (G-X1, G-X2).                
227500     IF RND > 0.968                                                       
227600         ADD +1                  TO  G-SBASE (G-X1, G-X2).                
227700     ADD G-SBASE (G-X1, G-X2)    TO  SBASE-TOTAL.                         
227800     PERFORM C40-GET-RND THRU C40-EXIT.                                   
227900     COMPUTE G-STARS (G-X1, G-X2) = 3 + (6.9 * RND).                      
228000     MOVE '0'                    TO  G-STATUS (G-X1, G-X2).               
228100 W70-EXIT.  EXIT.                                                         
228200     SKIP1                                                                
228300 X10-STAR-DESTROYED.                                                      
228400     MOVE L-NULL TO SECTOR-DISP (SECTOR-X1, SECTOR-X2).                   
228500     SET G-X1 TO EPRISE-GLXY-ROW.                                         
228600     SET G-X2 TO EPRISE-GLXY-COL.                                         
228700     SUBTRACT +1 FROM G-STARS (G-X1, G-X2).                               
228800     MOVE 'TORPEDO DESTROYED THE STAR AT X:X'                             
228900                                 TO  MSG-LINE.                            
229000     MOVE TOR-ROW              TO  MSG-LINE31.                            
229100     MOVE TOR-COL              TO  MSG-LINE33.                            
229200     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
229300     PERFORM C40-GET-RND THRU C40-EXIT.                                   
229400     IF RND > +0.20                                                       
229500         GO TO X10-EXIT.                                                  
229600     MOVE L-NULL-TABLE           TO  SECTOR-AREA.                         
229700     MOVE +3                     TO  WRAPUP-CODE.                         
229800     SUBTRACT CURR-KLING FROM KLING-TOTAL.                                
229900     MOVE '  UNFORTUNATELY, THE STAR WAS UNSTABLE AND BECAME'             
230000                                 TO  MSG-LINE.                            
230100     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
230200     MOVE '  A NOVA, VAPORIZING EVERYTHING IN THIS QUADRANT.'             
230300                                 TO  MSG-LINE.                            
230400     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
230500 X10-EXIT.  EXIT.                                                         
230600     SKIP1                                                                
230700 Z10-CLEAR-THE-SCREEN.                                                    
230800** DFHTC TYPE=(ERASE,WRITE,WAIT)                                          
230900 Z10-EXIT.  EXIT.                                                         
231000     SKIP1                                                                
231100 Z80-GAME-OVER.                                                           
231200     GO TO Z81, Z86, Z84, Z83, Z87, Z82                                   
231300         DEPENDING ON WRAPUP-CODE.                                        
231400     MOVE '*** PROGRAM ERROR (INVALID WRAPUP-CODE) ***'                   
231500                                 TO  MSG-LINE.                            
231600     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
231700     GO TO Z89-END-PROGRAM.                                               
231800 Z81.                                                                     
231900     MOVE '*** FATAL ERROR ***.  YOU JUST STRANDED YOUR SHIP'             
232000                                 TO  MSG-LINE.                            
232100     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
232200     MOVE '   IN SPACE!!  YOU HAVE INSUFFICIENT MANUEVERING'              
232300                                 TO  MSG-LINE.                            
232400     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
232500     MOVE '   ENERGY, AND SHIELD CONTROL IS PRESENTLY UNABLE'             
232600                                 TO  MSG-LINE.                            
232700     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
232800     MOVE '   TO CROSS-CIRCUIT SHIELD ENERGY TO THE ENGINES!!'            
232900                                 TO  MSG-LINE.                            
233000     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
233100     GO TO Z85.                                                           
233200 Z82.                                                                     
233300     MOVE 'THAT DOES IT, CAPTAIN!!  YOU ARE HEREBY RELIEVED'              
233400                                 TO  MSG-LINE.                            
233500     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
233600     MOVE '   OF COMMAND AND SENTENCED TO 99 STARDATES'                   
233700                                 TO  MSG-LINE.                            
233800     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
233900     MOVE '   AT HARD LABOR ON CYGNUS 12!!'                               
234000                                 TO  MSG-LINE.                            
234100     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
234200     GO TO Z86.                                                           
234300 Z83.                                                                     
234400     MOVE 'YOU HAVE FAILED TO COMPLETE YOUR MISSION IN TIME.'             
234500                                 TO  MSG-LINE.                            
234600     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
234700 Z84.                                                                     
234800     PERFORM C60-BLANK-MSG THRU C60-EXIT.                                 
234900     MOVE 'THE ENTERPRISE HAS BEEN DESTROYED.'                            
235000                                 TO  MSG-LINE.                            
235100     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
235200     MOVE 'THE FEDERATION WILL BE CONQUERED.'                             
235300                                 TO  MSG-LINE.                            
235400     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
235500 Z85.                                                                     
235600     PERFORM C60-BLANK-MSG THRU C60-EXIT.                                 
235700     MOVE 'IT IS STARDATE'                                                
235800                                 TO  MSG-LINE.                            
235900     MOVE STARDATE TO TEMP-ZZZ9.                                          
236000     MOVE TEMP-ZZZ9              TO  MSG-LINE16-19.                       
236100     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
236200 Z86.                                                                     
236300     PERFORM C60-BLANK-MSG THRU C60-EXIT.                                 
236400     MOVE 'THERE WERE    KLINGON BATTLE CRUISERS LEFT'                    
236500                                 TO  MSG-LINE.                            
236600     MOVE KLING-TOTAL TO TEMP-Z9.                                         
236700     MOVE TEMP-Z9                TO  MSG-LINE12-13.                       
236800     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
236900     MOVE '   AT THE END OF YOUR MISSION.'                                
237000                                 TO  MSG-LINE.                            
237100     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
237200     GO TO Z89-END-PROGRAM.                                               
237300 Z87.                                                                     
237400     PERFORM C60-BLANK-MSG THRU C60-EXIT.                                 
237500     MOVE 'CONGRATULATIONS, CAPTAIN!!   THE LAST KLINGON'                 
237600                                 TO  MSG-LINE.                            
237700     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
237800     MOVE 'BATTLE CRUSIER MENACING THE FEDERATION'                        
237900                                 TO  MSG-LINE.                            
238000     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
238100     MOVE 'HAS BEEN DESTROYED, WITH ZZVZ STARDATES TO SPARE.'             
238200                                 TO  MSG-LINE.                            
238300     COMPUTE TEMP-ZZVZ = ENDDATE - STARDATE.                              
238400     MOVE TEMP-ZZVZ              TO  MSG-LINE26-29.                       
238500     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
238600     PERFORM C60-BLANK-MSG THRU C60-EXIT.                                 
238700     MOVE 'YOUR EFFICIENCY RATING IS      ON THIS MISSION.'               
238800                                 TO  MSG-LINE.                            
238900     COMPUTE TEMP-ZZZ9 ROUNDED =                                          
239000        ((ORIG-KLING * 1000) / (STARDATE - FIRST-STARDATE)).              
239100     MOVE TEMP-ZZZ9              TO  MSG-LINE27-30.                       
239200     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
239300     MOVE 'YOU USED ZZ.9 OF YOUR ZZ.9 STARDATES TO DESTROY'               
239400                                 TO  MSG-LINE.                            
239500     COMPUTE TEMP-ZZVZ = STARDATE - FIRST-STARDATE.                       
239600     MOVE TEMP-ZZVZ              TO  MSG-LINE10-13.                       
239700     COMPUTE TEMP-ZZVZ = ENDDATE - FIRST-STARDATE.                        
239800     MOVE TEMP-ZZVZ              TO  MSG-LINE23-26.                       
239900     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
240000     MOVE 'THE Z9 KLINGONS, WITH THE SUPPORT OF Z9 STARBASES.'            
240100                                 TO  MSG-LINE.                            
240200     MOVE ORIG-KLING TO TEMP-Z9.                                          
240300     MOVE TEMP-Z9                TO  MSG-LINE05-06.                       
240400     MOVE ORIG-SBASE TO TEMP-Z9.                                          
240500     MOVE TEMP-Z9                TO  MSG-LINE38-39.                       
240600     PERFORM C50-NEXT-MSG THRU C50-EXIT.                                  
240700 Z89-END-PROGRAM.                                                         
240800 Z80-EXIT.  EXIT.                                                         
240900 Z99-ABEND.                                                               
241000     STOP RUN.                                                            
  