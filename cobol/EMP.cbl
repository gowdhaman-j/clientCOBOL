000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID.    EMPEROR.                                                  
000300 AUTHOR. R THORNTON                                                       
000400*REMARKS. EMPEROR GAME TRANSLITERATED FROM PL/I. THE TGET, TPUT,          
000500*         AND RAND SUBROUTINE CALLS WERE CONVERTED TO USE IN-LINE         
000600*         PERFORMED ROUTINES RATHER THAN CALLS TO EXTERNAL                
000700*         ASSEMBLER LANGUAGE ROUTINES. IN ADDITION, THE USE OF            
000800*         TGET AND TPUT WAS CHANGED TO SYSIN READS AND SYSPRINT           
000900*         WRITES. OTHER CHANGES WERE MADE AS NECESSARY TO FIT             
001000*         THE COBOL LANGUAGE.                                             
001100 ENVIRONMENT DIVISION.                                                    
001200 CONFIGURATION SECTION.                                                   
001300 INPUT-OUTPUT SECTION.                                                    
001400 FILE-CONTROL.                                                            
001500     SELECT INPUT-FILE ASSIGN TO SYSIN.                                   
001600     SELECT PRINT-FILE ASSIGN TO SYSPRINT.                                
001700 DATA DIVISION.                                                           
001800 FILE SECTION.                                                            
001900*                                                                         
002000 FD  INPUT-FILE                                                           
002100     BLOCK CONTAINS 0 RECORDS                                             
002200     RECORD CONTAINS 80 CHARACTERS                                        
002300     RECORDING MODE IS F                                                  
002400     LABEL RECORDS ARE STANDARD.                                          
002500 01  INPUT-RECORD.                                                        
002600     05 INPUT-BYTE            PIC X OCCURS 80 INDEXED BY IX, IY.          
002700*                                                                         
002800 FD  PRINT-FILE                                                           
002900     BLOCK CONTAINS 0 RECORDS                                             
003000     RECORD CONTAINS 80 CHARACTERS                                        
003100     RECORDING MODE IS F                                                  
003200     LABEL RECORDS ARE STANDARD.                                          
003300 01  PRINT-RECORD             PIC X(80).                                  
003310 01  PRINT-REDEFINITION.                                                  
003320     05  PRINT-BYTE           PIC X OCCURS 80                             
003330                              INDEXED BY PX, PY.                          
003400*                                                                         
003500 WORKING-STORAGE SECTION.                                                 
003600 01  MISCELLANEOUS-FIELDS.                                                
003700     05  INPUT-NUMERIC-SWITCH PIC X VALUE 'N'.                            
003800         88  INPUT-IS-NOT-NUMERIC VALUE 'N'.                              
003900         88  INPUT-IS-NUMERIC     VALUE 'Y'.                              
003910     05  LAST-BYTE-MOVED-SWITCH PIC X VALUE 'N'.                          
003920         88  LAST-BYTE-MOVED-WAS-BLANK VALUE 'Y'.                         
003930         88  LAST-BYTE-MOVED-WAS-SOLID VALUE 'N'.                         
004000     05  L                    PIC X.                                      
004100     05  IN-NUMBER            PIC 9(15).                                  
004200     05  IN-DIGIT             REDEFINES IN-NUMBER                         
004210                              PIC S9 OCCURS 15 INDEXED BY IZ.             
004300     05  PEOPLE               PIC S9(5) COMP-3 VALUE +0.                  
004400     05  Y-ARS                PIC S9(3) COMP-3 VALUE +0.                  
004500     05  IMMIGRANTS           PIC S9(5) COMP-3 VALUE +0.                  
004600     05  GRAIN                PIC S9(9) COMP-3 VALUE +0.                  
004700     05  ACRES                PIC S9(9) COMP-3 VALUE +0.                  
004800     05  RATS                 PIC S9(9) COMP-3 VALUE +0.                  
004900     05  FARMLAND             PIC S9(9) COMP-3 VALUE +0.                  
005000     05  YIELD                PIC S9(4)V9 COMP-3 VALUE +0.                
005100     05  FERT-AMT             PIC S9(4)V9 COMP-3 VALUE +0.                
005200     05  FOOD                 PIC S9(9) COMP-3 VALUE +0.                  
005300     05  STARVED              PIC S9(5) COMP-3 VALUE +0.                  
005400     05  PLAGUE               PIC S9(5) COMP-3 VALUE +0.                  
005500     05  R                    PIC S9(9) COMP-3 VALUE +0.                  
005600     05  X                    PIC S9(5) COMP-3 VALUE +0.                  
005700     05  FIRST-TIME-SWITCH    PIC X VALUE 'Y'.                            
005800         88 FIRST-TIME        VALUE 'Y'.                                  
005900     05  FERT-SW              PIC X VALUE 'N'.                            
005910         88  FERTILIZER-WANTED VALUE 'Y'.                                 
005920         88  NO-FERTILIZER-WANTED VALUE 'N'.                              
006000     05  PLAG-SW              PIC X VALUE 'N'.                            
006010         88  NO-PLAGUE-THREAT VALUE 'N'.                                  
006020         88  PLAGUE-THREAT    VALUE 'Y'.                                  
006100     05  BIN-NUMBER           PIC S9(9) COMP-3 VALUE +0.                  
006200     05  NBR-RAND             PIC S9(6) COMP-3.                           
006300     05  RAND-NBR REDEFINES NBR-RAND PIC SV9(6) COMP-3.                   
006400     05  WORK-TIME            PIC 9(8).                                   
006500     05  FILLER REDEFINES WORK-TIME.                                      
006600         10  HOUR-TENS        PIC 9.                                      
006700         10  HOUR-UNITS       PIC 9.                                      
006800         10  MIN-TENS         PIC 9.                                      
006900         10  MIN-UNITS        PIC 9.                                      
007000         10  SEC-TENS         PIC 9.                                      
007100         10  SEC-UNITS        PIC 9.                                      
007200         10  SEC-TENTHS       PIC 9.                                      
007300         10  SEC-HUNDREDTHS   PIC 9.                                      
007400     05  WORK-RAND            PIC 9(6).                                   
007500     05  FILLER REDEFINES WORK-RAND.                                      
007600         10  RAND-1           PIC 9.                                      
007700         10  RAND-2           PIC 9.                                      
007800         10  RAND-3           PIC 9.                                      
007900         10  RAND-4           PIC 9.                                      
008000         10  RAND-5           PIC 9.                                      
008100         10  RAND-6           PIC 9.                                      
008200     05  DIFF                 PIC S9(9) COMP-3 VALUE +0.                  
008300     05  COMPUTED-VALUE       PIC S9(5) COMP-3 VALUE +0.                  
008400     05  TRADE                PIC S9(9) COMP-3 VALUE +0.                  
008500     05  SERUM                PIC S9(5) COMP-3 VALUE +0.                  
008600     05  GAMES-TABLE.                                                     
008700         10  FILLER           PIC X(80)                                   
008800                              VALUE 'PING PONG FOR A YEAR'.               
008900         10  FILLER           PIC X(80)                                   
009000                              VALUE 'BRIDGE FOR A YEAR'.                  
009100         10  FILLER           PIC X(80)                                   
009200                          VALUE 'LITTLE SALLY SAUCER FOR A YEAR'.         
009300         10  FILLER           PIC X(80)                                   
009400                              VALUE 'MONOPLY FOR A YEAR'.                 
009500         10  FILLER           PIC X(80)                                   
009600                              VALUE 'MARBLES FOR A YEAR'.                 
009700         10  FILLER           PIC X(80)                                   
009800                              VALUE 'SPIN THE BOTTLE FOR A YEAR'.         
009900         10  FILLER           PIC X(80)                                   
010000                              VALUE 'PINOCHLE FOR A YEAR'.                
010100         10  FILLER           PIC X(80)                                   
010200                              VALUE 'LEAP FROG FOR A YEAR'.               
010300         10  FILLER           PIC X(80)                                   
010400                              VALUE 'STRIP POKER FOR A YEAR'.             
010500         10  FILLER           PIC X(80)                                   
010600                              VALUE 'RUN-SHEEP-RUN FOR A YEAR'.           
010700     05  GAMES                REDEFINES GAMES-TABLE                       
010800                              PIC X(80) OCCURS 10.                        
010900     05  INNOCULATED          PIC S9(9) COMP-3 VALUE +0.                  
011000     05  TOT-IMM              PIC S9(9) COMP-3 VALUE +0.                  
011100     05  TOT-STARVED          PIC S9(9) COMP-3 VALUE +0.                  
011200     05  TOT-PLAGUED          PIC S9(9) COMP-3 VALUE +0.                  
011300     05  ACRES-BOT            PIC S9(9) COMP-3 VALUE +0.                  
011400     05  ACRES-SOLD           PIC S9(9) COMP-3 VALUE +0.                  
011500     05  ACRES-START          PIC S9(9) COMP-3 VALUE +0.                  
011600     05  PEOPL-START          PIC S9(7) COMP-3 VALUE +0.                  
011700     05  TOT-FERT             PIC S9(7) COMP-3 VALUE +0.                  
011800     05  PLANTED              PIC S9(9) COMP-3 VALUE +0.                  
011900     05  HARVESTD             PIC S9(9) COMP-3 VALUE +0.                  
012000     05  GRAIN-PLANT          PIC S9(7) COMP-3 VALUE +0.                  
012100     05  HARVEST-LST          PIC S9(7) COMP-3 VALUE +0.                  
012200     05  GRAIN-BUY            PIC S9(9) COMP-3 VALUE +0.                  
012300     05  GRAIN-SOLD           PIC S9(9) COMP-3 VALUE +0.                  
012400     05  GRAIN-START          PIC S9(5) COMP-3 VALUE +0.                  
012500     05  LAST-SHOT            PIC S9(5) COMP-3 VALUE +0.                  
012600     05  TOTL-SHOT            PIC S9(7) COMP-3 VALUE +0.                  
012700     05  ROBBED               PIC S9(5) COMP-3 VALUE +0.                  
012800     05  THIS-SHOT            PIC S9(5) COMP-3 VALUE +0.                  
012900 01  PRINT-FIELDS.                                                        
013000     05  HDR1                 PIC X(34) VALUE                             
013100                            'F I S  ECONOMETRIC MODELING SYSTEM'.         
013200     05  HDR2                 PIC X(34) VALUE                             
013300                            '=================================='.         
013400     05  HDR3                 PIC X(33) VALUE                             
013500                            'DO YOU NEED INSTRUCTIONS? Y OR N?'.          
013600     05  HDR4                 PIC X(37) VALUE                             
013700                         'YOU ARE THE EMPEROR OF "SOMEWHEREIA"!'.         
013800     05  HDR5                 PIC X(41) VALUE                             
013900                     'WHO WILL REIGN FOR A MAXIMUM OF 25 YEARS,'.         
014000     05  HDR6                 PIC X(42) VALUE                             
014100                   'AND WILL BE GIVEN PEOPLE, LAND, AND GRAIN.'.          
014200     05  HDR7                 PIC X(36) VALUE                             
014300                          'YOUR GOAL IS TO INCREASE YOUR EMPIRE'.         
014400     05  HDR8                 PIC X(36) VALUE                             
014500                          'BY HAVING YOUR PEOPLE FARM YOUR LAND'.         
014600     05  HDR9                 PIC X(42) VALUE                             
014700                    'TO PRODUCE THE GRAIN FOR YOUR USE AS FOOD,'.         
014800     05  HDR10                PIC X(38) VALUE                             
014900                        'SEED FOR PLANTING, AND THE TRADE BASIS'.         
015000     05  HDR11                PIC X(36) VALUE                             
015100                          'THAT MAY BE USED TO BUY MORE LAND TO'.         
015200     05  HDR12                PIC X(40) VALUE                             
015300                      'PRODUCE MORE GRAIN TO BUY MORE LAND, ETC'.         
015400     05  HDR13                PIC X(40) VALUE                             
015500                      'SIMPLE, RIGHT? THEN LETS GET STARTED....'.         
015600     05  STATUS-DASHES        PIC X(35) VALUE                             
015700                           '-----------------------------------'.         
015800     05  STATUS-1             PIC X(35) VALUE                             
015900                           'STATE OF YOUR EMPIRE AFTER ONE YEAR'.         
016000     05  STATUS-2.                                                        
016100         10  FILLER           PIC X(26) VALUE                             
016200                                    'STATE OF YOUR EMPIRE AFTER'.         
016300         10  STATUS-2-YARS    PIC ZZ9.                                    
016400         10  FILLER           PIC X(6) VALUE ' YEARS'.                    
016500     05  STATUS-3.                                                        
016600         10  FILLER           PIC X(11) VALUE 'THERE WERE '.              
016700         10  STATUS-3-IMMIGRANTS  PIC ZZ,ZZ9.                             
016800         10  FILLER           PIC X(11) VALUE ' IMMIGRANTS'.              
016900     05  STATUS-4.                                                        
017000         10  STATUS-4-STARVED PIC ZZ,ZZ9.                                 
017100         10  FILLER           PIC X(25) VALUE                             
017200                                     ' PEOPLE STARVED TO DEATH.'.         
017300     05  STATUS-5             PIC X(33) VALUE                             
017400                             'BUT THE WORST IS YET TO COME. . .'.         
017500     05  STATUS-6             PIC X(45) VALUE                             
017600                 'THE PLAGUE WIPED OUT YOUR ENTIRE POPULATION!!'.         
017700     05  STATUS-7             PIC X(25) VALUE                             
017800                                     'BETTER FORTUNE NEXT TIME!'.         
017900     05  STATUS-8.                                                        
018000         10  STATUS-8-PLAGUE  PIC ZZ,ZZ9.                                 
018010         10  FILLER           PIC X(34) VALUE                             
018020                            ' PEOPLE WERE KILLED BY THE PLAGUE.'.         
018100     05  STATUS-9.                                                        
018200         10  FILLER           PIC X(38) VALUE                             
018300                        'THEREFORE, YOUR CURRENT POPULATION IS '.         
018400         10  STATUS-9-PEOPLE  PIC ZZ,ZZ9.                                 
018500                  10  FILLER           PIC X(8) VALUE ' PEOPLE,'.         
018600     05  STATUS-10.                                                       
018700         10  FILLER           PIC X(27) VALUE                             
018800                                   'YOUR CURRENT POPULATION IS '.         
018900         10  STATUS-10-PEOPLE PIC ZZ,ZZ9.                                 
019000         10  FILLER           PIC X(8) VALUE ' PEOPLE,'.                  
019100     05  STATUS-11.                                                       
019200         10  FILLER           PIC X(11) VALUE 'AND YOU OWN'.              
019300         10  STATUS-11-ACRES  PIC ZZZ,ZZZ,ZZ9.                            
019400         10  FILLER           PIC X(19) VALUE                             
019500                                          ' ACRES, AND CONTROL'.          
019600         10  STATUS-11-GRAIN  PIC ZZZ,ZZZ,ZZ9.                            
019700         10  FILLER           PIC X(18) VALUE                             
019800                                           ' BUSHELS OF GRAIN.'.          
019900     05  STATUS-12            PIC X(47) VALUE                             
020000               'YOUR SITUATION IS HOPELESS. THE SOUNDS YOU HEAR'.         
020100     05  STATUS-13            PIC X(47) VALUE                             
020200               'ARE THE SHOUTED CURSES AS THE PEOPLE LEAVE YOUR'.         
020300     05  STATUS-14            PIC X(16) VALUE 'STINKING EMPIRE!'.         
020400     05  STATUS-15.                                                       
020500         10  FILLER           PIC X(8) VALUE 'FARMING '.                  
020600         10  STATUS-15-ACRES  PIC ZZZ,ZZZ,ZZ9.                            
020700         10  FILLER           PIC X(20) VALUE                             
020800                                          ' ACRES, AND STORING '.         
020900         10  STATUS-15-GRAIN  PIC ZZZ,ZZZ,ZZ9.                            
021000         10  FILLER           PIC X(9) VALUE ' BUSHELS'.                  
021100     05  STATUS-16.                                                       
021200         10  FILLER           PIC X(9) VALUE 'YOU HAVE '.                 
021300         10  STATUS-16-ACRES  PIC ZZZ,ZZZ,ZZ9.                            
021400         10  FILLER           PIC X(15) VALUE ' ACRES OF LAND'.           
021500     05  STATUS-17.                                                       
021600         10  FILLER           PIC X(25) VALUE                             
021700                                     'ALL YOUR LAND WOULD BRING'.         
021800         10  STATUS-17-GRAIN  PIC ZZZ,ZZZ,ZZ9.                            
021900         10  FILLER           PIC X(18) VALUE                             
022000                                            ' BUSHELS OF GRAIN.'.         
022100     05  STATUS-18.                                                       
022200         10  FILLER           PIC X(9) VALUE 'YOU HAVE '.                 
022300         10  STATUS-18-GRAIN  PIC ZZZ,ZZZ,ZZ9.                            
022400         10  FILLER           PIC X(30) VALUE                             
022500                               ' BUSHELS OF GRAIN IN STORAGE, '.          
022600     05  STATUS-19.                                                       
022700         10  FILLER           PIC X(5) VALUE ' AND '.                     
022800         10  STATUS-19-PEOPLE PIC ZZ,ZZ9.                                 
022900         10  FILLER           PIC X(16) VALUE                             
023000                                            ' PEOPLE TO FEED.'.           
023100     05  STATUS-20.                                                       
023200         10  FILLER           PIC X(12) VALUE 'THAT LEAVES '.             
023300         10  STATUS-20-GRAIN  PIC ZZZ,ZZZ,ZZ9.                            
023400         10  FILLER           PIC X(12) VALUE ' IN STORAGE.'.             
023500     05  STATUS-21.                                                       
023600         10  FILLER           PIC X(9) VALUE 'YOU HAVE '.                 
023700         10  STATUS-21-ACRES  PIC ZZZ,ZZZ,ZZ9.                            
023800         10  FILLER           PIC X(19) VALUE                             
023900                                           ' ACRES OF LAND, AND'.         
024000         10  STATUS-21-PEOPLE PIC ZZ,ZZ9.                                 
024100         10  FILLER           PIC X(19) VALUE                             
024200                                           ' PEOPLE AS FARMERS.'.         
024300     05  CONTINUE-1           PIC X(32) VALUE                             
024400                              'DO YOU WANT TO CONTINUE? Y OR N?'.         
024500     05  SO-LONG-1            PIC X(28) VALUE                             
024600                                  'SO LONG, SUCCESSFUL. . . . .'.         
024700     05  GRAIN-1.                                                         
024800         10  FILLER           PIC X(9) VALUE 'YOU HAVE '.                 
024900         10  GRAIN-1-GRAIN    PIC ZZZ,ZZZ,ZZ9.                            
025000         10  FILLER           PIC X(29) VALUE                             
025100                                 ' BUSHELS OF GRAIN IN STORAGE.'.         
025200     05  LAND-1.                                                          
025300         10  FILLER           PIC X(19) VALUE                             
025400                                           'LAND IS TRADING FOR'.         
025500         10  LAND-1-VALUE     PIC ZZ,ZZ9.                                 
025600         10  FILLER           PIC X(27) VALUE                             
025700                                   ' BUSHELS OF GRAIN PER ACRE.'.         
025800     05  LAND-2               PIC X(45) VALUE                             
025900                 'UNFORTUNATELY, YOU ARE IN NO POSITION TO BUY!'.         
026000     05  BUY-1.                                                           
026100         10  FILLER                 PIC X(27) VALUE                       
026200                                  'THE MAXIMUM YOU MAY BUY IS '.          
026300         10  BUY-1-ACRES            PIC ZZZ,ZZZ,ZZ9.                      
026400         10  FILLER                 PIC X(7) VALUE ' ACRES.'.             
026500     05  BUY-2                      PIC X(37) VALUE                       
026600                        'HOW MANY ACRES WOULD YOU LIKE TO BUY?'.          
026700     05  BUY-3                PIC X(28) VALUE                             
026800                                 'SORRY, I DONT BUY ON MARGIN.'.          
026900     05  BUY-4.                                                           
027000         10  FILLER           PIC X(30) VALUE                             
027100                               'I WILL BUY AS MUCH AS YOU OWN,'.          
027200         10  FILLER           PIC X(24) VALUE                             
027300                                     ' BUT NO MORE THAN THAT!!'.          
027400     05  NUMBERS-ONLY         PIC X(22) VALUE                             
027500                                        'NUMERICS ONLY, PLEASE.'.         
027600     05  NICE-TRY             PIC X(29) VALUE                             
027700                                'NICE TRY THERE, EMPEROR BABY.'.          
027800     05  TRADING-1.                                                       
027900         10  FILLER           PIC X(24) VALUE                             
028000                                     'YOUR LAND TRADING LEAVES'.          
028100         10  TRADING-1-GRAIN  PIC ZZZ,ZZZ,ZZ9.                            
028200         10  FILLER           PIC X(18) VALUE                             
028300                                           ' BUSHELS OF GRAIN.'.          
028400     05  HOW-MANY-SELL        PIC X(38) VALUE                             
028500                        'HOW MANY ACRES WOULD YOU LIKE TO SELL?'.         
028600     05  GROWL-1.                                                         
028700         10  FILLER           PIC X(39) VALUE                             
028800                       'THAT SOUND YOU HEAR IS THE GROWLING OF '.         
028900         10  GROWL-1-PEOPLE   PIC ZZ,ZZ9.                                 
029000         10  FILLER           PIC X(17) VALUE                             
029100                                             ' HUNGRY STOMACHS!'.         
029200     05  NO-GRAIN             PIC X(41) VALUE                             
029300                     'YOU HAVE NO GRAIN TO SET ASIDE FOR FOOD!!'.         
029400     05  HOW-MANY-GRAIN.                                                  
029500         10  FILLER           PIC X(35) VALUE                             
029600                           'HOW MANY BUSHELS OF GRAIN SHOULD BE'.         
029700         10  FILLER           PIC X(20) VALUE                             
029800                                          ' SET ASIDE FOR FOOD?'.         
029900     05  NOT-ENOUGH-GRAIN     PIC X(48) VALUE                             
030000             'YOU DONT HAVE ENOUGH GRAIN TO BUY THAT MUCH LAND'.          
030100     05  INSULTED             PIC X(51) VALUE                             
030200           'YOU CANT SET ASIDE MORE GRAIN THAN YOU HAVE, DUMMY!'.         
030300     05  RECANTED             PIC X(43) VALUE                             
030400                   'I MEAN YOUR EMPORERSHIP (HE MUMBLED HUMBLY)'.         
030500     05  GENOCIDE             PIC X(26) VALUE                             
030600                                    'THATS GENOCIDE, YOU SWINE!'.         
030700     05  APOCALYPSE           PIC X(48) VALUE                             
030800              'BEWARE OF THE FOUR HORSEMEN OF THE APOCALYPSE...'.         
030900     05  NO-LAND-1            PIC X(25) VALUE                             
031000                                    'YOU HAVE NO LAND TO SELL!'.          
031100     05  NO-LAND-2            PIC X(39) VALUE                             
031200                       'YOU HAVE NO FARMLAND! SORRY ABOUT THAT!'.         
031300     05  NO-SEED              PIC X(30) VALUE                             
031400                                'BUT YOU HAVE NO SEED TO PLANT!'.         
031500     05  HOW-MANY-ACRES       PIC X(35) VALUE                             
031600                           'HOW MANY ACRES DO YOU WANT TO FARM?'.         
031700     05  DONT-UNDERSTAND      PIC X(50) VALUE                             
031800            'I DONT THINK YOU QUITE UNDERSTAND, EMPEROR, SIR...'.         
031900     05  CANT-FARM            PIC X(39) VALUE                             
032000                       'YOU CANNOT FARM MORE LAND THAN YOU OWN!'.         
032100     05  NOT-ENOUGH-PEOPLE    PIC X(51) VALUE                             
032200           'YOU DONT HAVE ENOUGH PEOPLE TO FARM THAT MUCH LAND!'.         
032300     05  NOT-ENOUGH-SEED      PIC X(49) VALUE                             
032400             'YOU DONT HAVE ENOUGH SEED TO FARM THAT MUCH LAND!'.         
032500     05  FERTILIZER-1.                                                    
032600         10  FILLER           PIC X(26) VALUE                             
032700                                    'FERTILIZER IS SELLING FOR '.         
032800         10  FERTILIZER-1-PRICE PIC Z,ZZ9.9.                              
032900         10  FILLER           PIC X(14) VALUE ' BUSHELS/ACRE.'.           
033000     05  WISH-TO-ENHANCE      PIC X(47) VALUE                             
033100               'DO YOU WISH TO ENHANCE YOUR CROP YIELD? Y OR N?'.         
033200     05  NO-CREDIT            PIC X(51) VALUE                             
033300          'NICE TRY EMP, BUT I DONT SELL THE MANURE ON CREDIT!'.          
033400     05  GRAIN-ROT            PIC X(47) VALUE                             
033500              'A RARE GRAIN ROT ATTACKS YOUR GRAIN IN STORAGE!'.          
033600     05  ONLY-SAVED-1.                                                    
033700         10  FILLER           PIC X(5) VALUE 'ONLY '.                     
033800         10  ONLY-SAVED-1-GRAIN PIC ZZZ,ZZZ,ZZ9.                          
033900         10  FILLER           PIC X(24) VALUE                             
034000                                     ' BUSHELS COULD BE SAVED.'.          
034100     05  NO-SHOTS-NEEDED      PIC X(47) VALUE                             
034200              'PLAGUE INNOCULATIONS ARE UNNECESSARY THIS YEAR,'.          
034300     05  IMMUNIZED            PIC X(35) VALUE                             
034400                          'ALL YOUR EMPIRE IS STILL IMMUNIZED!'.          
034500     05  DISTRIBUTING-1       PIC X(45) VALUE                             
034600                 'U.N. WORLD HEALTH IS DISTRIBUTING BLACK DEATH'.         
034700     05  DISTRIBUTING-2.                                                  
034800         10  FILLER           PIC X(17) VALUE                             
034900                                             'INNOCULATIONS AT '.         
035000         10  DISTRIBUTING-2-SERUM PIC ZZ,ZZ9.                             
035100         10  FILLER           PIC X(28) VALUE                             
035200                                  ' BUSHELS OF GRAIN PER PERSON'.         
035300     05  CANT-AFFORD-SHOTS-1  PIC X(47) VALUE                             
035400               'UNFORTUNATELY, YOU CANT AFFORD IT....TSK....TSK'.         
035500     05  CAN-AFFORD-SHOTS-1.                                              
035600         10  FILLER           PIC X(29) VALUE                             
035700                                  'YOU CAN AFFORD TO INNOCULATE'.         
035800         10  CAN-AFFORD-SHOTS-1-PEOPLE PIC ZZ,ZZ9.                        
035900         10  FILLER           PIC X(8) VALUE ' PEOPLE.'.                  
036000     05  STILL-IMMUNIZED-1.                                               
036100         10  FILLER           PIC X(9) VALUE 'HOWEVER, '.                 
036200         10  STILL-IMMUNIZED-1-LAST PIC ZZ,ZZ9.                           
036300         10  FILLER           PIC X(36) VALUE                             
036400                          ' OF YOUR PEOPLE ARE STILL IMMUNIZED,'.         
036500     05  STILL-IMMUNIZED-2.                                               
036600         10  FILLER           PIC X(18) VALUE                             
036700                                            'AND YOU NEED ONLY '.         
036800         10  STILL-IMMUNIZED-2-REST PIC ZZ,ZZ9.                           
036900         10  FILLER           PIC X(12) VALUE ' MORE SHOTS.'.             
037000     05  HOW-MANY-GET         PIC X(22) VALUE                             
037100                                        'HOW MANY GET THE SHOT?'.         
037200     05  EASY-TO-GAMBLE       PIC X(52) VALUE                             
037300         'ITS EASY TO GAMBLE WITH SOMEONE ELSES LIFE, ISNT IT?'.          
037400     05  GRAIN-LEFT-1.                                                    
037500         10  FILLER           PIC X(12) VALUE 'THAT LEAVES '.             
037600         10  GRAIN-LEFT-1-GRAIN PIC ZZZ,ZZZ,ZZ9.                          
037700         10  FILLER           PIC X(16) VALUE                             
037800                                              ' BUSHELS STORED.'.         
037900     05  GRAIN-LEFT-2.                                                    
038000         10  FILLER           PIC X(13) VALUE 'LEAVING ONLY '.            
038100         10  GRAIN-LEFT-2-GRAIN PIC ZZZ,ZZZ,ZZ9.                          
038200         10  FILLER           PIC X(25) VALUE                             
038300                                    ' BUSHELS OF GRAIN STORED.'.          
038400     05  NOTHING-FARMED.                                                  
038500         10  FILLER           PIC X(45) VALUE                             
038600                 'WITH NO LAND BEING FARMED, THE PEOPLE PLAYED '.         
038700         10  NOTHING-FARMED-GAME PIC X(16).                               
038800     05  RATS-OVERRAN         PIC X(39) VALUE                             
038900                       'WHILE THE RATS OVERRAN YOUR GRAINERIES.'.         
039000     05  RATS-ATE-CATS        PIC X(48) VALUE                             
039100              'WITH NOTHING LEFT TO EAT, THE RATS ATE THE CATS!'.         
039200     05  WORKING-AS-TOLD      PIC X(56) VALUE                             
039300     'THE PEOPLE ARE WORKING UNDER YOUR PRESCRIBED CONDITIONS.'.          
039400     05  REVOLTED             PIC X(52) VALUE                             
039500         'A REVOLT OF THE PEASANTRY HAS OVERTHROWN YOUR REIGN!'.          
039600     05  LUCKY-TO-ESCAPE      PIC X(45) VALUE                             
039700                'YOU WERE LUCKY TO ESCAPE WITH YOUR LIFE . . .'.          
039800     05  HERE-ARE-FRUITS      PIC X(41) VALUE                             
039900                    'HERE ARE THE FRUITS OF THEIR LABOR. . . .'.          
040000     05  OVER-FERTILIZED      PIC X(49) VALUE                             
040100             'YOUR OVER-FERTILIZED LAND CAUSES A CROP BURN-OUT!'.         
040200     05  UNABLE-TO-SAVE       PIC X(41) VALUE                             
040300                     'YOU WERE UNABLE TO SAVE ANY OF YOUR CROP.'.         
040400     05  YIELD-PER-ACRE.                                                  
040500         10  FILLER           PIC X(24) VALUE                             
040600                                      'YOUR YIELD PER ACRE WAS '.         
040700         10  YIELD-PER-ACRE-YIELD PIC ZZ,ZZ9.                             
040800         10  FILLER           PIC X(9) VALUE ' BUSHELS.'.                 
040900     05  TOTAL-HARVEST.                                                   
041000         10  FILLER           PIC X(23) VALUE                             
041100                                        'FOR A TOTAL HARVEST OF'.         
041200         10  TOTAL-HARVEST-BUSHELS PIC ZZZ,ZZZ,ZZ9.                       
041300         10  FILLER           PIC X(9) VALUE ' BUSHELS.'.                 
041400     05  LOCUSTS-ATE          PIC X(43) VALUE                             
041500                  'A SWARM OF LOCUSTS DEVASTATED YOUR HARVEST!'.          
041600     05  RATS-ATE-1           PIC X(38) VALUE                             
041700                       'BUT THE RATS ATE IT ALL. (THOSE PIGS)!'.          
041800     05  RATS-ATE-2.                                                      
041900         10  FILLER           PIC X(17) VALUE                             
042000                                            'BUT THE RATS ATE '.          
042100         10  RATS-ATE-2-BUSHELS PIC ZZZ,ZZZ,ZZ9.                          
042200         10  FILLER           PIC X(9) VALUE ' BUSHELS.'.                 
042300     05  PEASANTS-STEALING    PIC X(53) VALUE                             
042400        'YOUR UNDERFED PEASANTS ARE PILFERING YOUR GRAINERIES.'.          
042500     05  ATE-SHOES            PIC X(51) VALUE                             
042600          'WITH NO FOOD AVAILABLE, THE PEOPLE ATE THEIR SHOES.'.          
042700     05  STOLE-GRAIN.                                                     
042800         10  FILLER           PIC X(11) VALUE 'THEY STOLE '.              
042900         10  STOLE-GRAIN-BUSHELS PIC ZZZ,ZZZ,ZZ9.                         
043000         10  FILLER           PIC X(18) VALUE                             
043100                                           ' BUSHELS FROM YOU!'.          
043200     05  FINAL-1              PIC X(53) VALUE                             
043300        'SINCE YOU NOW HAVE MORE THAN 1,000,000 ACRES OF LAND,'.          
043400     05  FINAL-2              PIC X(47) VALUE                             
043500              'AND/OR MORE THAN 4,000,000 BUSHELS OF GRAIN . .'.          
043600     05  FINAL-3              PIC X(41) VALUE                             
043700                     'YOU FAR SURPASSED ALL ROYAL EXPECTATIONS!'.         
043800     05  FINAL-4              PIC X(51) VALUE                             
043900           'AND YOUR IMMENSE EMPIRE IS BECOMING TOO TYRANNICAL.'.         
044000     05  FINAL-5              PIC X(56) VALUE                             
044100      'AFTER 25 YEARS, YOU HAVE EXCEEDED YOUR ROYAL USEFULNESS.'.         
044200     05  FINAL-6              PIC X(48) VALUE                             
044300              'IN AN EFFORT TO MAINTAIN A HAPPY POPULATION. . .'.         
044400     05  FINAL-7              PIC X(53) VALUE                             
044500         'YOU DECIDE TO ABDICATE IN FAVOR OF THE PRIME MINISTER'.         
044600     05  FINAL-8              PIC X(37) VALUE                             
044700                         'THE KING IS DEAD! LONG LIVE THE KING!'.         
044800     05  FINAL-9.                                                         
044900         10  FILLER           PIC X(15) VALUE 'YOU BEGAN WITH'.           
045000         10  FINAL-9-PEOPLE   PIC ZZ,ZZ9.                                 
045100         10  FILLER           PIC X(9) VALUE ' PEOPLE,'.                  
045200     05  FINAL-10.                                                        
045300         10  FILLER           PIC X(11) VALUE 'THERE WERE '.              
045400         10  FINAL-10-IMMIGRANTS PIC ZZ,ZZ9.                              
045500         10  FILLER           PIC X(12) VALUE ' IMMIGRANTS,'.             
045600     05  FINAL-11.                                                        
045700         10  FILLER           PIC X(4) VALUE 'BUT '.                      
045800         10  FINAL-11-STARVED PIC ZZ,ZZ9.                                 
045900         10  FILLER           PIC X(18) VALUE                             
046000                                            ' STARVED TO DEATH.'.         
046100     05  FINAL-12.                                                        
046200         10  FILLER           PIC X(4) VALUE 'AND '.                      
046300         10  FINAL-12-PLAGUE  PIC ZZ,ZZ9.                                 
046400         10  FILLER           PIC X(20) VALUE                             
046500                                          ' DIED OF THE "CRUD".'.         
046600     05  FINAL-13.                                                        
046700         10  FILLER           PIC X(12) VALUE 'EVEN THOUGH '.             
046800         10  FINAL-13-SHOTS   PIC ZZ,ZZ9.                                 
046900         10  FILLER           PIC X(11) VALUE ' HAD SHOTS.'.              
047000     05  FINAL-14.                                                        
047100         10  FILLER           PIC X(8) VALUE 'LEAVING '.                  
047200         10  FINAL-14-PEOPLE  PIC ZZ,ZZ9.                                 
047300         10  FILLER           PIC X(18) VALUE                             
047400                                            ' ALIVE AT THE END.'.         
047500     05  FINAL-15             PIC X(48) VALUE                             
047600              'AS A RESULT OF YOUR LAND TRADING ENTERPRISES....'.         
047700     05  FINAL-16.                                                        
047800         10  FILLER           PIC X(17) VALUE                             
047900                                     'YOU STARTED WITH '.                 
048000         10  FINAL-16-ACRES   PIC ZZZ,ZZZ,ZZ9.                            
048100         10  FILLER           PIC X(24) VALUE                             
048200                                 ' ACRES OF LAND, AND WITH'.              
048300     05  FINAL-17.                                                        
048400         10  FINAL-17-GRAIN   PIC ZZZ,ZZZ,ZZ9.                            
048500         10  FILLER           PIC X(18) VALUE                             
048600                                      ' BUSHELS OF GRAIN,'.               
048700     05  FINAL-18.                                                        
048800         10  FILLER           PIC X(11) VALUE 'YOU BOUGHT '.              
048900         10  FINAL-18-ACRES   PIC ZZZ,ZZZ,ZZ9.                            
049000         10  FILLER           PIC X(14) VALUE ' ACRES, PAYING'.           
049100     05  FINAL-19.                                                        
049200         10  FINAL-19-BUSHELS PIC ZZZ,ZZZ,ZZ9.                            
049300         10  FILLER           PIC X(9) VALUE ' BUSHELS,'.                 
049400     05  FINAL-20.                                                        
049500         10  FILLER           PIC X(13) VALUE 'AND YOU SOLD '.            
049600         10  FINAL-20-ACRES   PIC ZZZ,ZZZ,ZZ9.                            
049700         10  FILLER           PIC X(17) VALUE                             
049800                                             ' ACRES, RECEIVING'.         
049900     05  FINAL-21.                                                        
050000         10  FINAL-21-BUSHELS PIC ZZZ,ZZZ,ZZ9.                            
050100         10  FILLER           PIC X(9) VALUE ' BUSHELS,'.                 
050200     05  FINAL-22.                                                        
050300         10  FILLER           PIC X(15) VALUE 'ENDING UP WITH '.          
050400         10  FINAL-22-ACRES   PIC ZZZ,ZZZ,ZZ9.                            
050500         10  FILLER           PIC X(16) VALUE ' ACRES, AND WITH'.         
050600     05  FINAL-23.                                                        
050700         10  FINAL-23-BUSHELS PIC ZZZ,ZZZ,ZZ9.                            
050800         10  FILLER           PIC X(20) VALUE                             
050900                                          ' BUSHELS IN STORAGE.'.         
051000     05  FINAL-24.                                                        
051100         10  FILLER           PIC X(44) VALUE                             
051200                  'OVER THE SEASONS OF YOUR IMPERIAL REIGN, OF '.         
051300         10  FINAL-24-YEARS   PIC ZZ9.                                    
051400         10  FILLER           PIC X(7) VALUE ' YEARS,'.                   
051500     05  FINAL-25.                                                        
051600         10  FILLER           PIC X(12) VALUE 'YOU PLANTED '.             
051700         10  FINAL-25-ACRES   PIC ZZZ,ZZZ,ZZ9.                            
051800         10  FILLER           PIC X(13) VALUE ' ACRES, WITH '.            
051900         10  FINAL-25-PLANT   PIC ZZZ,ZZZ,ZZ9.                            
052000     05  FINAL-26.                                                        
052100         10  FILLER           PIC X(16) VALUE 'BUSHELS OF SEED,'.         
052200     05  FINAL-27.                                                        
052300         10  FILLER           PIC X(9) VALUE 'YIELDING '.                 
052400         10  FINAL-27-HARVESTED PIC ZZZ,ZZZ,ZZ9.                          
052500         10  FILLER           PIC X(15) VALUE ' BUSHELS, USING'.          
052600     05  FINAL-28.                                                        
052700         10  FINAL-28-BUSHELS PIC ZZZ,ZZZ,ZZ9.                            
052710         10  FILLER           PIC X(23) VALUE                             
052720                                       ' BUSHELS AS FERTILIZER,'.         
052800     05  FINAL-29.                                                        
052900         10  FILLER           PIC X(13) VALUE 'BUT YOU LOST '.            
053000         10  FINAL-29-HARVEST PIC ZZZ,ZZZ,ZZ9.                            
053100         10  FILLER           PIC X(28) VALUE                             
053200                                 ' BUSHELS TO THE RATS, ET.AL.'.          
053600*                                                                         
053700 PROCEDURE DIVISION.                                                      
053800     OPEN INPUT INPUT-FILE, OUTPUT PRINT-FILE.                            
053900     MOVE SPACES TO PRINT-RECORD.                                         
054000     MOVE HDR1 TO PRINT-RECORD.                                           
054100     PERFORM PRINT-ROUTINE.                                               
054200     MOVE HDR2 TO PRINT-RECORD.                                           
054300     PERFORM PRINT-ROUTINE.                                               
054400     PERFORM PRINT-ROUTINE.                                               
054500     PERFORM RANDOMIZE.                                                   
054600     COMPUTE ACRES = RAND-NBR * 1000.                                     
054700     COMPUTE ACRES-START = ACRES.                                         
054800     PERFORM RANDOMIZE.                                                   
054900     COMPUTE PEOPLE = RAND-NBR * 1000.                                    
055000     COMPUTE PEOPL-START = PEOPLE.                                        
055100     PERFORM RANDOMIZE.                                                   
055200     COMPUTE GRAIN = RAND-NBR * 2000.                                     
055300     COMPUTE GRAIN-START = GRAIN.                                         
055400     MOVE HDR3 TO PRINT-RECORD.                                           
055500     PERFORM PRINT-ROUTINE.                                               
055600     PERFORM GET-REPLY                                                    
055700     IF X =  0  OR                                                        
055800         L = 'N'                                                          
055900             GO TO POP.                                                   
056000     MOVE HDR4 TO PRINT-RECORD.                                           
056100     PERFORM PRINT-ROUTINE.                                               
056200     MOVE HDR5 TO PRINT-RECORD.                                           
056300     PERFORM PRINT-ROUTINE.                                               
056400     MOVE HDR6 TO PRINT-RECORD.                                           
056500     PERFORM PRINT-ROUTINE.                                               
056600     MOVE HDR7 TO PRINT-RECORD.                                           
056700     PERFORM PRINT-ROUTINE.                                               
056800     MOVE HDR8 TO PRINT-RECORD.                                           
056900     PERFORM PRINT-ROUTINE.                                               
057000     MOVE HDR9 TO PRINT-RECORD.                                           
057100     PERFORM PRINT-ROUTINE.                                               
057200     MOVE HDR10 TO PRINT-RECORD.                                          
057300     PERFORM PRINT-ROUTINE.                                               
057400     MOVE HDR11 TO PRINT-RECORD.                                          
057500     PERFORM PRINT-ROUTINE.                                               
057600     MOVE HDR12 TO PRINT-RECORD.                                          
057700     PERFORM PRINT-ROUTINE.                                               
057800     MOVE HDR13 TO PRINT-RECORD.                                          
057900     PERFORM PRINT-ROUTINE.                                               
058000     PERFORM PRINT-ROUTINE.                                               
058100     GO TO POP.                                                           
058200 STATUS-DISPLAY.                                                          
058300     PERFORM PRINT-ROUTINE.                                               
058400     IF FIRST-TIME                                                        
058500         MOVE STATUS-1 TO PRINT-RECORD                                    
058600         MOVE 'N' TO FIRST-TIME-SWITCH                                    
058700     ELSE                                                                 
058710         MOVE Y-ARS TO STATUS-2-YARS                                      
058720         MOVE STATUS-2 TO PRINT-RECORD                                    
058800         IF FOOD > (PEOPLE * 1.3)                                         
058900             COMPUTE GRAIN = GRAIN + ((FOOD - (PEOPLE * 1.3))             
059000                 / 2).                                                    
059300     PERFORM PRINT-ROUTINE.                                               
059400     MOVE STATUS-DASHES TO PRINT-RECORD.                                  
059500     PERFORM PRINT-ROUTINE.                                               
059600     IF IMMIGRANTS = 0                                                    
059700         GO TO STARVOUT.                                                  
059800     MOVE IMMIGRANTS TO STATUS-3-IMMIGRANTS.                              
059900     MOVE STATUS-3 TO PRINT-RECORD.                                       
060000     PERFORM PRINT-ROUTINE.                                               
060100     COMPUTE TOT-IMM = TOT-IMM + IMMIGRANTS.                              
060200 STARVOUT.                                                                
060300     IF FIRST-TIME                                                        
060400         GO TO DIED.                                                      
060500     IF STARVED >= (IMMIGRANTS + PEOPLE)                                  
060600         MOVE PEOPLE TO STARVED.                                          
060700     IF STARVED = 0                                                       
060800         GO TO DIED.                                                      
060900     MOVE STARVED TO STATUS-4-STARVED.                                    
061000     MOVE STATUS-4 TO PRINT-RECORD.                                       
061100     PERFORM PRINT-ROUTINE.                                               
061200     COMPUTE TOT-STARVED = TOT-STARVED + STARVED.                         
061300 DIED.                                                                    
061400     IF PLAGUE = 0                                                        
061500         GO TO POP.                                                       
061600     IF PLAGUE >= (PEOPLE + IMMIGRANTS) - STARVED                         
061700         MOVE STATUS-5 TO PRINT-RECORD                                    
061800         PERFORM PRINT-ROUTINE                                            
061900         MOVE STATUS-6 TO PRINT-RECORD                                    
062000         PERFORM PRINT-ROUTINE                                            
062100         MOVE STATUS-7 TO PRINT-RECORD                                    
062200         PERFORM PRINT-ROUTINE                                            
062300         COMPUTE TOT-PLAGUED = TOT-PLAGUED + PLAGUE                       
062400         MOVE 0 TO PEOPLE                                                 
062500         GO TO FINIS.                                                     
062510     MOVE PLAGUE TO STATUS-8-PLAGUE.                                      
062600     MOVE STATUS-8 TO PRINT-RECORD                                        
062700     PERFORM PRINT-ROUTINE.                                               
062800     COMPUTE TOT-PLAGUED = TOT-PLAGUED + PLAGUE.                          
062900 POP.                                                                     
063000     COMPUTE PEOPLE = PEOPLE - (STARVED + PLAGUE) + IMMIGRANTS.           
063100     IF STARVED > 0 OR PLAGUE > 0                                         
063200         MOVE PEOPLE TO STATUS-9-PEOPLE                                   
063300         MOVE STATUS-9 TO PRINT-RECORD                                    
063400     ELSE                                                                 
063500         MOVE PEOPLE TO STATUS-10-PEOPLE                                  
063600         MOVE STATUS-10 TO PRINT-RECORD.                                  
063700     PERFORM PRINT-ROUTINE.                                               
063800     MOVE ACRES TO STATUS-11-ACRES.                                       
063900     MOVE GRAIN TO STATUS-11-GRAIN.                                       
064000     MOVE STATUS-11 TO PRINT-RECORD.                                      
064100     PERFORM PRINT-ROUTINE.                                               
064200     IF FIRST-TIME                                                        
064300         GO TO GRAIN-CNT.                                                 
064400     IF ACRES > 1000000                                                   
064500         GO TO MILL-YUN-AIR.                                              
064600     IF GRAIN > 4000000                                                   
064700         GO TO MILL-YUN-AIR.                                              
064800     IF ACRES = 0 AND GRAIN = 0                                           
064900         MOVE STATUS-12 TO PRINT-RECORD                                   
065000         PERFORM PRINT-ROUTINE                                            
065100         MOVE STATUS-13 TO PRINT-RECORD                                   
065200         PERFORM PRINT-ROUTINE                                            
065300         MOVE STATUS-14 TO PRINT-RECORD                                   
065400         PERFORM PRINT-ROUTINE                                            
065500         GO TO FINIS.                                                     
065600 GRAINCOUNT.                                                              
065700     IF Y-ARS > 24                                                        
065800         GO TO OVER-AGE.                                                  
065900     MOVE CONTINUE-1 TO PRINT-RECORD.                                     
066000     PERFORM PRINT-ROUTINE.                                               
066100     PERFORM GET-REPLY.                                                   
066200     IF L = 'N'                                                           
066300         MOVE SO-LONG-1 TO PRINT-RECORD                                   
066400         PERFORM PRINT-ROUTINE                                            
066500         GO TO FINIS.                                                     
066600 GRAIN-CNT.                                                               
066700     COMPUTE Y-ARS = Y-ARS + 1.                                           
066800     MOVE GRAIN TO GRAIN-1-GRAIN.                                         
066810     MOVE GRAIN-1 TO PRINT-RECORD.                                        
066900     PERFORM PRINT-ROUTINE.                                               
067000 LANDTRADE.                                                               
067100     IF FIRST-TIME                                                        
067200         GO TO EAT.                                                       
067300     PERFORM RANDOMIZE.                                                   
067400     COMPUTE COMPUTED-VALUE = (86 * RAND-NBR) + 1.                        
067500 RETRY.                                                                   
067600     MOVE COMPUTED-VALUE TO LAND-1-VALUE.                                 
067700     MOVE LAND-1 TO PRINT-RECORD.                                         
067800     PERFORM PRINT-ROUTINE.                                               
067900     IF COMPUTED-VALUE > GRAIN                                            
068000         MOVE LAND-2 TO PRINT-RECORD                                      
068100         PERFORM PRINT-ROUTINE                                            
068200         GO TO SELL.                                                      
068300 BUY.                                                                     
068400     COMPUTE TRADE = GRAIN / COMPUTED-VALUE.                              
068500     MOVE TRADE TO BUY-1-ACRES.                                           
068600     MOVE BUY-1 TO PRINT-RECORD.                                          
068700     PERFORM PRINT-ROUTINE.                                               
068800     MOVE BUY-2 TO PRINT-RECORD.                                          
068900     PERFORM PRINT-ROUTINE.                                               
069000     PERFORM GET-REPLY                                                    
069100     IF X = 0                                                             
069200         GO TO SELL.                                                      
069300     IF L = '?'                                                           
069400         MOVE PEOPLE TO STATUS-10-PEOPLE                                  
069500         MOVE STATUS-10 TO PRINT-RECORD                                   
069600         PERFORM PRINT-ROUTINE                                            
069700         MOVE ACRES TO STATUS-15-ACRES                                    
069800         MOVE GRAIN TO STATUS-15-GRAIN                                    
069900         MOVE STATUS-15 TO PRINT-RECORD                                   
070000         PERFORM PRINT-ROUTINE                                            
070100         GO TO RETRY.                                                     
070200     IF INPUT-IS-NUMERIC                                                  
070300         NEXT SENTENCE                                                    
070400     ELSE                                                                 
070500         MOVE NUMBERS-ONLY TO PRINT-RECORD                                
070600         PERFORM PRINT-ROUTINE                                            
070700         GO TO BUY.                                                       
070800     MOVE BIN-NUMBER TO TRADE.                                            
070900     IF TRADE = 0 THEN GO TO SELL.                                        
071000     IF (TRADE * COMPUTED-VALUE) > GRAIN                                  
071100         MOVE NOT-ENOUGH-GRAIN TO PRINT-RECORD                            
071200         PERFORM PRINT-ROUTINE                                            
071300         MOVE NICE-TRY TO PRINT-RECORD                                    
071400         PERFORM PRINT-ROUTINE                                            
071500         GO TO RETRY.                                                     
071600     COMPUTE ACRES-BOT = ACRES-BOT + TRADE.                               
071700     COMPUTE GRAIN = GRAIN - (TRADE * COMPUTED-VALUE).                    
071800     COMPUTE GRAIN-BUY = GRAIN-BUY + (TRADE * COMPUTED-VALUE).            
071900     COMPUTE ACRES = ACRES + TRADE.                                       
072000     MOVE GRAIN TO TRADING-1-GRAIN.                                       
072100     MOVE TRADING-1 TO PRINT-RECORD.                                      
072200     PERFORM PRINT-ROUTINE.                                               
072300     GO TO EAT-EM.                                                        
072400 SELL.                                                                    
072500     IF ACRES = 0                                                         
072600         MOVE NO-LAND-1 TO PRINT-RECORD                                   
072700         PERFORM PRINT-ROUTINE                                            
072800         GO TO EAT.                                                       
072900     MOVE ACRES TO STATUS-16-ACRES.                                       
073000     MOVE STATUS-16 TO PRINT-RECORD.                                      
073100     PERFORM PRINT-ROUTINE.                                               
073200     COMPUTE TRADE = ACRES * COMPUTED-VALUE.                              
073300     MOVE TRADE TO STATUS-17-GRAIN.                                       
073400     MOVE STATUS-17 TO PRINT-RECORD.                                      
073500     PERFORM PRINT-ROUTINE.                                               
073600 RETRYAGAIN.                                                              
073700     MOVE HOW-MANY-SELL TO PRINT-RECORD.                                  
073800     PERFORM PRINT-ROUTINE.                                               
073900     PERFORM GET-REPLY.                                                   
074000     IF X = 0                                                             
074100         GO TO EAT.                                                       
074200     IF L = '?'                                                           
074300         MOVE PEOPLE TO STATUS-10-PEOPLE                                  
074400         MOVE STATUS-10 TO PRINT-RECORD                                   
074500         PERFORM PRINT-ROUTINE                                            
074600         MOVE ACRES TO STATUS-15-ACRES                                    
074700         MOVE GRAIN TO STATUS-15-GRAIN                                    
074800         MOVE STATUS-15 TO PRINT-RECORD                                   
074900         PERFORM PRINT-ROUTINE                                            
075000         GO TO RETRYAGAIN.                                                
075100     IF INPUT-IS-NUMERIC                                                  
075200         NEXT SENTENCE                                                    
075300     ELSE                                                                 
075400         MOVE NUMBERS-ONLY TO PRINT-RECORD                                
075500         PERFORM PRINT-ROUTINE                                            
075600         GO TO RETRYAGAIN.                                                
075700     MOVE BIN-NUMBER TO TRADE.                                            
075800     IF TRADE = 0                                                         
075900         GO TO EAT.                                                       
076000     IF TRADE > ACRES                                                     
076100         MOVE BUY-3 TO PRINT-RECORD                                       
076200         PERFORM PRINT-ROUTINE                                            
076300         MOVE BUY-4 TO PRINT-RECORD                                       
076400         PERFORM PRINT-ROUTINE                                            
076500         GO TO RETRYAGAIN.                                                
076600     COMPUTE GRAIN = GRAIN + (TRADE * COMPUTED-VALUE).                    
076700     COMPUTE ACRES-SOLD = ACRES-SOLD + TRADE.                             
076800     COMPUTE GRAIN-SOLD = GRAIN-SOLD + (TRADE * COMPUTED-VALUE).          
076900     COMPUTE ACRES = ACRES - TRADE.                                       
077000 EAT.                                                                     
077100     IF FIRST-TIME                                                        
077200         GO TO PASS.                                                      
077300     MOVE GRAIN TO STATUS-18-GRAIN.                                       
077400     MOVE STATUS-18 TO PRINT-RECORD.                                      
077500     PERFORM PRINT-ROUTINE.                                               
077600 EAT-EM.                                                                  
077700     MOVE PEOPLE TO STATUS-19-PEOPLE.                                     
077800     MOVE STATUS-19 TO PRINT-RECORD.                                      
077900     PERFORM PRINT-ROUTINE.                                               
078000 PASS.                                                                    
078100     IF GRAIN = 0                                                         
078200         MOVE PEOPLE TO GROWL-1-PEOPLE                                    
078300         MOVE GROWL-1 TO PRINT-RECORD                                     
078400         PERFORM PRINT-ROUTINE                                            
078500         MOVE NO-GRAIN TO PRINT-RECORD                                    
078600         PERFORM PRINT-ROUTINE                                            
078700         GO TO FARM.                                                      
078800     MOVE HOW-MANY-GRAIN TO PRINT-RECORD.                                 
078900     PERFORM PRINT-ROUTINE.                                               
079000     PERFORM GET-REPLY.                                                   
079100     IF X = 0                                                             
079200         MOVE '0' TO L.                                                   
079300     IF L = '?'                                                           
079400         MOVE PEOPLE TO STATUS-10-PEOPLE                                  
079500         MOVE STATUS-10 TO PRINT-RECORD                                   
079600         PERFORM PRINT-ROUTINE                                            
079700         MOVE ACRES TO STATUS-15-ACRES                                    
079800         MOVE GRAIN TO STATUS-15-GRAIN                                    
079900         MOVE STATUS-15 TO PRINT-RECORD                                   
080000         PERFORM PRINT-ROUTINE                                            
080100         GO TO PASS.                                                      
080200     IF INPUT-IS-NUMERIC                                                  
080300         NEXT SENTENCE                                                    
080400     ELSE                                                                 
080500         MOVE NUMBERS-ONLY TO PRINT-RECORD                                
080600         PERFORM PRINT-ROUTINE                                            
080700         GO TO EAT.                                                       
080800     MOVE BIN-NUMBER TO FOOD.                                             
080900     IF FOOD > GRAIN                                                      
081000         MOVE INSULTED TO PRINT-RECORD                                    
081100         PERFORM PRINT-ROUTINE                                            
081200         MOVE RECANTED TO PRINT-RECORD                                    
081300         PERFORM PRINT-ROUTINE                                            
081400         GO TO EAT.                                                       
081500     IF L = 0                                                             
081600         MOVE GENOCIDE TO PRINT-RECORD                                    
081700         PERFORM PRINT-ROUTINE                                            
081800         MOVE APOCALYPSE TO PRINT-RECORD                                  
081900         PERFORM PRINT-ROUTINE.                                           
082000     COMPUTE GRAIN = GRAIN - FOOD.                                        
082100     MOVE GRAIN TO STATUS-20-GRAIN.                                       
082110     MOVE STATUS-20 TO PRINT-RECORD.                                      
082200     PERFORM PRINT-ROUTINE.                                               
082300 FARM.                                                                    
082400     IF ACRES = 0                                                         
082500         MOVE NO-LAND-2 TO PRINT-RECORD                                   
082600         PERFORM PRINT-ROUTINE                                            
082700         MOVE 0 TO FARMLAND                                               
082800         GO TO DOIT.                                                      
082900     MOVE ACRES TO STATUS-21-ACRES.                                       
083000     MOVE PEOPLE TO STATUS-21-PEOPLE.                                     
083100     MOVE STATUS-21 TO PRINT-RECORD.                                      
083200     PERFORM PRINT-ROUTINE.                                               
083300     IF GRAIN = 0                                                         
083400         MOVE NO-SEED TO PRINT-RECORD                                     
083500         PERFORM PRINT-ROUTINE                                            
083600         MOVE 0 TO FARMLAND                                               
083700         GO TO DOIT.                                                      
083800 RETRY1.                                                                  
083900     MOVE HOW-MANY-ACRES TO PRINT-RECORD.                                 
084000     PERFORM PRINT-ROUTINE.                                               
084100     PERFORM GET-REPLY.                                                   
084200     IF X = 0                                                             
084300         MOVE 0 TO L.                                                     
084400     IF L = '?'                                                           
084500         MOVE PEOPLE TO STATUS-10-PEOPLE                                  
084600         MOVE STATUS-10 TO PRINT-RECORD                                   
084700         PERFORM PRINT-ROUTINE                                            
084800         MOVE ACRES TO STATUS-15-ACRES                                    
084900         MOVE GRAIN TO STATUS-15-GRAIN                                    
085000         MOVE STATUS-15 TO PRINT-RECORD                                   
085100         PERFORM PRINT-ROUTINE                                            
085200         GO TO RETRY1.                                                    
085300     IF INPUT-IS-NUMERIC                                                  
085310         NEXT SENTENCE                                                    
085320     ELSE                                                                 
085400         MOVE NUMBERS-ONLY TO PRINT-RECORD                                
085500         PERFORM PRINT-ROUTINE                                            
085600         GO TO RETRY1.                                                    
085700     MOVE BIN-NUMBER TO FARMLAND.                                         
085800     IF FARMLAND > ACRES                                                  
085900         MOVE DONT-UNDERSTAND TO PRINT-RECORD                             
086000         PERFORM PRINT-ROUTINE                                            
086100         MOVE CANT-FARM TO PRINT-RECORD                                   
086200         PERFORM PRINT-ROUTINE                                            
086300         GO TO RETRY1.                                                    
086400     IF (FARMLAND * 1.8) > PEOPLE                                         
086500         MOVE NOT-ENOUGH-PEOPLE TO PRINT-RECORD                           
086600         PERFORM PRINT-ROUTINE                                            
086700         GO TO RETRY1.                                                    
086800     IF (FARMLAND * .78) > GRAIN                                          
086900         MOVE NOT-ENOUGH-SEED TO PRINT-RECORD                             
087000         PERFORM PRINT-ROUTINE                                            
087100         GO TO RETRY1.                                                    
087200     IF FARMLAND = 0                                                      
087300         GO TO DOIT.                                                      
087400     IF GRAIN = 0                                                         
087500         GO TO DOIT.                                                      
087600     COMPUTE GRAIN = GRAIN - (FARMLAND * .78).                            
087700     COMPUTE PLANTED = PLANTED + FARMLAND.                                
087800     COMPUTE GRAIN-PLANT = GRAIN-PLANT + (FARMLAND * .78).                
087900     PERFORM RANDOMIZE.                                                   
088000     IF RAND-NBR > .698765                                                
088100         MOVE 'N' TO FERT-SW                                              
088200         GO TO DOIT.                                                      
088300     PERFORM RANDOMIZE.                                                   
088400     COMPUTE FERT-AMT = (10 * RAND-NBR) + 0.5.                            
088500     MOVE FERT-AMT TO FERTILIZER-1-PRICE.                                 
088600     MOVE FERTILIZER-1 TO PRINT-RECORD.                                   
088700     PERFORM PRINT-ROUTINE.                                               
088800     MOVE WISH-TO-ENHANCE TO PRINT-RECORD.                                
088900     PERFORM PRINT-ROUTINE.                                               
089000     PERFORM GET-REPLY.                                                   
089100     IF X =  0                                                            
089200         MOVE 'Y' TO L.                                                   
089300     IF L NOT = 'Y'                                                       
089400         MOVE 'N' TO FERT-SW                                              
089500         GO TO DOIT.                                                      
089600     MOVE 'Y' TO FERT-SW.                                                 
089700     COMPUTE COMPUTED-VALUE = FARMLAND * FERT-AMT.                        
089800     IF COMPUTED-VALUE > GRAIN                                            
089900         MOVE NO-CREDIT TO PRINT-RECORD                                   
090000         PERFORM PRINT-ROUTINE                                            
090100         MOVE 'N' TO FERT-SW                                              
090200         GO TO DOIT.                                                      
090300     COMPUTE GRAIN = GRAIN - COMPUTED-VALUE.                              
090400     COMPUTE TOT-FERT = TOT-FERT + COMPUTED-VALUE.                        
090500 DOIT.                                                                    
090600     IF GRAIN > 22222                                                     
090700         PERFORM RANDOMIZE                                                
090800         IF RAND-NBR > .920000                                            
090900            MOVE GRAIN-ROT TO PRINT-RECORD                                
091000            PERFORM PRINT-ROUTINE                                         
091100            COMPUTE HARVEST-LST = HARVEST-LST + (GRAIN / 1.91)            
091200            COMPUTE GRAIN = GRAIN / 1.91                                  
091300            MOVE GRAIN TO ONLY-SAVED-1-GRAIN                              
091400            MOVE ONLY-SAVED-1 TO PRINT-RECORD                             
091500            PERFORM PRINT-ROUTINE.                                        
091600     IF NO-PLAGUE-THREAT                                                  
091610         GO TO NOSHOTS.                                                   
091700     IF LAST-SHOT >= PEOPLE                                               
091800         MOVE NO-SHOTS-NEEDED TO PRINT-RECORD                             
091900         PERFORM PRINT-ROUTINE                                            
092000         MOVE IMMUNIZED TO PRINT-RECORD                                   
092100         PERFORM PRINT-ROUTINE                                            
092200         COMPUTE INNOCULATED = LAST-SHOT / 2                              
092300         GO TO NO-SHOTS.                                                  
092500     PERFORM RANDOMIZE                                                    
092600     COMPUTE SERUM = (13 * RAND-NBR) + 1                                  
092700     IF GRAIN > 2000000                                                   
092800         COMPUTE SERUM = SERUM + 128                                      
092900     ELSE                                                                 
093000         IF GRAIN > 1500000                                               
093100             COMPUTE SERUM = SERUM + 64                                   
093200         ELSE                                                             
093300             IF GRAIN > 1000000                                           
093400                 COMPUTE SERUM = SERUM + 32                               
093500             ELSE                                                         
093600                 IF GRAIN > 500000                                        
093700                     COMPUTE SERUM = SERUM + 16                           
093800                 ELSE                                                     
093900                     IF GRAIN > 100000                                    
094000                         COMPUTE SERUM = SERUM + 8                        
094100                     ELSE                                                 
094200                         IF GRAIN > 50000                                 
094300                             COMPUTE SERUM = SERUM + 4                    
094400                         ELSE                                             
094500                             IF GRAIN > 25000                             
094600                                 COMPUTE SERUM = SERUM + 2.               
094800     MOVE DISTRIBUTING-1 TO PRINT-RECORD                                  
094900     PERFORM PRINT-ROUTINE                                                
095000     MOVE SERUM TO DISTRIBUTING-2-SERUM                                   
095100     MOVE DISTRIBUTING-2 TO PRINT-RECORD                                  
095200     PERFORM PRINT-ROUTINE                                                
095300     IF SERUM > GRAIN                                                     
095400         MOVE CANT-AFFORD-SHOTS-1 TO PRINT-RECORD                         
095500         PERFORM PRINT-ROUTINE                                            
095600         MOVE 0 TO INNOCULATED                                            
095700         GO TO NOSHOTS.                                                   
095800 PEOPLESHOTS.                                                             
095900     COMPUTE INNOCULATED = GRAIN / SERUM.                                 
096000     IF INNOCULATED >= PEOPLE                                             
096100         MOVE PEOPLE TO INNOCULATED.                                      
096200     MOVE INNOCULATED TO CAN-AFFORD-SHOTS-1-PEOPLE.                       
096300     MOVE CAN-AFFORD-SHOTS-1 TO PRINT-RECORD.                             
096400     PERFORM PRINT-ROUTINE.                                               
096500     IF INNOCULATED <= (PEOPLE - LAST-SHOT)                               
096600         GO TO SHOTS.                                                     
096700     IF LAST-SHOT = 0                                                     
096800         GO TO SHOTS.                                                     
096900     COMPUTE INNOCULATED = PEOPLE - LAST-SHOT.                            
097000     MOVE LAST-SHOT TO STILL-IMMUNIZED-1-LAST.                            
097100     MOVE STILL-IMMUNIZED-1 TO PRINT-RECORD.                              
097200     PERFORM PRINT-ROUTINE.                                               
097300     MOVE INNOCULATED TO STILL-IMMUNIZED-2-REST.                          
097400     MOVE STILL-IMMUNIZED-2 TO PRINT-RECORD.                              
097500     PERFORM PRINT-ROUTINE.                                               
097600 SHOTS.                                                                   
097700     MOVE HOW-MANY-GET TO PRINT-RECORD.                                   
097800     PERFORM PRINT-ROUTINE.                                               
097900     PERFORM GET-REPLY.                                                   
098000     IF X = 0                                                             
098100         MOVE 0 TO L.                                                     
098200     IF INPUT-IS-NUMERIC                                                  
098210         NEXT SENTENCE                                                    
098220     ELSE                                                                 
098300         MOVE NUMBERS-ONLY TO PRINT-RECORD                                
098400         PERFORM PRINT-ROUTINE                                            
098500         GO TO SHOTS.                                                     
098600     MOVE BIN-NUMBER TO INNOCULATED.                                      
098700     MOVE INNOCULATED TO THIS-SHOT.                                       
098800     IF INNOCULATED > (GRAIN / SERUM)                                     
098900         MOVE 'W H A A T ?' TO PRINT-RECORD                               
099000         PERFORM PRINT-ROUTINE                                            
099100         GO TO PEOPLESHOTS.                                               
099200     IF INNOCULATED = 0                                                   
099300         MOVE EASY-TO-GAMBLE TO PRINT-RECORD                              
099400         PERFORM PRINT-ROUTINE                                            
099500         GO TO NOSHOTS.                                                   
099600     COMPUTE GRAIN = GRAIN - (INNOCULATED * SERUM).                       
099700     MOVE GRAIN TO GRAIN-LEFT-1-GRAIN.                                    
099800     MOVE GRAIN-LEFT-1 TO PRINT-RECORD.                                   
099900     PERFORM PRINT-ROUTINE.                                               
100000 NOSHOTS.                                                                 
100100     COMPUTE TOTL-SHOT = TOTL-SHOT + INNOCULATED.                         
100200 NO-SHOTS.                                                                
100400     IF FARMLAND = 0                                                      
100500         PERFORM RANDOMIZE                                                
100600         COMPUTE X = RAND-NBR * 10                                        
100700         MOVE GAMES (X) TO NOTHING-FARMED-GAME                            
100800         MOVE NOTHING-FARMED TO PRINT-RECORD                              
100900         PERFORM PRINT-ROUTINE                                            
101000         PERFORM RANDOMIZE                                                
101100         COMPUTE RATS = GRAIN * RAND-NBR                                  
101200         MOVE RATS-OVERRAN TO PRINT-RECORD                                
101300         PERFORM PRINT-ROUTINE                                            
101400         COMPUTE GRAIN = GRAIN - RATS                                     
101500         IF GRAIN = 0                                                     
101600             MOVE RATS-ATE-CATS TO PRINT-RECORD                           
101700             PERFORM PRINT-ROUTINE                                        
101800             GO TO PASSRATS                                               
101900         ELSE                                                             
102000             MOVE GRAIN TO GRAIN-LEFT-2-GRAIN                             
102100             MOVE GRAIN-LEFT-2 TO PRINT-RECORD                            
102200             PERFORM PRINT-ROUTINE                                        
102300             GO TO PASSRATS.                                              
102400     MOVE WORKING-AS-TOLD TO PRINT-RECORD.                                
102500     PERFORM PRINT-ROUTINE.                                               
102600     PERFORM RANDOMIZE.                                                   
102700     IF RAND-NBR < .001111                                                
102800         MOVE REVOLTED TO PRINT-RECORD                                    
102900         PERFORM PRINT-ROUTINE                                            
103000         MOVE LUCKY-TO-ESCAPE TO PRINT-RECORD                             
103100         PERFORM PRINT-ROUTINE                                            
103200         GO TO REVOLT.                                                    
103300     MOVE HERE-ARE-FRUITS TO PRINT-RECORD.                                
103400     PERFORM PRINT-ROUTINE.                                               
103500     MOVE 0 TO RATS.                                                      
103600     PERFORM RANDOMIZE.                                                   
103700     IF RAND-NBR < .002222                                                
103800         IF NO-FERTILIZER-WANTED                                          
103900             GO TO NOT-FERT-ED                                            
104000         ELSE                                                             
104100             MOVE OVER-FERTILIZED TO PRINT-RECORD                         
104200             PERFORM PRINT-ROUTINE                                        
104300             MOVE UNABLE-TO-SAVE TO PRINT-RECORD                          
104400             PERFORM PRINT-ROUTINE                                        
104500             GO TO PASSRATS.                                              
104600 NOT-FERT-ED.                                                             
104700     IF FARMLAND = 0                                                      
104800         MOVE 0 TO YIELD                                                  
104900     ELSE                                                                 
105000         PERFORM RANDOMIZE                                                
105100         COMPUTE YIELD = (17 * RAND-NBR) + 0.1.                           
105200     IF FERTILIZER-WANTED                                                 
105300         PERFORM RANDOMIZE                                                
105400         COMPUTE YIELD = YIELD + (FERT-AMT * (RAND-NBR + 1.0) +           
105500             FERT-AMT).                                                   
105600     IF FARMLAND > 0                                                      
105700         COMPUTE YIELD = YIELD + 0.8.                                     
105800     MOVE YIELD TO YIELD-PER-ACRE-YIELD.                                  
105900     MOVE YIELD-PER-ACRE TO PRINT-RECORD.                                 
106000     PERFORM PRINT-ROUTINE.                                               
106100     COMPUTE DIFF = YIELD * FARMLAND.                                     
106200     IF DIFF = 0                                                          
106300         GO TO PASSRATS.                                                  
106400     ADD 1 TO DIFF.                                                       
106500     COMPUTE HARVESTD = HARVESTD + DIFF.                                  
106600     MOVE DIFF TO TOTAL-HARVEST-BUSHELS.                                  
106700     MOVE TOTAL-HARVEST TO PRINT-RECORD.                                  
106800     PERFORM PRINT-ROUTINE.                                               
106900     PERFORM RANDOMIZE.                                                   
107000     IF RAND-NBR < .760000                                                
107100         COMPUTE GRAIN = GRAIN + DIFF                                     
107200         GO TO PASSRATS                                                   
107300     ELSE                                                                 
107400         IF RAND-NBR > .960000                                            
107500             MOVE DIFF TO RATS                                            
107600         ELSE                                                             
107700             COMPUTE RATS =(((RAND-NBR * RAND-NBR) *                      
107800                              RAND-NBR) * DIFF) + 1.                      
107900     IF DIFF = 0                                                          
108000         GO TO PASSRATS.                                                  
108100     IF RATS >= DIFF                                                      
108200         IF RAND-NBR > .860000                                            
108300             MOVE LOCUSTS-ATE TO PRINT-RECORD                             
108400         ELSE                                                             
108500             MOVE RATS-ATE-1 TO PRINT-RECORD                              
108600             PERFORM PRINT-ROUTINE                                        
108700     ELSE                                                                 
108800         IF FERTILIZER-WANTED                                             
108900             COMPUTE RATS = RATS / 2                                      
109000             MOVE RATS TO RATS-ATE-2-BUSHELS                              
109100             MOVE RATS-ATE-2 TO PRINT-RECORD                              
109200             PERFORM PRINT-ROUTINE                                        
109300             COMPUTE GRAIN =(GRAIN + DIFF) - RATS.                        
109400 PASSRATS.                                                                
109500     COMPUTE HARVEST-LST = HARVEST-LST + RATS + ROBBED.                   
109600     MOVE 0 TO ROBBED.                                                    
109700     IF FOOD > (PEOPLE * 1.3)                                             
109800         MOVE 0 TO STARVED                                                
109900     ELSE                                                                 
110000         COMPUTE STARVED = PEOPLE - (FOOD / 1.3).                         
110100     IF STARVED NOT = 0                                                   
110200         MOVE PEASANTS-STEALING TO PRINT-RECORD                           
110300         PERFORM PRINT-ROUTINE                                            
110400         IF (STARVED * 1.11) >= GRAIN                                     
110500             MOVE GRAIN TO ROBBED                                         
110600             MOVE 0 TO GRAIN                                              
110700         ELSE                                                             
110800             COMPUTE ROBBED = (STARVED * 1.11)                            
110900             COMPUTE GRAIN = GRAIN - ROBBED.                              
111000     IF STARVED NOT = 0 AND ROBBED = 0 THEN                               
111100         MOVE ATE-SHOES TO PRINT-RECORD                                   
111110         PERFORM PRINT-ROUTINE                                            
111200     ELSE                                                                 
111300         MOVE ROBBED TO STOLE-GRAIN-BUSHELS                               
111400         MOVE STOLE-GRAIN TO PRINT-RECORD                                 
111500         PERFORM PRINT-ROUTINE.                                           
111600     PERFORM RANDOMIZE.                                                   
111700     COMPUTE IMMIGRANTS = RAND-NBR * 100.                                 
111800     MOVE 0 TO PLAGUE.                                                    
111900     PERFORM RANDOMIZE.                                                   
112000     IF RAND-NBR > .499999                                                
112100         MOVE 'Y' TO PLAG-SW                                              
112200         PERFORM RANDOMIZE                                                
112300         COMPUTE PLAGUE = ((PEOPLE - LAST-SHOT) / 2) -                    
112400             (RAND-NBR * 99)                                              
112500         IF PLAGUE < 0                                                    
112600             COMPUTE PLAGUE = PLAGUE * (-1).                              
112700     IF RAND-NBR > .499999                                                
112800         IF (INNOCULATED + LAST-SHOT) >= PLAGUE                           
112900             MOVE 0 TO PLAGUE                                             
113000         ELSE                                                             
113100             COMPUTE PLAGUE = PLAGUE -(INNOCULATED + LAST-SHOT).          
113200     IF RAND-NBR > .499999                                                
113300         COMPUTE LAST-SHOT = (THIS-SHOT / 2)                              
113400     ELSE                                                                 
113500         COMPUTE LAST-SHOT = LAST-SHOT + (THIS-SHOT * 0.7).               
113600     GO TO STATUS-DISPLAY.                                                
113700 MILL-YUN-AIR.                                                            
113800     MOVE FINAL-1 TO PRINT-RECORD.                                        
113900     PERFORM PRINT-ROUTINE.                                               
114000     MOVE FINAL-2 TO PRINT-RECORD.                                        
114100     PERFORM PRINT-ROUTINE.                                               
114200     MOVE FINAL-3 TO PRINT-RECORD.                                        
114300     PERFORM PRINT-ROUTINE.                                               
114400     MOVE FINAL-4 TO PRINT-RECORD.                                        
114500     PERFORM PRINT-ROUTINE.                                               
114600     GO TO HAPPI-NESS.                                                    
114700 OVER-AGE.                                                                
114800     MOVE FINAL-5 TO PRINT-RECORD.                                        
114900     PERFORM PRINT-ROUTINE.                                               
115000 HAPPI-NESS.                                                              
115100     MOVE FINAL-6 TO PRINT-RECORD.                                        
115200     PERFORM PRINT-ROUTINE.                                               
115300 REVOLT.                                                                  
115400     MOVE FINAL-7 TO PRINT-RECORD.                                        
115500     PERFORM PRINT-ROUTINE.                                               
115600     MOVE FINAL-8 TO PRINT-RECORD.                                        
115700     PERFORM PRINT-ROUTINE.                                               
115800 FINIS.                                                                   
115900     PERFORM PRINT-ROUTINE.                                               
116000     MOVE PEOPL-START TO FINAL-9-PEOPLE.                                  
116100     MOVE FINAL-9 TO PRINT-RECORD.                                        
116200     PERFORM PRINT-ROUTINE.                                               
116300     MOVE TOT-IMM TO FINAL-10-IMMIGRANTS.                                 
116400     MOVE FINAL-10 TO PRINT-RECORD.                                       
116500     PERFORM PRINT-ROUTINE.                                               
116600     MOVE TOT-STARVED TO FINAL-11-STARVED.                                
116700     MOVE FINAL-11 TO PRINT-RECORD.                                       
116800     PERFORM PRINT-ROUTINE.                                               
116900     MOVE TOT-PLAGUED TO FINAL-12-PLAGUE.                                 
117000     MOVE FINAL-12 TO PRINT-RECORD.                                       
117100     PERFORM PRINT-ROUTINE.                                               
117200     MOVE TOTL-SHOT TO FINAL-13-SHOTS.                                    
117300     MOVE FINAL-13 TO PRINT-RECORD.                                       
117400     PERFORM PRINT-ROUTINE.                                               
117500     MOVE PEOPLE TO FINAL-14-PEOPLE.                                      
117600     PERFORM PRINT-ROUTINE.                                               
117700     MOVE FINAL-15 TO PRINT-RECORD.                                       
117800     PERFORM PRINT-ROUTINE.                                               
117900     MOVE ACRES-START TO FINAL-16-ACRES.                                  
118000     MOVE FINAL-16 TO PRINT-RECORD.                                       
118100     PERFORM PRINT-ROUTINE.                                               
118200     MOVE GRAIN-START TO FINAL-17-GRAIN.                                  
118300     MOVE FINAL-17 TO PRINT-RECORD.                                       
118400     PERFORM PRINT-ROUTINE.                                               
118500     MOVE ACRES-BOT TO FINAL-18-ACRES.                                    
118600     MOVE FINAL-18 TO PRINT-RECORD.                                       
118700     PERFORM PRINT-ROUTINE.                                               
118800     MOVE GRAIN-BUY TO FINAL-19-BUSHELS.                                  
118900     MOVE FINAL-19 TO PRINT-RECORD.                                       
119000     PERFORM PRINT-ROUTINE.                                               
119100     MOVE ACRES-SOLD TO FINAL-20-ACRES.                                   
119200     MOVE FINAL-20 TO PRINT-RECORD.                                       
119300     PERFORM PRINT-ROUTINE.                                               
119400     MOVE GRAIN-SOLD TO FINAL-21-BUSHELS.                                 
119500     MOVE FINAL-21 TO PRINT-RECORD.                                       
119600     PERFORM PRINT-ROUTINE.                                               
119700     MOVE ACRES TO FINAL-22-ACRES.                                        
119800     MOVE FINAL-22 TO PRINT-RECORD.                                       
119900     PERFORM PRINT-ROUTINE.                                               
120000     MOVE GRAIN TO FINAL-23-BUSHELS.                                      
120100     MOVE FINAL-23 TO PRINT-RECORD.                                       
120200     PERFORM PRINT-ROUTINE.                                               
120300     MOVE Y-ARS TO FINAL-24-YEARS.                                        
120400     MOVE FINAL-24 TO PRINT-RECORD.                                       
120500     PERFORM PRINT-ROUTINE.                                               
120600     MOVE PLANTED TO FINAL-25-ACRES.                                      
120700     MOVE GRAIN-PLANT TO FINAL-25-PLANT.                                  
120800     MOVE FINAL-25 TO PRINT-RECORD.                                       
120900     PERFORM PRINT-ROUTINE.                                               
121000     MOVE FINAL-26 TO PRINT-RECORD.                                       
121100     PERFORM PRINT-ROUTINE.                                               
121200     MOVE HARVESTD TO FINAL-27-HARVESTED.                                 
121300     MOVE FINAL-27 TO PRINT-RECORD.                                       
121400     PERFORM PRINT-ROUTINE.                                               
121410     MOVE TOT-FERT TO FINAL-28-BUSHELS.                                   
121500     MOVE FINAL-28 TO PRINT-RECORD.                                       
121600     PERFORM PRINT-ROUTINE.                                               
121700     MOVE HARVEST-LST TO FINAL-29-HARVEST.                                
121800     MOVE FINAL-29 TO PRINT-RECORD.                                       
121900     PERFORM PRINT-ROUTINE.                                               
122000     PERFORM PRINT-ROUTINE.                                               
122100     GOBACK.                                                              
122200 PRINT-ROUTINE.                                                           
122201     SET PX TO 1.                                                         
122202     SET PY TO 1.                                                         
122203     IF PRINT-BYTE (1) = ' '                                              
122204         MOVE 'Y' TO LAST-BYTE-MOVED-SWITCH                               
122205     ELSE                                                                 
122206         MOVE 'N' TO LAST-BYTE-MOVED-SWITCH.                              
122210     PERFORM SQUEEZE-OUT-BLANKS                                           
122220         UNTIL PY > 80.                                                   
122230     PERFORM CLEAR-TO-END                                                 
122240         UNTIL PX > 80.                                                   
122300     WRITE PRINT-RECORD.                                                  
122400     MOVE ALL ' ' TO PRINT-RECORD.                                        
122440 SQUEEZE-OUT-BLANKS.                                                      
122452     IF LAST-BYTE-MOVED-WAS-BLANK                                         
122453         IF PRINT-BYTE (PY) = ' '                                         
122454             SET PX DOWN BY 1                                             
122460         ELSE                                                             
122470             MOVE PRINT-BYTE (PY) TO PRINT-BYTE (PX)                      
122471             MOVE 'N' TO LAST-BYTE-MOVED-SWITCH                           
122491     ELSE                                                                 
122492         MOVE PRINT-BYTE (PY) TO PRINT-BYTE (PX)                          
122493         IF PRINT-BYTE (PX) = ' '                                         
122494             MOVE 'Y' TO LAST-BYTE-MOVED-SWITCH.                          
122499     SET PX UP BY 1.                                                      
122500     SET PY UP BY 1.                                                      
122501 CLEAR-TO-END.                                                            
122503     MOVE ' ' TO PRINT-BYTE (PX).                                         
122504     SET PX UP BY 1.                                                      
122510 RANDOMIZE.                                                               
122600     PERFORM GET-TIME.                                                    
122700     MOVE SEC-HUNDREDTHS TO RAND-1.                                       
122800     MOVE SEC-TENTHS TO RAND-2.                                           
122900     MOVE SEC-UNITS TO RAND-3.                                            
123000     PERFORM GET-TIME UNTIL SEC-HUNDREDTHS NOT = RAND-1.                  
123100     MOVE SEC-HUNDREDTHS TO RAND-4.                                       
123200     MOVE SEC-TENTHS TO RAND-5.                                           
123300     MOVE SEC-UNITS TO RAND-6.                                            
123400     MOVE WORK-RAND TO NBR-RAND.                                          
123800 GET-TIME.                                                                
123900     ACCEPT WORK-TIME FROM TIME.                                          
124000 GET-REPLY.                                                               
124100     MOVE ' ' TO L.                                                       
124200     MOVE +0 TO X.                                                        
124300     MOVE ZEROES TO IN-NUMBER.                                            
124500     READ INPUT-FILE                                                      
124600         AT END GO TO FINIS.                                              
124700     IF INPUT-RECORD IS EQUAL TO ALL ' '                                  
124710         MOVE 'N' TO INPUT-NUMERIC-SWITCH                                 
124900     ELSE                                                                 
125000         MOVE 'Y' TO INPUT-NUMERIC-SWITCH                                 
125100         SET IX TO 1                                                      
125200         PERFORM FIND-LEFT-END                                            
125300             UNTIL INPUT-BYTE (IX) NOT = ' '                              
125400         MOVE INPUT-BYTE (IX) TO L                                        
125500         SET IY TO 80                                                     
125600         PERFORM FIND-RIGHT-END                                           
125700             UNTIL INPUT-BYTE (IY) NOT = ' '                              
125800         PERFORM MOVE-INPUT-NUMBER                                        
125900             VARYING IZ FROM 15 BY -1 UNTIL                               
126000                 IY < IX OR                                               
126100                     X > 14.                                              
126200         IF INPUT-IS-NUMERIC                                              
126300             MOVE IN-NUMBER TO BIN-NUMBER                                 
126400         ELSE                                                             
126500             MOVE ZEROES TO BIN-NUMBER, IN-NUMBER.                        
126600 FIND-LEFT-END.                                                           
126700     IF INPUT-BYTE (IX) = ' '                                             
126800         SET IX UP BY 1.                                                  
126900 FIND-RIGHT-END.                                                          
127000     IF INPUT-BYTE (IY) = ' '                                             
127100         SET IY DOWN BY 1.                                                
127200 MOVE-INPUT-NUMBER.                                                       
127300     MOVE INPUT-BYTE (IY) TO IN-DIGIT (IZ).                               
127400     IF INPUT-BYTE (IY) IS < '0' OR                                       
127500         INPUT-BYTE (IY) IS > '9'                                         
127600             MOVE 'N' TO INPUT-NUMERIC-SWITCH.                            
127700     SET IY DOWN BY 1.                                                    
127800     ADD 1 TO X.                                                          

