000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID.    TSTGNLDT.                                                 
000300*AUTHOR. R THORNTON                                                       
000400*REMARKS. UTILITY PROGRAM TO TEST THE GENLDATE SUBROUTINE.                
000500 ENVIRONMENT DIVISION.                                                    
000600 CONFIGURATION SECTION.                                                   
000700 INPUT-OUTPUT SECTION.                                                    
000800 FILE-CONTROL.                                                            
000900     SELECT PRINT-FILE ASSIGN TO UT-S-PRINT1.                             
001000 DATA DIVISION.                                                           
001100 FILE SECTION.                                                            
001200 FD  PRINT-FILE                                                           
001300     RECORDING MODE IS F                                                  
001400     RECORD CONTAINS 80 CHARACTERS                                        
001500     BLOCK CONTAINS 0 RECORDS                                             
001600     LABEL RECORDS ARE STANDARD                                           
001700     DATA RECORD IS PRINT-LINE.                                           
001800 01  PRINT-LINE                  PIC X(80).                               
001900                                                                          
002000 WORKING-STORAGE SECTION.                                                 
002100 77  FILLER PIC X(36)  VALUE                                              
002200     'TSTGNLDT WORKING STORAGE BEGINS HERE'.                              
002300                                                                          
002400 01  DATEPARM.                                                            
002500     05  DATE-TYPE                           PIC X(01).                   
002600         88  JULIAN-SUPPLIED                 VALUE 'J'.                   
002700         88  GREGORIAN-SUPPLIED              VALUE 'G'.                   
002800     05  DATE-ERROR                          PIC X(01).                   
002900         88  NO-DATE-ERROR                   VALUE ' '.                   
003000         88  DATE-IN-ERROR                   VALUE 'E'.                   
003100     05  DAY-OF-THE-WEEK                     PIC X(01).                   
003200         88  MONDAY                          VALUE '1'.                   
003300         88  TUESDAY                         VALUE '2'.                   
003400         88  WEDNESDAY                       VALUE '3'.                   
003500         88  THURSDAY                        VALUE '4'.                   
003600         88  FRIDAY                          VALUE '5'.                   
003700         88  SATURDAY                        VALUE '6'.                   
003800         88  SUNDAY                          VALUE '7'.                   
003900     05  HOLIDAY-INDICATOR                   PIC X(01).                   
004000         88  NOT-A-HOLIDAY                   VALUE ' '.                   
004100         88  NEW-YEARS-DAY                   VALUE '1'.                   
004200         88  WASHINGTONS-BIRTHDAY            VALUE '2'.                   
004300         88  GOOD-FRIDAY                     VALUE '3'.                   
004400         88  MEMORIAL-DAY                    VALUE '4'.                   
004500         88  INDEPENDENCE-DAY                VALUE '5'.                   
004600         88  LABOR-DAY                       VALUE '6'.                   
004700         88  THANKSGIVING-DAY                VALUE '7'.                   
004800         88  THANKSGIVING-FRIDAY             VALUE '8'.                   
004900         88  CHRISTMAS-DAY                   VALUE '9'.                   
005000     05  YEAR-TYPE                           PIC X(01).                   
005100         88  NOT-LEAP-YEAR                   VALUE '0'.                   
005200         88  LEAP-YEAR                       VALUE '1'.                   
005300     05  JULIAN-DATE                         PIC 9(05) COMP-3.            
005400     05  GREGORIAN-DATE                      PIC 9(06).                   
005500     05  LITERAL-DATE                        PIC X(18).                   
005600     05  JULIAN-SLASH                        PIC X(06).                   
005700     05  GREGORIAN-SLASH                     PIC X(08).                   
005800     05  DAY-LITERAL                         PIC X(09).                   
005900     05  MONTH-LITERAL                       PIC X(09).                   
006000                                                                          
006100                                                                          
006200 01  WS-PRINT-LINE.                                                       
006300     05  FILLER                  PIC X(01) VALUE SPACE.                   
006400     05  PDATE-ERROR             PIC X(01).                               
006500     05  FILLER                  PIC X(01) VALUE SPACE.                   
006600     05  PDAY-OF-THE-WEEK        PIC X(01).                               
006700     05  FILLER                  PIC X(01) VALUE SPACE.                   
006800     05  PHOLIDAY-INDICATOR      PIC X(01).                               
006900     05  FILLER                  PIC X(01) VALUE SPACE.                   
007000     05  PYEAR-TYPE              PIC X(01).                               
007100     05  FILLER                  PIC X(01) VALUE SPACE.                   
007200     05  PJULIAN-DATE            PIC 9(05) COMP-3.                        
007300     05  FILLER                  PIC X(01) VALUE SPACE.                   
007400     05  PGREGORIAN-DATE         PIC 9(06).                               
007500     05  FILLER                  PIC X(01) VALUE SPACE.                   
007600     05  PLITERAL-DATE           PIC X(18).                               
007700     05  FILLER                  PIC X(01) VALUE SPACE.                   
007800     05  PJULIAN-SLASH           PIC X(06).                               
007900     05  FILLER                  PIC X(01) VALUE SPACE.                   
008000     05  PGREGORIAN-SLASH        PIC X(08).                               
008100     05  FILLER                  PIC X(01) VALUE SPACE.                   
008200     05  PDAY-LITERAL            PIC X(09).                               
008300     05  FILLER                  PIC X(01) VALUE SPACE.                   
008400     05  PMONTH-LITERAL          PIC X(09).                               
008500                                                                          
008600 PROCEDURE DIVISION.                                                      
008700     OPEN OUTPUT PRINT-FILE.                                              
008800     MOVE SPACES TO DATEPARM.                                             
008900     MOVE 'J' TO DATE-TYPE.                                               
009000     MOVE 90001 TO JULIAN-DATE.                                           
009100     PERFORM B100-PRINT-DATE THRU B100-EXIT                               
009200         VARYING JULIAN-DATE FROM 90001 BY 1                              
009300         UNTIL JULIAN-DATE IS GREATER THAN 90366.                         
009400     CLOSE PRINT-FILE.                                                    
009500     GOBACK.                                                              
009600 B100-PRINT-DATE.                                                         
009700     CALL 'GENLDATE' USING DATEPARM.                                      
009800     IF NOT-A-HOLIDAY AND NO-DATE-ERROR                                   
009900         GO TO B100-EXIT.                                                 
010000     MOVE DATE-ERROR TO PDATE-ERROR.                                      
010100     MOVE DAY-OF-THE-WEEK TO PDAY-OF-THE-WEEK.                            
010200     MOVE  HOLIDAY-INDICATOR TO PHOLIDAY-INDICATOR.                       
010300     MOVE  YEAR-TYPE TO PYEAR-TYPE.                                       
010400     MOVE  JULIAN-DATE TO PJULIAN-DATE.                                   
010500     MOVE  GREGORIAN-DATE TO PGREGORIAN-DATE.                             
010600     MOVE  LITERAL-DATE TO PLITERAL-DATE.                                 
010700     MOVE  JULIAN-SLASH TO PJULIAN-SLASH.                                 
010800     MOVE  GREGORIAN-SLASH TO PGREGORIAN-SLASH.                           
010900     MOVE  DAY-LITERAL TO PDAY-LITERAL.                                   
011000     MOVE  MONTH-LITERAL TO PMONTH-LITERAL.                               
011100     WRITE PRINT-LINE FROM WS-PRINT-LINE.                                 
011200 B100-EXIT. EXIT.                                                         

