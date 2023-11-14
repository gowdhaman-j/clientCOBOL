000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID.    GENLDAT2.                                                 
000300 AUTHOR. R THORNTON                                                       
000800***********************************************************************   
000500* GENLDAT2 IS A FUNCTIONAL EQUIVALENT IN COBOL FOR THE ORIGINAL  *        
000600* ASSEMBLER VERSION. IT WAS REWRITTEN IN COBOL/390 TO IMPROVE ITS*        
000700* MAINTAINABILITY AND TO PROVIDE OPERATION BOTH ABOVE AND BELOW  *        
000710* THE 16M LINE.                                                  *        
000711*                                                                *        
000712*                 **** TESTING ****                              *        
000713* CALLGNDT IS A TEST DRIVER THAT CAN BE USED TO TEST CHANGES TO  *        
000714* THIS SUBROUTINE. SEE ALSO THE JCL FOR CALLGND0 AND CALLGND9    *        
000720*                                                                *        
000800******************************************************************        
000900* THIS SUBROUTINE PROVIDES A TABLE OF INFORMATION ABOUT THE DATE *        
000910* RECEIVED FROM THE CALLER. INFORMATION RETURNED TO THE CALLER   *        
001000* INCLUDES DAY OF WEEK, HOLIDAY INDICATION, AND VARIOUS DATE     *        
001010* FORMATS.                                                       *        
001100* THE CALLER MAY SUPPLY THE DATE TO BE ANALYZED IN EITHER JULIAN *        
001200* OR GREGORIAN FORMAT.                                           *        
001210* GENLDAT2 IS VALID FOR DATES FROM JAN 1, 1900 THRU DEC 31, 2099.*        
001400*                                                                *        
001500* A SINGLE PARAMETER IS SUPPLIED, FORMATTED AS FOLLOWS:          *        
001600*               BYTE 1     DATE SENT BY CALLER: J=JULIAN DATE IS *        
001700*                          SUPPLIED, G=GREGORIAN DATE SUPPLIED.  *        
001800*               BYTE 2     ERROR INDICATOR. BLANK=SUCCESSFUL DATE*        
001900*                          PROCESSING. E=ERROR IN DATE SUPPLIED. *        
002000*               BYTE 3     DAY OF WEEK INDICATOR: 1=MONDAY,      *        
002100*                          2=TUESDAY, 3=WEDNESDAY, 4=THURSDAY,   *        
002200*                          5=FRIDAY, 6=SATURDAY, 7=SUNDAY.       *        
002300*               BYTE 4     HOLIDAY INDICATOR: BLANK=NOT A HOLIDAY*        
002400*                          1=NEW YEAR'S DAY, 2=(NOT USED),       *        
002500*                          3=(NOT USED),  4=MEMORIAL DAY,        *        
002600*                          5=INDEPENDENCE DAY, 6=LABOR DAY,      *        
002700*                          7=THANKSGIVING DAY, 8=FRIDAY AFTER    *        
002800*                          THANKSGIVING, 9=CHRISTMAS DAY         *        
002900*               BYTE 5     LEAP YEAR INDICATOR: 0=NOT LEAP YEAR, *        
003000*                          1=LEAP YEAR                           *        
003100*              BYTES 6-9   JULIAN DATE. IF DATE SENT  IS J THIS  *        
003200*                          FIELD IS SUPPLIED BY THE CALLER. IF   *        
003210*                          NOT, THE JULIAN EQUIVALENT OF THE     *        
003300*                          GREGORIAN DATE SUPPLIED WILL BE       *        
003400*                          RETURNED HERE. THIS IS AN UNSIGNED,   *        
003500*                          PACKED DECIMAL FIELD WITH THE FORMAT  *        
003600*                          YYYYDDDF                              *        
003700*              BYTES 10-17 GREGORIAN DATE. IF DATE SENT IS G THIS*        
003800*                          IS SUPPLIED BY THE CALLER. IF NOT, THE*        
003900*                          GREGORIAN EQUIVALENT OF THE JULIAN    *        
004000*                          DATE SUPPLIED WILL BE RETURNED. THIS  *        
004100*                          IS AN UNSIGNED ZONED DECIMAL FIELD    *        
004200*                          WITH THE FORMAT MMDDYYYY              *        
004300*             BYTES 18-35  A LITERAL DATE, LEFT JUSTIFIED,       *        
004400*                          IN AN 18-BYTE FIELD.                  *        
004500*                          EXAMPLES ARE: JANUARY 16, 1979        *        
004600*                                        MAY 9, 1982             *        
004700*             BYTES 36-43  JULIAN DATE WITH SLASH: YYYY/DDD.     *        
004800*             BYTES 44-53  GREGORIAN DATE WITH SLASHES:          *        
004900*                          MM/DD/YYYY                            *        
005000*                                                                *        
005100*  AUTHOR R. THORNTON  DEC 2001                                  *        
005200******************************************************************        
005300*                   MAINTENANCE CHANGES                          *        
005310******************************************************************        
005500*                                                                *        
005600*                                                                *        
005700*                                                                *        
005800*                                                                *        
005900*                                                                *        
006000*                                                                *        
006100*                                                                *        
006200******************************************************************        
006300 ENVIRONMENT DIVISION.                                                    
006400 CONFIGURATION SECTION.                                                   
006500 INPUT-OUTPUT SECTION.                                                    
006600 FILE-CONTROL.                                                            
006700 DATA DIVISION.                                                           
006800 FILE SECTION.                                                            
006900 WORKING-STORAGE SECTION.                                                 
007000 77  FILLER PIC X(36)  VALUE                                              
007100     'GENLDAT2 WORKING STORAGE BEGINS HERE'.                              
007200 01  CONVERSION-AREAS.                                                    
007300    05  GREGCV1.                                                          
007400        10  JANUARY-OFFSET     PIC S9(3) COMP-3 VALUE +0.                 
007500        10  FEBRUARY-OFFSET    PIC S9(3) COMP-3 VALUE +31.                
007600        10  MARCH-OFFSET       PIC S9(3) COMP-3 VALUE +59.                
007700        10  APRIL-OFFSET       PIC S9(3) COMP-3 VALUE +90.                
007800        10  MAY-OFFSET         PIC S9(3) COMP-3 VALUE +120.               
007900        10  JUNE-OFFSET        PIC S9(3) COMP-3 VALUE +151.               
008000        10  JULY-OFFSET        PIC S9(3) COMP-3 VALUE +181.               
008100        10  AUGUST-OFFSET      PIC S9(3) COMP-3 VALUE +212.               
008200        10  SEPTEMBER-OFFSET   PIC S9(3) COMP-3 VALUE +243.               
008300        10  OCTOBER-OFFSET     PIC S9(3) COMP-3 VALUE +273.               
008400        10  NOVEMBER-OFFSET    PIC S9(3) COMP-3 VALUE +304.               
008500        10  DECEMBER-OFFSET    PIC S9(3) COMP-3 VALUE +334.               
008600        10  TABLE-END-VALUE    PIC S9(3) COMP-3 VALUE +999.               
008700    05  MONTH-OFFSET REDEFINES GREGCV1 PIC S9(3) COMP-3                   
008800                               OCCURS 13 TIMES.                           
008900    05  GRGVLDY.                                                          
009000        10 DAYS-IN-JANUARY     PIC 99 VALUE 31.                           
009100        10 DAYS-IN-FEBRUARY    PIC 99 VALUE 28.                           
009200        10 DAYS-IN-MARCH       PIC 99 VALUE 31.                           
009300        10 DAYS-IN-APRIL       PIC 99 VALUE 30.                           
009400        10 DAYS-IN-MAY         PIC 99 VALUE 31.                           
009500        10 DAYS-IN-JUNE        PIC 99 VALUE 30.                           
009600        10 DAYS-IN-JULY        PIC 99 VALUE 31.                           
009700        10 DAYS-IN-AUGUST      PIC 99 VALUE 31.                           
009800        10 DAYS-IN-SEPTEMBER   PIC 99 VALUE 30.                           
009900        10 DAYS-IN-OCTOBER     PIC 99 VALUE 31.                           
010000        10 DAYS-IN-NOVEMBER    PIC 99 VALUE 30.                           
010100        10 DAYS-IN-DECEMBER    PIC 99 VALUE 31.                           
010200    05  DAYS-IN-MONTH REDEFINES GRGVLDY PIC 99                            
010300                               OCCURS 12 TIMES.                           
010400    05  NEWYR                  PIC X VALUE '0'.                           
010500        88 DEC-31-THIS-YEAR-IS-NEW-YEARS VALUE IS '1'.                    
010600    05  HOLIDAY-FOUND-INDICATOR         PIC X.                            
010700        88  HOLIDAY-DATE-FOUND            VALUE IS 'Y'.                   
010800    05  YEARDATA.                                                         
010900        10  YEAR-START-DAY              PIC 9.                            
011000            88 YEAR-STARTS-ON-MONDAY      VALUE IS 0.                     
011100            88 YEAR-STARTS-ON-TUESDAY     VALUE IS 1.                     
011200            88 YEAR-STARTS-ON-WEDNESDAY   VALUE IS 2.                     
011300            88 YEAR-STARTS-ON-THURSDAY    VALUE IS 3.                     
011400            88 YEAR-STARTS-ON-FRIDAY      VALUE IS 4.                     
011500            88 YEAR-STARTS-ON-SATURDAY    VALUE IS 5.                     
011600            88 YEAR-STARTS-ON-SUNDAY      VALUE IS 6.                     
011700        10  LEAP-YEAR-INDICATOR             PIC X.                        
011800            88  THIS-YEAR-IS-A-LEAP-YEAR      VALUE IS '1'.               
011900            88  THIS-YEAR-IS-NOT-A-LEAP-YEAR  VALUE IS '0'.               
012000        10  YRHOL.                                                        
012100            15  HOLIDAYS-THIS-YEAR          OCCURS 8 TIMES.               
012200                20  HOLIDAY-IDENTIFIER      PIC X.                        
012300                    88  NEW-YEARS-DAY         VALUE IS '1'.               
012400                    88  MEMORIAL-DAY          VALUE IS '4'.               
012500                    88  INDEPENDENCE-DAY      VALUE IS '5'.               
012600                    88  LABOR-DAY             VALUE IS '6'.               
012700                    88  THANKSGIVING-DAY      VALUE IS '7'.               
012800                    88  DAY-AFTER-THANKSGIVING VALUE IS '8'.              
012900                    88  CHRISTMAS-DAY         VALUE IS '9'.               
013000                    88  NULL-HOLIDAY-ENTRY    VALUE IS '0'.               
013100                20  HOLIDAY-DATE.                                         
013200                    25  HOLIDAY-MONTH         PIC 99.                     
013300                    25  HOLIDAY-DAY           PIC 99.                     
013400    05  HSUB                           PIC S9(5) COMP-3.                  
013500    05  CPRDAY                         PIC S9(5) COMP-3.                  
013600    05  BLANX                          PIC X(20) VALUE SPACES.            
013700    05  UNPACKED-JULIAN-DATE           PIC 9(7).                          
013800    05  FILLER REDEFINES UNPACKED-JULIAN-DATE.                            
013900        10  UNPACKED-JULIAN-YEAR       PIC 9(4).                          
014000        10  UNPACKED-JULIAN-DAY        PIC 9(3).                          
014100    05  MTH                            PIC S9(5) COMP-3.                  
014200    05  JULIAN-MONTH-FOUND-INDICATOR   PIC X.                             
014300        88 JULIAN-MONTH-FOUND          VALUE 'Y'.                         
014400    05  WORK1                          PIC S9(5) COMP-3.                  
014500    05  WORK2                          PIC S9(5) COMP-3.                  
014600    05  OFFSET                         PIC S9(5) COMP-3.                  
014700    05  YRSUB                          PIC S9(5) COMP-3.                  
014800*********************************************************************     
014900*        TABLE OF LITERAL MONTH NAMES                            *        
015000*********************************************************************     
015100 01  MONTH-NAME-TABLE.                                                    
015200     05  MONTH-NAMES.                                                     
015300         10 FILLER           PIC X(9) VALUE 'JANUARY'.                    
015400         10 FILLER           PIC S9(2) COMP VALUE +9.                     
015500         10 FILLER           PIC X(9) VALUE 'FEBRUARY'.                   
015600         10 FILLER           PIC S9(2) COMP VALUE +10.                    
015700         10 FILLER           PIC X(9) VALUE 'MARCH'.                      
015800         10 FILLER           PIC S9(2) COMP VALUE +7.                     
015900         10 FILLER           PIC X(9) VALUE 'APRIL'.                      
016000         10 FILLER           PIC S9(2) COMP VALUE +7.                     
016100         10 FILLER           PIC X(9) VALUE 'MAY'.                        
016200         10 FILLER           PIC S9(2) COMP VALUE +5.                     
016300         10 FILLER           PIC X(9) VALUE 'JUNE'.                       
016400         10 FILLER           PIC S9(2) COMP VALUE +6.                     
016500         10 FILLER           PIC X(9) VALUE 'JULY'.                       
016600         10 FILLER           PIC S9(2) COMP VALUE +6.                     
016700         10 FILLER           PIC X(9) VALUE 'AUGUST'.                     
016800         10 FILLER           PIC S9(2) COMP VALUE +8.                     
016900         10 FILLER           PIC X(9) VALUE 'SEPTEMBER'.                  
017000         10 FILLER           PIC S9(2) COMP VALUE +11.                    
017100         10 FILLER           PIC X(9) VALUE 'OCTOBER'.                    
017200         10 FILLER           PIC S9(2) COMP VALUE +9.                     
017300         10 FILLER           PIC X(9) VALUE 'NOVEMBER'.                   
017400         10 FILLER           PIC S9(2) COMP VALUE +10.                    
017500         10 FILLER           PIC X(9) VALUE 'DECEMBER'.                   
017600         10 FILLER           PIC S9(2) COMP VALUE +10.                    
017700     05  MONTH-ENTRY REDEFINES MONTH-NAMES,                               
017800                             OCCURS 12 TIMES.                             
017900         10  MONTH-NAME      PIC X(9).                                    
018000         10  MONTH-SIZE      PIC S9(2) COMP.                              
018100*********************************************************************     
018200*        TABLE OF LITERAL DAY-OF-THE-WEEK NAMES                  *        
018300*********************************************************************     
018400    05  DWKTBL.                                                           
018500        10  FILLER                    PIC X(9) VALUE 'MONDAY'.            
018600        10  FILLER                    PIC X(9) VALUE 'TUESDAY'.           
018700        10  FILLER                    PIC X(9) VALUE 'WEDNESDAY'.         
018800        10  FILLER                    PIC X(9) VALUE 'THURSDAY'.          
018900        10  FILLER                    PIC X(9) VALUE 'FRIDAY'.            
019000        10  FILLER                    PIC X(9) VALUE 'SATURDAY'.          
019100        10  FILLER                    PIC X(9) VALUE 'SUNDAY'.            
019200    05  DAY-OF-THE-WEEK-NAME REDEFINES DWKTBL OCCURS 7 TIMES              
019300                                      PIC X(9).                           
019400*********************************************************************     
019500*YEARTBL1 CONTAINS 1-BYTE SUBSCRIPTS TO YEARTBL2. WHEN THE NEXT  *        
019600*YEAR IS 07 OR 14, JAN 1ST NEXT YEAR FALLS ON SATURDAY, AND WILL *        
019700*BE OBSERVED ON DECEMBER 31ST OF THIS YEAR (THE PRECEDING FRIDAY)*        
019800*********************************************************************     
019900    05  YEARTBL1.                                                         
020000        10 YRS-1900-1909 PIC X(20) VALUE '09030405130102031106'.          
020100        10 YRS-1910-1919 PIC X(20) VALUE '07010904050614020304'.          
020200        10 YRS-1920-1929 PIC X(20) VALUE '12070102100506070803'.          
020300        10 YRS-1930-1939 PIC X(20) VALUE '04051301020311060701'.          
020400        10 YRS-1940-1949 PIC X(20) VALUE '09040506140203041207'.          
020500        10 YRS-1950-1959 PIC X(20) VALUE '01021005060708030405'.          
020600        10 YRS-1960-1969 PIC X(20) VALUE '13010203110607010904'.          
020700        10 YRS-1970-1979 PIC X(20) VALUE '05061402030412070102'.          
020800        10 YRS-1980-1989 PIC X(20) VALUE '10050607080304051301'.          
020900        10 YRS-1990-1999 PIC X(20) VALUE '02031106070109040506'.          
021000        10 YRS-2000-2009 PIC X(20) VALUE '14020304120701021005'.          
021100        10 YRS-2010-2019 PIC X(20) VALUE '06070803040513010203'.          
021200        10 YRS-2020-2029 PIC X(20) VALUE '11060701090405061402'.          
021300        10 YRS-2030-2039 PIC X(20) VALUE '03041207010210050607'.          
021400        10 YRS-2040-2049 PIC X(20) VALUE '08030405130102031106'.          
021500        10 YRS-2050-2059 PIC X(20) VALUE '07010904050614020304'.          
021600        10 YRS-2060-2069 PIC X(20) VALUE '12070102100506070803'.          
021700        10 YRS-2070-2079 PIC X(20) VALUE '04051301020311060701'.          
021800        10 YRS-2080-2089 PIC X(20) VALUE '09040506140203041207'.          
021900        10 YRS-2090-2099 PIC X(20) VALUE '01021005060708030405'.          
022000        10 YEAR-2100     PIC X(2)  VALUE '06'.                            
022100    05  YEAR-SUBSCRIPT REDEFINES YEARTBL1 OCCURS 201 TIMES                
022200                                          PIC 99.                         
022300*********************************************************************     
022400* YEARTBL2 CONTAINS DATA ABOUT THE YEAR: DAY OF WEEK FOR JANUARY *        
022500* 1ST, LEAP YEAR INDICATOR, AND HOLIDAY DATES. FORMAT IS:        *        
022600*     BYTE 1  : DAY OF WEEK FOR JANUARY 1ST, 0=MONDAY - 6=SUNDAY *        
022700*     BYTE 2  : LEAP YEAR INDICATOR, 1=LEAP YEAR ELSE ZERO.      *        
022800*     BYTES 3-42: 8 HOLIDAY FIELDS OF 5 BYTES EACH. FORMAT OF    *        
022900*                 EACH FIELD IS:                                 *        
023000*                   BYTE 1 : HOLIDAY TYPE INDICATOR (SAME AS IN  *        
023100*                            PARAMETER AREA)                     *        
023200*                   BYTES 2-5: MMDD DATE OF THE HOLIDAY          *        
023300*********************************************************************     
023400    05  YEARTBL2.                                                         
023500        10  NON-LEAP-YEAR-STARTING-SUN   PIC X(42)                        
023600              VALUE '6010102 0220405295070460904711238112491225'.         
023700        10  NON-LEAP-YEAR-STARTING-MON   PIC X(42)                        
023800              VALUE '0010101 0219405285070460903711228112391225'.         
023900        10  NON-LEAP-YEAR-STARTING-TUE   PIC X(42)                        
024000              VALUE '1010101 0218405275070460902711288112991225'.         
024100        10  NON-LEAP-YEAR-STARTING-WED   PIC X(42)                        
024200              VALUE '2010101 0217405265070460901711278112891225'.         
024300        10  NON-LEAP-YEAR-STARTING-THUR  PIC X(42)                        
024400              VALUE '3010101 0216405255070360907711268112791225'.         
024500        10  NON-LEAP-YEAR-STARTING-FRI   PIC X(42)                        
024600              VALUE '4010101 0215405315070560906711258112691224'.         
024700        10  NON-LEAP-YEAR-STARTING-SAT   PIC X(42)                        
024800              VALUE '5000000 0221405305070460905711248112591226'.         
024900        10  LEAP-YEAR-STARTING-SUN       PIC X(42)                        
025000              VALUE '6110102 0220405285070460903711228112391225'.         
025100        10  LEAP-YEAR-STARTING-MON       PIC X(42)                        
025200              VALUE '0110101 0219405275070460902711288112991225'.         
025300        10  LEAP-YEAR-STARTING-TUES      PIC X(42)                        
025400              VALUE '1110101 0218405265070460901711278112891225'.         
025500        10  LEAP-YEAR-STARTING-WED       PIC X(42)                        
025600              VALUE '2110101 0217405255070360907711268112791225'.         
025700        10  LEAP-YEAR-STARTING-THUR      PIC X(42)                        
025800              VALUE '3110101 0216405315070560906711258112691224'.         
025900        10  LEAP-YEAR-STARTING-FRI       PIC X(42)                        
026000              VALUE '4110101 0215405305070460905711248112591226'.         
026100        10  LEAP-YEAR-STARTING-SAT       PIC X(42)                        
026200              VALUE '5100000 0221405295070460904711238112491225'.         
026300    05  YEAR-TABLE-2 REDEFINES YEARTBL2 OCCURS 14 TIMES                   
026400                                        PIC X(42).                        
031400                                                                          
031500 LINKAGE SECTION.                                                         
031600 01  PARAMETER-FIELD.                                                     
031700     05  DATE-TYPE                 PIC X(01).                             
031800         88  JULIAN-SUPPLIED                 VALUE 'J'.                   
031900         88  GREGORIAN-MDY-SUPPLIED          VALUE 'G'.                   
032000         88  GREGORIAN-YMD-SUPPLIED          VALUE 'Y'.                   
032100     05  DATE-ERROR                PIC X(01).                             
032200         88  NO-DATE-ERROR                   VALUE SPACE.                 
032300         88  YES-DATE-ERROR                  VALUE 'E'.                   
032400     05  DAY-OF-THE-WEEK           PIC 9(01).                             
032500         88  MONDAY                          VALUE 1.                     
032600         88  TUESDAY                         VALUE 2.                     
032700         88  WEDNESDAY                       VALUE 3.                     
032800         88  THURSDAY                        VALUE 4.                     
032900         88  FRIDAY                          VALUE 5.                     
033000         88  SATURDAY                        VALUE 6.                     
033100         88  SUNDAY                          VALUE 7.                     
033200     05  HOLIDAY-INDICATOR         PIC X(01).                             
033300         88  NOT-A-HOLIDAY                   VALUE ' '.                   
033400         88  NEW-YEARS-DAY                   VALUE '1'.                   
033600         88  MEMORIAL-DAY                    VALUE '4'.                   
033700         88  INDEPENDENCE-DAY                VALUE '5'.                   
033800         88  LABOR-DAY                       VALUE '6'.                   
033900         88  THANKSGIVING-DAY                VALUE '7'.                   
034000         88  THANKSGIVING-FRIDAY             VALUE '8'.                   
034100         88  CHRISTMAS-DAY                   VALUE '9'.                   
034200     05  YEAR-TYPE                 PIC X(01).                             
034300         88  NOT-LEAP-YEAR                   VALUE '0'.                   
034400         88  LEAP-YEAR                       VALUE '1'.                   
034500     05  JULIAN-DATE               PIC 9(07) COMP-3.                      
034600     05  GREGORIAN-MDY-DATE.                                              
034700         10  GREG-MDY-MONTH-DAY.                                          
034800             15  GREG-MDY-MONTH        PIC 99.                            
034900             15  GREG-MDY-DAY          PIC 99.                            
035000         10  GREG-MDY-YEAR         PIC 9(4).                              
035100     05  GREGORIAN-YMD-DATE.                                              
035200         10  GREG-YMD-YEAR         PIC 9(4).                              
035300         10  GREG-YMD-MONTH        PIC 99.                                
035400         10  GREG-YMD-DAY          PIC 99.                                
035500     05  LITERAL-DATE              PIC X(18).                             
035600     05  JULIAN-SLASH.                                                    
035700         10  JULIAN-SLASH-YEAR     PIC 9(4).                              
035800         10  JULIAN-SLASH-SLASH    PIC X.                                 
035900         10  JULIAN-SLASH-DAY      PIC 999.                               
036000     05  GREGORIAN-SLASH.                                                 
036100         10  GREG-SLASH-MONTH      PIC 99.                                
036200         10  GREG-SLASH-SLASH1     PIC X.                                 
036300         10  GREG-SLASH-DAY        PIC 99.                                
036400         10  GREG-SLASH-SLASH2     PIC X.                                 
036500         10  GREG-SLASH-YEAR       PIC 9(4).                              
036600     05  DAY-LITERAL               PIC X(09).                             
036700     05  MONTH-LITERAL             PIC X(09).                             
036800                                                                          
036900 PROCEDURE DIVISION USING PARAMETER-FIELD.                                
037000     MOVE SPACES TO DATE-ERROR,                                           
037100                    HOLIDAY-INDICATOR,                                    
037200                    LITERAL-DATE.                                         
037300     IF JULIAN-SUPPLIED                                                   
037400         PERFORM 1000-JULIAN-DATE-SUPPLIED THRU 1000-EXIT                 
037500     ELSE                                                                 
037600         IF GREGORIAN-YMD-SUPPLIED                                        
037700             PERFORM 2000-YMD-DATE-SUPPLIED THRU 2000-EXIT                
037800         ELSE                                                             
037900             IF GREGORIAN-MDY-SUPPLIED                                    
038000                 PERFORM 3000-MDY-DATE-SUPPLIED THRU 3000-EXIT            
038100             ELSE                                                         
038200                 MOVE 'E' TO DATE-ERROR.                                  
038300     GOBACK.                                                              
038400*********************************************************************     
038500*        CALLER PASSED A JULIAN DATE                             *        
038600*********************************************************************     
038700 1000-JULIAN-DATE-SUPPLIED.                                               
038800     IF JULIAN-DATE IS NUMERIC                                            
038900         NEXT SENTENCE                                                    
039000     ELSE                                                                 
039100         MOVE 'E' TO DATE-ERROR                                           
039200         GO TO 1000-EXIT.                                                 
039300     MOVE JULIAN-DATE TO UNPACKED-JULIAN-DATE.                            
039400     MOVE UNPACKED-JULIAN-YEAR TO GREG-YMD-YEAR.                          
039500     MOVE UNPACKED-JULIAN-YEAR TO GREG-MDY-YEAR.                          
039600     PERFORM 5000-LOCATE-YEAR-DATA THRU 5000-EXIT.                        
039700     IF UNPACKED-JULIAN-DAY = 0                                           
039800         MOVE 'E' TO DATE-ERROR                                           
039900         GO TO 1000-EXIT.                                                 
040000     IF UNPACKED-JULIAN-DAY > 366                                         
040100         MOVE 'E' TO DATE-ERROR                                           
040200         GO TO 1000-EXIT.                                                 
040300     IF UNPACKED-JULIAN-DAY = 366                                         
040400         IF THIS-YEAR-IS-A-LEAP-YEAR                                      
040500             NEXT SENTENCE                                                
040600         ELSE                                                             
040700             MOVE 'E' TO DATE-ERROR                                       
040800             GO TO 1000-EXIT.                                             
040900     IF THIS-YEAR-IS-A-LEAP-YEAR                                          
041000         IF UNPACKED-JULIAN-DAY = 60                                      
041100             MOVE 02 TO GREG-YMD-MONTH                                    
041200             MOVE 29 TO GREG-YMD-DAY                                      
041300             MOVE 02 TO GREG-MDY-MONTH                                    
041400             MOVE 29 TO GREG-MDY-DAY                                      
041500             PERFORM 6000-COMMON-DATE-ROUTINE THRU 6000-EXIT              
041600             GO TO 1000-EXIT                                              
041700         ELSE                                                             
041800             IF UNPACKED-JULIAN-DAY > 60                                  
041900                 SUBTRACT 1 FROM UNPACKED-JULIAN-DAY.                     
042000     PERFORM 1100-CONVERT-JULIAN-TO-GREG THRU 1100-EXIT.                  
042100     PERFORM 6000-COMMON-DATE-ROUTINE THRU 6000-EXIT.                     
042200 1000-EXIT.                                                               
042300     EXIT.                                                                
043800*********************************************************************     
043900*     CONVERT THE JULIAN-DAY (WHICH HAS BEEN ADJUSTED FOR LEAP   *        
044000*     YEAR) TO GREGORIAN MONTH AND DAY.                          *        
044100*********************************************************************     
044200 1100-CONVERT-JULIAN-TO-GREG.                                             
044300     MOVE 'N' TO JULIAN-MONTH-FOUND-INDICATOR.                            
044400     PERFORM 1110-FIND-JULIAN-MONTH THRU 1110-EXIT                        
044500         VARYING MTH FROM 1 BY 1 UNTIL                                    
044600             JULIAN-MONTH-FOUND.                                          
044700     SUBTRACT 2 FROM MTH.                                                 
044800     SUBTRACT MONTH-OFFSET (MTH) FROM UNPACKED-JULIAN-DAY.                
044900     MOVE UNPACKED-JULIAN-DAY TO GREG-YMD-DAY.                            
045000     MOVE UNPACKED-JULIAN-DAY TO GREG-MDY-DAY.                            
045100     MOVE MTH TO GREG-YMD-MONTH.                                          
045200     MOVE MTH TO GREG-MDY-MONTH.                                          
045300 1100-EXIT.                                                               
045400     EXIT.                                                                
045500*********************************************************************     
045600*     USE THE JULIAN DAY AS AN ARGUMENT TO LOCATE THE MONTH      *        
045700*     ENTRY IN THE JULIAN MONTH TABLE. THE TABLE CONTAINS THE    *        
045800*     CUMULATIVE NUMBER OF DAYS TO THE START OF EACH MONTH IN THE*        
045900*     YEAR.                                                      *        
046000*********************************************************************     
046100 1110-FIND-JULIAN-MONTH.                                                  
046200     IF UNPACKED-JULIAN-DAY <= MONTH-OFFSET (MTH)                         
046300         MOVE 'Y' TO JULIAN-MONTH-FOUND-INDICATOR.                        
046400 1110-EXIT.                                                               
046500     EXIT.                                                                
046600*********************************************************************     
046700*     CALLER PASSED A GREGORIAN YMD DATE                         *        
046800*********************************************************************     
046900 2000-YMD-DATE-SUPPLIED.                                                  
047000     MOVE GREG-YMD-MONTH TO GREG-MDY-MONTH.                               
047100     MOVE GREG-YMD-DAY TO GREG-MDY-DAY.                                 TE
047200     MOVE GREG-YMD-YEAR TO GREG-MDY-YEAR.                               TE
047300     PERFORM 4000-COMMON-GREGORIAN-ROUTINE THRU 4000-EXIT.                
047310     PERFORM 6000-COMMON-DATE-ROUTINE THRU 6000-EXIT.                     
047400 2000-EXIT.                                                               
047500     EXIT.                                                                
047600*********************************************************************     
047700*     CALLER PASSED A GREGORIAN MDY DATE                         *        
047800*********************************************************************     
047900 3000-MDY-DATE-SUPPLIED.                                                  
048000     MOVE GREG-MDY-MONTH TO GREG-YMD-MONTH.                               
048100     MOVE GREG-MDY-DAY TO GREG-YMD-DAY.                                 TE
048200     MOVE GREG-MDY-YEAR TO GREG-YMD-YEAR.                               TE
048300     PERFORM 4000-COMMON-GREGORIAN-ROUTINE THRU 4000-EXIT.                
048310     PERFORM 6000-COMMON-DATE-ROUTINE THRU 6000-EXIT.                     
048400 3000-EXIT.                                                               
048500     EXIT.                                                                
050000*********************************************************************     
050100* EDIT THE GREGORIAN MDY DATE: THIS VALIDITY CHECKING LOGIC      *        
050200* IS DONE WHENEVER ANY GREGORIAN DATE IS RECEIVED (MDY OR YMD)   *        
050300*********************************************************************     
050400 4000-COMMON-GREGORIAN-ROUTINE.                                           
050500     IF GREGORIAN-MDY-DATE IS NUMERIC                                     
050600         NEXT SENTENCE                                                    
050700     ELSE                                                                 
050800         MOVE 'E' TO DATE-ERROR                                           
050900         GO TO 4000-EXIT.                                                 
051000     IF GREG-MDY-DAY < 01 OR                                              
051100        GREG-MDY-MONTH < 01 OR                                            
051200        GREG-MDY-MONTH > 12                                               
051300             MOVE 'E' TO DATE-ERROR                                       
051400             GO TO 4000-EXIT.                                             
051500     PERFORM 5000-LOCATE-YEAR-DATA THRU 5000-EXIT.                        
051600     IF GREG-MDY-DAY > DAYS-IN-MONTH (GREG-MDY-MONTH)                     
051700         IF GREG-MDY-MONTH = 02 AND                                       
051800            GREG-MDY-DAY = 29 AND                                         
051810            THIS-YEAR-IS-A-LEAP-YEAR                                      
051820             NEXT SENTENCE                                                
051830         ELSE                                                             
051840             MOVE 'E' TO DATE-ERROR                                       
051850             GO TO 4000-EXIT.                                             
051860     MOVE GREG-MDY-YEAR TO UNPACKED-JULIAN-YEAR.                          
051870     MOVE MONTH-OFFSET (GREG-MDY-MONTH) TO UNPACKED-JULIAN-DAY.           
051880     ADD GREG-MDY-DAY TO UNPACKED-JULIAN-DAY.                             
051890     IF THIS-YEAR-IS-A-LEAP-YEAR AND                                      
051891        GREG-MDY-MONTH > 02                                               
051892         ADD 1 TO UNPACKED-JULIAN-DAY.                                    
051893     MOVE UNPACKED-JULIAN-DATE TO JULIAN-DATE.                            
051894 4000-EXIT.                                                               
051895     EXIT.                                                                
051896*********************************************************************     
051897*     DO YEAR TABLE LOOKUPS. A TWO-TABLE LOOKUP IS USED, THE     *        
051898*     FIRST TABLE HAS ENTRIES FOR ALL YEARS FROM 1900 TO 2100    *        
051899*     AND CONTAINS SUBSCRIPT VALUES TO LOCATE THE APPROPRIATE    *        
051900*     ENTRY IN THE SECOND TABLE. THE SECOND TABLE (YEARTBL2)     *        
051901*     HAS 14 ENTRIES, ONE FOR EACH UNIQUE YEAR, WHICH HAVE THE   *        
051902*     SPECIFIC INFORMATION ABOUT THAT YEAR, SUCH AS HOLIDAY DATES*        
051903*     FIRST, THE ENTRY FOR NEXT YEAR IS FOUND TO SEE IF IT BEGINS*        
051904*     ON SATURDAY. IF IT DOES, THE NEWYR INDICATOR IS SET TO SHOW*        
051905*     THAT NEW YEARS DAY FOR NEXT YEAR WILL BE CELEBRATED ON DEC *        
051906*     31ST OF THIS YEAR.                                         *        
051907*********************************************************************     
051908 5000-LOCATE-YEAR-DATA.                                                   
051909     MOVE '0' TO NEWYR.                                                   
051910     MOVE GREG-MDY-YEAR TO YRSUB.                                         
051911     SUBTRACT 1898 FROM YRSUB.                                            
051912     IF YEAR-SUBSCRIPT (YRSUB) = 07 OR                                    
051913         YEAR-SUBSCRIPT (YRSUB) = 14                                      
051915             MOVE '1' TO NEWYR.                                           
051917     SUBTRACT 1 FROM YRSUB.                                               
051918     MOVE YEAR-SUBSCRIPT (YRSUB) TO YRSUB.                                
051919     MOVE YEAR-TABLE-2 (YRSUB) TO YEARDATA.                               
051920     MOVE LEAP-YEAR-INDICATOR TO YEAR-TYPE.                               
051921 5000-EXIT.                                                               
051922     EXIT.                                                                
051930*********************************************************************     
052000*     COMMON DATE PROCESSING                                     *        
052100*********************************************************************     
052200 6000-COMMON-DATE-ROUTINE.                                                
052210     MOVE JULIAN-DATE TO UNPACKED-JULIAN-DATE.                            
052300     MOVE UNPACKED-JULIAN-YEAR TO JULIAN-SLASH-YEAR.                      
052400     MOVE '/' TO JULIAN-SLASH-SLASH.                                      
052500     MOVE UNPACKED-JULIAN-DAY TO JULIAN-SLASH-DAY.                        
052600     MOVE GREG-MDY-MONTH TO GREG-SLASH-MONTH.                             
052700     MOVE '/' TO GREG-SLASH-SLASH1.                                       
052800     MOVE GREG-MDY-DAY TO GREG-SLASH-DAY.                                 
052900     MOVE '/' TO GREG-SLASH-SLASH2.                                       
053000     MOVE GREG-MDY-YEAR TO GREG-SLASH-YEAR.                               
053100     PERFORM 6100-CHECK-FOR-HOLIDAY THRU 6100-EXIT.                       
053200     PERFORM 6200-DAY-OF-WEEK-PROCESSING THRU 6200-EXIT.                  
053300     PERFORM 6300-BUILD-LITERAL-DATE THRU 6300-EXIT.                      
053400 6000-EXIT.                                                               
053500     EXIT.                                                                
053600*********************************************************************     
053700*     DETERMINE IF DATE IS A HOLIDAY                             *        
053800*********************************************************************     
053900 6100-CHECK-FOR-HOLIDAY.                                                  
054000     IF GREG-MDY-MONTH-DAY = 1231 AND                                     
054100        DEC-31-THIS-YEAR-IS-NEW-YEARS                                     
054200         MOVE '1' TO HOLIDAY-INDICATOR                                    
054300         GO TO 6100-EXIT.                                                 
054900     MOVE 'N' TO HOLIDAY-FOUND-INDICATOR.                                 
055000     PERFORM 6110-SEARCH-HOLIDAY-TABLE THRU 6110-EXIT                     
055100             VARYING HSUB FROM 1 BY 1                                     
055200         UNTIL HSUB > 8 OR HOLIDAY-DATE-FOUND.                            
055300 6100-EXIT.                                                               
055400     EXIT.                                                                
055500*********************************************************************     
055600*     SEARCH TABLE OF HOLIDAYS THIS YEAR TO SEE IF THE DATE IS   *        
055700*     A HOLIDAY.                                                 *        
055800*********************************************************************     
055900 6110-SEARCH-HOLIDAY-TABLE.                                               
056000     IF HOLIDAY-DATE (HSUB) = GREG-MDY-MONTH-DAY                          
056100         MOVE HOLIDAY-IDENTIFIER (HSUB) TO HOLIDAY-INDICATOR              
056200         MOVE 'Y' TO HOLIDAY-FOUND-INDICATOR.                             
056300 6110-EXIT.                                                               
056400     EXIT.                                                                
056500*********************************************************************     
056600*     DETERMINE DAY OF WEEK                                      *        
056700*********************************************************************     
056800 6200-DAY-OF-WEEK-PROCESSING.                                             
056900     MOVE YEAR-START-DAY TO WORK1.                                        
057000     ADD UNPACKED-JULIAN-DAY TO WORK1.                                    
057100     DIVIDE WORK1 BY 7 GIVING WORK2 REMAINDER DAY-OF-THE-WEEK.            
057200     IF DAY-OF-THE-WEEK = 0                                               
057300         MOVE 7 TO DAY-OF-THE-WEEK.                                       
057400 6200-EXIT.                                                               
057500     EXIT.                                                                
057600*********************************************************************     
057700*     BUILD LITERAL DATE  AND THE DAY AND MONTH LITERALS         *        
057800*********************************************************************     
057900 6300-BUILD-LITERAL-DATE.                                                 
058000     MOVE MONTH-NAME (GREG-MDY-MONTH) TO LITERAL-DATE.                    
058100     MOVE MONTH-SIZE (GREG-MDY-MONTH) TO OFFSET.                          
058200     IF GREG-MDY-DAY < '10'                                               
058300         MOVE GREG-MDY-DAY (2 : 1) TO LITERAL-DATE (OFFSET : 1)           
058400         ADD 1 TO OFFSET                                                  
058500     ELSE                                                                 
058600         MOVE GREG-MDY-DAY TO LITERAL-DATE (OFFSET : 2)                   
058700         ADD 2 TO OFFSET.                                                 
058800     MOVE ',' TO LITERAL-DATE (OFFSET  : 1).                              
058900     MOVE GREG-MDY-YEAR TO LITERAL-DATE (OFFSET + 2 : 4).                 
059000     MOVE DAY-OF-THE-WEEK-NAME (DAY-OF-THE-WEEK) TO DAY-LITERAL.          
059100     MOVE MONTH-NAME (GREG-MDY-MONTH) TO MONTH-LITERAL.                   
059200 6300-EXIT.                                                               
059300     EXIT.                                                                
  