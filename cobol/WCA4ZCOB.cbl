      ******************************************************************
      * ONLY COBOL STATEMENTS WITH 200 LINES OF CODE IN A PARAGRAPH
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. WCA4ZCOB.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *
       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01 WS-TEMP-VARS.
           03 WS-TEMP-VARS-01           PIC X(20) VALUE SPACES.
           03 WS-TEMP-VARS-01-REF REDEFINES WS-TEMP-VARS-01.
                05  WS-TEMP-VARS-01-ARRAY OCCURS 20 TIMES.
                    08  WS-TEMP-VARS-01-FLD
                                        PIC X(01).

           03 WS-PROD-DETAILS             OCCURS 05 TIMES.
                05  WS-PROD-AGE         PIC 9(03) VALUE ZEROS.
                05  WS-PROD-NAME        PIC X(15) VALUE SPACES.

           03  WS-DOB-TEMP.
                05 WS-DOB-TEMP-CCYY     PIC 9(04) VALUE ZEROS.
                05  WS-DOB-TEMP-MM      PIC 9(02) VALUE ZEROS.
                05  WS-DOB-TEMP-DD      PIC 9(02) VALUE ZEROS.
           03   WS-DOB-TEMP-FORMATTED.
                05 WS-DOB-TEMP-CCYY     PIC 9(04) VALUE ZEROS.
                05  FILLERS             PIC X(01) VALUE '-'.
                05  WS-DOB-TEMP-MM      PIC 9(02) VALUE ZEROS.
                05  FILLERS             PIC X(01) VALUE '-'.
                05  WS-DOB-TEMP-DD      PIC 9(02) VALUE ZEROS.

           03  WS-LENGTH                PIC 9(04) COMP VALUE ZEROS.
           03  I                        PIC 9(04) COMP VALUE ZEROS.
           03   IN-CUSTOMER-DETAILS.
              05 IN-FIRST-NAME          PIC X(10).
              05 IN-LAST-NAME           PIC X(20).
              05 IN-DOB                 PIC X(10).
              05 IN-HOUSE-NAME          PIC X(20).
              05 IN-HOUSE-NUM           PIC X(4).
              05 IN-POSTCODE            PIC X(8).
              05 IN-NUM-POLICIES        PIC X(3).
              05 IN-PHONE-MOBILE        PIC X(20).
              05 IN-PHONE-HOME          PIC X(20).
              05 IN-EMAIL-ADDRESS       PIC X(100).
              05 IN-SALARY              PIC 9(09)V99 VALUE ZEROS.

           03   IN-AMOUNT-DETAILS.
              05 IN-LOAN-AMOUNT         PIC 9(09)V99 VALUE ZEROS.
              05 IN-LOAN-INT-RATE       PIC 9(03)V99 VALUE ZEROS.
              05 IN-LOAN-DURATION       PIC 9(03)    VALUE ZEROS.
              05 IN-LOAN-YEAR-MONTH     PIC X(01)    VALUE SPACES.
              05 IN-LOAN-INT-PYMT       PIC 9(09)V99 VALUE ZEROS.
           03 WS-LOAN-DURATION          PIC 9(03)    VALUE ZEROS.
           03   WS-CUSTOMER-DETAILS.
              05 WS-FIRST-NAME          PIC X(10).
              05 WS-LAST-NAME           PIC X(20).
              05 WS-DOB                 PIC X(10).
              05 WS-HOUSE-NAME          PIC X(20).
              05 WS-HOUSE-NUM           PIC X(4).
              05 WS-POSTCODE            PIC X(8).
              05 WS-NUM-POLICIES        PIC 9(3).
              05 WS-PHONE-MOBILE        PIC X(20).
              05 WS-PHONE-HOME          PIC X(20).
              05 WS-EMAIL-ADDRESS       PIC X(100).
              05 WS-AGE                 PIC 9(03)    VALUE ZEROS.
              05 WS-PROD-NAME-SELECT    PIC X(15)    VALUE SPACES.
              05 WS-RISK-FACTOR         PIC X(01)    VALUE SPACES.
              05 WS-FIRST-NAME-SMALL    PIC X(10).
              05 WS-LAST-NAME-SMALL     PIC X(20).

           03 WS-CURRENT-DATE-DATA.
             05  WS-CURRENT-DATE.
                 10  WS-CURRENT-YEAR         PIC 9(04).
                 10  WS-CURRENT-MONTH        PIC 9(02).
                 10  WS-CURRENT-DAY          PIC 9(02).
             05  WS-CURRENT-TIME.
                 10  WS-CURRENT-HOURS        PIC 9(02).
                 10  WS-CURRENT-MINUTE       PIC 9(02).
                 10  WS-CURRENT-SECOND       PIC 9(02).
                 10  WS-CURRENT-MILLISECONDS PIC 9(02).

           03 WS-BIRTH-YEAR                  PIC 9(04).
           03 WS-JSON-RECORD                 PIC X(300).
           03 WS-HEADERS.
             05  WS-HEADER-01                PIC X(80).
             05  WS-HEADER-02                PIC X(80).
             05  WS-HEADER-03                PIC X(80).
             05  WS-HEADER-04                PIC X(80).
             05  WS-HEADER-05                PIC X(80).
             05  WS-HEADER-06                PIC X(80).
             05  WS-HEADER-07                PIC X(80).
             05  WS-HEADER-08                PIC X(80).
             05  WS-HEADER-09                PIC X(80).
             05  WS-HEADER-10                PIC X(80).
      ******************************************************************
      *    P R O C E D U R E S
      ******************************************************************
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
       MAINLINE SECTION.
           DISPLAY 'MAINLINE'.
            PERFORM A200-LINE-JUST-COBOL-PARA.
            PERFORM A300-DIS-COBOL-PARA.
            STOP RUN.

       A200-LINE-JUST-COBOL-PARA.
           DISPLAY 'A200-LINE...'.
            INITIALIZE                  WS-TEMP-VARS-01
                                        WS-LENGTH.

            MOVE 'A B C E F G H I J'        TO IN-LAST-NAME.
            MOVE IN-LAST-NAME               TO WS-TEMP-VARS-01.
            MOVE LENGTH OF WS-TEMP-VARS-01  TO WS-LENGTH.
            PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-LENGTH
                IF WS-TEMP-VARS-01-FLD(I) = ' '
                    MOVE 'Z'                TO WS-TEMP-VARS-01-FLD(I)
                END-IF
            END-PERFORM.
            MOVE WS-TEMP-VARS-01            TO  WS-LAST-NAME.
            DISPLAY 'LAST NAME ' WS-LAST-NAME.

            INITIALIZE                  WS-TEMP-VARS-01
                                        WS-LENGTH.

            MOVE 'K L M N O P Q'            TO IN-FIRST-NAME.
            MOVE IN-FIRST-NAME              TO WS-TEMP-VARS-01.
            MOVE LENGTH OF WS-TEMP-VARS-01  TO WS-LENGTH.
            PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-LENGTH
                IF WS-TEMP-VARS-01-FLD(I) = ' '
                    MOVE 'Y'                TO WS-TEMP-VARS-01-FLD(I)
                END-IF
            END-PERFORM.
            MOVE WS-TEMP-VARS-01            TO  WS-FIRST-NAME.
            DISPLAY 'FIRST NAME ' WS-FIRST-NAME.

            MOVE '20200101'                 TO IN-DOB.
            MOVE IN-DOB                     TO WS-DOB-TEMP
            MOVE CORR WS-DOB-TEMP           TO WS-DOB-TEMP-FORMATTED.
            MOVE WS-DOB-TEMP-FORMATTED      TO WS-DOB.
            DISPLAY 'DOB ' WS-DOB.

            EVALUATE TRUE
                 WHEN WS-DOB-TEMP-MM OF WS-DOB-TEMP = 01
                      DISPLAY ' JAN MONTH..'
                 WHEN WS-DOB-TEMP-MM OF WS-DOB-TEMP = 02
                      DISPLAY ' FEB MONTH..'
                 WHEN WS-DOB-TEMP-MM OF WS-DOB-TEMP = 03
                      DISPLAY ' MAR MONTH..'
                 WHEN WS-DOB-TEMP-MM OF WS-DOB-TEMP = 04
                      DISPLAY ' APR MONTH..'
                 WHEN WS-DOB-TEMP-MM OF WS-DOB-TEMP = 05
                      DISPLAY ' MAY MONTH..'
                 WHEN WS-DOB-TEMP-MM OF WS-DOB-TEMP = 06
                      DISPLAY ' JUN MONTH..'
                 WHEN WS-DOB-TEMP-MM OF WS-DOB-TEMP = 07
                      DISPLAY ' JUL MONTH..'
                 WHEN WS-DOB-TEMP-MM OF WS-DOB-TEMP = 08
                      DISPLAY ' AUG MONTH..'
                 WHEN WS-DOB-TEMP-MM OF WS-DOB-TEMP = 09
                      DISPLAY ' SEP MONTH..'
                 WHEN WS-DOB-TEMP-MM OF WS-DOB-TEMP = 10
                      DISPLAY ' OCT MONTH..'
                 WHEN WS-DOB-TEMP-MM OF WS-DOB-TEMP = 11
                      DISPLAY ' NOV MONTH..'
                 WHEN WS-DOB-TEMP-MM OF WS-DOB-TEMP = 12
                      DISPLAY ' DEC MONTH..'
                 WHEN OTHER
                      DISPLAY ' INV MONTH..'
            END-EVALUATE.

            MOVE 'XYZ HOME'                 TO IN-HOUSE-NAME.
            INSPECT IN-HOUSE-NAME REPLACING ALL SPACES BY "#".
            MOVE IN-HOUSE-NAME              TO WS-HOUSE-NAME.
            DISPLAY 'HOUSE NAME ' WS-HOUSE-NAME.

            MOVE '  12'                     TO IN-HOUSE-NUM.
            INSPECT IN-HOUSE-NUM  REPLACING ALL SPACES BY "0".
            MOVE IN-HOUSE-NUM               TO WS-HOUSE-NUM.
            DISPLAY 'HOUSE NUM '               WS-HOUSE-NUM.

            MOVE '12345'                    TO IN-POSTCODE.
            INSPECT IN-POSTCODE   REPLACING ALL SPACES BY '0'.
            MOVE IN-POSTCODE                TO WS-POSTCODE.
            DISPLAY 'POSTCODE '                WS-POSTCODE.

            MOVE '786'                      TO IN-NUM-POLICIES.
            IF IN-NUM-POLICIES NOT NUMERIC
               INSPECT IN-NUM-POLICIES REPLACING ALL SPACES BY '0'
            END-IF.
            MOVE IN-NUM-POLICIES            TO WS-NUM-POLICIES.
            DISPLAY 'POLICIES '                WS-NUM-POLICIES.

            MOVE '732-485-236'              TO IN-PHONE-MOBILE.
            MOVE IN-PHONE-MOBILE            TO WS-PHONE-MOBILE.
            DISPLAY 'MOB NO '                  WS-PHONE-MOBILE.

            INITIALIZE                      I.
            MOVE 'SAMPLE@IBM.COM'           TO IN-EMAIL-ADDRESS.
            INSPECT IN-EMAIL-ADDRESS TALLYING I FOR ALL "@".
            IF I >  1
               MOVE 'INVALID EMAIL ID '     TO IN-EMAIL-ADDRESS
            END-IF.
            MOVE IN-EMAIL-ADDRESS           TO WS-EMAIL-ADDRESS.
            DISPLAY 'EMAIL '                WS-EMAIL-ADDRESS.

            MOVE 5000000                    TO IN-LOAN-AMOUNT.
            MOVE 12                         TO IN-LOAN-INT-RATE.
            MOVE 15                         TO IN-LOAN-DURATION.
            MOVE 'Y'                        TO IN-LOAN-YEAR-MONTH.
            IF IN-LOAN-YEAR-MONTH = 'Y'
               COMPUTE WS-LOAN-DURATION = IN-LOAN-DURATION * 12
            ELSE
               MOVE IN-LOAN-DURATION        TO WS-LOAN-DURATION
            END-IF.

            COMPUTE IN-LOAN-INT-PYMT =                                  E
                         ( IN-LOAN-AMOUNT * IN-LOAN-INT-RATE / 12 )     E
            DISPLAY 'INT PYMT ' IN-LOAN-INT-PYMT.

            MOVE 50000                          TO IN-SALARY.
            DISPLAY 'SALARY IS ...'                IN-SALARY.

            IF IN-LOAN-INT-PYMT > IN-SALARY
               MOVE 'H'                         TO WS-RISK-FACTOR
            ELSE
               MOVE 'L'                         TO WS-RISK-FACTOR
            END-IF.

            DISPLAY 'RISK FACTOR..'                WS-RISK-FACTOR.

            EVALUATE TRUE
                WHEN IN-LOAN-INT-PYMT < 10000
                     DISPLAY ' MANAGEABLE INTEREST '
                WHEN IN-LOAN-INT-PYMT >= 10000 AND <= 40000
                     DISPLAY ' HIGH INTEREST '
                WHEN OTHER
                     DISPLAY ' WILL NOT BE ABLE TO MANAGE '
            END-EVALUATE.

            MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-DATA.
            DISPLAY 'CURRENT DATE...'     WS-CURRENT-DATE-DATA.
            MOVE      IN-DOB(1:4)      TO WS-BIRTH-YEAR.
            SUBTRACT  WS-BIRTH-YEAR  FROM WS-CURRENT-YEAR
                      GIVING WS-AGE.
            DISPLAY 'AGE....' WS-AGE.
       A300-DIS-COBOL-PARA.
******* LOAD PRODUCT TABLES
            MOVE  20                   TO WS-PROD-AGE(01).
            MOVE 'PROD UNDER 20..'     TO WS-PROD-NAME(01).
            MOVE  40                   TO WS-PROD-AGE(02).
            MOVE 'PROD UNDER 40..'     TO WS-PROD-NAME(02).
            MOVE  60                   TO WS-PROD-AGE(03).
            MOVE 'PROD UNDER 60..'     TO WS-PROD-NAME(03).
            MOVE  80                   TO WS-PROD-AGE(04).
            MOVE 'PROD UNDER 80..'     TO WS-PROD-NAME(04).
            MOVE  100                  TO WS-PROD-AGE(05).
            MOVE 'PROD UNDER 100.'     TO WS-PROD-NAME(05).

            PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
                 IF WS-AGE <= WS-PROD-AGE(I)
                    MOVE WS-PROD-NAME(I)
                                       TO WS-PROD-NAME-SELECT
                    MOVE 6             TO I
                 END-IF
            END-PERFORM.
            DISPLAY 'SELECTED PROD... '   WS-PROD-NAME-SELECT.

            MOVE FUNCTION LOWER-CASE(WS-FIRST-NAME)
                                       TO WS-FIRST-NAME-SMALL.
            MOVE FUNCTION LOWER-CASE(WS-LAST-NAME)
                                       TO WS-LAST-NAME-SMALL.

            STRING '{ '  DELIMITED BY SIZE,
                 WS-FIRST-NAME    DELIMITED BY SIZE,
                   ', '           DELIMITED BY SIZE,
                 WS-LAST-NAME     DELIMITED BY SIZE,
                   ', '           DELIMITED BY SIZE,
                 WS-DOB           DELIMITED BY SIZE,
                   ', '           DELIMITED BY SIZE,
                 WS-HOUSE-NAME    DELIMITED BY SIZE,
                   ', '           DELIMITED BY SIZE,
                 WS-HOUSE-NUM     DELIMITED BY SIZE,
                   ', '           DELIMITED BY SIZE,
                 WS-POSTCODE      DELIMITED BY SIZE,
                   ', '           DELIMITED BY SIZE,
                 WS-NUM-POLICIES  DELIMITED BY SIZE,
                   ', '           DELIMITED BY SIZE,
                 WS-PHONE-MOBILE  DELIMITED BY SIZE,
                   ', '           DELIMITED BY SIZE,
                 WS-PHONE-HOME    DELIMITED BY SIZE,
                   ', '           DELIMITED BY SIZE,
                 WS-EMAIL-ADDRESS DELIMITED BY SIZE,
                   ', '           DELIMITED BY SIZE,
                 WS-AGE           DELIMITED BY SIZE,
                   ', '           DELIMITED BY SIZE,
                 WS-PROD-NAME-SELECT
                                  DELIMITED BY SIZE,
                   ', '           DELIMITED BY SIZE,
                 WS-RISK-FACTOR   DELIMITED BY SIZE,
                   ', '           DELIMITED BY SIZE,
                 WS-FIRST-NAME-SMALL
                                  DELIMITED BY SIZE,
                   ', '           DELIMITED BY SIZE,
                 WS-LAST-NAME-SMALL
                                  DELIMITED BY SIZE,
                   '}'            DELIMITED BY SIZE
                        INTO WS-JSON-RECORD
            END-STRING.
            DISPLAY 'JSON REC..' WS-JSON-RECORD.

            MOVE '*********** HEADER 01 **********'
                                       TO WS-HEADER-01.
            DISPLAY                       WS-HEADER-01.
            MOVE '*********** HEADER 02 **********'
                                       TO WS-HEADER-02.
            DISPLAY                       WS-HEADER-02.
            MOVE '*********** HEADER 03 **********'
                                       TO WS-HEADER-03.
            DISPLAY                       WS-HEADER-03.
            MOVE '*********** HEADER 04 **********'
                                       TO WS-HEADER-04.
            DISPLAY                       WS-HEADER-04.
            MOVE '*********** HEADER 05 **********'
                                       TO WS-HEADER-05.
            DISPLAY                       WS-HEADER-05.
            MOVE '*********** HEADER 06 **********'
                                       TO WS-HEADER-06.
            DISPLAY                       WS-HEADER-06.
            MOVE '*********** HEADER 07 **********'
                                       TO WS-HEADER-07.
            DISPLAY                       WS-HEADER-07.
            MOVE '*********** HEADER 08 **********'
                                       TO WS-HEADER-08.
            DISPLAY                       WS-HEADER-08.
            MOVE '*********** HEADER 09 **********'
                                       TO WS-HEADER-09.
            DISPLAY                       WS-HEADER-09.
            MOVE '*********** HEADER 10 **********'
                                       TO WS-HEADER-10.
            DISPLAY                       WS-HEADER-10.
           EXIT.
