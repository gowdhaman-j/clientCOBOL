      ******************************************************************
      *                                                                *
      * LICENSED MATERIALS - PROPERTY OF IBM                           *
      *                                                                *
      * "RESTRICTED MATERIALS OF IBM"                                  *
      *                                                                *
      * (C) COPYRIGHT IBM CORP. 2021       ALL RIGHTS RESERVED         *
      *                                                                *
      * US GOVERNMENT USERS RESTRICTED RIGHTS - USE, DUPLICATION,      *
      * OR DISCLOSURE RESTRICTED BY GSA ADP SCHEDULE                   *
      * CONTRACT WITH IBM CORPORATION                                  *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GETAAVG.
      *****************************************************************
      *                                                               *
      *                                                               *
      *****************************************************************
      * THIS PROGRAM IS TO BE USED ONLY FOR IBM INTERNAL USE ONLY     *
      *****************************************************************
      *                                                               *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01 WS-SWITCHES.

          05 WS-EOF-CSR          PIC X(01)  VALUE ' '.
             88 END-OF-CSR                  VALUE 'Y'.

       01 WS-WORK.
          05 WS-AVG-PREMIUM      PIC S9(09) COMP.
          05 WS-CUSTOMER-NUMBER  PIC S9(09) COMP.
          05 WS-E-SUMASSURED     PIC S9(09) COMP.
          05 WS-H-VALUE          PIC S9(09) COMP.
          05 WS-STATUS-CODE      PIC X(02)  VALUE SPACES.

            EXEC SQL
             INCLUDE SQLCA
            END-EXEC.

            EXEC SQL
              INCLUDE DGENAPP
            END-EXEC.

            COPY LGPOLICY.

       LINKAGE SECTION.
        01 WS-IN-REC.
           05 IN-REQUEST-ID       PIC X(06).
           05 IN-CUST-NUMBER      PIC 9(10).
           05 IN-OVERPAID-FLAG    PIC X(01).
           05 OUT-PREMIUM         PIC 9(06).
           05 OUT-STATUS-CODE     PIC X(02).


       PROCEDURE DIVISION USING WS-IN-REC.
       0001-MAIN.

           DISPLAY 'START OF PROGRAM GETAAVG'

           PERFORM 1000-INITIALIZATION
              THRU 1000-EXIT

           PERFORM 2000-CHECK-TYPE
              THRU 2000-EXIT

           MOVE WS-STATUS-CODE TO OUT-STATUS-CODE

           DISPLAY 'END OF PROGRAM GETAAVG'

           PERFORM 9000-END-PARA
           .
       0001-MAIN-EXIT.
           EXIT.

       1000-INITIALIZATION.

           INITIALIZE WS-SWITCHES

           DISPLAY "CUSTOMER NUMBER IS " IN-CUST-NUMBER
           MOVE IN-CUST-NUMBER TO WS-CUSTOMER-NUMBER
           .
       1000-EXIT.
           EXIT.

       2000-CHECK-TYPE.

           EVALUATE IN-REQUEST-ID
           WHEN '0AVCUS'
                MOVE '08' TO WS-STATUS-CODE
                DISPLAY 'PLEASE SELECT POLICY TYPE!'
                PERFORM 9000-END-PARA

           WHEN '0AVMOT'
                PERFORM 3100-GET-AVG-MOT-PREMIUM
                   THRU 3100-EXIT

           WHEN '0AVEND'
                PERFORM 3200-GET-ENDOWMENT
                   THRU 3200-EXIT

           WHEN '0AVHOU'
                PERFORM 3300-GET-HOUSE
                   THRU 3300-EXIT

           WHEN '0AVCOM'
                PERFORM 3400-GET-COMMERCIAL
                   THRU 3400-EXIT

           WHEN OTHER
                MOVE '16' TO WS-STATUS-CODE
                DISPLAY 'INVALID REQUEST ID:' IN-REQUEST-ID
                PERFORM 9000-END-PARA

           END-EVALUATE.


       2000-EXIT.
           EXIT.


       3100-GET-AVG-MOT-PREMIUM.

           EXEC SQL
                SELECT AVG(MOT.PREMIUM)
                INTO :WS-AVG-PREMIUM
                FROM CUSTOMER CUST
                INNER JOIN POLICY POL
                ON POL.CUSTOMERNUMBER = CUST.CUSTOMERNUMBER
                INNER JOIN MOTOR MOT
                ON POL.POLICYNUMBER = MOT.POLICYNUMBER
                WHERE POL.CUSTOMERNUMBER = :WS-CUSTOMER-NUMBER
                GROUP BY POL.CUSTOMERNUMBER
                END-EXEC

           DISPLAY 'SQLCODE:' SQLCODE

           EVALUATE SQLCODE
           WHEN 0
                MOVE WS-AVG-PREMIUM TO OUT-PREMIUM
      *          ADD 1 TO LS-AVG-PREMIUM
                DISPLAY 'PREMIUM:' OUT-PREMIUM
                MOVE '00' TO WS-STATUS-CODE
           WHEN 100
                MOVE '02' TO WS-STATUS-CODE
           WHEN OTHER
                MOVE '16' TO WS-STATUS-CODE
                PERFORM 9000-END-PARA
           END-EVALUATE

           .
       3100-EXIT.
           EXIT.


       3200-GET-ENDOWMENT.

           EXEC SQL
                SELECT SUMASSURED
                INTO :WS-E-SUMASSURED
                FROM CUSTOMER CUST
                INNER JOIN POLICY POL
                ON POL.CUSTOMERNUMBER = CUST.CUSTOMERNUMBER
                INNER JOIN ENDOWMENT END1
                ON POL.POLICYNUMBER = END1.POLICYNUMBER
                WHERE POL.CUSTOMERNUMBER = :WS-CUSTOMER-NUMBER
                END-EXEC

           DISPLAY 'SQLCODE:' SQLCODE

           EVALUATE SQLCODE
           WHEN 0
                MOVE WS-E-SUMASSURED TO OUT-PREMIUM
                DISPLAY 'NO AVG PREMIUM FOR ENDOWMENT POLICY!'
                MOVE '01' TO WS-STATUS-CODE
           WHEN 100
                MOVE '02' TO WS-STATUS-CODE
           WHEN OTHER
                MOVE '16' TO WS-STATUS-CODE
                PERFORM 9000-END-PARA
           END-EVALUATE

           .
       3200-EXIT.
           EXIT.


       3300-GET-HOUSE.

           EXEC SQL
                SELECT HOUS.VALUE
                INTO :WS-H-VALUE
                FROM CUSTOMER CUST
                INNER JOIN POLICY POL
                ON POL.CUSTOMERNUMBER = CUST.CUSTOMERNUMBER
                INNER JOIN HOUSE HOUS
                ON POL.POLICYNUMBER = HOUS.POLICYNUMBER
                WHERE POL.CUSTOMERNUMBER = :WS-CUSTOMER-NUMBER
                END-EXEC

           DISPLAY 'SQLCODE:' SQLCODE

           EVALUATE SQLCODE
           WHEN 0
                MOVE WS-H-VALUE TO OUT-PREMIUM
                DISPLAY 'NO AVG PREMIUM FOR HOUSING POLICY!'
                MOVE '01' TO WS-STATUS-CODE
           WHEN 100
                MOVE '02' TO WS-STATUS-CODE
           WHEN OTHER
                MOVE '16' TO WS-STATUS-CODE
                PERFORM 9000-END-PARA
           END-EVALUATE

           .
       3300-EXIT.
           EXIT.



       3400-GET-COMMERCIAL.

           EXEC SQL
                SELECT CUSTOMER
                INTO :DB2-CUSTOMER
                FROM CUSTOMER CUST
                INNER JOIN POLICY POL
                ON POL.CUSTOMERNUMBER = CUST.CUSTOMERNUMBER
                INNER JOIN COMMERCIAL COMM
                ON POL.POLICYNUMBER = COMM.POLICYNUMBER
                WHERE POL.CUSTOMERNUMBER = :WS-CUSTOMER-NUMBER
                END-EXEC

           DISPLAY 'SQLCODE:' SQLCODE

           EVALUATE SQLCODE
           WHEN 0
                MOVE DB2-CUSTOMER TO OUT-PREMIUM
                DISPLAY 'NO AVG PREMIUM FOR COMMERCIAL POLICY!'
                MOVE '01' TO WS-STATUS-CODE
           WHEN 100
                MOVE '02' TO WS-STATUS-CODE
           WHEN OTHER
                MOVE '16' TO WS-STATUS-CODE
                PERFORM 9000-END-PARA
           END-EVALUATE
           .
       3400-EXIT.
           EXIT.

       9000-END-PARA.

           DISPLAY 'STATUS CODE:' WS-STATUS-CODE

           GOBACK.