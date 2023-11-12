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
       PROGRAM-ID. GETPAVG.
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
       FILE-CONTROL.
      ******************************************************************
      *                  INPUT MAIN                                    *
      ******************************************************************
           SELECT INPUT01-FILE ASSIGN TO INFILE
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-INPUT01.

           SELECT OUT01-FILE ASSIGN TO OUTFILE
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-OUT01.

       DATA DIVISION.
       FILE SECTION.
      ******************************************************************
      *                  INPUT FILE                                    *
      ******************************************************************
       FD  INPUT01-FILE
           RECORDING MODE IS F.

       01 IN-REC                 PIC X(20).

      ******************************************************************
      *                 OUTPUT FILE                                    *
      ******************************************************************
       FD  OUT01-FILE
           RECORDING MODE IS F
           DATA RECORD IS OUT-REC.
       01 OUT-REC                PIC X(30).


       WORKING-STORAGE SECTION.

       01 FS-FILE-STATUS.
          05 FS-INPUT01          PIC X(02)  VALUE SPACES.
             88 INP-OK                      VALUE '00'.
          05 FS-OUT01            PIC X(02)  VALUE SPACES.
             88 OUT-OK                      VALUE '00'.

       01 WS-SWITCHES.
          05 WS-EOF-INP          PIC X(01)  VALUE ' '.
             88 END-OF-INP                  VALUE 'Y'.

          05 WS-EOF-CSR          PIC X(01)  VALUE ' '.
             88 END-OF-CSR                  VALUE 'Y'.

       01 WS-IN-REC.
          05 IN-REQUEST-ID       PIC X(06).
          05 IN-CUST-NUMBER      PIC 9(10).
          05 IN-OVERPAID-FLAG    PIC X(01).
      *   05 FILLER                        PIC X(09).
          05 IN-NEWFIELD         PIC X(01).
          05 FILLER              PIC X(02).

       01 WS-OUT-REC.
          05 OUT-REQUEST-ID      PIC X(06).
          05 OUT-CUST-NUMBER     PIC 9(10).
          05 OUT-PREMIUM-AVG     PIC 9(06).
          05 FILLER              PIC X(02).

       01 WS-WORK.
          05 WS-PREMIUM          PIC 9(06).
          05 WS-GET-POLICY       PIC X(07) VALUE  'GETAAVG'.

       01 WS-STATUS-CODE         PIC X(02)  VALUE SPACES.


       PROCEDURE DIVISION.
       0001-MAIN.

           DISPLAY 'START OF PROGRAM'
           PERFORM 1000-INITIALIZATION
              THRU 1000-EXIT
           PERFORM 1500-READ-INPUT
              THRU 1500-EXIT

           IF NOT END-OF-INP
              PERFORM 2000-GET-POLICY
                 THRU 2000-EXIT
                 UNTIL END-OF-INP
           END-IF

           PERFORM 9000-END-PARA
           .
       0001-MAIN-EXIT.
           EXIT.

       1000-INITIALIZATION.
           INITIALIZE WS-SWITCHES
                      WS-STATUS-CODE
           PERFORM 1100-OPEN-FILES
              THRU 1100-EXIT
           .
       1000-EXIT.
           EXIT.

       1100-OPEN-FILES.

           OPEN INPUT INPUT01-FILE

           IF NOT INP-OK
              DISPLAY '1100-OPEN-FILES:'
              DISPLAY 'INVALID FILE STATUS ON OPEN INPUT:' FS-INPUT01
              MOVE '02' TO WS-STATUS-CODE
              PERFORM 9000-END-PARA
           END-IF

           OPEN OUTPUT OUT01-FILE

           IF NOT OUT-OK
              DISPLAY '1100-OPEN-FILES:'
              DISPLAY 'INVALID FILE STATUS ON OPEN OUTPUT:' FS-OUT01
              MOVE '02' TO WS-STATUS-CODE
              PERFORM 9000-END-PARA
           END-IF
           .
       1100-EXIT.
           EXIT.

       1500-READ-INPUT.

           DISPLAY 'BEFORE INPUT FILE READ:'

           READ INPUT01-FILE INTO WS-IN-REC
           AT END
              SET END-OF-INP TO TRUE.

           IF NOT INP-OK AND NOT END-OF-INP
              DISPLAY 'INVALID FILE STATUS ON READ:' FS-INPUT01
              MOVE '03' TO WS-STATUS-CODE
              PERFORM 9000-END-PARA
           END-IF

           DISPLAY "CUSTOMER NUMBER IS " IN-CUST-NUMBER
           .
       1500-EXIT.
           EXIT.

       2000-GET-POLICY.

           CALL WS-GET-POLICY USING IN-REQUEST-ID
                                    IN-CUST-NUMBER 
                                    IN-OVERPAID-FLAG
                                    WS-PREMIUM 
                                    WS-STATUS-CODE  

           PERFORM 4000-WRITE-OUT-CUSTOMER
              THRU 4000-EXIT

           PERFORM 1500-READ-INPUT
              THRU 1500-EXIT
           .

       2000-EXIT.
           EXIT.


       4000-WRITE-OUT-CUSTOMER.

      *    Missing numeric check

ABC11      IF IN-OVERPAID-FLAG = 'Y' AND IN-REQUEST-ID = '0AVMOT'
              COMPUTE OUT-PREMIUM-AVG = WS-PREMIUM * -1
           ELSE
              MOVE WS-PREMIUM TO OUT-PREMIUM-AVG
           END-IF

           MOVE IN-CUST-NUMBER TO OUT-CUST-NUMBER
           DISPLAY 'OUT-CUST:' OUT-CUST-NUMBER
           DISPLAY 'PREMIUM:' OUT-PREMIUM-AVG

           WRITE OUT-REC FROM WS-OUT-REC.

           IF NOT OUT-OK
              DISPLAY 'INVALID FILE STATUS ON WRITE:' FS-OUT01
              MOVE 0003 TO WS-STATUS-CODE
      *       MOVE 8    TO RETURN-CODE
              PERFORM 9000-END-PARA
           END-IF

           .
       4000-EXIT.
           EXIT.

       9000-END-PARA.

           DISPLAY 'STATUS CODE:' WS-STATUS-CODE

           GOBACK.