       CBL
      ******************************************************************
      *                                                                *
      * Licensed Materials - Property of IBM                           *
      *                                                                *
      * (c) Copyright IBM Corp. 2020.                                  *
      *                                                                *
      * US Government Users Restricted Rights - Use, duplication       *
      * or disclosure restricted by GSA ADP Schedule Contract          *
      * with IBM Corp.                                                 *
      *                                                                *
      ******************************************************************

      ******************************************************************
      * Copies the VSAM CUSTOMER file one record at a time to a new
      * CUSTOMER file which has been extended to contain the 3 new
      * attributes TELNO, EMAIL and MRKT-PREF. The TELNO should be set
      * to 00000000000, the EMAIL should be set to SPACES and the
      * MRKT-PREF should be set to POST on each record.
      *
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXTDCUST.
       AUTHOR. Jon Collett.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *SOURCE-COMPUTER. MAINFRAME WITH DEBUGGING MODE.
       SOURCE-COMPUTER. MAINFRAME.
      *****************************************************************
      *** File Control                                              ***
      *****************************************************************
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT VSAMIN
                  ASSIGN TO VSAMIN
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS SEQUENTIAL
                  RECORD KEY IS OLD-CUSTOMER-KEY
                  FILE STATUS IS VSAMIN-STATUS.

           SELECT VSAMOUT
                  ASSIGN TO VSAMOUT
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS SEQUENTIAL
                  RECORD KEY   IS CUSTOMER-KEY
                  FILE STATUS  IS VSAMOUT-STATUS.

       DATA DIVISION.
      *****************************************************************
      *** File Section                                              ***
      *****************************************************************
       FILE SECTION.
       FD  VSAMIN.
       01 OLD-REC.
           03 OLD-CUSTOMER-RECORD.
              05 OLD-CUSTOMER-EYECATCHER                 PIC X(4).
                 88 OLD-CUSTOMER-EYECATCHER-VALUE        VALUE 'CUST'.
              05 OLD-CUSTOMER-KEY.
                 07 OLD-CUSTOMER-SORTCODE            PIC 9(6) DISPLAY.
                 07 OLD-CUSTOMER-NUMBER              PIC 9(10) DISPLAY.
              05 OLD-CUSTOMER-NAME                   PIC X(60).
              05 OLD-CUSTOMER-ADDRESS                PIC X(160).
              05 OLD-CUSTOMER-DATE-OF-BIRTH          PIC 9(8).
              05 OLD-CUSTOMER-DOB-GROUP
                 REDEFINES OLD-CUSTOMER-DATE-OF-BIRTH.
                 07 OLD-CUSTOMER-BIRTH-DAY           PIC 99.
                 07 OLD-CUSTOMER-BIRTH-MONTH         PIC 99.
                 07 OLD-CUSTOMER-BIRTH-YEAR          PIC 9999.
              05 OLD-CUSTOMER-CREDIT-SCORE           PIC 999.
              05 OLD-CUSTOMER-CS-REVIEW-DATE         PIC 9(8).
              05 OLD-CUSTOMER-CS-GROUP
                 REDEFINES OLD-CUSTOMER-CS-REVIEW-DATE.
                 07 OLD-CUSTOMER-CS-REVIEW-DAY       PIC 99.
                 07 OLD-CUSTOMER-CS-REVIEW-MONTH     PIC 99.
                 07 OLD-CUSTOMER-CS-REVIEW-YEAR      PIC 9999.

       FD  VSAMOUT.
       01  VSAM-RECORD.
       COPY CUSTOMER.

      *****************************************************************
      *** Working storage                                           ***
      *****************************************************************
       WORKING-STORAGE SECTION.
      * Copyright statement as a literal to go into the load module
       77 FILLER PIC X(80) VALUE
           'Licensed Materials - Property of IBM'.
       77 FILLER PIC X(80) VALUE
           '(c) Copyright IBM Corp. 2020. All Rights Reserved.'.
       77 FILLER PIC X(80) VALUE
           'US Government Users Restricted Rights - Use, duplication '.
       77 FILLER PIC X(80) VALUE
           'or disclosure restricted by GSA ADP Schedule Contract '.
       77 FILLER PIC X(80) VALUE
           'with IBM Corp.'.


       01  VSAMIN-STATUS.
           05 VSAMIN-STATUS1              PIC X.
           05 VSAMIN-STATUS2              PIC X.

       01  VSAMOUT-STATUS.
           05 VSAMOUT-STATUS1             PIC X.
           05 VSAMOUT-STATUS2             PIC X.

       01 WS-EXIT                          PIC X    VALUE 'N'.
       01 WS-CNT                           PIC 9(5) VALUE 0.



      *****************************************************************
      *** Linkage Storage                                           ***
      *****************************************************************
       LINKAGE SECTION.

      *****************************************************************
      *** Main Processing                                           ***
      *****************************************************************
       PROCEDURE DIVISION.
       PREMIERE SECTION.
       P010.
           DISPLAY 'Started EXTDCUST.'.
      *
      *    Open the OUTPUT VSAM file
      *
           DISPLAY 'About  to OPEN VSAMOUT.'.

           OPEN OUTPUT VSAMOUT.
           IF VSAMOUT-STATUS NOT EQUAL '00' AND
           VSAMOUT-STATUS NOT EQUAL '97'
               DISPLAY 'Error opening VSAM OUT file, status='
                       VSAMOUT-STATUS
               MOVE 12 TO RETURN-CODE
               PERFORM PROGRAM-DONE
           END-IF.

      *
      *    Open the INPUT VSAM file
      *
           DISPLAY 'About  to OPEN VSAMIN.'.

           OPEN INPUT VSAMIN.
           IF VSAMIN-STATUS NOT EQUAL '00' AND
           VSAMOUT-STATUS NOT EQUAL '00'
               DISPLAY 'Error opening VSAM IN file, status='
                       VSAMIN-STATUS
               MOVE 12 TO RETURN-CODE
               PERFORM PROGRAM-DONE
           END-IF.

           MOVE 'N' TO WS-EXIT.

           DISPLAY 'About  to READ VSAMIN.'.

           READ VSAMIN

           IF VSAMIN-STATUS = '10'
              CONTINUE
           ELSE
              IF VSAMIN-STATUS NOT EQUAL '00'
                 DISPLAY 'Error reading VSAM IN file, status='
                         VSAMIN-STATUS
                 MOVE 12 TO RETURN-CODE
                 PERFORM PROGRAM-DONE
              END-IF
           END-IF.

           PERFORM VARYING WS-CNT FROM 1 BY 1 UNTIL WS-EXIT = 'Y'

              MOVE OLD-CUSTOMER-EYECATCHER TO CUSTOMER-EYECATCHER
              MOVE OLD-CUSTOMER-KEY TO CUSTOMER-KEY
              MOVE OLD-CUSTOMER-NAME TO CUSTOMER-NAME
              MOVE OLD-CUSTOMER-ADDRESS TO CUSTOMER-ADDRESS
              MOVE OLD-CUSTOMER-DATE-OF-BIRTH TO CUSTOMER-DATE-OF-BIRTH
              MOVE OLD-CUSTOMER-CREDIT-SCORE TO CUSTOMER-CREDIT-SCORE
              MOVE OLD-CUSTOMER-CS-REVIEW-DATE TO
                 CUSTOMER-CS-REVIEW-DATE
              MOVE 00000000000 TO CUSTOMER-TELNO
              MOVE SPACES TO CUSTOMER-EMAIL
              MOVE 'POST     ' TO CUSTOMER-MRKT-PREF

              DISPLAY 'About  to WRITE VSAM-RECORD.'

              WRITE VSAM-RECORD

              IF VSAMOUT-STATUS NOT EQUAL '00'
                   DISPLAY 'Error writing to VSAM out file, status='
                           VSAMOUT-STATUS
                   MOVE 12 TO RETURN-CODE
                   PERFORM PROGRAM-DONE
              END-IF

      *
      *       Now get the next input record
      *

              READ VSAMIN

              IF VSAMIN-STATUS = '10'
                 MOVE 'Y' TO WS-EXIT
              END-IF

           END-PERFORM.

           DISPLAY 'You have copied ' WS-CNT ' records from the'
                   ' input file to the output file'.


           PERFORM PROGRAM-DONE.

       P999.
           EXIT.


       PROGRAM-DONE SECTION.
       PD010.

           PERFORM CLOSE-FILE.

           GOBACK.

       PD999.
           EXIT.

       CLOSE-FILE SECTION.
       CF010.
           DISPLAY 'About  to CLOSE VSAMIN.'.

           CLOSE VSAMIN.

           DISPLAY 'About  to CLOSE VSAMOUT.'.

           CLOSE VSAMOUT.

       CF999.
           EXIT.

