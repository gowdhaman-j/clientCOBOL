       CBL CICS('SP,EDF')
       CBL SQL
      ******************************************************************
      *                                                                *
      * Licensed Materials - Property of IBM                           *
      *                                                                *
      * (c) Copyright IBM Corp. 2017,2020.                             *
      *                                                                *
      * US Government Users Restricted Rights - Use, duplication       *
      * or disclosure restricted by GSA ADP Schedule Contract          *
      * with IBM Corp.                                                 *
      *                                                                *
      ******************************************************************


      ******************************************************************
      * This program will get the next available customer number for
      * another program to create. Alternatively it will reset the
      * counter if there is an error.
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. NEWCUSNO.
       AUTHOR. OGRADYJ.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *SOURCE-COMPUTER.  IBM-370 WITH DEBUGGING MODE.
       SOURCE-COMPUTER.  IBM-370.
       OBJECT-COMPUTER.  IBM-370.

       INPUT-OUTPUT SECTION.



       DATA DIVISION.
       FILE SECTION.


       WORKING-STORAGE SECTION.
       LOCAL-STORAGE SECTION.
       01 WS-ERROR PIC X(40) VALUE ALL '#'.

      *
      * CUSTOMER NCS definitions
      *
       01 NCS-CUS-NO-STUFF.
          03 NCS-CUS-NO-NAME.
             05 NCS-CUS-NO-ACT-NAME  PIC X(8)
                                 VALUE 'CBSACUST'.
             05 NCS-CUS-NO-TEST-SORT PIC X(6)
                                 VALUE '      '.
             05 NCS-CUS-NO-FILL      PIC XX
                                 VALUE '  '.

          03 NCS-CUS-NO-INC   PIC 9(16) COMP
                                 VALUE 0.
          03 NCS-CUS-NO-VALUE PIC 9(16) COMP
                                 VALUE 0.

          03 NCS-CUS-NO-RESP  PIC XX VALUE '00'.

       01 NCS-CUS-NO-DISP                PIC 9(16) VALUE 0.

       01 WS-ABEND-PGM                  PIC X(8) VALUE 'ABNDPROC'.

       01 ABNDINFO-REC.
           COPY ABNDINFO.


      * Copyright statement as a literal to go into the load module
       77 FILLER PIC X(80) VALUE
           'Licensed Materials - Property of IBM'.
       77 FILLER PIC X(80) VALUE
           '(c) Copyright IBM Corp. 2017,2020. All Rights Reserved.'.
       77 FILLER PIC X(80) VALUE
           'US Government Users Restricted Rights - Use, duplication '.
       77 FILLER PIC X(80) VALUE
           'or disclosure restricted by GSA ADP Schedule Contract '.
       77 FILLER PIC X(80) VALUE
           'with IBM Corp.'.

       COPY SORTCODE.
       01 NCS-UPDATED                        PIC X VALUE 'N'.

       01 WS-CICS-RESP PIC S9(8) BINARY.
       01 WS-CICS-RESP2 PIC S9(8) BINARY.


       LINKAGE SECTION.
       01 DFHCOMMAREA.
          COPY NEWCUSNO.

       PROCEDURE DIVISION USING DFHCOMMAREA.
       PREMIERE SECTION.
       P010.
      *

           IF NEWCUSNO-FUNCTION-GETNEW

             PERFORM ENQ-NAMED-COUNTER
      *
      * Get the next CUSTOMER number from the CUSTOMER Named Counter
      *
             PERFORM UPD-NCS
      D    DISPLAY 'PGM NEWCUSNO THE OUTPUT DATA IS='
      D       DFHCOMMAREA
             PERFORM GET-ME-OUT-OF-HERE
           END-IF.

           IF NEWCUSNO-FUNCTION-ROLLBACK
             PERFORM RESTORE-NCS
             PERFORM DEQ-NAMED-COUNTER
             PERFORM GET-ME-OUT-OF-HERE
           END-IF.

           IF NEWCUSNO-FUNCTION-CURRENT

             PERFORM GET-NCS
      D    DISPLAY 'PGM NEWCUSNO THE OUTPUT DATA IS='
      D       DFHCOMMAREA
             PERFORM GET-ME-OUT-OF-HERE
           END-IF.

       P999.
           EXIT.



       ENQ-NAMED-COUNTER SECTION.
       ENC010.

           MOVE SORTCODE TO
              NCS-CUS-NO-TEST-SORT.

           EXEC CICS ENQ
              RESOURCE(NCS-CUS-NO-NAME)
              LENGTH(16)
              RESP(WS-CICS-RESP)
              RESP2(WS-CICS-RESP2)
           END-EXEC.

           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
             MOVE 'N' TO NEWCUSNO-SUCCESS IN DFHCOMMAREA
             MOVE '3' TO NEWCUSNO-FAIL-CODE IN DFHCOMMAREA
             PERFORM GET-ME-OUT-OF-HERE
           END-IF.

       ENC999.
           EXIT.

       DEQ-NAMED-COUNTER SECTION.
       DNC010.

           MOVE SORTCODE TO
              NCS-CUS-NO-TEST-SORT.


           EXEC CICS DEQ
              RESOURCE(NCS-CUS-NO-NAME)
              LENGTH(16)
              RESP(WS-CICS-RESP)
              RESP2(WS-CICS-RESP2)
           END-EXEC.



           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
             MOVE 'N' TO NEWCUSNO-SUCCESS IN DFHCOMMAREA
             MOVE '5' TO NEWCUSNO-FAIL-CODE IN DFHCOMMAREA
             PERFORM GET-ME-OUT-OF-HERE
           END-IF.



       DNC999.
           EXIT.

       UPD-NCS SECTION.
       UN010.

           MOVE 1 TO NCS-CUS-NO-INC.

            MOVE SORTCODE TO
              NCS-CUS-NO-TEST-SORT

      *
      * Increment the value for the CUSTOMER named counter (get the
      * next available number in the sequence)
      *
             EXEC CICS GET DCOUNTER(NCS-CUS-NO-NAME)
                           VALUE(NCS-CUS-NO-VALUE)
                           POOL(NAMED-COUNTER-POOL)
                           INCREMENT(NCS-CUS-NO-INC)
                           RESP(WS-CICS-RESP)
                           RESP2(WS-CICS-RESP2)
             END-EXEC

             COMPUTE NCS-CUS-NO-VALUE = NCS-CUS-NO-VALUE + 1


      D      DISPLAY 'Just after NCS INC. Resp code=' ws-cics-resp
      D      ' resp2=' WS-CICS-RESP2
      D      ' The NCS number is now=' NCS-CUS-NO-VALUE

             IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
      D         DISPLAY 'WS-CICS-RESP NOT NORMAL IN NEWCUSNO'
                DISPLAY 'NEWCUSNO - CUSTOMER NCS ' NCS-CUS-NO-NAME
                        ' CANNOT BE CUSESSED. RESP CODE='
                        WS-CICS-RESP ',RESP2=' WS-CICS-RESP2
                MOVE 'N' TO NEWCUSNO-SUCCESS IN DFHCOMMAREA
                MOVE '4' TO NEWCUSNO-FAIL-CODE IN DFHCOMMAREA
                PERFORM DEQ-NAMED-COUNTER
                PERFORM GET-ME-OUT-OF-HERE
             END-IF
           MOVE 'Y' TO NCS-UPDATED.
           MOVE NCS-CUS-NO-VALUE TO CUSTOMER-NUMBER IN DFHCOMMAREA.
           MOVE 'Y' TO NEWCUSNO-SUCCESS.
           MOVE 0 TO NEWCUSNO-FAIL-CODE.

       UN999.
           EXIT.

       RESTORE-NCS SECTION.
       RN010.
           COMPUTE NCS-CUS-NO-VALUE = CUSTOMER-number of dfhcommarea
             - 1
           MOVE SORTCODE  TO
              NCS-CUS-NO-TEST-SORT

           EXEC CICS UPDATE DCOUNTER(NCS-CUS-NO-NAME)
              VALUE(NCS-CUS-NO-VALUE)
              POOL(NAMED-COUNTER-POOL)
              RESP(WS-CICS-RESP)
              RESP2(WS-CICS-RESP2)
           END-EXEC

      D     DISPLAY 'Just after NCS UPD. Resp code=' ws-cics-resp
      D     ' resp2=' WS-CICS-RESP2
      D     ' The NCS number is now=' NCS-CUS-NO-VALUE

           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
              MOVE 'N' TO NEWCUSNO-SUCCESS IN DFHCOMMAREA
              MOVE '5' TO NEWCUSNO-FAIL-CODE IN DFHCOMMAREA
              PERFORM DEQ-NAMED-COUNTER
               PERFORM GET-ME-OUT-OF-HERE
           END-IF


           MOVE NCS-CUS-NO-VALUE TO CUSTOMER-NUMBER IN DFHCOMMAREA.
           MOVE 'Y' TO NEWCUSNO-SUCCESS.
           MOVE 0 TO NEWCUSNO-FAIL-CODE.
       RN999.
           EXIT.

      *
      * Finish
      *
       GET-ME-OUT-OF-HERE SECTION.
       GMOFH010.
      * We try not to abend any more but set a return code and leave
      * If we did not succeed, logically we should write to rejtran
      D    DISPLAY 'NEWCUSNO GET-ME-OUT-OF-HERE ' NEWCUSNO-SUCCESS
      D      IN DFHCOMMAREA

           EXEC CICS RETURN
           END-EXEC.

       GMOFH999.
           EXIT.



       GET-NCS SECTION.
       UN010.

           MOVE 0 TO NCS-CUS-NO-INC.

            MOVE SORTCODE TO
              NCS-CUS-NO-TEST-SORT

      *
      * Increment the value for the CUSTOMER named counter (get the
      * next available number in the sequence)
      *
             EXEC CICS GET DCOUNTER(NCS-CUS-NO-NAME)
                           VALUE(NCS-CUS-NO-VALUE)
                           POOL(NAMED-COUNTER-POOL)
                           INCREMENT(NCS-CUS-NO-INC)
                           RESP(WS-CICS-RESP)
                           RESP2(WS-CICS-RESP2)
             END-EXEC



      D      DISPLAY 'Just after NCS INC. Resp code=' ws-cics-resp
      D      ' resp2=' WS-CICS-RESP2
      D      ' The NCS number is now=' NCS-CUS-NO-VALUE

             IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
      D         DISPLAY 'WS-CICS-RESP NOT NORMAL IN NEWCUSNO'
                DISPLAY 'NEWCUSNO - CUSTOMER NCS ' NCS-CUS-NO-NAME
                        ' CANNOT BE CUSESSED. RESP CODE='
                        WS-CICS-RESP
                MOVE 'N' TO NEWCUSNO-SUCCESS IN DFHCOMMAREA
                MOVE '4' TO NEWCUSNO-FAIL-CODE IN DFHCOMMAREA
                PERFORM DEQ-NAMED-COUNTER
                PERFORM GET-ME-OUT-OF-HERE
             END-IF
           MOVE 'Y' TO NCS-UPDATED.
           MOVE NCS-CUS-NO-VALUE TO CUSTOMER-NUMBER IN DFHCOMMAREA.
           MOVE 'Y' TO NEWCUSNO-SUCCESS.
           MOVE 0 TO NEWCUSNO-FAIL-CODE.

       GN999.
           EXIT.

