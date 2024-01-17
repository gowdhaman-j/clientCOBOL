       IDENTIFICATION DIVISION.
       PROGRAM-ID. POCPAAS.
       AUTHOR. FD
       REMARKS.
      *****************************************************************
      *                                                               *
      * MODULE NAME = TEST PAAS INSERT                                *
      * Ajouter USAGE IS COMPUTATIONAL-5 pour les puces INTEL         *
      *                                                               *
      *****************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRINTER ASSIGN TO S-OUT1.
       DATA DIVISION.
      *    USAGE IS COMPUTATIONAL-5.
       FILE SECTION.
       FD  PRINTER
           RECORD CONTAINS 120 CHARACTERS
           DATA RECORD IS PRT-TC-RESULTS
           LABEL RECORD IS OMITTED.
       01  PRT-TC-RESULTS.
           03  PRT-BLANK              PIC X(120).
       WORKING-STORAGE SECTION.
      *****************************************************************
      * Variable declarations                                         *
      *****************************************************************
       01  H-EMPTBL.
           05  H-EMPNO   PIC X(6).
           05  H-NAME.
               49 H-NAME-LN   PIC S9(4) COMP-4.
               49 H-NAME-DA   PIC X(32).
           05  H-ADDRESS.
               49 H-ADDRESS-LN   PIC S9(4) COMP-4.
               49 H-ADDRESS-DA   PIC X(36).
           05  H-CITY.
               49 H-CITY-LN   PIC S9(4) COMP-4.
               49 H-CITY-DA   PIC X(36).
           05  H-EMPLOC   PIC X(4).
           05  H-SSNO     PIC X(11).
           05  H-BORN     PIC X(10).
           05  H-SEX      PIC X(1).
           05  H-HIRED    PIC X(10).
           05  H-DEPTNO   PIC X(3).
           05  H-JOBCODE  PIC S9(3)V COMP-3.
           05  H-SRATE    PIC S9(5) COMP.
           05  H-EDUC     PIC S9(5) COMP.
           05  H-SAL      PIC S9(6)V9(2) COMP-3.
           05  H-VALIDCHK PIC S9(6)V COMP-3.
       01  H-EMPTBL-IND-TABLE.
           02  H-EMPTBL-IND        PIC S9(4) COMP OCCURS 15 TIMES.
      *****************************************************************
      * Includes for the variables used in the COBOL standard         *
      * language procedures and the SQLCA.                            *
      *****************************************************************
          EXEC SQL INCLUDE SQLCA END-EXEC.
      *****************************************************************
      * Declaration for the table that contains employee information  *
      *****************************************************************
           EXEC SQL DECLARE WSYGUCH.EMPLOYE TABLE
               (EMPNO   CHAR(6) NOT NULL,
                NAME    VARCHAR(32),
                ADDRESS VARCHAR(36) ,
                CITY    VARCHAR(36) ,
                EMPLOC  CHAR(4) NOT NULL,
                SSNO    CHAR(11),
                BORN    DATE,
                SEX     CHAR(1),
                HIRED   CHAR(10),
                DEPTNO  CHAR(3) NOT NULL,
                JOBCODE DECIMAL(3),
                SRATE   SMALLINT,
                EDUC    SMALLINT,
                SAL     DECIMAL(8,2) NOT NULL,
                VALCHK  DECIMAL(6))
           END-EXEC.
      *****************************************************************
      * Constants                                                     *
      *****************************************************************
       77  STNAME                    PIC X(120) VALUE SPACES.
       77  SITE-1                    PIC X(16) VALUE 'POSTGRE'.
       77  TEMP-ADDRESS-LN           PIC 99    VALUE 15.
       77  TEMP-CITY-LN              PIC 99    VALUE 18.
       77  TEMP-EMPNO                PIC X(6)  VALUE '080000'.
       77  TEMP-NAME-LN              PIC 99    VALUE 8.
       77  WS-NBEMP                  PIC 9(06) VALUE 1.
       77  WS-UPD                    PIC 9(06) VALUE 1.
      *****************************************************************
      * Declaration of the cursor that will be used to retrieve       *
      * information about a transferring employee                     *
      *****************************************************************
           EXEC SQL DECLARE C1 CURSOR FOR
                SELECT EMPNO, NAME, ADDRESS, CITY, EMPLOC,
                       SSNO, BORN, SEX, HIRED, DEPTNO, JOBCODE,
                       SRATE, EDUC, SAL, VALCHK
                FROM   wsyguch.employe
                WHERE EMPNO = :TEMP-EMPNO
           END-EXEC.
       PROCEDURE DIVISION.
       A101-HOUSE-KEEPING.
           OPEN OUTPUT PRINTER.
      *****************************************************************
      * Corp du Programme                                             *
      *****************************************************************
       MAINLINE.
      * Connexion Base Distante DB2 Connect via Alias
           PERFORM INIT-TABLE.
      * Première Boucle Insert Table
           PERFORM VARYING ws-nbemp FROM 2 BY 1
              UNTIL ws-nbemp > 999998
              MOVE WS-NBEMP TO H-EMPNO
              PERFORM INSERT-TABLE
           END-PERFORM.
      * Curseur de Lecture de la Table
           PERFORM VARYING WS-UPD FROM 1000 BY 100
              UNTIL WS-UPD > 999998
              MOVE WS-UPD TO H-EMPNO
              PERFORM UPDATE-ADDRESS
           END-PERFORM.
           PERFORM PROCESS-CURSOR-SITE-1
           PERFORM CLOSE-CURSOR-SITE-1.
           PERFORM COMMIT-WORK.
       PROG-END.
           CLOSE PRINTER.
           GOBACK.
      *****************************************************************
      * AlimenTation défaut champs table                              *
      *****************************************************************
       INIT-TABLE.
           MOVE 'INIT TABLE   ' TO STNAME
           WRITE PRT-TC-RESULTS FROM STNAME.
           MOVE WS-NBEMP             TO H-EMPNO.
           MOVE TEMP-NAME-LN         TO H-NAME-LN.
           MOVE 'GUICHARD'           TO H-NAME-DA.
           MOVE TEMP-ADDRESS-LN      TO H-ADDRESS-LN.
           MOVE '1500 NEW STREET'    TO H-ADDRESS-DA.
           MOVE TEMP-CITY-LN         TO H-CITY-LN.
           MOVE 'NEW CITY, CA 97804' TO H-CITY-DA.
           MOVE 'SJCA'               TO H-EMPLOC.
           MOVE '1710691174'         TO H-SSNO.
           MOVE '1971-06-19'         TO H-BORN.
           MOVE 'M'                  TO H-SEX.
           MOVE '1999-08-01'         TO H-HIRED.
           MOVE 'LTZ'                TO H-DEPTNO.
           MOVE 7                    TO H-JOBCODE.
           MOVE 1                    TO H-SRATE.
           MOVE 2                    TO H-EDUC.
           MOVE 2500.00              TO H-SAL.
           MOVE 1                    TO H-VALIDCHK.
      *****************************************************************
      * Curseur après connexion du POstGre                            *
      *****************************************************************
       PROCESS-CURSOR-SITE-1.
           MOVE 'OPEN CURSOR C1      ' TO STNAME
           WRITE PRT-TC-RESULTS FROM STNAME
           EXEC SQL
              OPEN C1
           END-EXEC.
      *    PERFORM PTSQLCA.
           IF SQLCODE IS EQUAL TO ZERO
              PERFORM FETCH-CURSOR
           END-IF.
      *****************************************************************
      * Fermeture du Curseur                                          *
      *****************************************************************
       FETCH-CURSOR.
           MOVE 'FETCH C1      ' TO STNAME
           WRITE PRT-TC-RESULTS FROM STNAME
           EXEC SQL
              FETCH C1 INTO :H-EMPTBL:H-EMPTBL-IND
           END-EXEC.
      *    PERFORM PTSQLCA.
           IF SQLCODE IS EQUAL TO ZERO
              DISPLAY 'H-EMPTBL :' H-EMPTBL
           END-IF.
      *****************************************************************
      * Fermeture du Curseur                                          *
      *****************************************************************
       CLOSE-CURSOR-SITE-1.
           MOVE 'CLOSE CURSOR C1     ' TO STNAME
           WRITE PRT-TC-RESULTS FROM STNAME
           EXEC SQL
              CLOSE C1
           END-EXEC.
      *    PERFORM PTSQLCA.
      *****************************************************************
      * Update certain employee information in order to make it       *
      * current.                                                      *
      *****************************************************************
       UPDATE-ADDRESS.
           MOVE TEMP-ADDRESS-LN      TO H-ADDRESS-LN.
           MOVE '2000 NEW STREET'    TO H-ADDRESS-DA.
           MOVE TEMP-CITY-LN         TO H-CITY-LN.
           MOVE 'NEW CITY, CA 99999' TO H-CITY-DA.
           MOVE 'SQY2'               TO H-EMPLOC.
           EXEC SQL
              UPDATE wsyguch.employe
              SET ADRESS = :H-ADDRESS,
                  CITY   = :H-CITY,
                  EMPLOC = :H-EMPLOC
              WHERE EMPNO = :H-EMPNO
           END-EXEC.
      *    PERFORM PTSQLCA.
      *****************************************************************
      * Paragraphe pour les inserts                                   *
      *****************************************************************
       INSERT-TABLE.
           MOVE 'INSERT EMPLOYEE     ' TO STNAME
           WRITE PRT-TC-RESULTS FROM STNAME
           EXEC SQL
              INSERT INTO wsyguch.employe VALUES
              (:H-EMPNO,
               :H-NAME,
               :H-ADDRESS,
               :H-CITY,
               :H-EMPLOC,
               :H-SSNO,
               :H-BORN,
               :H-SEX,
               :H-HIRED,
               :H-DEPTNO,
               :H-JOBCODE,
               :H-SRATE,
               :H-EDUC,
               :H-SAL,
               :H-VALIDCHK)
           END-EXEC.
      *    PERFORM PTSQLCA.
      *****************************************************************
      * COMMIT any changes that were made at STLEC1 and STLEC2.       *
      *****************************************************************
       COMMIT-WORK.
           MOVE 'COMMIT WORK         ' TO STNAME
           WRITE PRT-TC-RESULTS FROM STNAME
           EXEC SQL
              COMMIT
           END-EXEC.
      *    PERFORM PTSQLCA.
      *****************************************************************
      * Include COBOL standard language procedures                    *
      *****************************************************************
      *INCLUDE-SUBS.
      *    EXEC SQL INCLUDE COBSSUB END-EXEC.
