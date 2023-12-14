
   
CREATE TABLE EMPLOYE (
				EMPNO CHAR(6) NOT NULL,
                NAME VARCHAR(32),
                ADDRESS VARCHAR(36) ,
                CITY VARCHAR(36) ,
                EMPLOC CHAR(4) NOT NULL,
                SSNO CHAR(11),
                BORN DATE,
                SEX CHAR(1),
                HIRED CHAR(10),
                DEPTNO CHAR(3) NOT NULL,
                JOBCODE DECIMAL(3),
                SRATE SMALLINT,
                EDUC SMALLINT,
                SAL DECIMAL(8,2) NOT NULL,
                VALCHK DECIMAL(6),
   PRIMARY KEY(EMPNO))
   CCSID EBCDIC
   IN GENATS01;   
   
 