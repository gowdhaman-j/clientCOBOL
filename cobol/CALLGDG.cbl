000100 IDENTIFICATION DIVISION.                                               00
000200 PROGRAM-ID.    CALLGDG.                                                00
000300 AUTHOR. R THORNTON                                                     00
000400*REMARKS. TEST BUCKET FOR CALLING GETGDG.                               00
000500 ENVIRONMENT DIVISION.                                                  00
000600 CONFIGURATION SECTION.                                                 00
000700 INPUT-OUTPUT SECTION.                                                  00
000800                                                                        00
000900 DATA DIVISION.                                                         00
001000 WORKING-STORAGE SECTION.                                               00
001100                                                                        00
001200 77  FILLER PIC X(36)  VALUE                                            00
001300     'CALLGDG WORKING STORAGE BEGINS HERE'.                             00
001400                                                                        00
001500 01  GETGDG-PARAMETERS.                                                 00
001600     05 GDG-DSNAME PIC X(44) VALUE 'NTM.NETMAN.R410.TABLE(0)'.          00
001700     05 GDG-GENNO  PIC 9(4).                                            00
001800                                                                        00
001900 PROCEDURE DIVISION.                                                    00
002000                                                                        00
002100 A100-EXECUTIVE-CONTROL.                                                00
002200     CALL 'GETGDG' USING GDG-DSNAME, GDG-GENNO.                         00
002300     IF GDG-GENNO = HIGH-VALUES,                                        00
002400         CALL 'COBABEND'.                                               00
002500     GOBACK.                                                            00
  