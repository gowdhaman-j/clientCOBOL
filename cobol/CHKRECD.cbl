000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID.    CHEKRECD.                                                 
000300 AUTHOR. R THORNTON                                                       
000400 REMARKS. UTILITY SKELETON FOR CODING, TESTING RECORDS.                   
000500 ENVIRONMENT DIVISION.                                                    
000600 CONFIGURATION SECTION.                                                   
000700 INPUT-OUTPUT SECTION.                                                    
000800 DATA DIVISION.                                                           
000900 WORKING-STORAGE SECTION.                                                 
001000                                                                          
001100****************************************************************  ******* 
001200*                                                              *        * 
001300*        RECORDS FROM SMF FEEDBACK ACTIVITY.                   *        * 
001400*                                                              *        * 
001500****************************************************************  ******* 
001600****************************************************************  ******* 
001700*                                                              *        * 
001800*        TYPE 04 - STEP TERMINATION RECORD.                    *        * 
001900*                                                              *        * 
002000****************************************************************  ******* 
002100 01  L04LOG.                                                              
002200     05 L04RECLL                 PIC S9(4) COMP.                          
002300     05 L04RECBB                 PIC S9(4) COMP.                          
002400     05 L04PREFX.                                                         
002500        10 L04USRID              PIC X.                                   
002600        10 L04TYPE               PIC X.                                   
002700           88 L04RECD            VALUE 'œ'.                               
002800        10 L04CDATE              PIC S9(7) COMP-3.                        
002900        10 L04CTIME              PIC S9(8) COMP.                          
003000        10 L04SEQNO              PIC S9(8) COMP.                          
003100        10 L04SCTAD PIC S9(8) COMP.                                       
003200     05 L04SYSID                 PIC X(2).                                
003300     05 L04MODID                 PIC X(2).                                
003400     05 L04JOBNM                 PIC X(8).                                
003500     05 L04USER                  PIC X(8).                                
003600     05 L04STETM                 PIC S9(8) COMP.                          
003700     05 L04STEDT                 PIC S9(7) COMP-3.                        
003800     05 L04STSTM                 PIC S9(8) COMP.                          
003900     05 L04STSDT                 PIC S9(7) COMP-3.                        
004000     05 L04STPNO                 PIC X.                                   
004100     05 L04COMCD                 PIC X(2).                                
004200     05 L04STPNM                 PIC X(8).                                
004300     05 L04REGSZ                 PIC S9(4) COMP.                          
004400     05 L04CPUTM                 PIC X(3).                                
004500     05 L04STPTI                 PIC X.                                   
004600     05 L04JCTFG                 PIC X.                                   
004700     05 L04RESRV                 PIC X(4).                                
004800                                                                          
004900 PROCEDURE DIVISION.                                                      
005000 B100-MAINLINE-PROCESSING.                                                
005100     GOBACK.                                                              

