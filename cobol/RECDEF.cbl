000100****************************************************************          
000200*                                                              *          
000300*        RECORDS FROM SMF FEEDBACK ACTIVITY.                   *          
000400*                                                              *          
000500****************************************************************          
000600****************************************************************          
000700*                                                              *          
000800*        TYPE 04 - STEP TERMINATION RECORD.                    *          
000900*                                                              *          
001000****************************************************************          
001100 01  L04LOG.                                                              
001200     05 L04RECLL                 PIC S9(4) COMP.                          
001300     05 L04RECBB                 PIC S9(4) COMP.                          
001400     05 L04PREFX.                                                         
001500        10 L04USRID              PIC X.                                   
001600        10 L04TYPE               PIC X.                                   
001700           88 L04RECD            VALUE '�'.                               
001800        10 L04CDATE              PIC S9(7) COMP-3.                        
001900        10 L04CTIME              PIC S9(8) COMP.                          
002000        10 L04SEQNO              PIC S9(8) COMP.                          
002100        10 L04SCTAD PIC S9(8) COMP.                                       
002200     05 L04SYSID                 PIC X(2).                                
002300     05 L04MODID                 PIC X(2).                                
002400     05 L04JOBNM                 PIC X(8).                                
002500     05 L04USER                  PIC X(8).                                
002600     05 L04STETM                 PIC S9(8) COMP.                          
002700     05 L04STEDT                 PIC S9(7) COMP-3.                        
002800     05 L04STSTM                 PIC S9(8) COMP.                          
002900     05 L04STSDT                 PIC S9(7) COMP-3.                        
003000     05 L04STPNO                 PIC X.                                   
003100     05 L04COMCD                 PIC X(2).                                
003200     05 L04STPNM                 PIC X(8).                                
003300     05 L04REGSZ                 PIC S9(4) COMP.                          
003400     05 L04CPUTM                 PIC X(3).                                
003500     05 L04STPTI                 PIC X.                                   
003600     05 L04JCTFG                 PIC X.                                   
003700     05 L04RESRV                 PIC X(4).                                
  