000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID.  COPYERRS.                                                   
000300 AUTHOR.  R THORNTON.                                                     
000400 INSTALLATION.  BLUE CROSS AND BLUE SHIELD OF VIRGINIA.                   
000500 DATE-WRITTEN.  NOV, 1987.                                                
000600 DATE-COMPILED.                                                           
000700****************************************************************          
000800*          THE PURPOSE OF THIS PROGRAM IS TO DETERMINE WHY A   *          
000900*          PROGRAM GETS AN ERROR ON A COPY BOOK.               *          
001000****************************************************************          
001100 ENVIRONMENT DIVISION.                                                    
001200 DATA DIVISION.                                                           
001300 WORKING-STORAGE SECTION.                                                 
001400 77  FILLER                     PIC X(36)  VALUE                          
001500     'COPYERRS WORKING STORAGE BEGINS HERE'.                              
001600 01  MISCELLANEOUS-WORK.                                                  
001700     05  COMPILE-DATE           PIC X(20).                                
001800     05  FIRST-TIME-SWITCH      PIC X VALUE ' '.                          
001900 01  MISC-WORK.                                                           
002000     05  FILLER                 PIC X(20).                                
002100 01  CLAIMANT-SEGMENT COPY CSCLMT00.                                      
002200 PROCEDURE DIVISION.                                                      
002300 A100-MAINLINE.                                                           
002400     PERFORM B100-INITIALIZATION THRU B100-EXIT.                          
002500     GOBACK.                                                              
002600 B100-INITIALIZATION.                                                     
002700     MOVE '1' TO FIRST-TIME-SWITCH.                                       
002800 B100-EXIT.                                                               
002900     EXIT.                                                                
  