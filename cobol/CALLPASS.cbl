000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID.    CALLPASS.                                                 
000300 AUTHOR. R THORNTON                                                       
000400*REMARKS. CALLS ANOTHER COBOL PROGRAM PASSING AN ARGUMENT FIELD           
000410*         CONTAINED IN ITS WORKING-STORAGE SECTION. WHEN COMPILED         
000420*         WITH RENT,DATA(31), THE WORKING STORAGE WILL BE GOTTEN          
000430*         DYNAMICALLY FROM ABOVE THE LINE. PURPOSE OF THIS PROGRAM        
000440*         IS TO TEST THE PROBLEMS (IF ANY) CAUSED BY USE OF THIS          
000450*         SCENARIO IN CALLS BETWEEN AMODE=31 AND AMODE=24 COBOL           
000460*         PROGRAMS. A S0C7 ABEND IS CREATED FOLLOWING RETURN FROM         
000470*         RECEIVER IN ORDER TO PRODUCE A DUMP.                            
000500 ENVIRONMENT DIVISION.                                                    
000600 CONFIGURATION SECTION.                                                   
000700 INPUT-OUTPUT SECTION.                                                    
000710 FILE-CONTROL.                                                            
000800 DATA DIVISION.                                                           
000810 FILE SECTION.                                                            
000900 WORKING-STORAGE SECTION.                                                 
001000 77  PASSED-DATA             PIC X(36)  VALUE                             
001100     'CALLPASS WORKING STORAGE BEGINS HERE'.                              
001200 01  MISCELLANY.                                                          
001300     05  INVAL-PACKED        PIC X(3) VALUE LOW-VALUES.                   
001400     05  MAKE-S0C7           REDEFINES INVAL-PACKED                       
001500                             PIC S9(5) COMP-3.                            
001600                                                                          
001700 PROCEDURE DIVISION.                                                      
001800     DISPLAY 'CALLPASS: CALLING RECEIVER'.                                
002000     CALL 'RECEIVER' USING PASSED-DATA.                                   
002100     DISPLAY 'CALLPASS: RETURNED FROM RECEIVER.'                          
002200     MOVE LOW-VALUES TO INVAL-PACKED.                                     
002210     ADD 1 TO MAKE-S0C7.                                                  
002300     GOBACK.                                                              
  