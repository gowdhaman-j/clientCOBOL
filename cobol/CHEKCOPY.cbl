000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID.    CHEKCOPY.                                                 
000300 AUTHOR. R THORNTON                                                       
000400 REMARKS. UTILITY SKELETON FOR CODING, TESTING COPY FUNCTIONS.            
000500 ENVIRONMENT DIVISION.                                                    
000600 CONFIGURATION SECTION.                                                   
000700 INPUT-OUTPUT SECTION.                                                    
000800 DATA DIVISION.                                                           
000900 WORKING-STORAGE SECTION.                                                 
001000 01  ABC  COPY FDGRPMST.                                                  
001100                                                                          
001200 PROCEDURE DIVISION.                                                      
001300 B100-MAINLINE-PROCESSING.                                                
001400     GOBACK.                                                              
  