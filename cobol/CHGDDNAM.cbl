014500 IDENTIFICATION DIVISION.                                                 
014600 PROGRAM-ID.    CHGDDNAM.                                                 
014700 DATE-COMPILED.                                                           
014800******************************************************************        
014900*THE CHGDDNAM SUBROUTINE SETS THE DDNAME IN THE FD PASSED BY THE *        
015000*CALLER TO DATE. THIS IS NECESSARY IN ORDER TO DUPLICATE THE     *        
015100*FUNCTION OF THE OLD DATE SUBROUTINE WHICH WAS WRITTEN IN        *        
015200*ASSEMBLER AND USED THE DDNAME DATE, WHICH IS A RESERVED WORD    *        
015300*IN COBOL.                                                       *        
015400*                                                                *        
015500*AUTHOR R THORNTON - 11/01/93                                    *        
015600******************************************************************        
015700 ENVIRONMENT DIVISION.                                                    
015800 INPUT-OUTPUT SECTION.                                                    
015900 FILE-CONTROL.                                                            
016000 DATA DIVISION.                                                           
016100 FILE SECTION.                                                            
016200 WORKING-STORAGE SECTION.                                                 
016300 LINKAGE SECTION.                                                         
016400 01  FD-AREA.                                                             
016500     05  FILLER              PIC X(40).                                   
016600     05  FD-DDNAME           PIC X(8).                                    
016700 PROCEDURE DIVISION USING FD-AREA.                                        
016800     MOVE 'DATE    ' TO FD-DDNAME.                                        
016900     GOBACK.                                                              
017000 END PROGRAM CHGDDNAM.                                                    
  