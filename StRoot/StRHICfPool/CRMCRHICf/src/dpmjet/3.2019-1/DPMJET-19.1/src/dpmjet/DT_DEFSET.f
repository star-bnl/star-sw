
      BLOCK DATA DT_DEFSET
 
      IMPLICIT NONE
      SAVE 
 
C flags for input different options
      INCLUDE 'inc/dtflg1'
C emulsion treatment
      INCLUDE 'inc/dtcomp'
 
      INCLUDE 'inc/dtflka'
 
C / DTFLG1 /
      DATA IFRag/2 , 1/
      DATA IREsco/1/
      DATA IMShl/1/
      DATA IREsrj/0/
      DATA IOUlev/ - 1 , -1 , -1 , -1 , -1 , -1/
      DATA LEMcck/.FALSE./
      DATA LHAdro/.FALSE. , .TRUE. , .TRUE. , .TRUE. , .TRUE. , .TRUE. , 
     &     .TRUE. , .TRUE. , .TRUE. , .TRUE./
      DATA LSEadi/.TRUE./
      DATA LEVapo/.TRUE./
      DATA IFRame/1/
      DATA ITRspt/0/
 
C / DTCOMP /
      DATA EMUfra/NCOMPX*0.0D0/
      DATA IEMuma/NCOMPX*1/
      DATA IEMuch/NCOMPX*1/
      DATA NCOmpo/0/
      DATA IEMul/0/
 
C / DTFLKA /
      DATA LINp , LOUt , LDAt/5 , 19 , 9/
 
      END BLOCK DATA
