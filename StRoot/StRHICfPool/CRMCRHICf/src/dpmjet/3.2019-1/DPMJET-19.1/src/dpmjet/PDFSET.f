#ifndef FOR_CORSIKA

C**********************************************************************
C
C   dummy subroutines, remove to link PDFLIB
C
C**********************************************************************
      SUBROUTINE PDFSET(Param,Value)
      IMPLICIT NONE
      DOUBLE PRECISION Value
      DIMENSION Param(20) , Value(20)
      CHARACTER*20 Param
      END SUBROUTINE
#endif
