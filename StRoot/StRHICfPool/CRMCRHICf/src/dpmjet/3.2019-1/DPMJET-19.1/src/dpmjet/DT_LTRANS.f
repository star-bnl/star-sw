
      SUBROUTINE DT_LTRANS(Pxi,Pyi,Pzi,Pei,Pxo,Pyo,Pzo,Peo,Id,Mode)
 
C***********************************************************************
C Lorentz-transformations.                                             *
C   MODE = 1(-1)    projectile rest syst.   --> Lab (back)             *
C        = 2(-2)    projectile rest syst.   --> nucl.-nucl.cms (back)  *
C        = 3(-3)    target rest syst. (=Lab)--> nucl.-nucl.cms (back)  *
C This version dated 01.11.95 is written by  S. Roesler.               *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION amdif2 , amo2 , amorq2 , delta , Pei , Peo , po , 
     &                 po1 , Pxi , Pxo , Pyi , Pyo , Pzi , Pzo , 
     &                 SQTINF , TINY3 , TWO , ZERO
      INTEGER Id , Mode
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (TINY3=1.0D-3,ZERO=0.0D0,TWO=2.0D0)
 
      PARAMETER (SQTINF=1.0D+15)
 
C particle properties (BAMJET index convention)
      INCLUDE 'inc/dtpart'
 
      Pxo = Pxi
      Pyo = Pyi
      CALL DT_LTNUC(Pzi,Pei,Pzo,Peo,Mode)
 
C check particle mass for consistency (numerical rounding errors)
      po = SQRT(Pxo*Pxo+Pyo*Pyo+Pzo*Pzo)
      amo2 = (Peo-po)*(Peo+po)
      amorq2 = AAM(Id)**2
      amdif2 = ABS(amo2-amorq2)
      IF ( (amdif2.GT.TINY3) .AND. (Peo.LT.SQTINF) .AND. (po.GT.ZERO) )
     &     THEN
         delta = (amorq2-amo2)/(TWO*(Peo+po))
         Peo = Peo + delta
         po1 = po - delta
         Pxo = Pxo*po1/po
         Pyo = Pyo*po1/po
         Pzo = Pzo*po1/po
C        WRITE(6,*) 'LTRANS corrected', AMDIF2,PZI,PEI,PZO,PEO,MODE,ID
      END IF
 
      END SUBROUTINE
