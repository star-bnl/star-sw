
      SUBROUTINE PHO_SWAPFLAV(Pd,Ioldfl,Inewfl)
C*********************************************************************
 
C     Change exchange densities of 2 flavors incl. anti-flavor
 
C     input:    pd(-6:6) parton density array
C               ioldfl   old flavor index
C               inewfl   new flavor index
 
C     output:   pd(-6,6) with swapped flavors
 
C*********************************************************************
      IMPLICIT NONE
 
      DOUBLE PRECISION Pd(-6:6) , fl , antifl
      INTEGER Ioldfl , Inewfl
 
      fl = Pd(Ioldfl)
      antifl = Pd(-Ioldfl)
      Pd(Ioldfl) = Pd(Inewfl)
      Pd(-Ioldfl) = Pd(-Inewfl)
      Pd(Inewfl) = fl
      Pd(-Inewfl) = antifl
 
      END SUBROUTINE
