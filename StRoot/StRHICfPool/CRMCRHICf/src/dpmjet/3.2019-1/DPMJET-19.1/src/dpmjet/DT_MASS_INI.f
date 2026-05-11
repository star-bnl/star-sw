
      SUBROUTINE DT_MASS_INI
C...Initialize  the kinematics for the quasi-elastic cross section
 
      IMPLICIT NONE
      INTEGER j , j0
      SAVE 
 
C particle masses used in qel neutrino scattering modules
      INCLUDE 'inc/qnmass'
 
      EML(1) = 0.51100D-03   ! e-
      EML(2) = EML(1)        ! e+
      EML(3) = 0.105659D0      ! mu-
      EML(4) = EML(3)        ! mu+
      EML(5) = 1.7777D0        ! tau-
      EML(6) = EML(5)        ! tau+
      EMProt = 0.93827231D0    ! p
      EMNeut = 0.93956563D0    ! n
      EMProtsq = EMProt**2
      EMNeutsq = EMNeut**2
      EMN = (EMProt+EMNeut)/2.
      EMNsq = EMN**2
      DO j = 1 , 3
         j0 = 2*(j-1)
         EMN1(j0+1) = EMNeut
         EMN1(j0+2) = EMProt
         EMN2(j0+1) = EMProt
         EMN2(j0+2) = EMNeut
      END DO
      DO j = 1 , 6
         EMLsq(j) = EML(j)**2
         ETQe(j) = ((EMN2(j)+EML(j))**2-EMN1(j)**2)/(2.*EMN1(j))
      END DO
      END SUBROUTINE
