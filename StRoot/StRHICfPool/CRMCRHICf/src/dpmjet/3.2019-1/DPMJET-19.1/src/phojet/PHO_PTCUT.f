
      DOUBLE PRECISION FUNCTION PHO_PTCUT(Ecm,Ip)
C***********************************************************************
C
C     calculate energy-dependent transverse momentum cutoff
C
C***********************************************************************
 
      IMPLICIT NONE
 
      SAVE 
 
      DOUBLE PRECISION Ecm
      INTEGER Ip
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
 
      IF ( IPAmdl(7).EQ.0 ) THEN
C  energy-independent cutoff
         PHO_PTCUT = PARmdl(35+Ip)
      ELSE IF ( IPAmdl(7).EQ.1 ) THEN
C  Bopp et al. type (DPMJET)
         PHO_PTCUT = PARmdl(35+Ip)
     &               + MAX(0.D0,0.12D0*(LOG10(Ecm/50.D0))**3)
      ELSE IF ( IPAmdl(7).EQ.2 ) THEN
C  Gribov-Levin-Ryskin type energy dependence
         PHO_PTCUT = PARmdl(35+Ip)
     &               + 0.065D0*EXP(0.9D0*SQRT(2.D0*LOG(Ecm)))
      ELSE IF ( IPAmdl(7).EQ.3 ) THEN
C  Golec-Biernat-Wusthoff type dependence
         PHO_PTCUT = PARmdl(250)*((Ecm+PARmdl(251))/PARmdl(252))
     &               **PARmdl(253)
      ELSE
         PHO_PTCUT = 0.D0
         WRITE (LO,*) 'PHO_PTCUT: UNKNOWN MODEL SWITCH' , IPAmdl(7)
         CALL PHO_ABORT
      END IF
 
      END FUNCTION
