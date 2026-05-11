
      SUBROUTINE DT_DTUINI(Nevts,Epn,Npmass,Npchar,Ntmass,Ntchar,Idp,
     &                     Iemu)
 
      IMPLICIT NONE
      DOUBLE PRECISION dum , Epn
      INTEGER i , Idp , Iemu , iglau , Nevts , Npchar , Npmass , 
     &        Ntchar , Ntmass
      SAVE 
 
C emulsion treatment
      INCLUDE 'inc/dtcomp'
C Glauber formalism: flags and parameters for statistics
      INCLUDE 'inc/dtglgp'
 
      CALL DT_INIT(Nevts,Epn,Npmass,Npchar,Ntmass,Ntchar,Idp,iglau)
      CALL DT_STATIS(1)
 
      CALL PHO_PHIST(1000,dum)
 
      IF ( NCOmpo.LE.0 ) THEN
         CALL DT_SHMAKI(Npmass,Npchar,Ntmass,Ntchar,Idp,Epn,iglau)
      ELSE
         DO i = 1 , NCOmpo
            CALL DT_SHMAKI(Npmass,Npchar,IEMuma(i),IEMuch(i),Idp,Epn,0)
         END DO
      END IF
      IF ( IOGlb.NE.100 ) CALL DT_SIGEMU
      Iemu = IEMul
 
      END SUBROUTINE
