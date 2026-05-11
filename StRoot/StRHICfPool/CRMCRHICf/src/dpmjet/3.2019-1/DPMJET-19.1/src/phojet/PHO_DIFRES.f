
      SUBROUTINE PHO_DIFRES(Idmoth,Ival1,Ival2,Idpdg,Idbam,Rmass,Rgam,
     &                      Rwg,Listl)
C**********************************************************************
C
C     list of resonance states for low mass resonances
C
C     input:   IDMOTH       PDG ID of mother particle
C              IVAL1,2      quarks (photon only)
C
C     output:  IDPDG        list of PDG IDs for possible resonances
C              IDBAM        list of corresponding CPC IDs
C              RMASS        mass
C              RGAMS        decay width
C              RMASS        additional weight factor
C              LISTL        entries in current list
C
C**********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION DEPS , EPS , Rgam , Rmass , Rwg , rwght
      INTEGER i , i1 , i2 , Idbam , Idmoth , Idpdg , init , 
     &        IPHO_PDG2ID , irbam , irpdg , Ival1 , Ival2 , Listl
      SAVE 
 
      DIMENSION Idpdg(10) , Idbam(10) , Rmass(10) , Rgam(10) , Rwg(10)
 
      PARAMETER (EPS=1.D-10,DEPS=1.D-15)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  particle ID translation table
      INCLUDE 'inc/popar1'
C  general particle data
      INCLUDE 'inc/popar2'
 
      DIMENSION rwght(20) , irpdg(20) , irbam(20)
      DATA irpdg/113 , 223 , 333 , 50223 , 40113 , 60223 , 10333 , 
     &     30113 , 12212 , 42212 , -12212 , -42212 , 8*0/
      DATA rwght/1.D0 , 0.11D0 , 0.1D0 , 0.11D0 , 1.D0 , 0.11D0 , 
     &     0.1D0 , 1.D0 , 1.D0 , 1.D0 , 1.D0 , 1.D0 , 8*1.D0/
 
      DATA init/0/
 
C  initialize table
      IF ( init.EQ.0 ) THEN
         DO i = 1 , 20
            IF ( irpdg(i).NE.0 ) irbam(i) = IPHO_PDG2ID(irpdg(i))
         END DO
         init = 1
      END IF
 
C  copy table with particles and isospin weights
      Listl = 0
      IF ( Idmoth.EQ.22 ) THEN
         i1 = 4
         i2 = 8
      ELSE IF ( Idmoth.EQ.2212 ) THEN
         i1 = 9
         i2 = 10
      ELSE IF ( Idmoth.EQ.-2212 ) THEN
         i1 = 11
         i2 = 12
      ELSE
         RETURN
      END IF
 
      DO i = i1 , i2
         Listl = Listl + 1
         Idbam(Listl) = irbam(i)
         Idpdg(Listl) = irpdg(i)
         Rmass(Listl) = XM_list(ABS(Idbam(Listl)))
         Rgam(Listl) = GAM_list(ABS(Idbam(Listl)))
         Rwg(Listl) = rwght(i)
      END DO
 
C  debug output
      IF ( IDEb(85).GE.20 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,3I7)')
     &         'PHO_DIFRES: mother,quarks' , Idmoth , Ival1 , Ival2
         DO i = 1 , Listl
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,I3,2I7,E12.4)') i , Idbam(i)
     &           , Idpdg(i) , Rmass(i)
         END DO
      END IF
 
      END SUBROUTINE
