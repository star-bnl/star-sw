
      SUBROUTINE PHO_HARZSP(Ifla,Iflb,Nfsh,Zmin,Zmax,Zz)
C*********************************************************************
C
C     sampling of z values from DGLAP kernels
C
C     input:  IFLA,IFLB      parton flavours
C             NFSH           flavours involved in hard processes
C             ZMIN           minimal ZZ allowed
C             ZMAX           maximal ZZ allowed
C
C     output: ZZ             z value
C
C*********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION c1 , c2 , DEPS , DT_RNDM , Zmax , Zmin , Zz
      INTEGER Ifla , Iflb , Nfsh
      SAVE 
 
      PARAMETER (DEPS=1.D-10)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  internal rejection counters
      INCLUDE 'inc/poloop'
 
      IF ( Zmax.LE.Zmin ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,2E12.3)')
     &         'PHO_HARZSP: ZMAX<ZMIN (ZMAX,ZMIN)' , Zmax , Zmin
         CALL PHO_PREVNT(-1)
         Zz = 0.D0
         RETURN
      END IF
C
      IF ( Iflb.EQ.0 ) THEN
         IF ( Ifla.EQ.0 ) THEN
C  g --> g g
            c1 = Zmax/Zmin*(1.D0-Zmin)/(1.D0-Zmax)
            c2 = (1.D0-Zmin)/Zmin
 20         Zz = 1.D0/(1.D0+c2/c1**DT_RNDM(Zmin))
            IF ( ((1.D0-Zz*(1.D0-Zz))**2).LT.DT_RNDM(Zmax) ) GOTO 20
         ELSE IF ( ABS(Ifla).LE.Nfsh ) THEN
C  q --> q g
            c1 = Zmax/Zmin
 40         Zz = Zmin*c1**DT_RNDM(Zmin)
            IF ( (0.5D0*(1.D0+(1.D0-Zz)**2)).LT.DT_RNDM(Zmax) ) GOTO 40
         ELSE
            GOTO 100
         END IF
      ELSE IF ( ABS(Iflb).LE.Nfsh ) THEN
         IF ( Ifla.EQ.0 ) THEN
C  g --> q qb
            c1 = Zmax - Zmin
 60         Zz = Zmin + c1*DT_RNDM(Zmin)
            IF ( (2.D0*Zz*(Zz-1.D0)+1.D0).LT.DT_RNDM(Zmax) ) GOTO 60
         ELSE IF ( ABS(Ifla).LE.Nfsh ) THEN
C  q --> g q
            c1 = (1.D0-Zmax)/(1.D0-Zmin)
            c2 = 1.D0 - Zmin
 80         Zz = 1.D0 - c2*c1**DT_RNDM(Zmin)
            IF ( 0.5D0*(1.D0+Zz**2).LT.DT_RNDM(Zmax) ) GOTO 80
         ELSE
            GOTO 100
         END IF
      ELSE
         GOTO 100
      END IF
C  debug output
      IF ( LPRi.GT.4 .AND. IDEb(84).GE.20 )
     &      WRITE (LO,'(1X,A,2I3,3E11.3)')
     &      'PHO_HARZSP: IFLA,IFLB,ZZ,ZMIN,ZMAX' , Ifla , Iflb , Zz , 
     &     Zmin , Zmax
      RETURN
 
 100  IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,2I7)')
     &                         'PHO_HARZSP:ERROR: invalid flavours A,B'
     &                        , Ifla , Iflb
      CALL PHO_ABORT
 
      END SUBROUTINE
