
      SUBROUTINE PHO_VALFLA(Ipar,Ifl1,Ifl2,E1,E2)
C***********************************************************************
C
C     selection of valence flavour decomposition of particle IPAR
C
C     input:    IPAR   particle index in /POEVT1/
C                      -1   initialization
C                      -2   output of statistics
C               XMASS  mass of particle
C                      (important for pomeron:
C                       mass dependent flavour sampling)
C
C     output:   IFL1,IFL2
C               baryon: IFL1  diquark flavour
C               (valence flavours according to PDG conventions)
C
C***********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION DEPS , DT_RNDM , dum , E1 , E2 , emin , EPS
      INTEGER ibar , id , id1 , Ifl1 , Ifl2 , Ipar , IPHO_BAR3 , 
     &        IPHO_DIQU , iter , itmx , k , k1 , k2
      SAVE 
 
      PARAMETER (EPS=0.1D0,DEPS=1.D-15)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
 
C  standard particle data interface
 
 
      INCLUDE 'inc/poevt1'
C  extension to standard particle data interface (PHOJET specific)
      INCLUDE 'inc/poevt2'
 
C  general particle data
      INCLUDE 'inc/popar2'
 
      DATA itmx/5/
 
      IF ( Ipar.GT.0 ) THEN
         k = Ipar
C  select particle code
         id1 = IDHep(k)
         id = ABS(IMPart(k))
         ibar = IPHO_BAR3(k,2)
         iter = 0
 
C10     CONTINUE
 
         Ifl1 = 0
         Ifl2 = 0
         iter = iter + 1
         IF ( iter.GT.itmx ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(1x,2a,i10,1p,2e11.3)')
     &            'PHO_VALFLA: ' , 
     &           'no valences found for (IPAR,E1,E2)' , Ipar , E1 , E2
            RETURN
         END IF
 
C  not baryon
         IF ( ibar.NE.0 ) THEN
            k = INT(2.999999D0*DT_RNDM(E2)) + 1
            k1 = MOD(k,3) + 1
            k2 = MOD(k1,3) + 1
            Ifl1 = IPHO_DIQU(IQ_list(k1,id),IQ_list(k2,id))
            Ifl2 = IQ_list(k,id)
 
C  photon
         ELSE IF ( id1.EQ.22 ) THEN
C  charge dependent flavour sampling
 20         k = INT(DT_RNDM(E1)*6.D0) + 1
            IF ( k.LE.4 ) THEN
               Ifl1 = 2
               Ifl2 = -2
            ELSE IF ( k.EQ.5 ) THEN
               Ifl1 = 1
               Ifl2 = -1
            ELSE
               Ifl1 = 3
               Ifl2 = -3
            END IF
C  optional strangeness suppression
            IF ( (Ifl1.EQ.3) .AND. (DT_RNDM(E2).GT.PARmdl(160)) )
     &           GOTO 20
            IF ( DT_RNDM(dum).LT.0.5D0 ) THEN
               k = Ifl1
               Ifl1 = Ifl2
               Ifl2 = k
            END IF
 
C  pomeron, reggeon
         ELSE IF ( (id1.EQ.990) .OR. (id1.EQ.110) ) THEN
            IF ( ISWmdl(19).EQ.0 ) THEN
C  SU(3) symmetric valences
               k = INT(DT_RNDM(E1)*3.D0) + 1
               IF ( DT_RNDM(dum).LT.0.5D0 ) THEN
                  Ifl1 = k
               ELSE
                  Ifl1 = -k
               END IF
               Ifl2 = -Ifl1
            ELSE IF ( ISWmdl(19).EQ.1 ) THEN
C  mass dependent flavour sampling
               emin = MIN(E1,E2)
               CALL PHO_SEAFLA(Ipar,Ifl1,Ifl2,emin)
            ELSE
               IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,2A,I5)')
     &              'PHO_VALFLA: ' , 
     &              'invalid flavour selection mode ISWMDL(19)' , 
     &              ISWmdl(19)
               CALL PHO_ABORT
            END IF
 
C  meson with flavour mixing
         ELSE IF ( (id1.EQ.111) .OR. (id1.EQ.113) .OR. (id1.EQ.223) )
     &             THEN
            k = INT(2.D0*DT_RNDM(E1)) + 1
            Ifl1 = k
            Ifl2 = -k
C  meson (standard)
         ELSE
            k = INT(2.D0*DT_RNDM(E1)) + 1
            Ifl1 = IQ_list(k,id)
            k = MOD(k,2) + 1
            Ifl2 = IQ_list(k,id)
            IF ( Ifl1.EQ.0 ) THEN
               emin = MIN(E1,E2)
               CALL PHO_SEAFLA(Ipar,Ifl1,Ifl2,emin)
            END IF
 
C  baryon
         END IF
 
C  change sign for antiparticles
         IF ( id1.LT.0 ) THEN
            Ifl1 = -Ifl1
            Ifl2 = -Ifl2
         END IF
 
C***********************************************************************
C  check kinematic constraints
C       IF((PHO_PMASS(IFL1,3).GT.E1)
C    &     .OR.(PHO_PMASS(IFL2,3).GT.E2)) GOTO 10
C***********************************************************************
 
C  debug output
         IF ( LPRi.GT.4 .AND. IDEb(46).GE.10 )
     &         WRITE (LO,'(1X,A,I5,2E12.4,2I7)')
     &         'PHO_VALFLA: IPAR,MASS1/2,FL1/2' , Ipar , E1 , E2 , 
     &        Ifl1 , Ifl2
 
      ELSE IF ( Ipar.EQ.-1 ) THEN
C  initialization
 
      ELSE IF ( Ipar.NE.-2 ) THEN
C  output of final statistics
 
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I10)')
     &         'PHO_VALFLA:ERROR: invalid input particle (IPAR)' , Ipar
         CALL PHO_ABORT
      END IF
 
      END SUBROUTINE
