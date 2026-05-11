
      SUBROUTINE PHO_SEAFLA(Ipar,Ifl1,Ifl2,Chmass)
C**********************************************************************
C
C     selection of sea flavour content of particle IPAR
C
C     input:    IPAR    particle index in /POEVT1/
C               CHMASS  available invariant string mass
C                       positive mass --> use BAMJET method
C                       negative mass --> SU(3) symmetric sea according
C                       to values given in PARMDL(1-6)
C               IPAR    -1 initialization
C                       -2 output of statistics
C
C     output:   sea flavours according to PDG conventions
C
C**********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION Chmass , DEPS , DT_RNDM , EPS , sum , xi
      INTEGER Ifl1 , Ifl2 , Ipar , k , nfsea
      SAVE 
 
      PARAMETER (EPS=0.1D0,DEPS=1.D-15)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  some hadron information, will be deleted in future versions
      INCLUDE 'inc/pohdrn'
 
      IF ( Ipar.GT.0 ) THEN
         IF ( (ISWmdl(20).EQ.1) .OR. (Chmass.LT.0.D0) ) THEN
C  constant weights for sea
 20         sum = 0.D0
            DO k = 1 , nfsea
               sum = sum + PARmdl(k)
            END DO
            xi = DT_RNDM(sum)*sum
            sum = 0.D0
            DO k = 1 , nfsea
               sum = sum + PARmdl(k)
               IF ( xi.LE.sum ) GOTO 40
            END DO
 40         IF ( k.GT.nfsea ) GOTO 20
         ELSE
C  mass dependent flavour sampling
 60         CALL PHO_FLAUX(Chmass,k)
            IF ( k.GT.nfsea ) GOTO 60
         END IF
         IF ( DT_RNDM(Chmass).GT.0.5D0 ) k = -k
         Ifl1 = k
         Ifl2 = -k
         IF ( IDEb(46).GE.10 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,3I5,E12.4)')
     &            'PHO_SEAFLA:IPAR,IFL1,IFL2,MASS' , Ipar , Ifl1 , 
     &           Ifl2 , Chmass
         END IF
      ELSE IF ( Ipar.EQ.-1 ) THEN
C  initialization
         nfsea = NFS
      ELSE IF ( Ipar.NE.-2 ) THEN
C  output of statistics
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I10)')
     &         'PHO_SEAFLA:ERROR:INVALID IPAR' , Ipar
         CALL PHO_ABORT
      END IF
 
      END SUBROUTINE
