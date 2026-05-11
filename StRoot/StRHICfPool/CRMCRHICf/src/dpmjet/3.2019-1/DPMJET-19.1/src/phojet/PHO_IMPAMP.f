
      SUBROUTINE PHO_IMPAMP(Ee,Bmin,Bmax,Nstep)
C*********************************************************************
C
C     calculation of physical  impact parameter amplitude
C
C     input:   EE      cm energy (GeV)
C              BMIN    lower bound in B
C              BMAX    upper bound in B
C              NSTEP   number of values (linear)
C
C     output:  values written to output unit
C
C*********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION bb , Bmax , Bmin , bstep , DEPS , Ee , ONEM , 
     &                 THOUS
      INTEGER i , Nstep
      SAVE 
 
      PARAMETER (ONEM=-1.D0,THOUS=1.D3,DEPS=1.D-20)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  global event kinematics and particle IDs
      INCLUDE 'inc/pogcms'
C  complex Born graph amplitudes used for unitarization
      INCLUDE 'inc/point4'
 
      ECM = Ee
      bstep = (Bmax-Bmin)/DBLE(Nstep-1)
C
      IF ( LPRi.GT.4 ) WRITE (LO,'(3(/,1X,A))')
     &                         'impact parameter amplitudes:' , 
     &   '  B  AMP-EL  AMP-LMSD(1,2)  AMP-HMSD(1,2)  AMP-LMDD  AMP-HMDD'
     &   , 
     &   '-------------------------------------------------------------'
C
      bb = Bmin
      DO i = 1 , Nstep
C  calculate impact parameter amplitudes
         IF ( i.EQ.1 ) THEN
            CALL PHO_EIKON(1,-1,Bmin)
         ELSE
            CALL PHO_EIKON(1,1,bb)
         END IF
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,8E12.4)') bb , DREAL(AMPel) , 
     &        DREAL(AMLmsd(1)) , DREAL(AMLmsd(2)) , DREAL(AMHmsd(1)) , 
     &        DREAL(AMHmsd(2)) , DREAL(AMLmdd) , DREAL(AMHmdd)
         bb = bb + bstep
      END DO
 
      END SUBROUTINE
