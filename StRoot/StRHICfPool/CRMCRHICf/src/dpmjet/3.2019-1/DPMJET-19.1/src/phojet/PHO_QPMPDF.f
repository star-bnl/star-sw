
      SUBROUTINE PHO_QPMPDF(Iq,X,Scale2,Ptref,Pvirt,Fxp)
C***************************************************************
C
C     contribution to photon PDF from box graph
C     (Bethe-Heitler process)
C
C     input:      IQ       quark flavour
C                 SCALE2   scale (GeV**2, positive)
C                 PTREF    reference scale (GeV, positive)
C                 X        parton momentum fraction
C                 PVIRT    photon virtuality (GeV**2, positive)
C                 FXP      x*f(x,Q**2), x times parton density
C
C***************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION be , bm , bp , Fxp , Ptref , Pvirt , qm , qm2 , 
     &                 Scale2 , w2 , X
      INTEGER i , Iq
      SAVE 
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  internal rejection counters
      INCLUDE 'inc/poloop'
C  some constants
      INCLUDE 'inc/pocons'
 
      DIMENSION qm(6)
      DATA qm/0.2D0 , 0.25D0 , 0.5D0 , 1.5D0 , 4.5D0 , 174.D0/
 
      Fxp = 0.D0
      i = ABS(Iq)
C
C     QM2 = MAX(QM(I),PTREF)**2
C     QM2 = MAX(QM2,PVIRT)
C     BBE = (1.D0-X)*SCALE2
C     IF(BBE.LE.0.D0) THEN
C       IF(IDEB(27).GE.5) WRITE(LO,'(1X,A,4E10.3)')
C    &    'PHO_QPMPDF: over mass limit (X,Q2,P2,QM)',X,SCALE2,
C    &    PVIRT,QM(I)
C     ENDIF
C     FXP = X*(4.D0-3.D0*MOD(I,2))/9.D0*3.D0/(2.D0*137.D0*PI)
C    &  *((X**2+(1.D0-X)**2)*LOG(BBE/(QM2*X))+8.D0*X*(1.D0-X)-1.D0)
C  Bethe-Heitler process approximation for 2*x*p2/q2 << 1
      qm2 = MAX(qm(i),Ptref)**2
      w2 = Scale2/X*(1.D0-X-X*Pvirt/Scale2)
      IF ( w2.GT.4.D0*qm2 ) THEN
         be = SQRT(1.D0-4.D0*qm2/w2)
         bp = SQRT(1.D0+be*(1.D0-4.D0*X*X*Pvirt/Scale2))
         bm = SQRT(1.D0-be*(1.D0-4.D0*X*X*Pvirt/Scale2))
C       FXP = X*(4.D0-3.D0*MOD(I,2))/9.D0*3.D0/(137.D0*PI)*(BE*(-1.D0
         Fxp = X*Q_Ch2(i)*3.D0/(137.D0*PI)
     &         *(be*(-1.D0+6.D0*X-6.D0*X*X)+2.D0*X*X*((2.D0*qm2-Pvirt)
     &         /Scale2-4.D0*qm2*qm2/Scale2**2)*(1.D0/bm-1.D0/bp)
     &         +(X*X+(1.D0-X)**2+X*(1-3.D0*X)*4.D0*qm2/Scale2-
     &         X*X*8.D0*qm2*qm2/Scale2**2)*LOG(bp/bm))
      ELSE
         IF ( LPRi.GT.4 .AND. IDEb(27).GE.5 ) WRITE (LO,'(1X,A,4E10.3)')
     &         'PHO_QPMPDF: under mass limit (X,Q2,P2,QM)' , X , 
     &        Scale2 , Pvirt , qm(i)
      END IF
C  debug output
      IF ( LPRi.GT.4 .AND. IDEb(27).GE.20 )
     &      WRITE (LO,'(1X,A,I3,1P,5E10.3)') 'PHO_QPMPDF: X,Q2,P2,QM' , 
     &     i , X , Scale2 , Pvirt , qm(i) , Fxp
      END SUBROUTINE
