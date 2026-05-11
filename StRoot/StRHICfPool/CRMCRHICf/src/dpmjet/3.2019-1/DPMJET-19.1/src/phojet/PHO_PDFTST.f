
      SUBROUTINE PHO_PDFTST(Idpdg,Scale2,P2mass)
C*********************************************************************
C
C     structure function test utility
C
C     input:    IDPDG    PDG ID of particle
C               SCALE2   squared scale (GeV**2)
C               P2MASS   particle virtuality (pos, GeV**2)
C
C     output:   tables of PDF, sum rule checking, table of F2
C
C*********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION ala , f2 , fxp , P2mass , pd , pdave , pdsum , 
     &                 q2ma , q2mi , Scale2 , x , xcontr , xdelta , 
     &                 xfirst , xlower , xma , xmi , xsum , xupper , xx
      INTEGER i , Idpdg , iter , k , nstep
      SAVE 
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  currently activated parton density parametrizations
      INCLUDE 'inc/poppdf'
C  some constants
      INCLUDE 'inc/pocons'
 
      DIMENSION pd(-6:6) , pdsum(-6:6) , pdave(-6:6) , fxp(4)
      CHARACTER*8 pdfna
 
      CALL PHO_ACTPDF(Idpdg,1)
      CALL PHO_GETPDF(1,pdfna,ala,q2mi,q2ma,xmi,xma)
 
      IF ( LPRi.GT.4 ) WRITE (LO,'(/,A)') 
     &                        ' *** Structure Function Test Utility ***'
      IF ( LPRi.GT.4 ) WRITE (LO,'(A)') 
     &                        ' ======================================='
 
      IF ( LPRi.GT.4 ) WRITE (LO,'(/,A,3I10)')
     &                         ' used structure function:' , ITYpe(1) , 
     &                        IGRp(1) , ISEt(1)
      IF ( LPRi.GT.4 ) WRITE (LO,'(A,A)') ' corresponds to ' , pdfna
      IF ( LPRi.GT.4 ) WRITE (LO,'(A,E12.3)')
     &                         '  used squared scale (GeV**2):' , Scale2
      IF ( LPRi.GT.4 ) WRITE (LO,'(A,E12.3)')
     &                         ' particle virtuality (GeV**2):' , P2mass
      IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A)') 'x times parton densities'
      IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A)') '    X         PD(-4 - 4)'
      IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A)') 
     &   ' ============================================================'
 
C  logarithmic loop over x values
C  upper bound
      xupper = 0.9999D0
C  lower bound
      xlower = 1.D-4
C  number of steps
      nstep = 50
 
      xfirst = LOG(xlower)
      xdelta = LOG(xupper/xlower)/DBLE(nstep-1)
      DO i = 1 , nstep
         x = EXP(xfirst)
         xcontr = x
         CALL PHO_PDF(1,x,Scale2,P2mass,pd)
         IF ( x.NE.xcontr ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,*) ' x changed! old: ' , xcontr , 
     &           ' new: ' , x
         END IF
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,1P,10E11.4)') xcontr , 
     &        (pd(k),k=-4,4)
         xfirst = xfirst + xdelta
      END DO
 
      IF ( Idpdg.EQ.22 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A)')
     &         'comparison PDF to contribution due to box diagram'
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A)')
     &         '    X   PD(1),PB(1), .... ,PD(4),PB(4)'
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A)') 
     &   ' ============================================================'
         xfirst = LOG(xlower)
         xdelta = LOG(xupper/xlower)/DBLE(nstep-1)
         DO i = 1 , nstep
            x = EXP(xfirst)
            CALL PHO_PDF(1,x,Scale2,P2mass,pd)
            DO k = 1 , 4
               CALL PHO_QPMPDF(k,x,Scale2,0.D0,P2mass,fxp(k))
            END DO
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,1P,9E11.4)') x , 
     &           (pd(k),fxp(k),k=1,4)
            xfirst = xfirst + xdelta
         END DO
      END IF
 
C  check momentum sum rule
 
      IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A)')
     &                         'PHO_PDFTST: estimate of quark sum rules'
      DO i = -6 , 6
         pdsum(i) = 0.D0
         pdave(i) = 0.D0
      END DO
      iter = 5000
      DO i = 1 , iter
         xx = DBLE(i)/DBLE(iter)
         IF ( xx.EQ.1.D0 ) xx = 0.999999D0
         CALL PHO_PDF(1,xx,Scale2,P2mass,pd)
         DO k = -6 , 6
            pdsum(k) = pdsum(k) + pd(k)/xx
            pdave(k) = pdave(k) + pd(k)
         END DO
      END DO
      IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A)') 
     &       'Table: parton-ID, dx-integral over Q(X,Q**2), X*Q(X,Q**2)'
      xsum = 0.D0
      DO i = -6 , 6
         pdsum(i) = pdsum(i)/DBLE(iter)
         pdave(i) = pdave(i)/DBLE(iter)
         xsum = xsum + pdave(i)
         IF ( LPRi.GT.4 ) WRITE (LO,'(9X,I3,3X,2E15.4)') i , pdsum(i) , 
     &        pdave(i)
      END DO
      IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A)')
     &                         'PHO_PDFTST: valence flavours'
      DO i = 1 , 6
         IF ( LPRi.GT.4 ) WRITE (LO,'(9X,I3,E12.4)') i , pdsum(i)
     &        - pdsum(-i)
      END DO
      IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,E12.4)') 'momentum sum rule' , 
     &                        xsum
      IF ( LPRi.GT.4 ) WRITE (LO,'(A/)') 
     &                  ' ============================================='
 
C  table of F2
 
      IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,E12.4,/1X,A)') 
     &                     'PHO_PDFTST: TABLE OF X, F2(X,Q**2) FOR Q**2'
     &                     , Scale2 , 
     &           '-----------------------------------------------------'
      iter = 100
      DO i = 1 , iter
         xx = DBLE(i)/DBLE(iter)
         IF ( xx.EQ.1.D0 ) xx = 0.9999D0
         CALL PHO_PDF(1,xx,Scale2,P2mass,pd)
         f2 = 0.D0
         DO k = -6 , 6
            IF ( k.NE.0 ) f2 = f2 + Q_Ch2(k)*pd(k)
         END DO
         IF ( LPRi.GT.4 ) WRITE (LO,'(5X,1P,2E14.5)') xx , f2
      END DO
      IF ( LPRi.GT.4 ) WRITE (LO,'(A/)') 
     &                  ' ============================================='
      END SUBROUTINE
