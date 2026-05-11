
      SUBROUTINE PHO_DFWRAP(Mode,Jm1,Jm2)
C**********************************************************************
C
C     wrapper for diffraction dissociation in hadron-nucleus and
C     nucleus-nucleus collisions with DPMJET
C
C     input:      MODE     1:   transformation into CMS
C                          2:   transformation into Lab
C                 JM1/2    indices of old mother particles
C                 JM1/2N   indices of new mother particles
C
C**********************************************************************
 
      IMPLICIT NONE
 
      SAVE 
 
      INTEGER Mode , Jm1 , Jm2
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
 
C  standard particle data interface
 
 
      INCLUDE 'inc/poevt1'
C  extension to standard particle data interface (PHOJET specific)
      INCLUDE 'inc/poevt2'
 
C  nucleon-nucleus / nucleus-nucleus interface to DPMJET
      INCLUDE 'inc/pohdfl'
 
      DOUBLE PRECISION gambed(4) , p1(4) , p2(4) , p3(4) , p4(4) , xm1 , 
     &                 xm2
      DOUBLE PRECISION ss , ecmd , ptot1 , codd , sidd , cofd , sifd , 
     &                 anorf
 
      INTEGER i , nheps , jm1s , jm2s , jm1n , jm2n , irej
 
C  transformation into CMS
 
      IF ( Mode.EQ.1 ) THEN
 
         jm1s = Jm1
         jm2s = Jm2
         nheps = NHEp
 
         xm1 = PHEp(5,Jm1)
         xm2 = PHEp(5,Jm2)
 
C  boost into CMS
         p1(1) = PHEp(1,Jm1) + PHEp(1,Jm2)
         p1(2) = PHEp(2,Jm1) + PHEp(2,Jm2)
         p1(3) = PHEp(3,Jm1) + PHEp(3,Jm2)
         p1(4) = PHEp(4,Jm1) + PHEp(4,Jm2)
         ss = (p1(4)+p1(3))*(p1(4)-p1(3)) - p1(1)**2 - p1(2)**2
         ecmd = SQRT(ss)
         DO i = 1 , 4
            gambed(i) = p1(i)/ecmd
         END DO
         CALL PHO_ALTRA(gambed(4),-gambed(1),-gambed(2),-gambed(3),
     &                  PHEp(1,Jm1),PHEp(2,Jm1),PHEp(3,Jm1),PHEp(4,Jm1),
     &                  ptot1,p1(1),p1(2),p1(3),p1(4))
C  rotation angles
         codd = p1(3)/ptot1
         sidd = SQRT(p1(1)**2+p1(2)**2)/ptot1
         cofd = 1.D0
         sifd = 0.D0
         IF ( ptot1*sidd.GT.1.D-5 ) THEN
            cofd = p1(1)/(sidd*ptot1)
            sifd = p1(2)/(sidd*ptot1)
            anorf = SQRT(cofd*cofd+sifd*sifd)
            cofd = cofd/anorf
            sifd = sifd/anorf
         END IF
 
C  initial particles in CMS
 
         p1(1) = 0.D0
         p1(2) = 0.D0
         p1(3) = ecmd/2.D0*XPSub
         p1(4) = p1(3)
 
         p2(1) = 0.D0
         p2(2) = 0.D0
         p2(3) = -ecmd/2.D0*XTSub
         p2(4) = -p2(3)
 
         CALL PHO_MSHELL(p1,p2,xm1,xm2,p3,p4,irej)
 
         CALL PHO_REGPAR(1,IDHep(Jm1),IMPart(Jm1),Jm1,Jm2,p3(1),p3(2),
     &                   p3(3),p3(4),IPHist(1,Jm1),IPHist(2,Jm1),
     &                   ICOlor(1,Jm1),ICOlor(2,Jm1),jm1n,1)
 
         CALL PHO_REGPAR(1,IDHep(Jm2),IMPart(Jm2),Jm2,Jm1,p4(1),p4(2),
     &                   p4(3),p4(4),IPHist(1,Jm2),IPHist(2,Jm2),
     &                   ICOlor(1,Jm2),ICOlor(2,Jm2),jm2n,1)
 
         Jm1 = jm1n
         Jm2 = jm2n
 
C  transformation into lab.
 
      ELSE IF ( Mode.EQ.2 ) THEN
 
         CALL PHO_LTRHEP(Jm1,NHEp,codd,sidd,cofd,sifd,gambed(4),
     &                   gambed(1),gambed(2),gambed(3))
 
         Jm1 = jm1s
         Jm2 = jm2s
 
C  clean up after rejection
 
      ELSE IF ( Mode.EQ.-2 ) THEN
 
         NHEp = nheps
 
         Jm1 = jm1s
         Jm2 = jm2s
 
      ELSE
 
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I6)')
     &         'PHO_DFWRAP: invalid MODE parameter:' , Mode
 
      END IF
 
      END SUBROUTINE
