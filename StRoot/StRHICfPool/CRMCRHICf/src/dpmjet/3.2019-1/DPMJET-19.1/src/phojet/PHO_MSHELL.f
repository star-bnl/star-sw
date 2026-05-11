
      SUBROUTINE PHO_MSHELL(Pa1,Pa2,Xm1,Xm2,P1,P2,Irej)
C********************************************************************
C
C    rescaling of momenta of two partons to put both
C                                       on mass shell
C
C    input:       PA1,PA2   input momentum vectors
C                 XM1,2     desired masses of particles afterwards
C                 P1,P2     changed momentum vectors
C
C********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION anorf , bgx , bgy , bgz , cod , cof , del , 
     &                 DEPS , ee , ee1 , ee2 , gam , P1 , P2 , Pa1 , 
     &                 Pa2 , pcmp , PHO_XLAM , ptot1 , ptot2
      DOUBLE PRECISION px , py , pz , sid , sif , ss , Xm1 , xm12 , 
     &                 Xm2 , xm22 , xms , xx , yy , zz
      INTEGER idev , Irej , k
      SAVE 
 
      PARAMETER (DEPS=1.D-20)
 
      DIMENSION Pa1(*) , Pa2(*) , P1(*) , P2(*)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  internal rejection counters
      INCLUDE 'inc/poloop'
 
      Irej = 0
      idev = 0
C  debug output
      IF ( IDEb(40).GE.10 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A)')
     &         'PHO_MSHELL: input momenta:'
         IF ( LPRi.GT.4 ) WRITE (LO,'(5X,4E12.5)') (Pa1(k),k=1,4)
         IF ( LPRi.GT.4 ) WRITE (LO,'(5X,4E12.5)') (Pa2(k),k=1,4)
         IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,2E12.3)') 'new masses:' , 
     &        Xm1 , Xm2
      END IF
 
C  Lorentz transformation into system CMS
      px = Pa1(1) + Pa2(1)
      py = Pa1(2) + Pa2(2)
      pz = Pa1(3) + Pa2(3)
      ee = Pa1(4) + Pa2(4)
      xms = ee**2 - px**2 - py**2 - pz**2
      IF ( xms.LT.(Xm1+Xm2)**2 ) THEN
         Irej = 1
         IFAil(37) = IFAil(37) + 1
 
         IF ( (Xm1.GT.1.D4) .OR. (Xm2.GT.1.D4) ) Irej = Irej/idev
 
         IF ( IDEb(40).GE.3 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I12)')
     &            'PHO_MSHELL:reject: too small string mass (KEVENT)' , 
     &           KEVent
            IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,3E12.4)')
     &            'two-part.mass, part.masses:' , 
     &           SIGN(SQRT(ABS(xms)),xms) , Xm1 , Xm2
            IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,4E11.4)') 'PX,PY,PZ,EE:' , 
     &           px , py , pz , ee
            idev = 5
            IF ( IDEb(40).GE.3 ) GOTO 100
         END IF
         RETURN
      END IF
      xms = SQRT(xms)
      bgx = px/xms
      bgy = py/xms
      bgz = pz/xms
      gam = ee/xms
      CALL PHO_ALTRA(gam,-bgx,-bgy,-bgz,Pa1(1),Pa1(2),Pa1(3),Pa1(4),
     &               ptot1,P1(1),P1(2),P1(3),P1(4))
C  rotation angles
      ptot1 = MAX(DEPS,ptot1)
      cod = P1(3)/ptot1
      sid = SQRT(P1(1)**2+P1(2)**2)/ptot1
      cof = 1.D0
      sif = 0.D0
      IF ( ptot1*sid.GT.1.D-5 ) THEN
         cof = P1(1)/(sid*ptot1)
         sif = P1(2)/(sid*ptot1)
         anorf = SQRT(cof*cof+sif*sif)
         cof = cof/anorf
         sif = sif/anorf
      END IF
 
C  new CM momentum and energies (for masses XM1,XM2)
      xm12 = Xm1**2
      xm22 = Xm2**2
      ss = xms**2
      pcmp = PHO_XLAM(ss,xm12,xm22)/(2.D0*xms)
      ee1 = SQRT(xm12+pcmp**2)
      ee2 = xms - ee1
C  back rotation
      CALL PHO_TRANS(0.D0,0.D0,pcmp,cod,sid,cof,sif,xx,yy,zz)
      CALL PHO_ALTRA(gam,bgx,bgy,bgz,xx,yy,zz,ee1,ptot1,P1(1),P1(2),
     &               P1(3),P1(4))
      CALL PHO_ALTRA(gam,bgx,bgy,bgz,-xx,-yy,-zz,ee2,ptot2,P2(1),P2(2),
     &               P2(3),P2(4))
 
C  check consistency
      del = xms*0.0001
      IF ( ABS(px-P1(1)-P2(1)).GT.del ) THEN
         idev = 1
      ELSE IF ( ABS(py-P1(2)-P2(2)).GT.del ) THEN
         idev = 2
      ELSE IF ( ABS(pz-P1(3)-P2(3)).GT.del ) THEN
         idev = 3
      ELSE IF ( ABS(ee-P1(4)-P2(4)).GT.del ) THEN
         idev = 4
      END IF
C  debug output
 100  IF ( idev.NE.0 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I3)')
     &         'PHO_MSHELL: inconsistent transformation (IDEV)' , idev
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A)')
     &         'PHO_MSHELL: input momenta:'
         IF ( LPRi.GT.4 ) WRITE (LO,'(5X,4E12.5)') (Pa1(k),k=1,4)
         IF ( LPRi.GT.4 ) WRITE (LO,'(5X,4E12.5)') (Pa2(k),k=1,4)
         IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,3E12.3)')
     &        'ava.mass,masses:' , xms , Xm1 , Xm2
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A)')
     &         'PHO_MSHELL: output momenta:'
         IF ( LPRi.GT.4 ) WRITE (LO,'(5X,4E12.5)') (P1(k),k=1,4)
         IF ( LPRi.GT.4 ) WRITE (LO,'(5X,4E12.5)') (P2(k),k=1,4)
      ELSE IF ( IDEb(40).GE.10 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A)')
     &         'PHO_MSHELL: output momenta:'
         IF ( LPRi.GT.4 ) WRITE (LO,'(5X,4E12.5)') (P1(k),k=1,4)
         IF ( LPRi.GT.4 ) WRITE (LO,'(5X,4E12.5)') (P2(k),k=1,4)
      END IF
      END SUBROUTINE
