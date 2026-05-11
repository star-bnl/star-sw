
      SUBROUTINE DT_MASHEL(Pa1,Pa2,Xm1,Xm2,P1,P2,Irej)
 
C***********************************************************************
C                                                                      *
C    rescaling of momenta of two partons to put both                   *
C                                       on mass shell                  *
C                                                                      *
C    input:       PA1,PA2   input momentum vectors                     *
C                 XM1,2     desired masses of particles afterwards     *
C                 P1,P2     changed momentum vectors                   *
C                                                                      *
C The original version is written by R. Engel.                         *
C This version dated 12.12.94 is modified by S. Roesler.               *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION anorf , bgx , bgy , bgz , cod , cof , del , 
     &                 DT_YLAMB , ee , ee1 , ee2 , gam , ONE , P1 , P2 , 
     &                 Pa1 , Pa2 , pcmp , ppt , ptot1
      DOUBLE PRECISION ptot2 , px , py , pz , sid , sif , ss , TINY10 , 
     &                 Xm1 , xm12 , Xm2 , xm22 , xms , xptot , xx , yy , 
     &                 ZERO , zz
      INTEGER idev , Irej , k , mode
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (TINY10=1.0D-10,ONE=1.0D0,ZERO=0.0D0)
 
      DIMENSION Pa1(4) , Pa2(4) , P1(4) , P2(4)
 
      Irej = 0
 
C Lorentz transformation into system CMS
      px = Pa1(1) + Pa2(1)
      py = Pa1(2) + Pa2(2)
      pz = Pa1(3) + Pa2(3)
      ee = Pa1(4) + Pa2(4)
      xptot = SQRT(px**2+py**2+pz**2)
      xms = (ee-xptot)*(ee+xptot)
C        WRITE(LOUT,'(3E12.4)')XMS,XM1,XM2
      IF ( xms.LT.(Xm1+Xm2)**2 ) THEN
 
         Irej = 1
         GOTO 99999
      END IF
      xms = SQRT(xms)
      bgx = px/xms
      bgy = py/xms
      bgz = pz/xms
      gam = ee/xms
      CALL DT_DALTRA(gam,-bgx,-bgy,-bgz,Pa1(1),Pa1(2),Pa1(3),Pa1(4),
     &               ptot1,P1(1),P1(2),P1(3),P1(4))
C rotation angles
      cod = P1(3)/ptot1
C     SID = SQRT((ONE-COD)*(ONE+COD))
      ppt = SQRT(P1(1)**2+P1(2)**2)
      sid = ppt/ptot1
      cof = ONE
      sif = ZERO
      IF ( ptot1*sid.GT.TINY10 ) THEN
         cof = P1(1)/(sid*ptot1)
         sif = P1(2)/(sid*ptot1)
         anorf = SQRT(cof*cof+sif*sif)
         cof = cof/anorf
         sif = sif/anorf
      END IF
C new CM momentum and energies (for masses XM1,XM2)
      xm12 = SIGN(Xm1**2,Xm1)
      xm22 = SIGN(Xm2**2,Xm2)
      ss = xms**2
      pcmp = DT_YLAMB(ss,xm12,xm22)/(2.D0*xms)
      ee1 = SQRT(xm12+pcmp**2)
      ee2 = xms - ee1
C back rotation
      mode = 1
      CALL DT_MYTRAN(mode,ZERO,ZERO,pcmp,cod,sid,cof,sif,xx,yy,zz)
      CALL DT_DALTRA(gam,bgx,bgy,bgz,xx,yy,zz,ee1,ptot1,P1(1),P1(2),
     &               P1(3),P1(4))
      CALL DT_DALTRA(gam,bgx,bgy,bgz,-xx,-yy,-zz,ee2,ptot2,P2(1),P2(2),
     &               P2(3),P2(4))
C check consistency
      del = xms*0.0001D0
      IF ( ABS(px-P1(1)-P2(1)).GT.del ) THEN
         idev = 1
      ELSE IF ( ABS(py-P1(2)-P2(2)).GT.del ) THEN
         idev = 2
      ELSE IF ( ABS(pz-P1(3)-P2(3)).GT.del ) THEN
         idev = 3
      ELSE IF ( ABS(ee-P1(4)-P2(4)).GT.del ) THEN
         idev = 4
      ELSE
         idev = 0
      END IF
      IF ( idev.NE.0 ) THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,'(/1X,A,I3)')
     &         'MASHEL: inconsistent transformation' , idev
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,'(1X,A)')
     &         'MASHEL: input momenta/masses:'
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,'(1X,5E12.5)') (Pa1(k),k=1,4) , 
     &        Xm1
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,'(1X,5E12.5)') (Pa2(k),k=1,4) , 
     &        Xm2
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,'(1X,A)')
     &         'MASHEL: output momenta:'
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,'(5X,4E12.5)') (P1(k),k=1,4)
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,'(5X,4E12.5)') (P2(k),k=1,4)
      END IF
99999 END SUBROUTINE
