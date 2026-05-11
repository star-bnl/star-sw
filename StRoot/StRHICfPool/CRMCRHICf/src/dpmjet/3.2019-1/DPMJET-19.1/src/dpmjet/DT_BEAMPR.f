
      SUBROUTINE DT_BEAMPR(What,Plab,Mode)
 
C***********************************************************************
C Initialization of event generation                                   *
C This version dated  7.4.98  is written by S. Roesler.                *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION anorf , bge , bgx , bgy , bgz , BOG , cod , cof , 
     &                 e1 , e2 , ecm , elab , ONE , p1cms , p1tot , 
     &                 p2cms , p2tot , pecms , ph , Plab
      DOUBLE PRECISION pp1 , pp2 , ppt , ptot , pxcms , pycms , pzcms , 
     &                 sid , sif , th , TINY10 , TWOPI , What , ZERO
      INTEGER i , Mode
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ZERO=0.0D0,ONE=1.0D0,TINY10=1.0D-10)
      PARAMETER (TWOPI=6.283185307D0,BOG=TWOPI/360.0D0)
 
      LOGICAL lbeam
 
C event history
 
 
      INCLUDE 'inc/dtevt1'
C extended event history
      INCLUDE 'inc/dtevt2'
C properties of interacting particles
      INCLUDE 'inc/dtprta'
C particle properties (BAMJET index convention)
      INCLUDE 'inc/dtpart'
C beam momenta
      INCLUDE 'inc/dtbeam'
 
C     DIMENSION WHAT(6),P1(4),P2(4),P1CMS(4),P2CMS(4)
      DIMENSION What(6) , p1cms(4) , p2cms(4)
 
      DATA lbeam/.FALSE./
 
      IF ( Mode.EQ.2 ) THEN
 
 
         IF ( lbeam ) THEN
            IF ( (NPOint(4).EQ.0) .OR. (NHKk.LT.NPOint(4)) ) RETURN
            DO i = NPOint(4) , NHKk
               IF ( (ABS(ISThkk(i)).EQ.1) .OR. (ISThkk(i).EQ.1000) .OR. 
     &              (ISThkk(i).EQ.1001) ) THEN
                  CALL DT_MYTRAN(1,PHKk(1,i),PHKk(2,i),PHKk(3,i),cod,
     &               sid,cof,sif,pxcms,pycms,pzcms)
                  pecms = PHKk(4,i)
                  CALL DT_DALTRA(bge,bgx,bgy,bgz,pxcms,pycms,pzcms,
     &               pecms,ptot,PHKk(1,i),PHKk(2,i),PHKk(3,i),PHKk(4,i))
               END IF
            END DO
         ELSE
            Mode = -1
         END IF
         GOTO 99999
      END IF
 
 
      e1 = What(1)
      IF ( e1.LT.ZERO ) e1 = DBLE(IPZ)/DBLE(IP)*ABS(What(1))
      e2 = What(2)
      IF ( e2.LT.ZERO ) e2 = DBLE(ITZ)/DBLE(IT)*ABS(What(2))
      pp1 = SQRT((e1+AAM(IJProj))*(e1-AAM(IJProj)))
      pp2 = SQRT((e2+AAM(IJTarg))*(e2-AAM(IJTarg)))
      th = 1.D-6*What(3)/2.D0
      ph = What(4)*BOG
      P1(1) = pp1*SIN(th)*COS(ph)
      P1(2) = pp1*SIN(th)*SIN(ph)
      P1(3) = pp1*COS(th)
      P1(4) = e1
      P2(1) = pp2*SIN(th)*COS(ph)
      P2(2) = pp2*SIN(th)*SIN(ph)
      P2(3) = -pp2*COS(th)
      P2(4) = e2
      ecm = SQRT((P1(4)+P2(4))**2-(P1(1)+P2(1))**2-(P1(2)+P2(2))
     &      **2-(P1(3)+P2(3))**2)
      elab = (ecm**2-AAM(IJProj)**2-AAM(IJTarg)**2)/(2.0D0*AAM(IJTarg))
      Plab = SQRT((elab+AAM(IJProj))*(elab-AAM(IJProj)))
      bgx = (P1(1)+P2(1))/ecm
      bgy = (P1(2)+P2(2))/ecm
      bgz = (P1(3)+P2(3))/ecm
      bge = (P1(4)+P2(4))/ecm
      CALL DT_DALTRA(bge,-bgx,-bgy,-bgz,P1(1),P1(2),P1(3),P1(4),p1tot,
     &               p1cms(1),p1cms(2),p1cms(3),p1cms(4))
      CALL DT_DALTRA(bge,-bgx,-bgy,-bgz,P2(1),P2(2),P2(3),P2(4),p2tot,
     &               p2cms(1),p2cms(2),p2cms(3),p2cms(4))
      cod = p1cms(3)/p1tot
C     SID = SQRT((ONE-COD)*(ONE+COD))
      ppt = SQRT(p1cms(1)**2+p1cms(2)**2)
      sid = ppt/p1tot
      cof = ONE
      sif = ZERO
      IF ( p1tot*sid.GT.TINY10 ) THEN
         cof = p1cms(1)/(sid*p1tot)
         sif = p1cms(2)/(sid*p1tot)
         anorf = SQRT(cof*cof+sif*sif)
         cof = cof/anorf
         sif = sif/anorf
      END IF
C*check
C     WRITE(LOUT,'(4E15.4)') P1(1),P1(2),P1(3),P1(4)
C     WRITE(LOUT,'(4E15.4)') P2(1),P2(2),P2(3),P2(4)
C     WRITE(LOUT,'(5E15.4)') P1CMS(1),P1CMS(2),P1CMS(3),P1CMS(4),P1TOT
C     WRITE(LOUT,'(5E15.4)') P2CMS(1),P2CMS(2),P2CMS(3),P2CMS(4),P2TOT
C     PAX = ZERO
C     PAY = ZERO
C     PAZ = P1TOT
C     PAE = SQRT(AAM(IJPROJ)**2+PAZ**2)
C     PBX = ZERO
C     PBY = ZERO
C     PBZ = -P2TOT
C     PBE = SQRT(AAM(IJTARG)**2+PBZ**2)
C     WRITE(LOUT,'(4E15.4)') PAX,PAY,PAZ,PAE
C     WRITE(LOUT,'(4E15.4)') PBX,PBY,PBZ,PBE
C     CALL DT_MYTRAN(1,PAX,PAY,PAZ,COD,SID,COF,SIF,
C    &            P1CMS(1),P1CMS(2),P1CMS(3))
C     CALL DT_MYTRAN(1,PBX,PBY,PBZ,COD,SID,COF,SIF,
C    &            P2CMS(1),P2CMS(2),P2CMS(3))
C     WRITE(LOUT,'(4E15.4)') P1CMS(1),P1CMS(2),P1CMS(3),P1CMS(4)
C     WRITE(LOUT,'(4E15.4)') P2CMS(1),P2CMS(2),P2CMS(3),P2CMS(4)
C     CALL DT_DALTRA(BGE,BGX,BGY,BGZ,P1CMS(1),P1CMS(2),P1CMS(3),P1CMS(4),
C    &            P1TOT,P1(1),P1(2),P1(3),P1(4))
C     CALL DT_DALTRA(BGE,BGX,BGY,BGZ,P2CMS(1),P2CMS(2),P2CMS(3),P2CMS(4),
C    &            P2TOT,P2(1),P2(2),P2(3),P2(4))
C     WRITE(LOUT,'(4E15.4)') P1(1),P1(2),P1(3),P1(4)
C     WRITE(LOUT,'(4E15.4)') P2(1),P2(2),P2(3),P2(4)
C     STOP
C*
 
      lbeam = .TRUE.
 
 
99999 END SUBROUTINE
