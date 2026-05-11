
      SUBROUTINE PHO_GLU2QU(Ig,Iq1,Iq2,Irej)
C********************************************************************
C
C    split gluon with index I in POEVT1
C          (massless gluon assumed)
C
C    input:      /POEVT1/
C                IG      gluon index
C                IQ1     first quark index
C                IQ2     second quark index
C
C    output:     new quarks in /POEVT1/
C                IREJ    1 splitting impossible
C                        0 splitting successful
C
C********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION cmass1 , cmass2 , cutm , DEPS , EPS , p1 , p2 , 
     &                 PHO_GLUSPL , zfrac , zmin , zmin1 , zmin2
      INTEGER i , ic1 , ic2 , Ig , ipos , Iq1 , Iq2 , Irej , k
      SAVE 
 
      PARAMETER (DEPS=1.D-15,EPS=1.D-5)
 
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
 
C  internal rejection counters
      INCLUDE 'inc/poloop'
 
      DIMENSION p1(4) , p2(4)
      DATA cutm/0.02D0/
 
      Irej = 0
 
C  calculate string masses max possible
      IF ( ISWmdl(9).EQ.1 ) THEN
         cmass1 = 2.D0*(PHEp(4,Ig)*PHEp(4,Iq1)-PHEp(1,Ig)*PHEp(1,Iq1)
     &            -PHEp(2,Ig)*PHEp(2,Iq1)-PHEp(3,Ig)*PHEp(3,Iq1))
         IF ( cmass1.LT.cutm ) THEN
            IF ( IDEb(73).GE.5 ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,3I4,4E10.3)')
     &               'PHO_GLU2QU:REJECTION:IG,IQ1,IQ2,CMASS1' , Ig , 
     &              Iq1 , Iq2 , cmass1
            END IF
            IFAil(33) = IFAil(33) + 1
            Irej = 1
            RETURN
         END IF
         cmass2 = 2.D0*(PHEp(4,Ig)*PHEp(4,Iq2)-PHEp(1,Ig)*PHEp(1,Iq2)
     &            -PHEp(2,Ig)*PHEp(2,Iq2)-PHEp(3,Ig)*PHEp(3,Iq2))
         IF ( cmass2.LT.cutm ) THEN
            IF ( IDEb(73).GE.5 ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,3I4,4E10.3)')
     &               'PHO_GLU2QU:REJECTION:IG,IQ1,IQ2,CMASS2' , Ig , 
     &              Iq1 , Iq2 , cmass2
            END IF
            IFAil(33) = IFAil(33) + 1
            Irej = 1
            RETURN
         END IF
C
C  calculate minimal z
         zmin1 = (cutm-SIGN(PHEp(5,Iq1)**2,PHEp(5,Iq1)))/cmass1 + EPS
         zmin2 = (cutm-SIGN(PHEp(5,Iq2)**2,PHEp(5,Iq2)))/cmass2 + EPS
         zmin = MIN(zmin1,zmin2)
         IF ( MAX(zmin1,zmin2).GE.0.45D0 ) THEN
            IF ( IDEb(73).GE.5 ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,3I3,4E10.3)') 
     &         'PHO_GLU2QU:REJECTION:IG,IQ1,IQ2,ZMIN1,ZMIN2,P1*PG,P2*PG'
     &         , Ig , Iq1 , Iq2 , zmin1 , zmin2 , cmass1 , cmass2
            END IF
            IFAil(33) = IFAil(33) + 1
            Irej = 1
            RETURN
         END IF
      ELSE
         zmin = MIN(0.1D0,0.5D0/PHEp(4,Ig))
      END IF
C
      zfrac = PHO_GLUSPL(zmin)
      IF ( (zfrac.LT.zmin1) .OR. ((1.D0-zfrac).LT.zmin2) )
     &     zfrac = 1.D0 - zfrac
      DO i = 1 , 4
         p1(i) = PHEp(i,Ig)*zfrac
         p2(i) = PHEp(i,Ig)*(1.D0-zfrac)
      END DO
C  quark flavours
      cmass1 = SQRT(zfrac*cmass1+SIGN(PHEp(5,Iq1)**2,PHEp(5,Iq1)))
      cmass2 = SQRT((1.D0-zfrac)*cmass2+SIGN(PHEp(5,Iq2)**2,PHEp(5,Iq2))
     &         )
      CALL PHO_SEAFLA(Ig,k,i,MIN(cmass1,cmass2))
 
      IF ( ABS(IDHep(Iq1)).GT.6 ) THEN
         k = SIGN(ABS(k),IDHep(Iq1))
      ELSE
         k = -SIGN(ABS(k),IDHep(Iq1))
      END IF
C  colors
      IF ( k.GT.0 ) THEN
         ic1 = MAX(ICOlor(1,Ig),ICOlor(2,Ig))
         ic2 = MIN(ICOlor(1,Ig),ICOlor(2,Ig))
      ELSE
         ic1 = MIN(ICOlor(1,Ig),ICOlor(2,Ig))
         ic2 = MAX(ICOlor(1,Ig),ICOlor(2,Ig))
      END IF
C  register new partons
      CALL PHO_REGPAR(-1,k,0,Ig,0,p1(1),p1(2),p1(3),p1(4),IPHist(1,Ig),
     &                0,ic1,0,ipos,1)
      CALL PHO_REGPAR(-1,-k,0,Ig,0,p2(1),p2(2),p2(3),p2(4),IPHist(1,Ig),
     &                0,ic2,0,ipos,1)
C  debug output
      IF ( IDEb(73).GE.20 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,/1X,A,3I3,5E10.3)')
     &         'PHO_GLU2QU:' , '   IG,IQ1,IQ2,ZMIN1,2,Z,P1*PG,P2*PG' , 
     &        Ig , Iq1 , Iq2 , zmin1 , zmin2 , zfrac , cmass1 , cmass2
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,4I5)')
     &         '   flavours, colors  ' , k , -k , ic1 , ic2
      END IF
      END SUBROUTINE
