
      SUBROUTINE MUSQBS2(Nc1,Nc1p,Nc1t,Nc2,Nc2p,Nc2t,Irej,Ip1,Ip21,Ip22,
     &                   Ipp1,Ipp2,Ipip,Isq,Igcoun)
C
C                  USQBS-2 diagram (split target diquark)
C
      IMPLICIT NONE
      DOUBLE PRECISION chamal , DT_RNDM , DT_SAMPEX , pg1 , pg2 , pg3 , 
     &                 pg4 , unoprv , v , xaqmax , xdiqt , xmist , 
     &                 xqmax , xsaq , xsaq1 , xsq , xsq1 , xvqp , 
     &                 xvthr , xvthro
      DOUBLE PRECISION xvtqi , xvtqii
      INTEGER icou , Igcoun , iig , iiglu1 , iiglu2 , Ip1 , Ip21 , 
     &        Ip22 , Ipip , Ipp1 , Ipp2 , Irej , isaq , isaq1 , Isq , 
     &        isq1 , ivthr , jjg , kkg , Nc1
      INTEGER Nc1p , Nc1t , Nc2 , Nc2p , Nc2t
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
C event history
 
 
      INCLUDE 'inc/dtevt1'
C extended event history
      INCLUDE 'inc/dtevt2'
C Lorentz-parameters of the current interaction
      INCLUDE 'inc/dtltra'
C diquark-breaking mechanism
      INCLUDE 'inc/dtdiqb'
 
C
      INCLUDE 'inc/hkktmp'
CKEEP,XSEADI.
      INCLUDE 'inc/xseadi'
CKEEP,DPRIN.
      INCLUDE 'inc/dprin'
      INCLUDE 'inc/evflag'
C
C                  USQBS-2 diagram (split target diquark)
C
C
C     Input chain 1(NC1) valence-quark(NC1P)-valence-diquark(NC1T)
C     Input chain 2(NC2) sea-antiquark(NC2P)-sea-quark(NC2T)
C
C     Create antiquark(aqsP)-quark(qsT) pair, energy from NC1P and NC1T
C     Split remaining valence diquark(NC1T) into quarks vq1T and vq2T
C
C     Create chains 3 sea antiquark(NC2P 1)-valence-quark(vq1T 2)
C                   6 sea-antiquark(aqsP 4)-valence-quark(vq2T 5)
C                   9 valence-quark(NC1P 7)-diquark(NC2T+qsT 8)
C
C
C       Put new chains into COMMON /HKKTMP/
C
      iiglu1 = Nc1t - Nc1p - 1
      iiglu2 = Nc2t - Nc2p - 1
      Igcoun = 0
C     WRITE(LOUT,*)'MUSQBS2: IIGLU1,IIGLU2 ',IIGLU1,IIGLU2
      CVQ = 1.D0
      Irej = 0
      IF ( Ipip.EQ.2 ) THEN
C     IF(NUMEV.EQ.-324)THEN
C     WRITE(LOUT,*)' MUSQBS2(NC1,NC1P,NC1T,NC2,NC2P,NC2T,IREJ,',
C    *             'IP1,IP21,IP22,IPP1,IPP2,IPIP,IGCOUN)',
C    *NC1,NC1P,NC1T,NC2,NC2P,NC2T,IREJ,
C    *              IP1,IP21,IP22,IPP1,IPP2,IPIP,IGCOUN
      END IF
C
C
C
C     determine x-values of NC1T diquark
      xdiqt = PHKk(4,Nc1t)*2.D0/UMO
      xvqp = PHKk(4,Nc1p)*2.D0/UMO
C
C     determine x-values of sea quark pair
C
      IPCo = 1
      icou = 0
 100  icou = icou + 1
      IF ( icou.GE.500 ) THEN
         Irej = 1
         IF ( Isq.EQ.3 ) Irej = 3
         IF ( IPCo.GE.3 ) WRITE (LOUt,*)
     &         ' MUSQBS2 Rejection 2234 ICOU. GT.500'
         IPCo = 0
         RETURN
      END IF
      IF ( IPCo.GE.3 ) WRITE (LOUt,*)
     &                         'MUSQBS2 call  XSEAPA: UMO,XDIQT,XVQP ' , 
     &                        UMO , xdiqt , xvqp
      xsq = 0.D0
      xsaq = 0.D0
C*NEW
C     CALL XSEAPA(UMO,XDIQT/2.D0,ISQ,ISAQ,XSQ,XSAQ,IREJ)
      IF ( Ipip.EQ.1 ) THEN
         xqmax = xdiqt/2.0D0
         xaqmax = 2.D0*xvqp/3.0D0
      ELSE
         xqmax = 2.D0*xvqp/3.0D0
         xaqmax = xdiqt/2.0D0
      END IF
      CALL DT_CQPAIR(xqmax,xaqmax,xsq,xsaq,Isq,Irej)
      isaq = 6 + Isq
C     write(*,*) 'MUSQBS2: ',ISQ,XSQ,XDIQT,XSAQ,XVQP
C*
 
      IF ( IPCo.GE.3 .AND. LPRi.GT.4 ) WRITE (LOUt,*)
     &      'MUSQBS2 after XSEAPA' , Isq , isaq , xsq , xsaq
      IF ( Irej.GE.1 ) THEN
 
         IF ( IPCo.GE.3 .AND. LPRi.GT.4 ) WRITE (LOUt,*)
     &         'MUSQBS2 reject XSEAPA' , Isq , isaq , xsq , xsaq
         IPCo = 0
         RETURN
      END IF
      IF ( Ipip.EQ.1 ) THEN
         IF ( xsaq.GE.2.D0*xvqp/3.D0 ) GOTO 100
      ELSE IF ( Ipip.EQ.2 ) THEN
         IF ( xsq.GE.2.D0*xvqp/3.D0 ) GOTO 100
      END IF
      IF ( IPCo.GE.3 ) THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,'(A,4E12.4)')
     &         ' MUSQBS2 XDIQT,XVQP,XSQ,XSAQ ' , xdiqt , xvqp , xsq , 
     &        xsaq
      END IF
C
C     subtract xsq,xsaq from NC1T diquark and NC1P quark
C
C     XSQ=0.D0
      IF ( Ipip.EQ.1 ) THEN
         xdiqt = xdiqt - xsq
         xvqp = xvqp - xsaq
      ELSE IF ( Ipip.EQ.2 ) THEN
         xdiqt = xdiqt - xsaq
         xvqp = xvqp - xsq
      END IF
 
      IF ( IPCo.GE.3 .AND. LPRi.GT.4 ) WRITE (LOUt,*)
     &      'XDIQT,XVQP after subtraction' , xdiqt , xvqp
C
C     Split remaining valence diquark(NC1T) into quarks vq1T and vq2T
C
      xvthro = CVQ/UMO
      ivthr = 0
 200  IF ( ivthr.EQ.10 ) THEN
         Irej = 1
         IF ( Isq.EQ.3 ) Irej = 3
         IF ( IPCo.GE.3 ) WRITE (LOUt,*) ' MUSQBS2 3466 reject IVTHR 10'
         IPCo = 0
         RETURN
      END IF
      ivthr = ivthr + 1
      xvthr = xvthro/(201-ivthr)
      unoprv = UNOn
C380  CONTINUE
      IF ( xvthr.GT.0.66D0*xdiqt ) THEN
         Irej = 1
         IF ( Isq.EQ.3 ) Irej = 3
         IF ( IPCo.GE.3 ) WRITE (LOUt,*)
     &         ' MUSQBS2 Rejection 380 XVTHR  large ' , xvthr
         IPCo = 0
         RETURN
      END IF
      IF ( DT_RNDM(v).LT.0.5D0 ) THEN
         xvtqi = DT_SAMPEX(xvthr,0.66D0*xdiqt)
         xvtqii = xdiqt - xvtqi
      ELSE
         xvtqii = DT_SAMPEX(xvthr,0.66D0*xdiqt)
         xvtqi = xdiqt - xvtqii
      END IF
      IF ( IPCo.GE.3 ) THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,'(A,2E12.4)')
     &         '  MUSQBS2:XVTQI,XVTQII ' , xvtqi , xvtqii
      END IF
C
C     Prepare 4 momenta of new chains and chain ends
C
C     COMMON /HKKTMP/NHKKT,NEVHKT,ISTHKT(NTMHKK),IDHKT(NTMHKK),JMOHKT
C    +(2,NTMHKK),JDAHKT(2,NTMHKK), PHKT(5,NTMHKK),VHKT(4,NTMHKK),WHKT
C    +(4,NTMHKK)
C
C     Create chains 3 sea antiquark(NC2P 1)-valence-quark(vq1T 2)
C                   6 sea-antiquark(aqsP 4)-valence-quark(vq2T 5)
C                   9 valence-quark(NC1P 7)-diquark(NC2T+qsT 8)
C
C     SUBROUTINE MUSQBS2(NC1,NC1P,NC1T,NC2,NC2P,NC2T,IREJ,
C    *              IP1,IP21,IP22,IPP1,IPP2)
C
      IF ( Ipip.EQ.1 ) THEN
         xsq1 = xsq
         xsaq1 = xsaq
         isq1 = Isq
         isaq1 = isaq
      ELSE IF ( Ipip.EQ.2 ) THEN
         xsq1 = xsaq
         xsaq1 = xsq
         isq1 = isaq
         isaq1 = Isq
      END IF
      IDHkt(1) = Ipp1
      ISThkt(1) = 951
      JMOhkt(1,1) = Nc2p
      JMOhkt(2,1) = 0
      JDAhkt(1,1) = 3 + iiglu1
      JDAhkt(2,1) = 0
C     Create chains 3 sea antiquark(NC2P 1)-valence-quark(vq1T 2)
      PHKt(1,1) = PHKk(1,Nc2p)
      PHKt(2,1) = PHKk(2,Nc2p)
      PHKt(3,1) = PHKk(3,Nc2p)
      PHKt(4,1) = PHKk(4,Nc2p)
C     PHKT(5,1)  =PHKK(5,NC2P)
      xmist = (PHKt(4,1)**2-PHKt(3,1)**2-PHKt(2,1)**2-PHKt(1,1)**2)
      IF ( xmist.GT.0.D0 ) THEN
         PHKt(5,1) = SQRT(PHKt(4,1)**2-PHKt(3,1)**2-PHKt(2,1)
     &               **2-PHKt(1,1)**2)
      ELSE
C     WRITE(LOUT,*)'MUSQBS2 parton 1 mass square LT.0 ',XMIST
         PHKt(5,1) = 0.D0
      END IF
      VHKt(1,1) = VHKk(1,Nc2p)
      VHKt(2,1) = VHKk(2,Nc2p)
      VHKt(3,1) = VHKk(3,Nc2p)
      VHKt(4,1) = VHKk(4,Nc2p)
      WHKt(1,1) = WHKk(1,Nc2p)
      WHKt(2,1) = WHKk(2,Nc2p)
      WHKt(3,1) = WHKk(3,Nc2p)
      WHKt(4,1) = WHKk(4,Nc2p)
C     Add here IIGLU1 gluons to this chaina
      pg1 = 0.D0
      pg2 = 0.D0
      pg3 = 0.D0
      pg4 = 0.D0
      IF ( iiglu1.GE.1 ) THEN
         jjg = Nc1p
         DO iig = 2 , 2 + iiglu1 - 1
            kkg = jjg + iig - 1
            IDHkt(iig) = IDHkk(kkg)
            ISThkt(iig) = 921
            JMOhkt(1,iig) = kkg
            JMOhkt(2,iig) = 0
            JDAhkt(1,iig) = 3 + iiglu1
            JDAhkt(2,iig) = 0
            PHKt(1,iig) = PHKk(1,kkg)
            pg1 = pg1 + PHKt(1,iig)
            PHKt(2,iig) = PHKk(2,kkg)
            pg2 = pg2 + PHKt(2,iig)
            PHKt(3,iig) = PHKk(3,kkg)
            pg3 = pg3 + PHKt(3,iig)
            PHKt(4,iig) = PHKk(4,kkg)
            pg4 = pg4 + PHKt(4,iig)
            PHKt(5,iig) = PHKk(5,kkg)
            VHKt(1,iig) = VHKk(1,kkg)
            VHKt(2,iig) = VHKk(2,kkg)
            VHKt(3,iig) = VHKk(3,kkg)
            VHKt(4,iig) = VHKk(4,kkg)
            WHKt(1,iig) = WHKk(1,kkg)
            WHKt(2,iig) = WHKk(2,kkg)
            WHKt(3,iig) = WHKk(3,kkg)
            WHKt(4,iig) = WHKk(4,kkg)
         END DO
      END IF
      IDHkt(2+iiglu1) = Ip21
      ISThkt(2+iiglu1) = 952
      JMOhkt(1,2+iiglu1) = Nc1t
      JMOhkt(2,2+iiglu1) = 0
      JDAhkt(1,2+iiglu1) = 3 + iiglu1
      JDAhkt(2,2+iiglu1) = 0
      PHKt(1,2+iiglu1) = PHKk(1,Nc1t)*xvtqi/(xdiqt+xsq1)
      PHKt(2,2+iiglu1) = PHKk(2,Nc1t)*xvtqi/(xdiqt+xsq1)
      PHKt(3,2+iiglu1) = PHKk(3,Nc1t)*xvtqi/(xdiqt+xsq1)
      PHKt(4,2+iiglu1) = PHKk(4,Nc1t)*xvtqi/(xdiqt+xsq1)
C     PHKT(5,2)  =PHKK(5,NC1T)
      xmist = (PHKt(4,2+iiglu1)**2-PHKt(3,2+iiglu1)**2-PHKt(2,2+iiglu1)
     &        **2-PHKt(1,2+iiglu1)**2)
      IF ( xmist.GT.0.D0 ) THEN
         PHKt(5,2+iiglu1) = SQRT(PHKt(4,2+iiglu1)**2-PHKt(3,2+iiglu1)
     &                      **2-PHKt(2,2+iiglu1)**2-PHKt(1,2+iiglu1)**2)
      ELSE
C      WRITE(LOUT,*)' parton 4 mass square LT.0 ',XMIST
         PHKt(5,5+iiglu1) = 0.D0
      END IF
      VHKt(1,2+iiglu1) = VHKk(1,Nc1t)
      VHKt(2,2+iiglu1) = VHKk(2,Nc1t)
      VHKt(3,2+iiglu1) = VHKk(3,Nc1t)
      VHKt(4,2+iiglu1) = VHKk(4,Nc1t)
      WHKt(1,2+iiglu1) = WHKk(1,Nc1t)
      WHKt(2,2+iiglu1) = WHKk(2,Nc1t)
      WHKt(3,2+iiglu1) = WHKk(3,Nc1t)
      WHKt(4,2+iiglu1) = WHKk(4,Nc1t)
      IDHkt(3+iiglu1) = 88888
      ISThkt(3+iiglu1) = 95
      JMOhkt(1,3+iiglu1) = 1
      JMOhkt(2,3+iiglu1) = 2 + iiglu1
      JDAhkt(1,3+iiglu1) = 0
      JDAhkt(2,3+iiglu1) = 0
      PHKt(1,3+iiglu1) = PHKt(1,1) + PHKt(1,2+iiglu1) + pg1
      PHKt(2,3+iiglu1) = PHKt(2,1) + PHKt(2,2+iiglu1) + pg2
      PHKt(3,3+iiglu1) = PHKt(3,1) + PHKt(3,2+iiglu1) + pg3
      PHKt(4,3+iiglu1) = PHKt(4,1) + PHKt(4,2+iiglu1) + pg4
      xmist = (PHKt(4,3+iiglu1)**2-PHKt(1,3+iiglu1)**2-PHKt(2,3+iiglu1)
     &        **2-PHKt(3,3+iiglu1)**2)
      IF ( xmist.GT.0.D0 ) THEN
         PHKt(5,3+iiglu1) = SQRT(PHKt(4,3+iiglu1)**2-PHKt(1,3+iiglu1)
     &                      **2-PHKt(2,3+iiglu1)**2-PHKt(3,3+iiglu1)**2)
      ELSE
C      WRITE(LOUT,*)' parton 4 mass square LT.0 ',XMIST
         PHKt(5,5+iiglu1) = 0.D0
      END IF
      IF ( Ipip.GE.2 ) THEN
C     IF(NUMEV.EQ.-324)THEN
C     WRITE(LOUT,*)1,ISTHKT(1),IDHKT(1),JMOHKT(1,1),JMOHKT(2,1),
C    * JDAHKT(1,1),
C    *JDAHKT(2,1),(PHKT(III,1),III=1,5)
         DO iig = 2 , 2 + iiglu1 - 1
C     WRITE(LOUT,*)IIG,ISTHKT(IIG),IDHKT(IIG),
C    &             JMOHKT(1,IIG),JMOHKT(2,IIG),
C    * JDAHKT(1,IIG),
C    *JDAHKT(2,IIG),(PHKT(III,IIG),III=1,5)
         END DO
C     WRITE(LOUT,*)2+IIGLU1,ISTHKT(2+IIGLU1),IDHKT(2+IIGLU1),
C    * JMOHKT(1,2+IIGLU1),JMOHKT(2,2+IIGLU1),JDAHKT(1,2+IIGLU1),
C    *JDAHKT(2,2+IIGLU1),(PHKT(III,2+IIGLU1),III=1,5)
C     WRITE(LOUT,*)3+IIGLU1,ISTHKT(3+IIGLU1),IDHKT(3+IIGLU1),
C    * JMOHKT(1,3+IIGLU1),JMOHKT(2,3+IIGLU1),JDAHKT(1,3+IIGLU1),
C    *JDAHKT(2,3+IIGLU1),(PHKT(III,3+IIGLU1),III=1,5)
      END IF
      chamal = CHAm1
      IF ( Ipip.EQ.1 ) THEN
         IF ( Ipp1.LE.-3 .OR. Ip21.GE.3 ) chamal = CHAm3
      ELSE IF ( Ipip.EQ.2 ) THEN
         IF ( Ipp1.GE.3 .OR. Ip21.LE.-3 ) chamal = CHAm3
      END IF
      IF ( PHKt(5,3+iiglu1).LT.chamal ) THEN
C       IREJ=1
         IPCo = 0
C       RETURN
C       WRITE(LOUT,*)' MUSQBS1 jump back from chain 3'
         GOTO 200
      END IF
      VHKt(1,3+iiglu1) = VHKk(1,Nc1)
      VHKt(2,3+iiglu1) = VHKk(2,Nc1)
      VHKt(3,3+iiglu1) = VHKk(3,Nc1)
      VHKt(4,3+iiglu1) = VHKk(4,Nc1)
      WHKt(1,3+iiglu1) = WHKk(1,Nc1)
      WHKt(2,3+iiglu1) = WHKk(2,Nc1)
      WHKt(3,3+iiglu1) = WHKk(3,Nc1)
      WHKt(4,3+iiglu1) = WHKk(4,Nc1)
      IF ( Ipip.EQ.1 ) THEN
         IDHkt(4+iiglu1) = -(isaq1-6)
      ELSE IF ( Ipip.EQ.2 ) THEN
         IDHkt(4+iiglu1) = isaq1
      END IF
      ISThkt(4+iiglu1) = 951
      JMOhkt(1,4+iiglu1) = Nc1p
      JMOhkt(2,4+iiglu1) = 0
      JDAhkt(1,4+iiglu1) = 6 + iiglu1
      JDAhkt(2,4+iiglu1) = 0
C     create chain    6 sea-antiquark(aqsP 4)-valence-quark(vq2T 5)
      PHKt(1,4+iiglu1) = PHKk(1,Nc1p)*xsaq1/(xvqp+xsaq1)
      PHKt(2,4+iiglu1) = PHKk(2,Nc1p)*xsaq1/(xvqp+xsaq1)
      PHKt(3,4+iiglu1) = PHKk(3,Nc1p)*xsaq1/(xvqp+xsaq1)
      PHKt(4,4+iiglu1) = PHKk(4,Nc1p)*xsaq1/(xvqp+xsaq1)
C     PHKT(5,4+IIGLU1)  =PHKK(5,NC1P)
      xmist = (PHKt(4,4+iiglu1)**2-PHKt(3,4+iiglu1)**2-PHKt(2,4+iiglu1)
     &        **2-PHKt(1,4+iiglu1)**2)
      IF ( xmist.GT.0.D0 ) THEN
         PHKt(5,4+iiglu1) = SQRT(PHKt(4,4+iiglu1)**2-PHKt(3,4+iiglu1)
     &                      **2-PHKt(2,4+iiglu1)**2-PHKt(1,4+iiglu1)**2)
      ELSE
C     WRITE(LOUT,*)'MUSQBS2 parton 4 mass square LT.0 ',XMIST
         PHKt(5,4+iiglu1) = 0.D0
      END IF
      VHKt(1,4+iiglu1) = VHKk(1,Nc1p)
      VHKt(2,4+iiglu1) = VHKk(2,Nc1p)
      VHKt(3,4+iiglu1) = VHKk(3,Nc1p)
      VHKt(4,4+iiglu1) = VHKk(4,Nc1p)
      WHKt(1,4+iiglu1) = WHKk(1,Nc1p)
      WHKt(2,4+iiglu1) = WHKk(2,Nc1p)
      WHKt(3,4+iiglu1) = WHKk(3,Nc1p)
      WHKt(4,4+iiglu1) = WHKk(4,Nc1p)
      IDHkt(5+iiglu1) = Ip22
      ISThkt(5+iiglu1) = 952
      JMOhkt(1,5+iiglu1) = Nc1t
      JMOhkt(2,5+iiglu1) = 0
      JDAhkt(1,5+iiglu1) = 6 + iiglu1
      JDAhkt(2,5+iiglu1) = 0
      PHKt(1,5+iiglu1) = PHKk(1,Nc1t)*xvtqii/(xdiqt+xsq1)
      PHKt(2,5+iiglu1) = PHKk(2,Nc1t)*xvtqii/(xdiqt+xsq1)
      PHKt(3,5+iiglu1) = PHKk(3,Nc1t)*xvtqii/(xdiqt+xsq1)
      PHKt(4,5+iiglu1) = PHKk(4,Nc1t)*xvtqii/(xdiqt+xsq1)
C     PHKT(5,5+IIGLU1)  =PHKK(5,NC1T)
      xmist = (PHKt(4,5+iiglu1)**2-PHKt(3,5+iiglu1)**2-PHKt(2,5+iiglu1)
     &        **2-PHKt(1,5+iiglu1)**2)
      IF ( xmist.GT.0.D0 ) THEN
         PHKt(5,5+iiglu1) = SQRT(PHKt(4,5+iiglu1)**2-PHKt(3,5+iiglu1)
     &                      **2-PHKt(2,5+iiglu1)**2-PHKt(1,5+iiglu1)**2)
      ELSE
C      WRITE(6,*)' parton 4 mass square LT.0 ',XMIST
         PHKt(5,5+iiglu1) = 0.D0
      END IF
      VHKt(1,5+iiglu1) = VHKk(1,Nc1t)
      VHKt(2,5+iiglu1) = VHKk(2,Nc1t)
      VHKt(3,5+iiglu1) = VHKk(3,Nc1t)
      VHKt(4,5+iiglu1) = VHKk(4,Nc1t)
      WHKt(1,5+iiglu1) = WHKk(1,Nc1t)
      WHKt(2,5+iiglu1) = WHKk(2,Nc1t)
      WHKt(3,5+iiglu1) = WHKk(3,Nc1t)
      WHKt(4,5+iiglu1) = WHKk(4,Nc1t)
      IDHkt(6+iiglu1) = 88888
      ISThkt(6+iiglu1) = 95
      JMOhkt(1,6+iiglu1) = 4 + iiglu1
      JMOhkt(2,6+iiglu1) = 5 + iiglu1
      JDAhkt(1,6+iiglu1) = 0
      JDAhkt(2,6+iiglu1) = 0
      PHKt(1,6+iiglu1) = PHKt(1,4+iiglu1) + PHKt(1,5+iiglu1)
      PHKt(2,6+iiglu1) = PHKt(2,4+iiglu1) + PHKt(2,5+iiglu1)
      PHKt(3,6+iiglu1) = PHKt(3,4+iiglu1) + PHKt(3,5+iiglu1)
      PHKt(4,6+iiglu1) = PHKt(4,4+iiglu1) + PHKt(4,5+iiglu1)
      xmist = (PHKt(4,6+iiglu1)**2-PHKt(1,6+iiglu1)**2-PHKt(2,6+iiglu1)
     &        **2-PHKt(3,6+iiglu1)**2)
      IF ( xmist.GT.0.D0 ) THEN
         PHKt(5,6+iiglu1) = SQRT(PHKt(4,6+iiglu1)**2-PHKt(1,6+iiglu1)
     &                      **2-PHKt(2,6+iiglu1)**2-PHKt(3,6+iiglu1)**2)
      ELSE
C      WRITE(6,*)' parton 4 mass square LT.0 ',XMIST
         PHKt(5,5+iiglu1) = 0.D0
      END IF
C     IF(IPIP.GE.2)THEN
C     IF(NUMEV.EQ.-324)THEN
C     WRITE(6,*)4+IIGLU1,ISTHKT(4+IIGLU1),IDHKT(4+IIGLU1),
C    * JMOHKT(1,4+IIGLU1),JMOHKT(2,4+IIGLU1),JDAHKT(1,4+IIGLU1),
C    *JDAHKT(2,4+IIGLU1),(PHKT(III,4+IIGLU1),III=1,5)
C     WRITE(6,*)5+IIGLU1,ISTHKT(5+IIGLU1),IDHKT(5+IIGLU1),
C    * JMOHKT(1,5+IIGLU1),JMOHKT(2,5+IIGLU1),JDAHKT(1,5+IIGLU1),
C    *JDAHKT(2,5+IIGLU1),(PHKT(III,5+IIGLU1),III=1,5)
C     WRITE(6,*)6+IIGLU1,ISTHKT(6+IIGLU1),IDHKT(6+IIGLU1),
C    * JMOHKT(1,6+IIGLU1),JMOHKT(2,6+IIGLU1),JDAHKT(1,6+IIGLU1),
C    *JDAHKT(2,6+IIGLU1),(PHKT(III,6+IIGLU1),III=1,5)
C     ENDIF
      chamal = CHAm1
      IF ( Ipip.EQ.1 ) THEN
         IF ( Ip22.GE.3 .OR. isaq1.GE.9 ) chamal = CHAm3
      ELSE IF ( Ipip.EQ.2 ) THEN
         IF ( Ip22.LE.-3 .OR. isaq1.GE.3 ) chamal = CHAm3
      END IF
      IF ( PHKt(5,6+iiglu1).LT.chamal ) THEN
C       IREJ=1
         IPCo = 0
C       RETURN
C       WRITE(6,*)' MUSQBS1 jump back from chain 6',
C    *  CHAMAL,PHKT(5,6+IIGLU1)
         GOTO 200
      END IF
      VHKt(1,6+iiglu1) = VHKk(1,Nc1)
      VHKt(2,6+iiglu1) = VHKk(2,Nc1)
      VHKt(3,6+iiglu1) = VHKk(3,Nc1)
      VHKt(4,6+iiglu1) = VHKk(4,Nc1)
      WHKt(1,6+iiglu1) = WHKk(1,Nc1)
      WHKt(2,6+iiglu1) = WHKk(2,Nc1)
      WHKt(3,6+iiglu1) = WHKk(3,Nc1)
      WHKt(4,6+iiglu1) = WHKk(4,Nc1)
C     IDHKT(7)   =1000*IPP1+100*ISQ+1
      IDHkt(7+iiglu1) = Ip1
      ISThkt(7+iiglu1) = 951
      JMOhkt(1,7+iiglu1) = Nc1p
      JMOhkt(2,7+iiglu1) = 0
C*NEW
C     JDAHKT(1,7+IIGLU1)=9+IIGLU1
      JDAhkt(1,7+iiglu1) = 9 + iiglu1 + iiglu2
C*
      JDAhkt(2,7+iiglu1) = 0
      PHKt(1,7+iiglu1) = PHKk(1,Nc1p)*xvqp/(xvqp+xsaq1)
      PHKt(2,7+iiglu1) = PHKk(2,Nc1p)*xvqp/(xvqp+xsaq1)
      PHKt(3,7+iiglu1) = PHKk(3,Nc1p)*xvqp/(xvqp+xsaq1)
      PHKt(4,7+iiglu1) = PHKk(4,Nc1p)*xvqp/(xvqp+xsaq1)
C     PHKT(5,7+IIGLU1)  =PHKK(5,NC1P)
      xmist = (PHKt(4,7+iiglu1)**2-PHKt(3,7+iiglu1)**2-PHKt(2,7+iiglu1)
     &        **2-PHKt(1,7+iiglu1)**2)
      IF ( xmist.GT.0.D0 ) THEN
         PHKt(5,7+iiglu1) = SQRT(PHKt(4,7+iiglu1)**2-PHKt(3,7+iiglu1)
     &                      **2-PHKt(2,7+iiglu1)**2-PHKt(1,7+iiglu1)**2)
      ELSE
C     WRITE(6,*)'MUSQBS2 parton 7 mass square LT.0 ',XMIST
         PHKt(5,7+iiglu1) = 0.D0
      END IF
      VHKt(1,7+iiglu1) = VHKk(1,Nc1p)
      VHKt(2,7+iiglu1) = VHKk(2,Nc1p)
      VHKt(3,7+iiglu1) = VHKk(3,Nc1p)
      VHKt(4,7+iiglu1) = VHKk(4,Nc1p)
      WHKt(1,7+iiglu1) = WHKk(1,Nc1p)
      WHKt(2,7+iiglu1) = WHKk(2,Nc1p)
      WHKt(3,7+iiglu1) = WHKk(3,Nc1p)
      WHKt(4,7+iiglu1) = WHKk(4,Nc2p)
C     Insert here the IIGLU2 gluons
      pg1 = 0.D0
      pg2 = 0.D0
      pg3 = 0.D0
      pg4 = 0.D0
      IF ( iiglu2.GE.1 ) THEN
         jjg = Nc2p
         DO iig = 7 + iiglu1 + 1 , 7 + iiglu1 + iiglu2
            kkg = jjg + iig - 7 - iiglu1
            IDHkt(iig) = IDHkk(kkg)
            ISThkt(iig) = 921
            JMOhkt(1,iig) = kkg
            JMOhkt(2,iig) = 0
            JDAhkt(1,iig) = 9 + iiglu1 + iiglu2
            JDAhkt(2,iig) = 0
            PHKt(1,iig) = PHKk(1,kkg)
            pg1 = pg1 + PHKt(1,iig)
            PHKt(2,iig) = PHKk(2,kkg)
            pg2 = pg2 + PHKt(2,iig)
            PHKt(3,iig) = PHKk(3,kkg)
            pg3 = pg3 + PHKt(3,iig)
            PHKt(4,iig) = PHKk(4,kkg)
            pg4 = pg4 + PHKt(4,iig)
            PHKt(5,iig) = PHKk(5,kkg)
            VHKt(1,iig) = VHKk(1,kkg)
            VHKt(2,iig) = VHKk(2,kkg)
            VHKt(3,iig) = VHKk(3,kkg)
            VHKt(4,iig) = VHKk(4,kkg)
            WHKt(1,iig) = WHKk(1,kkg)
            WHKt(2,iig) = WHKk(2,kkg)
            WHKt(3,iig) = WHKk(3,kkg)
            WHKt(4,iig) = WHKk(4,kkg)
         END DO
      END IF
      IF ( Ipip.EQ.1 ) THEN
         IDHkt(8+iiglu1+iiglu2) = 1000*Ipp2 + 100*isq1 + 3
         IF ( IDHkt(8+iiglu1+iiglu2).EQ.1203 ) IDHkt(8+iiglu1+iiglu2)
     &        = 2103
         IF ( IDHkt(8+iiglu1+iiglu2).EQ.1303 ) IDHkt(8+iiglu1+iiglu2)
     &        = 3103
         IF ( IDHkt(8+iiglu1+iiglu2).EQ.2303 ) IDHkt(8+iiglu1+iiglu2)
     &        = 3203
      ELSE IF ( Ipip.EQ.2 ) THEN
         IDHkt(8+iiglu1+iiglu2) = 1000*Ipp2 + 100*(-isq1+6) - 3
         IF ( IDHkt(8+iiglu1+iiglu2).EQ.-1203 ) IDHkt(8+iiglu1+iiglu2)
     &        = -2103
         IF ( IDHkt(8+iiglu1+iiglu2).EQ.-1303 ) IDHkt(8+iiglu1+iiglu2)
     &        = -3103
         IF ( IDHkt(8+iiglu1+iiglu2).EQ.-2303 ) IDHkt(8+iiglu1+iiglu2)
     &        = -3203
      END IF
      ISThkt(8+iiglu1+iiglu2) = 952
      JMOhkt(1,8+iiglu1+iiglu2) = Nc2t
      JMOhkt(2,8+iiglu1+iiglu2) = 0
      JDAhkt(1,8+iiglu1+iiglu2) = 9 + iiglu1 + iiglu2
      JDAhkt(2,8+iiglu1+iiglu2) = 0
      PHKt(1,8+iiglu1+iiglu2) = PHKk(1,Nc2t) + PHKk(1,Nc1t)
     &   *xsq1/(xdiqt+xsq1)
      PHKt(2,8+iiglu1+iiglu2) = PHKk(2,Nc2t) + PHKk(2,Nc1t)
     &   *xsq1/(xdiqt+xsq1)
      PHKt(3,8+iiglu1+iiglu2) = PHKk(3,Nc2t) + PHKk(3,Nc1t)
     &   *xsq1/(xdiqt+xsq1)
      PHKt(4,8+iiglu1+iiglu2) = PHKk(4,Nc2t) + PHKk(4,Nc1t)
     &   *xsq1/(xdiqt+xsq1)
C     WRITE(6,*)'PHKK(4,NC1T),PHKK(4,NC2T), PHKT(4,7)',
C    * PHKK(4,NC1T),PHKK(4,NC2T), PHKT(4,7)
      IF ( PHKt(4,8+iiglu1+iiglu2).GE.PHKk(4,Nc1t) ) THEN
C       IREJ=1
C       WRITE(6,*)'reject PHKT(4,8+IIGLU1+IIGLU2).GE. PHKK(4,NC1T)'
C    *  ,PHKT(4,8+IIGLU1+IIGLU2), PHKK(4,NC2T),NC2T
         IPCo = 0
C       RETURN
         GOTO 200
      END IF
C     PHKT(5,8)  =PHKK(5,NC2T)
      xmist = (PHKt(4,8+iiglu1+iiglu2)**2-PHKt(3,8+iiglu1+iiglu2)
     &        **2-PHKt(2,8+iiglu1+iiglu2)**2-PHKt(1,8+iiglu1+iiglu2)**2)
      IF ( xmist.GT.0.D0 ) THEN
         PHKt(5,8+iiglu1+iiglu2)
     &      = SQRT(PHKt(4,8+iiglu1+iiglu2)**2-PHKt(3,8+iiglu1+iiglu2)
     &      **2-PHKt(2,8+iiglu1+iiglu2)**2-PHKt(1,8+iiglu1+iiglu2)**2)
      ELSE
C      WRITE(6,*)' parton 4 mass square LT.0 ',XMIST
         PHKt(5,5+iiglu1) = 0.D0
      END IF
      VHKt(1,8+iiglu1+iiglu2) = VHKk(1,Nc2t)
      VHKt(2,8+iiglu1+iiglu2) = VHKk(2,Nc2t)
      VHKt(3,8+iiglu1+iiglu2) = VHKk(3,Nc2t)
      VHKt(4,8+iiglu1+iiglu2) = VHKk(4,Nc2t)
      WHKt(1,8+iiglu1+iiglu2) = WHKk(1,Nc2t)
      WHKt(2,8+iiglu1+iiglu2) = WHKk(2,Nc2t)
      WHKt(3,8+iiglu1+iiglu2) = WHKk(3,Nc2t)
      WHKt(4,8+iiglu1+iiglu2) = WHKk(4,Nc2t)
      IDHkt(9+iiglu1+iiglu2) = 88888
      ISThkt(9+iiglu1+iiglu2) = 95
      JMOhkt(1,9+iiglu1+iiglu2) = 7 + iiglu1
      JMOhkt(2,9+iiglu1+iiglu2) = 8 + iiglu1 + iiglu2
      JDAhkt(1,9+iiglu1+iiglu2) = 0
      JDAhkt(2,9+iiglu1+iiglu2) = 0
C*NEW
C     PHKT(1,9+IIGLU1+IIGLU2)
C    * =PHKT(1,7+IIGLU1+IIGLU2)+PHKT(1,8+IIGLU1+IIGLU2)+PG1
C     PHKT(2,9+IIGLU1+IIGLU2)
C    * =PHKT(2,7+IIGLU1+IIGLU2)+PHKT(2,8+IIGLU1+IIGLU2)+PG2
C     PHKT(3,9+IIGLU1+IIGLU2)
C    * =PHKT(3,7+IIGLU1+IIGLU2)+PHKT(3,8+IIGLU1+IIGLU2)+PG3
C     PHKT(4,9+IIGLU1+IIGLU2)
C    * =PHKT(4,7+IIGLU1+IIGLU2)+PHKT(4,8+IIGLU1+IIGLU2)+PG4
      PHKt(1,9+iiglu1+iiglu2) = PHKt(1,7+iiglu1)
     &   + PHKt(1,8+iiglu1+iiglu2) + pg1
      PHKt(2,9+iiglu1+iiglu2) = PHKt(2,7+iiglu1)
     &   + PHKt(2,8+iiglu1+iiglu2) + pg2
      PHKt(3,9+iiglu1+iiglu2) = PHKt(3,7+iiglu1)
     &   + PHKt(3,8+iiglu1+iiglu2) + pg3
      PHKt(4,9+iiglu1+iiglu2) = PHKt(4,7+iiglu1)
     &   + PHKt(4,8+iiglu1+iiglu2) + pg4
C*
      xmist = (PHKt(4,9+iiglu1+iiglu2)**2-PHKt(1,9+iiglu1+iiglu2)
     &        **2-PHKt(2,9+iiglu1+iiglu2)**2-PHKt(3,9+iiglu1+iiglu2)**2)
      IF ( xmist.GT.0.D0 ) THEN
         PHKt(5,9+iiglu1+iiglu2)
     &      = SQRT(PHKt(4,9+iiglu1+iiglu2)**2-PHKt(1,9+iiglu1+iiglu2)
     &      **2-PHKt(2,9+iiglu1+iiglu2)**2-PHKt(3,9+iiglu1+iiglu2)**2)
      ELSE
C      WRITE(6,*)' parton 4 mass square LT.0 ',XMIST
         PHKt(5,5+iiglu1) = 0.D0
      END IF
      IF ( Ipip.GE.2 ) THEN
C     IF(NUMEV.EQ.-324)THEN
C     WRITE(6,*)7+IIGLU1,ISTHKT(7+IIGLU1),IDHKT(7+IIGLU1),
C    * JMOHKT(1,7+IIGLU1),JMOHKT(2,7+IIGLU1),JDAHKT(1,7+IIGLU1),
C    *JDAHKT(2,7+IIGLU1),(PHKT(III,7+IIGLU1),III=1,5)
C     DO 91 IIG=7+IIGLU1+1,7+IIGLU1+IIGLU2
C     WRITE(6,*)IIG,ISTHKT(IIG),IDHKT(IIG),JMOHKT(1,IIG),JMOHKT(2,IIG),
C    * JDAHKT(1,IIG),
C    *JDAHKT(2,IIG),(PHKT(III,IIG),III=1,5)
C  91 CONTINUE
C     WRITE(6,*)8+IIGLU1+IIGLU2,ISTHKT(8+IIGLU1+IIGLU2),
C    * IDHKT(8+IIGLU1+IIGLU2),JMOHKT(1,8+IIGLU1+IIGLU2),
C    *JMOHKT(2,8+IIGLU1+IIGLU2),JDAHKT(1,8+IIGLU1+IIGLU2),
C    *JDAHKT(2,8+IIGLU1+IIGLU2),(PHKT(III,8+IIGLU1+IIGLU2),III=1,5)
C     WRITE(6,*)9+IIGLU1+IIGLU2,ISTHKT(9+IIGLU1+IIGLU2),
C    * IDHKT(9+IIGLU1+IIGLU2),JMOHKT(1,9+IIGLU1+IIGLU2),
C    *JMOHKT(2,9+IIGLU1+IIGLU2),JDAHKT(1,9+IIGLU1+IIGLU2),
C    *JDAHKT(2,9+IIGLU1+IIGLU2),(PHKT(III,9+IIGLU1+IIGLU2),III=1,5)
      END IF
      chamal = CHAb1
      IF ( Ipip.EQ.1 ) THEN
         IF ( Ip1.GE.3 .OR. Ipp2.GE.3 .OR. isq1.GE.3 ) chamal = CHAb3
      ELSE IF ( Ipip.EQ.2 ) THEN
         IF ( Ip1.LE.-3 .OR. Ipp2.LE.-3 .OR. isq1.GE.9 ) chamal = CHAb3
      END IF
      IF ( PHKt(5,9+iiglu1+iiglu2).LT.chamal ) THEN
C       IREJ=1
         IPCo = 0
C       RETURN
C       WRITE(6,*)' MUSQBS1 jump back from chain 9',
C    *  'CHAMAL,PHKT(5,9+IIGLU1+IIGLU2)',CHAMAL,PHKT(5,9+IIGLU1+IIGLU2)
         GOTO 200
      END IF
      VHKt(1,9+iiglu1+iiglu2) = VHKk(1,Nc1)
      VHKt(2,9+iiglu1+iiglu2) = VHKk(2,Nc1)
      VHKt(3,9+iiglu1+iiglu2) = VHKk(3,Nc1)
      VHKt(4,9+iiglu1+iiglu2) = VHKk(4,Nc1)
      WHKt(1,9+iiglu1+iiglu2) = WHKk(1,Nc1)
      WHKt(2,9+iiglu1+iiglu2) = WHKk(2,Nc1)
      WHKt(3,9+iiglu1+iiglu2) = WHKk(3,Nc1)
      WHKt(4,9+iiglu1+iiglu2) = WHKk(4,Nc1)
C
      IPCo = 0
      Igcoun = 9 + iiglu1 + iiglu2
      END SUBROUTINE
