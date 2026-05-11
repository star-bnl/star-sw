
      SUBROUTINE MUSQBS1(Nc1,Nc1p,Nc1t,Nc2,Nc2p,Nc2t,Irej,Ip11,Ip12,Ip2,
     &                   Ipp1,Ipp2,Ipip,Isq,Igcoun)
C
C                  USQBS-1 diagram (split projectile diquark)
C
      IMPLICIT NONE
      DOUBLE PRECISION chamal , DT_RNDM , DT_SAMPEX , pg1 , pg2 , pg3 , 
     &                 pg4 , unoprv , v , xaqmax , xdiqp , xmist , 
     &                 xqmax , xsaq , xsaq1 , xsq , xsq1 , xvpqi , 
     &                 xvpqii , xvqt
      DOUBLE PRECISION xvthr , xvthro
      INTEGER icou , Igcoun , iig , iiglu1 , iiglu2 , iii , Ip11 , 
     &        Ip12 , Ip2 , Ipip , Ipp1 , Ipp2 , Irej , isaq , isaq1 , 
     &        Isq , isq1 , ivthr , jjg , kkg
      INTEGER Nc1 , Nc1p , Nc1t , Nc2 , Nc2p , Nc2t
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
C                  USQBS-1 diagram (split projectile diquark)
C
C     Input chain 1(NC1) valence-diquark(NC1P)-valence-quark(NC1T)
C     Input chain 2(NC2) sea-quark(NC2P)-sea-antiquark(NC2T)
C
C     Create quark(qsP)-antiquark(aqsT) pair, energy from NC1P and NC1T
C     Split remaining valence diquark(NC1P) into quarks vq1P and vq2P
C
C     Create chains 3 valence quark(vq1P 1)-sea-antiquark(NC2T 2)
C                   6 valence quark(vq2P 4)-sea-quark(aqsT 5)
C                   9 diquark(qsP+NC2P 7)-valence quark(NC1T 8)
C
C       Put new chains into COMMON /HKKTMP/
C
      iiglu1 = Nc1t - Nc1p - 1
      iiglu2 = Nc2t - Nc2p - 1
      Igcoun = 0
C     WRITE(6,*)'MUSQBS1: IIGLU1,IIGLU2,IPIP ',IIGLU1,IIGLU2,IPIP
      CVQ = 1.D0
      Irej = 0
      IF ( Ipip.EQ.3 ) THEN
C     IF(NUMEV.EQ.-324)THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,*)
     &         ' MUSQBS1(NC1,NC1P,NC1T,NC2,NC2P,NC2T,IREJ,' , 
     &        ' IP11,IP12,IP2,IPP1,IPP2,IPIP,IGCOUN)' , Nc1 , Nc1p , 
     &        Nc1t , Nc2 , Nc2p , Nc2t , Irej , Ip11 , Ip12 , Ip2 , 
     &        Ipp1 , Ipp2 , Ipip , Igcoun
      END IF
C
C
C
C     determine x-values of NC1P diquark
      xdiqp = PHKk(4,Nc1p)*2.D0/UMO
      xvqt = PHKk(4,Nc1t)*2.D0/UMO
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
     &         ' MUSQBS1 Rejection 2234 ICOU. GT.100'
         IPCo = 0
         RETURN
      END IF
      IF ( IPCo.GE.3 ) WRITE (LOUt,*)
     &                         'MUSQBS1 call  XSEAPA: UMO,XDIQP,XVQT ' , 
     &                        UMO , xdiqp , xvqt
      xsq = 0.D0
      xsaq = 0.D0
C*NEW
C     CALL XSEAPA(UMO,XDIQP/2.D0,ISQ,ISAQ,XSQ,XSAQ,IREJ)
      IF ( Ipip.EQ.1 ) THEN
         xqmax = xdiqp/2.0D0
         xaqmax = 2.D0*xvqt/3.0D0
      ELSE
         xqmax = 2.D0*xvqt/3.0D0
         xaqmax = xdiqp/2.0D0
      END IF
      CALL DT_CQPAIR(xqmax,xaqmax,xsq,xsaq,Isq,Irej)
      isaq = 6 + Isq
C     write(*,*) 'MUSQBS1: ',ISQ,XSQ,XDIQP,XSAQ,XVQT
C*
      IF ( IPCo.GE.3 ) WRITE (LOUt,*) 'MUSQBS1 after XSEAPA' , Isq , 
     &                        isaq , xsq , xsaq
      IF ( Irej.GE.1 ) THEN
 
         IF ( IPCo.GE.3 .AND. LPRi.GT.4 ) WRITE (LOUt,*)
     &         'MUSQBS1 reject XSEAPA' , Isq , isaq , xsq , xsaq
         IPCo = 0
         RETURN
      END IF
      IF ( Ipip.EQ.1 ) THEN
         IF ( xsaq.GE.2.D0*xvqt/3.D0 ) GOTO 100
      ELSE IF ( Ipip.EQ.2 ) THEN
         IF ( xsq.GE.2.D0*xvqt/3.D0 ) GOTO 100
      END IF
      IF ( IPCo.GE.3 ) THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,'(A,4E12.4)')
     &         ' MUSQBS1 XDIQP,XVQT,XSQ,XSAQ ' , xdiqp , xvqt , xsq , 
     &        xsaq
      END IF
C
C     subtract xsq,xsaq from NC1P diquark and NC1T quark
C
C     XSQ=0.D0
      IF ( Ipip.EQ.1 ) THEN
         xdiqp = xdiqp - xsq
         xvqt = xvqt - xsaq
      ELSE IF ( Ipip.EQ.2 ) THEN
         xdiqp = xdiqp - xsaq
         xvqt = xvqt - xsq
      END IF
 
      IF ( IPCo.GE.3 .AND. LPRi.GT.4 ) WRITE (LOUt,*)
     &      'XDIQP,XVQT after subtraction' , xdiqp , xvqt
C
C     Split remaining valence diquark(NC1P) into quarks vq1P and vq2P
C
      xvthro = CVQ/UMO
      ivthr = 0
 200  IF ( ivthr.EQ.10 ) THEN
         Irej = 1
         IF ( Isq.EQ.3 ) Irej = 3
         IF ( IPCo.GE.3 ) WRITE (LOUt,*) ' MUSQBS1 3466 reject IVTHR 10'
         IPCo = 0
         RETURN
      END IF
      ivthr = ivthr + 1
      xvthr = xvthro/(201-ivthr)
      unoprv = UNOn
C380  CONTINUE
      IF ( xvthr.GT.0.66D0*xdiqp ) THEN
         Irej = 1
         IF ( Isq.EQ.3 ) Irej = 3
         IF ( IPCo.GE.3 ) WRITE (LOUt,*)
     &         ' MUSQBS1 Rejection 380 XVTHR  large ' , xvthr
         IPCo = 0
         RETURN
      END IF
      IF ( DT_RNDM(v).LT.0.5D0 ) THEN
         xvpqi = DT_SAMPEX(xvthr,0.66D0*xdiqp)
         xvpqii = xdiqp - xvpqi
      ELSE
         xvpqii = DT_SAMPEX(xvthr,0.66D0*xdiqp)
         xvpqi = xdiqp - xvpqii
      END IF
      IF ( IPCo.GE.3 ) THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,'(A,2E12.4)')
     &         '  MUSQBS1:XVPQI,XVPQII ' , xvpqi , xvpqii
      END IF
C
C     Prepare 4 momenta of new chains and chain ends
C
C     COMMON /HKKTMP/NHKKT,NEVHKT,ISTHKT(NTMHKK),IDHKT(NTMHKK),JMOHKT
C    +(2,NTMHKK),JDAHKT(2,NTMHKK), PHKT(5,NTMHKK),VHKT(4,NTMHKK),WHKT
C    +(4,NTMHKK)
C     Create chains 3 valence quark(vq1P 1)-sea-antiquark(NC2T 2)
C                   6 valence quark(vq2P 4)-sea-quark(aqsT 5)
C                   9 diquark(qsP+NC2P 7)-valence quark(NC1T 8)
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
      IDHkt(1) = Ip11
      ISThkt(1) = 931
      JMOhkt(1,1) = Nc1p
      JMOhkt(2,1) = 0
      JDAhkt(1,1) = 3 + iiglu1
      JDAhkt(2,1) = 0
C     Create chains 3 valence quark(vq1P 1)-sea-antiquark(NC2T 2)
      PHKt(1,1) = PHKk(1,Nc1p)*xvpqi/(xdiqp+xsq1)
      PHKt(2,1) = PHKk(2,Nc1p)*xvpqi/(xdiqp+xsq1)
      PHKt(3,1) = PHKk(3,Nc1p)*xvpqi/(xdiqp+xsq1)
      PHKt(4,1) = PHKk(4,Nc1p)*xvpqi/(xdiqp+xsq1)
C     PHKT(5,1)  =PHKK(5,NC1P)
      xmist = (PHKt(4,1)**2-PHKt(3,1)**2-PHKt(2,1)**2-PHKt(1,1)**2)
      IF ( xmist.GE.0.D0 ) THEN
         PHKt(5,1) = SQRT(PHKt(4,1)**2-PHKt(3,1)**2-PHKt(2,1)
     &               **2-PHKt(1,1)**2)
      ELSE
C      WRITE(6,*)'MUSQBS1 parton 1 mass square LT.0 ',XMIST
         PHKt(5,1) = 0.D0
      END IF
      VHKt(1,1) = VHKk(1,Nc1p)
      VHKt(2,1) = VHKk(2,Nc1p)
      VHKt(3,1) = VHKk(3,Nc1p)
      VHKt(4,1) = VHKk(4,Nc1p)
      WHKt(1,1) = WHKk(1,Nc1p)
      WHKt(2,1) = WHKk(2,Nc1p)
      WHKt(3,1) = WHKk(3,Nc1p)
      WHKt(4,1) = WHKk(4,Nc1p)
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
      IDHkt(2+iiglu1) = Ipp2
      ISThkt(2+iiglu1) = 932
      JMOhkt(1,2+iiglu1) = Nc2t
      JMOhkt(2,2+iiglu1) = 0
      JDAhkt(1,2+iiglu1) = 3 + iiglu1
      JDAhkt(2,2+iiglu1) = 0
      PHKt(1,2+iiglu1) = PHKk(1,Nc2t)
      PHKt(2,2+iiglu1) = PHKk(2,Nc2t)
      PHKt(3,2+iiglu1) = PHKk(3,Nc2t)
      PHKt(4,2+iiglu1) = PHKk(4,Nc2t)
C     PHKT(5,2+IIGLU1)  =PHKK(5,NC2T)
      xmist = (PHKt(4,2+iiglu1)**2-PHKt(3,2+iiglu1)**2-PHKt(2,2+iiglu1)
     &        **2-PHKt(1,2+iiglu1)**2)
      IF ( xmist.GT.0.D0 ) THEN
         PHKt(5,2+iiglu1) = SQRT(PHKt(4,2+iiglu1)**2-PHKt(3,2+iiglu1)
     &                      **2-PHKt(2,2+iiglu1)**2-PHKt(1,2+iiglu1)**2)
      ELSE
C      WRITE(6,*)' parton 4 mass square LT.0 ',XMIST
         PHKt(5,2+iiglu1) = 0.D0
      END IF
      VHKt(1,2+iiglu1) = VHKk(1,Nc2t)
      VHKt(2,2+iiglu1) = VHKk(2,Nc2t)
      VHKt(3,2+iiglu1) = VHKk(3,Nc2t)
      VHKt(4,2+iiglu1) = VHKk(4,Nc2t)
      WHKt(1,2+iiglu1) = WHKk(1,Nc2t)
      WHKt(2,2+iiglu1) = WHKk(2,Nc2t)
      WHKt(3,2+iiglu1) = WHKk(3,Nc2t)
      WHKt(4,2+iiglu1) = WHKk(4,Nc2t)
      IDHkt(3+iiglu1) = 88888
      ISThkt(3+iiglu1) = 94
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
      IF ( xmist.GE.0.D0 ) THEN
         PHKt(5,3+iiglu1) = SQRT(PHKt(4,3+iiglu1)**2-PHKt(1,3+iiglu1)
     &                      **2-PHKt(2,3+iiglu1)**2-PHKt(3,3+iiglu1)**2)
      ELSE
C      WRITE(6,*)'MUSQBS1 parton 1 mass square LT.0 ',XMIST
         PHKt(5,1) = 0.D0
      END IF
      IF ( Ipip.GE.3 ) THEN
C     IF(NUMEV.EQ.-324)THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,*) 1 , ISThkt(1) , IDHkt(1) , 
     &        JMOhkt(1,1) , JMOhkt(2,1) , JDAhkt(1,1) , JDAhkt(2,1) , 
     &        (PHKt(iii,1),iii=1,5)
         DO iig = 2 , 2 + iiglu1 - 1
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,*) iig , ISThkt(iig) , 
     &           IDHkt(iig) , JMOhkt(1,iig) , JMOhkt(2,iig) , 
     &           JDAhkt(1,iig) , JDAhkt(2,iig) , (PHKt(iii,iig),iii=1,5)
         END DO
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,*) 2 + iiglu1 , ISThkt(2+iiglu1) , 
     &        IDHkt(2+iiglu1) , JMOhkt(1,2+iiglu1) , JMOhkt(2,2+iiglu1)
     &        , JDAhkt(1,2+iiglu1) , JDAhkt(2,2+iiglu1) , 
     &        (PHKt(iii,2+iiglu1),iii=1,5)
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,*) 3 + iiglu1 , ISThkt(3+iiglu1) , 
     &        IDHkt(3+iiglu1) , JMOhkt(1,3+iiglu1) , JMOhkt(2,3+iiglu1)
     &        , JDAhkt(1,3+iiglu1) , JDAhkt(2,3+iiglu1) , 
     &        (PHKt(iii,3+iiglu1),iii=1,5)
      END IF
      chamal = CHAm1
      IF ( Ipip.EQ.1 ) THEN
         IF ( Ip11.GE.3 .OR. Ipp2.GE.3 ) chamal = CHAm3
      ELSE IF ( Ipip.EQ.2 ) THEN
         IF ( Ip11.LE.-3 .OR. Ipp2.LE.-3 ) chamal = CHAm3
      END IF
      IF ( PHKt(5,3+iiglu1).LT.chamal ) THEN
C       IREJ=1
         IPCo = 0
C       RETURN
C       WRITE(6,*)' MUSQBS1 jump back from chain 3'
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
      IDHkt(4+iiglu1) = Ip12
      ISThkt(4+iiglu1) = 931
      JMOhkt(1,4+iiglu1) = Nc1p
      JMOhkt(2,4+iiglu1) = 0
      JDAhkt(1,4+iiglu1) = 6 + iiglu1
      JDAhkt(2,4+iiglu1) = 0
C   create  chain   6 valence quark(vq2P 4)-sea-quark(aqsT 5)
      PHKt(1,4+iiglu1) = PHKk(1,Nc1p)*xvpqii/(xdiqp+xsq1)
      PHKt(2,4+iiglu1) = PHKk(2,Nc1p)*xvpqii/(xdiqp+xsq1)
      PHKt(3,4+iiglu1) = PHKk(3,Nc1p)*xvpqii/(xdiqp+xsq1)
      PHKt(4,4+iiglu1) = PHKk(4,Nc1p)*xvpqii/(xdiqp+xsq1)
C     PHKT(5,4+IIGLU1)  =PHKK(5,NC1P)
      xmist = (PHKt(4,4+iiglu1)**2-PHKt(3,4+iiglu1)**2-PHKt(2,4+iiglu1)
     &        **2-PHKt(1,4+iiglu1)**2)
      IF ( xmist.GT.0.D0 ) THEN
         PHKt(5,4+iiglu1) = SQRT(PHKt(4,4+iiglu1)**2-PHKt(3,4+iiglu1)
     &                      **2-PHKt(2,4+iiglu1)**2-PHKt(1,4+iiglu1)**2)
      ELSE
C      WRITE(6,*)' parton 4 mass square LT.0 ',XMIST
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
      IF ( Ipip.EQ.1 ) THEN
         IDHkt(5+iiglu1) = -(isaq1-6)
      ELSE IF ( Ipip.EQ.2 ) THEN
         IDHkt(5+iiglu1) = isaq1
      END IF
      ISThkt(5+iiglu1) = 932
      JMOhkt(1,5+iiglu1) = Nc1t
      JMOhkt(2,5+iiglu1) = 0
      JDAhkt(1,5+iiglu1) = 6 + iiglu1
      JDAhkt(2,5+iiglu1) = 0
      PHKt(1,5+iiglu1) = PHKk(1,Nc1t)*xsaq1/(xvqt+xsaq1)
      PHKt(2,5+iiglu1) = PHKk(2,Nc1t)*xsaq1/(xvqt+xsaq1)
      PHKt(3,5+iiglu1) = PHKk(3,Nc1t)*xsaq1/(xvqt+xsaq1)
      PHKt(4,5+iiglu1) = PHKk(4,Nc1t)*xsaq1/(xvqt+xsaq1)
C     IF( PHKT(4,5).EQ.0.D0)THEN
C       IREJ=1
CIPCO=0
CRETURN
C     ENDIF
C     PHKT(5,5)  =PHKK(5,NC1T)
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
      ISThkt(6+iiglu1) = 94
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
      IF ( xmist.GE.0.D0 ) THEN
         PHKt(5,6+iiglu1) = SQRT(PHKt(4,6+iiglu1)**2-PHKt(1,6+iiglu1)
     &                      **2-PHKt(2,6+iiglu1)**2-PHKt(3,6+iiglu1)**2)
      ELSE
C      WRITE(6,*)'MUSQBS1 parton 1 mass square LT.0 ',XMIST
         PHKt(5,1) = 0.D0
      END IF
C     IF(IPIP.EQ.3)THEN
      chamal = CHAm1
      IF ( Ipip.EQ.1 ) THEN
         IF ( Ip12.GE.3 .OR. isaq1.GE.9 ) chamal = CHAm3
      ELSE IF ( Ipip.EQ.2 ) THEN
         IF ( Ip12.LE.-3 .OR. isaq1.GE.3 ) chamal = CHAm3
      END IF
      IF ( PHKt(5,6+iiglu1).LT.chamal ) THEN
C       IREJ=1
         IPCo = 0
C       RETURN
C       WRITE(6,*)' MGSQBS1 jump back from chain 6',
C    *  CHAMAL,PHKT(5,6+IIGLU1)
         GOTO 200
      END IF
      IF ( Ipip.GE.3 ) THEN
C     IF(NUMEV.EQ.-324)THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,*) 4 + iiglu1 , ISThkt(4+iiglu1) , 
     &        IDHkt(4+iiglu1) , JMOhkt(1,4+iiglu1) , JMOhkt(2,4+iiglu1)
     &        , JDAhkt(1,4+iiglu1) , JDAhkt(2,4+iiglu1) , 
     &        (PHKt(iii,4+iiglu1),iii=1,5)
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,*) 5 + iiglu1 , ISThkt(5+iiglu1) , 
     &        IDHkt(5+iiglu1) , JMOhkt(1,5+iiglu1) , JMOhkt(2,5+iiglu1)
     &        , JDAhkt(1,5+iiglu1) , JDAhkt(2,5+iiglu1) , 
     &        (PHKt(iii,5+iiglu1),iii=1,5)
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,*) 6 + iiglu1 , ISThkt(6+iiglu1) , 
     &        IDHkt(6+iiglu1) , JMOhkt(1,6+iiglu1) , JMOhkt(2,6+iiglu1)
     &        , JDAhkt(1,6+iiglu1) , JDAhkt(2,6+iiglu1) , 
     &        (PHKt(iii,6+iiglu1),iii=1,5)
      END IF
      VHKt(1,6+iiglu1) = VHKk(1,Nc1)
      VHKt(2,6+iiglu1) = VHKk(2,Nc1)
      VHKt(3,6+iiglu1) = VHKk(3,Nc1)
      VHKt(4,6+iiglu1) = VHKk(4,Nc1)
      WHKt(1,6+iiglu1) = WHKk(1,Nc1)
      WHKt(2,6+iiglu1) = WHKk(2,Nc1)
      WHKt(3,6+iiglu1) = WHKk(3,Nc1)
      WHKt(4,6+iiglu1) = WHKk(4,Nc1)
      IF ( Ipip.EQ.1 ) THEN
         IDHkt(7+iiglu1) = 1000*Ipp1 + 100*Isq + 3
         IF ( IDHkt(7+iiglu1).EQ.1203 ) IDHkt(7+iiglu1) = 2103
         IF ( IDHkt(7+iiglu1).EQ.1303 ) IDHkt(7+iiglu1) = 3103
         IF ( IDHkt(7+iiglu1).EQ.2303 ) IDHkt(7+iiglu1) = 3203
      ELSE IF ( Ipip.EQ.2 ) THEN
         IDHkt(7+iiglu1) = 1000*Ipp1 + 100*(-isq1+6) - 3
         IF ( IDHkt(7+iiglu1).EQ.-1203 ) IDHkt(7+iiglu1) = -2103
         IF ( IDHkt(7+iiglu1).EQ.-1303 ) IDHkt(7+iiglu1) = -3103
         IF ( IDHkt(7+iiglu1).EQ.-2303 ) IDHkt(7+iiglu1) = -3203
C       WRITE(6,*)'IDHKT(7+IIGLU1),IPP1,ISQ1',IDHKT(7+IIGLU1),IPP1,ISQ1
      END IF
      ISThkt(7+iiglu1) = 931
      JMOhkt(1,7+iiglu1) = Nc2p
      JMOhkt(2,7+iiglu1) = 0
      JDAhkt(1,7+iiglu1) = 9 + iiglu1 + iiglu2
      JDAhkt(2,7+iiglu1) = 0
C    create chain     9 diquark(qsP+NC2P 7)-valence quark(NC1T 8)
      PHKt(1,7+iiglu1) = PHKk(1,Nc2p) + PHKk(1,Nc1p)*xsq1/(xdiqp+xsq1)
      PHKt(2,7+iiglu1) = PHKk(2,Nc2p) + PHKk(2,Nc1p)*xsq1/(xdiqp+xsq1)
      PHKt(3,7+iiglu1) = PHKk(3,Nc2p) + PHKk(3,Nc1p)*xsq1/(xdiqp+xsq1)
      PHKt(4,7+iiglu1) = PHKk(4,Nc2p) + PHKk(4,Nc1p)*xsq1/(xdiqp+xsq1)
C     WRITE(6,*)'PHKK(4,NC1P),PHKK(4,NC2P), PHKT(4,7)',
C    * PHKK(4,NC1P),PHKK(4,NC2P), PHKT(4,7)
      IF ( PHKt(4,7+iiglu1).GE.PHKk(4,Nc1p) ) THEN
C       IREJ=1
C       WRITE(6,*)'reject PHKT(4,7+IIGLU1).GE. PHKK(4,NC1P)'
         IPCo = 0
C       RETURN
         GOTO 200
      END IF
C     PHKT(5,7)  =PHKK(5,NC2P)
      PHKt(5,7+iiglu1) = SQRT(PHKt(4,7+iiglu1)**2-PHKt(3,7+iiglu1)
     &                   **2-PHKt(2,7+iiglu1)**2-PHKt(1,7+iiglu1)**2)
      VHKt(1,7+iiglu1) = VHKk(1,Nc2p)
      VHKt(2,7+iiglu1) = VHKk(2,Nc2p)
      VHKt(3,7+iiglu1) = VHKk(3,Nc2p)
      VHKt(4,7+iiglu1) = VHKk(4,Nc2p)
      WHKt(1,7+iiglu1) = WHKk(1,Nc2p)
      WHKt(2,7+iiglu1) = WHKk(2,Nc2p)
      WHKt(3,7+iiglu1) = WHKk(3,Nc2p)
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
      IDHkt(8+iiglu1+iiglu2) = Ip2
      ISThkt(8+iiglu1+iiglu2) = 932
      JMOhkt(1,8+iiglu1+iiglu2) = Nc1t
      JMOhkt(2,8+iiglu1+iiglu2) = 0
      JDAhkt(1,8+iiglu1+iiglu2) = 9 + iiglu1 + iiglu2
      JDAhkt(2,8+iiglu1+iiglu2) = 0
      PHKt(1,8+iiglu1+iiglu2) = PHKk(1,Nc1t)*xvqt/(xsaq1+xvqt)
      PHKt(2,8+iiglu1+iiglu2) = PHKk(2,Nc1t)*xvqt/(xsaq1+xvqt)
      PHKt(3,8+iiglu1+iiglu2) = PHKk(3,Nc1t)*xvqt/(xsaq1+xvqt)
      PHKt(4,8+iiglu1+iiglu2) = PHKk(4,Nc1t)*xvqt/(xsaq1+xvqt)
C     PHKT(5,8+IIGLU1+IIGLU2)  =PHKK(5,NC1T)
      xmist = (PHKt(4,8+iiglu1+iiglu2)**2-PHKt(3,8+iiglu1+iiglu2)
     &        **2-PHKt(2,8+iiglu1+iiglu2)**2-PHKt(1,8+iiglu1+iiglu2)**2)
      IF ( xmist.GT.0.D0 ) THEN
         PHKt(5,8+iiglu1+iiglu2)
     &      = SQRT(PHKt(4,8+iiglu1+iiglu2)**2-PHKt(3,8+iiglu1+iiglu2)
     &      **2-PHKt(2,8+iiglu1+iiglu2)**2-PHKt(1,8+iiglu1+iiglu2)**2)
      ELSE
C      WRITE(6,*)' parton 4 mass square LT.0 ',XMIST
         PHKt(5,8+iiglu1+iiglu2) = 0.D0
      END IF
      VHKt(1,8+iiglu1+iiglu2) = VHKk(1,Nc1t)
      VHKt(2,8+iiglu1+iiglu2) = VHKk(2,Nc1t)
      VHKt(3,8+iiglu1+iiglu2) = VHKk(3,Nc1t)
      VHKt(4,8+iiglu1+iiglu2) = VHKk(4,Nc1t)
      WHKt(1,8+iiglu1+iiglu2) = WHKk(1,Nc1t)
      WHKt(2,8+iiglu1+iiglu2) = WHKk(2,Nc1t)
      WHKt(3,8+iiglu1+iiglu2) = WHKk(3,Nc1t)
      WHKt(4,8+iiglu1+iiglu2) = WHKk(4,Nc1t)
      IDHkt(9+iiglu1+iiglu2) = 88888
      ISThkt(9+iiglu1+iiglu2) = 94
      JMOhkt(1,9+iiglu1+iiglu2) = 7 + iiglu1
      JMOhkt(2,9+iiglu1+iiglu2) = 8 + iiglu1 + iiglu2
      JDAhkt(1,9+iiglu1+iiglu2) = 0
      JDAhkt(2,9+iiglu1+iiglu2) = 0
      PHKt(1,9+iiglu1+iiglu2) = PHKt(1,7+iiglu1)
     &   + PHKt(1,8+iiglu1+iiglu2) + pg1
      PHKt(2,9+iiglu1+iiglu2) = PHKt(2,7+iiglu1)
     &   + PHKt(2,8+iiglu1+iiglu2) + pg2
      PHKt(3,9+iiglu1+iiglu2) = PHKt(3,7+iiglu1)
     &   + PHKt(3,8+iiglu1+iiglu2) + pg3
      PHKt(4,9+iiglu1+iiglu2) = PHKt(4,7+iiglu1)
     &   + PHKt(4,8+iiglu1+iiglu2) + pg4
      xmist = (PHKt(4,9+iiglu1+iiglu2)**2-PHKt(1,9+iiglu1+iiglu2)
     &        **2-PHKt(2,9+iiglu1+iiglu2)**2-PHKt(3,9+iiglu1+iiglu2)**2)
      IF ( xmist.GE.0.D0 ) THEN
         PHKt(5,9+iiglu1+iiglu2)
     &      = SQRT(PHKt(4,9+iiglu1+iiglu2)**2-PHKt(1,9+iiglu1+iiglu2)
     &      **2-PHKt(2,9+iiglu1+iiglu2)**2-PHKt(3,9+iiglu1+iiglu2)**2)
      ELSE
C      WRITE(6,*)'MUSQBS1 parton 1 mass square LT.0 ',XMIST
         PHKt(5,1) = 0.D0
      END IF
      IF ( Ipip.GE.3 ) THEN
C     IF(NUMEV.EQ.-324)THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,*) 7 + iiglu1 , ISThkt(7+iiglu1) , 
     &        IDHkt(7+iiglu1) , JMOhkt(1,7+iiglu1) , JMOhkt(2,7+iiglu1)
     &        , JDAhkt(1,7+iiglu1) , JDAhkt(2,7+iiglu1) , 
     &        (PHKt(iii,7+iiglu1),iii=1,5)
         DO iig = 7 + iiglu1 + 1 , 7 + iiglu1 + iiglu2
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,*) iig , ISThkt(iig) , 
     &           IDHkt(iig) , JMOhkt(1,iig) , JMOhkt(2,iig) , 
     &           JDAhkt(1,iig) , JDAhkt(2,iig) , (PHKt(iii,iig),iii=1,5)
         END DO
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,*) 8 + iiglu1 + iiglu2 , 
     &        ISThkt(8+iiglu1+iiglu2) , IDHkt(8+iiglu1+iiglu2) , 
     &        JMOhkt(1,8+iiglu1+iiglu2) , JMOhkt(2,8+iiglu1+iiglu2) , 
     &        JDAhkt(1,8+iiglu1+iiglu2) , JDAhkt(2,8+iiglu1+iiglu2) , 
     &        (PHKt(iii,8+iiglu1+iiglu2),iii=1,5)
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,*) 9 + iiglu1 + iiglu2 , 
     &        ISThkt(9+iiglu1+iiglu2) , IDHkt(9+iiglu1+iiglu2) , 
     &        JMOhkt(1,9+iiglu1+iiglu2) , JMOhkt(2,9+iiglu1+iiglu2) , 
     &        JDAhkt(1,9+iiglu1+iiglu2) , JDAhkt(2,9+iiglu1+iiglu2) , 
     &        (PHKt(iii,9+iiglu1+iiglu2),iii=1,5)
      END IF
      chamal = CHAb1
      IF ( Ipip.EQ.1 ) THEN
         IF ( Ip2.GE.3 .OR. Ipp1.GE.3 .OR. isq1.GE.3 ) chamal = CHAb3
      ELSE IF ( Ipip.EQ.2 ) THEN
         IF ( Ip2.LE.-3 .OR. Ipp1.LE.-3 .OR. isq1.GE.9 ) chamal = CHAb3
      END IF
      IF ( PHKt(5,9+iiglu1+iiglu2).LT.chamal ) THEN
C       IREJ=1
         IPCo = 0
C       RETURN
C       WRITE(6,*)' MGSQBS1 jump back from chain 9',
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
