
      SUBROUTINE MGSQBS1(Nc1,Nc1p,Nc1t,Nc2,Nc2p,Nc2t,Irej,Ip11,Ip12,Ip2,
     &                   Ipp1,Ipp21,Ipp22,Ipip,Isq,Igcoun)
C
C                  GSQBS-1 diagram (split projectile diquark)
C
      IMPLICIT NONE
      DOUBLE PRECISION chamal , DT_RNDM , DT_SAMPEX , pg1 , pg2 , pg3 , 
     &                 pg4 , unoprv , v , xaqmax , xdiqp , xgive , 
     &                 xmist , xqmax , xsaq , xsaq1 , xsq , xsq1 , 
     &                 xvpqi , xvpqii
      DOUBLE PRECISION xvqt , xvthr , xvthro , xxmist
      INTEGER icou , Igcoun , iig , iiglu1 , iiglu2 , iii , Ip11 , 
     &        Ip12 , Ip2 , Ipip , Ipp1 , Ipp21 , Ipp22 , Irej , isaq , 
     &        isaq1 , Isq , isq1 , ivthr , jjg
      INTEGER kk11 , kk21 , kk22 , kkg , kkkc1 , kkkc2 , mmmc1 , mmmc2 , 
     &        Nc1 , Nc1p , Nc1t , Nc2 , Nc2p , Nc2t , nnnc1 , nnnc2
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
C
C                  GSQBS-1 diagram (split projectile diquark)
C
C
C     Input chain 1(NC1) valence-diquark(NC1P)-valence-quark(NC1T)
C     Input chain 2(NC2) sea-quark(NC2P)-valence-diquark(NC2T)
C
C     Create quark(qs)-antiquark(aqs) pair energy from NC1P and NC1T
C     Split remaining valence diquark(NC1P) into quarks vq1P and vq2P
C
C     Create chains 3 valence quark(vq1P 1)-valence diquark(NC2T 2)
C                   6 valence quark(vq2P 4)-sea-quark(aqsP 5)
C                   9 diquark(qsP+NC2P 7)-valence quark(NC1T 8)
C
C       Put new chains into COMMON /HKKTMP/
C
      iiglu1 = Nc1t - Nc1p - 1
      iiglu2 = Nc2t - Nc2p - 1
      Igcoun = 0
C     WRITE(6,*)' IIGLU1,IIGLU2 ',IIGLU1,IIGLU2
      CVQ = 1.D0
      nnnc1 = IDHkk(Nc1)/1000
      mmmc1 = IDHkk(Nc1) - nnnc1*1000
      kkkc1 = ISThkk(Nc1)
      nnnc2 = IDHkk(Nc2)/1000
      mmmc2 = IDHkk(Nc2) - nnnc2*1000
      kkkc2 = ISThkk(Nc2)
      Irej = 0
      IF ( Ipip.EQ.3 ) THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,*)
     &         ' MGSQBS1(NC1,NC1P,NC1T,NC2,NC2P,NC2T,IREJ,' , 
     &        ' IP11,IP12,IP2,IPP1,IPP21,IPP22,IPIP,IGCOUN)' , Nc1 , 
     &        Nc1p , Nc1t , Nc2 , Nc2p , Nc2t , Irej , Ip11 , Ip12 , 
     &        Ip2 , Ipp1 , Ipp21 , Ipp22 , Ipip , Igcoun
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
     &         ' MGSQBS1 Rejection 2234 ICOU. GT.100'
         IPCo = 0
         RETURN
      END IF
      IF ( IPCo.GE.3 ) WRITE (LOUt,*)
     &                         'MGSQBS1 call  XSEAPA: UMO,XDIQP,XVQT ' , 
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
C     write(*,*) 'MGSQBS1: ',ISQ,XSQ,XDIQP,XSAQ,XVQT
C*
 
      IF ( IPCo.GE.3 .AND. LPRi.GT.4 ) WRITE (LOUt,*)
     &      'MGSQBS1 after XSEAPA' , Isq , isaq , xsq , xsaq
      IF ( Irej.GE.1 ) THEN
 
         IF ( IPCo.GE.3 .AND. LPRi.GT.4 ) WRITE (LOUt,*)
     &         'MGSQBS1 reject XSEAPA' , Isq , isaq , xsq , xsaq
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
     &         ' MGSQBS1 XDIQP,XVQT,XSQ,XSAQ ' , xdiqp , xvqt , xsq , 
     &        xsaq
      END IF
C
C     subtract xsq,xsaq from NC1P diquark and NC1T quark
C
C     XSQ=0.D0
      IF ( Ipip.EQ.1 ) THEN
         xdiqp = xdiqp - xsq
C*NEW
C       IF (XDIQP.LT.0.0D0) WRITE(*,*) ' mgsqbs1: XDIQP<0!!',XDIQP
C*
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
         IF ( IPCo.GE.3 ) WRITE (LOUt,*) ' MGSQBS1 3466 reject IVTHR 10'
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
 
         IF ( IPCo.GE.3 .AND. LPRi.GT.4 ) WRITE (LOUt,*)
     &         ' MGSQBS1 Rejection 380 XVTHR  large ' , xvthr
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
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,'(A,4E12.4)')
     &         '  MGSQBS1:XVTHR,XDIQP,XVPQI,XVPQII ' , xvthr , xdiqp , 
     &        xvpqi , xvpqii
      END IF
C
C     Prepare 4 momenta of new chains and chain ends
C
C     COMMON /HKKTMP/NHKKT,NEVHKT,ISTHKT(NTMHKK),IDHKT(NTMHKK),JMOHKT
C    +(2,NTMHKK),JDAHKT(2,NTMHKK), PHKT(5,NTMHKK),VHKT(4,NTMHKK),WHKT
C    +(4,NTMHKK)
C     Create chains 3 valence quark(vq1P 1)-valence diquark(NC2T 2)
C                   6 valence quark(vq2P 4)-sea-quark(aqsP 5)
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
      kk11 = Ip11
C     IDHKT(2)   =1000*IPP21+100*IPP22+1
      kk21 = Ipp21
      kk22 = Ipp22
      xgive = 0.D0
      IDHkt(4+iiglu1) = Ip12
      ISThkt(4+iiglu1) = 921
      JMOhkt(1,4+iiglu1) = Nc1p
      JMOhkt(2,4+iiglu1) = 0
      JDAhkt(1,4+iiglu1) = 6 + iiglu1
      JDAhkt(2,4+iiglu1) = 0
C*NEW
 
      IF ( (xdiqp.LT.0.0D0) .OR. (xvpqii.LT.0.0D0) .OR. (xsq1.LT.0.0D0)
     &     .AND. LPRi.GT.4 ) WRITE (LOUt,*) ' mgsqbs1: ' , xdiqp , 
     &     xvpqii , xsq1
C*
      PHKt(1,4+iiglu1) = PHKk(1,Nc1p)*xvpqii/(xdiqp+xsq1)
      PHKt(2,4+iiglu1) = PHKk(2,Nc1p)*xvpqii/(xdiqp+xsq1)
      PHKt(3,4+iiglu1) = PHKk(3,Nc1p)*xvpqii/(xdiqp+xsq1)
      PHKt(4,4+iiglu1) = PHKk(4,Nc1p)*xvpqii/(xdiqp+xsq1)
C     PHKT(5,4+IIGLU1)  =PHKK(5,NC1P)
      xxmist = (PHKt(4,4+iiglu1)**2-PHKt(3,4+iiglu1)**2-PHKt(2,4+iiglu1)
     &         **2-PHKt(1,4+iiglu1)**2)
      IF ( xxmist.GT.0.D0 ) THEN
         PHKt(5,4+iiglu1) = SQRT(xxmist)
      ELSE
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,*) 'MGSQBS1 XXMIST' , xxmist
         xxmist = ABS(xxmist)
         PHKt(5,4+iiglu1) = SQRT(xxmist)
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
      ISThkt(5+iiglu1) = 922
      JMOhkt(1,5+iiglu1) = Nc1t
      JMOhkt(2,5+iiglu1) = 0
      JDAhkt(1,5+iiglu1) = 6 + iiglu1
      JDAhkt(2,5+iiglu1) = 0
C*NEW
 
      IF ( (xsaq1.LT.0.0D0) .OR. (xvqt.LT.0.0D0) .AND. LPRi.GT.4 )
     &     WRITE (LOUt,*) ' mgsqbs2: ' , xsaq1 , xvqt
C*
      PHKt(1,5+iiglu1) = PHKk(1,Nc1t)*xsaq1/(xvqt+xsaq1)
      PHKt(2,5+iiglu1) = PHKk(2,Nc1t)*xsaq1/(xvqt+xsaq1)
      PHKt(3,5+iiglu1) = PHKk(3,Nc1t)*xsaq1/(xvqt+xsaq1)
      PHKt(4,5+iiglu1) = PHKk(4,Nc1t)*xsaq1/(xvqt+xsaq1)
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
C     IDHKT(6)   =1000*NNNC1+MMMC1
      ISThkt(6+iiglu1) = 93
C     ISTHKT(6)  =KKKC1
      JMOhkt(1,6+iiglu1) = 4 + iiglu1
      JMOhkt(2,6+iiglu1) = 5 + iiglu1
      JDAhkt(1,6+iiglu1) = 0
      JDAhkt(2,6+iiglu1) = 0
      PHKt(1,6+iiglu1) = PHKt(1,4+iiglu1) + PHKt(1,5+iiglu1)
      PHKt(2,6+iiglu1) = PHKt(2,4+iiglu1) + PHKt(2,5+iiglu1)
      PHKt(3,6+iiglu1) = PHKt(3,4+iiglu1) + PHKt(3,5+iiglu1)
      PHKt(4,6+iiglu1) = PHKt(4,4+iiglu1) + PHKt(4,5+iiglu1)
      PHKt(5,6+iiglu1) = SQRT(PHKt(4,6+iiglu1)**2-PHKt(1,6+iiglu1)
     &                   **2-PHKt(2,6+iiglu1)**2-PHKt(3,6+iiglu1)**2)
      chamal = CHAm1
      IF ( Ipip.EQ.1 ) THEN
         IF ( Ip12.GE.3 .OR. isaq.GE.9 ) chamal = CHAm3
      ELSE IF ( Ipip.EQ.2 ) THEN
         IF ( Ip12.LE.-3 .OR. isaq.GE.3 ) chamal = CHAm3
      END IF
      IF ( PHKt(5,6+iiglu1).LT.chamal ) THEN
         IF ( IDHkt(5+iiglu1).EQ.-IDHkt(4+iiglu1) ) THEN
C                    we drop chain 6 and give the energy to chain 3
            IDHkt(6+iiglu1) = 33888
            xgive = 1.D0
C         WRITE(6,*)' drop chain 6 xgive=1'
            GOTO 300
         ELSE IF ( IDHkt(5+iiglu1).EQ.-Ip11 ) THEN
C                    we drop chain 6 and give the energy to chain 3
C                    and change KK11 to IDHKT(4)
            IDHkt(6+iiglu1) = 33888
            xgive = 1.D0
C         WRITE(6,*)' drop chain 6 xgive=1 KK11=IDHKT(4+IIGLU1)'
            kk11 = IDHkt(4+iiglu1)
            GOTO 300
         ELSE IF ( IDHkt(5+iiglu1).EQ.-Ipp21 ) THEN
C                    we drop chain 6 and give the energy to chain 3
C                    and change KK21 to IDHKT(4)
C     IDHKT(2)   =1000*IPP21+100*IPP22+1
            IDHkt(6+iiglu1) = 33888
            xgive = 1.D0
C         WRITE(6,*)' drop chain 6 xgive=1 KK21=IDHKT(4+IIGLU1)'
            kk21 = IDHkt(4+iiglu1)
            GOTO 300
         ELSE IF ( IDHkt(5+iiglu1).EQ.-Ipp22 ) THEN
C                    we drop chain 6 and give the energy to chain 3
C                    and change KK22 to IDHKT(4)
C     IDHKT(2)   =1000*IPP21+100*IPP22+1
            IDHkt(6+iiglu1) = 33888
            xgive = 1.D0
C         WRITE(6,*)' drop chain 6 xgive=1 KK22=IDHKT(4+IIGLU1)'
            kk22 = IDHkt(4+iiglu1)
            GOTO 300
         END IF
C      IREJ=1
         IPCo = 0
C      RETURN
C       WRITE(6,*)' MGSQBS1 jump back from chain 6'
         GOTO 200
      END IF
 300  IF ( Ipip.GE.3 ) THEN
 
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
C     IDHKT(1)   =IP11
      IDHkt(1) = kk11
      ISThkt(1) = 921
      JMOhkt(1,1) = Nc1p
      JMOhkt(2,1) = 0
      JDAhkt(1,1) = 3 + iiglu1
      JDAhkt(2,1) = 0
C    * +0.5D0*PHKK(1,NC2P)
      PHKt(1,1) = PHKk(1,Nc1p)*xvpqi/(xdiqp+xsq1)
     &            + xgive*PHKt(1,4+iiglu1)
C    * +0.5D0*PHKK(2,NC2P)
      PHKt(2,1) = PHKk(2,Nc1p)*xvpqi/(xdiqp+xsq1)
     &            + xgive*PHKt(2,4+iiglu1)
C    * +0.5D0*PHKK(3,NC2P)
      PHKt(3,1) = PHKk(3,Nc1p)*xvpqi/(xdiqp+xsq1)
     &            + xgive*PHKt(3,4+iiglu1)
C    * +0.5D0*PHKK(4,NC2P)
      PHKt(4,1) = PHKk(4,Nc1p)*xvpqi/(xdiqp+xsq1)
     &            + xgive*PHKt(4,4+iiglu1)
C     PHKT(5,1)  =PHKK(5,NC1P)
      xmist = (PHKt(4,1)**2-PHKt(3,1)**2-PHKt(2,1)**2-PHKt(1,1)**2)
      IF ( xmist.GE.0.D0 ) THEN
         PHKt(5,1) = SQRT(PHKt(4,1)**2-PHKt(3,1)**2-PHKt(2,1)
     &               **2-PHKt(1,1)**2)
      ELSE
C      WRITE(6,*)'MGSQBS1 parton 1 mass square LT.0 ',XMIST
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
C     IDHKT(2)   =1000*IPP21+100*IPP22+1
      IF ( Ipip.EQ.1 ) THEN
         IDHkt(2+iiglu1) = 1000*kk21 + 100*kk22 + 3
         IF ( IDHkt(2+iiglu1).EQ.1203 ) IDHkt(2+iiglu1) = 2103
         IF ( IDHkt(2+iiglu1).EQ.1303 ) IDHkt(2+iiglu1) = 3103
         IF ( IDHkt(2+iiglu1).EQ.2303 ) IDHkt(2+iiglu1) = 3203
      ELSE IF ( Ipip.EQ.2 ) THEN
         IDHkt(2+iiglu1) = 1000*kk21 + 100*kk22 - 3
         IF ( IDHkt(2+iiglu1).EQ.-1203 ) IDHkt(2+iiglu1) = -2103
         IF ( IDHkt(2+iiglu1).EQ.-1303 ) IDHkt(2+iiglu1) = -3103
         IF ( IDHkt(2+iiglu1).EQ.-2303 ) IDHkt(2+iiglu1) = -3203
      END IF
      ISThkt(2+iiglu1) = 922
      JMOhkt(1,2+iiglu1) = Nc2t
      JMOhkt(2,2+iiglu1) = 0
      JDAhkt(1,2+iiglu1) = 3 + iiglu1
      JDAhkt(2,2+iiglu1) = 0
      PHKt(1,2+iiglu1) = PHKk(1,Nc2t) + xgive*PHKt(1,5+iiglu1)
      PHKt(2,2+iiglu1) = PHKk(2,Nc2t) + xgive*PHKt(2,5+iiglu1)
      PHKt(3,2+iiglu1) = PHKk(3,Nc2t) + xgive*PHKt(3,5+iiglu1)
      PHKt(4,2+iiglu1) = PHKk(4,Nc2t) + xgive*PHKt(4,5+iiglu1)
C     PHKT(5,2)  =PHKK(5,NC2T)
      xmist = (PHKt(4,2+iiglu1)**2-PHKt(3,2+iiglu1)**2-PHKt(2,2+iiglu1)
     &        **2-PHKt(1,2+iiglu1)**2)
      IF ( xmist.GT.0.D0 ) THEN
         PHKt(5,2+iiglu1) = SQRT(PHKt(4,2+iiglu1)**2-PHKt(3,2+iiglu1)
     &                      **2-PHKt(2,2+iiglu1)**2-PHKt(1,2+iiglu1)**2)
      ELSE
C     WRITE(6,*)'MUSQBS2 parton 1 mass square LT.0 ',XMIST
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
C     IDHKT(3)   =1000*NNNC1+MMMC1+10
      ISThkt(3+iiglu1) = 93
C     ISTHKT(3)  =KKKC1
      JMOhkt(1,3+iiglu1) = 1
      JMOhkt(2,3+iiglu1) = 2 + iiglu1
      JDAhkt(1,3+iiglu1) = 0
      JDAhkt(2,3+iiglu1) = 0
      PHKt(1,3+iiglu1) = PHKt(1,1) + PHKt(1,2+iiglu1) + pg1
      PHKt(2,3+iiglu1) = PHKt(2,1) + PHKt(2,2+iiglu1) + pg2
      PHKt(3,3+iiglu1) = PHKt(3,1) + PHKt(3,2+iiglu1) + pg3
      PHKt(4,3+iiglu1) = PHKt(4,1) + PHKt(4,2+iiglu1) + pg4
      PHKt(5,3+iiglu1) = SQRT(PHKt(4,3+iiglu1)**2-PHKt(1,3+iiglu1)
     &                   **2-PHKt(2,3+iiglu1)**2-PHKt(3,3+iiglu1)**2)
      IF ( Ipip.GE.3 ) THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,*) 1 , ISThkt(1) , IDHkt(1) , 
     &        JMOhkt(1,1) , JMOhkt(2,1) , JDAhkt(1,1) , JDAhkt(2,1) , 
     &        (PHKt(iii,1),iii=1,5)
         DO iig = 2 , 2 + iiglu1 - 1
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,*) iig , ISThkt(iig) , 
     &           IDHkt(iig) , JMOhkt(1,iig) , JMOhkt(2,iig) , 
     &           JDAhkt(1,iig) , JDAhkt(2,iig) , (PHKt(iii,iig),iii=1,5)
         END DO
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,*) 2 + iiglu1 , ISThkt(2+iiglu1) , 
     &        IDHkt(2) , JMOhkt(1,2+iiglu1) , JMOhkt(2,2+iiglu1) , 
     &        JDAhkt(1,2+iiglu1) , JDAhkt(2,2+iiglu1) , 
     &        (PHKt(iii,2+iiglu1),iii=1,5)
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,*) 3 + iiglu1 , ISThkt(3+iiglu1) , 
     &        IDHkt(3+iiglu1) , JMOhkt(1,3+iiglu1) , JMOhkt(2,3+iiglu1)
     &        , JDAhkt(1,3+iiglu1) , JDAhkt(2,3+iiglu1) , 
     &        (PHKt(iii,3+iiglu1),iii=1,5)
      END IF
      chamal = CHAb1
C*NEW
C     IF(IPIP.EQ.1)THEN
C       IF(IPP21.GE.3.OR.IPP22.GE.3.OR.IP11.GE.3)CHAMAL=CHAB3
C     ELSEIF(IPIP.EQ.2)THEN
C       IF(IPP21.LE.-3.OR.IPP22.LE.-3.OR.IP11.LE.-3)CHAMAL=CHAB3
C     ENDIF
      IF ( Ipip.EQ.1 ) THEN
         IF ( kk21.GE.3 .OR. kk22.GE.3 .OR. kk11.GE.3 ) chamal = CHAb3
      ELSE IF ( Ipip.EQ.2 ) THEN
         IF ( kk21.LE.-3 .OR. kk22.LE.-3 .OR. kk11.LE.-3 )
     &        chamal = CHAb3
      END IF
C*
      IF ( PHKt(5,3+iiglu1).LT.chamal ) THEN
C       IREJ=1
         IPCo = 0
C       RETURN
C       WRITE(6,*)' MGSQBS1 jump back from chain 3'
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
         IDHkt(7+iiglu1) = 1000*Ipp1 + 100*isq1 + 3
         IF ( IDHkt(7+iiglu1).EQ.1203 ) IDHkt(7+iiglu1) = 2103
         IF ( IDHkt(7+iiglu1).EQ.1303 ) IDHkt(7+iiglu1) = 3103
         IF ( IDHkt(7+iiglu1).EQ.2303 ) IDHkt(7+iiglu1) = 3203
      ELSE IF ( Ipip.EQ.2 ) THEN
         IDHkt(7+iiglu1) = 1000*Ipp1 + 100*(-isq1+6) - 3
         IF ( IDHkt(7+iiglu1).EQ.-1203 ) IDHkt(7+iiglu1) = -2103
         IF ( IDHkt(7+iiglu1).EQ.-1303 ) IDHkt(7+iiglu1) = -3103
         IF ( IDHkt(7+iiglu1).EQ.-2303 ) IDHkt(7+iiglu1) = -3203
C       WRITE(6,*)'IDHKT(7),IPP1,ISQ1',IDHKT(7),IPP1,ISQ1
      END IF
      ISThkt(7+iiglu1) = 921
      JMOhkt(1,7+iiglu1) = Nc2p
      JMOhkt(2,7+iiglu1) = 0
      JDAhkt(1,7+iiglu1) = 9 + iiglu1 + iiglu2
      JDAhkt(2,7+iiglu1) = 0
C     PHKT(1,7)  =0.5D0*PHKK(1,NC2P)+PHKK(1,NC1P)*XSQ/(XDIQP+XSQ)
C     PHKT(2,7)  =0.5D0*PHKK(2,NC2P)+PHKK(2,NC1P)*XSQ/(XDIQP+XSQ)
C     PHKT(3,7)  =0.5D0*PHKK(3,NC2P)+PHKK(3,NC1P)*XSQ/(XDIQP+XSQ)
C     PHKT(4,7+IIGLU1)  =0.5D0*PHKK(4,NC2P)+PHKK(4,NC1P)*XSQ/(XDIQP+XSQ)
C*NEW
 
      IF ( (xsq1.LT.0.0D0) .OR. (xdiqp.LT.0.0D0) .AND. LPRi.GT.4 )
     &     WRITE (LOUt,*) ' mgsqbs3: ' , xsq1 , xdiqp
C*
      PHKt(1,7+iiglu1) = PHKk(1,Nc2p) + PHKk(1,Nc1p)*xsq1/(xdiqp+xsq1)
      PHKt(2,7+iiglu1) = PHKk(2,Nc2p) + PHKk(2,Nc1p)*xsq1/(xdiqp+xsq1)
      PHKt(3,7+iiglu1) = PHKk(3,Nc2p) + PHKk(3,Nc1p)*xsq1/(xdiqp+xsq1)
      PHKt(4,7+iiglu1) = PHKk(4,Nc2p) + PHKk(4,Nc1p)*xsq1/(xdiqp+xsq1)
C     WRITE(6,*)'PHKK(4,NC1P),PHKK(4,NC2P), PHKT(4,7)',
C    * PHKK(4,NC1P),PHKK(4,NC2P), PHKT(4,7)
      IF ( PHKt(4,7+iiglu1).GE.PHKk(4,Nc1p) ) THEN
C       IREJ=1
C       WRITE(6,*)'reject PHKT(4,7).GE. PHKK(4,NC1P)'
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
      ISThkt(8+iiglu1+iiglu2) = 922
      JMOhkt(1,8+iiglu1+iiglu2) = Nc1t
      JMOhkt(2,8+iiglu1+iiglu2) = 0
      JDAhkt(1,8+iiglu1+iiglu2) = 9 + iiglu1 + iiglu2
      JDAhkt(2,8+iiglu1+iiglu2) = 0
C*NEW
 
      IF ( (xvqt.LT.0.0D0) .OR. (xsaq1.LT.0.0D0) .AND. LPRi.GT.4 )
     &     WRITE (LOUt,*) ' mgsqbs4: ' , xvqt , xsaq1
C*
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
C     WRITE(6,*)'MUSQBS2 parton 1 mass square LT.0 ',XMIST
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
C     IDHKT(9)   =1000*NNNC2+MMMC2+10
      ISThkt(9+iiglu1+iiglu2) = 93
C     ISTHKT(9)  =KKKC2
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
      PHKt(5,9+iiglu1+iiglu2)
     &   = SQRT(PHKt(4,9+iiglu1+iiglu2)**2-PHKt(1,9+iiglu1+iiglu2)
     &   **2-PHKt(2,9+iiglu1+iiglu2)**2-PHKt(3,9+iiglu1+iiglu2)**2)
      IF ( Ipip.GE.3 ) THEN
 
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
      Igcoun = 9 + iiglu1 + iiglu2
      IPCo = 0
      END SUBROUTINE
