
      SUBROUTINE DT_DHADRI(N,Plab,Elab,Cx,Cy,Cz,Itta)
 
      IMPLICIT NONE
      DOUBLE PRECISION am1 , am11 , am2 , am22 , am3 , amn , amn2 , 
     &                 ams , amt , amt2 , bgam , cod1 , cod2 , cod3 , 
     &                 cof1 , cof2 , cof3 , Cx , Cy , Cz
      DOUBLE PRECISION dec , decc , decm , delim , dete , DT_DAMG , 
     &                 DT_RNDM , ecm , ecm1 , ecm2 , ecm3 , ecmmh , 
     &                 ecmn , ecmo , eco , eklim , Elab , gam , hecm , 
     &                 humo
      INTEGER i1001 , i310 , ianth , iatmpt , ib1 , ibn , IDT_IEFUND , 
     &        idwk , ie , ieco , ielim , iiei , iik , iiki , iiwk , ik , 
     &        imach , inrk , ire , ist
      INTEGER it , it1 , it11 , it2 , it22 , it3 , itprf , Itta , 
     &        itwth , itwthc , iwk , kz1 , lowp , N , nnn , nstab
      DOUBLE PRECISION pcm1 , pcm2 , pcm3 , pcmn , Plab , rr , rx , ry , 
     &                 rz , si , sif1 , sif2 , sif3 , umoda , umodat , 
     &                 vv , wdk , wico , wicor , wkk
      DOUBLE PRECISION wok , ww
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
C
C-----------------------------
C  IR COUNTS THE NUMBER OF PRODUCED PARTICLES
C----------------------------
 
      INCLUDE 'inc/hngamr'
      INCLUDE 'inc/hnredv'
      INCLUDE 'inc/hnreac'
C particle properties (BAMJET index convention),
C (dublicate of DTPART for HADRIN)
      INCLUDE 'inc/hnablt'
      INCLUDE 'inc/hnspli'
      INCLUDE 'inc/hnmetl'
      INCLUDE 'inc/hndrun'
C particle properties (BAMJET index convention)
      INCLUDE 'inc/dtpart'
C final state from HADRIN interaction
      INCLUDE 'inc/hnfspa'
 
      DIMENSION itprf(110)
C
      DATA nnn/0/
      DATA umoda/0.D+00/
      DATA itprf/ - 1 , -1 , 5*1 , -1 , -1 , 1 , 1 , 1 , -1 , -1 , -1 , 
     &     -1 , 6*1 , -1 , -1 , -1 , 85*1/
      lowp = 0
      IF ( N.LE.0 .OR. N.GE.111 ) N = 1
C       WRITE (6,1000)
C    +  ' FALSE USE OF THE PARTICLE TYPE INDEX: N, ITTA', N, ITTA
C       STOP
C1000   FORMAT (3(5H ****/),A,2I4,3(5H ****/))
C    +  45H FALSE USE OF THE PARTICLE TYPE INDEX, N,LUE ,I4,3(5H ****/))
      IF ( itprf(N).GT.0 .OR. Itta.GT.8 ) GOTO 1400
      iatmpt = 0
C     IF(IPRI.GE.1) WRITE (6,1010) PLAB
C     STOP
      IF ( ABS(Plab-5.0D0).LT.4.99999D0 ) THEN
      END IF
C1010 FORMAT ( '  PROJECTILE HADRON MOMENTUM OUTSIDE OF THE
C    + ALLOWED REGION, PLAB=',1E15.5)
 
      umodat = N*1.11111D0 + Itta*2.19291D0
      IF ( umodat.NE.umoda ) CALL DT_DCALUM(N,Itta)
      umoda = umodat
 100  iatmpt = 0
      lowp = lowp + 1
 200  imach = 0
      REDu = 2.0D0
C        WRITE(LOUT,*) ' jump 1'
      IF ( lowp.GT.20 ) GOTO 1400
      nnn = N
      IF ( nnn.NE.N ) THEN
         RUNtes = 0.0D0
         EFTes = 0.0D0
      END IF
      IS = 1
      IRH = 0
      ist = 1
      nstab = 23
      ire = NURe(N,1)
      IF ( Itta.GT.1 ) ire = NURe(N,2)
C
C-----------------------------
C----------------------------
      CALL DT_DSIGIN(ire,Plab,N,ie,amt,amn,ecm,si,Itta)
      ianth = -1
C*sr
C     IF (AMH(1).NE.0.93828D0) IANTH=1
      IF ( AMH(1).NE.0.9383D0 ) ianth = 1
C*
      IF ( ianth.GE.0 ) si = 1.0D0
      ecmmh = ecm
C
C-----------------------------
C    ENERGY INDEX
C  IRE CHARACTERIZES THE REACTION
C  IE IS THE ENERGY INDEX
C----------------------------
C        WRITE(LOUT,*) ' jump 2'
      IF ( si.LT.1.D-6 ) GOTO 1400
      IF ( N.GT.nstab ) THEN
         RUNtes = RUNtes + 1.0D0
 
         IF ( LPRi.GT.4 .AND. RUNtes.LT.20.D0 ) WRITE (LOUt,99010) N
99010    FORMAT (' N=',I10,' THE PROEKTILE IS A RESONANCE ')
         IF ( IBArh(N).EQ.1 ) N = 8
         IF ( IBArh(N).EQ.-1 ) N = 9
      END IF
 300  imach = imach + 1
C*sr 19.2.97: loop for direct channel suppression
C     IF (IMACH.GT.10) THEN
C*
C        WRITE(LOUT,*) ' jump 3'
      IF ( imach.GT.1000 ) GOTO 1400
      ecm = ecmmh
      amn2 = amn**2
      amt2 = amt**2
      ecmn = (ecm**2+amn2-amt2)/(2.0D0*ecm)
      IF ( ecmn.LE.amn ) ecmn = amn
      pcmn = SQRT(ecmn**2-amn2)
      gam = (Elab+amt)/ecm
      bgam = Plab/ecm
      IF ( ianth.GE.0 ) ecm = 2.1D0
C
C-----------------------------
C----------------------------
      ist = 0
      vv = DT_RNDM(amn2)
      vv = vv - 1.D-17
C
C-----------------------------
C----------------------------
      iiei = IEIi(ire)
      idwk = IEIi(ire+1) - iiei
      iiwk = IRIi(ire)
      iiki = IKIi(ire)
C
C-----------------------------
C----------------------------
      hecm = ecm
      humo = 2.0D0*UMO(iiei+idwk) - UMO(iiei+idwk-1)
      IF ( humo.LT.ecm ) ecm = humo
C
C-----------------------------
C----------------------------
      ecmo = UMO(ie)
      ecm1 = UMO(ie-1)
      decm = ecmo - ecm1
      dec = ecmo - ecm
C
C-----------------------------
C----------------------------
      ik = 0
      wkk = 0.0D0
      wicor = 0.0D0
 400  ik = ik + 1
      iwk = iiwk + (ik-1)*idwk + ie - iiei
      wok = WK(iwk)
      wdk = wok - WK(iwk-1)
C
C-----------------------------
C    GO TO NEXT CHANNEL, BECAUSE WKK((IK))-WKK((IK-1))=0, IK CAN NOT
C    CONTRIBUTE
C----------------------------
      IF ( Plab.LT.PLAbf(iiei+2) ) wdk = 0.0D0
      wico = wok*1.23459876D0 + wdk*1.735218469D0
      IF ( wico.EQ.wicor ) GOTO 400
      IF ( UMO(iiei+idwk).LT.hecm ) wdk = 0.0D0
      wicor = wico
C
C-----------------------------
C----------------------------
      eklim = -THResh(iiki+ik)
      ielim = IDT_IEFUND(eklim,ire)
      delim = UMO(ielim) + eklim + 1.D-16
      dete = (ecm-(ecmo-eklim)*0.5D0)*2.0D0
      IF ( delim*delim.LE.dete*dete ) THEN
         decc = decm
      ELSE
         decc = delim
      END IF
      wkk = wok - wdk*dec/(decc+1.D-9)
C
C-----------------------------
C----------------------------
C
C
C***IK IS THE REACTION CHANNEL
C----------------------------
      IF ( vv.GT.wkk ) GOTO 400
      inrk = IKIi(ire) + ik
      ecm = hecm
      i1001 = 0
C
 500  it1 = NRK(1,inrk)
      am1 = DT_DAMG(it1)
      it2 = NRK(2,inrk)
      am2 = DT_DAMG(it2)
      ams = am1 + am2
      i1001 = i1001 + 1
C
      IF ( i1001.GT.50 ) GOTO 300
      IF ( it2*ams.GT.it2*ecm ) GOTO 500
      it11 = it1
      it22 = it2
      IF ( ianth.GE.0 ) ecm = Elab + amt + 0.00001D0
      am11 = am1
      am22 = am2
C*sr 19.2.97: supress direct channel for pp-collisions
      IF ( it2.GT.0 ) THEN
         ww = DT_RNDM(eco)
         IF ( ww.GE.0.5D0 ) THEN
            it1 = it22
            it2 = it11
            am1 = am22
            am2 = am11
         END IF
C
C-----------------------------
C   THE FIRST PARTICLE IS DEFINED TO BE THE FORWARD GOING ONE AT SMALL T
         ibn = IBArh(N)
         ib1 = IBArh(it1)
         it11 = it1
         it22 = it2
         am11 = am1
         am22 = am2
         IF ( ib1.NE.ibn ) THEN
            it1 = it22
            it2 = it11
            am1 = am22
            am2 = am11
         END IF
C-----------------------------
C***IT1,IT2 ARE THE CREATED PARTICLES
C***MOMENTA AND DIRECTION COSINA IN THE CM - SYSTEM
C------------------------
         CALL DT_DTWOPA(ecm1,ecm2,pcm1,pcm2,cod1,cod2,cof1,cof2,sif1,
     &                  sif2,it1,it2,ecm,ecmn,pcmn,N,am1,am2)
         ist = ist + 1
         ITS(ist) = it1
         AMM(ist) = am1
C
C-----------------------------
C***TRANSFORMATION INTO LAB SYSTEM AND ROTATION
C----------------------------
         CALL DT_DTRAFO(gam,bgam,Cx,Cy,Cz,cod1,cof1,sif1,pcm1,ecm1,
     &                  PLS(ist),CXS(ist),CYS(ist),CZS(ist),ELS(ist))
         ist = ist + 1
         ITS(ist) = it2
         AMM(ist) = am2
         CALL DT_DTRAFO(gam,bgam,Cx,Cy,Cz,cod2,cof2,sif2,pcm2,ecm2,
     &                  PLS(ist),CXS(ist),CYS(ist),CZS(ist),ELS(ist))
      ELSE
         IF ( (N.EQ.1) .AND. (Itta.EQ.1) .AND. (it2.LE.0) ) THEN
            rr = DT_RNDM(am11)
            IF ( rr.LE.0.75D0 ) GOTO 300
         END IF
C*
C
C-----------------------------
C  INCLUSION OF DIRECT RESONANCES
C  RANDOM CHOICE OF DECAY CHANNELS OF THE DIRECT RESONANCE  IT1
C------------------------
         kz1 = K1H(it1)
         ist = ist + 1
         ieco = 0
         eco = ecm
         gam = (Elab+amt)/eco
         bgam = Plab/eco
         CXS(1) = Cx
         CYS(1) = Cy
         CZS(1) = Cz
         GOTO 700
      END IF
C
C-----------------------------
C***TEST   STABLE OR UNSTABLE
C----------------------------
 600  DO WHILE ( ITS(ist).LE.nstab )
         IRH = IRH + 1
C
C-----------------------------
C***IRH IS THE NUMBER OF THE FINAL STABLE PARTICLE
C----------------------------
C*    IF (REDU.LT.0.D0) GO TO 1009
         ITRh(IRH) = ITS(ist)
         PLRh(IRH) = PLS(ist)
         CXRh(IRH) = CXS(ist)
         CYRh(IRH) = CYS(ist)
         CZRh(IRH) = CZS(ist)
         ELRh(IRH) = ELS(ist)
         ist = ist - 1
C 270 CONTINUE
         IF ( ist.LT.1 ) RETURN
      END DO
C
C  RANDOM CHOICE OF DECAY CHANNELS
C----------------------------
C
      it = ITS(ist)
      eco = AMM(ist)
      gam = ELS(ist)/eco
      bgam = PLS(ist)/eco
      ieco = 0
      kz1 = K1H(it)
 700  ieco = ieco + 1
      vv = DT_RNDM(gam)
      vv = vv - 1.D-17
      iik = kz1 - 1
 800  iik = iik + 1
C
C  IIK IS THE DECAY CHANNEL
C----------------------------
      IF ( vv.GT.WTI(iik) ) GOTO 800
      it1 = NZKi(iik,1)
      i310 = 0
 900  i310 = i310 + 1
      am1 = DT_DAMG(it1)
      it2 = NZKi(iik,2)
      am2 = DT_DAMG(it2)
      IF ( it2.LT.1 ) GOTO 1300
      it3 = NZKi(iik,3)
      am3 = DT_DAMG(it3)
      ams = am1 + am2 + am3
C
C  IF  IIK-KIN.LIM.GT.ACTUAL TOTAL CM-ENERGY, DO AGAIN RANDOM IIK-CHOICE
C----------------------------
      IF ( ieco.LE.10 ) THEN
         IF ( i310.GT.50 ) GOTO 700
C
C  FOR THE DECAY CHANNEL
C  IT1,IT2, IT3 ARE THE PRODUCED PARTICLES FROM  IT
C----------------------------
         IF ( ams.GT.eco ) GOTO 900
         IF ( REDu.LT.0.D0 ) GOTO 100
         itwthc = 0
         REDu = 2.0D0
         IF ( it3.EQ.0 ) GOTO 1100
      ELSE
         iatmpt = iatmpt + 1
C        WRITE(LOUT,*) ' jump 4'
         IF ( iatmpt.LE.3 ) GOTO 200
         GOTO 1400
      END IF
 1000 itwth = 1
      CALL DT_DTHREP(eco,ecm1,ecm2,ecm3,pcm1,pcm2,pcm3,cod1,cof1,sif1,
     &               cod2,cof2,sif2,cod3,cof3,sif3,am1,am2,am3)
      GOTO 1200
 1100 CALL DT_DTWOPD(eco,ecm1,ecm2,pcm1,pcm2,cod1,cof1,sif1,cod2,cof2,
     &               sif2,am1,am2)
      itwth = -1
      it3 = 0
 1200 itwthc = itwthc + 1
      IF ( REDu.LE.0.D0 ) THEN
         REDu = 2.0D0
         IF ( itwthc.GT.100 ) GOTO 100
         IF ( itwth.LE.0 ) GOTO 1100
         GOTO 1000
      END IF
 1300 ITS(ist) = it1
      IF ( it2.GE.1 ) THEN
         ITS(ist+1) = it2
         ITS(ist+2) = it3
         rx = CXS(ist)
         ry = CYS(ist)
         rz = CZS(ist)
         AMM(ist) = am1
         CALL DT_DTRAFO(gam,bgam,rx,ry,rz,cod1,cof1,sif1,pcm1,ecm1,
     &                  PLS(ist),CXS(ist),CYS(ist),CZS(ist),ELS(ist))
         ist = ist + 1
         AMM(ist) = am2
         CALL DT_DTRAFO(gam,bgam,rx,ry,rz,cod2,cof2,sif2,pcm2,ecm2,
     &                  PLS(ist),CXS(ist),CYS(ist),CZS(ist),ELS(ist))
         IF ( it3.GT.0 ) THEN
            ist = ist + 1
            AMM(ist) = am3
            CALL DT_DTRAFO(gam,bgam,rx,ry,rz,cod3,cof3,sif3,pcm3,ecm3,
     &                     PLS(ist),CXS(ist),CYS(ist),CZS(ist),ELS(ist))
         END IF
      END IF
      GOTO 600
C
C----------------------------
C
C   ZERO CROSS SECTION CASE
C----------------------------
C
 1400 IRH = 1
      ITRh(1) = N
      CXRh(1) = Cx
      CYRh(1) = Cy
      CZRh(1) = Cz
      ELRh(1) = Elab
      PLRh(1) = Plab
      END SUBROUTINE
