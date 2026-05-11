
      SUBROUTINE DT_DIAGR(Na,Nb,Ijproj,B,Js,Jt,Jnt,Inta,Intb,Idirec,
     &                    Nidx)
 
C***********************************************************************
C Based on the original version by Shmakov et al.                      *
C This version dated 21.04.95 is revised by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION afluc , ai , ALPHEM , AMP , AMP2 , AMRHO0 , amv , 
     &                 amv2 , ar , B , bdum , dcoh , DT_RNDM , DT_SAM2 , 
     &                 DT_SIGVP , dumzer , elab , fca , gam , GEV2FM
      DOUBLE PRECISION GEV2MB , ONE , p , PI , plab , qq1 , qq2 , rca , 
     &                 rpnt , s , sdum1 , sdum2 , sdum3 , sigel , sq2 , 
     &                 TINY10 , TWO , TWOPI , x
      DOUBLE PRECISION xnu , xy , ZERO , zero1
      INTEGER i , icnt , Idirec , idx , ifluk , Ijproj , ina , inb , 
     &        Inta , Intb , ipnt , j , ji1 , ji2 , Jnt , jnt0 , Js , 
     &        js0 , Jt , jt0
      INTEGER k , kint , kk1 , MAXINT , MAXNCL , MAXSQU , MAXVQU , Na , 
     &        Nb , ncall , Nidx , ntarg , ntargo , ntry , nwa , nwamax , 
     &        nwb , nwbmax
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ZERO=0.0D0,TINY10=1.0D-10,ONE=1.0D0,TWO=2.0D0)
C proton mass
C rho0 mass
      PARAMETER (TWOPI=6.283185307179586454D+00,PI=TWOPI/TWO,
     &           GEV2MB=0.38938D0,GEV2FM=0.1972D0,ALPHEM=ONE/137.0D0,
     &           AMP=0.938D0,AMP2=AMP**2,AMRHO0=0.77D0)
 
      COMPLEX*16 c , ca , ci
 
 
      PARAMETER (MAXNCL=260,MAXVQU=MAXNCL,MAXSQU=20*MAXVQU,
     &           MAXINT=MAXVQU+MAXSQU)
C particle properties (BAMJET index convention)
      INCLUDE 'inc/dtpart'
C emulsion treatment
      INCLUDE 'inc/dtcomp'
C Glauber formalism: parameters
      INCLUDE 'inc/dtglam'
C Glauber formalism: cross sections
      INCLUDE 'inc/dtglxs'
C VDM parameter for photon-nucleus interactions
      INCLUDE 'inc/dtvdmp'
C nucleon-nucleon event-generator
      INCLUDE 'inc/dtmodl'
C*PHOJET105a
C     COMMON /CUTOFF/ PTCUT(4),CUTMU(4),FPS(4),FPH(4),PSOMIN,XSOMIN
C*PHOJET112
C  obsolete cut-off information
      INCLUDE 'inc/pocut1'
C*
C coordinates of nucleons
      INCLUDE 'inc/dtnuco'
C interface between Glauber formalism and DPM
      INCLUDE 'inc/dtglif'
C statistics: Glauber-formalism
      INCLUDE 'inc/dtsta3'
C n-n cross section fluctuations
      INCLUDE 'inc/dtxsfl'
 
      DIMENSION Js(MAXNCL) , Jt(MAXNCL) , js0(MAXNCL) , 
     &          jt0(MAXNCL,MAXNCL) , ji1(MAXNCL,MAXNCL) , 
     &          ji2(MAXNCL,MAXNCL) , jnt0(MAXNCL)
      DIMENSION nwa(0:MAXNCL) , nwb(0:MAXNCL)
 
      LOGICAL lfirst
      DATA lfirst/.TRUE./
 
      DATA ntargo , icnt/0 , 0/
 
      ntarg = ABS(Nidx)
 
      IF ( lfirst ) THEN
         lfirst = .FALSE.
         IF ( NCOmpo.EQ.0 ) THEN
            ncall = 0
            nwamax = Na
            nwbmax = Nb
            DO i = 0 , MAXNCL
               nwa(i) = 0
               nwb(i) = 0
            END DO
         END IF
      END IF
      IF ( ntarg.EQ.-1 ) THEN
         IF ( NCOmpo.EQ.0 ) THEN
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,*)
     &            ' DIAGR: distribution of wounded nucleons'
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,'(8X,A,3I7)')
     &            'NCALL,NWAMAX,NWBMAX = ' , ncall , nwamax , nwbmax
            DO i = 1 , MAX(nwamax,nwbmax)
 
               IF ( LPRi.GT.4 ) WRITE (LOUt,'(8X,2I7,E12.4,I7,E12.4)')
     &              i , nwa(i) , DBLE(nwa(i))/DBLE(ncall) , nwb(i) , 
     &              DBLE(nwb(i))/DBLE(ncall)
            END DO
         END IF
         RETURN
      END IF
 
      dcoh = 1.0D10
      ipnt = 0
 
      sq2 = Q2
      IF ( sq2.LE.ZERO ) sq2 = 0.0001D0
      s = ECMnow**2
      x = sq2/(s+sq2-AMP2)
      xnu = (s+sq2-AMP2)/(TWO*AMP)
C photon projectiles: recalculate photon-nucleon amplitude
      IF ( Ijproj.EQ.7 ) THEN
C  VDM assumption: mass of V-meson
 50      amv2 = DT_SAM2(sq2,ECMnow)
         amv = SQRT(amv2)
         IF ( amv.GT.2.0D0*PTCut(1) ) GOTO 50
C  check for pointlike interaction
         CALL DT_POILIK(Nb,ntarg,ECMnow,sq2,ipnt,rpnt,1)
C*sr 27.10.
C        SIGSH  = DT_SIGVP(X,SQ2)/(AMV2+SQ2+RL2)/10.0D0
         SIGsh = (ONE-rpnt)*DT_SIGVP(x,sq2)/(amv2+sq2+RL2)/10.0D0
C*
         ROSh = 0.1D0
         BSLope = 2.0D0*(2.0D0+AMRHO0**2/(amv2+sq2)
     &            +0.25D0*LOG(s/(amv2+sq2)))
C  coherence length
         IF ( ISHad(3).EQ.1 ) dcoh = TWO*xnu/(amv2+sq2)*GEV2FM
      ELSE IF ( ((Ijproj.LE.40) .OR. ((Ijproj.GE.97) .AND. (Ijproj.LE.
     &          103)) .OR. (Ijproj.EQ.109) .OR. (Ijproj.EQ.115)) .AND. 
     &          (Ijproj.NE.7) ) THEN
         IF ( MCGene.EQ.2 ) THEN
            zero1 = ZERO
            CALL DT_PHOXS(Ijproj,1,ECMnow,zero1,sdum1,sdum2,sdum3,
     &                    BSLope,0)
         ELSE
            BSLope = 8.5D0*(1.0D0+0.065D0*LOG(s))
         END IF
         IF ( ECMnow.LE.3.0D0 ) THEN
            ROSh = -0.43D0
         ELSE IF ( (ECMnow.GT.3.0D0) .AND. (ECMnow.LE.50.D0) ) THEN
            ROSh = -0.63D0 + 0.175D0*LOG(ECMnow)
         ELSE IF ( ECMnow.GT.50.0D0 ) THEN
            ROSh = 0.1D0
         END IF
         elab = (s-AAM(Ijproj)**2-AMP2)/(TWO*AMP)
         plab = SQRT((elab-AAM(Ijproj))*(elab+AAM(Ijproj)))
         IF ( MCGene.EQ.2 ) THEN
            zero1 = ZERO
            CALL DT_PHOXS(Ijproj,1,ECMnow,zero1,SIGsh,sdum2,sdum3,bdum,
     &                    0)
            SIGsh = SIGsh/10.0D0
         ELSE
C           SIGSH = DT_SHNTOT(IJPROJ,1,ZERO,PLAB)/10.0D0
            dumzer = ZERO
            CALL DT_XSHN(Ijproj,1,plab,dumzer,SIGsh,sigel)
            SIGsh = SIGsh/10.0D0
         END IF
      ELSE
         BSLope = 6.0D0*(1.0D0+0.065D0*LOG(s))
         ROSh = 0.01D0
         elab = (s-AAM(Ijproj)**2-AMP2)/(TWO*AMP)
         plab = SQRT((elab-AAM(Ijproj))*(elab+AAM(Ijproj)))
C        SIGSH = DT_SHNTOT(IJPROJ,1,ZERO,PLAB)/10.0D0
         dumzer = ZERO
         CALL DT_XSHN(Ijproj,1,plab,dumzer,SIGsh,sigel)
         SIGsh = SIGsh/10.0D0
      END IF
      GSH = 10.0D0/(TWO*BSLope*GEV2MB)
      gam = GSH
      rca = gam*SIGsh/TWOPI
      fca = -ROSh*rca
      ca = DCMPLX(rca,fca)
      ci = DCMPLX(ONE,ZERO)
 
C impact parameter
 
 
 100  IF ( MCGene.NE.3 ) CALL DT_MODB(B,Nidx)
      ntry = 0
 200  ntry = ntry + 1
C initializations
      Jnt = 0
      DO i = 1 , Na
         Js(i) = 0
      END DO
      DO i = 1 , Nb
         Jt(i) = 0
      END DO
      IF ( Ijproj.EQ.7 ) THEN
         DO i = 1 , MAXNCL
            js0(i) = 0
            jnt0(i) = 0
            DO j = 1 , Nb
               jt0(i,j) = 0
            END DO
         END DO
      END IF
 
C nucleon configuration
C     IF ((NTARG.NE.NTARGO).OR.(MOD(ICNT,5).EQ.0)) THEN
      IF ( (ntarg.NE.ntargo) .OR. (MOD(icnt,1).EQ.0) ) THEN
C        CALL DT_CONUCL(PKOO,NA,RASH,2)
C        CALL DT_CONUCL(TKOO,NB,RBSH(NTARG),1)
         IF ( Nidx.LE.-1 ) THEN
            CALL DT_CONUCL(PKOo,Na,RASh(1),0)
            CALL DT_CONUCL(TKOo,Nb,RBSh(ntarg),0)
         ELSE
            CALL DT_CONUCL(PKOo,Na,RASh(ntarg),0)
            CALL DT_CONUCL(TKOo,Nb,RBSh(1),0)
         END IF
         ntargo = ntarg
      END IF
      icnt = icnt + 1
 
C LEPTO: pick out one struck nucleon
      IF ( MCGene.EQ.3 ) THEN
         Jnt = 1
         Js(1) = 1
         idx = INT(DT_RNDM(x)*Nb) + 1
         Jt(idx) = 1
         B = ZERO
         GOTO 300
      END IF

#ifdef FOR_CORSIKA
      if (LPRI.GT.4) write(LOUT,*)'DT_DIAGR: before loop 4'
#endif
 
      DO ina = 1 , Na
C cross section fluctuations
         afluc = ONE
         IF ( IFLuct.EQ.1 ) THEN
            ifluk = INT((DT_RNDM(x)+0.001D0)*1000.0D0)
            afluc = FLUixx(ifluk)
         END IF
         kk1 = 1
         kint = 1
         DO inb = 1 , Nb
C photon-projectile: check for supression by coherence length
            IF ( Ijproj.EQ.7 ) THEN
               IF ( ABS(TKOo(3,inb)-TKOo(3,kk1)).GT.dcoh ) THEN
                  kk1 = inb
                  kint = kint + 1
               END IF
            END IF
            qq1 = B + TKOo(1,inb) - PKOo(1,ina)
            qq2 = TKOo(2,inb) - PKOo(2,ina)
            xy = gam*(qq1*qq1+qq2*qq2)
            IF ( xy.LE.15.0D0 ) THEN
               c = ci - ca*afluc*EXP(-xy)
               ar = DBLE(c)
               ai = DIMAG(c)
               p = ar*ar + ai*ai
               IF ( DT_RNDM(xy).GE.p ) THEN
                  Jnt = Jnt + 1
                  IF ( Ijproj.EQ.7 ) THEN
                     jnt0(kint) = jnt0(kint) + 1
                     IF ( jnt0(kint).GT.MAXNCL ) THEN
 
                        IF ( LPRi.GT.4 ) WRITE (LOUt,99010) MAXNCL
99010                   FORMAT (1X,
     &                     'DIAGR:  no. of requested interactions',
     &                     ' exceeds array dimensions ',I4)
                        STOP
                     END IF
                     js0(kint) = js0(kint) + 1
                     jt0(kint,inb) = jt0(kint,inb) + 1
                     ji1(kint,jnt0(kint)) = ina
                     ji2(kint,jnt0(kint)) = inb
                  ELSE
                     IF ( Jnt.GT.MAXINT ) THEN
 
                        IF ( LPRi.GT.4 ) WRITE (LOUt,99020) Jnt , MAXINT
99020                   FORMAT (1X,
     &                     'DIAGR:  no. of requested interactions (',I4,
     &                     ') exceeds array dimensions (',I4,')')
                        STOP
                     END IF
                     Js(ina) = Js(ina) + 1
                     Jt(inb) = Jt(inb) + 1
                     INTer1(Jnt) = ina
                     INTer2(Jnt) = inb
                  END IF
               END IF
            END IF
         END DO
      END DO
 
      IF ( Jnt.EQ.0 ) THEN
C           WRITE(6,*) ' new impact parameter required (old= ',B,')'
         IF ( ntry.LT.500 ) GOTO 200
         GOTO 100
      END IF
 
      Idirec = 0
      IF ( Ijproj.EQ.7 ) THEN
         k = INT(ONE+DT_RNDM(x)*DBLE(kint))
 250     IF ( jnt0(k).EQ.0 ) THEN
            k = k + 1
            IF ( k.GT.kint ) k = 1
            GOTO 250
         END IF
C supress Glauber-cascade by direct photon processes
         CALL DT_POILIK(Nb,ntarg,ECMnow,sq2,ipnt,rpnt,2)
         IF ( ipnt.GT.0 ) THEN
            Jnt = 1
            Js(1) = 1
            DO inb = 1 , Nb
               Jt(inb) = jt0(k,inb)
               IF ( Jt(inb).GT.0 ) GOTO 260
            END DO
 260        INTer1(1) = 1
            INTer2(1) = inb
            Idirec = ipnt
         ELSE
            Jnt = jnt0(k)
            Js(1) = js0(k)
            DO inb = 1 , Nb
               Jt(inb) = jt0(k,inb)
            END DO
            DO i = 1 , Jnt
               INTer1(i) = ji1(k,i)
               INTer2(i) = ji2(k,i)
            END DO
         END IF
      END IF
 
 300  Inta = 0
      Intb = 0
      DO i = 1 , Na
         IF ( Js(i).NE.0 ) Inta = Inta + 1
      END DO
      DO i = 1 , Nb
         IF ( Jt(i).NE.0 ) Intb = Intb + 1
      END DO
      ICWpg = Inta
      ICWtg = Intb
      ICIg = Jnt
      IPGlb = IPGlb + Inta
      ITGlb = ITGlb + Intb
      NGLb = NGLb + 1
 
      IF ( NCOmpo.EQ.0 ) THEN
         ncall = ncall + 1
         nwa(Inta) = nwa(Inta) + 1
         nwb(Intb) = nwb(Intb) + 1
      END IF

#ifdef FOR_CORSIKA
      if (LPRI.GT.4) write(LOUT,*)'DT_DIAGR: at end'
#endif
 
      END SUBROUTINE
