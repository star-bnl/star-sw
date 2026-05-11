
      SUBROUTINE DT_SAMSDQ(Ecm,Idx1,Idx2,Mode,Irej)
 
C***********************************************************************
C SAMpling of Sea-DiQuarks                                             *
C              ECM        cm-energy of the nucleon-nucleon system      *
C              IDX1,2     indices of x-values of the participating     *
C                         partons (IDX2 is always the sea-q-pair to be *
C                         changed to sea-qq-pair)                      *
C              MODE       = 1  valence-q - sea-diq                     *
C                         = 2  sea-diq   - valence-q                   *
C                         = 3  sea-q     - sea-diq                     *
C                         = 4  sea-diq   - sea-q                       *
C Based on DIQVS, DIQSV, DIQSSD, DIQDSS.                               *
C This version dated 17.10.95 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION am1 , am2 , DT_RNDM , Ecm , rr1 , rr2 , rr3 , 
     &                 sr123 , xdthr , xxd , xxpsaq , xxpsq , xxpv , 
     &                 xxtsaq , xxtsq , xxtv , ZERO
      INTEGER Idx1 , Idx2 , idxsp , idxst , idxvp , idxvt , Irej , 
     &        MAXINT , MAXNCL , MAXSQU , MAXVQU , Mode
      SAVE 
 
      PARAMETER (ZERO=0.0D0)
 
C threshold values for x-sampling (DTUNUC 1.x)
      INCLUDE 'inc/dtxcut'
C various options for treatment of partons (DTUNUC 1.x)
C (chain recombination, Cronin,..)
      INCLUDE 'inc/dtchai'
 
 
      PARAMETER (MAXNCL=260,MAXVQU=MAXNCL,MAXSQU=20*MAXVQU,
     &           MAXINT=MAXVQU+MAXSQU)
C x-values of partons (DTUNUC 1.x)
      INCLUDE 'inc/dtdpmx'
C flavors of partons (DTUNUC 1.x)
      INCLUDE 'inc/dtdpmf'
C auxiliary common for x-value and flavor storage of partons (DTUNUC 1.x)
      INCLUDE 'inc/dtdpmi'
C auxiliary common for x-value and flavor storage of partons (DTUNUC 1.x)
      INCLUDE 'inc/dtdpm0'
C auxiliary common for chain system storage (DTUNUC 1.x)
      INCLUDE 'inc/dtchsy'
 
      Irej = 0
C  threshold-x for valence diquarks
      xdthr = CDQ/Ecm
 
      IF ( Mode.EQ.2 ) THEN
 
C---------------------------------------------------------------------
C proj. sea partons - targ. valence partons
C get x-values and flavors for projectile sea-diquark pair
 
         idxsp = Idx2
         idxvt = Idx1
 
C  index of corr. val-diquark-x in projectile nucleon
         idxvp = ITOvp(IFRosp(idxsp))
C  available x above diquark thresholds for valence- and sea-diquarks
         xxd = XPVd(idxvp) + XPSq(idxsp) + XPSaq(idxsp) - 3.0D0*xdthr
 
         IF ( xxd.GE.ZERO ) THEN
C  x-values for the three diquarks of the projectile nucleon
            rr1 = DT_RNDM(xxd)
            rr2 = DT_RNDM(rr1)
            rr3 = DT_RNDM(rr2)
            sr123 = rr1 + rr2 + rr3
            xxpv = xdthr + rr1*xxd/sr123
            xxpsq = xdthr + rr2*xxd/sr123
            xxpsaq = xdthr + rr3*xxd/sr123
         ELSE
            xxpv = XPVd(idxvp)
            xxpsq = XPSq(idxsp)
            xxpsaq = XPSaq(idxsp)
         END IF
C  flavor of the second quarks in the sea-diquark pair
         IPSq2(idxsp) = INT(1.0D0+DT_RNDM(xxd)*(2.0D0+SEAsq))
         IPSaq2(idxsp) = -IPSq2(idxsp)
C  check masses of the new sea-qq - val-q, sea-aqaq - val-qq chains
         am1 = xxpsq*XTVq(idxvt)*Ecm**2
         am2 = xxpsaq*XTVd(idxvt)*Ecm**2
C    ss-asas pair
         IF ( (IPSq(idxsp).EQ.3) .AND. (IPSq2(idxsp).EQ.3) .AND. 
     &        ((am2.LE.18.0D0) .OR. (am1.LE.6.6D0)) ) THEN
            Irej = 1
            RETURN
C    at least one strange quark
         ELSE IF ( ((IPSq(idxsp).EQ.3) .OR. (IPSq2(idxsp).EQ.3)) .AND. 
     &             ((am2.LE.14.6D0) .OR. (am1.LE.5.8D0)) ) THEN
            Irej = 1
            RETURN
         ELSE IF ( (am2.LE.13.4D0) .OR. (am1.LE.5.0D0) ) THEN
            Irej = 1
            RETURN
         END IF
C  accept the new sea-diquark
         XPVd(idxvp) = xxpv
         XPSq(idxsp) = xxpsq
         XPSaq(idxsp) = xxpsaq
         NDV = NDV + 1
         INTdv1(NDV) = idxsp
         INTdv2(NDV) = idxvt
         ISKpch(5,NDV) = 0
         RETURN
      ELSE IF ( Mode.EQ.3 ) THEN
 
C---------------------------------------------------------------------
C proj. sea partons - targ. sea partons
C get x-values and flavors for target sea-diquark pair
 
         idxsp = Idx1
         idxst = Idx2
 
C  index of corr. val-diquark-x in target nucleon
         idxvt = ITOvt(IFRost(idxst))
C  available x above diquark thresholds for valence- and sea-diquarks
         xxd = XTVd(idxvt) + XTSq(idxst) + XTSaq(idxst) - 3.0D0*xdthr
 
         IF ( xxd.GE.ZERO ) THEN
C  x-values for the three diquarks of the target nucleon
            rr1 = DT_RNDM(xxd)
            rr2 = DT_RNDM(rr1)
            rr3 = DT_RNDM(rr2)
            sr123 = rr1 + rr2 + rr3
            xxtv = xdthr + rr1*xxd/sr123
            xxtsq = xdthr + rr2*xxd/sr123
            xxtsaq = xdthr + rr3*xxd/sr123
         ELSE
            xxtv = XTVd(idxvt)
            xxtsq = XTSq(idxst)
            xxtsaq = XTSaq(idxst)
         END IF
C  flavor of the second quarks in the sea-diquark pair
         ITSq2(idxst) = INT(1.0D0+DT_RNDM(xxd)*(2.0D0+SEAsq))
         ITSaq2(idxst) = -ITSq2(idxst)
C  check masses of the new sea-q - sea-qq, sea-aq - sea-aqaq chains
         am1 = xxtsq*XPSq(idxsp)*Ecm**2
         am2 = xxtsaq*XPSaq(idxsp)*Ecm**2
C    ss-asas pair
         IF ( (ITSq(idxst).EQ.3) .AND. (ITSq2(idxst).EQ.3) .AND. 
     &        ((am2.LE.6.6D0) .OR. (am1.LE.6.6D0)) ) THEN
            Irej = 1
            RETURN
C    at least one strange quark
         ELSE IF ( ((ITSq(idxst).EQ.3) .OR. (ITSq2(idxst).EQ.3)) .AND. 
     &             ((am2.LE.5.8D0) .OR. (am1.LE.5.8D0)) ) THEN
            Irej = 1
            RETURN
         ELSE IF ( (am2.LE.5.0D0) .OR. (am1.LE.5.0D0) ) THEN
            Irej = 1
            RETURN
         END IF
C  accept the new sea-diquark
         XTVd(idxvt) = xxtv
         XTSq(idxst) = xxtsq
         XTSaq(idxst) = xxtsaq
         NSD = NSD + 1
         INTsd1(NSD) = idxsp
         INTsd2(NSD) = idxst
         ISKpch(3,NSD) = 0
         RETURN
      ELSE IF ( Mode.EQ.4 ) THEN
 
C---------------------------------------------------------------------
C proj. sea partons - targ. sea partons
C get x-values and flavors for projectile sea-diquark pair
 
         idxsp = Idx2
         idxst = Idx1
 
C  index of corr. val-diquark-x in projectile nucleon
         idxvp = ITOvp(IFRosp(idxsp))
C  available x above diquark thresholds for valence- and sea-diquarks
         xxd = XPVd(idxvp) + XPSq(idxsp) + XPSaq(idxsp) - 3.0D0*xdthr
 
         IF ( xxd.GE.ZERO ) THEN
C  x-values for the three diquarks of the projectile nucleon
            rr1 = DT_RNDM(xxd)
            rr2 = DT_RNDM(rr1)
            rr3 = DT_RNDM(rr2)
            sr123 = rr1 + rr2 + rr3
            xxpv = xdthr + rr1*xxd/sr123
            xxpsq = xdthr + rr2*xxd/sr123
            xxpsaq = xdthr + rr3*xxd/sr123
         ELSE
            xxpv = XPVd(idxvp)
            xxpsq = XPSq(idxsp)
            xxpsaq = XPSaq(idxsp)
         END IF
C  flavor of the second quarks in the sea-diquark pair
         IPSq2(idxsp) = INT(1.0D0+DT_RNDM(rr3)*(2.0D0+SEAsq))
         IPSaq2(idxsp) = -IPSq2(idxsp)
C  check masses of the new sea-qq - sea-q, sea-aqaq - sea-qq chains
         am1 = xxpsq*XTSq(idxst)*Ecm**2
         am2 = xxpsaq*XTSaq(idxst)*Ecm**2
C    ss-asas pair
         IF ( (IPSq(idxsp).EQ.3) .AND. (IPSq2(idxsp).EQ.3) .AND. 
     &        ((am2.LE.6.6D0) .OR. (am1.LE.6.6D0)) ) THEN
            Irej = 1
            RETURN
C    at least one strange quark
         ELSE IF ( ((IPSq(idxsp).EQ.3) .OR. (IPSq2(idxsp).EQ.3)) .AND. 
     &             ((am2.LE.5.8D0) .OR. (am1.LE.5.8D0)) ) THEN
            Irej = 1
            RETURN
         ELSE IF ( (am2.LE.5.0D0) .OR. (am1.LE.5.0D0) ) THEN
            Irej = 1
            RETURN
         END IF
C  accept the new sea-diquark
         XPVd(idxvp) = xxpv
         XPSq(idxsp) = xxpsq
         XPSaq(idxsp) = xxpsaq
         NDS = NDS + 1
         INTds1(NDS) = idxsp
         INTds2(NDS) = idxst
         ISKpch(2,NDS) = 0
         GOTO 99999
      END IF
 
C---------------------------------------------------------------------
C proj. valence partons - targ. sea partons
C get x-values and flavors for target sea-diquark pair
 
      idxvp = Idx1
      idxst = Idx2
 
C  index of corr. val-diquark-x in target nucleon
      idxvt = ITOvt(IFRost(idxst))
C  available x above diquark thresholds for valence- and sea-diquarks
      xxd = XTVd(idxvt) + XTSq(idxst) + XTSaq(idxst) - 3.0D0*xdthr
 
      IF ( xxd.GE.ZERO ) THEN
C  x-values for the three diquarks of the target nucleon
         rr1 = DT_RNDM(xxd)
         rr2 = DT_RNDM(rr1)
         rr3 = DT_RNDM(rr2)
         sr123 = rr1 + rr2 + rr3
         xxtv = xdthr + rr1*xxd/sr123
         xxtsq = xdthr + rr2*xxd/sr123
         xxtsaq = xdthr + rr3*xxd/sr123
      ELSE
         xxtv = XTVd(idxvt)
         xxtsq = XTSq(idxst)
         xxtsaq = XTSaq(idxst)
      END IF
C  flavor of the second quarks in the sea-diquark pair
      ITSq2(idxst) = INT(1.0D0+DT_RNDM(rr3)*(2.0D0+SEAsq))
      ITSaq2(idxst) = -ITSq2(idxst)
C  check masses of the new val-q - sea-qq, val-qq - sea-aqaq chains
      am1 = xxtsq*XPVq(idxvp)*Ecm**2
      am2 = xxtsaq*XPVd(idxvp)*Ecm**2
C    ss-asas pair
      IF ( (ITSq(idxst).EQ.3) .AND. (ITSq2(idxst).EQ.3) .AND. 
     &     ((am2.LE.18.0D0) .OR. (am1.LE.6.6D0)) ) THEN
         Irej = 1
         RETURN
C    at least one strange quark
      ELSE IF ( ((ITSq(idxst).EQ.3) .OR. (ITSq2(idxst).EQ.3)) .AND. 
     &          ((am2.LE.14.6D0) .OR. (am1.LE.5.8D0)) ) THEN
         Irej = 1
         RETURN
      ELSE IF ( (am2.LE.13.4D0) .OR. (am1.LE.5.0D0) ) THEN
         Irej = 1
         RETURN
      END IF
C  accept the new sea-diquark
      XTVd(idxvt) = xxtv
      XTSq(idxst) = xxtsq
      XTSaq(idxst) = xxtsaq
      NVD = NVD + 1
      INTvd1(NVD) = idxvp
      INTvd2(NVD) = idxst
      ISKpch(7,NVD) = 0
99999 END SUBROUTINE
