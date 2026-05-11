
      SUBROUTINE DT_EVA2HE(Mo,Eexcf,Ircl,Irej)
 
C***********************************************************************
C Interface between common's of evaporation module (FKFINU,FKFHVY)     *
C and DTEVT1.                                                          *
C    MO    DTEVT1-index of "mother" (residual) nucleus before evap.    *
C    EEXCF exitation energy of residual nucleus after evaporation      *
C    IRCL  = 1 projectile residual nucleus                             *
C          = 2 target     residual nucleus                             *
C This version dated 19.04.95 is written by S. Roesler.                *
C***********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

#ifdef FOR_FLUKA
      INCLUDE '(DIMPAR)'
      INCLUDE '(GENSTK)'
      INCLUDE '(RESNUC)'
      INCLUDE '(FHEAVY)'
#else
      INCLUDE 'DIMPAR'
      INCLUDE 'GENSTK'
      INCLUDE 'RESNUC'
      INCLUDE 'FHEAVY'
#endif

      DOUBLE PRECISION am , dum , Eexcf , pe , px , py , pz , TINY10 , 
     &                 TINY3
      INTEGER i , ibtot , id , idheav , idnuc , idpdg , IDT_IPDGHA , 
     &        idum , iptokp , Ircl , Irej , iztot , Mo
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (TINY10=1.0D-10,TINY3=1.0D-3)
 
C event history
 
 
      INCLUDE 'inc/dtevt1'
C Note: DTEVT2 - special use for heavy fragments !
C       (IDRES(I) = mass number, IDXRES(I) = charge)
C extended event history
      INCLUDE 'inc/dtevt2'
C particle properties (BAMJET index convention)
      INCLUDE 'inc/dtpart'
C flags for input different options
      INCLUDE 'inc/dtflg1'
C statistics: residual nuclei
      INCLUDE 'inc/dtsta2'
C treatment of residual nuclei: properties of residual nuclei
      INCLUDE 'inc/dtrnu2'
 
      DIMENSION iptokp(39)
      DATA iptokp/1 , 2 , 3 , 4 , 5 , 6 , 7 , 8 , 9 , 10 , 11 , 12 , 
     &     13 , 14 , 15 , 16 , 17 , 18 , 19 , 20 , 21 , 22 , 23 , 24 , 
     &     25 , 26 , 27 , 28 , 29 , 30 , 99 , 100 , 101 , 97 , 102 , 
     &     98 , 103 , 109 , 115/
 
      Irej = 0
 
C skip if evaporation package is not included
 
C update counter
      IF ( .NOT.LEVapo ) RETURN
      IF ( NREsev(3).NE.NEVhkk ) THEN
         NREsev(3) = NEVhkk
         NREsev(4) = NREsev(4) + 1
      END IF
 
      IF ( LEMcck ) CALL DT_EVTEMC(PHKk(1,Mo),PHKk(2,Mo),PHKk(3,Mo),
     &     PHKk(4,Mo),1,idum,idum)
C mass number/charge of residual nucleus before evaporation
      ibtot = IDRes(Mo)
      iztot = IDXres(Mo)
 
C protons/neutrons/gammas
      DO i = 1 , NP
         px = CXR(i)*PLR(i)
         py = CYR(i)*PLR(i)
         pz = CZR(i)*PLR(i)
         id = iptokp(KPArt(i))
         idpdg = IDT_IPDGHA(id)
         am = ((PLR(i)+TKI(i))*(PLR(i)-TKI(i)))
     &        /(2.0D0*MAX(TKI(i),TINY10))
         IF ( ABS(am-AAM(id)).GT.TINY3 ) THEN
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,99010) id , am , AAM(id)
99010       FORMAT (1X,'EVA2HE:  inconsistent mass of evap. ',
     &              'particle',I3,2E10.3)
         END IF
         pe = TKI(i) + am
         CALL DT_EVTPUT(-1,idpdg,Mo,0,px,py,pz,pe,0,0,0)
         NOBam(NHKk) = Ircl
         IF ( LEMcck ) CALL DT_EVTEMC(-px,-py,-pz,-pe,2,idum,idum)
         ibtot = ibtot - IIBar(id)
         iztot = iztot - IICh(id)
      END DO
 
C heavy fragments
      DO i = 1 , NPHeav
         px = CXHeav(i)*PHEavy(i)
         py = CYHeav(i)*PHEavy(i)
         pz = CZHeav(i)*PHEavy(i)
         idheav = 80000
         am = ((PHEavy(i)+TKHeav(i))*(PHEavy(i)-TKHeav(i)))
     &        /(2.0D0*MAX(TKHeav(i),TINY10))
         pe = TKHeav(i) + am
         CALL DT_EVTPUT(-1,idheav,Mo,0,px,py,pz,pe,IBHeav(KHEavy(i)),
     &                  ICHeav(KHEavy(i)),0)
         NOBam(NHKk) = Ircl
         IF ( LEMcck ) CALL DT_EVTEMC(-px,-py,-pz,-pe,2,idum,idum)
         ibtot = ibtot - IBHeav(KHEavy(i))
         iztot = iztot - ICHeav(KHEavy(i))
      END DO
 
      IF ( IBRes.GT.0 ) THEN
C residual nucleus after evaporation
         idnuc = 80000
         CALL DT_EVTPUT(1001,idnuc,Mo,0,PXRes,PYRes,PZRes,EREs,IBRes,
     &                  ICRes,0)
         NOBam(NHKk) = Ircl
      END IF
      Eexcf = TVCms
      NTOtfi(Ircl) = IBRes
      NPRofi(Ircl) = ICRes
      IF ( LEMcck ) CALL DT_EVTEMC(-PXRes,-PYRes,-PZRes,-EREs,2,idum,
     &     idum)
      ibtot = ibtot - IBRes
      iztot = iztot - ICRes
 
C count events with fission
      NEVafi(1,Ircl) = NEVafi(1,Ircl) + 1
      IF ( LRNfss ) NEVafi(2,Ircl) = NEVafi(2,Ircl) + 1
 
C energy-momentum conservation check
C     IF (IREJ.GT.0) THEN
C        CALL DT_EVTOUT(4)
C        WRITE(*,*) EEXC(2),EEXCFI(2),NP,NPHEAV
C     ENDIF
C baryon-number/charge conservation check
      IF ( LEMcck ) CALL DT_EVTEMC(dum,dum,dum,dum,5,40,Irej)
      IF ( ibtot+iztot.NE.0 ) THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99020) NEVhkk , ibtot , iztot
99020    FORMAT (1X,'EVA2HE:   baryon-number/charge conservation ',
     &           'failure at event ',I8,' :  IBTOT,IZTOT = ',2I3)
      END IF
 
      END SUBROUTINE
