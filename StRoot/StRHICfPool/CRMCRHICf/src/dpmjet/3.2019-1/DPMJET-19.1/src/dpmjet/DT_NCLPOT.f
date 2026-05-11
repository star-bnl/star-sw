
      SUBROUTINE DT_NCLPOT(Ipz,Ip,Itz,It,Aferp,Afert,Mode)
 
C***********************************************************************
C Calculation of Coulomb and nuclear potential for a given configurat. *
C               IPZ, IP       charge/mass number of proj.              *
C               ITZ, IT       charge/mass number of targ.              *
C               AFERP,AFERT   factors modifying proj./target pot.      *
C                             if =0, FERMOD is used                    *
C               MODE = 0      calculation of binding energy            *
C                    = 1      pre-calculated binding energy is used    *
C This version dated 16.11.95  is written by S. Roesler.               *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION Aferp , Afert , aip , aipz , ait , aitz , an , 
     &                 bip , bipz , bit , bitz , EXMSAZ , fermip , 
     &                 fermit , ONE , TINY10 , TINY2 , TINY3 , ZERO
      INTEGER i , idxpot , Ip , Ipz , It , Itz , izdum , Mode
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ZERO=0.0D0,ONE=1.0D0,TINY3=1.0D-3,TINY2=1.0D-2,
     &           TINY10=1.0D-10)
 
      LOGICAL lstart
 
C particle properties (BAMJET index convention)
      INCLUDE 'inc/dtpart'
C nuclear potential
      INCLUDE 'inc/dtnpot'
 
      DIMENSION idxpot(14)
C                   ap   an  lam  alam sig- sig+ sig0 tet0 tet- asig-
C                 asig0 asig+ atet0 atet+
      DATA idxpot/2 , 9 , 17 , 18 , 20 , 21 , 22 , 97 , 98 , 99 , 100 , 
     &     101 , 102 , 103/
 
      DATA an/0.4D0/
      DATA lstart/.TRUE./
 
      IF ( Mode.EQ.0 ) THEN
         EBIndp(1) = ZERO
         EBIndn(1) = ZERO
         EBIndp(2) = ZERO
         EBIndn(2) = ZERO
      END IF
      aip = DBLE(Ip)
      aipz = DBLE(Ipz)
      ait = DBLE(It)
      aitz = DBLE(Itz)
 
      fermip = Aferp
      IF ( Aferp.LE.ZERO ) fermip = FERmod
      fermit = Afert
      IF ( Afert.LE.ZERO ) fermit = FERmod
 
C Fermi momenta and binding energy for projectile
      IF ( (Ip.GT.1) .AND. LFErmi ) THEN
         IF ( Mode.EQ.0 ) THEN
C           EBINDP(1) = DT_EBIND(IP,IPZ)-DT_EBIND(IP-1,IPZ-1)
C           EBINDN(1) = DT_EBIND(IP,IPZ)-DT_EBIND(IP-1,IPZ)
            bip = aip - ONE
            bipz = aipz - ONE
C  A.F.
C           EBINDP(1) = 1.0D-3*(ENERGY(ONE,ONE)+ENERGY(BIP,BIPZ)
C    &                                          -ENERGY(AIP,AIPZ))
            EBIndp(1) = 1.0D-3*(EXMSAZ(ONE,ONE,.TRUE.,izdum)+EXMSAZ(bip,
     &                  bipz,.TRUE.,izdum)-EXMSAZ(aip,aipz,.TRUE.,izdum)
     &                  )
 
            IF ( aip.LE.aipz ) THEN
               EBIndn(1) = EBIndp(1)
 
               IF ( LPRi.GT.10 ) WRITE (LOUt,*)
     &               ' DT_NCLPOT: AIP.LE.AIPZ (' , aip , aipz , ')'
            ELSE
 
C  A.F.
C              EBINDN(1) = 1.0D-3*(ENERGY(ONE,ZERO)+ENERGY(BIP,AIPZ)
C    &                                             -ENERGY(AIP,AIPZ))
               EBIndn(1) = 1.0D-3*(EXMSAZ(ONE,ZERO,.TRUE.,izdum)+EXMSAZ(
     &                     bip,aipz,.TRUE.,izdum)
     &                     -EXMSAZ(aip,aipz,.TRUE.,izdum))
 
            END IF
         END IF
         PFErmp(1) = fermip*an*(aipz/aip)**0.333333D0
         PFErmn(1) = fermip*an*((aip-aipz)/aip)**0.33333D0
      ELSE
         PFErmp(1) = ZERO
         PFErmn(1) = ZERO
      END IF
C effective nuclear potential for projectile
C     EPOT(1,1) = PFERMP(1)**2/(2.0D0*AAM(1)) + EBINDP(1)
C     EPOT(1,8) = PFERMN(1)**2/(2.0D0*AAM(8)) + EBINDN(1)
      EPOt(1,1) = SQRT(PFErmp(1)**2+AAM(1)**2) - AAM(1) + EBIndp(1)
      EPOt(1,8) = SQRT(PFErmn(1)**2+AAM(8)**2) - AAM(8) + EBIndn(1)
 
C Fermi momenta and binding energy for target
      IF ( (It.GT.1) .AND. LFErmi ) THEN
         IF ( Mode.EQ.0 ) THEN
C           EBINDP(2) = DT_EBIND(IT,ITZ)-DT_EBIND(IT-1,ITZ-1)
C           EBINDN(2) = DT_EBIND(IT,ITZ)-DT_EBIND(IT-1,ITZ)
            bit = ait - ONE
            bitz = aitz - ONE
C  A.F.
C           EBINDP(2) = 1.0D-3*(ENERGY(ONE,ONE)+ENERGY(BIT,BITZ)
C    &                                         -ENERGY(AIT,AITZ))
            EBIndp(2) = 1.0D-3*(EXMSAZ(ONE,ONE,.TRUE.,izdum)+EXMSAZ(bit,
     &                  bitz,.TRUE.,izdum)-EXMSAZ(ait,aitz,.TRUE.,izdum)
     &                  )
 
            IF ( ait.LE.aitz ) THEN
               EBIndn(2) = EBIndp(2)
 
               IF ( LPRi.GT.4 ) WRITE (LOUt,*)
     &               ' DT_NCLPOT: AIT.LE.AITZ (' , ait , aitz , ')'
            ELSE
C  A.F.
C              EBINDN(2) = 1.0D-3*(ENERGY(ONE,ZERO)+ENERGY(BIT,AITZ)
C    &                                             -ENERGY(AIT,AITZ))
               EBIndn(2) = 1.0D-3*(EXMSAZ(ONE,ZERO,.TRUE.,izdum)+EXMSAZ(
     &                     bit,aitz,.TRUE.,izdum)
     &                     -EXMSAZ(ait,aitz,.TRUE.,izdum))
 
            END IF
         END IF
         PFErmp(2) = fermit*an*(aitz/ait)**0.333333D0
         PFErmn(2) = fermit*an*((ait-aitz)/ait)**0.33333D0
      ELSE
         PFErmp(2) = ZERO
         PFErmn(2) = ZERO
      END IF
C effective nuclear potential for target
C     EPOT(2,1) = PFERMP(2)**2/(2.0D0*AAM(1)) + EBINDP(2)
C     EPOT(2,8) = PFERMN(2)**2/(2.0D0*AAM(8)) + EBINDN(2)
      EPOt(2,1) = SQRT(PFErmp(2)**2+AAM(1)**2) - AAM(1) + EBIndp(2)
      EPOt(2,8) = SQRT(PFErmn(2)**2+AAM(8)**2) - AAM(8) + EBIndn(2)
 
      DO i = 1 , 14
         EPOt(1,idxpot(i)) = EPOt(1,8)
         EPOt(2,idxpot(i)) = EPOt(2,8)
      END DO
 
C Coulomb energy
      ETAcou(1) = ZERO
      ETAcou(2) = ZERO
      IF ( ICOul.EQ.1 ) THEN
         IF ( Ip.GT.1 ) ETAcou(1) = 0.001116D0*aipz/(1.0D0+aip**0.333D0)
         IF ( It.GT.1 ) ETAcou(2) = 0.001116D0*aitz/(1.0D0+ait**0.333D0)
      END IF
 
      IF ( lstart ) THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99010) Ip , Ipz , It , Itz , 
     &        EBIndp , EBIndn , EPOt(1,1) - EBIndp(1) , EPOt(2,1)
     &        - EBIndp(2) , EPOt(1,8) - EBIndn(1) , EPOt(2,8)
     &        - EBIndn(2) , FERmod , ETAcou
99010    FORMAT (/,/,1X,'NCLPOT:    quantities for inclusion of nuclear'
     &           ,' effects',/,12X,'---------------------------',
     &           '----------------',/,/,38X,'projectile','      target',
     &           /,/,1X,'Mass number / charge',17X,I3,' /',I3,6X,I3,
     &           ' /',I3,/,1X,'Binding energy  -',' proton   (GeV) ',
     &           2E14.4,/,17X,'- neutron  (GeV)',1X,2E14.4,/,1X,
     &           'Fermi-potential - proton   (GeV)',1X,2E14.4,/,17X,
     &           '- neutron  (GeV) ',2E14.4,/,/,1X,
     &           'Scale factor for Fermi-momentum    ',F4.2,/,/,1X,
     &           'Coulomb-energy ',2(E14.4,' GeV  '),/,/)
         lstart = .FALSE.
      END IF
 
      END SUBROUTINE
