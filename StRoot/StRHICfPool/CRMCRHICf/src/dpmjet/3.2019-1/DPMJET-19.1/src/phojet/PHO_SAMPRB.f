
      SUBROUTINE PHO_SAMPRB(Ecmi,Ip,Isam,Jsam,Ksam)
C********************************************************************
C
C     routine to sample number of cut graphs of different kind
C
C     input:  IP      scattering particle combination
C             ECMI    CMS energy
C             IP      -1         initialization
C                     -2         output of statistics
C                     others     sampling of cuts
C
C     output: ISAM    number of soft Pomerons cut
C             JSAM    number of soft Reggeons cut
C             KSAM    number of hard Pomerons cut
C
C     PHO_PRBDIS has to be called before
C
C********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION averb , averc , DT_RNDM , ecmc , Ecmi , ecms1 , 
     &                 ecms2 , ecmx , fac1 , fac2 , fsupp , pacc , 
     &                 preg , pro , wgx , xi , xm
      INTEGER i , i1 , i2 , imax1 , Ip , Isam , iter , Jsam , k , klim , 
     &        Ksam
      SAVE 
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  current beam selection
      INCLUDE 'inc/pobeam'
C  general process information
      INCLUDE 'inc/poprcs'
C  nucleon-nucleus / nucleus-nucleus interface to DPMJET
      INCLUDE 'inc/pohdfl'
C  obsolete cut-off information
      INCLUDE 'inc/pocut1'
C  cut probability distribution
#ifndef FOR_CORSIKA
      INCLUDE 'inc/poprob'
#else
      INCLUDE 'inc/poprob50'
#endif
C  global event kinematics and particle IDs
      INCLUDE 'inc/pogcms'
C  cross sections
      INCLUDE 'inc/pocsec'
C  table of particle indices for recursive PHOJET calls
      INCLUDE 'inc/porecu'
C**anfe Common blocks included to provide MAXSOF and MSCAHD params
C  light-cone x fractions and c.m. momenta of soft cut string ends
      INCLUDE 'inc/posoft'
C  hard scattering data
      INCLUDE 'inc/pohslt'
 
      DIMENSION ecms1(4) , ecms2(4) , averb(0:3,4) , averc(0:3,4)
 
C  sample number of interactions
      IF ( Ip.GE.0 ) THEN
         iter = 0
         ecmx = Ecmi
         ecmc = Ecmi
         klim = 1
         IF ( (IPAmdl(13).GT.0) .AND. (IPRoce.EQ.1) .AND. (IPOix3.EQ.0)
     &        ) THEN
            IF ( IPAmdl(16).EQ.0 ) ecmc = SECm
            klim = 0
         END IF
 
C  sample up to kinematic limits only
         imax1 = MIN(IMAx,INT(0.4D0*ecmc/PARmdl(161)))
         IF ( imax1.LT.1 ) THEN
            IF ( IPAmdl(2).EQ.1 ) THEN
C  reggeon allowed
               Isam = 0
               Jsam = 1
               Ksam = 0
               averb(3,Ip) = averb(3,Ip) + 1.D0
            ELSE
C  only pomeron even at very low energies
               Isam = 1
               Jsam = 0
               Ksam = 0
               averb(1,Ip) = averb(1,Ip) + 1.D0
            END IF
            averb(0,Ip) = averb(0,Ip) + 1.D0
            GOTO 150
         END IF
C  find interpolation factors
         IF ( ecmx.LE.EPTab(1,Ip,IDXmpar) ) THEN
            i1 = 1
            i2 = 1
         ELSE IF ( ecmx.LT.EPTab(IEEmax,Ip,IDXmpar) ) THEN
            DO i = 2 , IEEmax
               IF ( ecmx.LE.EPTab(i,Ip,IDXmpar) ) GOTO 20
            END DO
 20         i1 = i - 1
            i2 = i
         ELSE
            IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,2E12.3)')
     &            'PHO_SAMPRB:too high energy' , ecmx , 
     &           EPTab(IEEmax,Ip,IDXmpar)
            CALL PHO_PREVNT(-1)
            i1 = IEEmax
            i2 = IEEmax
         END IF
         fac2 = 0.D0
         IF ( i1.NE.i2 ) fac2 = LOG(ecmx/EPTab(i1,Ip,IDXmpar))
     &        /LOG(EPTab(i2,Ip,IDXmpar)/EPTab(i1,Ip,IDXmpar))
         fac1 = 1.D0 - fac2
C  reggeon probability
         preg = -(PROb(i1,0,0,Ip,IDXmpar)*fac1+PROb(i2,0,0,Ip,IDXmpar)
     &          *fac2)
C  calculate soft suppression factor
         IF ( Ip.EQ.1 ) fsupp = PARmdl(35)
     &        **2/((PVIrt(1)+PARmdl(35))*(PVIrt(2)+PARmdl(35)))
C
 50      iter = iter + 1
         xi = DT_RNDM(fac2)
         DO Ksam = 0 , KMAx
            DO Isam = 0 , IMAx
               pro = PROb(i1,Isam,Ksam,Ip,IDXmpar)
     &               *fac1 + PROb(i2,Isam,Ksam,Ip,IDXmpar)*fac2
               IF ( pro.GT.xi ) GOTO 100
            END DO
         END DO
         Isam = MIN(IMAx,Isam)
         Ksam = MIN(KMAx,Ksam)
 
 
 100     IF ( iter.GT.100 ) THEN
 
            Isam = 0
            Jsam = 1
            Ksam = 0
            IF ( LPRi.GT.4 .AND. IDEb(12).GE.3 )
     &            WRITE (LO,'(1X,A,I10,E11.3,I6)')
     &            'PHO_SAMPRB: rejection (EV,ECM,ITER)' , KEVent , 
     &           ecmx , iter
 
         ELSE
 
C  reggeon contribution
            Jsam = 0
            IF ( IPAmdl(2).EQ.1 ) THEN
               DO i = 1 , Isam
                  IF ( DT_RNDM(pro).LT.preg ) Jsam = Jsam + 1
               END DO
               Isam = Isam - Jsam
            END IF
C  statistics of bare cuts
            IF ( iter.EQ.1 ) THEN
               averb(0,Ip) = averb(0,Ip) + 1.D0
               averb(1,Ip) = averb(1,Ip) + DBLE(Isam)
               averb(2,Ip) = averb(2,Ip) + DBLE(Ksam)
               averb(3,Ip) = averb(3,Ip) + DBLE(Jsam)
            END IF
C  limitation given by field dimensions
C**anfe  Conditional changed according to increased field sizes
            IF ( (Ksam.GT.MSCAHD) .OR. (2*Ksam+Jsam+2*Isam.GT.MAXSOF) )
     &           GOTO 50
 
            IF ( Ip.EQ.1 ) THEN
 
C  reweight according to virtualities and PDF treatment
               IF ( IPAmdl(115).GE.1 ) THEN
                  IF ( Ksam.EQ.0 ) THEN
                     IF ( FSUp(1)*FSUp(2).LT.DT_RNDM(Ecmi) ) GOTO 50
                  END IF
               END IF
 
C  reduce number of cuts according to photon virtualities
               IF ( IPAmdl(114).GE.1 ) THEN
 105              i = Isam + Jsam
                  wgx = fsupp**i
                  IF ( DT_RNDM(wgx).GT.wgx ) THEN
                     IF ( Isam+Jsam+Ksam.GT.1 ) THEN
                        IF ( Jsam.GT.0 ) THEN
                           Jsam = Jsam - 1
                           GOTO 105
                        ELSE IF ( Isam.GT.0 ) THEN
                           Isam = Isam - 1
                           GOTO 105
                        END IF
                     END IF
                  END IF
               END IF
 
            END IF
 
C  phase space limitation
 120        xm = DBLE(2*Isam+Jsam)*PARmdl(160+Ip) + DBLE(2*Ksam)
     &           *PTCut(Ip)
            pacc = EXP(PARmdl(9)*(PARmdl(160+Ip)-xm)/ecmc)
            IF ( DT_RNDM(xm).GT.pacc ) THEN
               IF ( Isam+Jsam+Ksam.GT.1 ) THEN
                  IF ( Jsam.GT.0 ) THEN
                     Jsam = Jsam - 1
                     GOTO 120
                  ELSE IF ( Isam.GT.0 ) THEN
                     Isam = Isam - 1
                     GOTO 120
                  ELSE IF ( Ksam.GT.klim ) THEN
                     Ksam = Ksam - 1
                     GOTO 120
                  END IF
               END IF
            END IF
 
         END IF
 
         Isam = Isam + Jsam/2
         Jsam = MOD(Jsam,2)
C  collect statistics
 150     ecms1(Ip) = ecms1(Ip) + ecmx
         ecms2(Ip) = ecms2(Ip) + ecmc
 
         averc(0,Ip) = averc(0,Ip) + 1.D0
         averc(1,Ip) = averc(1,Ip) + DBLE(Isam)
         averc(2,Ip) = averc(2,Ip) + DBLE(Ksam)
         averc(3,Ip) = averc(3,Ip) + DBLE(Jsam)
C
         IF ( LPRi.GT.4 .AND. IDEb(12).GE.10 )
     &         WRITE (LO,'(1X,A,2E11.4,3I4)') 'PHO_SAMPRB: ECM,I,J,K' , 
     &        ECM , ecmx , Isam , Jsam , Ksam
C
C  initialize statistics
      ELSE IF ( Ip.EQ.-1 ) THEN
         DO i = 1 , 4
            ecms1(i) = 0.D0
            ecms2(i) = 0.D0
            DO k = 0 , 3
               averb(k,i) = 0.D0
               averc(k,i) = 0.D0
            END DO
 
         END DO
         RETURN
C
C  write out statistics
      ELSE IF ( Ip.EQ.-2 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(2(/1X,A))')
     &         'PHO_SAMPRB: interaction statistics' , 
     &        '----------------------------------'
         DO i = 1 , 4
            IF ( averb(0,i).GE.2.D0 ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I3,1P,2E13.3)')
     &               'statistics for IP,<Ecm_1>,<Ecm_2>' , i , ecms1(i)
     &              /MAX(averb(0,i),1.D0) , ecms2(i)
     &              /MAX(averb(0,i),1.D0)
               IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A)')
     &               'average number of s-pom,h-pom,reg cuts (bare)'
               IF ( LPRi.GT.4 ) WRITE (LO,'(5X,F12.0,1P3E12.4)')
     &              averb(0,i) , (averb(k,i)/averb(0,i),k=1,3)
               IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A)')
     &               'average (with energy/virtuality corrections)'
               IF ( LPRi.GT.4 ) WRITE (LO,'(5X,F12.0,1P3E12.4)')
     &              averc(0,i) , (averc(k,i)/averc(0,i),k=1,3)
            END IF
 
         END DO
         RETURN
      END IF
      END SUBROUTINE
