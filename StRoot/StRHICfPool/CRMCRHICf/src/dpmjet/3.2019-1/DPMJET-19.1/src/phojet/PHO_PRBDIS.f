
      SUBROUTINE PHO_PRBDIS(Ip,Ecm,Ie)
C*********************************************************************
C
C     calculation of multi interactions probabilities
C
C     input:  IP        particle combination to scatter
C             ECM       CMS energy
C             IE        index for weight storing
C             /PROBAB/
C             IMAX      max. number of soft pomeron interactions
C             KMAX      max. number of hard pomeron interactions
C
C     output: /PROBAB/
C             PROB      field of probabilities
C
C*********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION ab , abstmp , absum2 , ampcof , auxfac , averii , 
     &                 averj , b24 , bmax , chi2 , chifac , chitmp , 
     &                 chksum , dtmp , Ecm , elast , EPS , fac , 
     &                 faclog , pchain
      DOUBLE PRECISION phard , phsum_1 , phsum_2 , proneg , psoft , 
     &                 pssum_1 , pssum_2 , sigmi , sigmk , sigml , 
     &                 sigmm , tmp , wght , xpnt
      INTEGER i , ib , ICHMAX , idxmlast , Ie , indx , init , Ip , 
     &        iplast , j , k , kd , l
      SAVE 
 
      PARAMETER (EPS=1.D-10)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  Reggeon phenomenology parameters
      INCLUDE 'inc/popreg'
C  parameters of 2x2 channel model
      INCLUDE 'inc/po2cha'
C  Born graph cross sections and slopes
      INCLUDE 'inc/posbrn'
C  obsolete cut-off information
      INCLUDE 'inc/pocut1'
C  Born graph cross sections after applying diffraction model
      INCLUDE 'inc/point1'
C  cross sections
      INCLUDE 'inc/pocsec'
C  current beam selection
      INCLUDE 'inc/pobeam'
C  cut probability distribution
#ifndef FOR_CORSIKA
      INCLUDE 'inc/poprob'
#else
      INCLUDE 'inc/poprob50'
#endif
C  energy-interpolation table
      INCLUDE 'inc/potabl'
C  average number of cut soft and hard ladders (obsolete)
      INCLUDE 'inc/point2'
C  some constants
      INCLUDE 'inc/pocons'
C  integration precision for hard cross sections (obsolete)
      INCLUDE 'inc/pogaup'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  unitarized amplitudes for different diffraction channels
      INCLUDE 'inc/point5'
 
C  local variables
      DIMENSION ab(4,4) , chi2(4) , absum2(4,4) , abstmp(4) , chitmp(4)
C**anfe Increased ICHMAX from 40 -> 100
      PARAMETER (ICHMAX=100)
      DIMENSION chifac(4,4) , ampcof(4)
      DIMENSION pchain(2,ICHMAX) , xpnt(96) , wght(96)
      DIMENSION faclog(0:KKMAX) , psoft(0:IIMAX) , phard(0:KKMAX)
 
C  combinatorical factors
      DATA chifac/1.D0 , 1.D0 , -1.D0 , -1.D0 , 1.D0 , -1.D0 , 1.D0 , 
     &     -1.D0 , 1.D0 , -1.D0 , -1.D0 , 1.D0 , 1.D0 , 1.D0 , 1.D0 , 
     &     1.D0/
 
      DATA elast/0.D0/
      DATA iplast/0/
      DATA idxmlast/0/
      DATA init/0/
 
      IF ( init.EQ.0 ) THEN
         faclog(0) = 0.D0
         DO i = 1 , KKMAX
            faclog(i) = LOG(DBLE(i))
         END DO
         init = 1
      END IF
C  test for redundant calculation: skip cs calculation
      IF ( (Ecm.NE.elast) .OR. (Ip.NE.iplast) .OR. (IDXmpar.NE.idxmlast)
     &     ) THEN
         elast = Ecm
         iplast = Ip
         idxmlast = IDXmpar
         CALL PHO_XSECT(Ip,0,elast)
         ISImax(IDXmpar) = Ie
         SIGecm(Ie,Ip,IDXmpar) = Ecm
         SIGtab(1,Ie,Ip,IDXmpar) = SIGtot
         SIGtab(2,Ie,Ip,IDXmpar) = SIGela
         j = 2
         DO i = 0 , 4
            DO k = 0 , 4
               j = j + 1
               SIGtab(j,Ie,Ip,IDXmpar) = SIGvm(i,k)
            END DO
         END DO
         SIGtab(28,Ie,Ip,IDXmpar) = SIGine
         SIGtab(29,Ie,Ip,IDXmpar) = SIGdir
         SIGtab(30,Ie,Ip,IDXmpar) = SIGlsd(1)
         SIGtab(31,Ie,Ip,IDXmpar) = SIGlsd(2)
         SIGtab(32,Ie,Ip,IDXmpar) = SIGhsd(1)
         SIGtab(33,Ie,Ip,IDXmpar) = SIGhsd(2)
         SIGtab(34,Ie,Ip,IDXmpar) = SIGldd
         SIGtab(35,Ie,Ip,IDXmpar) = SIGhdd
         SIGtab(36,Ie,Ip,IDXmpar) = SIGcdf(0)
         SIGtab(37,Ie,Ip,IDXmpar) = SIG1so
         SIGtab(38,Ie,Ip,IDXmpar) = SIG1ha
         SIGtab(39,Ie,Ip,IDXmpar) = SLOel
         j = 39
         DO i = 1 , 4
            DO k = 1 , 4
               j = j + 1
               SIGtab(j,Ie,Ip,IDXmpar) = SLOvm(i,k)
            END DO
         END DO
         SIGtab(56,Ie,Ip,IDXmpar) = SIGpom
         SIGtab(57,Ie,Ip,IDXmpar) = SIGreg
         SIGtab(58,Ie,Ip,IDXmpar) = SIGhar
         SIGtab(59,Ie,Ip,IDXmpar) = SIGdir
         SIGtab(60,Ie,Ip,IDXmpar) = SIGtr1(1)
         SIGtab(61,Ie,Ip,IDXmpar) = SIGtr1(2)
         SIGtab(62,Ie,Ip,IDXmpar) = SIGtr2(1)
         SIGtab(63,Ie,Ip,IDXmpar) = SIGtr2(2)
         SIGtab(64,Ie,Ip,IDXmpar) = SIGloo
         SIGtab(65,Ie,Ip,IDXmpar) = SIGdpo(1)
         SIGtab(66,Ie,Ip,IDXmpar) = SIGdpo(2)
         SIGtab(67,Ie,Ip,IDXmpar) = SIGdpo(3)
         SIGtab(68,Ie,Ip,IDXmpar) = SIGdpo(4)
C  consistency check
         SIGndf = SIGtot - SIGela - SIGvm(0,0) - SIGcdf(0) - SIGdir - 
     &            SIGlsd(1) - SIGhsd(1) - SIGlsd(2) - SIGhsd(2)
     &            - SIGldd - SIGhdd
         IF ( SIGndf.LE.0.D0 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(//1X,A,/)') 
     &          'PHO_PRBDIS:ERROR: neg.cross section for unitarization!'
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I3,1P,2E12.4)')
     &            'PHO_PRBDIS: IP,ECM,SIGNDF:' , Ip , Ecm , SIGndf
            IF ( LPRi.GT.4 ) WRITE (LO,'(4X,A,/1P,8E10.3)') 
     & '(SIGTOT,SIGELA,SIGVM,SIGCDF,SIGDIR,SIGLSD(1),SIGLSD(2),SIGLDD):'
     & , SIGtot , SIGela , SIGvm(0,0) , SIGcdf(0) , SIGdir , SIGlsd(1) , 
     & SIGlsd(2) , SIGldd
            STOP
         END IF
         IF ( (IDEb(55).GE.2) .AND. (Ip.EQ.1) ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,*)
     &            '------------------------------------------------'
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'IP,ECM:' , Ip , Ecm
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'SIGTOT:' , SIGtot
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'SIGELA:' , SIGela
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'SIGVM :' , SIGvm(0,0)
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'SIGCDF:' , SIGcdf(0)
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'SIGDIR:' , SIGdir
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'SIGLSD:' , SIGlsd
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'SIGHSD:' , SIGhsd
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'SIGLDD:' , SIGldd
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'SIGHDD:' , SIGhdd
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'SIGNDF:' , SIGndf
 
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'SIGPOM:' , SIGpom
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'SIGREG:' , SIGreg
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'SIGHAR:' , SIGhar
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'SIGDIR:' , SIGdir
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'SIGTR1:' , SIGtr1
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'SIGTR2:' , SIGtr2
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'SIGLOO:' , SIGloo
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'SIGDPO:' , SIGdpo
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'SIG1SO:' , SIG1so
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'SIG1HA:' , SIG1ha
         END IF
         SIGtab(77,Ie,Ip,IDXmpar) = PTCut(Ip)
         SIGtab(78,Ie,Ip,IDXmpar) = SIGndf
         auxfac = PI2/SIGndf
         IF ( ISWmdl(1).EQ.3 ) THEN
            DO i = 1 , 4
               ampcof(i) = 0.D0
               DO k = 1 , 4
                  ampcof(i) = ampcof(i) + 0.25D0*ELAfac(k)*chifac(k,i)
               END DO
               ampcof(i) = ampcof(i)*auxfac
            END DO
         END IF
C
C       BMAX=5.D0*SQRT(DBLE(BPOM))
         bmax = 10.D0
         EPTab(Ie,Ip,IDXmpar) = Ecm
         CALL PHO_GAUSET(0.D0,bmax,NGAuso,xpnt,wght)
C
      END IF
C
      DO k = 0 , KMAx
         DO i = 0 , IMAx
            PROb(Ie,i,k,Ip,IDXmpar) = 0.D0
         END DO
      END DO
      DO i = 1 , ICHMAX
         pchain(1,i) = 0.D0
         pchain(2,i) = 0.D0
      END DO
C
C  main cross section loop
C**********************************************************
      DO ib = 1 , NGAuso
         b24 = xpnt(ib)**2/4.D0
         fac = xpnt(ib)*wght(ib)
C
         IF ( (ISWmdl(1).EQ.3) .OR. (ISWmdl(1).EQ.4) ) THEN
C
C  amplitude construction
            DO i = 1 , 4
               ab(1,i) = ZXP(1,i)*EXP(-b24/BXP(1,i)) + ZXR(1,i)
     &                   *EXP(-b24/BXR(1,i))
               ab(2,i) = ZXH(1,i)*EXP(-b24/BXH(1,i))
               ab(3,i) = -ZXT1a(1,i)*EXP(-b24/BXT1a(1,i)) - ZXT1b(1,i)
     &                   *EXP(-b24/BXT1b(1,i)) - ZXT2a(1,i)
     &                   *EXP(-b24/BXT2a(1,i)) - ZXT2b(1,i)
     &                   *EXP(-b24/BXT2b(1,i)) - ZXL(1,i)
     &                   *EXP(-b24/BXL(1,i))
               ab(4,i) = ZXDpe(1,i)*EXP(-b24/BXDpe(1,i)) + ZXDpa(1,i)
     &                   *EXP(-b24/BXDpa(1,i)) + ZXDpb(1,i)
     &                   *EXP(-b24/BXDpb(1,i)) + ZXDpd(1,i)
     &                   *EXP(-b24/BXDpd(1,i))
               ab(1,i) = ab(1,i) + ab(3,i) + ab(4,i)
               ab(2,i) = ab(2,i)
               ab(3,i) = 0.D0
               ab(4,i) = 0.D0
C
            END DO
C
            DO i = 1 , 4
               DO k = 1 , 4
                  absum2(i,k) = 0.D0
                  DO l = 1 , 4
                     absum2(i,k) = absum2(i,k) + chifac(l,k)*ab(i,l)
                  END DO
                  absum2(i,k) = 2.D0*absum2(i,k)
               END DO
            END DO
            DO i = 1 , 4
               chi2(i) = 0.D0
               DO k = 1 , 4
                  chi2(i) = chi2(i) + absum2(k,i)
               END DO
            END DO
C  sums instead of products
            DO i = 1 , 4
               DO kd = 1 , 4
                  dtmp = ABS(absum2(i,kd))
                  IF ( dtmp.LT.1.D-15 ) THEN
                     absum2(i,kd) = -50.D0
                  ELSE
                     absum2(i,kd) = LOG(dtmp)
                  END IF
               END DO
            END DO
C **anfe   Conditional changed to reflect increase in array size
            IF ( (IMAx.GT.IIMAX) .OR. (KMAx.GT.KKMAX) ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,'(1X,2A,3I6)')
     &               'PHO_PRBDIS: internal field ' , 
     &              'dimensions too small (IMAX,KMAX,IIMAX,KKMAX):' , 
     &              IMAx , KMAx , IIMAX , KKMAX
               CALL PHO_ABORT
            END IF
            DO kd = 1 , 4
               DO i = 1 , 4
                  abstmp(i) = absum2(i,kd)
               END DO
C  recursive sum
               chitmp(1) = -absum2(1,kd)
               DO i = 0 , IMAx
                  chitmp(1) = chitmp(1) + abstmp(1) - faclog(i)
                  chitmp(2) = -abstmp(2)
                  DO k = 0 , KMAx
                     chitmp(2) = chitmp(2) + abstmp(2) - faclog(k)
C  calculation of elastic part
                     dtmp = -chi2(kd) + chitmp(1) + chitmp(2)
                     IF ( dtmp.LT.-30.D0 ) THEN
                        dtmp = 0.D0
                     ELSE
                        dtmp = EXP(dtmp)*fac*ampcof(kd)
                     END IF
                     PROb(Ie,i,k,Ip,IDXmpar) = PROb(Ie,i,k,Ip,IDXmpar)
     &                  + dtmp
                  END DO
               END DO
            END DO
            PROb(Ie,0,0,Ip,IDXmpar) = 0.D0
C
C**********************************************************
         ELSE
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I3)')
     &            'PHO_PRBDIS:ERROR: invalid setting of ISWMDL(1)' , 
     &           ISWmdl(1)
            STOP
         END IF
      END DO
 
C  debug output
      IF ( IDEb(55).GE.15 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(/,1X,A,I3,E11.4)')
     &         'PHO_PRBDIS: list of probabilities (uncorrected,IP,ECM)'
     &        , Ip , Ecm
         DO i = 0 , MIN(IMAx,5)
            DO k = 0 , MIN(KMAx,5)
               IF ( ABS(PROb(Ie,i,k,Ip,IDXmpar)).GT.1.D-10 .AND. 
     &              LPRi.GT.4 ) WRITE (LO,'(10X,2I3,5X,E15.8)') i , k , 
     &              PROb(Ie,i,k,Ip,IDXmpar)
            END DO
         END DO
      END IF
C  string probability (uncorrected)
      IF ( IDEb(55).GE.5 ) THEN
         DO i = 0 , IMAx
            DO k = 0 , KMAx
               indx = 2*i + 2*k
               IF ( (indx.LE.ICHMAX) .AND. (indx.GT.0) ) pchain(1,indx)
     &              = pchain(1,indx) + PROb(Ie,i,k,Ip,IDXmpar)
            END DO
         END DO
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,2A,E11.4)') 'PHO_PRBDIS: ' , 
     &        'list of selected probabilities (uncorr,ECM)' , Ecm
         IF ( LPRi.GT.4 ) WRITE (LO,'(10X,A)')
     &         'I,   0HPOM,   1HPOM,   2HPOM'
         DO i = 0 , IIMAX
            IF ( ABS(PROb(Ie,i,0,Ip,IDXmpar)).GT.1.D-10 .AND. 
     &           LPRi.GT.4 ) WRITE (LO,'(5X,I4,3E12.4)') i , 
     &           PROb(Ie,i,0,Ip,IDXmpar) , PROb(Ie,i,1,Ip,IDXmpar) , 
     &           PROb(Ie,i,2,Ip,IDXmpar)
         END DO
      END IF
C  substract high-mass single and double diffraction
      PROb(Ie,1,0,Ip,IDXmpar) = PROb(Ie,1,0,Ip,IDXmpar)
     &   - (SIGhsd(1)+SIGhsd(2)+SIGhdd+SIGcdf(0))/SIGndf
      PROb(Ie,1,0,Ip,IDXmpar) = MAX(0.01,PROb(Ie,1,0,Ip,IDXmpar))
C
C  probability check
      chksum = 0.D0
      proneg = 0.D0
      AVEri = 0.D0
      AVErk = 0.D0
      AVErl = 0.D0
      AVErm = 0.D0
      AVErn = 0.D0
      sigmi = 0.D0
      sigmk = 0.D0
      sigml = 0.D0
      sigmm = 0.D0
      DO i = 0 , IMAx
         psoft(i) = 0.D0
      END DO
      DO k = 0 , KMAx
         phard(k) = 0.D0
      END DO
      DO k = 0 , KMAx
         DO i = 0 , IMAx
            tmp = PROb(Ie,i,k,Ip,IDXmpar)
            IF ( tmp.LT.0.D0 ) THEN
               IF ( (IDEb(55).GE.0) .AND. (tmp.LT.-EPS) ) THEN
                  IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,4I4,E14.4)')
     &                  'PHO_PRBDIS: neg.probability:' , Ip , Ie , i , 
     &                 k , PROb(Ie,i,k,Ip,IDXmpar)
               END IF
               proneg = proneg + tmp
               tmp = 0.D0
            END IF
            chksum = chksum + tmp
            AVEri = AVEri + DBLE(i)*tmp
            AVErk = AVErk + DBLE(k)*tmp
            sigmi = sigmi + DBLE(i**2)*tmp
            sigmk = sigmk + DBLE(k**2)*tmp
            psoft(i) = psoft(i) + PROb(Ie,i,k,Ip,IDXmpar)
            phard(k) = phard(k) + PROb(Ie,i,k,Ip,IDXmpar)
            PROb(Ie,i,k,Ip,IDXmpar) = chksum
         END DO
      END DO
C
      IF ( LPRi.GT.4 .AND. IDEb(55).GE.1 ) WRITE (LO,'(/,1X,A,2E15.6)')
     &      'PHO_PRBDIS: first sum of probabilities' , chksum , proneg
C  cut probabilites output
      IF ( IDEb(55).GE.5 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A)')
     &         'list of cut probabilities (uncorr/corr)'
         DO i = 1 , ICHMAX
            IF ( ABS(pchain(1,i)).GT.1.D-10 .AND. LPRi.GT.4 )
     &            WRITE (LO,'(5X,I4,2E12.3)') i , pchain(1,i) , 
     &           pchain(1,i)/chksum
         END DO
      END IF
C  rescaling necessary
      IF ( ABS(chksum-1.D0).GT.1.D-15 ) THEN
         fac = 1.D0/chksum
         IF ( LPRi.GT.4 .AND. IDEb(55).GE.1 )
     &        WRITE (LO,'(/,1X,A,E15.6)')
     &         'PHO_PRBDIS: rescaling of probabilities with factor' , 
     &        fac
         DO k = 0 , KMAx
            DO i = 0 , IMAx
               PROb(Ie,i,k,Ip,IDXmpar) = PROb(Ie,i,k,Ip,IDXmpar)*fac
            END DO
         END DO
         AVEri = AVEri*fac
         AVErk = AVErk*fac
         AVErl = AVErl*fac
         AVErm = AVErm*fac
         sigmi = sigmi*fac**2
         sigmk = sigmk*fac**2
         sigml = sigml*fac**2
         sigmm = sigmm*fac**2
      END IF
C
C  probability to find Reggeon/Pomeron
      PROb(Ie,0,0,Ip,IDXmpar) = -SIGreg/(SIGpom+SIGreg)
      averj = -PROb(Ie,0,0,Ip,IDXmpar)*AVEri
      averii = AVEri - averj
C
      SIGtab(74,Ie,Ip,IDXmpar) = averii
      SIGtab(75,Ie,Ip,IDXmpar) = AVErk
      SIGtab(76,Ie,Ip,IDXmpar) = averj
C
      SIGtab(79,Ie,Ip,IDXmpar) = PROb(Ie,IMAx,0,Ip,IDXmpar)*SIGndf
      SIGtab(80,Ie,Ip,IDXmpar) = SIGndf - SIGtab(79,Ie,Ip,IDXmpar)
C
      IF ( IDEb(55).GE.1 ) THEN
 
C  average interaction probabilities
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,/1X,A)')
     &         'PHO_PRBDIS: expected interaction statistics' , 
     &        '-------------------------------------------'
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,E12.4,2I3)')
     &         'energy,IP,table index:' , EPTab(Ie,Ip,IDXmpar) , Ip , Ie
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,2I4)')
     &         'current limitations (soft,hard):' , IMAx , KMAx
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,E12.4/,4X,A,/,1X,6E11.3)')
     &         'averaged number of cuts per event (eff. cs):' , SIGndf , 
     &        ' (Pom / Pom-h / Reg / enh-tri-loop / enh-dble / sum):' , 
     &        averii , AVErk , averj , AVErl , AVErm , AVEri + AVErk + 
     &        AVErl + AVErm
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,/,4X,A,/,1X,4E11.3)')
     &         'standard deviation ( sqrt(sigma) ):' , 
     &        ' (Pomeron / Pomeron-h / enh-tri-loop / enh-dble):' , 
     &        SQRT(ABS(sigmi-AVEri**2)) , SQRT(ABS(sigmk-AVErk**2)) , 
     &        SQRT(ABS(sigml-AVErl**2)) , SQRT(ABS(sigmm-AVErm**2))
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A)')
     &         'cross section / probability  soft, hard'
         DO i = 0 , MIN(IMAx,KMAx)
            IF ( LPRi.GT.4 ) WRITE (LO,'(I5,2E15.7,3X,2E15.7)') i , 
     &           psoft(i)*SIGndf , psoft(i) , phard(i)*SIGndf , phard(i)
         END DO
 
C  cross check of probability distribution and inclusive cross section
         pssum_1 = 0.D0
         pssum_2 = 0.D0
         phsum_1 = 0.D0
         phsum_2 = 0.D0
         DO i = 1 , IMAx
            pssum_1 = pssum_1 + psoft(i)*fac
            pssum_2 = pssum_2 + psoft(i)*fac*DBLE(i)
         END DO
         DO k = 1 , KMAx
            phsum_1 = phsum_1 + phard(k)
            phsum_2 = phsum_2 + phard(k)*fac*DBLE(k)
         END DO
         IF ( LPRi.GT.4 ) WRITE (LO,'(1x,a,2E12.4,3X,2E12.4)') 'sum:' , 
     &        pssum_2*SIGndf , pssum_1 , phsum_2*SIGndf , phsum_1
 
      END IF
 
      END SUBROUTINE
