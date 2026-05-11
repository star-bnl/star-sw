
      SUBROUTINE PHO_CSINT(Ip,Ifpa,Ifpb,Ihla,Ihlb,Ecm,Pvir2a,Pvir2b)
C********************************************************************
C
C     calculate cross sections by interpolation
C
C     input:   IP          particle combination
C              IFPA/B      particle PDG number
C              IHLA/B      particle helicity (photons only)
C              ECM         c.m. energy (GeV)
C              PVIR2A      virtuality of particle A (GeV**2, positive)
C              PVIR2B      virtuality of particle B (GeV**2, positive)
C
C     output:  cross sections stored in /POCSEC/
C
C********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION DEPS , Ecm , EPS , f2 , f2m , f2s , f2_c , 
     &                 f2_fac , f2_light , fac1 , fac2 , facd , fach , 
     &                 facp , fcorr , fh_l , fh_t , fsul , fsut , p2
      DOUBLE PRECISION pd , PHO_PTCUT , Pvir2a , Pvir2b , pvirt , q2 , 
     &                 q2_max , q2_min , sigdih , sigeff , sighin , 
     &                 sigsrh , sigtmp , sig_ll , sig_lt , sig_tl , 
     &                 sig_tt , x , xnu , xpdf_c
      INTEGER i , i1 , i2 , Ifpa , Ifpb , ihel , Ihla , Ihlb , ilpap , 
     &        Ip , ipcomb , j , k
      SAVE 
 
      PARAMETER (EPS=1.D-5,DEPS=1.D-15)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  current beam selection
      INCLUDE 'inc/pobeam'
C  some constants
      INCLUDE 'inc/pocons'
C  event debugging information
      INCLUDE 'inc/podebg'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  energy-interpolation table
      INCLUDE 'inc/potabl'
C  cross sections
      INCLUDE 'inc/pocsec'
C  hard cross sections and MC selection weights
      INCLUDE 'inc/pohrcs'
C  obsolete cut-off information
      INCLUDE 'inc/pocut1'
 
      DIMENSION pvirt(2) , sigsrh(2) , fsut(2) , fsul(2) , ilpap(2) , 
     &          ihel(2)
 
      DIMENSION pd(-6:6) , fh_t(2) , fh_l(2)
      DATA ipcomb/0/
 
C  debug
      IF ( LPRi.GT.4 .AND. IDEb(15).GE.10 )
     &      WRITE (LO,'(1X,A,/10X,I3,2I6,1P3E12.4)')
     &      'PHO_CSINT: called with IP, IFP1, IFP2, ECM, PVIR1, PVIR2' , 
     &     Ip , Ifpa , Ifpb , Ecm , Pvir2a , Pvir2b
C**anfe Initialize the ptcut common block information
      PTCut(Ip) = PHO_PTCUT(Ecm,Ip)
C  check currently stored cross sections
      IF ( (Ip.EQ.IPFil) .AND. (Ecm.EQ.ECMfil) .AND. (Pvir2a.EQ.P2Afil)
     &     .AND. (Pvir2b.EQ.P2Bfil) .AND. (Ifpa.EQ.IFAfil) .AND. 
     &     (Ifpb.EQ.IFBfil) .AND. (ipcomb.EQ.IDXmpar) ) THEN
C  nothing to calculate
         IF ( IDEb(15).GE.20 .AND. LPRi.GT.4 ) WRITE (LO,'(1X,A)')
     &         'PHO_CSINT: nothing done'
         RETURN
      ELSE
 
C  copy to local fields
         ilpap(1) = Ifpa
         ilpap(2) = Ifpb
         ihel(1) = Ihla
         ihel(2) = Ihlb
         pvirt(1) = Pvir2a
         pvirt(2) = Pvir2b
C  load cross sections from interpolation table
         IF ( Ecm.LE.SIGecm(1,Ip,IDXmpar) ) THEN
            i1 = 1
            i2 = 2
         ELSE IF ( Ecm.LE.SIGecm(ISImax(IDXmpar),Ip,IDXmpar) ) THEN
            DO i = 2 , ISImax(IDXmpar)
               IF ( Ecm.LE.SIGecm(i,Ip,IDXmpar) ) GOTO 20
            END DO
 20         i1 = i - 1
            i2 = i
         ELSE
            IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,2E12.3)')
     &            'PHO_CSINT: too high energy' , Ecm , 
     &           SIGecm(ISImax(IDXmpar),Ip,IDXmpar)
            CALL PHO_PREVNT(-1)
            i1 = ISImax(IDXmpar) - 1
            i2 = ISImax(IDXmpar)
         END IF
         fac2 = 0.D0
         IF ( i1.NE.i2 ) fac2 = LOG(Ecm/SIGecm(i1,Ip,IDXmpar))
     &        /LOG(SIGecm(i2,Ip,IDXmpar)/SIGecm(i1,Ip,IDXmpar))
         fac1 = 1.D0 - fac2
 
C  cross section dependence on photon virtualities
         DO k = 1 , 2
            FSUp(k) = 1.D0
            FSUd(k) = 1.D0
            FSUh(k) = 1.D0
            IF ( ilpap(k).EQ.22 ) THEN
               IF ( ISWmdl(10).GE.1 ) THEN
                  FSUp(k) = 0.D0
                  fsut(k) = 0.D0
                  fsul(k) = 0.D0
                  FSUh(k) = 0.D0
C  GVDM factors for transverse/longitudinal photons
                  DO i = 1 , 3
                     fsut(k) = fsut(k) + PARmdl(26+i)
     &                  /(1.D0+pvirt(k)/PARmdl(30+i))**2
                     fsul(k) = fsul(k) + PARmdl(26+i)*pvirt(k)
     &                  /(4.D0*PARmdl(30+i))
     &                  /(1.D0+pvirt(k)/PARmdl(30+i))**2
                  END DO
                  fsut(k) = fsut(k) + PARmdl(30)
     &                      /(1.D0+pvirt(k)/PARmdl(34))
C  transverse part
                  IF ( (ABS(ihel(k)).EQ.1) .OR. (ISWmdl(10).EQ.1) ) THEN
                     FSUp(k) = fsut(k)
                     FSUh(k) = fsut(k)/(fsut(k)+fsul(k))
C  diffraction of trans. photons corresponds mainly to leading twist
                     FSUd(k) = 1.D0
                  END IF
C  longitudinal (scalar) part
                  IF ( (ihel(k).LE.0) .OR. (ISWmdl(10).EQ.1) ) THEN
                     FSUp(k) = FSUp(k) + fsul(k)
                     FSUh(k) = FSUh(k) + fsul(k)/(fsut(k)+fsul(k))
C  diffraction of long. photons corresponds mainly to higher twist
                     FSUd(k) = 0.5D0*LOG(((Ecm*PARmdl(45))**2+pvirt(k))
     &                  /((0.765D0+PARmdl(46))**2+pvirt(k)))
     &                  /LOG(Ecm*PARmdl(45)/(0.765D0+PARmdl(46)))
                  END IF
C  debug output
                  IF ( IDEb(15).GE.10 ) THEN
                     IF ( LPRi.GT.4 )
     &                     WRITE (LO,'(1x,2a,2i3,/,5x,1p5e12.4)')
     &                     'PHO_CSINT: ' , 
     &                 'side,helicity,F_tran,F_long,F_eff,F_hard,F_diff'
     &                 , k , ihel(k) , fsut(k) , fsul(k) , FSUp(k) , 
     &                 FSUh(k) , FSUd(k)
                  END IF
               END IF
            END IF
         END DO
 
         facp = FSUp(1)*FSUp(2)
         fach = FSUh(1)*FSUh(2)
         facd = FSUd(1)*FSUd(2)
 
C  matching of model cross section to F2(x,Q2,P2) in limit of Q2 >> P2
 
         IF ( (ilpap(1).EQ.22) .AND. (ilpap(2).EQ.22) .AND. 
     &        (IPAmdl(117).GT.0) ) THEN
C  check kinematic limit
            q2_max = MAX(pvirt(1),pvirt(2))
            q2_min = MIN(pvirt(1),pvirt(2))
            IF ( (q2_max.GT.1.D0) .AND. (q2_min.LT.1.D0) ) THEN
 
C  calculate F2 from current parton density
               IF ( pvirt(1).GT.pvirt(2) ) THEN
                  k = 2
               ELSE
                  k = 1
               END IF
               q2 = q2_max
               p2 = q2_min
               x = q2/(Ecm**2+q2+p2)
               CALL PHO_ACTPDF(ilpap(k),k)
               CALL PHO_PDF(k,x,q2,p2,pd)
C  light quark contribution
               f2_light = 0.D0
               DO j = 1 , 3
                  f2_light = f2_light + Q_Ch2(j)*(pd(j)+pd(-j))
               END DO
C  heavy quark contribution
               CALL PHO_QPMPDF(4,x,q2,0.D0,p2,xpdf_c)
               f2_c = 2.D0*4.D0/9.D0*xpdf_c
               f2 = (f2_light+f2_c)
 
C  calculate model prediction
               SIGtot = fac2*SIGtab(1,i2,Ip,IDXmpar)
     &                  + fac1*SIGtab(1,i1,Ip,IDXmpar)
               SIGine = fac2*SIGtab(28,i2,Ip,IDXmpar)
     &                  + fac1*SIGtab(28,i1,Ip,IDXmpar)
               CALL PHO_HARINT(Ip,Ecm,0.D0,0.D0,0,MAX_PRO_2,3,4,1)
 
               IF ( ISWmdl(10).GE.2 ) THEN
 
C  calculate all helicity combinations
                  IF ( IPAmdl(115).EQ.0 ) THEN
                     sigdih = HSIg(14,IDXmpar)
                     sigsrh(1) = HSIg(10,IDXmpar) + HSIg(11,IDXmpar)
                     sigsrh(2) = HSIg(12,IDXmpar) + HSIg(13,IDXmpar)
                     sigtmp = SIGtot - sigdih - sigsrh(1) - sigsrh(2)
C  photon helicity factors
                     fh_t(1) = fsut(1)/(fsut(1)+fsul(1))
                     fh_l(1) = 1.D0 - fh_t(1)
                     fh_t(2) = fsut(2)/(fsut(2)+fsul(2))
                     fh_l(2) = 1.D0 - fh_t(2)
                     sig_tt = sigtmp*fsut(1)*fsut(2) + sigdih*fh_t(1)
     &                        *fh_t(2) + sigsrh(1)*fh_t(1)*fsut(2)
     &                        + sigsrh(2)*fsut(1)*fh_t(2)
                     sig_tl = sigtmp*fsut(1)*fsul(2) + sigdih*fh_t(1)
     &                        *fh_l(2) + sigsrh(1)*fh_t(1)*fsul(2)
     &                        + sigsrh(2)*fsut(1)*fh_l(2)
                     sig_lt = sigtmp*fsul(1)*fsut(2) + sigdih*fh_l(1)
     &                        *fh_t(2) + sigsrh(1)*fh_l(1)*fsut(2)
     &                        + sigsrh(2)*fsul(1)*fh_t(2)
                     sig_ll = sigtmp*fsul(1)*fsul(2) + sigdih*fh_l(1)
     &                        *fh_l(2) + sigsrh(1)*fh_l(1)*fsul(2)
     &                        + sigsrh(2)*fsul(1)*fh_l(2)
                  ELSE
C  use explicit PDF virtuality dependence (pre-tabulated)
                     sigdih = HSIg(14,IDXmpar)
                     sigsrh(1) = HSIg(10,IDXmpar) + HSIg(11,IDXmpar)
                     sigsrh(2) = HSIg(12,IDXmpar) + HSIg(13,IDXmpar)
                     sigtmp = SIGtot - sigdih - sigsrh(1) - sigsrh(2)
                     IF ( LPRi.GT.4 ) WRITE (LO,*)
     &                     ' PHO_CSINT: invalid option for F2 matching'
                     STOP
C               CALL PHO_HARINT(IP,ECM,PVIRT(1),PVIRT(2),0,
C    &                          Max_pro_2,3,4,1)
C               SIG_TT = SIGtmp*FSUT(1)*FSUT(2)
C    &            + HSig(10,IDXMPAR)+HSig(12,IDXMPAR)+
C    &            +HSig(14,IDXMPAR)+HSig(16,IDXMPAR)+HSig(18,IDXMPAR)
C               SIG_TL = SIGtmp*FSUT(1)*FSUL(2)
C    &            + HSig(10,IDXMPAR)+HSig(12,IDXMPAR)+
C    &            + HSig(14,IDXMPAR)+HSig(16,IDXMPAR)+HSig(19,IDXMPAR)
C               SIG_LT = SIGtmp*FSUL(1)*FSUT(2)
C    &            + HSig(11,IDXMPAR)+HSig(13,IDXMPAR)+
C    &            + HSig(15,IDXMPAR)+HSig(17,IDXMPAR)+HSig(20,IDXMPAR)
C               SIG_LL = SIGtmp*FSUL(1)*FSUL(2)
C    &            + HSig(11,IDXMPAR)+HSig(13,IDXMPAR)+
C    &            + HSig(15,IDXMPAR)+HSig(17,IDXMPAR)+HSig(21,IDXMPAR)
                  END IF
                  xnu = Ecm*Ecm + q2 + p2
                  f2_fac = q2*xnu/SQRT(xnu*xnu-q2*p2)/(4.D0*PI*PI)
     &                     *137.D0/GEV2mb
                  IF ( k.EQ.2 ) THEN
                     f2m = f2_fac*(sig_tt+sig_lt-0.5D0*sig_tl-
     &                     0.5D0*sig_ll)
                     f2s = f2_fac*sigtmp*(fsut(1)*fsut(2)+fsul(1)
     &                     *fsut(2)-0.5D0*fsut(1)*fsul(2)-0.5D0*fsul(1)
     &                     *fsul(2))
                  ELSE
                     f2m = f2_fac*(sig_tt+sig_tl-0.5D0*sig_lt-
     &                     0.5D0*sig_ll)
                     f2s = f2_fac*sigtmp*(fsut(1)*fsut(2)+fsut(1)
     &                     *fsul(2)-0.5D0*fsul(1)*fsut(2)-0.5D0*fsul(1)
     &                     *fsul(2))
                  END IF
 
               ELSE
 
C  assume sig_eff = sigtot
                  sigdih = HSIg(14,IDXmpar)
                  sigsrh(1) = HSIg(10,IDXmpar) + HSIg(11,IDXmpar)
                  sigsrh(2) = HSIg(12,IDXmpar) + HSIg(13,IDXmpar)
                  sigtmp = SIGtot - sigsrh(1) - sigsrh(2) - sigdih
                  sigeff = sigtmp*FSUp(1)*FSUp(2) + sigsrh(1)*FSUp(2)
     &                     + sigsrh(2)*FSUp(1) + sigdih
                  xnu = Ecm*Ecm + q2 + p2
                  f2_fac = q2*xnu/SQRT(xnu*xnu-q2*p2)/(4.D0*PI*PI)
     &                     *137.D0/GEV2mb
                  f2m = f2_fac*sigeff
                  f2s = f2_fac*sigtmp*FSUp(1)*FSUp(2)
               END IF
C           WRITE(LO,*)' PHO_CSINT: Q2_1,Q2_2,W ',PVIRT(1),PVIRT(2),Ecm
C           WRITE(LO,*)' PHO_CSINT: F2_mod,F2_pdf,mod/pdf ',
C    &                    F2m,F2,F2m/F2
 
C  global factor to re-scale suppression of soft contributions
               fcorr = (f2-f2m+f2s)/f2s
C          WRITE(LO,*)' PHO_CSINT: re-scaling factor: ',Fcorr,FACP*Fcorr
               facp = facp*fcorr
 
            END IF
         END IF
 
         SIGtot = (fac2*SIGtab(1,i2,Ip,IDXmpar)
     &            +fac1*SIGtab(1,i1,Ip,IDXmpar))*facp
         SIGine = (fac2*SIGtab(28,i2,Ip,IDXmpar)
     &            +fac1*SIGtab(28,i1,Ip,IDXmpar))*facp
         SIGela = (fac2*SIGtab(2,i2,Ip,IDXmpar)
     &            +fac1*SIGtab(2,i1,Ip,IDXmpar))*facp
         j = 2
         DO i = 0 , 4
            DO k = 0 , 4
               j = j + 1
               SIGvm(i,k) = (fac2*SIGtab(j,i2,Ip,IDXmpar)+fac1*SIGtab(j,
     &                      i1,Ip,IDXmpar))*facp**2
            END DO
         END DO
 
         SIGdir = fac2*SIGtab(29,i2,Ip,IDXmpar)
     &            + fac1*SIGtab(29,i1,Ip,IDXmpar)
         SIGhar = fac2*SIGtab(58,i2,Ip,IDXmpar)
     &            + fac1*SIGtab(58,i1,Ip,IDXmpar)
C  suppression of multi-pomeron graphs (diffraction)
         SIGlsd(1) = (fac2*SIGtab(30,i2,Ip,IDXmpar)+fac1*SIGtab(30,i1,Ip
     &               ,IDXmpar))*facp*FSUp(2)*FSUd(1)
         SIGlsd(2) = (fac2*SIGtab(31,i2,Ip,IDXmpar)+fac1*SIGtab(31,i1,Ip
     &               ,IDXmpar))*facp*FSUp(1)*FSUd(2)
         SIGhsd(1) = (fac2*SIGtab(32,i2,Ip,IDXmpar)+fac1*SIGtab(32,i1,Ip
     &               ,IDXmpar))*facp*FSUp(2)*FSUd(1)
         SIGhsd(2) = (fac2*SIGtab(33,i2,Ip,IDXmpar)+fac1*SIGtab(33,i1,Ip
     &               ,IDXmpar))*facp*FSUp(1)*FSUd(2)
         SIGldd = (fac2*SIGtab(34,i2,Ip,IDXmpar)
     &            +fac1*SIGtab(34,i1,Ip,IDXmpar))*facp**2*facd
         SIGhdd = (fac2*SIGtab(35,i2,Ip,IDXmpar)
     &            +fac1*SIGtab(35,i1,Ip,IDXmpar))*facp
         SIGcdf(0) = (fac2*SIGtab(36,i2,Ip,IDXmpar)+fac1*SIGtab(36,i1,Ip
     &               ,IDXmpar))*facp**2
         SIGtr1(1) = (fac2*SIGtab(60,i2,Ip,IDXmpar)+fac1*SIGtab(60,i1,Ip
     &               ,IDXmpar))*facp*FSUp(2)*FSUd(1)
         SIGtr1(2) = (fac2*SIGtab(61,i2,Ip,IDXmpar)+fac1*SIGtab(61,i1,Ip
     &               ,IDXmpar))*facp*FSUp(2)*FSUd(1)
         SIGtr2(1) = (fac2*SIGtab(62,i2,Ip,IDXmpar)+fac1*SIGtab(62,i1,Ip
     &               ,IDXmpar))*facp*FSUp(1)*FSUd(2)
         SIGtr2(2) = (fac2*SIGtab(63,i2,Ip,IDXmpar)+fac1*SIGtab(63,i1,Ip
     &               ,IDXmpar))*facp*FSUp(1)*FSUd(2)
         SIGloo = (fac2*SIGtab(64,i2,Ip,IDXmpar)
     &            +fac1*SIGtab(64,i1,Ip,IDXmpar))*facp
         SIGdpo(1) = (fac2*SIGtab(65,i2,Ip,IDXmpar)+fac1*SIGtab(65,i1,Ip
     &               ,IDXmpar))*facp**2
         SIGdpo(2) = (fac2*SIGtab(66,i2,Ip,IDXmpar)+fac1*SIGtab(66,i1,Ip
     &               ,IDXmpar))*facp**2
         SIGdpo(3) = (fac2*SIGtab(67,i2,Ip,IDXmpar)+fac1*SIGtab(67,i1,Ip
     &               ,IDXmpar))*facp**2
         SIGdpo(4) = (fac2*SIGtab(68,i2,Ip,IDXmpar)+fac1*SIGtab(68,i1,Ip
     &               ,IDXmpar))*facp**2
 
C  corrections due to photon virtuality dependence of PDFs
         IF ( ISWmdl(2).EQ.1 ) THEN
            CALL PHO_HARINT(Ip,Ecm,0.D0,0.D0,0,MAX_PRO_2,3,4,1)
C  minimum bias event generation
            IF ( IPAmdl(115).GE.1 ) THEN
C  all the virtuality dependence is given by PDF parametrization
               sighin = fac2*SIGtab(80,i2,Ip,IDXmpar)
     &                  + fac1*SIGtab(80,i1,Ip,IDXmpar)
               IF ( IPAmdl(116).GE.2 ) THEN
C  direct interaction according to full QPM calculation
                  sigdih = HSIg(14,IDXmpar)
                  sigsrh(1) = HSIg(10,IDXmpar) + HSIg(11,IDXmpar)
                  sigsrh(2) = HSIg(12,IDXmpar) + HSIg(13,IDXmpar)
               ELSE
C  direct interaction suppressed according to helicity factor
                  sigdih = HSIg(14,IDXmpar)*fach
                  sigsrh(1) = (HSIg(10,IDXmpar)+HSIg(11,IDXmpar))
     &                        *FSUh(1)
                  sigsrh(2) = (HSIg(12,IDXmpar)+HSIg(13,IDXmpar))
     &                        *FSUh(2)
               END IF
               IF ( LPRi.GT.4 ) WRITE (LO,*)
     &               ' PHO_CSINT: option not supported yet'
               STOP
            ELSE
C  rescale relevant hard processes
               sigdih = HSIg(14,IDXmpar)
               sigsrh(1) = HSIg(10,IDXmpar) + HSIg(11,IDXmpar)
               sigsrh(2) = HSIg(12,IDXmpar) + HSIg(13,IDXmpar)
               sigtmp = SIGine - (sigdih+sigsrh(1)+sigsrh(2))*facp
               SIGdir = HSIg(14,IDXmpar)*fach + sigsrh(1)*FSUh(1)
     &                  *FSUp(2) + sigsrh(2)*FSUp(1)*FSUh(2)
               SIGine = sigtmp + SIGdir
               SIGtot = SIGine + SIGela
            END IF
         ELSE
C  only hard interactions
            CALL PHO_HARINT(Ip,Ecm,0.D0,0.D0,0,MAX_PRO_2,3,4,1)
            sigsrh(1) = (HSIg(10,IDXmpar)+HSIg(11,IDXmpar))*FSUh(1)
            sigsrh(2) = (HSIg(12,IDXmpar)+HSIg(13,IDXmpar))*FSUh(2)
            SIGdir = HSIg(14,IDXmpar) + sigsrh(1) + sigsrh(2)
            SIGhar = HSIg(9,IDXmpar)*fach
         END IF
 
         SIG1so = (fac2*SIGtab(37,i2,Ip,IDXmpar)
     &            +fac1*SIGtab(37,i1,Ip,IDXmpar))*facp
         SIG1ha = (fac2*SIGtab(38,i2,Ip,IDXmpar)
     &            +fac1*SIGtab(38,i1,Ip,IDXmpar))*fach
         SLOel = fac2*SIGtab(39,i2,Ip,IDXmpar)
     &           + fac1*SIGtab(39,i1,Ip,IDXmpar)
         j = 39
         DO i = 1 , 4
            DO k = 1 , 4
               j = j + 1
               SLOvm(i,k) = fac2*SIGtab(j,i2,Ip,IDXmpar)
     &                      + fac1*SIGtab(j,i1,Ip,IDXmpar)
            END DO
         END DO
         SIGpom = (fac2*SIGtab(56,i2,Ip,IDXmpar)
     &            +fac1*SIGtab(56,i1,Ip,IDXmpar))*facp
         SIGreg = (fac2*SIGtab(57,i2,Ip,IDXmpar)
     &            +fac1*SIGtab(57,i1,Ip,IDXmpar))*facp
 
         IPFil = Ip
         IFAfil = Ifpa
         IFBfil = Ifpb
         ECMfil = Ecm
         ipcomb = IDXmpar
         P2Afil = Pvir2a
         P2Bfil = Pvir2b
 
         IF ( IDEb(15).GE.20 .AND. LPRi.GT.4 ) WRITE (LO,'(1X,A)')
     &         'PHO_CSINT: cross sections calculated'
 
      END IF
 
      END SUBROUTINE
