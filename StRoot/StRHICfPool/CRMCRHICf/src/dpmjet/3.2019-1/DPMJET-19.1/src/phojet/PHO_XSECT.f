
      SUBROUTINE PHO_XSECT(Ip,Ifhard,Ee)
C*********************************************************************
C
C     calculation of physical cross sections
C
C     input:   IP      particle combination
C              IFHARD  -1 reset Born graph cross section tables
C                      0  calculate hard cross sections or take them
C                         from interpolation table (if available)
C                      1  assume that hard cross sections are already
C                         calculated and stored in /POSBRN/
C              EE      cms energy (GeV)
C
C     output:  /POSBRN/  input cross sections
C              /POZBRN/  scaled input cross values
C              /POCSEC/  physical cross sections and slopes
C
C              slopes in GeV**-2, cross sections in mb
C
C*********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION b2 , bmax , CDABS , del_dd , del_sd1 , del_sd2 , 
     &                 DEPS , Ee , etmp , fac , facsl , ONEM , sigdd , 
     &                 sigsd1 , sigsd2 , sig_dd , sig_sd1 , sig_sd2 , 
     &                 slel1 , slel2
      DOUBLE PRECISION slvm1 , slvm2 , ss , THOUS , wg , wgb , wght , 
     &                 xi_max , xi_min , xpnt
      INTEGER i , Ifhard , Ip , j , k
      SAVE 
 
      PARAMETER (ONEM=-1.D0,THOUS=1.D3,DEPS=1.D-20)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  some constants
      INCLUDE 'inc/pocons'
C  event debugging information
      INCLUDE 'inc/podebg'
C  integration precision for hard cross sections (obsolete)
      INCLUDE 'inc/pogaup'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  Born graph cross sections and slopes
      INCLUDE 'inc/posbrn'
C  cross sections
      INCLUDE 'inc/pocsec'
C  global event kinematics and particle IDs
      INCLUDE 'inc/pogcms'
 
      CHARACTER*15 PHO_PNAME
 
C  complex Born graph amplitudes used for unitarization
      INCLUDE 'inc/point4'
 
      DIMENSION xpnt(96) , wght(96) , slvm1(4,4) , slvm2(4,4)
      CHARACTER*8 vmesa(0:4) , vmesb(0:4)
      DATA vmesa/'vmeson  ' , 'rho     ' , 'omega   ' , 'phi     ' , 
     &     'pi+pi-  '/
      DATA vmesb/'vmeson  ' , 'rho     ' , 'omega   ' , 'phi     ' , 
     &     'pi+pi-  '/
 
      CDABS(AMPel) = ABS(AMPel)
 
      etmp = ECM
      IF ( Ee.GE.0.D0 ) THEN
         ECM = Ee
 
C  impact parameter integration
C     BMAX=12.D0*SQRT(MAX(BPOM,BREG))
         bmax = 10.D0
         CALL PHO_GAUSET(0.D0,bmax,NGAuso,xpnt,wght)
         SIGtot = 0.D0
         SIGine = 0.D0
         SIGela = 0.D0
         SIGndf = 0.D0
         SIGlsd(1) = 0.D0
         SIGlsd(2) = 0.D0
         SIGldd = 0.D0
         SIGhsd(1) = 0.D0
         SIGhsd(2) = 0.D0
         SIGhdd = 0.D0
         SIGcdf(0) = 0.D0
         SIG1so = 0.D0
         SIG1ha = 0.D0
         slel1 = 0.D0
         slel2 = 0.D0
         DO i = 1 , 4
            SIGcdf(i) = 0.D0
            DO k = 1 , 4
               SIGvm(i,k) = 0.D0
               slvm1(i,k) = 0.D0
               slvm2(i,k) = 0.D0
            END DO
         END DO
 
         DO i = 1 , NGAuso
            b2 = xpnt(i)**2
            wg = wght(i)*xpnt(i)
            wgb = b2*wg
 
C  calculate impact parameter amplitude, results in /POINT4/
            IF ( i.EQ.1 ) THEN
               CALL PHO_EIKON(Ip,Ifhard,xpnt(i))
            ELSE
               CALL PHO_EIKON(Ip,1,xpnt(i))
            END IF
 
            SIGtot = SIGtot + DREAL(AMPel)*wg
            SIGela = SIGela + CDABS(AMPel)**2*wg
            slel1 = slel1 + AMPel*wgb
            slel2 = slel2 + AMPel*wg
 
            DO j = 1 , 4
               DO k = 1 , 4
                  SIGvm(j,k) = SIGvm(j,k) + CDABS(AMPvm(j,k))**2*wg
                  slvm1(j,k) = slvm1(j,k) + AMPvm(j,k)*wgb
                  slvm2(j,k) = slvm2(j,k) + AMPvm(j,k)*wg
               END DO
               SIGcdf(j) = SIGcdf(j) + DREAL(AMPdp(j))*wg
            END DO
 
            SIGlsd(1) = SIGlsd(1) + CDABS(AMLmsd(1))**2*wg
            SIGlsd(2) = SIGlsd(2) + CDABS(AMLmsd(2))**2*wg
            SIGldd = SIGldd + CDABS(AMLmdd)**2*wg
            SIG1so = SIG1so + DREAL(AMPsof)*wg
            SIG1ha = SIG1ha + DREAL(AMPhar)*wg
            SIGhsd(1) = SIGhsd(1) + DREAL(AMHmsd(1))*wg
            SIGhsd(2) = SIGhsd(2) + DREAL(AMHmsd(2))*wg
            SIGhdd = SIGhdd + DREAL(AMHmdd)*wg
 
         END DO
 
         SIGdir = DREAL(SIGhd)
         fac = 4.D0*PI2
         SIGtot = SIGtot*fac
         SIGela = SIGela*fac
         facsl = 0.5D0/GEV2mb
         SLOel = slel1/MAX(DEPS,slel2)*facsl
 
         IF ( (IFPap(1).EQ.22) .OR. (IFPap(2).EQ.22) ) THEN
            DO i = 1 , 4
               DO j = 1 , 4
                  SIGvm(i,j) = SIGvm(i,j)*fac
                  SLOvm(i,j) = slvm1(i,j)/MAX(DEPS,slvm2(i,j))*facsl
               END DO
            END DO
            SIGvm(0,0) = 0.D0
            DO i = 1 , 4
               SIGvm(0,i) = 0.D0
               SIGvm(i,0) = 0.D0
               DO j = 1 , 4
                  SIGvm(0,i) = SIGvm(0,i) + SIGvm(j,i)
                  SIGvm(i,0) = SIGvm(i,0) + SIGvm(i,j)
               END DO
               SIGvm(0,0) = SIGvm(0,0) + SIGvm(i,0)
            END DO
         END IF
 
C  diffractive cross sections
 
         SIGlsd(1) = SIGlsd(1)*fac*PARmdl(40)
         SIGlsd(2) = SIGlsd(2)*fac*PARmdl(41)
         SIGldd = SIGldd*fac*PARmdl(42)
         SIGhsd(1) = (SIGhsd(1)-2.D0*(SIGcdf(1)+SIGcdf(2)))
     &               *fac*PARmdl(40)
         SIGhsd(2) = (SIGhsd(2)-2.D0*(SIGcdf(1)+SIGcdf(3)))
     &               *fac*PARmdl(41)
         SIGhdd = (SIGhdd-2.D0*(SIGcdf(2)+SIGcdf(3)+2.D0*SIGcdf(4)))
     &            *fac*PARmdl(42)
 
C  double pomeron scattering
 
         SIGcdf(0) = 0.D0
         DO i = 1 , 4
            SIGcdf(i) = SIGcdf(i)*fac
            SIGcdf(0) = SIGcdf(0) + SIGcdf(i)
         END DO
 
         SIG1so = SIG1so*fac
         SIG1ha = SIG1ha*fac
 
         SIGine = SIGtot - SIGela
 
C  user-forced change of diffractive cross section
 
         IF ( (Ip.EQ.1) .AND. (ISWmdl(30).GE.1) ) THEN
 
C  use optional explicit parametrization for single-diffraction
 
            sigsd1 = SIGlsd(1) + SIGhsd(1)
            sigsd2 = SIGlsd(2) + SIGhsd(2)
            ss = Ee*Ee
            xi_min = 1.5D0/ss
            xi_max = PARmdl(45)**2
            CALL PHO_CSDIFF(IFPap(1),IFPap(2),ss,xi_min,xi_max,sig_sd1,
     &                      sig_sd2,sig_dd)
            sig_sd1 = sig_sd1*PARmdl(40)
            sig_sd2 = sig_sd2*PARmdl(41)
 
C*sr
C       DEL_SD1 = SIG_SD1-SIGSD1
            del_sd1 = PARmdl(200)*(sig_sd1-sigsd1)
C*
 
            fac = SIGlsd(1)/sigsd1
            SIGlsd(1) = SIGlsd(1) + fac*del_sd1
            SIGhsd(1) = SIGhsd(1) + (1.D0-fac)*del_sd1
 
C       DEL_SD2 = SIG_SD2-SIGSD2
            del_sd2 = PARmdl(200)*(sig_sd2-sigsd2)
 
            fac = SIGlsd(2)/sigsd2
            SIGlsd(2) = SIGlsd(2) + fac*del_sd2
            SIGhsd(2) = SIGhsd(2) + (1.D0-fac)*del_sd2
 
            IF ( ISWmdl(30).GE.2 ) THEN
 
C  use explicit parametrization also for double diffraction diss.
               sigdd = SIGldd + SIGhdd
               sig_dd = sig_dd*PARmdl(42)
               del_dd = sig_dd - sigdd
               fac = SIGldd/sigdd
               SIGldd = SIGldd + fac*del_dd
               SIGhdd = SIGhdd + (1.D0-fac)*del_dd
               SIGcor = del_sd1 + del_sd2 + del_dd
 
            ELSE
 
C  rescale double diffraction cross sections
               SIGldd = SIGldd*PARmdl(42)
               SIGhdd = SIGhdd*PARmdl(42)
               SIGcor = del_sd1 + del_sd2 + (SIGldd+SIGhdd)
     &                  *(PARmdl(42)-1.D0)
 
            END IF
 
         ELSE
 
C  rescale unitarized cross sections for diffraction dissociation
 
            SIGlsd(1) = SIGlsd(1)*PARmdl(40)
            SIGhsd(1) = SIGhsd(1)*PARmdl(40)
            SIGlsd(2) = SIGlsd(2)*PARmdl(41)
            SIGhsd(2) = SIGhsd(2)*PARmdl(41)
            SIGldd = SIGldd*PARmdl(42)
            SIGhdd = SIGhdd*PARmdl(42)
            SIGcor = (SIGlsd(1)+SIGhsd(1))*(PARmdl(40)-1.D0)
     &               + (SIGlsd(2)+SIGhsd(2))*(PARmdl(41)-1.D0)
     &               + (SIGldd+SIGhdd)*(PARmdl(42)-1.D0)
 
         END IF
 
C  non-diffractive inelastic cross section
 
         SIGndf = SIGtot - SIGela - SIGvm(0,0) - SIGcdf(0) - SIGdir - 
     &            SIGlsd(1) - SIGhsd(1) - SIGlsd(2) - SIGhsd(2)
     &            - SIGldd - SIGhdd
      END IF
 
C  specify elastic scattering channel
 
      IF ( IFPap(1).NE.22 ) THEN
         vmesa(1) = PHO_PNAME(IFPab(1),0)
      ELSE
         vmesa(1) = 'rho           '
      END IF
      IF ( IFPap(2).NE.22 ) THEN
         vmesb(1) = PHO_PNAME(IFPab(2),0)
      ELSE
         vmesb(1) = 'rho           '
      END IF
 
C  write out physical cross sections
 
      IF ( IDEb(57).GE.5 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I3,/1X,A)')
     &         'PHO_XSECT: cross sections (mb) for combination' , Ip , 
     &        '----------------------------------------------'
         IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,E12.3,2E11.3)')
     &         'energy,virtualities' , ECM , PVIrt
         IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,E12.3)')
     &         '             total ' , SIGtot
         IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,E12.3)')
     &         '    purely elastic ' , SIGela
         IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,E12.3)')
     &         '         inelastic ' , SIGine
         IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,E12.3)')
     &         ' s-diff.particle 1 ' , SIGlsd(1) + SIGhsd(1)
         IF ( IDEb(57).GE.7 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,E12.3)')
     &            '     low-mass part ' , SIGlsd(1)
            IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,E12.3)')
     &            '    high-mass part ' , SIGhsd(1)
         END IF
         IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,E12.3)')
     &         ' s-diff.particle 2 ' , SIGlsd(2) + SIGhsd(2)
         IF ( IDEb(57).GE.7 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,E12.3)')
     &            '     low-mass part ' , SIGlsd(2)
            IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,E12.3)')
     &            '    high-mass part ' , SIGhsd(2)
         END IF
         IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,E12.3)')
     &         '       double diff ' , SIGldd + SIGhdd
         IF ( IDEb(57).GE.7 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,E12.3)')
     &            '     low-mass part ' , SIGldd
            IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,E12.3)')
     &            '    high-mass part ' , SIGhdd
         END IF
         IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,E12.3)')
     &         '    double pomeron ' , SIGcdf(0)
         IF ( IDEb(57).GE.7 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,E12.3)')
     &            '    purely elastic ' , SIGcdf(1)
            IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,E12.3)')
     &            ' excitation part.1 ' , SIGcdf(2)
            IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,E12.3)')
     &            ' excitation part.2 ' , SIGcdf(3)
            IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,E12.3)')
     &            '   excitation both ' , SIGcdf(4)
         END IF
         IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,E12.3)')
     &         '     elastic slope ' , SLOel
         DO i = 1 , 4
            DO j = 1 , 4
               IF ( SIGvm(i,j).GT.DEPS ) THEN
                  IF ( LPRi.GT.4 ) WRITE (LO,'(1X,3A)')
     &                  'q-elastic production of ' , vmesa(i) , vmesb(j)
                  IF ( LPRi.GT.4 ) WRITE (LO,'(10X,A,E12.3)')
     &                  'cross section ' , SIGvm(i,j)
                  IF ( (i.NE.0) .AND. (j.NE.0) .AND. LPRi.GT.4 )
     &                 WRITE (LO,'(18X,A,E12.3)') 'slope ' , SLOvm(i,j)
               END IF
            END DO
         END DO
         IF ( IDEb(57).GE.7 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,E12.3)')
     &            ' vmeson production ' , SIGvm(0,0)
            IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,E12.3)')
     &            '  one-pomeron soft ' , SIG1so
            IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,E12.3)')
     &            '  one-pomeron hard ' , SIG1ha
            IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,E12.3)')
     &            '  pomeron exchange ' , SIGpom
            IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,E12.3)')
     &            '  reggeon exchange ' , SIGreg
            IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,E12.3)')
     &            ' hard resolved QCD ' , DREAL(DSIgh(9))
            IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,E12.3/)')
     &            '   hard direct QCD ' , DREAL(DSIgh(15))
         END IF
      END IF
 
      ECM = etmp
 
      END SUBROUTINE
