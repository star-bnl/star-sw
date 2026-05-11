
      SUBROUTINE PHO_SOFTXX(Jm1,Jm2,Mspar1,Mspar2,Ival1,Ival2,Msm1,Msm2,
     &                      Xsum1,Xsum2,Xmax1,Xmax2,Xs1,Xs2,Irej)
C***********************************************************************
C
C    select soft x values
C
C    input:   JM1,JM2    mother particle index in POEVT1
C                        (0  flavour not known before)
C             MSPAR1,2   number of x values to select
C             IVAL1,2    number valence quarks involved in hard
C                        scattering (0,1,2)
C             MSM1,2     minimum number of soft x to get sampled
C             XSUM1,2    sum of all x values samples up this call
C             XMAX1,2    max. x value
C
C    output   XSUM1,2    new sum of x-values sampled
C             XS1,2      field containing sampled x values
C
C    x values of valence partons are first given
C
C***********************************************************************
      IMPLICIT NONE
      INTEGER i , ibar1 , ibar2 , IPHO_BAR3 , Irej , iswap , Ival1 , 
     &        Ival2 , Jm1 , Jm2 , msdiff , Msm1 , Msm2 , msmax , msmin , 
     &        msoft , Mspar1 , Mspar2
      DOUBLE PRECISION psbar , psmes , xbmin1 , xbmin2 , xbmina , xdel , 
     &                 xfac , Xmax1 , Xmax2 , xmaxp1 , xmaxp2 , xmin , 
     &                 xmin1 , xmin2 , xmins1 , xmins2 , xmmin1 , 
     &                 xmmin2 , xmmina , xpot1
      DOUBLE PRECISION xpot2 , Xs1 , Xs2 , xsmin1 , xsmin2 , xsmina , 
     &                 xss1 , xss2 , Xsum1 , Xsum2
      SAVE 
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  internal rejection counters
      INCLUDE 'inc/poloop'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  data of c.m. system of Pomeron / Reggeon exchange
      INCLUDE 'inc/popcms'
 
C  standard particle data interface
 
 
      INCLUDE 'inc/poevt1'
C  extension to standard particle data interface (PHOJET specific)
      INCLUDE 'inc/poevt2'
 
C  nucleon-nucleus / nucleus-nucleus interface to DPMJET
      INCLUDE 'inc/pohdfl'
C  obsolete cut-off information
      INCLUDE 'inc/pocut1'
C  data on most recent hard scattering
      INCLUDE 'inc/pockin'
 
      DIMENSION Xs1(*) , Xs2(*)
 
C** anfe increased MAXPOT = 50 -> 100
      INTEGER MAXPOT
      PARAMETER (MAXPOT=100)
      DIMENSION xpot1(MAXPOT) , xpot2(MAXPOT) , xmin(2,MAXPOT)
 
      Irej = 0
 
      msmax = MAX(Mspar1,Mspar2)
      msmin = MAX(Msm1,Msm2)
 
      IF ( msmax.GT.MAXPOT ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,2A,2I4)')
     &         'PHO_SOFTXX: no space left in ' , 
     &        'local fields XPOT1/2 (MSMAX,MAXPOT):' , msmax , MAXPOT
         Irej = 1
         RETURN
      END IF
 
C  determine exponents
      ibar1 = IPHO_BAR3(Jm1,2)
      ibar2 = IPHO_BAR3(Jm2,2)
      iswap = 0
      IF ( (ibar1*ibar2).LT.0 ) iswap = 1
C  meson-baryon scattering (asymmetric sea)
      IF ( (ABS(ibar1)+ABS(ibar2)).EQ.1 ) THEN
         psbar = PARmdl(53)
         psmes = PARmdl(57)
      ELSE
         psbar = PARmdl(52)
         psmes = PARmdl(56)
      END IF
 
C  lower limits for x sampling
      xmmina = 2.D0*PARmdl(157)/ECMp
      xbmina = 2.D0*PARmdl(158)/ECMp
      xsmina = 2.D0*PARmdl(159)/ECMp
      xmin1 = MAX(XSOmin,AS/Xmax2)
      xmin2 = MAX(XSOmin,AS/Xmax1)
      xmaxp1 = MIN(1.D0-xmin1*msmax,Xmax1)
      xmaxp2 = MIN(1.D0-xmin2*msmax,Xmax2)
      xmin1 = MAX(AS/Xmax2,xmin1)
      xmin2 = MAX(AS/Xmax1,xmin2)
 
C  particle 1
      xmmin1 = MAX(xmin1,xmmina)
      xbmin1 = MAX(xmin1,xbmina)
      xsmin1 = MAX(xmin1,xsmina)
C  mesonic particle
      IF ( ibar1.EQ.0 ) THEN
         IF ( IHFls(1).EQ.0 ) THEN
            xpot1(1) = PARmdl(62)
            xmin(1,1) = xsmin1
            xpot1(2) = PARmdl(63)
            xmin(1,2) = xsmin1
         ELSE
            xpot1(1) = PARmdl(54)
            xmin(1,1) = xmmin1
            xpot1(2) = PARmdl(55)
            xmin(1,2) = xmmin1
         END IF
         DO i = 3 - Ival1 , msmax
            xpot1(i) = psmes
            xmin(1,i) = xsmin1
         END DO
C  baryonic particle
      ELSE
         IF ( IHFls(1).EQ.0 ) THEN
            xpot1(1) = PARmdl(62)
            xmin(1,1) = xsmin1
            xpot1(2) = PARmdl(63)
            xmin(1,2) = xsmin1
         ELSE
            xpot1(1) = PARmdl(50)
            xmin(1,1) = xbmin1
            xpot1(2) = PARmdl(51)
            xmin(1,2) = xmmin1
         END IF
         DO i = 3 - Ival1 , msmax
            xpot1(i) = psbar
            xmin(1,i) = xsmin1
         END DO
      END IF
 
C  particle 2
      xmmin2 = MAX(xmin2,xmmina)
      xbmin2 = MAX(xmin2,xbmina)
      xsmin2 = MAX(xmin2,xsmina)
C  mesonic particle
      IF ( ibar2.EQ.0 ) THEN
         IF ( IHFls(2).EQ.0 ) THEN
            xpot2(1) = PARmdl(62)
            xmin(2,1) = xsmin2
            xpot2(2) = PARmdl(63)
            xmin(2,2) = xsmin2
         ELSE
            xpot2(1) = PARmdl(54)
            xmin(2,1) = xmmin2
            xpot2(2) = PARmdl(55)
            xmin(2,2) = xmmin2
         END IF
         DO i = 3 - Ival2 , msmax
            xpot2(i) = psmes
            xmin(2,i) = xsmin2
         END DO
C  baryonic particle
      ELSE
         IF ( IHFls(2).EQ.0 ) THEN
            xpot2(1) = PARmdl(62)
            xmin(2,1) = xsmin2
            xpot2(2) = PARmdl(63)
            xmin(2,2) = xsmin2
         ELSE
            xpot2(1) = PARmdl(50)
            xmin(2,1) = xbmin2
            xpot2(2) = PARmdl(51)
            xmin(2,2) = xmmin2
         END IF
         DO i = 3 - Ival2 , msmax
            xpot2(i) = psbar
            xmin(2,i) = xsmin2
         END DO
      END IF
 
      xss1 = Xsum1
      xss2 = Xsum2
      msoft = msmax
 
C  check limits (important for valences)
      IF ( (xmin(1,1).LT.xmaxp1) .AND. (xmin(1,2).LT.xmaxp1) ) THEN
         IF ( (xmin(2,1).LT.xmaxp2) .AND. (xmin(2,2).LT.xmaxp2) ) THEN
 
            xmins1 = xss1
            IF ( IHFls(1).NE.0 ) xmins1 = xmins1 + (PARmdl(166)/ECMp)**2
            xmins2 = xss2
            IF ( IHFls(2).NE.0 ) xmins2 = xmins2 + (PARmdl(166)/ECMp)**2
            DO i = 1 , msoft
               xmins1 = xmins1 + xmin(1,i)
               xmins2 = xmins2 + xmin(2,i)
            END DO
            IF ( (xmins1.LT.1.D0) .AND. (xmins2.LT.1.D0) ) THEN
 
C  try to sample x values
               IF ( IPAmdl(14).EQ.0 ) THEN
                  IF ( msoft.EQ.2 ) THEN
                     CALL PHO_SELSX2(xpot1,xpot2,xmin,xss1,xss2,xmaxp1,
     &                  xmaxp2,Xs1,Xs2,Irej)
                  ELSE IF ( msoft.LT.5 ) THEN
                     CALL PHO_SELSXR(msoft,msmin,xpot1,xpot2,xmin,xss1,
     &                  xss2,xmaxp1,xmaxp2,Xs1,Xs2,Irej)
                  ELSE
                     CALL PHO_SELSXS(msoft,msmin,xpot1,xpot2,xmin,xss1,
     &                  xss2,xmaxp1,xmaxp2,Xs1,Xs2,Irej)
                  END IF
               ELSE IF ( IPAmdl(14).EQ.1 ) THEN
                  IF ( msoft.EQ.2 ) THEN
                     CALL PHO_SELSX2(xpot1,xpot2,xmin,xss1,xss2,xmaxp1,
     &                  xmaxp2,Xs1,Xs2,Irej)
                  ELSE
                     CALL PHO_SELSXS(msoft,msmin,xpot1,xpot2,xmin,xss1,
     &                  xss2,xmaxp1,xmaxp2,Xs1,Xs2,Irej)
                  END IF
               ELSE IF ( IPAmdl(14).EQ.2 ) THEN
                  CALL PHO_SELSXS(msoft,msmin,xpot1,xpot2,xmin,xss1,
     &               xss2,xmaxp1,xmaxp2,Xs1,Xs2,Irej)
               ELSE IF ( IPAmdl(14).EQ.3 ) THEN
                  IF ( msoft.EQ.2 ) THEN
                     CALL PHO_SELSX2(xpot1,xpot2,xmin,xss1,xss2,xmaxp1,
     &                  xmaxp2,Xs1,Xs2,Irej)
                  ELSE IF ( Ival1+Ival2.EQ.0 ) THEN
                     CALL PHO_SELSXI(msoft,msmin,xpot1,xpot2,xmin,xss1,
     &                  xss2,xmaxp1,xmaxp2,Xs1,Xs2,Irej)
                  ELSE
                     CALL PHO_SELSXS(msoft,msmin,xpot1,xpot2,xmin,xss1,
     &                  xss2,xmaxp1,xmaxp2,Xs1,Xs2,Irej)
                  END IF
               ELSE
                  IF ( LPRi.GT.4 ) WRITE (LO,'(/,1X,A,I3)')
     &                  'PHO_SOFTXX:ERROR: unsupported IPAMDL(14)' , 
     &                 IPAmdl(14)
                  STOP
               END IF
               IF ( Irej.NE.0 ) THEN
                  IFAil(41) = IFAil(41) + 1
                  IF ( IDEb(60).GE.2 ) THEN
                     IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I12,4I3)')
     &                     'PHO_SOFTXX: rejection: EVE,MSP1/2,MSM1/2' , 
     &                    KEVent , Mspar1 , Mspar2 , Msm1 , Msm2
                     IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1P4E11.3)')
     &                     'XSUM1/2,XMAX1/2' , Xsum1 , Xsum2 , Xmax1 , 
     &                    Xmax2
                  END IF
                  RETURN
               END IF
               IF ( msoft.NE.msmax ) THEN
                  msdiff = msmax - msoft
                  Mspar1 = Mspar1 - msdiff
                  Mspar2 = Mspar2 - msdiff
               END IF
 
C  correct for different MSPAR numbers
               IF ( msoft.NE.Mspar1 ) THEN
                  IF ( Mspar1.GT.1 ) THEN
                     xdel = 0.D0
                     DO i = Mspar1 + 1 , msoft
                        xdel = xdel + Xs1(i)
                     END DO
                     xfac = (1.D0-Xsum1)/(1.D0-xdel-Xsum1)
                     DO i = 2 , Mspar1
                        Xs1(i) = Xs1(i)*xfac
                     END DO
                     xss1 = (xss1-xdel-Xsum1)*xfac + Xsum1
                  ELSE
                     xss1 = Xsum1
                  END IF
               END IF
               IF ( msoft.NE.Mspar2 ) THEN
                  IF ( Mspar2.GT.1 ) THEN
                     xdel = 0.D0
                     DO i = Mspar2 + 1 , msoft
                        xdel = xdel + Xs2(i)
                     END DO
                     xfac = (1.D0-Xsum2)/(1.D0-xdel-Xsum2)
                     DO i = 2 , Mspar2
                        Xs2(i) = Xs2(i)*xfac
                     END DO
                     xss2 = (xss2-xdel-Xsum2)*xfac + Xsum2
                  ELSE
                     xss2 = Xsum2
                  END IF
               END IF
 
C  first x entry
               Xs1(1) = 1.D0 - xss1
               Xs2(1) = 1.D0 - xss2
               Xsum1 = xss1
               Xsum2 = xss2
 
C  debug output
               IF ( IDEb(60).GE.10 ) THEN
                  IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I8,2I4,2E12.4)')
     &                  'PHO_SOFTXX: EVE,MSPAR1/2,XSUM1/2:' , KEVent , 
     &                 Mspar1 , Mspar2 , Xsum1 , Xsum2
                  IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A)')
     &                  'PHO_SOFTXX: I  XS1/2   XPOT1/2  XMIN1/2'
                  DO i = 1 , msoft
                     IF ( LPRi.GT.4 ) WRITE (LO,'(5X,I3,6E12.4)') i , 
     &                    Xs1(i) , Xs2(i) , xpot1(i) , xpot2(i) , 
     &                    xmin(1,i) , xmin(2,i)
                  END DO
               END IF
 
               RETURN
            END IF
         END IF
      END IF
 
C  not enough phase space
 
      IFAil(42) = IFAil(42) + 1
      Irej = 1
 
C  warning message
      IF ( IDEb(60).GE.1 ) THEN
         WRITE (LO,'(1X,A,1P,2E11.3,/1X,A,/5X,6E11.3)')
     &           'PHO_SOFTXX: Xmin>Xmax or sum(Xmin)>1 (ECM,AS)' , 
     &          ECMp , AS , 'PHO_SOFTXX: Xmin1/2,Xmaxp1/2,sum(Xmin1/2)'
     &          , xmin1 , xmin2 , xmaxp1 , xmaxp2 , xmins1 , xmins2
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1P,3E11.3)')
     &         'PHO_SOFTXX: Xmmina,Xbmina,Xsmina:' , xmmina , xbmina , 
     &        xsmina
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1P,3E11.3)')
     &         'PHO_SOFTXX: Xmmin1,Xbmin1,Xsmin1:' , xmmin1 , xbmin1 , 
     &        xsmin1
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1P,3E11.3)')
     &         'PHO_SOFTXX: Xmmin2,Xbmin2,Xsmin2:' , xmmin2 , xbmin2 , 
     &        xsmin2
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A)') 
     &     'PHO_SOFTXX: Table of lower x limits (I,Xmin(1,I),Xmin(2,I))'
         DO i = 1 , msoft
            IF ( LPRi.GT.4 ) WRITE (LO,'(5X,I3,1P,2E11.3)') i , 
     &           xmin(1,i) , xmin(2,i)
         END DO
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I10,2I4,2E11.3)')
     &         'PHO_SOFTXX: KEVENT,MSPAR1/2,XSUM1/2:' , KEVent , 
     &        Mspar1 , Mspar2 , Xsum1 , Xsum2
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A)')
     &         'PHO_SOFTXX: I   XPOT1/2   XMIN1/2'
         DO i = 1 , msoft
            IF ( LPRi.GT.4 ) WRITE (LO,'(5X,I3,4E12.4)') i , xpot1(i) , 
     &           xpot2(i) , xmin(1,i) , xmin(2,i)
         END DO
      END IF
 
      END SUBROUTINE
