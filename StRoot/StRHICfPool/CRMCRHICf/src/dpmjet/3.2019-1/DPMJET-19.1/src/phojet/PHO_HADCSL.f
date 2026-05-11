
      SUBROUTINE PHO_HADCSL(Id1,Id2,Ecm,Plab,Imode,Sigtot,Sigel,Sigdif,
     &                      Slope,Rho)
C***********************************************************************
C
C     low-energy cross section parametrizations
C
C     input:   ID1,ID2     PDG IDs of particles (meson first)
C              ECM         c.m. energy (GeV)
C              PLAB        lab. momentum (second particle at rest)
C              IMODE       1    ECM given, PLAB ignored
C                          2    PLAB given, ECM ignored
C
C     output:  SIGTOT      total cross section (mb)
C              SIGEL       elastic cross section (mb)
C              SIGDIF      diffracive cross section (sd-1,sd-2,dd), (mb)
C              SLOPE       forward elastic slope (GeV**-2)
C              RHO         real/imaginary part of elastic amplitude
C
C     comments:
C
C     - low-energy data interpolation uses PDG fits from 1992 issue
C     - high-energy extrapolation by Donnachie-Landshoff like fit made
C       by PDG 1996
C     - analytic extension of amplitude to calculate rho
C
C***********************************************************************
 
      IMPLICIT NONE
 
      SAVE 
 
      INTEGER Id1 , Id2 , Imode
      DOUBLE PRECISION Ecm , Plab , Sigtot , Sigel , Sigdif(3) , Slope , 
     &                 Rho
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  some constants
      INCLUDE 'inc/pocons'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
 
      INTEGER k
      DOUBLE PRECISION sigto1 , sigto2 , sigel1 , sigel2 , ss , pl , 
     &                 pll , e1 , xp , yp , ym , phr , php , x1 , x2
 
      DOUBLE PRECISION tpdg92(7,2,6) , tpdg96(9,6) , burq83(3,6) , 
     &                 xma(6)
 
      DATA tpdg92/3.D0 , 2100.D0 , 48.D0 , 0.D0 , 1.D0 , 0.522D0 , 
     &     -4.51D0 , 3.D0 , 2100.D0 , 11.9D0 , 26.9D0 , -1.21D0 , 
     &     0.169D0 , -1.85D0 , 5.D0 , 2100.D0 , 38.4D0 , 77.6D0 , 
     &     -0.64D0 , 0.26D0 , -1.2D0 , 5.D0 , 2100.D0 , 10.2D0 , 
     &     52.7D0 , -1.16D0 , 0.125D0 , -1.28D0 , 4.D0 , 340.D0 , 
     &     16.4D0 , 19.3D0 , -0.42D0 , 0.19D0 , 0.D0 , 4.D0 , 340.D0 , 
     &     0.D0 , 11.4D0 , -0.4D0 , 0.079D0 , 0.D0 , 2.5D0 , 370.D0 , 
     &     33.D0 , 14.D0 , -1.36D0 , 0.456D0 , -4.03D0 , 2.5D0 , 
     &     370.D0 , 1.76D0 , 11.2D0 , -0.64D0 , 0.043D0 , 0.D0 , 2.D0 , 
     &     310.D0 , 18.1D0 , 0.D0 , 1.D0 , 0.26D0 , -1.D0 , 2.D0 , 
     &     310.D0 , 5.D0 , 8.1D0 , -1.8D0 , 0.16D0 , -1.3D0 , 3.D0 , 
     &     310.D0 , 32.1D0 , 0.D0 , 1.D0 , 0.66D0 , -5.6D0 , 3.D0 , 
     &     310.D0 , 7.3D0 , 0.D0 , 1.D0 , 0.29D0 , -2.4D0/
 
      DATA tpdg96/50.D0 , 22.D0 , 0.079D0 , 0.25D0 , 0.D0 , 77.15D0 , 
     &     -21.05D0 , 0.46D0 , 0.9D0 , 50.D0 , 22.D0 , 0.079D0 , 
     &     0.25D0 , 0.D0 , 77.15D0 , 21.05D0 , 0.46D0 , 0.9D0 , 10.D0 , 
     &     13.70 , 0.079D0 , 0.25D0 , 0.D0 , 31.85D0 , -4.05D0 , 
     &     0.45D0 , 0.9D0 , 10.D0 , 13.70 , 0.079D0 , 0.25D0 , 0.D0 , 
     &     31.85D0 , 4.05D0 , 0.45D0 , 0.9D0 , 10.D0 , 12.20 , 0.079D0 , 
     &     0.25D0 , 0.D0 , 17.35D0 , -9.05D0 , 0.50D0 , 0.9D0 , 10.D0 , 
     &     12.20 , 0.079D0 , 0.25D0 , 0.D0 , 17.35D0 , 9.05D0 , 0.50D0 , 
     &     0.9D0/
 
      DATA burq83/11.13D0 , -6.21D0 , 0.30D0 , 11.13D0 , 7.23D0 , 
     &     0.30D0 , 9.11D0 , -0.73D0 , 0.28D0 , 9.11D0 , 0.65D0 , 
     &     0.28D0 , 8.55D0 , -5.98D0 , 0.28D0 , 8.55D0 , 1.60D0 , 
     &     0.28D0/
 
      DATA xma/2*0.93956563D0 , 2*0.13956995D0 , 2*0.493677D0/
 
C  find index
      IF ( Id2.NE.2212 ) THEN
 
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,2A,2I7)')
     &        'PHO_HADCSL:ERROR: ' , 'invalid particle combination: ' , 
     &        Id1 , Id2
      ELSE
         IF ( Id1.EQ.2212 ) THEN
            k = 1
         ELSE IF ( Id1.EQ.-2212 ) THEN
            k = 2
         ELSE IF ( Id1.EQ.211 ) THEN
            k = 3
         ELSE IF ( Id1.EQ.-211 ) THEN
            k = 4
         ELSE IF ( Id1.EQ.321 ) THEN
            k = 5
         ELSE IF ( Id1.EQ.-321 ) THEN
            k = 6
         ELSE
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,2A,2I7)')
     &            'PHO_HADCSL:ERROR: ' , 
     &           'invalid particle combination: ' , Id1 , Id2
            GOTO 99999
         END IF
 
C  calculate lab momentum
         IF ( Imode.EQ.1 ) THEN
            ss = Ecm**2
            e1 = 0.5D0/xma(1)*(ss-xma(1)**2-xma(k)**2)
            pl = SQRT(e1*e1-xma(k)**2)
         ELSE IF ( Imode.EQ.2 ) THEN
            pl = Plab
            ss = xma(1)**2 + xma(k)**2 + 2.D0*xma(1)
     &           *SQRT(pl**2+xma(k)**2)
            Ecm = SQRT(ss)
         ELSE
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I5)')
     &            'PHO_HADCSL:ERROR: invalid IMODE: ' , Imode
            RETURN
         END IF
         pll = LOG(pl)
 
C  check against lower limit
         IF ( Ecm.LE.xma(1)+xma(k) ) THEN
 
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,2A,1P,2E12.4)')
     &            'PHO_HADCSL:ERROR: ' , 
     &           'energy too small (Ecm,Plab): ' , Ecm , Plab
         ELSE
 
            xp = tpdg96(2,k)*ss**tpdg96(3,k)
            yp = tpdg96(6,k)/ss**tpdg96(8,k)
            ym = tpdg96(7,k)/ss**tpdg96(8,k)
 
            phr = TAN(PI/2.D0*(1.-tpdg96(8,k)))
            php = TAN(PI/2.D0*(1.+tpdg96(3,k)))
            Rho = (-yp/phr+ym*phr-xp/php)/(yp+ym+xp)
            Slope = burq83(1,k) + burq83(2,k)/SQRT(pl) + burq83(3,k)*pll
 
C  select energy range and interpolation method
            IF ( pl.LT.tpdg96(1,k) ) THEN
               Sigtot = tpdg92(3,1,k) + tpdg92(4,1,k)*pl**tpdg92(5,1,k)
     &                  + tpdg92(6,1,k)*pll**2 + tpdg92(7,1,k)*pll
               Sigel = tpdg92(3,2,k) + tpdg92(4,2,k)*pl**tpdg92(5,2,k)
     &                 + tpdg92(6,2,k)*pll**2 + tpdg92(7,2,k)*pll
            ELSE IF ( pl.LT.tpdg92(2,1,k) ) THEN
               sigto1 = tpdg92(3,1,k) + tpdg92(4,1,k)*pl**tpdg92(5,1,k)
     &                  + tpdg92(6,1,k)*pll**2 + tpdg92(7,1,k)*pll
               sigel1 = tpdg92(3,2,k) + tpdg92(4,2,k)*pl**tpdg92(5,2,k)
     &                  + tpdg92(6,2,k)*pll**2 + tpdg92(7,2,k)*pll
               sigto2 = yp + ym + xp
               sigel2 = sigto2**2/(16.D0*PI*Slope*GEV2mb)*(1.D0+Rho**2)
               x2 = LOG(pl/tpdg96(1,k))/LOG(tpdg92(2,1,k)/tpdg96(1,k))
               x1 = 1.D0 - x2
               Sigtot = sigto2*x2 + sigto1*x1
               Sigel = sigel2*x2 + sigel1*x1
            ELSE
               Sigtot = yp + ym + xp
               Sigel = Sigtot**2/(16.D0*PI*Slope*GEV2mb)*(1.D0+Rho**2)
            END IF
 
C  no parametrization of diffraction implemented
            Sigdif(1) = -1.D0
            Sigdif(2) = -1.D0
            Sigdif(3) = -1.D0
 
            RETURN
         END IF
      END IF
 
99999 END SUBROUTINE
