
      SUBROUTINE DT_TSAMCS(Kproj,Ekin,Cst)
 
C***********************************************************************
C Sampling of cos(theta) for nucleon-proton scattering according to    *
C hetkfa2/bertini parametrization.                                     *
C This is a revised version of the original (HJM 24/10/88)             *
C This version dated 28.10.95 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION bwfw , coef , Cst , dchn , dchna , dchnb , 
     &                 dclin , DT_RNDM , Ekin , emev , ener , flti , 
     &                 OHALF , ONE , p , pdch , pdci , r1 , r2 , r3
      DOUBLE PRECISION r4 , r5 , r6 , r7 , rnd , sum , TINY10 , TWO , 
     &                 univ , unive , value2 , ZERO
      INTEGER i , idat , ie , ii , k , Kproj
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (TWO=2.0D0,ONE=1.0D0,OHALF=0.5D0,ZERO=0.0D0,
     &           TINY10=1.0D-10)
 
      DIMENSION dclin(195) , dchn(143) , dchna(36) , dchnb(60)
      DIMENSION pdci(60) , pdch(55)
 
      DATA (dclin(i),i=1,80)/5.000D-01 , 1.000D+00 , 0.000D+00 , 
     &      1.000D+00 , 0.000D+00 , 4.993D-01 , 9.881D-01 , 5.963D-02 , 
     &      9.851D-01 , 5.945D-02 , 4.936D-01 , 8.955D-01 , 5.224D-01 , 
     &      8.727D-01 , 5.091D-01 , 4.889D-01 , 8.228D-01 , 8.859D-01 , 
     &      7.871D-01 , 8.518D-01 , 4.874D-01 , 7.580D-01 , 1.210D+00 , 
     &      7.207D-01 , 1.117D+00 , 4.912D-01 , 6.969D-01 , 1.516D+00 , 
     &      6.728D-01 , 1.309D+00 , 5.075D-01 , 6.471D-01 , 1.765D+00 , 
     &      6.667D-01 , 1.333D+00 , 5.383D-01 , 6.054D-01 , 1.973D+00 , 
     &      7.059D-01 , 1.176D+00 , 5.397D-01 , 5.990D-01 , 2.005D+00 , 
     &      7.023D-01 , 1.191D+00 , 5.336D-01 , 6.083D-01 , 1.958D+00 , 
     &      6.959D-01 , 1.216D+00 , 5.317D-01 , 6.075D-01 , 1.962D+00 , 
     &      6.897D-01 , 1.241D+00 , 5.300D-01 , 6.016D-01 , 1.992D+00 , 
     &      6.786D-01 , 1.286D+00 , 5.281D-01 , 6.063D-01 , 1.969D+00 , 
     &      6.786D-01 , 1.286D+00 , 5.280D-01 , 5.960D-01 , 2.020D+00 , 
     &      6.667D-01 , 1.333D+00 , 5.273D-01 , 5.920D-01 , 2.040D+00 , 
     &      6.604D-01 , 1.358D+00 , 5.273D-01 , 5.862D-01 , 2.069D+00 , 
     &      6.538D-01 , 1.385D+00/
      DATA (dclin(i),i=81,160)/5.223D-01 , 5.980D-01 , 2.814D+00 , 
     &      6.538D-01 , 1.385D+00 , 5.202D-01 , 5.969D-01 , 2.822D+00 , 
     &      6.471D-01 , 1.412D+00 , 5.183D-01 , 5.881D-01 , 2.883D+00 , 
     &      6.327D-01 , 1.469D+00 , 5.159D-01 , 5.866D-01 , 2.894D+00 , 
     &      6.250D-01 , 1.500D+00 , 5.133D-01 , 5.850D-01 , 2.905D+00 , 
     &      6.170D-01 , 1.532D+00 , 5.106D-01 , 5.833D-01 , 2.917D+00 , 
     &      6.087D-01 , 1.565D+00 , 5.084D-01 , 5.801D-01 , 2.939D+00 , 
     &      6.000D-01 , 1.600D+00 , 5.063D-01 , 5.763D-01 , 2.966D+00 , 
     &      5.909D-01 , 1.636D+00 , 5.036D-01 , 5.730D-01 , 2.989D+00 , 
     &      5.814D-01 , 1.674D+00 , 5.014D-01 , 5.683D-01 , 3.022D+00 , 
     &      5.714D-01 , 1.714D+00 , 4.986D-01 , 5.641D-01 , 3.051D+00 , 
     &      5.610D-01 , 1.756D+00 , 4.964D-01 , 5.580D-01 , 3.094D+00 , 
     &      5.500D-01 , 1.800D+00 , 4.936D-01 , 5.573D-01 , 3.099D+00 , 
     &      5.431D-01 , 1.827D+00 , 4.909D-01 , 5.509D-01 , 3.144D+00 , 
     &      5.313D-01 , 1.875D+00 , 4.885D-01 , 5.512D-01 , 3.142D+00 , 
     &      5.263D-01 , 1.895D+00 , 4.857D-01 , 5.437D-01 , 3.194D+00 , 
     &      5.135D-01 , 1.946D+00/
      DATA (dclin(i),i=161,195)/4.830D-01 , 5.353D-01 , 3.253D+00 , 
     &      5.000D-01 , 2.000D+00 , 4.801D-01 , 5.323D-01 , 3.274D+00 , 
     &      4.915D-01 , 2.034D+00 , 4.770D-01 , 5.228D-01 , 3.341D+00 , 
     &      4.767D-01 , 2.093D+00 , 4.738D-01 , 5.156D-01 , 3.391D+00 , 
     &      4.643D-01 , 2.143D+00 , 4.701D-01 , 5.010D-01 , 3.493D+00 , 
     &      4.444D-01 , 2.222D+00 , 4.672D-01 , 4.990D-01 , 3.507D+00 , 
     &      4.375D-01 , 2.250D+00 , 4.634D-01 , 4.856D-01 , 3.601D+00 , 
     &      4.194D-01 , 2.323D+00/
 
      DATA pdci/4.400D+02 , 1.896D-01 , 1.931D-01 , 1.982D-01 , 
     &     1.015D-01 , 1.029D-01 , 4.180D-02 , 4.228D-02 , 4.282D-02 , 
     &     4.350D-02 , 2.204D-02 , 2.236D-02 , 5.900D+02 , 1.433D-01 , 
     &     1.555D-01 , 1.774D-01 , 1.000D-01 , 1.128D-01 , 5.132D-02 , 
     &     5.600D-02 , 6.158D-02 , 6.796D-02 , 3.660D-02 , 3.820D-02 , 
     &     6.500D+02 , 1.192D-01 , 1.334D-01 , 1.620D-01 , 9.527D-02 , 
     &     1.141D-01 , 5.283D-02 , 5.952D-02 , 6.765D-02 , 7.878D-02 , 
     &     4.796D-02 , 6.957D-02 , 8.000D+02 , 4.872D-02 , 6.694D-02 , 
     &     1.152D-01 , 9.348D-02 , 1.368D-01 , 6.912D-02 , 7.953D-02 , 
     &     9.577D-02 , 1.222D-01 , 7.755D-02 , 9.525D-02 , 1.000D+03 , 
     &     3.997D-02 , 5.456D-02 , 9.804D-02 , 8.084D-02 , 1.208D-01 , 
     &     6.520D-02 , 8.233D-02 , 1.084D-01 , 1.474D-01 , 9.328D-02 , 
     &     1.093D-01/
 
      DATA pdch/1.000D+03 , 9.453D-02 , 9.804D-02 , 8.084D-02 , 
     &     1.208D-01 , 6.520D-02 , 8.233D-02 , 1.084D-01 , 1.474D-01 , 
     &     9.328D-02 , 1.093D-01 , 1.400D+03 , 1.072D-01 , 7.450D-02 , 
     &     6.645D-02 , 1.136D-01 , 6.750D-02 , 8.580D-02 , 1.110D-01 , 
     &     1.530D-01 , 1.010D-01 , 1.350D-01 , 2.170D+03 , 4.004D-02 , 
     &     3.013D-02 , 2.664D-02 , 5.511D-02 , 4.240D-02 , 7.660D-02 , 
     &     1.364D-01 , 2.300D-01 , 1.670D-01 , 2.010D-01 , 2.900D+03 , 
     &     1.870D-02 , 1.804D-02 , 1.320D-02 , 2.970D-02 , 2.860D-02 , 
     &     5.160D-02 , 1.020D-01 , 2.400D-01 , 2.250D-01 , 3.370D-01 , 
     &     4.400D+03 , 1.196D-03 , 8.784D-03 , 1.517D-02 , 2.874D-02 , 
     &     2.488D-02 , 4.464D-02 , 8.330D-02 , 2.008D-01 , 2.360D-01 , 
     &     3.567D-01/
 
      DATA (dchn(i),i=1,90)/4.770D-01 , 4.750D-01 , 4.715D-01 , 
     &      4.685D-01 , 4.650D-01 , 4.610D-01 , 4.570D-01 , 4.550D-01 , 
     &      4.500D-01 , 4.450D-01 , 4.405D-01 , 4.350D-01 , 4.300D-01 , 
     &      4.250D-01 , 4.200D-01 , 4.130D-01 , 4.060D-01 , 4.000D-01 , 
     &      3.915D-01 , 3.840D-01 , 3.760D-01 , 3.675D-01 , 3.580D-01 , 
     &      3.500D-01 , 3.400D-01 , 3.300D-01 , 3.200D-01 , 3.100D-01 , 
     &      3.000D-01 , 2.900D-01 , 2.800D-01 , 2.700D-01 , 2.600D-01 , 
     &      2.500D-01 , 2.400D-01 , 2.315D-01 , 2.240D-01 , 2.150D-01 , 
     &      2.060D-01 , 2.000D-01 , 1.915D-01 , 1.850D-01 , 1.780D-01 , 
     &      1.720D-01 , 1.660D-01 , 1.600D-01 , 1.550D-01 , 1.500D-01 , 
     &      1.450D-01 , 1.400D-01 , 1.360D-01 , 1.320D-01 , 1.280D-01 , 
     &      1.250D-01 , 1.210D-01 , 1.180D-01 , 1.150D-01 , 1.120D-01 , 
     &      1.100D-01 , 1.070D-01 , 1.050D-01 , 1.030D-01 , 1.010D-01 , 
     &      9.900D-02 , 9.700D-02 , 9.550D-02 , 9.480D-02 , 9.400D-02 , 
     &      9.200D-02 , 9.150D-02 , 9.100D-02 , 9.000D-02 , 8.990D-02 , 
     &      8.900D-02 , 8.850D-02 , 8.750D-02 , 8.700D-02 , 8.650D-02 , 
     &      8.550D-02 , 8.500D-02 , 8.499D-02 , 8.450D-02 , 8.350D-02 , 
     &      8.300D-02 , 8.250D-02 , 8.150D-02 , 8.100D-02 , 8.030D-02 , 
     &      8.000D-02 , 7.990D-02/
      DATA (dchn(i),i=91,143)/7.980D-02 , 7.950D-02 , 7.900D-02 , 
     &      7.860D-02 , 7.800D-02 , 7.750D-02 , 7.650D-02 , 7.620D-02 , 
     &      7.600D-02 , 7.550D-02 , 7.530D-02 , 7.500D-02 , 7.499D-02 , 
     &      7.498D-02 , 7.480D-02 , 7.450D-02 , 7.400D-02 , 7.350D-02 , 
     &      7.300D-02 , 7.250D-02 , 7.230D-02 , 7.200D-02 , 7.100D-02 , 
     &      7.050D-02 , 7.020D-02 , 7.000D-02 , 6.999D-02 , 6.995D-02 , 
     &      6.993D-02 , 6.991D-02 , 6.990D-02 , 6.870D-02 , 6.850D-02 , 
     &      6.800D-02 , 6.780D-02 , 6.750D-02 , 6.700D-02 , 6.650D-02 , 
     &      6.630D-02 , 6.600D-02 , 6.550D-02 , 6.525D-02 , 6.510D-02 , 
     &      6.500D-02 , 6.499D-02 , 6.498D-02 , 6.496D-02 , 6.494D-02 , 
     &      6.493D-02 , 6.490D-02 , 6.488D-02 , 6.485D-02 , 6.480D-02/
 
      DATA dchna/6.300D+02 , 7.810D-02 , 1.421D-01 , 1.979D-01 , 
     &     2.479D-01 , 3.360D-01 , 5.400D-01 , 7.236D-01 , 1.000D+00 , 
     &     1.540D+03 , 2.225D-01 , 3.950D-01 , 5.279D-01 , 6.298D-01 , 
     &     7.718D-01 , 9.405D-01 , 9.835D-01 , 1.000D+00 , 2.560D+03 , 
     &     2.625D-01 , 4.550D-01 , 5.963D-01 , 7.020D-01 , 8.380D-01 , 
     &     9.603D-01 , 9.903D-01 , 1.000D+00 , 3.520D+03 , 4.250D-01 , 
     &     6.875D-01 , 8.363D-01 , 9.163D-01 , 9.828D-01 , 1.000D+00 , 
     &     1.000D+00 , 1.000D+00/
 
      DATA dchnb/6.300D+02 , 3.800D-02 , 7.164D-02 , 1.275D-01 , 
     &     2.171D-01 , 3.227D-01 , 4.091D-01 , 5.051D-01 , 6.061D-01 , 
     &     7.074D-01 , 8.434D-01 , 1.000D+00 , 2.040D+03 , 1.200D-01 , 
     &     2.115D-01 , 3.395D-01 , 5.295D-01 , 7.251D-01 , 8.511D-01 , 
     &     9.487D-01 , 9.987D-01 , 1.000D+00 , 1.000D+00 , 1.000D+00 , 
     &     2.200D+03 , 1.344D-01 , 2.324D-01 , 3.754D-01 , 5.674D-01 , 
     &     7.624D-01 , 8.896D-01 , 9.808D-01 , 1.000D+00 , 1.000D+00 , 
     &     1.000D+00 , 1.000D+00 , 2.850D+03 , 2.330D-01 , 4.130D-01 , 
     &     6.610D-01 , 9.010D-01 , 9.970D-01 , 1.000D+00 , 1.000D+00 , 
     &     1.000D+00 , 1.000D+00 , 1.000D+00 , 1.000D+00 , 3.500D+03 , 
     &     3.300D-01 , 5.450D-01 , 7.950D-01 , 1.000D+00 , 1.000D+00 , 
     &     1.000D+00 , 1.000D+00 , 1.000D+00 , 1.000D+00 , 1.000D+00 , 
     &     1.000D+00/
 
      Cst = ONE
C
      IF ( Ekin.GT.3.5D0 ) RETURN
      IF ( Kproj.EQ.8 ) THEN
C-------------------------------- NP ELASTIC SCATTERING----------
         IF ( Ekin.GT.0.740D0 ) THEN
C
C********                                EKIN  .GT.  0.74 GEV
C
            ener = Ekin - 0.66D0
C     IE=ABS(ENER/0.02)
            ie = INT(ener/0.02D0)
            emev = Ekin*1D3
C
            univ = (ener-DBLE(ie)*0.020D0)/0.020D0
            k = ie
            bwfw = (dchn(k+1)-dchn(k))*univ + dchn(k)
            rnd = DT_RNDM(bwfw)
C                                        FORWARD NEUTRON
            IF ( rnd.GE.bwfw ) THEN
               DO k = 10 , 36 , 9
                  IF ( dchna(k).GT.emev ) THEN
                     unive = (emev-dchna(k-9))/(dchna(k)-dchna(k-9))
                     univ = DT_RNDM(unive)
                     DO i = 1 , 8
                        ii = k + i
                        p = (dchna(ii)-dchna(ii-9))*unive + dchna(ii-9)
C
                        IF ( p.GT.univ ) THEN
                           univ = DT_RNDM(unive)
                           flti = DBLE(i) - univ
                           IF ( i.EQ.1 .OR. i.EQ.2 .OR. i.EQ.3 .OR. 
     &                        i.EQ.4 ) GOTO 900
                           IF ( i.EQ.5 ) GOTO 1000
                           IF ( i.EQ.6 ) GOTO 1100
                           IF ( i.EQ.7 ) GOTO 1200
                           IF ( i.EQ.8 ) GOTO 1300
                        END IF
                     END DO
                  END IF
               END DO
C
            ELSE
C                                        BACKWARD NEUTRON
               DO k = 13 , 60 , 12
                  IF ( dchnb(k).GT.emev ) THEN
                     unive = (emev-dchnb(k-12))/(dchnb(k)-dchnb(k-12))
                     univ = DT_RNDM(unive)
                     DO i = 1 , 11
                        ii = k + i
                        p = (dchnb(ii)-dchnb(ii-12))
     &                      *unive + dchnb(ii-12)
C
                        IF ( p.GT.univ ) THEN
                           univ = DT_RNDM(p)
                           flti = DBLE(i) - univ
                           IF ( i.EQ.1 .OR. i.EQ.2 ) GOTO 10
                           IF ( i.EQ.3 ) GOTO 100
                           IF ( i.EQ.4 ) GOTO 200
                           IF ( i.EQ.5 .OR. i.EQ.6 ) GOTO 300
                           IF ( i.EQ.7 ) GOTO 400
                           IF ( i.EQ.8 ) GOTO 500
                           IF ( i.EQ.9 ) GOTO 600
                           IF ( i.EQ.10 ) GOTO 700
                           IF ( i.EQ.11 ) GOTO 800
                        END IF
                     END DO
                  END IF
               END DO
C
 10            Cst = 1.0D-2*flti - 1.0D0
            END IF
         ELSE
            IF ( Ekin.LT.0.300D0 ) THEN
C                                 EKIN .LT. 300 MEV
               idat = 1
            ELSE
C                                 300 MEV < EKIN < 740 MEV
               idat = 6
C
            END IF
            ener = Ekin
            ie = INT(ABS(ener/0.020D0))
            univ = (ener-DBLE(ie)*0.020D0)/0.020D0
C                                            FORWARD/BACKWARD DECISION
            k = idat + 5*ie
            bwfw = (dclin(k+5)-dclin(k))*univ + dclin(k)
            IF ( DT_RNDM(Cst).LT.bwfw ) THEN
               value2 = -1D0
               k = k + 1
            ELSE
               value2 = 1D0
               k = k + 3
C
            END IF
            coef = (dclin(k+5)-dclin(k))*univ + dclin(k)
            rnd = DT_RNDM(coef)
C
            IF ( rnd.LT.coef ) THEN
               Cst = DT_RNDM(rnd)
               Cst = Cst*value2
            ELSE
               r1 = DT_RNDM(Cst)
               r2 = DT_RNDM(r1)
               r3 = DT_RNDM(r2)
               r4 = DT_RNDM(r3)
C
               IF ( value2.GT.0.0 ) THEN
                  Cst = MAX(r1,r2,r3,r4)
               ELSE
                  r5 = DT_RNDM(r4)
C
                  IF ( idat.EQ.1 ) THEN
                     Cst = -MAX(r1,r2,r3,r4,r5)
                  ELSE
                     r6 = DT_RNDM(r5)
                     r7 = DT_RNDM(r6)
                     Cst = -MAX(r1,r2,r3,r4,r5,r6,r7)
C
                  END IF
C
               END IF
C
            END IF
         END IF
         GOTO 1400
      ELSE IF ( Kproj.EQ.1 ) THEN
C
C-----------------------------------  PP ELASTIC SCATTERING -------
C
         emev = Ekin*1D3
C
         IF ( Ekin.LE.0.500D0 ) THEN
            rnd = DT_RNDM(emev)
            Cst = 2.0D0*rnd - 1.0D0
            RETURN
C
         ELSE IF ( Ekin.LT.1.0D0 ) THEN
            DO k = 13 , 60 , 12
               IF ( pdci(k).GT.emev ) THEN
                  unive = (emev-pdci(k-12))/(pdci(k)-pdci(k-12))
                  univ = DT_RNDM(unive)
                  sum = 0
                  DO i = 1 , 11
                     ii = k + i
                     sum = sum + (pdci(ii)-pdci(ii-12))
     &                     *unive + pdci(ii-12)
C
                     IF ( univ.LT.sum ) THEN
                        univ = DT_RNDM(sum)
                        flti = DBLE(i) - univ
                        IF ( i.EQ.1 .OR. i.EQ.2 .OR. i.EQ.3 ) GOTO 1500
                        IF ( i.EQ.4 .OR. i.EQ.5 ) GOTO 1600
                        IF ( i.EQ.6 .OR. i.EQ.7 .OR. i.EQ.8 .OR. 
     &                       i.EQ.9 ) GOTO 1700
                        IF ( i.EQ.10 .OR. i.EQ.11 ) GOTO 1800
                     END IF
                  END DO
               END IF
            END DO
         ELSE
            DO k = 12 , 55 , 11
               IF ( pdch(k).GT.emev ) THEN
                  unive = (emev-pdch(k-11))/(pdch(k)-pdch(k-11))
                  univ = DT_RNDM(unive)
                  sum = 0.0D0
                  DO i = 1 , 10
                     ii = k + i
                     sum = sum + (pdch(ii)-pdch(ii-11))
     &                     *unive + pdch(ii-11)
C
                     IF ( univ.LT.sum ) THEN
                        univ = DT_RNDM(sum)
                        flti = univ + DBLE(i)
                        IF ( i.EQ.1 ) GOTO 20
                        IF ( i.EQ.2 ) GOTO 1500
                        IF ( i.EQ.3 .OR. i.EQ.4 ) GOTO 1600
                        IF ( i.EQ.5 .OR. i.EQ.6 .OR. i.EQ.7 .OR. 
     &                       i.EQ.8 ) GOTO 1700
                        IF ( i.EQ.9 .OR. i.EQ.10 ) GOTO 1800
                     END IF
                  END DO
               END IF
            END DO
C
 20         Cst = 0.4D0*univ
         END IF
         GOTO 1900
      ELSE
C*                                             INVALID REACTION
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,'(A,I5/A)')
     &         ' INVALID PARTICLE TYPE IN DNUPRE - KPROJ=' , Kproj , 
     &        ' COS(THETA) = 1D0 RETURNED'
         RETURN
      END IF
 100  Cst = 2.0D-2*univ - 0.98D0
      GOTO 1400
 200  Cst = 4.0D-2*univ - 0.96D0
      GOTO 1400
 300  Cst = 6.0D-2*flti - 1.16D0
      GOTO 1400
 400  Cst = 8.0D-2*univ - 0.80D0
      GOTO 1400
 500  Cst = 1.0D-1*univ - 0.72D0
      GOTO 1400
 600  Cst = 1.2D-1*univ - 0.62D0
      GOTO 1400
 700  Cst = 2.0D-1*univ - 0.50D0
      GOTO 1400
 800  Cst = 3.0D-1*(univ-1.0D0)
      GOTO 1400
C
 900  Cst = 1.0D0 - 2.5D-2*flti
      GOTO 1400
 1000 Cst = 0.85D0 + 0.5D-1*univ
      GOTO 1400
 1100 Cst = 0.70D0 + 1.5D-1*univ
      GOTO 1400
 1200 Cst = 0.50D0 + 2.0D-1*univ
      GOTO 1400
 1300 Cst = 0.50D0*univ
C
 1400 RETURN
 1500 Cst = 0.2D0*flti
      GOTO 1900
 1600 Cst = 0.3D0 + 0.1D0*flti
      GOTO 1900
 1700 Cst = 0.6D0 + 0.04D0*flti
      GOTO 1900
 1800 Cst = 0.78D0 + 0.02D0*flti
C
 1900 IF ( DT_RNDM(Cst).GT.0.5D0 ) Cst = -Cst
C
      END SUBROUTINE
