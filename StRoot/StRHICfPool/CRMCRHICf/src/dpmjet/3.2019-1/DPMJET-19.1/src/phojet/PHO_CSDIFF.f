
      SUBROUTINE PHO_CSDIFF(Id1,Id2,Ss,Xi_min,Xi_max,Sig_sd1,Sig_sd2,
     &                      Sig_dd)
C***********************************************************************
C
C     cross section for diffraction dissociation according to
C     Goulianos' parametrization (Ref: PL B358 (1995) 379)
C
C     in addition rescaling for different particles is applied using
C     internal rescaling tables (not implemented yet)
C
C     input:     Id1/2       PDG ID's of incoming particles
C                SS          squared c.m. energy (GeV**2)
C                Xi_min      min. diff mass (squared) = Xi_min*SS
C                Xi_max      max. diff mass (squared) = Xi_max*SS
C
C     output:    sig_sd1     cross section for diss. of particle 1 (mb)
C                sig_sd2     cross section for diss. of particle 2 (mb)
C                sig_dd      cross section for diss. of both particles
C
C***********************************************************************
 
      IMPLICIT NONE
 
      SAVE 
 
      INTEGER Id1 , Id2
      DOUBLE PRECISION Ss , Xi_min , Xi_max , Sig_sd1 , Sig_sd2 , Sig_dd
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  some constants
      INCLUDE 'inc/pocons'
 
      DOUBLE PRECISION xpos1(96) , xwgh1(96) , xpos2(96) , xwgh2(96)
      DOUBLE PRECISION delta , alphap , beta0 , gpom0 , xm_p , x_rad2 , 
     &                 xm4_p2 , fac , tt , t1 , t2 , tl , tu , xnorm , 
     &                 xi , xil , xiu , w_xi , alpha_t , f2_t , xms_1 , 
     &                 xms_2 , csdiff
 
      INTEGER ngau1 , ngau2 , i1 , i2
 
C  model parameters
 
      DATA delta/0.104D0/
      DATA alphap/0.25D0/
      DATA beta0/6.56D0/
      DATA gpom0/1.21D0/
      DATA xm_p/0.938D0/
      DATA x_rad2/0.71D0/
 
C  integration precision
 
      DATA ngau1/96/
      DATA ngau2/96/
 
      Sig_sd1 = 0.D0
      Sig_sd2 = 0.D0
      Sig_dd = 0.D0
 
      IF ( (ABS(Id1).EQ.2212) .AND. (ABS(Id2).EQ.2212) ) THEN
 
         xm4_p2 = 4.D0*xm_p**2
         fac = beta0**2/(16.D0*PI)
 
         t1 = -5.D0
         t2 = 0.D0
         tl = x_rad2/3.D0/(1.D0-t1/x_rad2)**3
         tu = x_rad2/3.D0/(1.D0-t2/x_rad2)**3
 
C  flux renormalization and cross section
 
         xnorm = 0.D0
 
         xil = LOG(1.5D0/Ss)
         xiu = LOG(0.1D0)
 
         IF ( xiu.GT.xil ) THEN
 
            CALL PHO_GAUSET(xil,xiu,ngau1,xpos1,xwgh1)
            CALL PHO_GAUSET(tl,tu,ngau2,xpos2,xwgh2)
 
            DO i1 = 1 , ngau1
 
               xi = EXP(xpos1(i1))
               w_xi = xwgh1(i1)
 
               DO i2 = 1 , ngau2
 
                  tt = x_rad2 - x_rad2*(x_rad2/(3.D0*xpos2(i2)))
     &                 **(1.D0/3.D0)
 
                  alpha_t = 1.D0 + delta + alphap*tt
                  f2_t = ((xm4_p2-2.8D0*tt)/(xm4_p2-tt))**2
 
                  xnorm = xnorm + f2_t*xi**(2.D0-2.D0*alpha_t)*xwgh2(i2)
     &                    *w_xi
 
               END DO
            END DO
 
            xnorm = xnorm*fac
         END IF
 
 
         xil = LOG(Xi_min)
         xiu = LOG(Xi_max)
 
         t1 = -5.D0
         t2 = 0.D0
 
         tl = x_rad2/3.D0/(1.D0-t1/x_rad2)**3
         tu = x_rad2/3.D0/(1.D0-t2/x_rad2)**3
 
C  single diffraction diss. cross section
 
         csdiff = 0.D0
 
         IF ( xiu.GT.xil ) THEN
 
            CALL PHO_GAUSET(xil,xiu,ngau1,xpos1,xwgh1)
            CALL PHO_GAUSET(tl,tu,ngau2,xpos2,xwgh2)
 
            DO i1 = 1 , ngau1
 
               xi = EXP(xpos1(i1))
               w_xi = xwgh1(i1)*beta0*gpom0*(xi*Ss)**delta
 
               DO i2 = 1 , ngau2
 
                  tt = x_rad2 - x_rad2*(x_rad2/(3.D0*xpos2(i2)))
     &                 **(1.D0/3.D0)
 
                  alpha_t = 1.D0 + delta + alphap*tt
                  f2_t = ((xm4_p2-2.8D0*tt)/(xm4_p2-tt))**2
 
                  csdiff = csdiff + f2_t*xi**(2.D0-2.D0*alpha_t)
     &                     *xwgh2(i2)*w_xi
 
               END DO
            END DO
 
            csdiff = csdiff*fac*GEV2mb/MAX(1.D0,xnorm)
 
C       WRITE(LO,'(1x,1p,4e14.3)')
C    &    sqrt(SS),Xnorm,2.*CSdiff*MAX(1.d0,Xnorm),2.*CSdiff
 
            Sig_sd1 = csdiff
            Sig_sd2 = csdiff
         END IF
 
 
C  double diffraction dissociation cross section
 
         csdiff = 0.D0
 
         xil = LOG(1.5D0/Ss)
         xiu = LOG(Xi_max/1.5D0)
 
         IF ( xiu.GT.xil ) THEN
 
            fac = (beta0*gpom0*Ss**delta/(4.D0*SQRT(PI)*MAX(1.D0,xnorm))
     &            )**2/(2.D0*alphap)
 
            CALL PHO_GAUSET(xil,xiu,ngau1,xpos1,xwgh1)
 
            DO i1 = 1 , ngau1
 
               xi = EXP(xpos1(i1))
               xms_1 = xi*Ss
 
               xiu = LOG(Xi_max/(xi*Ss))
 
               IF ( xil.LT.xiu ) THEN
 
                  CALL PHO_GAUSET(xil,xiu,ngau2,xpos2,xwgh2)
 
                  DO i2 = 1 , ngau2
 
                     xms_2 = EXP(xpos2(i2))*Ss
                     csdiff = csdiff + 
     &                        1.D0/((xms_1*xms_2)**delta*LOG(Ss/
     &                        (xms_1*xms_2)))*xwgh1(i1)*xwgh2(i2)
 
                  END DO
 
               END IF
 
            END DO
 
            Sig_dd = csdiff*fac*GEV2mb
         END IF
 
 
      ELSE
 
         IF ( LPRi.GT.4 ) WRITE (LO,'(1x,2a,2I8)') 'PHO_CSDIFF: ' , 
     &        'invalid particle combination (Id1/2)' , Id1 , Id2
 
      END IF
 
      END SUBROUTINE
