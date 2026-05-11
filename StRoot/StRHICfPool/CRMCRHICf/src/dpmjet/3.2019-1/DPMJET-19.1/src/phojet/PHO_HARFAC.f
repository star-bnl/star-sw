
      SUBROUTINE PHO_HARFAC(Ptcut,Ecmi)
C*********************************************************************
C
C     initialization: find scaling factors and maxima of remaining
C                     weights
C
C     input:   PTCUT  transverse momentum cutoff
C              ECMI   cms energy
C
C     output:  Hfac(-1:Max_pro_2)  field for sampling hard processes
C
C*********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION absz , aln , chrnf , Ecmi , f1 , f124 , f2 , 
     &                 fac , faxx , fff , fww , fww1 , fww2 , hln , 
     &                 Ptcut , s , s1 , s2 , ss , ua
      DOUBLE PRECISION ub , uc , ue , va , vb , vc , ve , weig , wl , 
     &                 wlog , z , z1 , z2 , zz
      INTEGER i , i1 , i2 , m , MXABWT , npoint
      SAVE 
 
      PARAMETER (MXABWT=96)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  current beam selection
      INCLUDE 'inc/pobeam'
C  data of c.m. system of Pomeron / Reggeon exchange
      INCLUDE 'inc/popcms'
C  some constants
      INCLUDE 'inc/pocons'
C  hard scattering parameters used for most recent hard interaction
      INCLUDE 'inc/pohapa'
C  integration precision for hard cross sections (obsolete)
      INCLUDE 'inc/pogaup'
C  data on most recent hard scattering
      INCLUDE 'inc/pockin'
C  hard cross sections and MC selection weights
      INCLUDE 'inc/pohrcs'
 
      DIMENSION absz(MXABWT) , weig(MXABWT)
      DIMENSION s(-1:MAX_PRO_2) , s1(-1:MAX_PRO_2) , s2(-1:MAX_PRO_2) , 
     &          f124(-1:MAX_PRO_2)
      DATA f124/1.D0 , 0.D0 , 4.D0 , 2.D0 , 2.D0 , 2.D0 , 4.D0 , 1.D0 , 
     &     4.D0 , 4.D0 , 0.D0 , 1.D0 , 2.D0 , 1.D0 , 2.D0 , 1.D0 , 
     &     0.D0 , 1.D0/
 
      ss = Ecmi*Ecmi
      AH = (2.D0*Ptcut/Ecmi)**2
      aln = LOG(AH)
      hln = LOG(0.5D0)
      npoint = NGAuin
      CALL PHO_GAUSET(0.D0,1.D0,npoint,absz,weig)
      DO m = -1 , MAX_PRO_2
         s1(m) = 0.D0
      END DO
 
C  resolved processes
      DO i1 = 1 , npoint
         z1 = absz(i1)
         X1 = EXP(aln*z1)
         DO m = -1 , 9
            s2(m) = 0.D0
         END DO
 
         DO i2 = 1 , npoint
            z2 = (1.D0-z1)*absz(i2)
            X2 = EXP(aln*z2)
            faxx = AH/(X1*X2)
            W = SQRT(1.D0-faxx)
            W1 = faxx/(1.+W)
            wlog = LOG(W1)
            fww = faxx*wlog/W
            DO m = -1 , 9
               s(m) = 0.D0
            END DO
 
            DO i = 1 , npoint
               z = absz(i)
               va = -0.5D0*W1/(W1+z*W)
               ua = -1.D0 - va
               vb = -0.5D0*faxx/(W1+2.D0*W*z)
               ub = -1.D0 - vb
               vc = -EXP(hln+z*wlog)
               uc = -1.D0 - vc
               ve = -0.5D0*(1.D0+W) + z*W
               ue = -1.D0 - ve
               s(1) = s(1) + (1.+W)
     &                *2.25*(va*va*(3.-ua*va-va/(ua*ua))-ua)*weig(i)
               s(2) = s(2) + (vc*vc+uc*uc)*((16./27.)/uc-(4./3.)*vc)
     &                *fww*weig(i)
               s(3) = s(3) + (1.+W)*(1.+ua*ua)*(1.-(4./9.)*va*va/ua)
     &                *weig(i)
               s(5) = s(5) + ((4./9.)*(1.+ub*ub+(ub*ub+vb*vb)*vb*vb)
     &                -(8./27.)*ua*ua*va)*weig(i)
               s(6) = s(6) + (4./9.)*(ue*ue+ve*ve)*faxx*weig(i)
               s(7) = s(7) + (1.+W)
     &                *((2./9.)*(1.+ua*ua+(1.+va*va)*va*va/(ua*ua))
     &                -(4./27.)*va/ua)*weig(i)
               s(8) = s(8) + (4./9.)*(1.+ub*ub)*weig(i)
               s(-1) = s(-1) + (1.+vc*vc)*(vc/(uc*uc)-(4./9.))
     &                 *fww*weig(i)
            END DO
            s(4) = s(2)*(9./32.)
            DO m = -1 , 8
               s2(m) = s2(m) + s(m)*weig(i2)*W
            END DO
         END DO
         DO m = -1 , 8
            s1(m) = s1(m) + s2(m)*(1.D0-z1)*weig(i1)
         END DO
      END DO
      s1(4) = s1(4)*NF
      s1(6) = s1(6)*MAX(0,NF-1)
C
C  direct processes
      IF ( (IDPdg1.EQ.22) .OR. (IDPdg1.EQ.990) .OR. (IDPdg2.EQ.22) .OR. 
     &     (IDPdg2.EQ.990) ) THEN
         DO i1 = 1 , npoint
            z2 = absz(i1)
            X2 = EXP(aln*z2)
            faxx = AH/X2
            W = SQRT(1.D0-faxx)
            W1 = faxx/(1.D0+W)
            wlog = LOG(W1)
            wl = LOG(faxx/(1.D0+W)**2)
            fww1 = faxx*wl/aln
            fww2 = faxx*wlog/aln
            DO m = 10 , 12
               s(m) = 0.D0
            END DO
C
            DO i = 1 , npoint
               z = absz(i)
               ua = -(1.D0+W)/2.D0*EXP(z*wl)
               va = -1.D0 - ua
               vb = -EXP(hln+z*wlog)
               ub = -1.D0 - vb
               s(10) = s(10) + (8.D0/3.D0)*(1.D0+ua*ua)*weig(i)*fww1
               s(11) = s(11) - (vb*vb+ub*ub)/ub*weig(i)*fww2
            END DO
            DO m = 10 , 11
               s1(m) = s1(m) + s(m)*weig(i1)
            END DO
         END DO
         s1(12) = s1(10)
         s1(13) = s1(11)
C  quark charges fractions
         IF ( IDPdg1.EQ.22 ) THEN
            chrnf = 0.D0
            DO i = 1 , NF
               chrnf = chrnf + Q_Ch2(i)
            END DO
            s1(11) = s1(11)*chrnf
         ELSE IF ( IDPdg1.EQ.990 ) THEN
            s1(11) = s1(11)*NF
         ELSE
            s1(11) = 0.D0
         END IF
         IF ( IDPdg2.EQ.22 ) THEN
            chrnf = 0.D0
            DO i = 1 , NF
               chrnf = chrnf + Q_Ch2(i)
            END DO
            s1(13) = s1(13)*chrnf
         ELSE IF ( IDPdg2.EQ.990 ) THEN
            s1(13) = s1(13)*NF
         ELSE
            s1(13) = 0.D0
         END IF
      END IF
C
C  global factors
      fff = PI*GEV2mb*aln*aln/(AH*ss)
      DO m = -1 , MAX_PRO_2
         HFAc(m,IDXmpar) = MAX(fff*f124(m)*s1(m),0.D0)
      END DO
C
C  double direct process
      IF ( ((IDPdg1.EQ.22) .OR. (IDPdg1.EQ.990)) .AND. 
     &     ((IDPdg2.EQ.22) .OR. (IDPdg2.EQ.990)) ) THEN
         fac = 0.D0
         DO i = 1 , NF
            IF ( IDPdg1.EQ.22 ) THEN
               f1 = Q_Ch2(i)
            ELSE
               f1 = 1.D0
            END IF
            IF ( IDPdg2.EQ.22 ) THEN
               f2 = Q_Ch2(i)
            ELSE
               f2 = 1.D0
            END IF
            fac = fac + f1*f2*3.D0
         END DO
         zz = SQRT(1.D0-4.D0*Ptcut*Ptcut/ss)
         HFAc(14,IDXmpar) = 4.D0*PI/ss*(LOG((1.D0+zz)/(1.D0-zz))-zz)
     &                      *GEV2mb*fac
      END IF
      END SUBROUTINE
