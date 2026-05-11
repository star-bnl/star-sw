
      SUBROUTINE DT_GETBIN(Ihis,Ibin,Kevt,Norm,Xlow,Xhi,Xmean,Ymean,
     &                     Yerr)
 
C***********************************************************************
C This version dated 23.4.95 is written  by S. Roesler.                *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION dx , ONE , TINY35 , Xhi , Xlow , Xmean , Yerr , 
     &                 Ymean , ymean2 , ysum , ZERO
      INTEGER Ibin , Ihis , Kevt , nevt , Norm
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ZERO=0.0D0,ONE=1.0D0,TINY35=1.0D-35)
 
C histograms
 
 
      INCLUDE 'inc/dthis1'
 
      Xlow = HISt(1,Ihis,Ibin)
      Xhi = HISt(1,Ihis,Ibin+1)
      IF ( (ISWi(Ihis).EQ.3) .OR. (ISWi(Ihis).EQ.4) ) THEN
         Xlow = 10**Xlow
         Xhi = 10**Xhi
      END IF
      IF ( Norm.EQ.2 ) THEN
         dx = Xhi - Xlow
         nevt = INT(DENtry(1,Ihis))
      ELSE IF ( Norm.EQ.3 ) THEN
         dx = ONE
         nevt = INT(HISt(2,Ihis,Ibin))
      ELSE IF ( Norm.EQ.4 ) THEN
         dx = Xhi**2 - Xlow**2
         nevt = Kevt
      ELSE IF ( Norm.EQ.5 ) THEN
         dx = LOG(ABS(Xhi)) - LOG(ABS(Xlow))
         nevt = Kevt
      ELSE IF ( Norm.EQ.6 ) THEN
         dx = ONE
         nevt = Kevt
      ELSE IF ( Norm.EQ.7 ) THEN
         dx = ONE
         nevt = INT(HISt(7,Ihis,Ibin))
      ELSE IF ( Norm.EQ.8 ) THEN
         dx = Xhi - Xlow
         nevt = INT(DENtry(2,Ihis))
      ELSE
         dx = ABS(Xhi-Xlow)
         nevt = Kevt
      END IF
      IF ( ABS(dx).LT.TINY35 ) dx = ONE
      nevt = MAX(nevt,1)
      Ymean = HISt(5,Ihis,Ibin)/dx/DBLE(nevt)
      ymean2 = HISt(6,Ihis,Ibin)/dx**2/DBLE(nevt)
      Yerr = SQRT(ABS(ymean2-Ymean**2))/SQRT(DBLE(nevt))
      ysum = HISt(5,Ihis,Ibin)
      IF ( ABS(ysum).LT.TINY35 ) ysum = ONE
C     XMEAN  = HIST(3,IHIS,IBIN)/YSUM/MAX(HIST(2,IHIS,IBIN),ONE)
      Xmean = HISt(3,Ihis,Ibin)/ysum
      IF ( Xmean.EQ.ZERO ) Xmean = Xlow
 
      END SUBROUTINE
