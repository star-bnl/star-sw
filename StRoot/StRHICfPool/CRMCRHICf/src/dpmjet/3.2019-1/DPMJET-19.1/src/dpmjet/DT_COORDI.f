
      SUBROUTINE DT_COORDI(X,Idxsrt,Icsrt,N,R)
 
C***********************************************************************
C Calculation of coordinates of nucleons within nuclei.                *
C        X(3,N)   spatial coordinates of nucleons (in fm)  (output)    *
C        N / R    number of nucleons / radius of nucleus   (input)     *
C Based on the original version by Shmakov et al.                      *
C This version dated 26.10.95 is revised by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION cfe , ct , dist2 , dr , DT_DENSIT , DT_RNDM , 
     &                 eps , f , ONE , ONETHI , pdif , R , r2min , rad , 
     &                 rd , rmax , sfe , sigma , SQRTWO , st
      DOUBLE PRECISION THREE , TWO , TWOPI , wd , X , x1 , x1sum , x2 , 
     &                 x2sum , x3 , x3sum , x4 , ZERO
      INTEGER i , i1 , i2 , Icsrt , Idxsrt , idxz , j , N , NSRT
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ZERO=0.0D0,ONE=1.0D0,TWO=2.0D0,THREE=3.0D0,
     &           ONETHI=ONE/THREE,SQRTWO=1.414213562D0)
 
      PARAMETER (TWOPI=6.283185307179586454D+00)
 
      LOGICAL lstart
 
      PARAMETER (NSRT=10)
      DIMENSION Idxsrt(NSRT,200) , Icsrt(NSRT)
      DIMENSION X(3,260) , wd(4) , rd(3)
 
      DATA pdif/0.545D0/ , r2min/0.16D0/
      DATA wd/0.0D0 , 0.178D0 , 0.465D0 , 1.0D0/
      DATA rd/2.09D0 , 0.935D0 , 0.697D0/
 
      x1sum = ZERO
      x2sum = ZERO
      x3sum = ZERO
 
      IF ( N.EQ.1 ) THEN
         X(1,1) = ZERO
         X(2,1) = ZERO
         X(3,1) = ZERO
      ELSE IF ( N.EQ.2 ) THEN
         eps = DT_RNDM(rd(1))
         DO i = 1 , 3
            IF ( (eps.GE.wd(i)) .AND. (eps.LE.wd(i+1)) ) GOTO 50
         END DO
 50      DO j = 1 , 3
            CALL DT_RANNOR(x1,x2)
            X(j,1) = rd(i)*x1
            X(j,2) = -X(j,1)
         END DO
      ELSE IF ( (N.EQ.3) .OR. (N.EQ.4) ) THEN
         sigma = R/SQRTWO
         lstart = .TRUE.
         CALL DT_RANNOR(x3,x4)
         DO i = 1 , N
            CALL DT_RANNOR(x1,x2)
            X(1,i) = sigma*x1
            X(2,i) = sigma*x2
            IF ( lstart ) THEN
               X(3,i) = sigma*x3
            ELSE
               X(3,i) = sigma*x4
               CALL DT_RANNOR(x3,x4)
            END IF
            lstart = .NOT.lstart
            x1sum = x1sum + X(1,i)
            x2sum = x2sum + X(2,i)
            x3sum = x3sum + X(3,i)
         END DO
         x1sum = x1sum/DBLE(N)
         x2sum = x2sum/DBLE(N)
         x3sum = x3sum/DBLE(N)
         DO i = 1 , N
            X(1,i) = X(1,i) - x1sum
            X(2,i) = X(2,i) - x2sum
            X(3,i) = X(3,i) - x3sum
         END DO
      ELSE
 
C maximum nuclear radius for coordinate sampling
         rmax = R + 4.605D0*pdif
 
C initialize pre-sorting
         DO i = 1 , NSRT
            Icsrt(i) = 0
         END DO
         dr = TWO*rmax/DBLE(NSRT)
 
C sample coordinates for N nucleons
         DO i = 1 , N
 60         rad = rmax*(DT_RNDM(dr))**ONETHI
            f = DT_DENSIT(N,rad,R)
            IF ( DT_RNDM(rad).GT.f ) GOTO 60
C   theta, phi uniformly distributed
            ct = ONE - TWO*DT_RNDM(f)
            st = SQRT((ONE-ct)*(ONE+ct))
            CALL DT_DSFECF(sfe,cfe)
            X(1,i) = rad*st*cfe
            X(2,i) = rad*st*sfe
            X(3,i) = rad*ct
C   ensure that distance between two nucleons is greater than R2MIN
            IF ( i.GE.2 ) THEN
               i1 = i - 1
               DO i2 = 1 , i1
                  dist2 = (X(1,i)-X(1,i2))**2 + (X(2,i)-X(2,i2))
     &                    **2 + (X(3,i)-X(3,i2))**2
                  IF ( dist2.LE.r2min ) GOTO 60
               END DO
            END IF
C   save index according to z-bin
            idxz = INT((X(3,i)+rmax)/dr) + 1
            Icsrt(idxz) = Icsrt(idxz) + 1
            Idxsrt(idxz,Icsrt(idxz)) = i
            x1sum = x1sum + X(1,i)
            x2sum = x2sum + X(2,i)
            x3sum = x3sum + X(3,i)
         END DO
         x1sum = x1sum/DBLE(N)
         x2sum = x2sum/DBLE(N)
         x3sum = x3sum/DBLE(N)
         DO i = 1 , N
            X(1,i) = X(1,i) - x1sum
            X(2,i) = X(2,i) - x2sum
            X(3,i) = X(3,i) - x3sum
         END DO
 
      END IF
 
      END SUBROUTINE
