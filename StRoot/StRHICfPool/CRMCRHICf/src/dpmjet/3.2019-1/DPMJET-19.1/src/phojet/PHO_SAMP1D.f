
      SUBROUTINE PHO_SAMP1D(Imode,X_inp,F_inp,F_int,N_dim,X_out)
C***********************************************************************
C
C     Monte Carlo sampling from arbitrary 1d distribution
C     (linear interpolation to improve reproduction of initial function)
C
C     input: Imode          -1  initialization
C                            1  sampling (after initialization)
C            X_inp(N_dim)   array with x values
C            F_inp(N_dim)   array with function values
C            F_int(N_dim)   array with integral
C
C     output:  X_out        sampled value (Imode=1)
C
C                                                 (R.E. 10/99)
C
C***********************************************************************
      IMPLICIT NONE
      SAVE 
 
C  input/output channels
      INCLUDE 'inc/poinou'
 
      INTEGER Imode , N_dim
      DOUBLE PRECISION X_inp , F_inp , F_int , X_out
      DIMENSION X_inp(N_dim) , F_inp(N_dim) , F_int(N_dim)
 
C  local variables
      INTEGER i
      DOUBLE PRECISION dum , xi , a , b
 
C  external functions
      DOUBLE PRECISION DT_RNDM
      EXTERNAL DT_RNDM
 
      IF ( Imode.EQ.-1 ) THEN
 
C  initialization
 
         F_int(1) = 0.D0
         DO i = 2 , N_dim
            F_int(i) = F_int(i-1) + 0.5D0*(F_inp(i)+F_inp(i-1))
     &                 *(X_inp(i)-X_inp(i-1))
         END DO
 
      ELSE IF ( Imode.EQ.1 ) THEN
 
C  sample from previously calculated integral
 
         xi = DT_RNDM(dum)*F_int(N_dim)
 
         DO i = 2 , N_dim
            IF ( xi.LT.F_int(i) ) THEN
               a = (F_inp(i)-F_inp(i-1))/(X_inp(i)-X_inp(i-1))
               b = F_inp(i) - a*X_inp(i)
               xi = xi - F_int(i-1) + 0.5D0*a*X_inp(i-1)
     &              **2 + b*X_inp(i-1)
               X_out = (SQRT(b**2+2.D0*a*xi)-b)/a
               RETURN
            END IF
         END DO
         X_out = X_inp(N_dim)
 
      ELSE
 
C  invalid option Imode
 
         IF ( LPRi.GT.4 ) WRITE (LO,'(1x,a,i6)')
     &         'PHO_SAMP1D: invalid option Imode: ' , Imode
         X_out = 0.D0
 
      END IF
 
      END SUBROUTINE
