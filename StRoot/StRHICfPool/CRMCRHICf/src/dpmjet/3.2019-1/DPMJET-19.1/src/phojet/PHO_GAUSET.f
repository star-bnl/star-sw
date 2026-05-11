
      SUBROUTINE PHO_GAUSET(Ax,Bx,Nx,Z,W)
C********************************************************************
C
C     N-point gauss zeros and weights for the interval (AX,BX) are
C           stored in  arrays Z and W respectively.
C
C*********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION alpha , Ax , beta , Bx , delta , W , win , 
     &                 wtemp , Z , zcntr
      INTEGER in , j , jd96 , jmid , jp , jtab , k , m , n , nd96 , Nx
      SAVE 
 
      INCLUDE 'inc/pogdat'
      DIMENSION Z(Nx) , W(Nx)
 
      alpha = 0.5D+00*(Bx+Ax)
      beta = 0.5D+00*(Bx-Ax)
      n = Nx
 
C  the N=1 case:
      IF ( n.EQ.1 ) THEN
         Z(1) = alpha
         W(1) = Bx - Ax
         RETURN
 
C  the Gauss cases:
      ELSE IF ( (n.GT.16) .OR. (n.LE.1) ) THEN
         IF ( n.NE.20 ) THEN
            IF ( n.NE.24 ) THEN
               IF ( n.NE.32 ) THEN
                  IF ( n.NE.40 ) THEN
                     IF ( n.NE.48 ) THEN
                        IF ( n.NE.64 ) THEN
                           IF ( n.NE.80 ) THEN
 
C  the extended Gauss cases:
                              IF ( n.NE.96 ) THEN
 
C  jump to center of intervall intrgration:
                               IF ( (n/96)*96.EQ.n ) THEN
 
C  get ND96 times chained 96 Gauss point array
 
                               CALL PHO_GAUDAT
C  print out message
C     -extract real points
                               k = KTAb(96)
                               nd96 = n/96
                               DO j = 1 , 48
C       extract values from big array
                               jtab = k - 1 + j
                               wtemp = beta*A(jtab)
                               delta = beta*X(jtab)
                               wtemp = wtemp/nd96
                               delta = delta/nd96
                               DO jd96 = 0 , nd96 - 1
                               zcntr = (alpha-beta)
     &                            + beta*DBLE(2*jd96+1)/DBLE(nd96)
C         store them backward
                               Z(j+jd96*96) = zcntr - delta
                               W(j+jd96*96) = wtemp
C         store them forward
                               jp = 96 + 1 - j
                               Z(jp+jd96*96) = zcntr + delta
                               W(jp+jd96*96) = wtemp
                               END DO
                               END DO
                               RETURN
                               ELSE
 
C  the center of intervall cases:
C  put in constant weight and equally spaced central points
                               n = ABS(n)
                               DO in = 1 , n
                               win = (Bx-Ax)/DBLE(n)
                               Z(in) = Ax + (DBLE(in)-.5D+00)*win
                               W(in) = win
                               END DO
                               GOTO 99999
                               END IF
                              END IF
                           END IF
                        END IF
                     END IF
                  END IF
               END IF
            END IF
         END IF
      END IF
 
C  get Gauss point array
 
      CALL PHO_GAUDAT
C  extract real points
      k = KTAb(n)
      m = n/2
      DO j = 1 , m
C       extract values from big array
         jtab = k - 1 + j
         wtemp = beta*A(jtab)
         delta = beta*X(jtab)
C       store them backward
         Z(j) = alpha - delta
         W(j) = wtemp
C       store them forward
         jp = n + 1 - j
         Z(jp) = alpha + delta
         W(jp) = wtemp
      END DO
C     store central point (odd N)
      IF ( (n-m-m).EQ.0 ) RETURN
      Z(m+1) = alpha
      jmid = k + m
      W(m+1) = beta*A(jmid)
 
99999 END SUBROUTINE
