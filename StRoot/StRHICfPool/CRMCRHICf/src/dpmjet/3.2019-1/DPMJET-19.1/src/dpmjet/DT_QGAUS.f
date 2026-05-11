
      SUBROUTINE DT_QGAUS(A,B,Ss,Enu,Ltyp)
 
      IMPLICIT NONE
      DOUBLE PRECISION A , B , DT_DSQEL_Q2 , dx , Enu , Ss , w , x , 
     &                 xm , xr
      INTEGER j , Ltyp
      SAVE 
 
      DIMENSION x(5) , w(5)
      DATA x/.1488743389D0 , .4333953941D0 , .6794095682D0 , 
     &     .8650633666D0 , .9739065285D0/
      DATA w/.2955242247D0 , .2692667193D0 , .2190863625D0 , 
     &     .1494513491D0 , .0666713443D0/
      xm = 0.5D0*(B+A)
      xr = 0.5D0*(B-A)
      Ss = 0
      DO j = 1 , 5
         dx = xr*x(j)
         Ss = Ss + w(j)
     &        *(DT_DSQEL_Q2(Ltyp,Enu,xm+dx)+DT_DSQEL_Q2(Ltyp,Enu,xm-dx))
      END DO
      Ss = xr*Ss
 
      END SUBROUTINE
