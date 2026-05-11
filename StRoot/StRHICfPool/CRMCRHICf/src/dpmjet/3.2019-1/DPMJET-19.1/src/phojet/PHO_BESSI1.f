
      DOUBLE PRECISION FUNCTION PHO_BESSI1(X)
C**********************************************************************
C
C      Bessel Function I1
C
C**********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION ax , besli1 , X , y
      SAVE 
 
      ax = ABS(X)
 
      IF ( ax.LT.3.75D0 ) THEN
         y = (X/3.75D0)**2
         besli1 = ax*
     &            (0.5D0+y*(0.87890594D0+y*(0.51498869D0+y*(0.15084934D0
     &            +y*(0.2658733D-1+y*(0.301532D-2+y*0.32411D-3))))))
      ELSE
         y = 3.75D0/ax
         besli1 = 0.2282967D-1 + 
     &            y*(-0.2895312D-1+y*(0.1787654D-1-y*0.420059D-2))
         besli1 = 0.39894228D0 + 
     &            y*(-0.3988024D-1+y*(-0.362018D-2+y*(0.163801D-2+
     &            y*(-0.1031555D-1+y*besli1))))
         besli1 = besli1*EXP(ax)/SQRT(ax)
      END IF
      IF ( X.LT.0.D0 ) besli1 = -besli1
 
      PHO_BESSI1 = besli1
 
      END FUNCTION
