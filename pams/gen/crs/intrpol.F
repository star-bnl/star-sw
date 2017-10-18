* $Id: intrpol.F,v 1.2 1998/02/14 23:02:46 fisyak Exp $
* $Log: intrpol.F,v $
* Revision 1.2  1998/02/14 23:02:46  fisyak
* Split Fortran sources
*
      Real*4 Function IntrPol(N,X,X_Points,Y_Points)

******-****************************************************************-******
*
*..Description: Polynomial Interpolation
*                 This function performs a N-1 order interpolation(or
*                 extrapolation)
*
*..Author     : Michael P Bringle
*               Lawrence Berkeley Lab
*
*..History    : First Version August 5, 1994
*
******-****************************************************************-*******
      Implicit none
*..Input:
      Integer N                         !Degree of polynomial fit
      Real*4 X                          !Value to Interpolate
      Real*4 X_Points(N+1),Y_Points(N+1)    !Arrays holding data points
*..Local:
      Integer L1,L2                     !Do Loop Variables
      Real*4 Ztot,Z
*=====-================================================================-=======
      Ztot=0.0d0

      Do L1=1, N+1
        Z=1.0d0
        Do L2=1, N+1
          If (L1.ne.L2)
     +      Z=Z*(X-X_Points(L2)) / (X_Points(L1)-X_Points(L2))
        Enddo
        Ztot=Ztot+Z*Y_Points(L1)
      Enddo

      IntrPol=Ztot

      Return
      End
