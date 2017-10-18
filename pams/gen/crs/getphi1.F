* $Id: getphi1.F,v 1.1 1998/02/14 23:02:44 fisyak Exp $
* $Log: getphi1.F,v $
* Revision 1.1  1998/02/14 23:02:44  fisyak
* Split Fortran sources
*
      Subroutine Getphi1(Rnum,Phi_E)

******-****************************************************************-*******
*
*..Description: This program gets a randomly distributed phi angle in
*                 the Earth frame of reference.  It is assumed that there
*                 is no preference in the phi direction so the entire
*                 range 0 -> 2Pi has equal probability.
*
*..Author     : Michael P Bringle
*               Lawrence Berkeley Lab
*
*..History    : First Version August 5, 1994
*               Second Version August 19, 1994
*
******-****************************************************************-*******
      Implicit none
*..Input:
      Real*4 Rnum       ! Random Number
*..Output:
      Real*4 Phi_E      ! Phi Angle(Earth)
*..Local:
      Real*4 Pi         !Constant
      Parameter (Pi=3.14159265358979323846264338328)
*=====-================================================================-=======
      Phi_E=Rnum*2.0d0*Pi

 999  Return
      End
