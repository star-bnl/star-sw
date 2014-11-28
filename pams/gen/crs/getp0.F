* $Id: getp0.F,v 1.1 1998/02/14 23:02:42 fisyak Exp $
* $Log: getp0.F,v $
* Revision 1.1  1998/02/14 23:02:42  fisyak
* Split Fortran sources
*
      Subroutine GetP0(P_Min,P)

******-****************************************************************-*******
*
*..Description: This program returns a momentum value equal to P_Min
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
      Real*4 P_Min         ! Momentum Value(GeV/c)
*..Output:
      Real*4 P             ! Momentum Value(GeV/c)
*..Local:
      Integer Flag         ! So will do initialization routine only once
*..Saved
      Save Flag
*=====-================================================================-=======
      P=P_Min

      If (Flag.eq.0) then
        Flag=1
*       Report Information to User
        Write(*,*)
        Write(*,*)'Momentum Spectrum Data:'
        Write(*,*)'   This simulation is using a momentum equal'
        Write(*,*)'   to P_Min.'
        Write(*,*)
      Endif

 999  Return
      End
