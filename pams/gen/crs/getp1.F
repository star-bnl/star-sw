* $Id: getp1.F,v 1.1 1998/02/14 23:02:43 fisyak Exp $
* $Log: getp1.F,v $
* Revision 1.1  1998/02/14 23:02:43  fisyak
* Split Fortran sources
*
      Subroutine GetP1(P_Min,P_Max,RNum,P)

******-****************************************************************-*******
*
*..Description: This program accepts a random number and returns
*                 a momentum value associated with that number.
*               The spectrum used in this method is:
*                   Intensity=(Constant/p^2)
*               The limits on this equation are P_Min and P_Max.  The
*                 equation has been solve explicitly so that the solution
*                 is an analytical one.
*
*               How it is done:
*                i) Integrate the 1/p^2 from P_Min to P_Max to find the
*                   normalization constant.
*
*               ii) Using the normalization constant now integrate the
*                   constant/p^2 from P_Min to x to get the enclosed area(the
*                   random number) and solve for x
*
*                    /- x   constant                    (  1      1 )
*                    |      -------- dp = u => constant*(----- - ---)
*                   _/ P_Min  p^2                       (P_Min    x )
*
*               iii) The solution is easy and analytical:
*                        (  1         u    )-1
*                    x=P=(----- -  --------)
*                        (P_Min    constant)
*
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
      Real*4 RNum          ! Random Number
      Real*4 P_Min,P_Max   ! Momentum Cutoff(GeV/c)
*..Output:
      Real*4 P             ! Momentum (GeV/c)
*..Local:
      Integer Flag         ! So will do initialization routine only once

                           ! (These are inverse to speed calculation)
      Real*4 IPmax,IPmin   ! Inverse P maximum and minimum (GeV/c)
      Real*4 Iconstant     ! Inverse Normalization Constant
*..Saved
      Save Flag,IPmin,IPMax,Iconstant
*=====-================================================================-=======
*Initialization Routine
      If (Flag.eq.0) then
        Flag=1
        IPmin=1.0d0/P_Min         !Inverse PMin
        IPmax=1.0d0/P_Max         !Inverse PMax
        Iconstant=(IPmin - IPMax) !Inverse constant

* Report Information to User
        Write(*,*)
        Write(*,*)'Momentum Spectrum Data:'
        Write(*,*)'   This simulation is using a momentum spectrum'
        Write(*,*)'   based on the 1/p^2 differential spectrum curve.'
        Write(*,*)'   The curve is normalized between the P_Max and'
        Write(*,*)'   P_Min limits.'
        Write(*,*)'   The momentum spectrum is valid for any values of'
        Write(*,*)'   of P but diverges from the actual spectrum at'
        Write(*,*)'   low values of momentum(<1.0GeV/c).'
        Write(*,*)
      Endif

      P=1.0d0/(IPmin-RNum*Iconstant)    !Finds the momentum from Rnum

 999  Return
      End
