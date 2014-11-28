* $Id: getp3.F,v 1.1 1998/02/14 23:02:44 fisyak Exp $
* $Log: getp3.F,v $
* Revision 1.1  1998/02/14 23:02:44  fisyak
* Split Fortran sources
*
      Subroutine GetP3(P_Min,P_Max,Rnum,Theta_R,P)

******-****************************************************************-*******
*
*..Description: Dar Inv Spectrum
*                 This method uses data tabulated in bins of integrated
*                   momentum vs angle theta.  That data for this subroutine
*                   comes from a BASIC program(darinv3.exe) that was given
*                   to us by Prof. Henry W. Kendall at Massachusetts Institute
*                   of Technology.  It calculates the momentum spectrum vs
*                   zenith angle using Arnon Dar's data from:
*                     Cosmic Ray Muons at Gound Level and Deep Underground
*                     By Arnon Dar               1984
*                     TECHNION-PHYS-84-41
*                     Department of Physics
*                     Technion _ Israel Institute of Technology
*                     Haifa, Israel
*                   The program (darinv3.exe) states that if the zenith angle
*                   is chosen with a Cos^2 this will yield the total momentum
*                   energy distribution for the given zenith angle.
*                   The data is tabulated in 1000 bins in each of 19 files.
*                   The files are tabulated for every 5 degrees of zenith
*                   angle.  This program takes the four bins nearest to each
*                   random number that are in the four theta files nearest
*                   to the current Theta_D value.  It then separately
*                   interpolates each Theta curve to find the momentum value
*                   on that curve then interpolates the four Theta curves
*                   to find the momentum of the event.  Basically it is a
*                   two dimensional interpolation
*                 The valid range for momentum values is between 1 and
*                   1000 GeV/c.
*
*..Author     : Michael P Bringle
*               Lawrence Berkeley Lab
*
*..History    : First Version August 5, 1994
*               Second Version August 19, 1994
*               Last-bin anomaly fixed, August 23, 1994.
*
******-****************************************************************-*******
      Implicit None
*..Input:
      Real*4 P_Min,P_Max        !Momentum cutoffs(GeV/c)
      Real*4 Rnum               !Random Number
      Real*4 Theta_R            !Theta_E in Radians
*..Output:
      Real*4 P                  !Momentum value(GeV/c)
*..Local:
      Integer Bins              !Number of bins in each file
      Parameter (Bins=1001)

      Integer L1,L2             !Do loop variables
      Integer Th_Indx,Rn_Indx   !Bin indexes for Theta and Rnum bins
      Integer Flag              !Initialization flag
      Real*4 Theta_D            !Theta_E in degrees
      Real*4 ISpc(Bins,0:18)    !Integrated Spectrum value for a given bin
                                !  number and theta index.
      Real*4 BNum(Bins,0:18)    !Bin number for random number indexing that
                                !  corresponds to ISpc(#1,#2)
      Real*4 X4(4),Y4(4)        !Interpolation points
      Real*4 T4(4),P4(4)        !Interpolation points

      Real*4 Rad_Deg            !Constant
      Parameter (Rad_Deg=180.0d0/3.14159265358979323846264338328)
*..Funct:
      Real*4 IntrPol            !An interpolation Routine
      External IntrPol
*..Saved:
      Save ISpc,BNum            !Save integrated spectrum
      Save Flag                 !Save so initialization routine done only once
*=====-================================================================-=======
*First Put Theta in degrees
      Theta_D=Acos(Abs(Cos(Theta_R)))*Rad_Deg !We want theta in degrees

*Second find the bin associated with our random number
      Rnum=Rnum*(Bins-1)+1  !We want Rnum 1 -> 1000

*Get the integrated Spectrum
      If (Flag.eq.0) then
        Call GetP3Ini(Bins,P_Min,P_Max,BNum,ISpc)
        Flag=1
      Endif

*Take Theta value and find which theta bins it lies between
      Th_Indx=Int(Theta_D/5.0d0)-2
      If (Th_Indx.lt.-1) Th_Indx=-1
      If (Th_Indx.gt.14) Th_Indx=14

      
*Take Random number and find which P bin it lies between taking into
*    account the theta bins just selected
      Do L1=1, 4
        Rn_Indx=Int(Rnum)-2
        If (Rn_Indx.lt.0) Rn_Indx=0
        If (Rn_Indx.gt.Bins-4) Rn_Indx=Bins-4
        If (RNum.gt.999.and.RNum.lt.1000) RN_Indx=RN_Indx-1
        Do L2=1,4
          X4(L2)=BNum(Rn_Indx+L2,Th_Indx+L1)
          Y4(L2)=ISPC(Rn_Indx+L2,Th_Indx+L1)
        Enddo

        P4(L1)=IntrPol(3,RNum,X4,Y4)
        T4(L1)=(Th_Indx+L1)*5.0
      Enddo

      P=IntrPol(3,Theta_D,T4,P4)

      Return
      End
