* $Id: getp2.F,v 1.1 1998/02/14 23:02:43 fisyak Exp $
* $Log: getp2.F,v $
* Revision 1.1  1998/02/14 23:02:43  fisyak
* Split Fortran sources
*
      Subroutine GetP2(P_Min,P_Max,Rnum,P)

******-****************************************************************-*******
*
*..Description:  Physics Data spectrum
*                  This method uses measured flux values of the Muon
*                  spectrum vs momentum to calculate the momentum
*                  distribution.  The data was taken from:
*                    Physics Data 25-1 'Cosmic Rays on Earth' (1984)
*                     (ISBN 0344-8401)   O.C. Allkofer and P.K.F. Grieder
*                     Table 2.2.3              page 241
*                  The data points were fit to a curve and that curve is
*                  used in the calculation of the momentum spectrum.  The
*                  curve was integrated numerically(see corresponding
*                  subroutine) to get the integrated spectrum.  The array
*                  is searched and when the closest bins to the random
*                  number are found the actual data point is then found by
*                  cubic interpolation.
*
*..Author     : Michael P Bringle
*               Lawrence Berkeley Lab
*
*..History    : First Version August 5, 1994
*               Second Version August 19, 1994
*
******-****************************************************************-*******
      Implicit None
*..Input:
      Real*4 Rnum               ! Random Number
      Real*4 P_Max,P_Min        ! Momentum Range(GeV/c)
*..Output:
      Real*4 P                  ! Momentum(GeV/c)
*..Local:
      Integer Bins              ! Number of divisions to break the
                                !   momentum spectrum into.
      Parameter (Bins=2000)

      Integer L1                ! Loop Variable
      Integer Flag              ! Initialization flag so only called once
      Integer Indx              ! Stores first Interpolation bin number
      Real*4 X4(4),Y4(4)        ! Store points for Interpolation
      Real*8 F(Bins)            ! Integrated Spectrum values
      Real*8 X_F(Bins)          ! P values for a given F(#)
*..Funct:
      Real*4 IntrPol            ! An interpolation routine
      External IntrPol
*..Saved:
      Save Flag                 ! Save so initialization routine only done once
      Save F,X_F                ! Save for use on later calls
*=====-================================================================-=======
*     Get the integrated Spectrum
      If (Flag.eq.0) then
        Call GetP2Ini(Bins,P_Min,P_Max,F,X_F)
        Flag=1
      Endif

* Take Random number and find which bins it lies between
      Do L1=3,Bins-4
        If (RNum.lt.F(L1)) then
          Indx=L1-3
          goto 100
        Endif
      Enddo

 100  Do L1=1,4
        X4(L1)=F(Indx+L1)
        Y4(L1)=X_F(Indx+L1)
      Enddo

      P=IntrPol(3,Rnum,X4,Y4) !Cubic Interpolation

      End
