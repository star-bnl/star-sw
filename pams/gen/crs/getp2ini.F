* $Id: getp2ini.F,v 1.1 1998/02/14 23:02:43 fisyak Exp $
* $Log: getp2ini.F,v $
* Revision 1.1  1998/02/14 23:02:43  fisyak
* Split Fortran sources
*
      Subroutine GetP2Ini(Bins,P_Min,P_Max,F,X_F)

******-****************************************************************-*******
*
*..Description: The program performs an integration of the input data
*                 to return a normalized spectrum.  The integration
*                 method used is the trapezoidal method with resolution
*                 enhancements to reduce the error.  For each 'bin' the
*                 curve is calculated at resolution number of points and
*                 that area is integrated and stored under the bin value.
*               The normalized  spectrum is returned in the F array in
*                 which the point F(L1) corresponds to the area under the
*                 curve from  X_F(1)->X_F(L1).  So F(1)=0 and F(Bins)=1
*                 The program also does the integration starting with the
*                 side that has the smallest Y values to help reduce
*                 rounding-off errors.  It was found that real*8 was
*                 necessary or the area divergent.
*
*               The function used in this method(PhysDatSpec) was derived
*                 from the data in:
*                    Physics Data 25-1 'Cosmic Rays on Earth' (1984)
*                     (ISBN 0344-8401)   O.C. Allkofer and P.K.F. Grieder
*                     Table 2.2.3              page 241
*                  and is a curve fit based upon the data in the table.  The
*                  curve is valid between 0.5 and 1000 GeV/c.
*
*..Author     : Michael P Bringle
*               Lawrence Berkeley Lab
*
*..History    : First Version August 5, 1994
*               Second Version August 19, 1994
*
*
******-****************************************************************-*******
      Implicit None
*..Input:
      Integer Bins              ! Number of divisions to break the
                                !   momentum spectrum into
*..Output:
      Real*8 F(Bins)            ! Integrated Spectrum values
      Real*8 X_F(Bins)          ! P values for a give F(#)
*..Local:
      Integer L1,L2             ! Loop variables
      Integer Res_Start,Res_End ! Resolution loop limits
      Integer Resolution        ! Number of steps between bins
      Real*4 P_Min,P_Max        ! Momentum Cutoff(GeV/c)
      Real*8 h1,h2              ! Height of trapezoid sides
      Real*8 dx                 ! Width of trapezoid
      Real*8 X                  ! Current X value
      Real*8 Section_Area       ! Area under the curve of the current section
      Real*8 Tot_Area           ! Total cumulative area under curve

*..Functions:
      Real*8 PhysDatSpec        ! Differential Momentum Spectrum(See above)
      External PhysDatSpec

*=====-================================================================-=======
*Report some information to the user
      Write(*,*)'Momentum Spectrum Data:'
      Write(*,*)'   This simulation is using a momentum spectrum'
      Write(*,*)'   based on the data in:'
      Write(*,*)'     O.C. Allkofer and P.K.F. Grieder'
      Write(*,*)'     Cosmic Rays on Earth'
      Write(*,*)'     Physics Data, No. 25-1 (1984) ISBN 0344-8401'
      Write(*,*)'     Table 2.2.3 on page 241'
      Write(*,*)'   and is a curve fit to the data in the table.'
      Write(*,*)'   The curve is valid between 0.5 and 1000 GeV/c.'

      If (P_Max.gt.1000.0.or.P_Min.lt.0.5) then
        Write(*,*)
        Write(*,*)'GetP2Ini:  Error - program terminating'
        Write(*,*)'   *************** WARNING ******************'
        Write(*,*)'   The Momentum limits you have chosen are'
        Write(*,*)'   outside the allowed range(see above.)'
        Write(*,10)'     P_Min allowed:    0.5 GeV/c   Yours: ',
     +            P_Min,' GeV/c'
        Write(*,10)'     P_Max allowed: 1000.0 GeV/c   Yours: ',
     +            P_Max,' GeV/c'
        Write(*,*)'   Program terminating...'
        Write(*,*)
        Stop    !Stopped in GetP2Ini
      Endif

      Write(*,*)

*Set-up boundary Values
      Resolution=1000     !Points to use in between bin values to find
                          !  the Area of the Curve

      Res_Start=Resolution-1
      Res_End  =0

*Find step Value
      dX=(1.0d0*(P_Max-P_Min)) / ((Bins-1)*resolution)

*Setup Last Bin(Should be zero by definition)
      F(Bins)=0.0d0
      X_F(Bins)=P_Max

*Ready Parameters to start on the Next to last bin
      L1=Bins-1
      X=P_Max+dX

*Calculate Areas
*     First get the total area under curve at each point(Backwards)
      Do While (L1.ge.1)
        Section_Area=0.0d0
        L1=L1-1

        Do L2=Res_Start, Res_End, -1
          X=X-dX
          h1=PhysDatSpec(X)
          h2=PhysDatSpec(X-dX)
          Section_Area=Section_Area+0.5d0*(h1+h2)*dX
        Enddo

        Tot_Area=Tot_Area+Section_Area
        F(L1+1)=Tot_Area
        X_F(L1+1)=X-dX
      Enddo

*     Second normalize by dividing each point by total area
      Do L1=1, Bins
        F(L1)=F(L1)/Tot_Area
      Enddo

*     Third Make sure the order is such that F(1)=0
      Do L1=1, Bins
        F(L1)=1.0d0-F(L1)
      Enddo

*Report Information to User
 10   Format(A,F13.4,A)
 11   Format(A,I8)
 12   Format(A,D21.8,A)

      Write(*,*)
      Write(*,*)'Curve Integration Package:'
      Write(*,*) ' The total area of the chosen momentum'
      Write(*,*) ' spectrum between the momentum limits is:'
      Write(*,12)'   Total Area: ',Tot_Area
      Write(*,*) ' The width of each momentum bin:'
      Write(*,12)'   dX        : ',dX,' (GeV/c)'
      Write(*,10)'   P-Minimum : ',P_Min,' (GeV/c)'
      Write(*,10)'   P-Maximum : ',P_Max,' (GeV/c)'
      Write(*,*) ' The number of points in each bin used for'
      Write(*,*) ' calculation of the area:'
      Write(*,11)'   Resolution: ',Resolution
      Write(*,*) ' The number of momentum bins:'
      Write(*,11)'   Bins      : ',Bins
      Write(*,*)

      Return
      End
