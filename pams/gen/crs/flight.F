* $Id: flight.F,v 1.3 1999/09/28 15:34:45 nevski Exp $
* $Log: flight.F,v $
* Revision 1.3  1999/09/28 15:34:45  nevski
* fix for pgf-m migration
*
* Revision 1.2  1999/09/28 15:30:27  nevski
* fix for pgf-m migration
*
* Revision 1.1  1998/02/14 23:02:41  fisyak
* Split Fortran sources
*
      Subroutine Flight(P_Mode,P_Min,P_Max,Theta_E,Phi_E,P,Charge)

******-****************************************************************-*******
*
*..Description: This program calls the programs that generate the
*                 Phi, Theta, P, and Charge distributions.  Four random
*                 numbers are generated and then passed to the respective
*                 subroutines which return the parameters.  The individual
*                 spectrum are generated in the following ways:
*                   Phi  : 0 -> 2Pi with a flat distribution(Analytical
*                          formula)
*                   Theta: Pi/2->Pi with a Cos^2 distribution(Analytical
*                          formula)
*                   P    : P_Min->P_Max using a method determined by the
*                          P_mode parameter.
*                          The choices include:
*                           P_Mode=0 Constant Spectrum P=P_Min
*                           P_Mode=1 1/p^2 Distribution(Analytical)
*                           P_Mode=2 Measured Spectrum [Physics Data(1984)]
*                                    (Interpolation)
*                           P_Mode=3 DarInv calculated spectrum(Interpolation)
*                                    [Arnon Dar and Henry Kendall]
*                   Charge: +1 or -1 (Analytical formula)
*                 A more detailed description of each procedure and references
*                    can be found in the respective subroutine.
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
      Integer P_Mode            ! Momentum Spectrum Mode(GeV/c)
      Real*4 P_Min,P_Max        ! Momentum Range(GeV/c)
*..Output:
      Integer Charge            ! Muon Charge
      Real*4 Theta_E,Phi_E      ! Angular orientation(Earth Frame)(Radians)
      Real*4 P                  ! Momentum (GeV/c)
*..Local:
      Integer L1                ! Do loop variable
      Integer Flag              ! Check Flag
      Integer Skip/1/           ! Random Number Index Advancer
      Real*4 Rnum(4)            ! Random Number variables
      Logical*4 First/.true./   ! First call
      Save First
*..Functs:
      Real*4 GetRan             ! Random number generator
      External GetRan           ! Random number alias function
*=====-================================================================-=======
*Get four random numbers for picking the parameters
      Do L1=1, 4
        RNum(L1)=GetRan(L1+Skip)
      Enddo

*Get Phi
      Call Getphi1(Rnum(1),Phi_E)

*Get Theta
      Call Gettheta1(Rnum(2),Theta_E)

*Get P
      If (First) then
        First = .false.
        Write (*, '(/, 1X, A, I3)')
     +    'HemiCosm/Flight:  Momentum spectrum selection', P_Mode
        Write (*, *) '  Pmode   Type'
        Write (*, *) '    0     Constant:  p = p_min'
        Write (*, *) '    1     1/p^2'
        Write (*, *) '    2     Measured Spectrum (Physics Data 1984)'
        Write (*, *) '    3     Theoretical Spectrum (Arnon Dar 1984)'
        Write (*, *) ' '
      Endif                     ! (First)
      Flag=0
      Do While (Flag.eq.0) 
C       Ensure that it is in the proper range
        If (P_Mode.eq.1) then
          Call GetP1(P_Min,P_Max,Rnum(3),P)
        Elseif (P_Mode.eq.2) then
          Call GetP2(P_Min,P_Max,Rnum(3),P)
        Elseif (P_Mode.eq.3) then
          Call GetP3(P_Min,P_Max,Rnum(3),Theta_E,P)
        Elseif (P_Mode.eq.0) then
          Call GetP0(P_Min,P)
          Flag=1
        Endif

        If (P.ge.P_Min.and.P.le.P_Max) Flag=1 ! Check to see that P is
                                              !  in proper range.
        Skip=Skip+1
        Rnum(3)=GetRan(L1+Skip) ! Get a new random number in case a new
                                ! value is needed.
      Enddo

*Get Charge
      Call GetCharge(Rnum(4),Charge)

 999  Return
      End
