* $Id: shadow.F,v 1.1 1998/02/14 23:02:47 fisyak Exp $
* $Log: shadow.F,v $
* Revision 1.1  1998/02/14 23:02:47  fisyak
* Split Fortran sources
*
      Subroutine Shadow(D_Mode,Flux,Rate,HemiRad,
     +                  Theta_E,Phi_E,X_S,Y_S)

******-****************************************************************-*******
*
*..Description: Spherical Shadow Impact Point Routine
*                 This program determines where a particle with a given
*                   Phi and Theta will impact on a sphere by assigning
*                   equal probability to each area element of the sphere's
*                   'shadow' and then picking one of these elements.
*                  How it works:
*                    We can assume that for a given angle of theta, the
*                    flux over a flat plane will be equal at every point.
*                    From this one takes the incident zenith angle and
*                    determines what area on the plane corresponds to the
*                    exposed area on the hemisphere.  This is analogous
*                    to the shadow cast by shining a very distant light source
*                    on a hemisphere.  Two random coordinates are chosen until
*                    the points lie in the shadow.  This is all done assuming
*                    phi=0 for simplicity.  Then the final result is rotated
*                    to the proper phi angle.
*                  Because part of our shadow lies outside the hemisphere
*                    base we have to account for that in our flux rates.
*                    For differing theta distributions this will give
*                    differing effects(set GetTheta1.)  Shadow provides a
*                    test to see if the current weight factor gives a
*                    reasonable weight factor.  This is done by summing up
*                    the inverse area elements for each particle and at the
*                    end multiplying by the flux.  This value should be close
*                    to the predicted flux rate (Rate).  The ratio shows
*                    how close they are to each other.
*                  The D_Mode variable here tells the program which mode to
*                    execute.
*                    D_Mode=1 Initialize the variables
*                    D_MOde=3 Report the Data
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
*..Input  :
      Integer D_Mode         ! Used to select Hit/Miss report options
      Real*4 Theta_E,Phi_E   ! Muon Trajectory(radians)
      Real*4 HemiRad         ! Sphere Radius(cm)
      Real*4 Flux0,Flux      ! Flux rate greater than 1.0GeV/c (Part/cm^2 sec)
      Real*4 Rate_PD,Rate    ! Rate Predicted(Hz)
*..Output :
      Real*4 X_S,Y_S         ! Shadow impact point(cm)
*..Local  :
      Integer L1             ! Do loop variable
      Integer Flag           ! Within shadow flag
      Integer LooP_Max       ! Maximum number of trials to pick point
      Integer Tries,Hits     ! Hit/Miss shadow variables
      Real*4 Ratio,dRatio    ! Predicted Ratio and expected error
      Real*4 Rate_MC         ! Flux Rate calculated

      Real*4 GetRan          ! Random Number Generator Alias
      Real*4 ICosTh          ! Inverse Cosine Theta
      Real*4 X_Max,Y_Max     ! Box Enclosing the Shadow(cm)
      Real*4 X_hit,Y_Hit     ! Randomly chosen impact points(cm)
      Real*4 X_Chk           ! Shadow Boundary for Y_hit point(cm)
      Real*4 Pi              ! Constant

      Real*8 IArea           ! Weighted Rate Counter
      Save Tries,Hits,IArea

      Parameter (Pi=3.14159265358979323846264338328)
      Parameter(LooP_Max=1000) !Maximum Trials to pick point
*=====-================================================================-=======

*Counter Routines
 10   Format(A,F15.6,A)
 11   Format(A,I8)
      If (D_Mode.eq.2.or.D_Mode.eq.23) then     !Increment Counter
        Hits=Hits+1
        IArea=IArea+(1.0d0/(0.5*Pi*HemiRad**2*
     +              (1.0d0+1.0d0/Abs(Cos(Theta_E)))))

      Elseif (D_Mode.eq.1) then !Reset Counters
        Tries=0
        Hits =0
        IArea=0.0d0
        Flux0=Flux
        Rate_PD=Rate
        Goto 999

      Elseif (D_Mode.eq.3) then !Report Information
        Ratio=Real(Hits)/Real(Tries)
        dRatio=SQRT( (Ratio)*(1.0-Ratio)/Tries)
        Write(*,*)

        Write(*,11)'HemiCosm/Shadow Hit Statistics:'
        Write(*,11)'   Total Hits :',Hits
        Write(*,11)'   Total Tries:',Tries
        Write(*,10)'   Ratio      :',Ratio
        Write(*,10)'   dRatio     :',dRatio
        Write(*,10)'   Ratio Test :',Ratio/(Pi/4.0)
        Write(*,10)'   Ratio dTest:',dRatio/(Pi/4.0)
        Write(*,*)'Flux Testing:'
        Write(*,10)'   Sum 1/Area       : ',IArea,' (cm^-2)'
        Write(*,10)'   Rate(Predicted  ): ',Rate_PD,' (Hz)'
        Rate_MC=Flux0*Hits/IArea
        Write(*,10)'   Rate(Monte Carlo): ',Rate_MC,' (Hz)'
        Write(*,10)'   Ratio(MC/Predict): ',Rate_MC/Rate_PD
        Write(*,*)'     The ratio would not be 1 if the Cos^2(Theta)'
        Write(*,*)'      distribution has been changed without'
        Write(*,*)'      updating the predicted rate'
        Write(*,*)
        Goto 999
      Endif

*Predefined Variables
      If (Theta_E.lt.(Pi/2.0+1.0e-8)) Theta_E=Pi/2.0+1.0e-8
      ICosTh=1.0d0/Abs(Cos(Theta_E))

*Determine Shadow Boundary(Enclosing Rectangle)
      X_Max=HemiRad*ICosTh+HemiRad !Shadow length+Radius
      Y_Max=2.0*HemiRad            !Diameter of hemisphere

      Do L1=1, LooP_Max   ! To prevent infinite loop
        Flag=1            ! Set flag to failure
        Tries=Tries+1     ! Increment number of tries

*       Pick a Point in the rectangle
        X_Hit=GetRan(L1)*X_Max-HemiRad       !-HemiRad to X_Max
        Y_Hit=(GetRan(L1+1000)-0.5)*Y_Max    !-(Y_Max/2) to (Ymax/2)

*Check to see if it lies in shadow(Note this assumes that the Sphere
*       is centered on (0,0,0)(Earth))

*       Extended Part of Shadow
        If (X_Hit.gt.0) then
*         Shadow boundary Equation for a given Y and Theta_E
          X_Chk=( SQRT(HemiRad*HemiRad-Y_Hit*Y_Hit)*ICosTh)

          If(Abs(X_Hit).le.X_Chk) then !If within shadow then keep it
            Flag=0              !Set flag to success
            Goto 111            !Exit loop
          Endif
        Endif

**Standard Circle part of shadow
        If (X_Hit.le.0) then
*         Shadow boundary Equation for a given Y and Theta_E
          X_Chk=( SQRT(HemiRad*HemiRad - Y_Hit*Y_Hit) )

          If(Abs(X_Hit).le.X_Chk) then !If within shadow then keep it
            Flag=0              !Set flag to success
            Goto 111            !Exit loop
          Endif
        Endif
      Enddo

 111  If (Flag.eq.1) then
        Write(*,*)
        Write(*,*)'Shadow:  Error - Shadow pick loops exceeded!'
        Write(*,*)'   The Program was configured to throw ',LooP_Max
        Write(*,*)'   trials at the "impact shadow" of the figure.'
        Write(*,*)'   This limit prevented an infinite loop.  The'
        Write(*,*)'   program has exceed this limit.  You can try'
        Write(*,*)'   increasing this limit,LooP_Max Variable in the'
        Write(*,*)'   shadow subroutine.  The value of the input'
        Write(*,*)'   data at the time of program termination was:'
        Write(*,10)'   Theta   : ',Theta_E
        Write(*,10)'   Phi     : ',Phi_E
        Write(*,10)'   HemiRad : ',HemiRad
        Write(*,10)'   X_S     : ',X_S
        Write(*,10)'   Y_S     : ',Y_S
        Write(*,*)'   Program terminating...'
        Write(*,*)
        Stop !Stopped in Shadow
      Endif

*Transform to proper coordinates(Rotate to proper Phi angle)
      X_S=Sqrt(X_Hit*X_Hit+Y_Hit*Y_Hit)*Cos(ATan2(Y_Hit,X_Hit)+Phi_E)
      Y_S=Sqrt(X_Hit*X_Hit+Y_Hit*Y_Hit)*Sin(ATan2(Y_Hit,X_Hit)+Phi_E)

999   Return
      End
