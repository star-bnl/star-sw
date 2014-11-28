* $Id: cylinder.F,v 1.1 1998/02/14 23:02:41 fisyak Exp $
* $Log: cylinder.F,v $
* Revision 1.1  1998/02/14 23:02:41  fisyak
* Split Fortran sources
*
      Subroutine Cylinder(Det_Len,Det_Rad,Theta_D,Phi_D,X_D,Y_D,Z_D,
     +                 Flag,X_C,Y_C,Z_C)

******-****************************************************************-*******
*
*..Description: Find where particle hits the cylinder.
*                The program traces the particle from a given point
*                 in space(the hemisphere) to the cylinder that just
*                 encloses the detector.  It does this in the following
*                 order:
*                   i) Check to see if it hit the end caps
*                  ii) Check to see if it hit the body
*                 The end caps are checked first because particle which
*                 are traveling nearly axial will give the body hit part
*                 a bad time.  The method for each process is described
*                 before each process.
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
      Real*4 Det_Len           ! Mother Cylinder Length(cm)
      Real*4 Det_Rad           ! Mother Cylinder Radius(cm)
      Real*4 X_D,Y_D,Z_D       ! Hemisphere impact points(Detector)
      Real*4 Theta_D,Phi_D     ! Particle Trajectory(Detector)
*..Output:
      Integer Flag             ! Did hit occur(1=yes)
      Real*4 X_C,Y_C,Z_C       ! Cylinder Impact points(Detector)
*..Local:
      Real*4 tparm1,tparm2,t
      Real*4 a,b,c
      Real*4 Z_max,Z_min       ! Cylinder Extents
      Real*4 Z_test,Test_solution
      Real*4 X_Solution1,X_Solution2
      Real*4 Y_Solution1,Y_Solution2
      Real*4 Rad_1, Rad_2
      Real*4 Pi,Pi_O2           !Constants
      Real*4 CosTh,SinTh        !Stored for speed
      Real*4 SinPh,CosPh        !Stored for speed

      Parameter (Pi=3.14159265358979323846264338328)
      Parameter (Pi_O2=Pi/2.0d0)
*=====-================================================================-=======
*Setup variables
      CosTh=Cos(Theta_D)
      SinTh=Sin(Theta_D)
      CosPh=Cos(Phi_D)
      SinPh=Sin(Phi_D)
      Flag=0
      t=0
      Z_Max= Det_Len/2.0 !Z Maximum of cylinder
      Z_Min=-Det_Len/2.0 !Z Minimum of cylinder

*First:
*  Check to see if it hits the two end caps.  Checking this first will help
*  remove some problems that might occur if the particle is traveling
*  down the barrel.  To find if it hits the barrel set the Z coordinate to
*  the ends of the barrel.  Then solve for t_parm at this z.  Plug this into
*  the parameterized version of the line to find the X & Y coordinates.  Then
*  check to see if they reside within the radius of the end cap.  If it does
*  then check to see if Phi is at the proper orientation that this would be
*  the first place struck.
      If (CosTh.ne.0.0) then
        tparm1=(Z_max-Z_D)/cosTh
        tparm2=(Z_min-Z_D)/CosTh

        Rad_1=Sqrt( (tparm1*CosPh*SinTh+X_D)**2 +
     +              (tparm1*SinPh*SinTh+Y_D)**2 )

        Rad_2=Sqrt( (tparm2*CosPh*SinTh+X_D)**2 +
     +              (tparm2*SinPh*SinTh+Y_D)**2 )

        If (Rad_1.le.Det_Rad.and.CosTh.lt.0) then
          Flag=1
          t=tparm1
          X_C=t*CosPh*SinTh + X_D
          Y_C=t*SinPh*SinTh + Y_D
          Z_C=t*           CosTh + Z_D
        Endif

        If (Rad_2.le.Det_Rad.and.CosTh.gt.0) then
          Flag=1
          t=tparm2
          X_C=t*CosPh*SinTh + X_D
          Y_C=t*SinPh*SinTh + Y_D
          Z_C=t*           CosTh + Z_D
        Endif
      Endif

*Second:
*  Project the cylinder onto the X_Z plane and find where the particle
*  would hit this circle.  Then using these Y and Z points pick the one
*  with the highest Y(vertical) value because our particle will always
*  be slanting downwards.  Then using these X & Z points plug one back into
*  the equation for the line(parameterized) to fine the proper 't' value.
*  Both variables should give the same t value.  Then plug this into the
*  Parameterized equation for z and check to see if our cylinder walls enclose
*  this value for z.

* X=t_P * CosPh + X_D
* Y=t_P * SinPh + Y_D
* X^2 + Y^2= Det_Rad
      If (Flag.eq.0) then
        a=1.0
        b=2.0*(CosPh*X_D + SinPh*Y_D)
        c=(X_D*X_D)+(Y_D*Y_D)-Det_Rad*Det_Rad

        Test_Solution=b*b-4.0*a*c

        If(Test_solution.ge.0) then !does it even hit the circle

          tparm1=(-b + Sqrt(b*b - 4.0*a*c))/2.0*a
          tparm2=(-b - Sqrt(b*b - 4.0*a*c))/2.0*a

          If (SinPh.ne.0.0) then
            Y_Solution1=tparm1*SinPh+Y_D
            Y_Solution2=tparm2*SinPh+Y_D
            If (Y_Solution1.gt.Y_Solution2) then
              t=(Y_Solution1-Y_D)/(SinTh*SinPh)
            Else
              t=(Y_Solution2-Y_D)/(SinTh*SinPh)

            Endif

          Else
            X_solution1=tparm1*CosPh+X_D
            X_solution2=tparm2*CosPh+X_D

            If (X_solution1.gt.X_solution2.and.Phi_D.lt.Pi_O2) then
              t=(X_Solution1-X_D)/(SinTh*CosPh)
            Else
              t=(X_Solution2-X_D)/(SinTh*CosPh)
            Endif
          Endif

          Z_test=t*CosTh+Z_D

          If (Z_test.le.Z_Max.and.Z_test.ge.Z_Min.and.t.ge.0) then
            Flag=1              !Signal: Found it
            X_C=t*CosPh*SinTh + X_D
            Y_C=t*SinPh*SinTh + Y_D
            Z_C=t*CosTh            + Z_D
          Endif
        endif
      Endif

      If(t.lt.0) then           !Check that it is going the right way
        Flag=-1                 !Set to -1 if traveled the wrong way
      Endif

 999  Return
      End
