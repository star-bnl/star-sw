* $Id: sphere.F,v 1.1 1998/02/14 23:02:47 fisyak Exp $
* $Log: sphere.F,v $
* Revision 1.1  1998/02/14 23:02:47  fisyak
* Split Fortran sources
*
      Subroutine Sphere(HemiRad,Theta_E,Phi_E,X_S,Y_S,X_E,Y_E,Z_E)

******-****************************************************************-*******
*
*..Description: This program determines where the particle hit on the
*                 hemisphere from where it hit on the shadow.  This is done
*                 by substituting the parametric equation for a line into
*                 the equation for a sphere and then solving for the
*                 parameter variable.  This variable is then put back into
*                 the equation for a line and the sphere coordinates result.
*                 Because we should be guaranteed a hit on the hemisphere
*                 a square root error should not result.  That would mean
*                 that the particle missed the sphere.
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
      Real*4 Theta_E, Phi_E    !Incident Trajectory
      Real*4 X_S,Y_S           !Shadow Impact points
      Real*4 HemiRad           !Sphere Radius
*..Output:
      Real*4 X_E,Y_E,Z_E       !Sphere Impact Points
*..Local:
      Real*4 CosTh,SinTh       !Predefined variables(for speed)
      Real*4 CosPh,SinPh       !Predefined variables(for speed)
      Real*4 a,b,c             !Quadratic Equation Coefficients
      Real*4 t1,t2             !Roots of Quadratic Equation
      Real*4 X1,Y1,Z1          !Incoming Ray Solution(First Root)
      Real*4 X2,Y2,Z2          !Incoming Ray Solution(Second Root)
      Real*4 Dist1,Dist2       !Distance from impact to shadow plane
      Real*4 temp1,temp2       !Temporary variables

*=====-================================================================-=======
*Predefined Variables(for speed)
      CosTh=Cos(-Theta_E) !Here we are assuming that Theta 90->180 deg
      SinTh=Sin(-Theta_E) !  so this will give us a positive value.
      CosPh=Cos(Phi_E)
      SinPh=Sin(Phi_E)

*Find Coefficients of quadratic Equation
      a=1.0
      b=2.0*SinTh*(X_S*CosPh + Y_S*SinPh)
      c=X_S*X_S + Y_S*Y_S - HemiRad*HemiRad

*Solve Quadratic Equation
      temp1=1.0d0/(2.0*a)
      temp2=(b*b-4.0*a*c)
      if (temp2.lt.0) temp2=0.0!sometimes when subtracting the two large
                               !numbers we get a small neg so set it equal
                               !to zero in that case.
      temp2=sqrt(temp2)
      t1=((-b)+temp2)*temp1
      t2=((-b)-temp2)*temp1

*Solve for Points(Parametric Line Equations)
      X1= ( t1*SinTh*CosPh + X_S )
      Y1= ( t1*SinTh*SinPh + Y_S )
      Z1=-( t1*CosTh             )

      X2= ( t2*SinTh*CosPh + X_S )
      Y2= ( t2*SinTh*SinPh + Y_S )
      Z2=-( t2*CosTh             )

*Find Distance from Impact Point
      Dist1=Sqrt((X1-X_S)*(X1-X_S)+(Y1-Y_S)*(Y1-Y_S)+(Z1)*(Z1))
      Dist2=Sqrt((X2-X_S)*(X2-X_S)+(Y2-Y_S)*(Y2-Y_S)+(Z2)*(Z2))

*Assign Furthest one from shadow to coordinates(Z>=0 of course)
      If ( (Dist2.gt.Dist1.and.z2.ge.0).or.(Z1.lt.0)) then
        X_E=X2
        Y_E=Y2
        Z_E=Z2
      Else
        X_E=X1
        Y_E=Y1
        Z_E=Z1
      Endif

999   Return
      End
