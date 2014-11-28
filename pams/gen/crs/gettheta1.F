* $Id: gettheta1.F,v 1.1 1998/02/14 23:02:45 fisyak Exp $
* $Log: gettheta1.F,v $
* Revision 1.1  1998/02/14 23:02:45  fisyak
* Split Fortran sources
*
      Subroutine Gettheta1(Rnum,Theta_E)

******-****************************************************************-*******
*
*..Description: This program solves the integral of the cross-section
*                 times the flux.  Given the current value for Rnum the
*                 subroutine will return a value of Theta picked randomly
*                 between 0 and Pi/2 radians according to the well known
*                 Cos^2 distribution. At the end we multiply Cos(theta)
*                 by -1 so it will be in the range of Pi/2 -> Pi radians
*                 to correspond to its downward trajectory.
*
*                 How It Is Done:
*
*                                         (    1)
*                 cross-section*Intensity=(1 + -) * x^2       x=Cos(Theta)
*                                         (    x)
*
*                  i) Integrate that over all x(0 -> 1) to get a
*                       normalized spectrum.
*
*                 ii) Next find an expression for the integral from 0 -> x
*                       till we get the enclosed area u(our random number)
*                       the equation is:
*                       x^3+(3/2)x^2+0-(5/2)u=0 (X=Cos(Theta))
*
*                iii) Finally solve that in terms of x and you have your
*                       answer (in terms of Cos(Theta) at least)
*
*                 The method used to solve the equation is Cardan's formula
*                 from 1545, and can be found in:
*                  The Universal Encyclopedia of Mathematics
*                  By James R Newman     ISBN(0-671-20348-7)  1964
*                  Pg. 197-198
*
*                 Note:
*                  If you change this distribution then you must also account
*                  for the change in the flux.  With the Cos^2 distribution
*                  the multiplication factor is 5/4.  The factor arises
*                  because of the fact our object is not a flat plane.  We
*                  must therefore count the additional particle which hit
*                  outside the 'footprint' of our hemisphere.  The following
*                  calculation shows how to find the(and how we got our)
*                  weight factor for any other theta distribution:
*
*                            / 1
*                            |   Area(x)*F(x) dx
*                           _/ 0
*                  Weight = --------------------
*                                 / 1
*                                 |   F(x) dx
*                                _/ 0
*
*                         X=Cos(Theta)
*                         Area(X)=Area of Shadow(cross-section) at given X
*                             (     1 )
*                            =(1 + ---)
*                             (     X )
*                         F(x)=Theta distribution function
*                            =Cos^2(Theta)=X^2
*
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
      Real*4 Rnum           !Random number input
*..Output:
      Real*4 Theta_E        !Theta trajectory(Earth Frame)
*..Local:
      Real*4 p,q,u,v        !Equation coefficients and simplifications
      Real*4 a,b,c,Phi      !Equation coefficients and simplifications
      Real*4 y1             !Substitution solution
      Real*4 S1             !Equation Solution
      Real*4 test           !Applicability to Cardan's Rule test
      Real*4 CosTheta       !Proper Solution to the equation
      Real*4 INV_3          !Pre Calculated number for speed
      Real*4 Pi             !Constant

      Parameter (Pi=3.14159265358979323846264338328)
      Parameter (INV_3=1.0d0/3.0d0)
*..Functs:
      Real*4   CubeRoot        !Takes the proper cube root of a neg number
      External CubeRoot
*=====-================================================================-=======
*Solving x^3+(3/2)x^2+0-(5/2)Rnum=0 (X=Cos(Theta))
      a= 1.5d0
      b= 0.0d0
      c=-2.5d0*Rnum

      p=(-(a*a)*INV_3 + b)*INV_3
      q=(2.0d0*(a*a*a)/27.0 - a*b*INV_3 + c)*0.5d0

*..Test is what needs to be positive for Cardan's rule to work
      test = q*q + p*p*p

      If (test .ge. 0.0d0.and.Rnum.gt.0.0d0) then
*       Solution satisfying q^2+p^3>=0 (Valid for Rnum=1 -> 0.2)
        u=CubeRoot(-q+Sqrt(test))
        v=CubeRoot(-q-Sqrt(test))
        y1=u + v

*       Only the 'real' root remains.  The other two are imaginary
*         or equal to zero so they can be ignored.
        S1=y1-a*INV_3
        CosTheta=S1
      Else
*       Solution satisfying p^3+q^3<=0(Valid for Rnum=0 -> 0.2)
        Phi=Acos(-q/Sqrt(-p*p*p))

*       The two other roots are discarded because they give a negative
*         result.  So only this one remains.
        y1= 2.0d0*Sqrt(-p)*Cos(Phi*INV_3)
        S1=y1-a*INV_3
        CosTheta=S1
      EndIf

      Theta_E=Acos(-CosTheta)   !Particle is traveling downwards

999   Return
      End
