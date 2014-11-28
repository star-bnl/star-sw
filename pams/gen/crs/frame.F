* $Id: frame.F,v 1.1 1998/02/14 23:02:42 fisyak Exp $
* $Log: frame.F,v $
* Revision 1.1  1998/02/14 23:02:42  fisyak
* Split Fortran sources
*
      Subroutine Frame(Det_Off,Det_Rad,Theta_E,Phi_E,X_E,Y_E,Z_E,
     +                 Theta_D,Phi_D,X_D,Y_D,Z_D)

******-****************************************************************-*******
*
*..Description: This program translates the Earth coordinate frame to the
*                 detector coordinate frame.  The frames differ in that the
*                 Earth frame is centered on the ground with the Z axis
*                 pointing 'up'.  The Detector frame however, has Y pointing
*                 up and is centered in the middle of the detector with
*                 the Z axis running along its axis of symmetry.  The
*                 geometry is as follows:
*
*               Earth                              Detector
*                +Z                                   +Y
*                 ^                                    ^
*                 |                                    |
*                 |                                    |
*                 |                                    |
*                 |                                    |
*                 |                                    |
*                 |                   ___   -----------*--------------->+Z
*                 |                    |                \
*                 |             Det_Rad+Det_Off          \
*                 |                    |                  \
*  ---------------*-------------->+X  ---                  \
*                  \                                        \
*                   \                                        \
*                    \                                        \
*                     \                                        \
*                      \                                       -X
*                       \
*                        \
*                        -Y
*
*       Phi  :  Between X and Y                            Between  X and Y
*       Theta:     Off of Z                                     Off of Z
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
      Real*4 Det_Rad         ! Mother Cylinder Radius(cm)
      Real*4 Det_Off         ! Mother Cylinder Offset from ground(cm)
      Real*4 Theta_E, Phi_E  ! Particle Trajectory(Earth)
      Real*4 X_E,Y_E,Z_E     ! Impact Position(Earth)
*..Output:
      Real*4 Theta_D, Phi_D  ! Particle Trajectory(Detector)
      Real*4 X_D, Y_D, Z_D   ! Particle Trajectory(Detector)
*..Local:
      Real*4 a,b,c           !Vector coefficients
      Real*4 Pi,Pi_O2        !Constants
      Parameter (Pi=3.14159265358979323846264338328)
      Parameter (Pi_O2=Pi/2.0d0)
*=====-================================================================-=======
*Translate position coordinates
      X_D=Y_E
      Y_D=Z_E+Det_Off-Det_Rad
      Z_D=X_E

*Translate trajectory vectors
*    Construct Unit vector in Muon frame
      a=Cos(Phi_E)*Sin(Theta_E)
      b=Sin(Phi_E)*Sin(Theta_E)
      c=           Cos(Theta_E)

*     Translate it over to detector frame
      Phi_D=Atan2(c,b)
      Theta_D=Atan2(Sqrt(b*b+c*c),a)

 999  Return
      End
