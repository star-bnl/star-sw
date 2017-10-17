* $Id: locate.F,v 1.1 1998/02/14 23:02:46 fisyak Exp $
* $Log: locate.F,v $
* Revision 1.1  1998/02/14 23:02:46  fisyak
* Split Fortran sources
*
      Subroutine Locate(D_Mode,HemiRad,Det_Len,Det_Rad,Det_Off
     +                  ,Theta_E,Phi_E,Theta,Phi,X,Y,Z,Flag)

******-****************************************************************-*******
*
*..Description: Incident Muon Spectrum Simulation
*                  This program calls the programs that place the
*                  particle on the cylinder enclosing the detector.
*                  If a mode 21 or 22 call is made the program just
*                  reformats the data in the desired manner.
*                  The program works by:
*                   i) Shadow:
*                      Taking an incoming particle with a given theta
*                      and phi angle and it places it on the X-Y plane
*                      in a region that would cause it to hit the hemisphere
*                      enclosing the detector.
*                  ii) Sphere:
*                      Next the program takes the particle and tracks it
*                      from the plane to where it would have hit on the
*                      hemisphere.
*                 iii) Frame:
*                      Translate Theta, Phi and the impact points to
*                      the detectors frame of reference.
*                  iv) Cylinder:
*                      Place the particle from the hemisphere onto the
*                      cylinder.  If the particle misses set flag=1 so
*                      a new event can be generated.
*
*..Author     : Michael P Bringle
*               Lawrence Berkeley Lab
*
*..History    : First Version August 5, 1994
*               Second Version August 19, 1994
*
******-****************************************************************-*******
      Implicit none
*..Input :
      Integer D_Mode           ! Data output mode
      Real*4 Theta_E,Phi_E     ! Momentum magnitude and direction(E Frame)
      Real*4 HemiRad           ! Enclosing hemisphere radius(cm)
      Real*4 Det_Len           ! Mother Cylinder Length(cm)
      Real*4 Det_Rad           ! Mother Cylinder Radius(cm)
      Real*4 Det_Off           ! Mother Cylinder Offset from ground(cm)
*..Output:
      Integer Flag             ! Indicates if particle hit cylinder
      Real*4 Theta,Phi         ! Momentum magnitude and direction(radians)
      Real*4 X,Y,Z             ! Particle location(cm)
*..Local :
      Real*4 Theta_D,Phi_D     ! Momentum magnitude and direction(D Frame)
      Real*4 X_D,Y_D,Z_D       ! Particle location(Detector Frame)
      Real*4 X_S,Y_S           ! Where particle hits Shadow Plane
      Real*4 X_E,Y_E,Z_E       ! Particle Location on hemisphere(Muon Frame)
      Real*4 X_C,Y_C,Z_C       ! Particle Location on cylinder(Muon Frame)
*..Saved :
      Save X_E,Y_E,Z_E         ! Save incase mode 21 call
      Save X_S,Y_S             ! Save incase mode 22 call
*=====-================================================================-=======
* Standard create event and report it in the Detector Frame
      If(D_Mode.eq.2) then
*       Throw the particle at the Spherical Shadow
        Call Shadow(D_Mode,0.0,0.0,HemiRad,Theta_E,Phi_E,X_S,Y_S)

*       Find where the particle hit on the hemisphere
        Call Sphere(HemiRad,Theta_E,Phi_E,X_S,Y_S,X_E,Y_E,Z_E)

*       Transformation to the detector coordinate system
        Call Frame(Det_Off,Det_Rad,Theta_E,Phi_E,X_E,Y_E,Z_E,
     +              Theta_D,Phi_D,X_D,Y_D,Z_D)

*       Place onto mother volume cylinder
        Call Cylinder(Det_Len,Det_Rad,Theta_D,Phi_D,X_D,Y_D,Z_D,
     +                Flag,X_C,Y_C,Z_C)

*       Swap Cylinder impact data into return variables
        X=X_C
        Y=Y_C
        Z=Z_C
        Theta=Theta_D
        Phi  =Phi_D

*     Reformat the data and report it in the Earth Frame
      Elseif(D_Mode.eq.21) then
*       Swap Earth Frame data into return variables
        X=X_E
        Y=Y_E
        Z=Z_E
        Theta=Theta_E
        Phi  =Phi_E

*     Reformat the data and report it in the Earth Frame
      Elseif(D_Mode.eq.22) then
*       Swap Shadow Frame data into return variables
*       Transform back to shadow  coordinates(Rotate to proper Phi angle)
        X=Sqrt(X_S*X_S+Y_S*Y_S)*Cos(ATan2(Y_S,X_S)-Phi_E)
        Y=Sqrt(X_S*X_S+Y_S*Y_S)*Sin(ATan2(Y_S,X_S)-Phi_E)

* Standard create event and report it in the Detector Frame but
*    only track it to the hemisphere.
      Elseif(D_Mode.eq.23) then
*       Throw the particle at the Spherical Shadow
        Call Shadow(D_Mode,0.0,0.0,HemiRad,Theta_E,Phi_E,X_S,Y_S)

*       Find where the particle hit on the hemisphere
        Call Sphere(HemiRad,Theta_E,Phi_E,X_S,Y_S,X_E,Y_E,Z_E)

*       Transformation to the detector coordinate system
        Call Frame(Det_Off,Det_Rad,Theta_E,Phi_E,X_E,Y_E,Z_E,
     +              Theta_D,Phi_D,X_D,Y_D,Z_D)

*       Swap Hemisphere impact data into return variables
        X=X_D
        Y=Y_D
        Z=Z_D
        Theta=Theta_D
        Phi  =Phi_D
      Endif

 999  Return
      End
