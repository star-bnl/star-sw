* $Id: cuberoot.F,v 1.2 1998/02/14 23:02:40 fisyak Exp $
* $Log: cuberoot.F,v $
* Revision 1.2  1998/02/14 23:02:40  fisyak
* Split Fortran sources
*
      Real*4 Function CubeRoot(x)
      Implicit none
      Real*4 X
      CubeRoot=(X/Abs(x))*(Abs(X)**(1.0d0/3.0d0))
      End
