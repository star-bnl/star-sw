*  Definitions for SMD-based pi0 reconstruction.  These parameters
*  set the geometry of the SMD planes.  Initialization in BLOCK DATA 
*  Changes:
*  1) 16 August 2002
*     Included definitions of arrays smxgain(60) and smygain(100)
*     These arrays contain software gain corrections, sorted by strip
*     number.  The arrays are filled in BLOCK DATA
*
      real smdz0                ! distance in cm from IP to mid point of SMD
      real atdf,btdf            ! offset,slope of BBC time diff --> z_vert
      real smxscl               ! h/sqrt(3)+cladding, with h=triangle hght (cm)
      real smyscl               ! h/sqrt(3)+cladding, with h=triangle hght (cm)
      real smdh0N,smdh0F        ! transverse dist from beam to mid-point of SMX
      real smdv0                ! transverse dist from beam to mid-point of SMY

      common /smdpar/ 
     $     smdz0,atdf,btdf,
     $     smxscl,smyscl,
     $     smdh0N,smdh0F,smdv0
*
      real smxgain,smygain

      common /smdgain/
     $     smxgain(60),
     $     smygain(100)


