******************************************************************************
Module     MFLDGEO  is the actual GUFLD routine for GSTAR
  author   Pavel Nevski
  created  11-apr-96
******************************************************************************
+CDE,AGECOM,GCUNIT.
      structure MFLG { version,  Bfield, RmaxInn, ZmaxInn }
* --------------------------------------------------------------------------
*
   FILL MFLG(1)      ! Magnetic Field Map 
      version  =  1        ! field version
      Bfield   =  5.0      !  field value
      RmaxInn  = 264.265   !  Inner field volume radius
      ZmaxInn  = 229.685   !  Inner field volume langth
   endfill
*
      USE  MFLG  version = 1
*     drop the previous field routine, if any 
      Call CSRMSL ('AGUFLD')   
      end
* ---------------------------------------------------------------------------
*
      Subroutine   AGUFLD (x,F)
      structure MFLG { version,  Bfield, RmaxInn, ZmaxInn }
      real         x(3),F(3)
      Integer      Ievent_old/-1/
      logical      first/.true./
*
      if (first) then
*     get parameter bank locally
         USE MFLDGEO/MFLG  
         first = .false.
      endif
*
      F={0,0,0}
      If ( abs(x(1)) < mflg_RmaxInn _
         & abs(x(2)) < mflg_RmaxInn _  
         & abs(x(3)) < mflg_ZmaxInn )  then
         IF ( x(1)**2+x(2)**2 < mflg_RmaxInn**2 )  F(3) = mflg_Bfield
      endif
      END      


