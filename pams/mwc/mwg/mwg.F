CC:>--------------------------------------------------------------------
CC: FILE:       mwg.F.template
CC: HISTORY:
CC:             15oct96-     jpw STAF Version  
CC:<--------------------------------------------------------------------

      INTEGER*4 FUNCTION MWG(
     1              geom_h,              geom ,
     2               cal_h,               cal ) 
      IMPLICIT NONE
#include "mwg.inc"
CC:>--------------------------------------------------------------------
CC: ROUTINE:    MWG
CC: DESCRIPTION: Fills MWC geometry tables
CC: AUTHOR:     jpw - J.P. Whitfield, whitfield@cmchem.chem.cmu.edu
CC: ARGUMENTS:
CC:          IN:
CC:       INOUT:
CC:             geom     - Geometry structure
CC:              cal     - Calibration parameters
CC:         OUT:
CC: RETURNS:    STAF Condition Value
CC:>--------------------------------------------------------------------

      INTEGER*4 Isec, Nsectors, Id
*--
*--     Set general geometry numbers
*--   
      Geom(1).init  =   1
      if (Geom(1).nphi .eq. 0) Geom(1).nphi  =  12
      if (Geom(1).neta .eq. 0) Geom(1).neta  =   32
      if (Geom(1).r1min .eq. 0) Geom(1).r1min =  53.0   
      if (Geom(1).r1max .eq. 0) Geom(1).r1max = 121.0 
      if (Geom(1).r2min .eq. 0) Geom(1).r2min = 122.5950 
      if (Geom(1).r2max .eq. 0) Geom(1).r2max = 191.3950 
*--
*--    Set Calibration constants and pedestals
*--
      Nsectors = 2 * Geom(1).nphi * Geom(1).neta
*--
      if ( Nsectors .le. Cal_h.maxlen ) then
         do Isec = 1, Nsectors
            Cal(Isec).cc = 0.02
            Cal(Isec).pd = 20.
            Cal(Isec).ps = 2.
         enddo
         Cal_h.nok = Nsectors
      else
         print*, 'MWC Calibration array too small !'
         mwg = STAFCV_BAD
         return
      endif
*--
      Mwg = STAFCV_OK
*--
*--   That's it
*--
      end








