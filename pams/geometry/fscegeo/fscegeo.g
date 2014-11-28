******************************************************************
Module FSCEGEO is the geometry of the Fiber Sampling Calorimeter
Created 04-Jul-2011
Author  D.Arkhipkin <arkhipkin@bnl.gov>
******************************************************************
+CDE,AGECOM,GCUNIT.
*
        Content FSCE, FSCT
*
        Structure FSCP {Version, towerWidth, towerLength,
                        nTowersX, nTowersY, nEmpty, distFromVtx }
*
*  Local variables
        Integer xdim, ydim, xposmin, xposmax, yposmin, yposmax

*
*--------------------------------------------------------------
*
   FILL FSCP            ! Fiber Sampling Calorimeter Data
     Version = 1        ! Geometry version number
     towerWidth = 1.25  ! Half width of tower, cm
     towerLength = 45.0 ! Half length of tower, cm
     nTowersX = 20      ! Half number of towers in X dimension
     nTowersY = 40      ! Half number of towers in Y dimension
     nEmpty  = 3        ! Half number of towers in a center (hole), like 3 for 6 * 6 hole
     distFromVtx = 716.7 ! Distance from event vertex (0,0) in cm, should equal to FMS
   endfill
*

   USE FSCP Version = 1
   Create   FSCE
   Position FSCE in CAVE z=fscp_distFromVtx+fscp_towerLength
   Prin1 ' FSCEgeo : FSCE created and positioned'; (A80);

*
*----------------------------------------------------------------
Block FSCE is the container volume for all towers

       Material Air
       Medium standard
       Attribute FSCE seen = 0 colo = 7
       SHAPE BOX  dx = fscp_towerWidth*fscp_nTowersX,
                  dy = fscp_towerWidth*fscp_nTowersY,
                  dz = fscp_towerLength
	   Create FSCT

       xposmin = fscp_nTowersX - fscp_nEmpty
       xposmax = fscp_nTowersX + fscp_nEmpty + 1
       yposmin = fscp_nTowersY - fscp_nEmpty
       yposmax = fscp_nTowersY + fscp_nEmpty + 1

       do xdim=1,fscp_nTowersX*2
          do ydim=1,fscp_nTowersY*2
             if ( xdim > xposmin .and. xdim < xposmax .and. ydim > yposmin .and. ydim < yposmax ) then
*            no nothing, leave hole for beampipe
             else
                   Position FSCT z=0,
                                 x=-fscp_towerWidth*2*fscp_nTowersX-fscp_towerWidth+xdim*fscp_towerWidth*2,
                                 y=-fscp_towerWidth*2*fscp_nTowersY-fscp_towerWidth+ydim*fscp_towerWidth*2
             endif
          enddo
       enddo

endblock
*---------------------------------------------------------------
Block FSCT is a sensitive Tungsten+Sci+Epoxy tower
**      Material  WSE
	  Component W   A=183.84 Z=74   W= 92.3
      Component C   A=12.01  Z=6    W= 7.1 
      Component H   A=1      Z=1    W= 0.6 
      Mixture  WSE Dens=8.297 Radl=0.8719
      Medium    sensitive  Isvol=1
      Attribute FSCT seen=1 colo=7 ! lightblue
      Shape BOX   dx = fscp_towerWidth,
                  dy = fscp_towerWidth,
                  dz = fscp_towerLength
      Call GSTPAR (ag_imed,'CUTGAM',0.0005)
      Call GSTPAR (ag_imed,'CUTELE',0.00015)
      HITS FSCT Eloss:0:(0,250) 
endblock
*-------------------------------------------------------------------
END
