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
                        ntowers, nempty }
*
*  Local variables
        Real z1, z2, z3, z4
        Integer xdim, ydim

*
*--------------------------------------------------------------
*
   FILL FSCP            ! Fiber Sampling Calorimeter Data
     Version = 1        ! Geometry version number
     towerWidth = 1.25  ! Half width of tower
     towerLength = 45.0 ! Half length of tower
     ntowers = 20       ! Half number of towers in ( X * Y ) block, like 20 for 40 * 40
     nempty  = 3        ! Half number of towers in a center (hole), like 3 for 6 * 6
   endfill
*
   Prin1 ' FSCEgeo : init '; (A80);

   USE FSCP Version = 1
   Create   FSCE
   Position FSCE in CAVE z=-795.0
   Prin1 ' FSCEgeo : FSCE positioned'; (A80);

*
*----------------------------------------------------------------
Block FSCE is the container volume for all towers

       Material Air
       Medium standard
       Attribute FSCE seen = 0 colo = 7
       SHAPE BOX  dx = fscp_towerWidth*fscp_ntowers,
                  dy = fscp_towerWidth*fscp_ntowers,
                  dz = fscp_towerLength
	   Create FSCT
	   Prin1 'FSCT created'; (A80);

       do xdim=1,40
          do ydim=1,40
             if ( xdim > 17 .and. xdim < 24 .and. ydim > 17 .and. ydim < 24 ) then
*            no nothing, leave hole for beampipe
             else
                   Position FSCT z=-fscp_towerLength,
                                 x=-fscp_towerWidth*2*fscp_ntowers-fscp_towerWidth+xdim*fscp_towerWidth*2,
                                 y=-fscp_towerWidth*2*fscp_ntowers-fscp_towerWidth+ydim*fscp_towerWidth*2

                   Prin1 -fscp_towerLength; ('FSC z:',F8.3);
		   Prin1 -fscp_towerWidth*2*fscp_ntowers+xdim*fscp_towerWidth*2; ('    x:',F8.3);
		   Prin1 -fscp_towerWidth*2*fscp_ntowers+ydim*fscp_towerWidth*2; ('    y:',F8.3);

             endif
          enddo
       enddo

endblock
*---------------------------------------------------------------
Block FSCT is a sensitive Tungsten+Sci+Epoxy tower
*     mix of tungsten powder and epoxy
	  Component W   A=183.84 Z=74   W= 92.3
      Component C   A=12     Z=6    W= 7.1 
      Component H   A=1      Z=1    W= 0.6 
      Mixture  WSE    Dens=8.297
      Material WSE
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
