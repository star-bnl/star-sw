c-----------------------------------------------------------------------------c
c                                                                             c
c                                    *                                        c
c                                   ***                                       c
c                                  ** **                                      c
c                                 **   **                                     c
c                        *********       *********                            c
c                           ***             ***                               c
c                             **           **                                 c
c                               *         *                                   c
c                             **     *     **                                 c
c                            **   **   **   **                                c
c                           ** **         ** **                               c
c                          **                 **                              c
c                                                                             c
c-----------------------------------------------------------------------------c
c                                                                             c
c         SSSSSSSSSS         V               V        TTTTTTTTTTTTTTTTT       c
c        SSSSSSSSSSSS        VV             VV         TTTTTTTTTTTTTTT        c
c        SS                   VV           VV                TTT              c
c        SS                    VVV       VVV                 TTT              c
c         SSSSSSSSS             VVV     VVV                  TTT              c
c          SSSSSSSSS             VVV   VVV                   TTT              c
c                  SS             VVV VVV                    TTT              c
c                  SS              VV VV                     TTT              c
c        SSSSSSSSSSSS               VVV                      TTT              c
c         SSSSSSSSSS                 V                        T               c
c-----------------------------------------------------------------------------c

c:>------------------------------------------------------------------
C:FILE:         srs_am.F
C:DESCRIPTION:  SVT resolution simulator
c:AUTHOR:       Ken Wilson/ Claude Pruneau
C:BUGS:         -- STILL IN DEVELOPMENT --
C:HISTORY:      10aug96- STAF Version Creation.
c:>------------------------------------------------------------------

c:>------------------------------------------------------------------
C:ROUTINE:      INTEGER*4 PAM
C:DESCRIPTION:  SVT resolution simulator:
c------------------------------------------------------------------------------c
c	Functional Description:                                                c
c		This code is the main driver for the svt resolution simulator, c
c               which uses info from svt_hit to fill svt_spt.                  c
c	Author: William Kenneth Wilson                                         c
c	Created on Feb, 21, 1994                                               c
c------------------------------------------------------------------------------c
c       The variable methode controls which of the srs packages are used:      c
c           Method=1 (default) uses Ken Wilson's svt_resolution_sim,           c
c                    the full blown resolution simulator                       c
c           Method=2 uses Claude Pruneau's spt_direct routine to make          c
c                    a direct copy from hits to space points (no smearing)     c
c           Method=3 uses Claude Pruneau's spt_direct_gs routine which         c
c                    smears the hits with a fixed sigma which is taken from    c
c                    the table direct(1).sd (drift direction)                  c
c                                           .st (transverse direction)         c
c------------------------------------------------------------------------------c
c       Put in space point merging March 16, 1994                              c
c       The table entry spt.flag gives the merging status of a hit             c
c       the code is:                                                           c
c       flag=0 unperturbed hit                                                 c
c       flag=1 merged hit                                                      c
c       Merging will take place if the table entry srspar.merge = 1            c
c       (The default is 1)                                                     c
c       id_globtrk initialized to zero when filling scs_spt LM sept-98         c
c------------------------------------------------------------------------------c
      INTEGER*4 FUNCTION SRS_AM(
     1            result_h,            result ,
     2           svt_hit_h,           svt_hit ,
     3               spt_h,               spt ,
     4              geom_h,              geom ,
     5            config_h,            config ,
     6             shape_h,             shape ,
     7            srspar_h,            srspar ,
     8            direct_h,            direct ,
     9           activea_h,           activea ) 
c      IMPLICIT NONE
#include "srs_am.inc"
c------------------------------------------------------------------------------c
c	integer j,k,ii,jj,kk
        integer i
        integer svt_resolution_sim,spt_direct,spt_direct_gs
        integer ihit,iwafer,wafer,layer,id_active,active,active_area
        integer filtered
        integer id_shape,get_geom_row,dead
        real gcoord(3),lcoord(3),drift_length,trans_length
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

      WRITE(*,*) 'SRS_AM: Starting ######################################'
      print*,' nok svt_hit = ',svt_hit_h.nok
      if (svt_hit_h.nok.eq.0) then
         spt_h.nok=0
         WRITE(*,*) 'SRS_AM: Stopping ######################################'
         SRS_AM=STAFCV_OK
         RETURN
      endif


c------------------------------------------------------------------------------c
c       REMOVE HITS IN NON-ACTIVE AREAS                                        c
c------------------------------------------------------------------------------c
        filtered=0
        do ihit=1,svt_hit_h.nok
                gcoord(1)=svt_hit(ihit).x(1) ! svt global to local
                gcoord(2)=svt_hit(ihit).x(2)
                gcoord(3)=svt_hit(ihit).x(3)
                iwafer = 0
                wafer  = svt_hit(ihit).volume_id
                iwafer = get_geom_row(geom_h,geom,wafer)
                if (iwafer.gt.0) then
			if (wafer.lt.7000) then ! hit on Drift Detectors
	                        call svtgtol(gcoord,lcoord,
     &  	                 geom(iwafer).x,
     &          	         geom(iwafer).d,
     &                  	 geom(iwafer).t,
     &                   	geom(iwafer).n)
        	                layer=geom(iwafer).layer
	                        id_shape=config(1).layer_shape(layer)
		                drift_length=shape(id_shape).shape(1)
        	                trans_length=shape(id_shape).shape(2)
	                        id_active=srspar(1).id_active(layer)
	                        active=active_area(id_active,lcoord,
     &  	                 drift_length,trans_length,
     &          	         activea_h,activea)
			else ! hit on Strip Detectors
				active=1
			endif
                        if (active.eq.1) then ! keep this one
                                filtered=filtered+1
                                svt_hit(filtered).id=
     &                                       svt_hit(ihit).id
                                svt_hit(filtered).volume_id=
     &                                       svt_hit(ihit).volume_id
                                svt_hit(filtered).next_tr_hit_p=
     &                                       svt_hit(ihit).next_tr_hit_p
                                svt_hit(filtered).track_p=
     &                                       svt_hit(ihit).track_p
                                svt_hit(filtered).de=
     &                                       svt_hit(ihit).de
                                svt_hit(filtered).ds=
     &                                       svt_hit(ihit).ds
                                svt_hit(filtered).tof=
     &                                       svt_hit(ihit).tof
                                do i=1,3
                                        svt_hit(filtered).p(i)=
     &                                            svt_hit(ihit).p(i)
                                        svt_hit(filtered).x(i)=
     &                                            svt_hit(ihit).x(i)
                                enddo
                        endif
                else
                        print*,'SVT Wafer Shape ID is not set!'
	                srs_am=STAFCV_BAD
                        return
                endif
        enddo
        dead=svt_hit_h.nok-filtered
        print*,'The number of hits on inactive areas = ',dead
        svt_hit_h.nok=filtered
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c


c------------------------------------------------------------------------------c
c       CALL THE CORRECT METHOD                                                c
c------------------------------------------------------------------------------c
	if (svt_hit_h.nok.gt.0) then
           if (srspar(1).method.eq.1) then
              srs_am=svt_resolution_sim(result_h, result,
     +             svt_hit_h, svt_hit, spt_h, spt, 
     +             geom_h, geom, config_h, config,
     +             shape_h, shape, srspar_h, srspar) 
           else if (srspar(1).method.eq.2) then
              srs_am=
     +             spt_direct(svt_hit_h,svt_hit,spt_h,spt)
           else if (srspar(1).method.eq.3) then
              srs_am=spt_direct_gs(direct_h,direct,
     +             geom_h,
     +             geom,svt_hit_h,svt_hit,spt_h,spt,srspar_h,srspar)
           else
              print*,'Undefined method for svt resolution simulator:'
              print*,'Method = ',srspar(1).method
              srs_am=STAFCV_BAD
           endif
        endif                   ! (svt_hit_h.nok.gt.0)
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c


c------------------------------------------------------------------------------c
c       DONE                                                                   c
c------------------------------------------------------------------------------c
      WRITE(*,*) 'SRS_AM: Stopping ######################################'
      SRS_AM=STAFCV_OK
      RETURN
      END


c*****************************PROGRAM BOUNDRY**********************************c

c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c
	integer function active_area(id_active,lcoord,drift_length,
     &   trans_length,activea_h,activea)
c        implicit none 
c------------------------------------------------------------------------------c
c	Functional Description:                                                c
c		This code returns 1 for hits on the active area                c
c	Author: William Kenneth Wilson                                         c
c	Created April 21, 1994                                                 c
c------------------------------------------------------------------------------c
c       For local coordinates, first componant is in drift direction
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------c
c	VARIABLE DECLARATIONS AND DATA:					       c
c------------------------------------------------------------------------------c
#include "PAM.inc"
#include "srs_activea.inc"
c------------------------------------------------------------------------------c
      RECORD      /TABLE_HEAD_ST/               activea_h
      RECORD     /SRS_ACTIVEA_ST/              activea(*)
c------------------------------------------------------------------------------c
c	integer i,j,k,ii,jj,kk
        integer id_active
        real lcoord(3),drift_length,trans_length,test,m,b
        real x1,x2,y1,y2
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------c
c	TEST THE HIT:      						       c
c------------------------------------------------------------------------------c
c       Rectangle, butterfly, and trapizoid all need to pass this test:
        active_area=1 ! set to keep
        if (id_active.gt.0.and.id_active.le.3) then 
                test=drift_length-activea(id_active).param(1)
                if (abs(lcoord(1)).gt.test) then
                        active_area=0
                        return
                endif
                test=trans_length-activea(id_active).param(2)
                if (abs(lcoord(2)).gt.test) then
                        active_area=0
                        return
                endif
                if (id_active.eq.2) then ! Butterfly test
                        x1=trans_length-
     &                   activea(id_active).param(2)-
     &                   activea(id_active).param(3)
                        x2=trans_length-activea(id_active).param(2)
                        y1=0
                        y2=drift_length-activea(id_active).param(1)
                        m=(y2-y1)/(x2-x1)
                        b=y1-m*x1
                        test=abs(lcoord(2))*m+b
                        if (abs(lcoord(1)).lt.test) then
                                active_area=0
                                return
                        endif
                endif
                if (id_active.eq.3) then ! Trapizoid test
                        x1=trans_length-
     &                   activea(id_active).param(2)-
     &                   activea(id_active).param(3)
                        x2=trans_length-activea(id_active).param(2)
                        y1=-drift_length+activea(id_active).param(1)
     &                   +activea(id_active).param(4)
                        y2=drift_length-activea(id_active).param(1)
                        m=(y2-y1)/(x2-x1)
                        b=y1-m*x1
                        test=abs(lcoord(2))*m+b
c                       test for "side" guard area
                        if (lcoord(1).lt.test) then 
                                active_area=0
                                return
                        endif
c                       test for "bottom" guard area       
                        if (lcoord(1).lt.y1) then 
                                active_area=0
                                return
                        endif
                endif
        else
                print*,'Unknown id_active :',id_active        
                active_area=0
        endif
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------c
c       DONE                                                                   c
c------------------------------------------------------------------------------c
	return
	end
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c*****************************PROGRAM BOUNDRY**********************************c

c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c
	integer function svt_resolution_sim(result_h, result,
     +  svt_hit_h, svt_hit, spt_h, spt, 
     +  geom_h, geom, config_h, config,
     +  shape_h, shape, srspar_h, srspar,
     +  activea_h,activea) 

        implicit none 
c 
c	Functional Description: 
c		This code is the svt resolution simulator
c	Author: William Kenneth Wilson 
c	Created from existing file svt_resolution on March 30, 1993 
c	Modified to update mct table svt_hit May 5, 1993 
c	Modified to output spt table (scs package) July 30, 1993 
c       Modified to not need a call to srs_make_event when receiving data
c       from svt_hit Nov. 4, 1993
c	Error conditions:
c------------------------------------------------------------------------------c
c	This program calculates the resolution of a STAR type SDD.             c
c	Begun by W.K.W. in Fall 1992				               c
c	Checked and modified by W.K.W. Dec. 1992- Jan. 1993                    c
c       Modified Jan 1994 to use the SVG geometry info.                        c
c------------------------------------------------------------------------------c
c	There are 2 include file which are needed to run the code:             c
c	svt_samples_common_inc --> keeps track of the data seen by the         c
c	adc for a given hit.                                                   c
c	svt_parameters_inc --> contains data on the SDD, including drift       c
c	velocity, # of time buckets, ENC, etc.			               c
c------------------------------------------------------------------------------c
c                anodes                 rotated system                         c
c                             drift                                            c
c                  z            .             q1                               c
c                  |           /|\            /                                c
c                  |  *         |            /                                 c
c                  |**          |           /                                  c
c                  |**          |          /                                   c
c                 ***---------x            \                                   c
c                **                         \                                  c
c		**                           \                                 c
c               *                             q2                               c
c	charge=param(1)							       c
c       anode offset=param(2)						       c
c	drift distance=param(3)						       c
c	sigma major axis=param(4) (q1)					       c
c	sigma minor axis=param(5  (q2) 				               c
c	phi angle of major axis wrt drift axis =param(6)		       c
c	x proj width=param(7) (depends on 4, 5, and 6)       		       c
c	z proj width=param(8) (depends on 4, 5, and 6)			       c
c       x direction is transverse to drift                                     c
c       z direction is parallel to drift                                       c
c       Thus, whien phi=0, sigma x = sigma 2 and sigma z = sigma 1             c
c------------------------------------------------------------------------------c
c       Detector ID Convention:  geom(ichip).id                            c
c       = 1000*ilayer+100*iwafer+iladder                                       c
c------------------------------------------------------------------------------c
c
c	VARIABLE DECLARATIONS AND DATA:					       c
c------------------------------------------------------------------------------c
#include "PAM.inc"
#include "srs_result.inc"
#include "g2t_svt_hit.inc"
#include "scs_spt.inc"
#include "svg_geom.inc"
#include "svg_config.inc"
#include "svg_shape.inc"
#include "srs_srspar.inc"
#include "srs_activea.inc"
c------------------------------------------------------------------------------c
      RECORD      /TABLE_HEAD_ST/                result_h
      RECORD      /SRS_RESULT_ST/               result(*)
      RECORD      /TABLE_HEAD_ST/               svt_hit_h
      RECORD     /G2T_SVT_HIT_ST/              svt_hit(*)
      RECORD      /TABLE_HEAD_ST/                   spt_h
      RECORD         /SCS_SPT_ST/                  spt(*)
      RECORD      /TABLE_HEAD_ST/                  geom_h
      RECORD        /SVG_GEOM_ST/                 geom(*)
      RECORD      /TABLE_HEAD_ST/                config_h
      RECORD      /SVG_CONFIG_ST/               config(*)
      RECORD      /TABLE_HEAD_ST/                 shape_h
      RECORD       /SVG_SHAPE_ST/                shape(*)
      RECORD      /TABLE_HEAD_ST/                srspar_h
      RECORD      /SRS_SRSPAR_ST/               srspar(*)
      RECORD      /TABLE_HEAD_ST/               activea_h
      RECORD     /SRS_ACTIVEA_ST/              activea(*)
c------------------------------------------------------------------------------c
	integer i,ii,jj,kk,nok,report,del
c       integer j,k
        integer get_geom_row
	real width1,width2,qtot,dist,theta,phi,offset,de
c       real mean,x,y,z,r,m
	real widthx,widthz,widthxpro,widthzpro
	real svt_qtot,param(8),uncer_x,uncer_z,s,c,ran2
	real pxin,pyin,pzin,pin,smear_anode,smear_drift
        real new_x,new_y,new_z
c       real tpc_phi
	real v1(3),mag1,v2(3),dot
c       real mag2,v3(3),mag3,v4(3),mag4,v5(3),mag5
        real dot1,dot2,xhat(3),yhat(3),zhat(3),tmax
        real gcoord(3),lcoord(3)
	real covar(3),deltax,deltay,ux,uy
	real seperation,resolution
	integer ihit,nanode,id_active
c       integer chip,iix,iiz
        integer seed,iwafer
	real drift_length,trans_length,thickness,gasdev
	integer id_shape,jhit,old_ok,new_ok
	integer layer,ladder,wafer,npoints(9,26,9),points(9,26,9,1000),wi,wj
	real posit(2,15000),uncer(2,15000)
        external ran2
c------------------------------------------------------------------------------c
c       samples(1,n),samples(2,n) = x and z coordinate of sample               c
c       samplesx(1,n),samplesz(1,n) anode,t bucket hit in x or z projection    c
c       samplesx(2,n) earliest t bucket hit for anode hit n                    c
c       samplesx(3,n) latest t bucket hit for anode hit n                      c
c       samplesz(2,n) earliest anode hit for t bucket hit n                    c
c       samplesz(3,n) latest anode hit for t bucket hit n                      c
c------------------------------------------------------------------------------c
#include "svt_samples_common.inc"
c------------------------------------------------------------------------------c
        data seed /111111/
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c


c------------------------------------------------------------------------------c
c	INITIALIIZATION:						       c
c------------------------------------------------------------------------------c
        if (srspar(1).init_ran.ne.0) then
           seed=srspar(1).init_ran
        endif
	nok=svt_hit_h.nok
        del=nok/10 ! this line and the next few are for printing out the
        report=del ! progress of the code
        do i=1,3
           xhat(i)=0
           yhat(i)=0
           zhat(i)=0
        enddo
        xhat(1)=1.0
        yhat(2)=1.0
        zhat(3)=1.0
	if (config(1).n_layer.gt.9) then
	    print*,'npoints array not large enough!'
	    return
        endif
        do i=1,config(1).n_layer
            if ((config(1).n_ladder(i).gt.26).or. 
     1          (config(1).n_wafer(i).gt.9)) then
	        print*,'npoints array not large enough!'
	        return                
            endif
        enddo
	do layer=1,config(1).n_layer
		do ladder=1,config(1).n_ladder(layer)
			do wafer=1,config(1).n_wafer(layer)
				npoints(layer,ladder,wafer)=0
			enddo
		enddo
	enddo
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------c
c	FIND HIT POSITION:						       c
c------------------------------------------------------------------------------c
	do ihit=1,nok ! main loop 
        if (ihit.ge.report) then
           print*,' SRS done with ',ihit
           report=report+del
        endif   
	gcoord(1)=svt_hit(ihit).x(1) ! vector for svt global to local routine
	gcoord(2)=svt_hit(ihit).x(2)
	gcoord(3)=svt_hit(ihit).x(3)
        pxin=svt_hit(ihit).p(1)
        pyin=svt_hit(ihit).p(2)
        pzin=svt_hit(ihit).p(3)
	pin=sqrt(pxin**2.+pyin**2.+pzin**2.)	    
        de=svt_hit(ihit).de
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------C
c       TRANSFORM GLOBAL COORD TO LOCAL COORD AND FIND DRIFT DIST, OFFSET      c
c       ANGLES, AND CHARGE                                                     c
c------------------------------------------------------------------------------c
        iwafer = 0
        wafer  = svt_hit(ihit).volume_id
        iwafer = get_geom_row(geom_h,geom,wafer)
        if (iwafer.gt.0) then
          call svtgtol(gcoord,lcoord,
     &           geom(iwafer).x,
     &           geom(iwafer).d,
     &           geom(iwafer).t,
     &           geom(iwafer).n)
          layer=geom(iwafer).layer
          id_shape=config(1).layer_shape(layer)
	  drift_length=shape(id_shape).shape(1)
          trans_length=shape(id_shape).shape(2)
          thickness=2.0*shape(id_shape).shape(3) ! 2.0 * half width
c         drift distance:
          id_active=srspar(1).id_active(layer)
          dist=drift_length-abs(lcoord(1))-
     1     activea(id_active).param(1)
	  if (dist.gt.drift_length) then
	     print*,'drift too long!',drift_length,lcoord(1)
	     dist=drift_length
	  endif
	  if (dist.lt.0) then ! drift negative (due to geant 10mic bin?)
	     dist=0.0001 ! minimum drift = 1 micron for numerical reasons
	  endif
c         offset with respect to anodes 
c         (assume there is an anode on the center line of wafer)
          nanode=nint(lcoord(2)/srspar(1).pitch)
c         offset can be negative or positive
          offset=(lcoord(2)-float(nanode)*srspar(1).pitch) 
c         theta (angle w.r.t. normal incidence)
	  call dproduct(svt_hit(ihit).p,geom(iwafer).n,dot)
	  dot=dot/(pin) 
	  dot=abs(dot) ! force theta to be between 0 and 90 degrees
	  if (dot.gt.1.0) then
	     dot=1.0		! safety from roundoff error
	     print*,'Problem with angle'
	  endif
	  if (dot.lt.0.1) then
	     dot=0.1 ! tracks at very shallow angles cause problems
	  endif
	  if (dot.ne.0.0) then ! angle ok
             theta=acosd(dot)
	  else ! angle bad
	     theta=0
	     phi=0
	     print*,'Angle problem... '

	     goto 100
	  endif ! angle check
          if (theta.gt.90.0) theta=180.0-theta ! keep between 0 and 90
c         phi (angle with drift direction projected onto detector plane)
c         find projected momentum onto wafer (v1) first
c         (here, 1=transverse, 2=drift, 3=normal direction)
          call dproduct(svt_hit(ihit).p,geom(iwafer).t,dot)
          v1(1)=dot
          call dproduct(svt_hit(ihit).p,geom(iwafer).d,dot)
          v1(2)=dot
          v1(3)=0
          mag1=sqrt(v1(1)*v1(1)+v1(2)*v1(2))
	  if (mag1.eq.0) then
	     phi=0 ! track normal to det surface, phi not defined
	     goto 100
	  endif
	  v1(1)=v1(1)/mag1
	  v1(2)=v1(2)/mag1
	  v2(1)=0
	  v2(2)=1.0
	  v2(3)=0
c         now we are ready to find the angle (should be between 0 and 180)
	  call dproduct(v1,v2,dot)
	  if (dot.gt.1.0.and.dot.le.1.01) dot=1.0 ! safety from roundoff error
	  if (dot.lt.-1.0.and.dot.ge.-1.01) dot=-1.0 ! safety from roundoff err
	  if (dot.ne.0.0) then ! angle ok
	     phi=acosd(dot)
	  else		! angle bad
	     theta=0
	     phi=0
	     goto 100
	  endif ! angle check
       else ! no wafer id
	  Print*,'Can not find wafer id'
	  svt_resolution_sim=STAFCV_BAD
	  return
       endif ! check if geometry available
100    continue
       qtot=svt_qtot(de,theta,srspar_h,srspar)
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c


c------------------------------------------------------------------------------c
c     CALCULATION OF SRS PARAMETERS:                                           c
c------------------------------------------------------------------------------c
	if (srspar(1).vd.eq.0) then ! need to find drit vel
	   tmax=float(srspar(1).nsca)/srspar(1).fsca
	   srspar(1).vd=drift_length/tmax ! vd is same for all wafers now
	   print*,'The drift velocity in SVT is :',srspar(1).vd
	   srspar(1).bucket=drift_length/float(srspar(1).nsca)
	   print*,'The cell size is ',srspar(1).bucket,' by ',srspar(1).pitch
	   srspar(1).thickness=thickness
	endif
c------------------------------------------------------------------------------c


c------------------------------------------------------------------------------c
c     CALCULATION OF WIDTHS:                                                   c
c------------------------------------------------------------------------------c
c       Remember: 1=major axis, 2=minor axis                                   c
c       phi=angle wrt drift direction                                          c
c       z=drift direction, x=transverse direction (from before)                c
c------------------------------------------------------------------------------c
c       Note: phi must be kept away from values 0-10 and 80-90 degrees due to  c
c       some problems with my approximation for the integration.               c
c	if (phi.gt.80) phi=90.00
c	if (phi.lt.10) phi=0.0
c------------------------------------------------------------------------------c
	call svt_width(theta,phi,dist,qtot,width1,width2,srspar_h,srspar)
	if (width1.lt.0.005) width1=0.005 ! put a minimum 50 mic width
	if (width2.lt.0.005) width2=0.005
	c=cosd(phi)
	s=sind(phi)	
	widthx=width1*width2/sqrt((c*width1)**2.+(s*width2)**2.)
	widthz=width1*width2/sqrt((s*width1)**2.+(c*width2)**2.)
	widthxpro=width1*width2/widthz ! projected width along anodes
	widthzpro=width1*width2/widthx ! projected width along drift
	param(1)=qtot
	param(2)=offset
	param(3)=dist
	param(4)=width1
	param(5)=width2
	param(6)=phi
	param(7)=widthxpro
	param(8)=widthzpro
c       calc the uncertainty:
	call svt_uncer(param,uncer_x,uncer_z,srspar_h,srspar) 
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------c
c	SMEARING:							       c
c------------------------------------------------------------------------------c
c       find amount to smear this event by: (only once for each direction)
        smear_drift=gasdev(seed)*uncer_z ! drift direction
        smear_anode=gasdev(seed)*uncer_x ! anode direction
c       x direction ! in tpc coord
	call dproduct(xhat,geom(iwafer).t,dot1) ! from anode direction
	call dproduct(xhat,geom(iwafer).d,dot2) ! from drift direction
        new_x=svt_hit(ihit).x(1)+dot1*smear_anode+dot2*smear_drift
	covar(1)=sqrt((uncer_x*dot1)**2.+(uncer_z*dot2)**2.)
c       y direction ! in tpc coord
	call dproduct(yhat,geom(iwafer).t,dot1) ! from anode direction
	call dproduct(yhat,geom(iwafer).d,dot2) ! from drift direction
        new_y=svt_hit(ihit).x(2)+dot1*smear_anode+dot2*smear_drift
	covar(2)=sqrt((uncer_x*dot1)**2.+(uncer_z*dot2)**2.)
c       z direction ! in tpc coord
	call dproduct(zhat,geom(iwafer).t,dot1) ! from anode direction
	call dproduct(zhat,geom(iwafer).d,dot2) ! from drift direction
        new_z=svt_hit(ihit).x(3)+dot1*smear_anode+dot2*smear_drift
	covar(3)=sqrt((uncer_x*dot1)**2.+(uncer_z*dot2)**2.)
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c


c------------------------------------------------------------------------------c
c	SAVE RESULTS:                                                          c
c------------------------------------------------------------------------------c
c       fill the srs test table for debuging 
        result_h.nok=ihit
        result(ihit).chip_id=svt_hit(ihit).volume_id
        result(ihit).n_anode_samps=nmeasx 
        result(ihit).n_time_samps=nmeasz
	result(ihit).drift_distance=dist
	result(ihit).offset=offset
	result(ihit).phi=phi
	result(ihit).sigma(1)=widthxpro
        result(ihit).sigma(2)=0.0
	result(ihit).sigma(3)=widthzpro
	result(ihit).theta=theta
	result(ihit).x(1)=gcoord(1)
	result(ihit).x(2)=gcoord(2)
	result(ihit).x(3)=gcoord(3)
	result(ihit).x_local(1)=lcoord(1)
	result(ihit).x_local(2)=lcoord(2)
	result(ihit).x_local(3)=lcoord(3)
c       fill the spt table for tracking
	if ((uncer_x.gt.0).and.(uncer_z.gt.0)) then ! ok to save
	   spt_h.nok=spt_h.nok+1
	   spt(spt_h.nok).x(1)=new_x
	   spt(spt_h.nok).x(2)=new_y
	   spt(spt_h.nok).x(3)=new_z
	   spt(spt_h.nok).flag=0 ! unmerged hit for now
	   spt(spt_h.nok).cov(1)=covar(1)
	   spt(spt_h.nok).cov(2)=covar(2)
	   spt(spt_h.nok).cov(3)=covar(3)
	   spt(spt_h.nok).id_mctrack=svt_hit(ihit).track_p
	   spt(spt_h.nok).id_track=0
	   spt(spt_h.nok).id_globtrk=0
	   spt(spt_h.nok).de(1)=svt_hit(ihit).de
	   spt(spt_h.nok).de(2)=0
c	   spt(spt_h.nok).id_cluster=svt_hit(ihit).id_spt
	   spt(spt_h.nok).id_wafer=svt_hit(ihit).volume_id  
	   spt(spt_h.nok).id=spt_h.nok
c          Save info for use in merging:
	   if (srspar(1).merge.ne.0) then
	      if (spt_h.nok.gt.10000) then
		 print*,'Exceeded size of merging arrays, merging off...'
		 srspar(1).merge=0
	      else
		 call decode_id(svt_hit(ihit).volume_id,layer,ladder,wafer)
		 if (layer.gt.config(1).n_layer.or.layer.eq.0) then
		    print*,'Problem with layer number'
		 endif
		 if (ladder.gt.config(1).n_ladder(layer).or.ladder.eq.0) then
		    print*,'Problem with ladder number'
		 endif
		 if (wafer.gt.config(1).n_wafer(layer).or.wafer.eq.0) then
		    print*,'Problem with wafer number'
		 endif
		 npoints(layer,ladder,wafer)=npoints(layer,ladder,wafer)+1
		 points(layer,ladder,wafer,npoints(layer,ladder,wafer))=
     1            spt_h.nok
		 posit(1,spt_h.nok)=lcoord(1)           
		 posit(2,spt_h.nok)=lcoord(2)           
		 uncer(1,spt_h.nok)=uncer_z
		 uncer(2,spt_h.nok)=uncer_x
	      endif
	    endif
	endif
	enddo ! loop over hits
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c


c------------------------------------------------------------------------------c
c	MERGE
c------------------------------------------------------------------------------c
	if (srspar(1).merge.eq.0) goto 999 ! skip merging
	do layer=1,config(1).n_layer
	   do ladder=1,config(1).n_ladder(layer)
	      do wafer=1,config(1).n_wafer(layer)
		 if (npoints(layer,ladder,wafer).gt.1) then
		    do ihit=1,npoints(layer,ladder,wafer)-1
		       do jhit=ihit+1,npoints(layer,ladder,wafer)
			  ii=points(layer,ladder,wafer,ihit)
			  jj=points(layer,ladder,wafer,jhit)
			  deltax=abs(posit(1,ii)-posit(1,jj))
			  deltay=abs(posit(2,ii)-posit(2,jj))
			  ux=sqrt(uncer(1,ii)**2+uncer(1,jj)**2)
			  uy=sqrt(uncer(2,ii)**2+uncer(2,jj)**2)
			  seperation=sqrt(deltax**2.+deltay**2.)
			  resolution=sqrt(ux**2.0+uy**2.0)
			  if (seperation.le.resolution) then ! merge
c                            we will merge i with j, weighting by ionization
			     wi=spt(ii).de(1)
			     wj=spt(jj).de(1)
			     do kk=1,3
				spt(jj).x(kk)=
     1                           (wi*spt(ii).x(kk)+
     2                           wj*spt(jj).x(kk))/
     3                           (wi+wj)
				spt(jj).cov(kk)=
     1                           sqrt(spt(ii).cov(kk)**2+
     2                           spt(ii).cov(kk)**2.0)
			     enddo
			     spt(jj).id_mctrack=0
			     spt(jj).id_cluster=0
			     spt(jj).de(1)=wi+wj
			     spt(ii).flag=-1
			     spt(jj).flag=1
			  endif
		       enddo
		    enddo
		 endif
	      enddo
	   enddo
	enddo
c       Now we loop through, getting rid of the merged (flag=-1) hits
	old_ok=spt_h.nok
	new_ok=0
	do ii=1,old_ok
	   if (spt(ii).flag.ge.0) then
	      new_ok=new_ok+1
	      if (ii.ne.new_ok) then
		 spt(new_ok).flag=spt(ii).flag
		 spt(new_ok).flag=new_ok
		 spt(new_ok).id_cluster=spt(ii).id_cluster
		 spt(new_ok).id_mctrack=spt(ii).id_mctrack
		 spt(new_ok).id_track=spt(ii).id_track
		 spt(new_ok).id_wafer=spt(ii).id_wafer
		 do kk=1,3
		    spt(new_ok).cov(kk)=spt(ii).cov(kk)
		    spt(new_ok).x(kk)=spt(ii).x(kk)
		 enddo
		 do kk=1,2
		    spt(new_ok).de(kk)=spt(ii).de(kk)
		 enddo
	      endif
	   endif
	enddo
	print*,'Number of svt space points before merging = ',old_ok
	print*,'Number of svt space points after merging = ',new_ok
	spt_h.nok=new_ok
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c



c------------------------------------------------------------------------------c
c	DONE:								       c
c------------------------------------------------------------------------------c
 999	continue
	svt_resolution_sim=STAFCV_OK
	return
	end
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c*****************************PROGRAM BOUNDRY**********************************c

c------------------------------------------------------------------------------c
c	HEADER:								       c
c------------------------------------------------------------------------------c
	function svt_qtot(de,angle,srspar_h,srspar)
c									       c
c	This program calculates the number of electons for a minimum	       c
c	ioniizing particle, assuming 20000 electrons for normal incidence.     c
c	Begun by W.K.W. on Aug-05-1992					       c
c       Changed to use GEANT de if available on Dec-07-1993                    c
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------c
c	VARIABLE DECLARATIONS AND DATA:					       c
c------------------------------------------------------------------------------c
c	implicit none
#include "PAM.inc"
#include "srs_srspar.inc"

      RECORD      /TABLE_HEAD_ST/                srspar_h
      RECORD      /SRS_SRSPAR_ST/               srspar(*)

c       integer i,j,k,ii,jj,kk
	real svt_qtot,pi,angle,de
c       real length
        real dqde
	data pi /3.1416/
        data dqde /2.78e8/
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------c
c	CALCULATE NUMBER OF CHARGES:					       c
c------------------------------------------------------------------------------c
           svt_qtot=de*dqde
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------c
c	DONE:								       c
c------------------------------------------------------------------------------c
	return
	end
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c*****************************PROGRAM BOUNDRY**********************************c

c------------------------------------------------------------------------------c
c	HEADER:								       c
c------------------------------------------------------------------------------c
	subroutine svt_width(theta,phi,distance,qtot,width1,width2,
     1    srspar_h,srspar)
c									       c
c	This program calculates the width of an electron cloud when it         c
c	gets to the anodes. (when phi=0 axis 1 is parallel to drift)           c
c	Begun by W.K.W. on Aug-05-1992					       c
c       Modified extensivly to remove geometrical bug Feb-15-1994              c
c------------------------------------------------------------------------------c
c	Effects this code should include: (x means it's in there!)	       c
c	1) diffusion (x)						       c
c	2) Coulomb repulsion (x for 20000 electrons due to normal incidence)   c
c	3) Responce of shaping electronics (x)				       c
c	4) Pad geometry	(x) but needs more checking		               c
c	5) Coulomb field of holes					       c
c------------------------------------------------------------------------------c
c	Effects 3) and 4) must be added parallel to the drift direction, not   c
c	axis 1 or 2.								       c
c------------------------------------------------------------------------------c
c	Note : I am still studying the relationship between the Coulomb        c
c	repulsion and angle of incidence. A more complete parameteriization    c
c	will be installed at a later date.                                     c
c------------------------------------------------------------------------------c
c	Units:								       c
c	length = micron 						       c
c	time   = nanosecond						       c
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------c
c	VARIABLE DECLARATIONS AND DATA:					       c
c------------------------------------------------------------------------------c
c	implicit none

#include "PAM.inc"
#include "srs_srspar.inc"

      RECORD      /TABLE_HEAD_ST/                srspar_h
      RECORD      /SRS_SRSPAR_ST/               srspar(*)

c	integer i,j,k,ii,jj,kk
c	real vec(3)coul_corr_fact,widthx,widthz,widthxpro,widthzpro
        real width1,width2
	real a,b,d,electronics,anode_geom
	real theta,phi,distance,qtot,time,angle
	real sigma_angle,sigma_diff
	real s,c
	data a,b,d,anode_geom /1.245,-2.85e4,34.0,0.0/
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------c
c	INITIALIIZATION:						       c
c------------------------------------------------------------------------------c
	electronics=srspar(1).shaper*srspar(1).vd
	angle=theta
	time=distance/srspar(1).vd
	s=sind(phi)
	c=cosd(phi)
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------c
c	SUM WIDTHS DUE TO EFFECTS ALONG #1 AXIS:                               c
c	(MAJOR AXIS OF CHARGE ELLIPSOID)                	               c
c------------------------------------------------------------------------------c
	sigma_diff=sqrt(2.0*d*time)
	sigma_angle=srspar(1).thickness*tand(angle)/sqrt(12.0)
	width1=sqrt(sigma_diff**2.0+sigma_angle**2.0)
	width1=width1*(a+time*b) ! coulomb repulsion correction
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------c
c	SUM WIDTHS DUE TO EFFECTS ALONG #2 AXIS:			       c
c	(MINOR AXIS OF CHARGE ELLIPSOID)                	               c
c------------------------------------------------------------------------------c
	sigma_diff=sqrt(2.0*d*time)
	width2=sigma_diff
	width2=width2*(a+time*b)
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------c
c	SUM WIDTHS THAT ACT ONLY IN THE DRIFT DIRECTION:		       c
c------------------------------------------------------------------------------c
c       Now using an approximation! (jan 17,94) wkw
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c
c	if (phi.lt.45.or.phi.gt.135) then ! major axis closer to drift direction
c	   width1=sqrt(width1**2.0+electronics**2.+anode_geom**2)
c        else ! minor axis closer to drift direction
c	   width2=sqrt(width2**2.0+electronics**2.+anode_geom**2)
c	endif
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------c
c	SUM WIDTHS THAT ACT ONLY IN THE DRIFT DIRECTION:		       c
c------------------------------------------------------------------------------c
c       New approx on March 14, 1994                                           c
c------------------------------------------------------------------------------c
	width1=sqrt(width1**2.0+(c*electronics)**2.0)
	width2=sqrt(width2**2.0+(s*electronics)**2.0)
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------c
c	DONE:								       c
c------------------------------------------------------------------------------c
	return
	end
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c


c*****************************PROGRAM BOUNDRY**********************************c

c------------------------------------------------------------------------------c
c	HEADER:								       c
c------------------------------------------------------------------------------c
	subroutine svt_uncer(true,uncer_x,uncer_z,srspar_h,srspar)
c									       c
c	This program calculates the minimum uncertainty in the position	       c
c	of the mean of an electron cloud along the anode direction	       c
c	and in the time direction.  			                       c
c------------------------------------------------------------------------------c
c	The true array stores the parameters of the pulse:	               c
c	charge=param(1)							       c
c       anode offset=param(2)						       c
c	drift distance=param(3)						       c
c	sigma major axis=param(4)					       c
c	sigma minor axis=param(5)					       c
c	phi angle wrt drift=param(6)					       c
c	x proj width=param(7) (depends on 4, 5, and 6)       		       c
c	z proj width=param(8) (depends on 4, 5, and 6)			       c
c	The true array stores the parameters which represent the pulse.        c
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------c
c	VARIABLE DECLARATIONS AND DATA:					       c
c------------------------------------------------------------------------------c
c	implicit none

#include "PAM.inc"
#include "srs_srspar.inc"

      RECORD      /TABLE_HEAD_ST/                srspar_h
      RECORD      /SRS_SRSPAR_ST/               srspar(*)

c	integer i,j,k,ii,jj,kk
c	real qtot,sigmax,sigmaz,x0,z0,angle,signal
	real true(8)
c       real trial(8),c,s
	real uncer_x,uncer_z,junk

c------------------------------------------------------------------------------c
c       samples(1,n),samples(2,n) = x and z coordinate of sample               c
c       samplesx(1,n),samplesz(1,n) anode,t bucket hit in x or z projection    c
c       samplesx(2,n) earliest t bucket hit for anode hit n                    c
c       samplesz(2,n) earliest anode hit for t bucket hit n                    c
c       samplesx(3,n) latest t bucket hit for anode hit n                      c
c       samplesz(3,n) latest anode hit for t bucket hit n                      c
c------------------------------------------------------------------------------c
#include "svt_samples_common.inc"
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c


c------------------------------------------------------------------------------c
c	INITIALIIZATION:						       c
c------------------------------------------------------------------------------c
	call svt_samples(true,srspar_h,srspar)
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------c
c	CALCULATE UNCERTAINTY FOR X AND Z PROJECTIONS USING SAMPLES:	       c
c------------------------------------------------------------------------------c
	if ((nmeasz.ge.4).or.(nmeasx.ge.4)) then
	    call svt_4_samp(true,junk,uncer_x,uncer_z,junk,junk,
     1	      srspar_h,srspar)
	endif
	if ((nmeasz.eq.3).or.(nmeasx.eq.3)) then
	    call svt_3_samp(true,uncer_x,uncer_z,junk,junk,
     1	      srspar_h,srspar)
	endif
	if ((nmeasz.le.2).or.(nmeasx.le.2)) then
	    call svt_2_samp(true,uncer_x,uncer_z,
     1	      srspar_h,srspar)
	endif
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------c
c	DONE:								       c
c------------------------------------------------------------------------------c
	return
	end
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c*****************************PROGRAM BOUNDRY**********************************c

c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c
c	HEADER:								       c
c------------------------------------------------------------------------------c
	function svt_charge(iix,iiz,par,srspar_h,srspar)
c									       c
c	This program integrates a double gaussian charge distribution	       c
c	to find the charge on anode iix in time bucket iiz.		       c
c	Begun by W.K.W. on Aug-04-1992					       c
c	Modified for two dimensions on Aug-16-1992                             c
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------c
c	VARIABLE DECLARATIONS AND DATA:					       c
c------------------------------------------------------------------------------c
c	implicit none

#include "PAM.inc"
#include "srs_srspar.inc"

      RECORD      /TABLE_HEAD_ST/                srspar_h
      RECORD      /SRS_SRSPAR_ST/               srspar(*)

c	integer i,j,k,ii,jj,kk
	real*8 par(8),svt_charge,xmin,xmax,zmin,zmax
	integer iix,iiz
	real svt_integrate_2d_gauss
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------c
c	INITIALIIZATIONS:						       c
c------------------------------------------------------------------------------c
	xmin=float(iix)*srspar(1).pitch-srspar(1).pitch/2.0
	xmax=float(iix)*srspar(1).pitch+srspar(1).pitch/2.0
	zmin=float(iiz)*srspar(1).bucket-srspar(1).bucket/2.0
	zmax=float(iiz)*srspar(1).bucket+srspar(1).bucket/2.0
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------c
c	CALCULATE THE CHARGE:						       c
c------------------------------------------------------------------------------c
	svt_charge=svt_integrate_2d_gauss(xmin,xmax,zmin,zmax,par,
     1     srspar_h,srspar)
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------c
c	DONE:								       c
c------------------------------------------------------------------------------c
	return
	end
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c*****************************PROGRAM BOUNDRY**********************************c

c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c
c	HEADER:								       c
c------------------------------------------------------------------------------c
      function svt_integrate_2d_gauss(xmin,xmax,zmin,zmax,par, 
     1   srspar_h,srspar)

c	function svt_integrate_2d_gauss(xmin,xmax,zmin,zmax,p,
c									       c
c	This program integrates a 2d gaussian using romberg integration.       c
c	Begun by W.K.W. on Aug-16-1992					       c
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------c
c	VARIABLE DECLARATIONS AND DATA:					       c
c------------------------------------------------------------------------------c
c	implicit none

#include "PAM.inc"
#include "srs_srspar.inc"

      RECORD      /TABLE_HEAD_ST/                srspar_h
      RECORD      /SRS_SRSPAR_ST/               srspar(*)

c	integer i,j,k,ii,jj,kk
c	real xmin,xmax,zmin,zmax,limits(2,2),p(8)
c	real integral,svt_integrate_2d_gauss
      integer i
c     integer j,k,ii,jj,kk
      real par(8)
      real*8 xmin,xmax,zmin,zmax,x(2),xlow,xup
      real*8 svt_integrate_2d_gauss
      real*8 pp(8)
      real*8 dgmlt2
      external fsub2
      common /for_2d_int/ pp,xlow,xup
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------c
c	INITIALIIZATIONS:						       c
c------------------------------------------------------------------------------c
c	limits(1,1)=xmin
c	limits(1,2)=xmax
c	limits(2,1)=zmin
c	limits(2,2)=zmax
      xlow=xmin
      xup=xmax
      do i=1,8
         pp(i)=par(i)
      enddo
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------c
c     INTEGRATE:			                        	       c
c------------------------------------------------------------------------------c
c        call gauss2d(p,limits,integral,srspar_h,srspar)
c	svt_integrate_2d_gauss=integral
      svt_integrate_2d_gauss=dgmlt2(fsub2,zmin,zmax,1,6,x)
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c


c------------------------------------------------------------------------------c
c	DONE:								       c
c------------------------------------------------------------------------------c
	return
	end
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c*****************************PROGRAM BOUNDRY**********************************c

c------------------------------------------------------------------------------c
c	HEADER:								       c
c------------------------------------------------------------------------------c
	subroutine svt_samples(par,srspar_h,srspar)
c									       c
c	This program calculates the number of samples detected by the SDD      c
c	using the criterea that the uncertainty in the signal must be less     c
c	than the signal. The coordinate indices of good samples are stored     c
c	in an array for later use.					       c
c	Begun by W.K.W. on Aug-16-1992					       c
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------c
c	VARIABLE DECLARATIONS AND DATA:					       c
c------------------------------------------------------------------------------c
c	implicit none

#include "PAM.inc"
#include "srs_srspar.inc"

      RECORD      /TABLE_HEAD_ST/                srspar_h
      RECORD      /SRS_SRSPAR_ST/               srspar(*)

c	integer i,k,ii,jj,kk
	integer j,iixmin,iixmax,iizmin,iizmax,iix,iiz,iix_last,event
	integer iixmid,iizmid,iixsig,iizsig
	real par(8),signal,err_sig,svt_charge
c       real x,z

c------------------------------------------------------------------------------c
c       samples(1,n),samples(2,n) = x and z coordinate of sample               c
c       samplesx(1,n),samplesz(1,n) anode,t bucket hit in x or z projection    c
c       samplesx(2,n) earliest t bucket hit for anode hit n                    c
c       samplesz(2,n) earliest anode hit for t bucket hit n                    c
c       samplesx(3,n) latest t bucket hit for anode hit n                      c
c       samplesz(3,n) latest anode hit for t bucket hit n                      c
c------------------------------------------------------------------------------c
#include "svt_samples_common.inc"
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------c
c	INITIALIIZATION:						       c
c------------------------------------------------------------------------------c
	iixmid=nint(par(2)/srspar(1).pitch)
	iizmid=nint(par(3)/srspar(1).bucket)
	iixsig=nint(par(7)/srspar(1).pitch)
	iizsig=nint(par(8)/srspar(1).bucket)
	if (iixsig.gt.10) iixsig=10
	if (iizsig.gt.10) iizsig=10
	iixmin=iixmid-2*iixsig-1
	iixmax=iixmid+2*iixsig+1
	iizmin=iizmid-2*iizsig-1
	iizmax=iizmid+2*iizsig+1
	if (iizmin.lt.1) iizmin=1
	if (nmeasx.ne.0) then
	    do iix=1,nmeasx
		true_valuesx(iix)=0
	    enddo
	endif
	if (nmeasz.ne.0) then
	    do iiz=1,nmeasz
		true_valuesz(iiz)=0
	    enddo
	endif
	nmeas=0
	nmeasx=0
	nmeasz=0
	iix_last=iixmin-1
	event=event+1
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c


c------------------------------------------------------------------------------c
c	FIND THE GOOD SAMPLES:						       c
c------------------------------------------------------------------------------c
	do iix=iixmin,iixmax
	do iiz=iizmin,iizmax
	    signal=svt_charge(iix,iiz,par,srspar_h,srspar)
	    err_sig=sqrt(signal+srspar(1).enc**2.0)
	    if (err_sig.le.signal) then
		nmeas=nmeas+1
		if (nmeas.gt.200) then	
		    print*,'Too many samples...'
		    goto 101
		endif
		samples(1,nmeas)=iix
		samples(2,nmeas)=iiz
		if (iix.ne.iix_last) then ! first hit on this pad
		    nmeasx=nmeasx+1
		    true_valuesx(nmeasx)=signal
		    iix_last=iix
		    samplesx(1,nmeasx)=iix ! the pad hit
		    samplesx(2,nmeasx)=iiz ! the earliest time bucket
		    samplesx(3,nmeasx)=iiz ! the latest time bucket
		    true_valuesx(nmeasx)=0
		endif
		do j=1,nmeasz
		    if (samplesz(1,j).eq.iiz) then ! the anode in this t bucket
			true_valuesz(j)=true_valuesz(j)+signal ! add to t bucket
			samplesz(3,j)=iix ! update the latest anode
			goto 100
		    endif
		enddo
		nmeasz=nmeasz+1	! first hit in this time bucket
		true_valuesz(nmeasz)=signal
		samplesz(1,nmeasz)=iiz
		samplesz(2,nmeasz)=iix ! the first anode for this time bucket
		samplesz(3,nmeasz)=iix ! the last anode for this time bucket
100		continue
		true_valuesx(nmeasx)=true_valuesx(nmeasx)+signal ! add to anode
		samplesx(3,nmeasx)=iiz ! updata the latest time bucket
		true_values(nmeas)=signal
	    endif ! good signal
	enddo
	enddo
101	continue
c	if (nmeasx.eq.0) then 
c	   print*,'nmeasx=0'
c	endif
c	if (nmeasz.eq.0) then
c	   print*,'nmeasz=0'
c	endif   

c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------c
c	SORT THE PROJECTED SAMPLES IN DESCENDING ORDER:			       c
c------------------------------------------------------------------------------c
	call numrec_sort(nmeasx,true_valuesx,samplesx)
	call numrec_sort(nmeasz,true_valuesz,samplesz)
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------c
c	DONE:								       c
c------------------------------------------------------------------------------c
	return
	end
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c*****************************PROGRAM BOUNDRY**********************************c

c------------------------------------------------------------------------------c
c	HEADER:								       c
c------------------------------------------------------------------------------c
	subroutine svt_2_samp(true,dx,dz,srspar_h,srspar)
c									       c
c	This program find the uncertainty in the mean of two		       c
c	independent gaussian distributions using 2 samples each.	       c
c------------------------------------------------------------------------------c
c	Begun by W.K.W. on Aug-20-1992					       c
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------c
c	VARIABLE DECLARATIONS AND DATA:					       c
c------------------------------------------------------------------------------c
c	implicit none

#include "PAM.inc"
#include "srs_srspar.inc"

      RECORD      /TABLE_HEAD_ST/                srspar_h
      RECORD      /SRS_SRSPAR_ST/               srspar(*)

	integer i
c       integer j,k,ii,jj,kk
	real x(2),z(2),sigmax,sigmaz,dx,dz,true(8)
	real meanx,meanz,xoff,zoff,errx(2),errz(2),channels

c------------------------------------------------------------------------------c
c       samples(1,n),samples(2,n) = x and z coordinate of sample               c
c       samplesx(1,n),samplesz(1,n) anode,t bucket hit in x or z projection    c
c       samplesx(2,n) earliest t bucket hit for anode hit n                    c
c       samplesz(2,n) earliest anode hit for t bucket hit n                    c
c       samplesx(3,n) latest t bucket hit for anode hit n                      c
c       samplesz(3,n) latest anode hit for t bucket hit n                      c
c------------------------------------------------------------------------------c
#include "svt_samples_common.inc"
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------c
c	INITIALIIZATION:							       c
c------------------------------------------------------------------------------c
	do i=1,2
	    x(i)=true_valuesx(i)
	    z(i)=true_valuesz(i)
	    channels=float(samplesx(3,i))-float(samplesx(2,i))+1.0
	    errx(i)=sqrt(x(i)+channels*srspar(1).enc**2.0)
	    channels=float(samplesz(3,i))-float(samplesz(2,i))+1.0
	    errz(i)=sqrt(z(i)+channels*srspar(1).enc**2.0)
	enddo
	meanx=true(2)
	meanz=true(3)
	sigmax=true(7)
	sigmaz=true(8)
	xoff=meanx-float(nint(meanx/srspar(1).pitch))*srspar(1).pitch
	zoff=meanz-float(nint(meanz/srspar(1).bucket))*srspar(1).bucket
	xoff=(srspar(1).pitch/2.0)-abs(xoff)
	zoff=(srspar(1).bucket/2.0)-abs(zoff)
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c


c------------------------------------------------------------------------------c
c	FIND POSITION UNCERTAINTIES:					       c
c------------------------------------------------------------------------------c
	if (nmeasx.eq.2) then
	    dx=(errx(2)/x(2))**2.+(errx(1)/x(1))**2.
	    dx=(sqrt(dx)*(sigmax)**2.0)/srspar(1).pitch
	else if (nmeasx.eq.1) then
	   dx=srspar(1).pitch/sqrt(12.0)
	else if (nmeasx.eq.0) then
	   dx=0			! we will throw this away
	endif
	if (nmeasz.eq.2) then
	    dz=(errz(2)/z(2))**2.+(errz(1)/z(1))**2.
	    dz=(sqrt(dz)*(sigmaz)**2.0)/srspar(1).bucket
	else if (nmeasz.eq.1) then 
	   dz=srspar(1).bucket/sqrt(12.0)
	else if (nmeasx.eq.0) then
	   dz=0			! we will throw this away
	endif
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------c
c	DONE:								       c
c------------------------------------------------------------------------------c
	return
	end
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c*****************************PROGRAM BOUNDRY**********************************c

c------------------------------------------------------------------------------c
c       HEADER:                                                                c
c------------------------------------------------------------------------------c
        subroutine svt_3_samp(true,dx,dz,dsigx,dsigz,srspar_h,srspar)
c                                                                              c
c       This program find the uncertainty in the mean and sigma of two         c
c       independent gaussian distributions using 3 samples each.               c
c------------------------------------------------------------------------------c
c       Begun by W.K.W. on Aug-20-1992                                         c
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------c
c       VARIABLE DECLARATIONS AND DATA:                                        c
c------------------------------------------------------------------------------c
c        implicit none

#include "PAM.inc"
#include "srs_srspar.inc"

      RECORD      /TABLE_HEAD_ST/                srspar_h
      RECORD      /SRS_SRSPAR_ST/               srspar(*)

        integer i
c       integer j,k,ii,jj,kk
        real x(3),z(3),sigmax,sigmaz,dx,dz,dsigx,dsigz,true(8)
        real meanx,meanz,xoff,zoff,err_x(3),err_z(3),channels

c------------------------------------------------------------------------------c
c       samples(1,n),samples(2,n) = x and z coordinate of sample               c
c       samplesx(1,n),samplesz(1,n) anode,t bucket hit in x or z projection    c
c       samplesx(2,n) earliest t bucket hit for anode hit n                    c
c       samplesz(2,n) earliest anode hit for t bucket hit n                    c
c       samplesx(3,n) latest t bucket hit for anode hit n                      c
c       samplesz(3,n) latest anode hit for t bucket hit n                      c
c------------------------------------------------------------------------------c
#include "svt_samples_common.inc"
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------c
c       INITIALIIZATION:                                                        c
c------------------------------------------------------------------------------c
        do i=1,3
            x(i)=true_valuesx(i)
            z(i)=true_valuesz(i)
            channels=float(samplesx(3,i))-float(samplesx(2,i))+1.0
            err_x(i)=sqrt(x(i)+channels*srspar(1).enc**2.0)
            channels=float(samplesz(3,i))-float(samplesz(2,i))+1.0
            err_z(i)=sqrt(z(i)+channels*srspar(1).enc**2.0)
        enddo
        meanx=true(2)
        meanz=true(3)
        sigmax=true(7)
        sigmaz=true(8)
        xoff=meanx-float(nint(meanx/srspar(1).pitch))*srspar(1).pitch
        zoff=meanz-float(nint(meanz/srspar(1).bucket))*srspar(1).bucket
        xoff=(srspar(1).pitch/2.0)-abs(xoff)
        zoff=(srspar(1).bucket/2.0)-abs(zoff)
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------c
c       FIND POSITION UNCERTAINTIES:                                           c
c------------------------------------------------------------------------------c
        if (nmeasx.eq.3) then
            dx=((2.0*(abs(xoff)-srspar(1).pitch/2.0))**2.0)*
     1       (err_x(2)/x(2))**2.
            dx=dx+(xoff*err_x(3)/x(3))**2.
            dx=dx+((abs(xoff)-srspar(1).pitch)**2.0)*
     1       (err_x(1)/x(1))**2.
            dx=sqrt(dx)*(sigmax/srspar(1).pitch)**2.0
        endif
        if (nmeasz.eq.3) then
            dz=((2.0*(abs(zoff)-srspar(1).bucket/2.0))**2.0)*
     1       (err_z(2)/z(2))**2.
            dz=dz+(zoff*err_z(3)/z(3))**2.
            dz=dz+((abs(zoff)-srspar(1).bucket)**2.0)*
     1       (err_z(1)/z(1))**2.
            dz=sqrt(dz)*(sigmaz/srspar(1).bucket)**2.0
        endif
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------c
c       FIND WIDTH  UNCERTAINTIES:                                             c
c------------------------------------------------------------------------------c
        if (nmeasx.eq.3) then
            dsigx=(err_x(2)/x(2))**2.+(err_x(3)/x(3))**2.+
     1	      (err_x(1)/x(1))**2.
            dsigx=.5*sqrt(dsigx)*sigmax*(sigmax/srspar(1).pitch)**2.0
        endif
        if (nmeasz.eq.3) then
            dsigz=(err_z(2)/z(2))**2.+(err_z(3)/z(3))**2.+
     1	      (err_z(1)/z(1))**2.
            dsigz=.5*sqrt(dsigz)*sigmaz*(sigmaz/srspar(1).bucket)**2.0
        endif
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------c
c       DONE:                                                                  c
c------------------------------------------------------------------------------c
        return
        end
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c*****************************PROGRAM BOUNDRY**********************************c

c------------------------------------------------------------------------------c
c 	the following subroutines are from "Numerical Recipes" by              c
c	Press, Flannery, Teukolsky, and Vettering, Cambridge University        c
c	Press, (1990).
c------------------------------------------------------------------------------c

	subroutine gauss2d(p,limits,integ,srspar_h,srspar)
c	implicit none

#include "PAM.inc"
#include "srs_srspar.inc"

      RECORD      /TABLE_HEAD_ST/                srspar_h
      RECORD      /SRS_SRSPAR_ST/               srspar(*)

c	integer i,j,k,ii,jj,kk
	real limits(2,2),p(8),integ,integ1,integ2
	real qdensx,qdensz,xcenter,zcenter
	external qdensx
	external qdensz
	common /position/ xcenter,zcenter
c------------------------------------------------------------------------------c

c       this assumes that x and z integrals factorize, the angle is negligable
c       so it is only good in small bins

	xcenter=limits(1,1)+(limits(1,2)-limits(1,1))/2.0
	zcenter=limits(2,1)+(limits(2,2)-limits(2,1))/2.0

	call qromb(qdensx,p,limits(1,1),limits(1,2),integ1)
	call qromb(qdensz,p,limits(2,1),limits(2,2),integ2)
	integ=integ1*integ2*p(1)
	
	return

	end

      SUBROUTINE TRAPZD(FUNC,p,A,B,S,N)
	real func
	external func
      REAL P(8),a,b,s,tnm,del,x,sum
      integer n,it,j
      IF (N.EQ.1) THEN
        S=0.5*(B-A)*(FUNC(A,P)+FUNC(B,P))
        IT=1
      ELSE
        TNM=IT
        DEL=(B-A)/TNM
        X=A+0.5*DEL
        SUM=0.
        DO 11 J=1,IT
          SUM=SUM+FUNC(X,P)
          X=X+DEL
11      CONTINUE
        S=0.5*(S+(B-A)*SUM/TNM)
        IT=2*IT
      ENDIF
      RETURN
      END

      SUBROUTINE POLINT(XA,YA,N,X,Y,DY)
      real x,y,dy,dif,dift,ho,hp,w,den
      integer nmax,i,ns,n,m
      PARAMETER (NMAX=10) 
      REAL XA(N),YA(N),C(NMAX),D(NMAX)
	do i=1,n
	    if (ya(i).ne.0) goto 10
	enddo
	y=0
	dy=0
	return
10	continue
      NS=1
      DIF=ABS(X-XA(1))
      DO 11 I=1,N 
        DIFT=ABS(X-XA(I))
        IF (DIFT.LT.DIF) THEN
          NS=I
          DIF=DIFT
        ENDIF
        C(I)=YA(I)
        D(I)=YA(I)
11    CONTINUE
      Y=YA(NS)
      NS=NS-1
      DO 13 M=1,N-1
        DO 12 I=1,N-M
          HO=XA(I)-X
          HP=XA(I+M)-X
          W=C(I+1)-D(I)
          DEN=HO-HP
c          IF(DEN.EQ.0.)PAUSE
          IF(DEN.NE.0.) then
          DEN=W/DEN
	  endif
          D(I)=HP*DEN
          C(I)=HO*DEN
12      CONTINUE
        IF (2*NS.LT.N-M)THEN
          DY=C(NS+1)
        ELSE
          DY=D(NS)
          NS=NS-1
        ENDIF
        Y=Y+DY
13    CONTINUE
      RETURN
      END
	


	function qdensx(x,P)
c       This function is for the integration of the 2d gaussian distribution.
c       By breaking up the qdens into x and y componants, I am assuming that
c       the problem can be factored locally, ie the angle doesn't make a big
c       difference in one space time bucket. wkw

c	implicit none
c	integer i,j,k,ii,jj,kk
	real p(8),x0prime,n,sigx
	real angle,sig1,sig2,xcenter,zcenter
	real c,s,x,x0,z0,pi,expon,qdensx
	data pi /3.1416/
	common /position/ xcenter,zcenter

	n=p(1)
        x0=p(2)
 	z0=p(3)
        sig1=p(4)
	sig2=p(5)
	angle=p(6)

	c=cosd(angle)
	s=sind(angle)
	if (angle.ne.90.0.and.angle.ne.270.0) then
	   x0prime=x0+tand(angle)*(zcenter-z0) ! center of gaussian in this z slice
	else
	   x0prime=x0
	endif
	sigx=sig1*sig2/sqrt((c*sig1)**2.+(s*sig2)**2.) ! x width in any z slice

	expon=exp((-(x-x0prime)**2.0)/(2.0*sigx**2.0))
	qdensx=expon/sqrt(2.0*pi*sigx**2.)

	return
	end

	function qdensz(z,P)
c       This function is for the integration of the 2d gaussian distribution.
c       By breaking up the qdens into x and y componants, I am assuming that
c       the problem can be factored locally, ie the angle doesn't make a big
c       difference in one space time bucket. wkw

c	implicit none
c	integer i,j,k,ii,jj,kk
	real p(8),z0prime,n,sigz
	real angle,sig1,sig2,xcenter,zcenter
	real c,s,z,x0,z0,pi,expon,qdensz
	data pi /3.1416/
	common /position/ xcenter,zcenter
 
	n=p(1)
        x0=p(2)
 	z0=p(3)
        sig1=p(4)
	sig2=p(5)
	angle=p(6)

	c=cosd(angle)
	s=sind(angle)
	if (angle.ne.0.0.and.angle.ne.180.0) then
	   z0prime=z0+(xcenter-x0)/tand(angle) ! center of gaussian in this x slice
	else
	   z0prime=z0
	endif
	sigz=sig1*sig2/sqrt((c*sig2)**2.+(s*sig1)**2.) ! z width in any x slice

	expon=exp((-(z-z0prime)**2.0)/(2.0*sigz**2.0))
	qdensz=expon/sqrt(2.0*pi*sigz**2.)

	return
	end


      SUBROUTINE QROMB(FUNC,p,A,B,SS)
      REAL P(8),eps,a,b,ss,dss,func
      integer jmax,jmaxp,k,km,j,l
      PARAMETER(EPS=1.E-4,JMAX=20,JMAXP=JMAX+1,K=5,KM=4)
      REAL S(JMAXP),H(JMAXP)
      H(1)=1.
      DO 11 J=1,JMAX
        CALL TRAPZD(FUNC,p,A,B,S(J),J)
        IF (J.GE.K) THEN
	  L=J-KM
          CALL POLINT(H(L),S(L),K,0.,SS,DSS)
          IF (ABS(DSS).LE.EPS*ABS(SS)) THEN
	    RETURN
	  ENDIF
        ENDIF
        S(J+1)=S(J)
        H(J+1)=0.25*H(J)
11    CONTINUE
c      PAUSE 'Too many steps.'
      print*, 'Too many steps.'
      END

      SUBROUTINE NUMREC_SORT(N,ARR,BRR)
      integer n,j,ii,i
      REAL ARR(200),BRR(3,200),B(3),a
      DO 12 J=2,N
        A=ARR(J)
	DO II=1,3
          B(II)=BRR(II,J)
	ENDDO
        DO 11 I=J-1,1,-1
          IF(ARR(I).GE.A)GO TO 10
          ARR(I+1)=ARR(I)
	  DO II=1,3
            BRR(II,I+1)=BRR(II,I)
	  ENDDO
11      CONTINUE
        I=0
10      ARR(I+1)=A
	DO II=1,3
      	  BRR(II,I+1)=B(II)
	ENDDO
12    CONTINUE
      RETURN
      END

c*****************************PROGRAM BOUNDRY**********************************c

	subroutine dproduct(vector1,vector2,dot)
c	implicit none
	integer i
c       integer j,k,ii,jj,kk
	real vector1(3),vector2(3),dot
	dot=0
	do i=1,3
	    dot=dot+vector1(i)*vector2(i)
	enddo
	return
	end

	subroutine cproduct(vector1,vector2,vector3)
c	implicit none
c	integer i,j,k,ii,jj,kk
	real vector1(3),vector2(3),vector3(3)
	vector3(1)=vector1(2)*vector2(3)-vector1(3)*vector2(2)
	vector3(2)=vector1(3)*vector2(1)-vector1(1)*vector2(3)
	vector3(3)=vector1(1)*vector2(2)-vector1(2)*vector2(1)
	return
	end

c*****************************PROGRAM BOUNDRY**********************************c

c------------------------------------------------------------------------------c
c       HEADER:                                                                c
c------------------------------------------------------------------------------c
	subroutine svt_4_samp(true,dn,dx,dz,dsigx,dsigz,srspar_h,srspar)
c                                                                              c
c       This program calculates the uncertainty in the position 	       c
c       assuming a gaussian fit to the	projected distribuitons.	       c
c	Begun by W.K.W. on Aug-19-1992					       c
c       Redone by W.K.W. on Feb-16-1994                                        c
c       This version calculates the derivative numerically.                    c
c------------------------------------------------------------------------------c
c	The true array stores the parameters of the pulse:	               c
c	charge=param(1)							       c
c       anode offset=param(2)						       c
c	drift distance=param(3)						       c
c	sigma major axis=param(4)					       c
c	sigma minor axis=param(5)					       c
c	phi angle of major axis wrt drift=param(6)			       c
c	x proj width=param(7) (depends on 4, 5, and 6)       		       c
c	z proj width=param(8) (depends on 4, 5, and 6)			       c
c       The ENC per time bucket is a parameter in svt_parameters_inc           c
c	The true array stores the parameters which represent the pulse.        c
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------c
c       VARIABLE DECLARATIONS AND DATA:                                        c
c------------------------------------------------------------------------------c
c        implicit none

#include "PAM.inc"
#include "srs_srspar.inc"

      RECORD      /TABLE_HEAD_ST/                srspar_h
      RECORD      /SRS_SRSPAR_ST/               srspar(*)

        integer i
c       integer j,k,ii,jj,kk
	real dx,dz,dn,dsigx,dsigz
        real chisqrx,chisqrz
        real true(8),trial(8),deriv,sigma1,sigma2
        real step,oldstep,delta,olddelta,test
	real c,s
        integer try,maxtry,p

c------------------------------------------------------------------------------c
c       samples(1,n),samples(2,n) = x and z coordinate of sample               c
c       samplesx(1,n),samplesz(1,n) anode,t bucket hit in x or z projection    c
c       samplesx(2,n) earliest t bucket hit for anode hit n                    c
c       samplesz(2,n) earliest anode hit for t bucket hit n                    c
c       samplesx(3,n) latest t bucket hit for anode hit n                      c
c       samplesz(3,n) latest anode hit for t bucket hit n                      c
c------------------------------------------------------------------------------c
#include "svt_samples_common.inc"
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------c
c       INITIALIIZATION:                                                        c
c------------------------------------------------------------------------------c
        c=cosd(true(6))
        s=sind(true(6))
	maxtry=30
	step=1e-4
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------c
c	CALCULATE POSITION UNCERTAINTY IN X:				       c
c------------------------------------------------------------------------------c
	if (nmeasx.ge.4) then
	    try=0
	    olddelta=0
	    do i=1,8
		trial(i)=true(i)
	    enddo
101	    try=try+1
	    trial(2)=true(2)+step ! anode offset
	    deriv=2.0*chisqrx(trial,srspar_h,srspar)
	    deriv=deriv/(step**2.)
	    if (deriv.eq.0) then
	        step=step*10.0
	        goto 101
	    endif
	    delta=sqrt(2.0/deriv)
	    test=abs(delta-olddelta)/delta
	    do while (test.gt.0.05) 
		olddelta=delta
		oldstep=step
		step=oldstep/2.0
201		try=try+1
		trial(2)=true(2)+step
		deriv=2.0*chisqrx(trial,srspar_h,srspar)
		deriv=deriv/step**2.
		if (deriv.eq.0) then
		    step=step*10
		    goto 201
		endif
		delta=sqrt(2.0/deriv)
		test=abs(delta-olddelta)/delta
		if (try.gt.maxtry) then
		    print*,'Trys exceed maximum !!! (anode resolution)'
		    dx=delta
		    goto 301
		endif
	    enddo ! (test.gt.0.05)
	    dx=delta
301	    continue
	endif ! (nmeasx.ge.4)
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------c
c	CALCULATE UNCERTAINTY IN Z:					       c
c------------------------------------------------------------------------------c
	if (nmeasz.ge.4) then
	    try=0
	    olddelta=0
	    do i=1,8
		trial(i)=true(i)
	    enddo
102	    try=try+1
	    trial(3)=true(3)+step
	    deriv=2.0*chisqrz(trial,srspar_h,srspar)
	    deriv=deriv/(step**2.)
	    if (deriv.eq.0) then
	        step=step*10.0
	        goto 102
	    endif
	    delta=sqrt(2.0/deriv)
	    test=abs(delta-olddelta)/delta
	    do while (test.gt.0.05) 
		olddelta=delta
		oldstep=step
		step=oldstep/2.0
202		try=try+1
		trial(3)=true(3)+step
		if (p.eq.7.or.p.eq.8) then ! calculate the change in sig1,sig2 
                    sigma2=(s*trial(7))**2.0-(c*trial(8))**2.0
                    sigma2=sqrt(sigma2/(s**2.0-c**2.0))
                    sigma1=(s*trial(8))**2.0-(c*trial(7))**2.0
                    sigma1=sqrt(sigma1/(s**2.0-c**2.0))
                    trial(4)=sigma1
                    trial(5)=sigma2
                endif
		deriv=2.0*chisqrz(trial,srspar_h,srspar)
		deriv=deriv/step**2.
		if (deriv.eq.0) then
		    step=step*10
		    goto 202
		endif
		delta=sqrt(2.0/deriv)
		test=abs(delta-olddelta)/delta
		if (try.gt.maxtry) then
		    write(6,*)'Trys exceed maximum !!! (drift resolution)'
		    dz=delta
		    goto 302
		endif
	    enddo ! (test.gt.0.05)
	    dz=delta
302	    continue
	endif ! (nmeasz.ge.4)
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------c
c       DONE:                                                                  c
c------------------------------------------------------------------------------c
        return
        end
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c



c*****************************PROGRAM BOUNDRY**********************************c

c------------------------------------------------------------------------------c
c	HEADER:								       c
c------------------------------------------------------------------------------c
	function chisqrx(trial,srspar_h,srspar)
c	chisqr
c									       c
c	This program calculates the chisqr for the x projection of two         c
c	gaussian distributions.						       c
c	Begun by W.K.W. on Aug-19-1992					       c
c------------------------------------------------------------------------------c
c	The true array stores the parameters of the pulse:	               c
c	charge=param(1)							       c
c       anode offset=param(2)						       c
c	drift distance=param(3)						       c
c	sigma major axis=param(4)					       c
c	sigma minor axis=param(5)					       c
c	phi angle wrt drift=param(6)					       c
c	x proj width=param(7) (depends on 4, 5, and 6)       		       c
c	z proj width=param(8) (depends on 4, 5, and 6)			       c
c	The true array stores the parameters which represent the pulse.        c
c	The ENC per time bucket is a parameter in svt_parameters_inc	       c
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------c
c	VARIABLE DECLARATIONS AND DATA:					       c
c------------------------------------------------------------------------------c
c	implicit none

#include "PAM.inc"
#include "srs_srspar.inc"

      RECORD      /TABLE_HEAD_ST/                srspar_h
      RECORD      /SRS_SRSPAR_ST/               srspar(*)

c	integer i,j,k,ii,jj,kk
	real trial_signal,chisqrx,term
c       real err_sig,gauss,true(8)
	real svt_charge,trial(8),total_enc
	integer n,iix,iiz

c------------------------------------------------------------------------------c
c       samples(1,n),samples(2,n) = x and z coordinate of sample               c
c       samplesx(1,n),samplesz(1,n) anode,t bucket hit in x or z projection    c
c       samplesx(2,n) earliest t bucket hit for anode hit n                    c
c       samplesz(2,n) earliest anode hit for t bucket hit n                    c
c       samplesx(3,n) latest t bucket hit for anode hit n                      c
c       samplesz(3,n) latest anode hit for t bucket hit n                      c
c------------------------------------------------------------------------------c
#include "svt_samples_common.inc"
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------c
c	INITIALIIZATION:							       c
c------------------------------------------------------------------------------c
	chisqrx=0
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------c
c       CALCULATE CHI SQUARED:                                                 c
c------------------------------------------------------------------------------c
        do n=1,nmeasx
            iix=samplesx(1,n)
            trial_signal=0
            total_enc=0
            do iiz=samplesx(2,n),samplesx(3,n)
                trial_signal=trial_signal+svt_charge(iix,iiz,trial,
     1	         srspar_h,srspar)
                total_enc=total_enc+srspar(1).enc**2.
            enddo
            total_enc=sqrt(total_enc)
            term=((true_valuesx(n)-trial_signal)**2.)/
     1       (true_valuesx(n)+total_enc**2.)
            chisqrx=chisqrx+term
        enddo
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------c
c       DONE:                                                                  c
c------------------------------------------------------------------------------c
        return
        end
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c
                                                                                
c*****************************PROGRAM BOUNDRY**********************************c

c------------------------------------------------------------------------------c
c       HEADER:                                                                c
c------------------------------------------------------------------------------c
        function chisqrz(trial,srspar_h,srspar)
c       chisqr
c                                                                              c
c       This program calculates the chisqr for the z projection of two         c
c       gaussian distributions.                                                c
c       Begun by W.K.W. on Aug-19-1992                                         c
c------------------------------------------------------------------------------c
c	The true array stores the parameters of the pulse:	               c
c	charge=param(1)							       c
c       anode offset=param(2)						       c
c	drift distance=param(3)						       c
c	sigma major axis=param(4)					       c
c	sigma minor axis=param(5)					       c
c	phi angle wrt drift=param(6)					       c
c	x proj width=param(7) (depends on 4, 5, and 6)       		       c
c	z proj width=param(8) (depends on 4, 5, and 6)			       c
c	The true array stores the parameters which represent the pulse.        c
c       The ENC per time bucket is a parameter in svt_parameters_inc           c
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------c
c       VARIABLE DECLARATIONS AND DATA:                                        c
c------------------------------------------------------------------------------c
c        implicit none

#include "PAM.inc"
#include "srs_srspar.inc"

      RECORD      /TABLE_HEAD_ST/                srspar_h
      RECORD      /SRS_SRSPAR_ST/               srspar(*)

c        integer i,j,k,ii,jj,kk
        real trial_signal,chisqrz,term
c       real err_sig,gauss,true(8)
        real svt_charge,trial(8),total_enc
        integer n,iix,iiz

c------------------------------------------------------------------------------c
c       samples(1,n),samples(2,n) = x and z coordinate of sample               c
c       samplesx(1,n),samplesz(1,n) anode,t bucket hit in x or z projection    c
c       samplesx(2,n) earliest t bucket hit for anode hit n                    c
c       samplesz(2,n) earliest anode hit for t bucket hit n                    c
c       samplesx(3,n) latest t bucket hit for anode hit n                      c
c       samplesz(3,n) latest anode hit for t bucket hit n                      c
c------------------------------------------------------------------------------c
#include "svt_samples_common.inc"
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------c
c       INITIALIIZATION:                                                       c
c------------------------------------------------------------------------------c
        chisqrz=0
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------c
c       CALCULATE CHI SQUARED:                                                 c
c------------------------------------------------------------------------------c
        do n=1,nmeasz
            iiz=samplesz(1,n)
            trial_signal=0
            total_enc=0
            do iix=samplesz(2,n),samplesz(3,n)
                trial_signal=trial_signal+svt_charge(iix,iiz,trial,
     1	         srspar_h,srspar)
                total_enc=total_enc+srspar(1).enc**2.
            enddo
            total_enc=sqrt(total_enc)
            term=((true_valuesz(n)-trial_signal)**2.)/
     1       (true_valuesz(n)+total_enc**2.)
            chisqrz=chisqrz+term
        enddo
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------c
c       DONE:                                                                  c
c------------------------------------------------------------------------------c
        return
        end
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c*****************************PROGRAM BOUNDRY**********************************c

c------------------------------------------------------------------------------c
c       HEADER:                                                                c
c------------------------------------------------------------------------------c
        subroutine decode_id(id,layer,ladder,wafer)
c                                                                              c
c       This program finds the layer, ladder, and wafer # from the             c
c       detector id in the svt.
c       Begun by W.K.W. on Mar-16-1994                                         c
c------------------------------------------------------------------------------c
c       Detector ID Convention:  geom(ichip).id                            c
c       = 1000*ilayer+100*iwafer+iladder                                       c
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------c
c       VARIABLE DECLARATIONS AND DATA:                                        c
c------------------------------------------------------------------------------c
c        implicit none
c        integer i,j,k,ii,jj,kk
	integer id,layer,ladder,wafer
	real temp
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------c
c       CALCULATE THE QUANTITIES:                                              c
c------------------------------------------------------------------------------c
	temp=id
	temp=temp/1000
	temp=int(temp)
	layer=temp
	temp=id-1000*layer
	temp=temp/100
	temp=int(temp)
	wafer=temp
	temp=id-1000*layer-100*wafer
	ladder=temp
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c------------------------------------------------------------------------------c
c       DONE:                                                                  c
c------------------------------------------------------------------------------c
        return
        end
c------------------------------------------------------------------------------c
c------------------------------------------------------------------------------c

c*****************************PROGRAM BOUNDRY**********************************c

c     This is from numerical recipies
c     I wanted a random number generator that was not system dependent.
c     Ken wilson 8-4-93

      FUNCTION RAN2(IDUM)
	real rm,ran2
	integer ia,ic,iff,idum,m,j,iy
      PARAMETER (M=714025,IA=1366,IC=150889,RM=1.4005112E-6)
      integer IR(97)
      DATA IFF /0/
      IF(IDUM.LT.0.OR.IFF.EQ.0)THEN
        IFF=1
        IDUM=MOD(IC-IDUM,M)
        DO 11 J=1,97
          IDUM=MOD(IA*IDUM+IC,M)
          IR(J)=IDUM
11      CONTINUE
        IDUM=MOD(IA*IDUM+IC,M)
        IY=IDUM
      ENDIF
      J=1+(97*IY)/M
c      IF(J.GT.97.OR.J.LT.1)PAUSE
      IY=IR(J)
      RAN2=IY*RM
      IDUM=MOD(IA*IDUM+IC,M)
      IR(J)=IDUM
      RETURN
      END

c*****************************PROGRAM BOUNDRY**********************************c
c     This is from numerical recipies
c     It picks from a gaussian distribtuion centered at 0 with sigma=1
c     Ken wilson 8-4-93

      FUNCTION GASDEV(IDUM)
	integer idum,iset
	real v1,v2,r,fac,gset,gasdev
      REAL RAN2
      EXTERNAL RAN2
      DATA ISET/0/
      IF (ISET.EQ.0) THEN
1       V1=2.*RAN2(IDUM)-1.
        V2=2.*RAN2(IDUM)-1.
        R=V1**2+V2**2
        IF(R.GE.1.)GO TO 1
        FAC=SQRT(-2.*LOG(R)/R)
        GSET=V1*FAC
        GASDEV=V2*FAC
        ISET=1
      ELSE
        GASDEV=GSET
        ISET=0
      ENDIF
      RETURN
      END

c*****************************PROGRAM BOUNDRY**********************************c

      integer function spt_direct(svt_hit_h,svt_hit,spt_h
     +,                                                       spt)
c      implicit none
c
c     DESCRIPTION
c     Input Arguments:
c       svt_hit_h   : header for svt_hit table
c       svt_hit     : rows of svt_hit_st table
c
c     Output Arguments:
c       spt_h     : header for spt table
c       spt       : rows of spt_st table
c
c     Functional Description: 
c     Makes a straight copy of the GEANT space point informations in table SVT_HIT to
c     the "reconstructed" space point table SPT. 
c     No smearing in this copy - error on the space points is therefore recorded as zero.
c     The dedx information is copied as is - no smearing, no calibration or gain adjustments made.
c
c     Created 16-july-93  C. Pruneau, WSU
c     Revised 1-Nov-93    C.A.P. debugging
c
c     Error Conditions:
c
c     Argument/Global Declarations:

#include "PAM.inc"
#include "g2t_svt_hit.inc"
#include "scs_spt.inc"

      RECORD      /TABLE_HEAD_ST/                   spt_h
      RECORD         /SCS_SPT_ST/                  spt(*)
      RECORD      /TABLE_HEAD_ST/               svt_hit_h
      RECORD     /G2T_SVT_HIT_ST/              svt_hit(*)

c     Local Declarations
c     ==================
c      integer   ix
       integer   ihit

c     Format
c     ======
 1000 format(1x,'f-spt_direct: no rows in svt_hit.')
 1001 format(1x,'f-spt_direct: too many rows in svt_hit to '
     +//'transfer to table spt.')

c     Executable Code
c     ===============

c     set return code to normal success

      spt_direct = STAFCV_OK

c     check if 
c     a)there is any rows in svt_hit
c     b)the number of data to be transfered is too big given the size of the target

      if (svt_hit_h.nok.le.0) then
         spt_direct = STAFCV_BAD
         write(6,1000)
      else if (spt_h.maxlen.lt.svt_hit_h.nok) then
         spt_direct = STAFCV_BAD
         write(6,1001) 
         return
      end if

c     loop on all rows of svt_hit and transfer the global coordinates, 
c     the dedx info
c     to spt, will set errors to zero

      do ihit = 1, svt_hit_h.nok

         spt(ihit).x(1)   = svt_hit(ihit).x(1) ! position of space point
         spt(ihit).x(2)   = svt_hit(ihit).x(2) ! position of space point
         spt(ihit).x(3)   = svt_hit(ihit).x(3) ! position of space point
         spt(ihit).de(1)      = svt_hit(ihit).de  ! energy loss
         spt(ihit).de(2)      = 0.                  ! error on energy loss
         spt(ihit).cov(1)     = 0.                  ! error on x(1)
         spt(ihit).cov(2)     = 0.                  ! error on x(2)
         spt(ihit).cov(3)     = 0.                  ! error on x(3)
         spt(ihit).flag       = 0                   ! quality of space point - perfect
         spt(ihit).id         = ihit                ! sequential index
         spt(ihit).id_globtrk = 0                   ! initialized to zero.
         spt(ihit).id_track   = 0                   ! link to reconstructed track - 
                                                        ! to   be left empty here
         spt(ihit).id_mctrack = svt_hit(ihit).track_p! link to monte carlo track
         spt(ihit).id_cluster = svt_hit(ihit).id  ! link to svt_hit
         spt(ihit).id_wafer   = svt_hit(ihit).volume_id
c         svt_hit(ihit).id_spt   = ihit
      end do

c     set the number of OK lines in spt

      spt_h.nok         = svt_hit_h.nok 
      return
      
      end




c*****************************PROGRAM BOUNDRY**********************************c

      integer function spt_direct_gs(
     &     direct_h, direct,
     &     geom_h, geom,
     &     svt_hit_h, svt_hit,
     &     spt_h, spt,
     &     srspar_h, srspar)
c      implicit none
c
c     DESCRIPTION
c     Input Arguments:
c       direct_h  : header for svt_direct table : smearing paramters
c       direct    : rows of svt_direct   table
c       geom_h    : header for geom         : geometry of the wafers
c       geom      : rows of geom
c       svt_hit_h   : header for svt_hit table
c       svt_hit     : rows of svt_hit_st table
c       srspar_h      : header for srspar table
c       srspar        : rows of srspar_st table
c
c     Output Arguments:
c       spt_h     : header for spt table
c       spt       : rows of spt_st table
c
c     Functional Description: 
c     Makes a smeared copy of the GEANT space point informations in table SVT_HIT to
c     the "reconstructed" space point table SPT. 
c     Gaussian smearing is performed in this copy - error on the space points is recorded as the
c     standard deviation of the smearing.
c     The dedx information is copied as is - no smearing, no calibration or gain adjustments made.
c     The smearing is performed in the wafer plane; i.e. a mmapping to the reference frame of the
c     wafer is performed (by svtgtol) smearing is done in both the transverse and drift directions
c     and the point is mapped back to the STAR global coordinate system (svtltog).
c
c     Created 1-Nov-93  C. Pruneau, WSU
c
c     Error Conditions:
c
c     Argument/Global Declarations:


#include "PAM.inc"
#include "g2t_svt_hit.inc"
#include "scs_spt.inc"
#include "svg_geom.inc"
#include "srs_srspar.inc"
#include "srs_direct.inc"
      RECORD      /TABLE_HEAD_ST/               svt_hit_h
      RECORD     /G2T_SVT_HIT_ST/              svt_hit(*)
      RECORD      /TABLE_HEAD_ST/                   spt_h
      RECORD         /SCS_SPT_ST/                  spt(*)
      RECORD      /TABLE_HEAD_ST/                  geom_h
      RECORD        /SVG_GEOM_ST/                 geom(*)
      RECORD      /TABLE_HEAD_ST/                srspar_h
      RECORD      /SRS_SRSPAR_ST/               srspar(*)
      RECORD      /TABLE_HEAD_ST/                direct_h
      RECORD      /SRS_DIRECT_ST/               direct(*)

c     Local Declarations
c     ==================
      integer    ihit, iwafer, wafer, ispt
      integer   get_geom_row
      real      xp(3)       ! local coordinate
      real      rg32, dummy
      real      dx(3), dxp(3), zero(3)

c     Format
c     ======
 1000 format(1x,'f-spt_direct_gs: no rows in svt_hit.')
 1001 format(1x,'f-spt_direct_gs: too many rows in svt_hit',
     &     ' to transfer to table spt.')
 1002 format(1x,
     +'f-spt_direct_gs: no rows in direct - will use defaults.')
 1003 format(1x,'f-spt_direct_gs: no rows in geom.')
 1004 format(1x,
     +'f-spt_direct_gs: unknown wafer id for space point:',i5)

c     Executable Code
c     ===============

c     initialize seed (wkw)
      if (srspar(1).init_ran.ne.0) then
           call rg32in(srspar(1).init_ran)
      endif

c     set return code to normal success

      spt_direct_gs = STAFCV_OK

c     check if 
c     a)there is any rows in svt_hit
c     b)the number of data to be transfered is too big given the size of the target

      if (svt_hit_h.nok.le.0) then
         spt_direct_gs = STAFCV_BAD
         write(6,1000)
      else if (geom_h.nok.le.0) then
         spt_direct_gs = STAFCV_BAD
         write(6,1003)
      else if (spt_h.maxlen.lt.svt_hit_h.nok) then
         spt_direct_gs = STAFCV_BAD
         write(6,1001) 
         return
      else if (direct_h.nok.le.0) then
         write(6,1002)
         direct(1).sd = 0.0020     ! 20 microns
         direct(1).st = 0.0020     ! 20 microns
         direct_h.nok = 1
      end if

c     loop on all rows of svt_hit and transfer the global coordinates, the dedx info
c     to spt, will set errors equal to smearing parameters.
c     map to wafer local frame coordinate, smear and map back to global frame

      ispt = 0
      do ihit = 1, svt_hit_h.nok
         wafer  = svt_hit(ihit).volume_id
         iwafer = get_geom_row(geom_h,geom,wafer)
         if (iwafer.gt.0) then
            ispt = ispt + 1
            spt(ispt).x(1)   = svt_hit(ihit).x(1) ! position of space point
            spt(ispt).x(2)   = svt_hit(ihit).x(2)
            spt(ispt).x(3)   = svt_hit(ihit).x(3)
            call svtgtol(spt(ispt).x,xp,
     &           geom(iwafer).x,
     &           geom(iwafer).d,
     &           geom(iwafer).t,
     &           geom(iwafer).n)
            xp(1) = xp(1)+ direct(1).sd *rg32(dummy)
            xp(2) = xp(2)+ direct(1).st *rg32(dummy)
            call svtltog(xp,spt(ispt).x,
     &           geom(iwafer).x,
     &           geom(iwafer).d,
     &           geom(iwafer).t,
     &           geom(iwafer).n)
            dxp(1) = direct(1).sd
            dxp(2) = direct(1).st
            dxp(3) = 0.
            zero(1) = 0.
            zero(2) = 0.
            zero(3) = 0.
            call svtltog(dxp,dx,
     &           zero,
     &           geom(iwafer).d,
     &           geom(iwafer).t,
     &           geom(iwafer).n)
            spt(ispt).de(1)      = svt_hit(ihit).de ! energy loss
            spt(ispt).de(2)      = 0. ! error on energy loss)
            spt(ispt).cov(1)     = abs(dx(1))
            spt(ispt).cov(2)     = abs(dx(2))
            spt(ispt).cov(3)     = abs(dx(3))
            spt(ispt).flag       = 0 ! quality of space point - perfect
            spt(ispt).id         = ihit ! sequential index
            spt(ispt).id_globtrk = 0 ! link to global track 
            spt(ispt).id_track   = 0 ! link to reconstructed track 
            spt(ispt).id_mctrack = svt_hit(ihit).track_p! link to monte carlo track
            spt(ispt).id_cluster = svt_hit(ihit).id ! link to mchits_svt
            spt(ispt).id_wafer   = svt_hit(ihit).volume_id
c            svt_hit(ihit).id_spt   = ihit
         else
            write(6,1004) ihit 
         end if
         
      end do
      
c     set the number of OK lines in spt 

      spt_h.nok       = ispt

      return
      
      end

c     ============================================================
      subroutine svtgtol(x,xp,wx,wd,wt,wn)
c      implicit none

c     DESCRIPTION:   global to local mapping for svt points
c
c     Input Arguments:
c       x     : global coordinate
c       wx    : wafer origin
c       wd    : wafer drift direction
c       wt    : wafer transverse direction
c       wn    : wafer normal
c
c     Output Arguments:
c       xp    : local coordinate
c
c     Functional Description: 
c      Make a global to local mapping by using the origin of the wafer coordinate and the
c     3 vectors defining the normal to the plane, the drift direction, and the transverse direction.
c     xp(1)  : component in the drift direction
c     xp(2)  : component in the transverse direction
c     xp(3)  : component in the direction normal to the wafer; should be small ...
c
c     Created 1-Nov-93  C. Pruneau, WSU
c
c     Error Conditions: none
c
c     Argument declarations:
c     ======================
      real  x(3), xp(3), wx(3), wd(3), wt(3), wn(3)

c     Local Declarations
c     ==================
      real  xl(3)

c     Executable Code
c     ===============
      xl(1) = x(1) - wx(1)
      xl(2) = x(2) - wx(2)
      xl(3) = x(3) - wx(3)

      xp(1) = xl(1)*wd(1) + xl(2)*wd(2) + xl(3)*wd(3) 
      xp(2) = xl(1)*wt(1) + xl(2)*wt(2) + xl(3)*wt(3) 
      xp(3) = xl(1)*wn(1) + xl(2)*wn(2) + xl(3)*wn(3) 

      return
      end

c     ============================================================
      subroutine svtltog(xp,x,wx,wd,wt,wn)
c      implicit none

c     DESCRIPTION:  local to global mapping for svt points
c
c     Input Arguments:
c       xp    : local coordinate
c       wx    : wafer origin
c       wd    : wafer drift direction
c       wt    : wafer transverse direction
c       wn    : wafer normal
c
c     Output Arguments:
c       x     : global coordinate
c
c     Functional Description: 
c      Make a local to global mapping by using the origin of the wafer coordinate and the
c     3 vectors defining the normal to the plane, the drift direction, and the transverse direction.
c     xp(1)  : component in the drift direction
c     xp(2)  : component in the transverse direction
c     xp(3)  : component in the direction normal to the wafer; should be small ...
c
c     Created 1-Nov-93  C. Pruneau, WSU
c
c     Error Conditions: none
c
c     Argument declarations:
c     ======================
      real  x(3), xp(3), wx(3), wd(3), wt(3), wn(3)

c     Local Declarations
c     ==================
      real  xl(3)

c     Executable Code
c     ===============
      xl(1) = xp(1)
      xl(2) = xp(2)
      xl(3) = xp(3)

      x(1) = wx(1) + xl(1)*wd(1) + xl(2)*wt(1) + xl(3)*wn(1) 
      x(2) = wx(2) + xl(1)*wd(2) + xl(2)*wt(2) + xl(3)*wn(2) 
      x(3) = wx(3) + xl(1)*wd(3) + xl(2)*wt(3) + xl(3)*wn(3) 

      return
      end
c====================================================================================
      integer function get_geom_row(geom_h,geom,wafer)
c      implicit none
c
c     DESCRIPTION
c     Get a row number for wafer number "wafer" in table geom
c
c     Return:
c     row number   : row number
c     -1           : error, id not found
c
c     Functional Description: 
c     Loops on all rows of geom table to find the wafer id "wafer"
c
c     Created 29-Nov-93  C. Pruneau, WSU
c
c     Error Conditions: returns -1 : id not found
c
c     Argument/Global Declarations:
c     =============================

#include "PAM.inc"
#include "svg_geom.inc"

      RECORD      /TABLE_HEAD_ST/                  geom_h
      RECORD        /SVG_GEOM_ST/                 geom(*)

      integer wafer

c     Local Declarations
c     ==================
      integer   iwafer

c     Format
c     ======
 1000 format(1x,'f-svt_wafer_id: wafer id not found in table geom:'
     +,i6)

c     Executable Code
c     ===============
      do iwafer = 1, geom_h.nok
         if (wafer .eq. geom(iwafer).id) then
            get_geom_row = iwafer
            return
         end if
      end do

      write(6,1000) wafer
      get_geom_row = -1
      
      return
      end


c*****************************PROGRAM BOUNDRY**********************************c


      subroutine fsub2(m,u2,f2,x)
c     for z integral
c      implicit none
      integer i
c      integer j,k,ii,jj,kk
      integer m
      real*8 u2(*),f2(*),x(2)
      real*8 pp(8),xlow,xup
      real*8 dgmlt1
      external fsub1
      common /for_2d_int/ pp,xlow,xup

      do i=1,m
         x(2)=u2(i)
         f2(i)=dgmlt1(fsub1,xlow,xup,1,6,x)
      enddo
      
      return
      end


c*****************************PROGRAM BOUNDRY**********************************c


      subroutine fsub1(m,u1,f1,x)
c     for x integral
c      implicit none
      integer i
c     integer j,k,ii,jj,kk
      integer m
      real*8 u1(*),f1(*),x(2)
      real*8 pp(8),xlow,xup
      common /for_2d_int/ pp,xlow,xup
      real*8 q1,q2,mean_q1,mean_q2,s,c,angle
      real*8 x0,z0,sig1,sig2,norm
      real*8 pi
      data pi /3.1415927/

c------------------------------------------------------------------------------c
c                anodes                 rotated system                         c
c                             drift                                            c
c                  z            .             q1                               c
c                  |           /|\            /                                c
c                  |  *         |            /                                 c
c                  |**          |           /                                  c
c                  |**          |          /                                   c
c                 ***---------x            \                                   c
c                **                         \                                  c
c		**                           \                                 c
c               *                             q2                               c
c	charge=param(1)							       c
c       anode offset=param(2)						       c
c	drift distance=param(3)						       c
c	sigma major axis=param(4) (q1)					       c
c	sigma minor axis=param(5  (q2) 				               c
c	phi angle of major axis wrt drift axis =param(6)		       c
c	x proj width=param(7) (depends on 4, 5, and 6)       		       c
c	z proj width=param(8) (depends on 4, 5, and 6)			       c
c       x direction is transverse to drift                                     c
c       z direction is parallel to drift                                       c
c       Thus, whien phi=0, sigma x = sigma 2 and sigma z = sigma 1             c
c------------------------------------------------------------------------------c

      angle=pp(6)
      c=cosd(angle)
      s=sind(angle)
      x0=pp(2)
      z0=pp(3)
      mean_q1=s*x0+c*z0
      mean_q2=c*x0-s*z0
      sig1=pp(4)
      sig2=pp(5)
      norm=pp(1)/(2.0*sig1*sig2*pi)

      do i=1,m      
         x(1)=u1(i)
         q1=s*x(1)+c*x(2)
         q2=c*x(1)-s*x(2)
         f1(i)=norm
         f1(i)=f1(i)*exp((-(q1-mean_q1)**2.0)/(2.0*sig1**2.0))
         f1(i)=f1(i)*exp((-(q2-mean_q2)**2.0)/(2.0*sig2**2.0))
      enddo
      
      return
      
      end









