* $Id: tpt_find_root.F,v 1.2 1998/01/27 00:35:45 fisyak Exp $
* $Log: tpt_find_root.F,v $
* Revision 1.2  1998/01/27 00:35:45  fisyak
* Split sources
*
      INTEGER FUNCTION TPT_FIND_ROOT(k,hit_h,hit,tpar_h,tpar,pad_pl_h,
     >                               pad_pl,nroot,root,next_hit,
     >                               slice_loc,slice_pnt,ipass)
C>---------------------------------------------------------------------
C
C TPT_FIND_ROOT - formation of the track seeds
C
C DESCRIPTION:
C INPUT ARGUMENTS:
C  pad_pl_h  - header for the pad_plane (pad_plane geometry) table
C  pad_pl   - pad_plane geometry table
C  tpcount  - count of hits for every row/sector
C OUTPUT ARGUMENTS:
C<---------------------------------------------------------------------
C
C Modified: 13-NOV-1997 by R.Bossingham
C           Eliminate unused variables.
C
      IMPLICIT NONE
C     pad_pl_h  - header for the pad_plane (pad_plane geometry) table
C     pad_pl   - pad_plane geometry table

#include "PAM.inc"

#include "tcl_tphit.inc"
#include "tpg_pad_plane.inc"
#include "tpt_pars.inc"
#include "tpt_root.inc"
#include "tpt_const.inc"
#include "tpt_mxdim.inc"
#include "tpt_mxhit.inc"

C_____________________________________________________________________

      RECORD /table_head_st/    tpar_h
      RECORD /tpt_pars_st/    tpar(*)
      RECORD /table_head_st/    hit_h
      RECORD /tcl_tphit_st/     hit(*)
      RECORD /table_head_st/    pad_pl_h
      RECORD /tpg_pad_plane_st/ pad_pl
C_____________________________________________________________________
C     Local variables
      RECORD /track_root/     root(mxroot)
      INTEGER next_hit(tpt_mxhit),slice_loc(mxsector,mxrow,mxslic)
      INTEGER slice_pnt(2,mxsector*mxrow*mxslic)
      INTEGER k,nroot,isect,ir3,ir2,ir1,kg1,kg2,nh1,nh2
      INTEGER tol23,i,j,iret,tol12,itest,ilc,jlc,ipass
      INTEGER tgc_row_par,tpt_ptcrs
      INTEGER islic,is_pnt,js_pnt
      INTEGER tpt_find_slic, irets
      REAL    zp3
      REAL    zorigl,r3,r2,delr23,zp2,delz2p,dely23
      REAL    delx23,delz23,an,bn,sl23,pred,delrp,zp1,xp1,yp1
      REAL    as,bs,ddr,fact,delrow,dummy
      LOGICAL h2good
C_____________________________________________________________________
C     check debug flag
      if( tpar(ipass).debug(3) .eq. 1 ) then
         write(6,*) 'starting root from hit',k
         read (5,*) itest
         if(itest.eq.0) tpar(ipass).debug(3)=0
      endif

      nroot = 0
      zorigl = 0

C     get information about the seed
      isect = hit(k).row / 100
      ir3 = hit(k).row - isect*100
      zp3 = hit(k).z
      r3 = sqrt(hit(k).x**2+hit(k).y**2)
       
C     set maximum allowable gap 
      kg2 = tpar(ipass).g2
      ir2 = ir3-1
      nh2 = 0
    

C     outer do-loop for second hit on a root
      do while ( kg2.ge.0 .and. nroot.eq.0 )

C     find in what slice the hits for the next row are going to be
      delrow = 2.5
      if(ir2.lt.14) delrow = 6.0
      zp2 = sign(1.0,zp3)*(r3-delrow)*abs(zp3)/r3
      irets=tpt_find_slic(zp2,tpar(ipass).nzslic,islic)
      is_pnt = slice_loc(isect,ir2,islic) 
      nh2 = slice_pnt(2,is_pnt)
      kg2 = kg2 - 1
      if( nh2 .eq. 0 ) then
C       total of tpar(ipass).g2 rows 
C       allowed between the last two hits
        ir2 =ir2 - 1
        if( ir2 .le. tpar(ipass).irow_seg_end ) go to 999
      else if( kg2 .lt. -1 ) then
        go to 999
      else if( nh2 .ne. 0 ) then
C       UPSTREAM point: set maximum allowable gap
        kg1 = tpar(ipass).g1
        ir1 = ir2 - 1
        nh1 = 0

C       inner do-loop for first hit on a root
        do while ( kg1.ge.0 .and. nroot.eq.0 )

          if( tpar(ipass).debug(3).eq.1 ) write(6,*) 
     >   'second loop','nh1',nh1,'kg1',kg1,'nroot',nroot
	  if( ir1 .le. tpar(ipass).irow_seg_end ) go to 999
          kg1 = kg1 - 1
          if ( kg1 .lt. -1 ) go to 999
C         Start looking for a link - down to business

          i = slice_pnt(1,is_pnt)
C         loop over all hits in slice-next i'th hit is set at enddo
          do ilc=1,nh2
	    if( hit(i).track .le. 0 ) then
	      r2 = sqrt(hit(i).x**2+hit(i).y**2)
	      delr23 = r3 - r2
	      if( delr23 .gt. 0.0000001 ) then
C               make tolerance proportional to the gap size
                tol23 = ir3 - ir2
                if( ir2 .lt. 14 ) tol23 = tol23 * 2
C               calculate prediction
	        zp2 = (hit(k).z - zorigl) * r2 / r3 + zorigl
                fact = 1.0
                if( ir2 .lt. 14 ) fact = 2.5
	        delz2p = (hit(i).z-zp2)/sqrt(1.0+
     >                   ((hit(k).z-zorigl)/r3)**2)
                if( abs(delz2p).lt.fact*tpar(ipass).delzlm*tol23 )
     >          then
		  dely23 = hit(k).y-hit(i).y
                  delx23 = hit(k).x-hit(i).x
                  if( dely23**2+delx23**2 .lt. 
     >              (fact*tpar(ipass).delrlm*tol23)**2) then
                    delz23 = hit(k).z - hit(i).z
                    h2good = .true.
C                   calculate prediction for the row ir1
                    iret = tgc_row_par(isect,real(ir1),as,bs,dummy,
     >              dummy)
                    an=(hit(k).y-hit(i).y)/
     >                 (hit(i).x*hit(k).y-hit(k).x*hit(i).y)
                    bn=-(hit(k).x-hit(i).x)/
     >                 (hit(i).x*hit(k).y-hit(k).x*hit(i).y)
                    iret = tpt_ptcrs(as,bs,an,bn,xp1,yp1)
                    sl23 = delz23/delr23
                    pred = sqrt(xp1**2+yp1**2)
                    delrp = r2 - pred
                    zp1 = hit(i).z - sl23*delrp
                    if(abs(zp1).lt.210.0) then
                    irets=tpt_find_slic(zp1,tpar(ipass).nzslic,islic)
                    js_pnt = slice_loc(isect,ir1,islic) 
                    j = slice_pnt(1,js_pnt)
C                   next j'th hit is set at enddo
                    do jlc=1,slice_pnt(2,js_pnt)
		      if( hit(j).track.le.0 ) then
		         tol12 = ir2 - ir1
		         ddr = fact*tpar(ipass).drlm1*tol12*hit(j).prf
                         if( (hit(j).x-xp1)**2+(hit(j).y-yp1)**2
     +                     .lt.ddr**2) then
			   if( abs(hit(j).z-zp1)/sqrt
     +                     (1.+sl23**2) .lt.tpar(ipass).dzlm1*
     +                     tol12*hit(j).zrf*fact) then
                               nroot=nroot+1
		      	       root(nroot).hit(1) = k
		  	       root(nroot).hit(2) = i
			       root(nroot).hit(3) = j
                               if(nroot.eq.mxroot) then
                                   if (tpar(ipass).debug(2).ne.0) then
                                   write(6,*) 'root limit!',nroot
                                   endif
                                   go to 999
                               endif
                           endif
                         endif
                      endif
                      j = next_hit(j)
                    enddo          ! loop over hits in ir1
                    endif
                  endif
                endif
              endif
            endif
            i = next_hit(i)
          enddo               ! loop over hits in ir2
	if(nroot.ne.0) go to 999
        end do                                    ! loop over ir1
      endif
      end do                                      ! loop over ir2
999   continue
      tpt_find_root = 0
      end
