* $Id: tpt_find_root.F,v 1.8 1998/09/01 13:32:35 sakrejda Exp $
* $Log: tpt_find_root.F,v $
* Revision 1.8  1998/09/01 13:32:35  sakrejda
* restored sector numbering 1-24 in tphit.row
*
* Revision 1.7  1998/07/12 21:54:09  sakrejda
* protecion against 0 denominator
*
* Revision 1.6  1998/05/26 04:15:12  sakrejda
* message type set to E to limit printing
*
* Revision 1.5  1998/04/23 17:13:01  sakrejda
* more comments added and speed improved by removing sqrt and sqaring tolerances
*
* Revision 1.4  1998/04/22 15:57:57  sakrejda
* write to unit 6 replaced by message calls
*
* Revision 1.3  1998/03/23 18:25:56  sakrejda
* tpg_pad-plane removed from the tpt module, it was not used there anyway
*
* Revision 1.2  1998/01/27 00:35:45  fisyak
* Split sources
*
      INTEGER FUNCTION TPT_FIND_ROOT(k,hit_h,hit,tpar_h,tpar,
     >                               nroot,root,next_hit,
     >                               slice_loc,slice_pnt,ipass)
C>---------------------------------------------------------------------
C
C TPT_FIND_ROOT - formation of the track seeds
C
C DESCRIPTION:
C INPUT ARGUMENTS:
C  tpcount  - count of hits for every row/sector
C OUTPUT ARGUMENTS:
C<---------------------------------------------------------------------
C
C Modified: 13-NOV-1997 by R.Bossingham
C           Eliminate unused variables.
C
      IMPLICIT NONE

#include "PAM.inc"

#include "tcl_tphit.inc"
#include "tpt_pars.inc"
#include "tpt_root.inc"
#include "tpt_mxdim.inc"
#include "tpt_mxhit.inc"

C_____________________________________________________________________

      RECORD /table_head_st/    tpar_h
      RECORD /tpt_pars_st/      tpar(*)
      RECORD /table_head_st/    hit_h
      RECORD /tcl_tphit_st/     hit(*)
C_____________________________________________________________________
C     Local variables
      RECORD /track_root/     root(mxroot)
      INTEGER next_hit(tpt_mxhit)
      INTEGER slice_loc(mxsector,mxrow,mxslic)
      INTEGER slice_pnt(2,mxsector*mxrow*mxslic)
      INTEGER k                            !pointer to the hit we start with
      INTEGER nroot                        !number of track roots starting from this point
      INTEGER isect,ir3,ir2,ir1
      INTEGER isect_trunc                  !sector number truncated to 1-12
      INTEGER gap_1                        !gap counter between ir2 and ir1
      INTEGER gap_2                        !gap counter between ir3 and ir2
      INTEGER nh1,nh2
      INTEGER tol23,i,j,iret,tol12,itest,ilc,jlc,ipass
      INTEGER tgc_row_par                  !function that returns row parametrisation
      INTEGER tpt_ptcrs                    !function calculating a crossing point of 2 lines
      INTEGER islic,is_pnt,js_pnt
      INTEGER tpt_find_slic, irets
      INTEGER tgc_sec24_to_sec12
      REAL    zp3
      REAL    zorigl,r3,r2,delr23,zp2,delz2p,dely23
      REAL    delx23,delz23,an,bn,sl23,zp1,xp1,yp1
      REAL    as,bs,ddr,fact,delrow,dummy, det_local
      LOGICAL h2good

c     message related variables
      character*132 m132
      integer       idR1
      data          idR1 /0/
      save          idR1
C_____________________________________________________________________

      nroot = 0
      zorigl = 0

C     get information about the seed
      isect = int(hit(k).row/100)
      iret  = tgc_sec24_to_sec12(isect,isect_trunc)
      ir3   = mod(hit(k).row,100)
      zp3   = hit(k).z
      r3    = sqrt(hit(k).x**2+hit(k).y**2)
       
C     set maximum allowable gap 
      gap_2 = tpar(ipass).g2
      ir2 = ir3-1
      nh2 = 0
    

C     outer do-loop for second hit on a root
      do while ( gap_2.ge.0 .and. nroot.eq.0 )

C     find in what slice the hits for the next row are going to be
      delrow = 2.5
      if(ir2.lt.14) delrow = 6.0
      zp2 = zp3*(1.0-delrow/r3)
      if(abs(zp2).gt.200.0) zp2=sign(200.0,zp2)
      irets=tpt_find_slic(zp2,tpar(ipass).nzslic,islic)
      is_pnt = slice_loc(isect_trunc,ir2,islic) 
      nh2 = slice_pnt(2,is_pnt)
      gap_2 = gap_2 - 1
c
      if( nh2 .eq. 0 ) then
C       total of tpar(ipass).g2 rows 
C       allowed between the last two hits
        ir2 =ir2 - 1
        if( ir2 .le. tpar(ipass).irow_seg_end ) go to 999
      else if( gap_2 .lt. -1 ) then
        go to 999
      else if( nh2 .ne. 0 ) then
C       UPSTREAM point: set maximum allowable gap
        gap_1 = tpar(ipass).g1
        ir1 = ir2 - 1
        nh1 = 0

C       inner do-loop for first hit on a root
        do while ( gap_1.ge.0 .and. nroot.eq.0 )
	  if( ir1 .le. tpar(ipass).irow_seg_end ) go to 999
          gap_1 = gap_1 - 1
          if ( gap_1 .lt. -1 ) go to 999
C         Start looking for a link - down to business
          i = slice_pnt(1,is_pnt)
C         loop over all hits in slice-next i'th hit is set at enddo
          do ilc=1,nh2
	    if( hit(i).track .le. 0 ) then
	      r2 = sqrt(hit(i).x**2+hit(i).y**2)
	      delr23 = r3 - r2
	      if( delr23 .gt. 0.0000001 ) then
C               make tolerance proportional to the gap size
                fact=1.0
                if( ir2 .lt. 14 ) fact=2.5
                tol23 = real(ir3 - ir2)*fact
C               calculate prediction
	        zp2 = (hit(k).z - zorigl) * r2 / r3 + zorigl
c               squared distance between the prediction and the track (r-z projection)
	        delz2p = (hit(i).z-zp2)**2/(1.0+((hit(k).z-zorigl)/r3)**2)
                if(delz2p.lt.(tpar(ipass).delzlm*tol23)**2)
     >          then
		  dely23 = hit(k).y-hit(i).y
                  delx23 = hit(k).x-hit(i).x
                  if( dely23**2+delx23**2 .lt. 
     >              (tpar(ipass).delrlm*tol23)**2) then
                    delz23 = hit(k).z - hit(i).z
c                   we have a candidate for the second point
                    h2good = .true.
C                   calculate prediction for the row ir1
c                   get the row parametrisation
                    iret = tgc_row_par(isect,real(ir1),as,bs,dummy,
     >              dummy)
c                   calculate the track parametrisation as an*x+bn*y=1
                    det_local=(hit(i).x*hit(k).y-hit(k).x*hit(i).y)
                    if(abs(det_local).gt.0.0000001) then
                    an=(hit(k).y-hit(i).y)/det_local
                    bn=-(hit(k).x-hit(i).x)/det_local
c                   calculate the prediction in x-y plane
                    iret = tpt_ptcrs(as,bs,an,bn,xp1,yp1)
c                   calculate the prediction in the r-z plane
                    sl23 = delz23/delr23
                    zp1 = hit(i).z - sl23*(r2-sqrt(xp1**2+yp1**2))
                    if(abs(zp1).gt.209.0) zp1=sign(209.0,zp1)
c                   find the slice number for the prediction and look for a hit there
                    irets=tpt_find_slic(zp1,tpar(ipass).nzslic,islic)
                    js_pnt = slice_loc(isect_trunc,ir1,islic) 
                    j = slice_pnt(1,js_pnt)
C                   next j'th hit is set at enddo
                    do jlc=1,slice_pnt(2,js_pnt)
		      if( hit(j).track.le.0 ) then
		         tol12 = real(ir2 - ir1)*fact
		         ddr = tpar(ipass).drlm1*tol12*hit(j).prf
                         if( (hit(j).x-xp1)**2+(hit(j).y-yp1)**2
     +                     .lt.ddr**2) then
c                          candidate passed cuts in x-y
			   if((hit(j).z-zp1)**2/(1.+sl23**2) 
     >                     .lt.(tpar(ipass).dzlm1*tol12*hit(j).zrf)**2) then
c                              we have a good root
                               nroot=nroot+1
		      	       root(nroot).hit(1) = k
		  	       root(nroot).hit(2) = i
			       root(nroot).hit(3) = j
                               if(nroot.eq.mxroot) then
                                   write(m132,*) 'TPTROOT1-E1 root limit!',nroot
                                   call Message(m132, 1, idR1)
                                   go to 999
                               endif
                           endif
                         endif
                      endif
                      j = next_hit(j)
                    enddo          ! loop over hits in ir1
                    endif  ! endif on bad denominator
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
