C     >--------------------------------------------------------------------
C     
C     EGR_REFIT_TRACK_KAL - track fitting routine
      
C     <--------------------------------------------------------------------
      INTEGER*4 FUNCTION EGR_REFIT_TRACK_KAL(param,igtrk,itpct,
     +     isvtt,ipriv,gtrk,
     +     tphit_h,tphit,privert_h,privert,svt_spt_h,svt_spt,
     +     svt_track_h,svt_track,svt_group_h,svt_group,
     +     egrpar_h,egrpar,evt_match_h,evt_match,covar)
      
C     >-----------------------------------------------------------------------
C     Input arguments
C     igtrk    - current fitting array id
C     itpct    - use this TPC track id number in the refit (=0 if don't)
C     isvtt    - use this SVT track index (not id no) in the refit (=0 if don't)
C     ipriv    - flag for whether or not to include the vertex in the fit
C     gtrk     - the refitting array
C     gtrk contains TPC stats if itpct>0, otherwise has SVT stats
C     tphit_h     - header for the tphit (tpc hits) table
C     tphit      - rows of the tphit table
C     privert_h  - header for the dst_vertex table,
C                  (used to get the primary vertex).
C     privert    - rows of the dst_vertex table
C     svt_spt_h  - header for the svt_spt (SVT points) table
C     svt_spt   - rows of the svt_spt table
C     svt_track_h - header for the svt_track (SVT candidates) table
C     svt_track  - rows of the svt_track table
C     svt_group_h - header for the svt_group (trk-spt) table
C     svt_group - rows of the svt_group table
C     egrpar_h    - header for the egpar (EGR parameters) table
C     egrpar     - rows of the egrpar table
C     evt_match_h - header for the evt_match (SVM matcher) table
C     evt_match  - rows of the evt_match table
C     
C     Output arguments :
C     Values in track
C     iok = user status code
C     
C     Functional description:
C     Fits a helix to the TPC track ipt, including primary vertex point.
C
C     Modifications:
C     1.  Removed the evr_privert table and replaced with the dst_vertex
C         table; the former having become obsolete -- L. Ray, 8/19/98
C
C<-----------------------------------------------------------------------
 
      IMPLICIT NONE
#include "PAM.inc"
#include "scs_spt.inc"
#include "tpt_track.inc"
#include "tcl_tphit.inc"
#include "dst_vertex.inc"
#include "egr_egrpar.inc"
#include "egr_track_pointers.inc"
#include "stk_track.inc"
#include "sgr_groups.inc"
#include "svm_evt_match.inc"

C______________________________________________________________________
 
      RECORD /table_head_st/     svt_spt_h
      RECORD /scs_spt_st/       svt_spt(*)
      RECORD /table_head_st/     tphit_h
      RECORD /tcl_tphit_st/         tphit(*)
      RECORD /table_head_st/     svt_track_h
      RECORD /stk_track_st/    svt_track(*)
      RECORD /table_head_st/   svt_group_h
      RECORD /sgr_groups_st/   svt_group(*)
      RECORD /table_head_st/     privert_h
      RECORD /dst_vertex_st/       privert(*)
      RECORD /table_head_st/     egrpar_h
      RECORD /egr_egrpar_st/        egrpar(*)
      RECORD /table_head_st/     evt_match_h
      RECORD /svm_evt_match_st/     evt_match(*)
      
C____________________________________________________________________

C     Local variables

      RECORD /track_pointers/ gtrk(1)
      
      INTEGER fitter, npfit
      INTEGER igtrk,itpct,isvtt,ipriv
      INTEGER ncir,ntry,iret,ibad,ient,ient1
      INTEGER i,j
      INTEGER egr_helix_fit, egr_find_outlier
      INTEGER egr_cross_fact
      INTEGER iplane,isvth
      INTEGER imsg
      LOGICAL good_hit(mxpnt)
      DOUBLE PRECISION    xcir(mxpnt),ycir(mxpnt),zcir(mxpnt)
      DOUBLE PRECISION    xerr(mxpnt),yerr(mxpnt),zerr(mxpnt),pnt(mxpnt)
      DOUBLE PRECISION    chisquare, param(5)

      REAL    as,bs,cf,xold,yold,theta
      DOUBLE PRECISION    covar(5,5)
      REAL*4  prim_vrtx_x, prim_vrtx_y, prim_vrtx_z
      DATA    imsg /0/

C Load the gtrk array with the vertex and SVT points

      if (isvtt.ne.0) then
         ient = gtrk(igtrk).nhit
         do iplane=1,svt_group_h.nok
            if (svt_group(iplane).id1 .eq. svt_track(isvtt).id) then
               gtrk(igtrk).nhit = gtrk(igtrk).nhit + 1
               ient = ient+1
               gtrk(igtrk).det(ient) = 2
               gtrk(igtrk).ipnt(ient) = svt_group(iplane).id2
               gtrk(igtrk).pos(ient) = ient
               
            endif
c Has to be maxpnt-1 in case primary vertex is added to fit
            if (ient .ge. mxpnt-1) goto 12
         enddo
 12      continue
         
            
      endif
      if (ipriv.ne.0) then
         gtrk(igtrk).nhit = gtrk(igtrk).nhit+1
         ient = gtrk(igtrk).nhit
         gtrk(igtrk).ipnt(ient) = -999	!!!*VP*
         gtrk(igtrk).pos(ient) = ient
         gtrk(igtrk).det(ient) = 3
      endif

C     at start mark all the points that belong to a track as good
      do i=1,gtrk(igtrk).nhit
         good_hit(i) = .true.
      enddo
      ntry   = 1

CC:   Load primary vertex position coordinates:

      prim_vrtx_x = 0.0
      prim_vrtx_y = 0.0
      prim_vrtx_z = 0.0
      if(privert_h.nok .gt. 0) then
         do i = 1,privert_h.nok
            if(privert(i).vtx_id .eq. 1) then
               prim_vrtx_x = privert(i).x
               prim_vrtx_y = privert(i).y
               prim_vrtx_z = privert(i).z
               go to 100
            end if
         end do
100      continue
      end if

C     try to fit until successful( enough=.true.) or maximum number of
C     iterations exceeded
C
CCC      do while (ntry.le.egrpar(1).mxtry.and..not.enough)
         ncir = 0            ! counts number of points used in the fit

C     load the fitting arrays for the primary vertex

         if (ipriv.eq.1) then
            ncir = ncir + 1
            xcir(ncir) = prim_vrtx_x
            ycir(ncir) = prim_vrtx_y 
            zcir(ncir) = prim_vrtx_z
CCC            wcir(ncir) = 500.0        ! Arbitrary, but large, for now
CCC            wlin(ncir) = 1000.0
            pnt(ncir) = gtrk(igtrk).nhit
         endif

C     Load the fitting arrays with the SVT space points
                                                                       
         if (isvtt.ne.0) then
            do iplane=gtrk(igtrk).ntpc+1,
     +           gtrk(igtrk).ntpc+svt_track(isvtt).nspt
               if (good_hit(iplane) ) then
                  ncir = ncir + 1
                  isvth = gtrk(igtrk).ipnt(iplane)
                  xcir(ncir) = svt_spt(isvth).x(1)
                  ycir(ncir) = svt_spt(isvth).x(2)
                  zcir(ncir) = svt_spt(isvth).x(3)
c                  wcir(ncir) = 1.0/(svt_spt(isvth).cov(1)**2+svt_spt(isvth).
c     +                 cov(2)**2+svt_spt(isvth).cov(3)**2)
C     WRONG should only be 1/dx**2+dy**2 (yes?)
C                  wcir(ncir) = 1.0/(0.01*0.01+0.01*0.01+0.01*0.01)
CCC                  wcir(ncir) = 1.0/(0.01*0.01+0.01*0.01)
c                     previous line is a hard wired 100 micron SVT resolution
CCC                  wlin(ncir) = 1.0/(0.01*0.01)
                  pnt(ncir) = iplane ! Used to identify outliers
               endif            ! good spt
            enddo               ! iplane
         endif   ! isvtt



C Load the fitting arrays with the TPC points

         if (itpct.ne.0) then
            cf = -1.0
            do i=gtrk(igtrk).ntpc,1,-1

C     load the working arrays for the fitting subroutines
C     omit points that were marked as outliers

               if(good_hit(i)) then
                  ncir = ncir+1
                  ient = gtrk(igtrk).ipnt(i)
                  xcir(ncir) = tphit(ient).x
                  ycir(ncir) = tphit(ient).y
                  zcir(ncir) = tphit(ient).z
                  xerr(ncir) = tphit(ient).dx
                  yerr(ncir) = tphit(ient).dy
                  zerr(ncir) = tphit(ient).dz
               endif

            enddo
         endif

        
C        write(6,*) param(1), param(2), param(3), param(4), param(5)
         EGR_REFIT_TRACK_KAL = STAFCV_BAD
         iret = fitter(ncir, xcir, ycir, zcir, xerr, yerr, zerr, param, 
     c   chisquare, 
     c   covar, egrpar(2).useglobal, npfit, good_hit)
CCC     c   covar, egrpar(1).prob(1), egrpar(1).minfit)
C        write(6,*) param(1), param(2), param(3), param(4), param(5)
         gtrk(igtrk).flag = iret
        if(iret.eq.0)then
         EGR_REFIT_TRACK_KAL = STAFCV_OK



         do i=1,5
            gtrk(igtrk).p(i) = param(i)
         enddo

         gtrk(igtrk).p(6) = sqrt(xcir(1)**2+ycir(1)**2)
         gtrk(igtrk).p(7) = chisquare
         gtrk(igtrk).nfit = npfit
CCC         gtrk(igtrk).nfit = ncir

C     clean up list of hits and remove outliers

         do i=1,gtrk(igtrk).nhit
            if( .not. good_hit(i)) then
               gtrk(igtrk).pos(i) = -99
            endif
         enddo
         
      endif                     ! if(iret.eq.0)
      
      return
      end
      
      

