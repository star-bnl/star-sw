C     >--------------------------------------------------------------------
C     
C     EGR_REFIT_TRACK - track fitting routine
      
C     <--------------------------------------------------------------------
      INTEGER*4 FUNCTION EGR_REFIT_TRACK(igtrk,itpct,isvtt,ipriv,gtrk,
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

      RECORD /track_pointers/ gtrk(mxtrack)
      
      INTEGER igtrk,itpct,isvtt,ipriv
      INTEGER ncir,ntry,iret,ibad,ient,ient1
      INTEGER i,j
      INTEGER egr_helix_fit, egr_find_outlier
      INTEGER egr_cross_fact
      INTEGER n_bad
      INTEGER iplane,isvth
      INTEGER imsg
      LOGICAL enough, good_hit(mxpnt)
      REAL    xcir(mxpnt),ycir(mxpnt),zcir(mxpnt)
      REAL    wcir(mxpnt),wlin(mxpnt),pnt(mxpnt)
      REAL    as,bs,cf,xold,yold,theta
      REAL    covar(3,3)
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
         gtrk(igtrk).ipnt(ient) = 999
         gtrk(igtrk).pos(ient) = ient
         gtrk(igtrk).det(ient) = 3
      endif

C     at start mark all the points that belong to a track as good
      do i=1,gtrk(igtrk).nhit
         good_hit(i) = .true.
      enddo
      ntry   = 1
      enough=.false.

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
      do while (ntry.le.egrpar(1).mxtry.and..not.enough)
         ncir = 0            ! counts number of points used in the fit
         cf = -1.0

C     load the fitting arrays for the primary vertex

         if (ipriv.eq.1) then
            ncir = ncir + 1
            xcir(ncir) = prim_vrtx_x
            ycir(ncir) = prim_vrtx_y 
            zcir(ncir) = prim_vrtx_z
            wcir(ncir) = 500.0        ! Arbitrary, but large, for now
            wlin(ncir) = 1000.0
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
                  wcir(ncir) = 1.0/(0.01*0.01+0.01*0.01)
c                     previous line is a hard wired 100 micron SVT resolution
                  wlin(ncir) = 1.0/(0.01*0.01)
                  pnt(ncir) = iplane ! Used to identify outliers
               endif            ! good spt
            enddo               ! iplane
         endif   ! isvtt



C Load the fitting arrays with the TPC points

         if (itpct.ne.0) then

            do i=gtrk(igtrk).ntpc,1,-1

C     load the working arrays for the fitting subroutines
C     omit points that were marked as outliers

               if(good_hit(i)) then
                  ncir = ncir+1
                  ient = gtrk(igtrk).ipnt(i)
                  xcir(ncir) = tphit(ient).x
                  ycir(ncir) = tphit(ient).y
                  zcir(ncir) = tphit(ient).z
                  wcir(ncir) = 1.0/(tphit(ient).dx**2+
     +                 tphit(ient).dy**2)

C     calculate correction to the error, coming from the crossing angle
                  iret = egr_cross_fact(tphit(ient).row,xcir(ncir),
     +                 ycir(ncir),xold,yold,cf,theta)
                  
                  if(ncir.eq.1) then
                     ient1 = ient
                  else if(ncir.eq.2) then
                     wcir(1) = wcir(1)/cf
                  endif
                  xold = xcir(ncir)
                  yold = ycir(ncir)
                  wcir(ncir) = wcir(ncir)/cf
                  if (i.eq.gtrk(igtrk).ntpc+1) then
                     wlin(ncir) = 0
                  else
                     wlin(ncir) = 1.0/tphit(ient).dz**2
                  endif
                  pnt(ncir)  = i ! pointer that identifies outliers
               endif
            enddo   ! i
         
         endif    ! itpct
C     
C     Now fit provided there are at least MINFIT hits left
         if(ncir.le.egrpar(1).minfit) then
            gtrk(igtrk).flag=-1 ! track destroyed by the outliers removal
            ntry =  egrpar(1).mxtry+1
         else
C     try the first full half of the circle
            bs=0
            j=2
            as=(xcir(1)-xcir(2))**2+(ycir(1)-ycir(2))**2
            do while (bs.lt.as.and.j.lt.ncir)
               j=j+1
               bs = as
               as = (xcir(1)-xcir(j))**2+(ycir(1)-ycir(j))**2
            end do
            if(bs.gt.as) then
               ncir = j-1
            endif
C     
C     Check whether enough hits survived
            if(ncir.le.egrpar(1).minfit) then
               gtrk(igtrk).flag=-2 ! not enough points on the first half
               ntry =  egrpar(1).mxtry+1
            else
C     If yes, call the helix fit
               iret=egr_helix_fit(igtrk,gtrk,xcir,ycir,zcir,
     >              wcir,wlin,ncir,egrpar_h,egrpar,covar)
               if(iret.eq.STAFCV_OK) then
                  if((gtrk(igtrk).p(8)/real(2*ncir-3).lt.
     >                 egrpar(1).prob(1).or.egrpar(1).prob(1)
     >                 .lt.0.0).and.
     >                 (gtrk(igtrk).p(9)/real(2*ncir-2).lt.
     >                 egrpar(1).prob(2)
     >                 .or.egrpar(1).prob(2).lt.0.0)) then
                     gtrk(igtrk).nfit = ncir
                     enough=.true. ! if fit succesful quit
                     gtrk(igtrk).flag=1
                  else
                     if(egrpar(1).debug(2).eq.1) then
                        call message('EGR_REFIT_TRACK-I: Calling 
     +                       outlier removal',1,imsg)
                     endif
                     iret=egr_find_outlier(igtrk,gtrk,xcir,ycir,zcir
     >                    ,wcir,wlin,ncir,ibad,egrpar_h,egrpar)
                     if(egrpar(1).debug(2).eq.1) then
                        call message('EGR_REFIT_TRACK-I: Outlier 
     +                       removal completed',1,imsg)
                     endif
                     if(iret.eq.STAFCV_OK) then
                        good_hit(pnt(ibad))= .false.
                        gtrk(igtrk).flag=-3 ! Too many iterations
                        ntry = ntry+1 ! increase number of iterations
                     else
C     could not identify the outlier
                        gtrk(igtrk).flag = -6
                        ntry=egrpar(1).mxtry+1 ! Leave fit have a bad trk
                     endif
                  endif
               else
                  gtrk(igtrk).flag=-4 ! Error from helix fit
                  ntry = egrpar(1).mxtry+1 ! Leave fit hsve s bsd track
               endif
            endif
         endif
      enddo
C
C     clean up list of hits and remove outliers
      if(.not.enough) then
C     fit wasn't succesful, reset all the points
         EGR_REFIT_TRACK = STAFCV_BAD
      else
         n_bad=0               ! number of bad hits
         if( ntry .gt. 1) then 
            do i=1,gtrk(igtrk).nhit
               if( good_hit(i)) then
                  gtrk(igtrk).ipnt(i-n_bad) = gtrk(igtrk).ipnt(i)
                  gtrk(igtrk).det(i-n_bad) = gtrk(igtrk).det(i)
                  gtrk(igtrk).pos(i-n_bad) = i-n_bad
               else
                  n_bad = n_bad + 1
               endif
            enddo
         endif

         EGR_REFIT_TRACK = STAFCV_OK
      endif
      
      return
      end

