      INTEGER*4 FUNCTION EGR_FITTER(
     1             tphit_h,             tphit ,
     2           privert_h,           privert ,
     3           tptrack_h,           tptrack ,
     4            tpeval_h,            tpeval ,
     5           svt_spt_h,           svt_spt ,
     6            egrpar_h,            egrpar ,
     7         svt_track_h,         svt_track ,
     8         svt_group_h,         svt_group ,
     9         evt_match_h,         evt_match , 
     9           globtrk_h,           globtrk,
     9       globtrk_aux_h,       globtrk_aux) 
C>--------------------------------------------------------------------
C *********************************************************************
C ***                                                                **
C ***  EGR_FITTER is the function to refit all requested data into a **
C ***  global track.                                                 **
C ***                                                                **
C ***  1/10/94 - MITCH - Original version to include TPC hits and the**
C ***                     primary vertex only.                       **
C ***  3/94    - MITCH - Now does matched and perfect SVT tracks,too **
C ***  5/30/94 - MITCH - Now fills the id_globtrk elements           **
C ***  7/1/94  - MITCH - Scenario system implemented through #8.     **
C ***  10/3/94 - MITCH - Scenario system tested through #10.         **
C ***  8/20/96 - MARGE - Move the damn thing to STAF                 **
C ***  6/6/97 - CAINES - Changed the code to try to make it more     **
C ***                    transparent as I couldn't figure out what it** 
C ***                      was up to before. Also changed            **
C ***                    egrpar.perfect to egrpar.useglobal          **
C ***  9/9/97 - CAINES - Updated the SVT track-hit  identification   **
C ***  12/2/97 - CAINES - Added message routine as official one now  **
C ***                     unmanaged                                  **
C ***  6/5/98 - CAINES - Changed globtrk to dst_track and dst_track_aux* 
C *********************************************************************

C Input Arguments:

C     tphit_h   =       header for tphit table (TPC hit information)
C     tphit     =       rows of the tphit table
C     privert_h =       header for the privert table (Primary 
C                              vertex info)
C     privert   =       rows of the privert table
C     tptrack_h =       header for the tptrack table (TPC 
C                              reconstructed tracks)
C     tptrack   =       rows of the tptrack table
C     tpeval_h  =       header for the tpeval table (for TPC 
C                              track MC ID)
C     tpeval    =       rows of the tpeval table
C     svt_spt_h =       header for the svt_spt table (SVT space points)
C     svt_spt   =       rows of the svt_spt table 
C     svt_group_h =    header for the svt_group table (SVT 
C                              track-hit pointer)
C     svt_group =      rows of the svt_group table
C     egrpar_h  =       header for the egpar table (EGR parameters)
C     egrpar    =       rows of the egrpar table
C     svt_track_h =     header for the svt_track table (SVT 
C                              reconstructed tracks)
C     svt_track =       rows of the svt_track table
C     evt_match_h =     header for the evt_match table (TPC/SVT 
C                              matcher info)
C     evt_match =       rows of the evt_match table

C Output Arguments:
C     globtrk_h =       header for the dst_track table
C     globtrk   =       rows of the globtrk table
C     globtrk_aux_h =      header for the dst_track_aux tables
C     globtrk_aux =     rows of teh dst_track table
C<----------------------------------------------------------------------
      IMPLICIT NONE
#include "egr_fitter.inc"
#include "egr_track_pointers.inc"
#include "math_constants.inc"
C ---------------------------------------------------------------------

      RECORD  /track_pointers/ gtrk(mxtrack)

      INTEGER i,j,jj,iret,itrk,ient,imatch
      INTEGER igtrk,itpct,isvtt,ipriv,iglob,ishit,ithit
      INTEGER tpcmc(mxtrack),lasthit,ifirst
      INTEGER*4 egr_refit_track,egr_load_globtrk
      INTEGER svt(mxtrack),tpc(mxtrack)
      INTEGER imsg,imsgw
      INTEGER imsg1,imsg2,imsg3,imsg4,imsg5,imsg6,imsg7,imsg8
      CHARACTER*132 errmsg,msg
      REAL covar(3,3),rfirst,rlast,r
      DATA imsg,imsgw,imsg1,imsg2,imsg3 /5*0/
      DATA imsg4,imsg5,imsg6,imsg7,imsg8 /5*0/

C ---------------------------------------------------------------------

      write(msg,'(A30)') ' EGR_REFITTER-I: started. '
      call message(msg,1,imsg)

C  Initialize number of refit tracks

      globtrk_h.nok = 0
      globtrk_aux_h.nok = 0
c      write(6,*) 'EGR: ',tphit_h.nok,tptrack_h.nok,tpeval_h.nok,globtrk_h.nok

CCC  Initialize the local track-pointer array structure to zero:
CCC  L. Ray, 9/16/98

      write(6,997) 
997   format(2x,'Initializing gtrk in egr_fitter.F')

      do i = 1,mxtrack
         gtrk(i).nhit         = 0
         gtrk(i).nfit         = 0
         gtrk(i).flag         = 0
         gtrk(i).ntpc         = 0
         do j = 1,mxpnt
            gtrk(i).ipnt(j)   = 0
            gtrk(i).pos(j)    = 0
            gtrk(i).det(j)    = 0
         end do
         do j = 1,9
            gtrk(i).p(j)      = 0.0
         end do
      end do

C  Use the egrpar(1).scenario variable to set the running schem

      if (egrpar(1).scenario.eq.1) then     ! Real TPC stand-alone tracking
         egrpar(1).useglobal = 0             ! Refit the tptrack hits
         egrpar(1).usetpc = 2
         egrpar(1).usesvt = 0
         egrpar(1).usevert = 0
         egrpar(1).useemc = 0
         egrpar(1).usetof = 0

      else if (egrpar(1).scenario.eq.2) then ! Perfect TPC stand-alone tracking
         egrpar(1).useglobal = 0          ! Straight copy of the tptrack table
         egrpar(1).usetpc = 1
         egrpar(1).usesvt = 0
         egrpar(1).usevert = 0
         egrpar(1).useemc = 0
         egrpar(1).usetof = 0
C Real SVT  stand-alone tracking. Refit the SVT hits
      else if (egrpar(1).scenario.eq.3) then                         
         egrpar(1).useglobal = 0
         egrpar(1).usetpc = 0
         egrpar(1).usesvt = 2
         egrpar(1).usevert = 0
         egrpar(1).useemc = 0
         egrpar(1).usetof = 0

C Perfect SVT stand-alone tracking. Straight copy of the svt_track table

      else if (egrpar(1).scenario.eq.4) then
         egrpar(1).useglobal = 0 
         egrpar(1).usetpc = 0
         egrpar(1).usesvt = 1
         egrpar(1).usevert = 0
         egrpar(1).useemc = 0
         egrpar(1).usetof = 0

C Refit TPC tracks with prim. vertex

      else if (egrpar(1).scenario.eq.5) then
         egrpar(1).useglobal = 0
         egrpar(1).usetpc = 2
         egrpar(1).usesvt = 0
         if (egrpar(1).usevert.eq.0) egrpar(1).usevert = 1
         egrpar(1).useemc = 0
         egrpar(1).usetof = 0

C  Real SVT/TPC matching with svm. Only tracks including TPC into globtrk

      else if (egrpar(1).scenario.eq.6) then
         egrpar(1).useglobal = 2
         egrpar(1).usetpc = 1
         egrpar(1).usesvt = 0
         egrpar(1).usevert = 0
         egrpar(1).useemc = 0
         egrpar(1).usetof = 0

C Perfect SVT/TPC matching (M.C.). Only tracks w/ TPC into globtrk

      else if (egrpar(1).scenario.eq.7) then
         egrpar(1).useglobal = 1
         egrpar(1).usetpc = 0
         egrpar(1).usesvt = 0
         egrpar(1).usevert = 0
         egrpar(1).useemc = 0
         egrpar(1).usetof = 0

C Improved real SVT/TPC matching. Also handles leftover SVT/TPC tracks 

      else if (egrpar(1).scenario.eq.8) then
         egrpar(1).useglobal = 2
         egrpar(1).usetpc = 1
         egrpar(1).usesvt = 1
         egrpar(1).usevert = 0
         egrpar(1).useemc = 0
         egrpar(1).usetof = 0

C  Improved perfect SVT/TPC matching. Also handles leftover SVT/TPC tracks

      elseif (egrpar(1).scenario.eq.9) then
         egrpar(1).useglobal = 1
         egrpar(1).usetpc = 1
         egrpar(1).usesvt = 1
         egrpar(1).usevert = 0
         egrpar(1).useemc = 0
         egrpar(1).usetof = 0

C Real SVT/TPC matching with svm  Only tracks including TPC into globtrk 
C All info from svt_track except invpt taken from tptrack.       
      else if (egrpar(1).scenario.eq.10) then 
         egrpar(1).useglobal = 3
         egrpar(1).usetpc = 1
         egrpar(1).usesvt = 0      
         egrpar(1).usevert = 0
         egrpar(1).useemc = 0
         egrpar(1).usetof = 0

C Perfect SVT/TPC matching (M.C.). Only tracks w/ TPC into globtrk
C Refit done

      else if (egrpar(1).scenario.eq.11) then
         egrpar(1).useglobal = 1
         egrpar(1).usetpc = -1
         egrpar(1).usesvt = -1
         egrpar(1).usevert = 0
         egrpar(1).useemc = 0
         egrpar(1).usetof = 0
      end if ! End of choosing scenario


C *************************************************************************
C  Fill the gtrk refitting array here.
C  First work on the TPC hit information.  Set up hit assignments.
C  This step is necessary for all scenarios except 4.
C  The vertex and SVT information will be added in egr_refit_track.

      if (egrpar(1).usetpc.gt.0 .or. 
     +     egrpar(1).useglobal .gt. 0) then

C  Need to fill the gtrk array, a temporary array containing the
C  hit information of all the hits to be used in the fitting procedure.
C  First, it will be filled with the outermost hits, or the TPC hits.
C  Abort if the TPC is needed but there are no TPC tracks

         if (tptrack_h.nok.eq.0 .or. tptrack_h.nok.gt.mxtrack) then
            call message('EGR_REFITTER-W: None or too many TPC 
     +           track - aborted.',1,imsgw)
            egr_fitter=STAFCV_BAD
            return
         endif
         
         do i=1,tptrack_h.nok ! Reset number of hits counter
            gtrk(i).nhit = 0
            gtrk(i).ntpc = 0
            gtrk(i).nfit = 0
            tpc(tptrack(i).id) = i
*--- reset foreign keys that are set here SM ---------*
	    tptrack(i).id_globtrk = 0
         end do
         do ithit=1,tphit_h.nok        ! Loop over number of hits
            tphit(ithit).id_globtrk = 0     !  Zero out F.key - L. Ray 9/16/98
            itrk = tphit(ithit).track/1000
            if (itrk.ge.1.and.itrk.le.mxtrack) then
               ient = tphit(ithit).track - itrk*1000
               if (ient.ge.1.and.ient.le.mxpnt_tpc) then
                  if ( gtrk(itrk).nhit .le. mxpnt_tpc) then
                     gtrk(itrk).ipnt(ient) = ithit
                     gtrk(itrk).pos(ient) = ient
                     gtrk(itrk).det(ient) = 1
                     gtrk(itrk).nhit = gtrk(itrk).nhit + 1
                     gtrk(itrk).ntpc = gtrk(itrk).ntpc + 1
                  endif
               else
                  if( ient .eq. mxpnt_tpc+1) then 
                     write (errmsg,'(A45,I5)')
     +                 ' EGR_REFITTER-W: Too many hits on track', itrk
                     call message(errmsg,1,imsg2)
                  endif
               endif
            else
               if (itrk.gt.mxtrack) then
                  write (errmsg,'(A45,I8)')
     +                 ' EGR_REFITTER-W: track id out of range',
     +                 tphit(ithit).track
                  call message(errmsg,1,imsg3)
               endif
            endif
         enddo                  ! Loop over ithit
      endif                     ! usetpc.gt.0
 

      if (egrpar(1).usevert.eq.1.or.egrpar(1).usevert.eq.-2) then
         ipriv=1
      else
         ipriv = 0
      endif

      do i=1,svt_track_h.nok
         if( svt_track(i).id .lt. mxtrack) then
            svt(svt_track(i).id) = i
         endif
      enddo
 
      if( egrpar(1).useglobal .eq. 1 ) then
         do itpct = 1,tptrack_h.nok  
            j=1
            jj=0
            do while (j.le.tpeval_h.nok.and.jj.eq.0)
               if (tpeval(j).rtrk.eq.tptrack(itpct).id) then
                  tpcmc(itpct) = tpeval(j).mtrk
                  jj=1
               endif
               j = j+1
            enddo
c     Find svt track with matching mc_id for perfect track matching
            
            isvtt = 0
            j = 1
            do while (isvtt .eq. 0 .and. j .le. svt_track_h.nok)
               if( svt_track(j).id_mctrack .eq. tpcmc(itpct)
     +              .and. svt_track(j).flag .gt. 0) then
                  isvtt = svt_track(j).id
               endif
               j = j +1 
            enddo
            if( isvtt .ne. 0) then
c     Fill tracks

               if( egrpar(1).usesvt .eq. 0 .and. 
     +              egrpar(1).usetpc .eq. 0) then
                  globtrk_h.nok = globtrk_h.nok + 1
                  globtrk_aux_h.nok = globtrk_aux_h.nok + 1
                  igtrk = globtrk_h.nok
                  globtrk(igtrk).id= igtrk
                 globtrk(igtrk).iflag = svt_track(svt(isvtt)).flag*10
     +                 +(sign(1.0,float(tptrack(itrk).flag)))*500
     +                 +tptrack(itrk).flag
                  globtrk(igtrk).det_id = 3
                  globtrk(igtrk).n_point = svt_track(svt(isvtt)).nspt +
     +                 tptrack(itpct).nrec
                  globtrk(igtrk).n_max_point = 0
                  globtrk(igtrk).n_fit_point = svt_track(svt(isvtt)).nspt +
     +                 tptrack(itpct).nfit
                  globtrk(igtrk).icharge=tptrack(itpct).q ! tpc q
                  if( abs(globtrk(igtrk).icharge) .ne. 1) then
                     globtrk(igtrk).iflag = -1.0*
     +                    abs(globtrk(igtrk).iflag+20)
                  endif
                  globtrk(igtrk).x0=svt_track(svt(isvtt)).r0*
     +                 cos(svt_track(svt(isvtt)).phi0*C_RAD_PER_DEG)
                  globtrk(igtrk).y0=svt_track(svt(isvtt)).r0*
     +                 sin(svt_track(svt(isvtt)).phi0*C_RAD_PER_DEG)
                  globtrk(igtrk).z0=svt_track(svt(isvtt)).z0
                  globtrk(igtrk).psi=svt_track(svt(isvtt)).psi
                  globtrk(igtrk).tanl=svt_track(svt(isvtt)).tanl
                  globtrk(igtrk).invpt=abs(tptrack(itpct).invp) ! tpc pt
                  globtrk(igtrk).covar_diag(1)
     +                 =svt_track(svt(isvtt)).cov(1)
                  globtrk(igtrk).covar_diag(2)
     +                 =svt_track(svt(isvtt)).cov(3)
                  globtrk(igtrk).covar_diag(3)
     +                 =svt_track(svt(isvtt)).cov(6)
                  globtrk(igtrk).covar_diag(4)
     +                 =svt_track(svt(isvtt)).cov(10)
                  globtrk(igtrk).covar_diag(5)
     +                 =svt_track(svt(isvtt)).cov(15)                 
                  globtrk(igtrk).chisq(1)=svt_track(svt(isvtt)).chisq(1)
                  globtrk(igtrk).chisq(2)=svt_track(svt(isvtt)).chisq(2)
C     Now find first point on track
                  rfirst = 99999.
                  do ishit = 1,svt_group_h.nok
                     if( svt_group(ishit).id1 .eq.
     +                    svt_track(svt(isvtt)).id) then
                        ient = svt_group(ishit).id2
                        r =  (svt_spt(ient).x(1)**2+svt_spt(ient).x(2)**2+
     +                       svt_spt(ient).x(3)**2)
                        if( r .lt. rfirst) then
                           rfirst = r
                           ifirst = ient 
                        endif   
                     endif
                  enddo
                  
                  globtrk(igtrk).x_first(1) = svt_spt(ifirst).x(1)
                  globtrk(igtrk).x_first(2) = svt_spt(ifirst).x(2)
                  globtrk(igtrk).x_first(3) = svt_spt(ifirst).x(3)
C     Now find last point on track and calculate track length.
                  
                  if (tptrack(itpct).id.gt.0.and.
     +                 tptrack(itpct).id.lt.mxtrack) then
                     lasthit = gtrk(tptrack(itpct).id).ipnt(1)
                  else
                     write (errmsg,'(A60)')
     +                    'EGR_REFITTER-W: lasthit trk id error'
                     call message(errmsg,1,imsg4)
                  endif
                  
                  globtrk(igtrk).x_last(1) = tphit(lasthit).x
                  globtrk(igtrk).x_last(2) = tphit(lasthit).y
                  globtrk(igtrk).x_last(3) = tphit(lasthit).z
                  globtrk(igtrk).length = sqrt((globtrk(igtrk).x_last(1) 
     +                - globtrk(igtrk).x_first(1))**2 +
     +                 (globtrk(igtrk).x_last(2) 
     +                - globtrk(igtrk).x_first(2))**2)

                  globtrk(igtrk).impact = 0.                  
                  globtrk(igtrk).ndegf = 2*(svt_track(svt(isvtt)).nspt +
     +                 tptrack(itpct).nfit)-5
                  globtrk(igtrk).id_global_pid = 0
                  globtrk(igtrk).id_hypo_pid = 0
                  globtrk(igtrk).id_start_vertex = 0
                  globtrk(igtrk).id_stop_vertex = 0
                  globtrk(igtrk).id_emc = 0
                  globtrk(igtrk).id_smd = 0


                  globtrk_aux(globtrk_aux_h.nok).id_track = 
     +                 globtrk(globtrk_aux_h.nok).id
                  globtrk_aux(globtrk_aux_h.nok).covar_off_diag(1)
     +                 =svt_track(svt(isvtt)).cov(2)
                  globtrk_aux(globtrk_aux_h.nok).covar_off_diag(2)
     +                 =svt_track(svt(isvtt)).cov(4)
                  globtrk_aux(globtrk_aux_h.nok).covar_off_diag(3)
     +                 =svt_track(svt(isvtt)).cov(5)
                  globtrk_aux(globtrk_aux_h.nok).covar_off_diag(4)
     +                 =svt_track(svt(isvtt)).cov(7)
                  globtrk_aux(globtrk_aux_h.nok).covar_off_diag(5)
     +                 =svt_track(svt(isvtt)).cov(8)
                  globtrk_aux(globtrk_aux_h.nok).covar_off_diag(6)
     +                 =svt_track(svt(isvtt)).cov(9)
                  globtrk_aux(globtrk_aux_h.nok).covar_off_diag(7)
     +                 =svt_track(svt(isvtt)).cov(11)
                  globtrk_aux(globtrk_aux_h.nok).covar_off_diag(8)
     +                 =svt_track(svt(isvtt)).cov(12)
                  globtrk_aux(globtrk_aux_h.nok).covar_off_diag(9)
     +                 =svt_track(svt(isvtt)).cov(13)
                  globtrk_aux(globtrk_aux_h.nok).covar_off_diag(10)
     +                 =svt_track(svt(isvtt)).cov(14)
                  globtrk_aux(globtrk_aux_h.nok).residuals(1) = 0.
                  globtrk_aux(globtrk_aux_h.nok).residuals(2) = 0.
                  if (globtrk(igtrk).length.gt.99999.9) then
                     globtrk(igtrk).length=99999.9
                  endif
                  
               else
                  iret = egr_refit_track(tptrack(itpct).id,tptrack(itpct).id,
     +                 svt(isvtt),ipriv,gtrk,
     +                 tphit_h,tphit,privert_h,privert,svt_spt_h,svt_spt,
     +                 svt_track_h,svt_track,svt_group_h,svt_group,
     +                 egrpar_h,egrpar,evt_match_h,evt_match,covar)
                  
                  iret = egr_load_globtrk(globtrk_h,globtrk,globtrk_aux_h
     +                 ,globtrk_aux,tptrack(itpct).id,gtrk,
     +                 tphit_h,tphit,svt_spt_h,svt_spt,covar,itpct,isvtt,
     +                 ipriv)
               endif
               tptrack(itpct).id_globtrk = globtrk_h.nok
               svt_track(svt(isvtt)).id_globtrk = globtrk_h.nok  
            endif               ! isvtt>0
c     mark spts as used
            do ient = 1, gtrk(itpct).nfit
               if( gtrk(itpct).det(ient) .eq. 1) then
                  ithit = gtrk(itpct).ipnt(ient)
                  tphit(ithit).id_globtrk = globtrk_h.nok
               elseif( gtrk(itpct).det(ient) .eq. 2)  then     
                  svt_spt(gtrk(itpct).ipnt(ient)).id_globtrk =  
     +                 globtrk_h.nok
               endif
            enddo
         enddo                  ! Loop over all tpc tracks
         
      elseif( egrpar(1).useglobal .gt. 1) then ! Use svms matching
         
         do imatch=1,evt_match_h.nok ! Loop over evt_match table
            itpct = evt_match(imatch).idtpc
            isvtt = evt_match(imatch).idsvt
            if( egrpar(1).useglobal .eq. 2) then
               
               igtrk = itpct             
               iret = egr_refit_track(igtrk,itpct,svt(isvtt),ipriv,gtrk,
     +              tphit_h,tphit,privert_h,privert,svt_spt_h,svt_spt,
     +              svt_track_h,svt_track,svt_group_h,svt_group,
     +              egrpar_h,egrpar,evt_match_h,evt_match,covar)
               
               iret = egr_load_globtrk(globtrk_h,globtrk,globtrk_aux_h
     +              ,globtrk_aux,igtrk,gtrk,tphit_h,tphit,svt_spt_h,
     +              svt_spt,covar,itpct,isvtt,ipriv)
               
c     mark spts as used
               
               do ient = 1, gtrk(itpct).nfit
                  if( gtrk(itpct).det(ient) .eq. 1) then
                     ithit = gtrk(itpct).ipnt(ient)
                     tphit(ithit).id_globtrk = globtrk_h.nok
                  elseif( gtrk(itpct).det(ient) .eq. 2)  then   
                     svt_spt(gtrk(itpct).ipnt(ient)).id_globtrk =  
     +                    globtrk_h.nok
                  endif
               enddo
               
            else                ! egrpar(1).useglobal=3
               
               globtrk_h.nok = globtrk_h.nok + 1
               globtrk_aux_h.nok = globtrk_aux_h.nok + 1
               igtrk = globtrk_h.nok
               globtrk(igtrk).id=igtrk              
               globtrk(igtrk).iflag = svt_track(svt(isvtt)).flag*10
     +              +(sign(1.0,float(tptrack(tpc(itpct)).flag)))*500
     +              +tptrack(tpc(itpct)).flag             
               globtrk(igtrk).det_id = 3
               globtrk(igtrk).n_point = svt_track(svt(isvtt)).nspt +
     +              tptrack(tpc(itpct)).nrec
                globtrk(igtrk).n_max_point = 0  
                globtrk(igtrk).n_fit_point = svt_track(svt(isvtt)).nspt +
     +              tptrack(tpc(itpct)).nfit 
               globtrk(igtrk).icharge=tptrack(tpc(itpct)).q ! tpc q
               globtrk(igtrk).x0=svt_track(svt(isvtt)).r0*
     +              cos(svt_track(svt(isvtt)).phi0*C_RAD_PER_DEG)            
               globtrk(igtrk).y0=svt_track(svt(isvtt)).r0*
     +              sin(svt_track(svt(isvtt)).phi0*C_RAD_PER_DEG)
               globtrk(igtrk).z0=svt_track(svt(isvtt)).z0
               globtrk(igtrk).psi=svt_track(svt(isvtt)).psi               
               globtrk(igtrk).tanl=svt_track(svt(isvtt)).tanl
               globtrk(igtrk).invpt=abs(tptrack(tpc(itpct)).invp) ! tpc pt
               globtrk(igtrk).covar_diag(1)=svt_track(svt(isvtt)).cov(1)
               globtrk(igtrk).covar_diag(2)=svt_track(svt(isvtt)).cov(3)
               globtrk(igtrk).covar_diag(3)=svt_track(svt(isvtt)).cov(6)
               globtrk(igtrk).covar_diag(4)=svt_track(svt(isvtt)).cov(10)
               globtrk(igtrk).covar_diag(5)=svt_track(svt(isvtt)).cov(15)
               globtrk(igtrk).chisq(1)=svt_track(svt(isvtt)).chisq(1)
               globtrk(igtrk).chisq(2)=svt_track(svt(isvtt)).chisq(2)
C     Now find first point on track
               rfirst = 99999.
               do ishit = 1,svt_group_h.nok
                  if( svt_group(ishit).id1 .eq.
     +                 svt_track(svt(isvtt)).id) then
                     ient = svt_group(ishit).id2
                     r = (svt_spt(ient).x(1)**2+svt_spt(ient).x(2)**2+
     +                    svt_spt(ient).x(3)**2)
                     if( r .lt. rfirst) then
                        rfirst = r
                        ifirst = ient 
                     endif   
                  endif
               enddo
               
               globtrk(igtrk).x_first(1)= svt_spt(ifirst).x(1)
               globtrk(igtrk).x_first(2)= svt_spt(ifirst).x(2)
               globtrk(igtrk).x_first(3)= svt_spt(ifirst).x(3)

C     Now find last point on track and calculate track length.

               if (tptrack(tpc(itpct)).id.gt.0.and.
     +              tptrack(tpc(itpct)).id.lt.mxtrack) then
                  lasthit = gtrk(tptrack(tpc(itpct)).id).ipnt(1)
               else
                  write (errmsg,'(A60)')
     +                 'EGR_REFITTER-W: lasthit trk id error'
                  call message(errmsg,1,imsg4)
               endif
               
               globtrk(igtrk).x_last(1) = tphit(lasthit).x
               globtrk(igtrk).x_last(2) = tphit(lasthit).y
               globtrk(igtrk).x_last(3) = tphit(lasthit).z
               globtrk(igtrk).length = sqrt((globtrk(igtrk).x_last(1)
     +              -globtrk(igtrk).x_last(1))**2 +
     +              (globtrk(igtrk).x_last(2) -
     +              globtrk(igtrk).x_last(2))**2 )           
               if (globtrk(igtrk).length.gt.99999.9) then
                  globtrk(igtrk).length=99999.9
               endif
           
               globtrk(igtrk).impact = 0
               globtrk(igtrk).ndegf = 2*(svt_track(svt(isvtt)).nspt +
     +              tptrack(tpc(itpct)).nfit) - 5
               globtrk(igtrk).id_global_pid = 0
               globtrk(igtrk).id_hypo_pid = 0
               globtrk(igtrk).id_start_vertex = 0
               globtrk(igtrk).id_stop_vertex = 0
               globtrk(igtrk).id_emc = 0
               globtrk(igtrk).id_smd = 0
               
               globtrk_aux(globtrk_aux_h.nok).id_track = igtrk
               globtrk_aux(globtrk_aux_h.nok).residuals(1) = 0
               globtrk_aux(globtrk_aux_h.nok).residuals(2) = 0
               globtrk_aux(globtrk_aux_h.nok).covar_off_diag(1) = 
     +              svt_track(svt(isvtt)).cov(2)
               globtrk_aux(globtrk_aux_h.nok).covar_off_diag(2) = 
     +              svt_track(svt(isvtt)).cov(4)
               globtrk_aux(globtrk_aux_h.nok).covar_off_diag(3) = 
     +              svt_track(svt(isvtt)).cov(5)
               globtrk_aux(globtrk_aux_h.nok).covar_off_diag(4) = 
     +              svt_track(svt(isvtt)).cov(7)
               globtrk_aux(globtrk_aux_h.nok).covar_off_diag(5) = 
     +              svt_track(svt(isvtt)).cov(8)
               globtrk_aux(globtrk_aux_h.nok).covar_off_diag(6) = 
     +              svt_track(svt(isvtt)).cov(9)
               globtrk_aux(globtrk_aux_h.nok).covar_off_diag(7) = 
     +              svt_track(svt(isvtt)).cov(11)
               globtrk_aux(globtrk_aux_h.nok).covar_off_diag(8) = 
     +              svt_track(svt(isvtt)).cov(12)
               globtrk_aux(globtrk_aux_h.nok).covar_off_diag(9) = 
     +              svt_track(svt(isvtt)).cov(13)
               globtrk_aux(globtrk_aux_h.nok).covar_off_diag(10) = 
     +              svt_track(svt(isvtt)).cov(14)
c     mark spts as used
               
               do ient = 1, gtrk(itpct).ntpc
                  if( gtrk(itpct).det(ient) .eq. 1) then
                     ithit = gtrk(itpct).ipnt(ient)
                     tphit(ithit).id_globtrk = igtrk
                  endif
               enddo
               
               ient = gtrk(itpct).ntpc
               do ishit = 1, svt_group_h.nok
                  if( svt_group(ishit).id1 .eq. 
     +                 svt_track(svt(isvtt)).id) then
                     
                     ient = ient+1
                     ithit = svt_group(ishit).id2
                     svt_spt(ithit).id_globtrk = igtrk
                  endif
               enddo
            endif
            
            tptrack(tpc(itpct)).id_globtrk = globtrk_h.nok
            svt_track(svt(isvtt)).id_globtrk = globtrk_h.nok
            
         enddo                  ! imatch
      endif                     ! if useglobal>0
      
C     Loop through unmatched TPC tracks here.  Straight copy of tptrack.
      
      if( egrpar(1).usetpc .gt. 0) then
         isvtt = 0
         do itrk = 1,tptrack_h.nok
            if( tptrack(itrk).id_globtrk.eq.0 .and.
     +           tptrack(itrk).invp .ne. 0.0   .and.
     +           tptrack(itrk).flag .gt. 0) then
C     +           tptrack(itrk).nrec .gt. 10 )    then
               
               if( egrpar(1).usetpc .gt. 1) then
                  
                  iret = egr_refit_track(tptrack(itrk).id,
     +                 tptrack(itrk).id,isvtt,ipriv,gtrk,tphit_h,
     +                 tphit,privert_h,privert,svt_spt_h,svt_spt,
     +                 svt_track_h,svt_track,svt_group_h,svt_group,
     +                 egrpar_h,egrpar,evt_match_h,evt_match,covar)
                  iret = egr_load_globtrk(globtrk_h,globtrk,globtrk_aux_h
     +                 ,globtrk_aux,tptrack(itrk).id,gtrk
     +                 ,tphit_h,tphit,svt_spt_h,svt_spt,covar,itrk,isvtt,
     +                 ipriv)
               else


                  gtrk(itrk).nfit = tptrack(itrk).nfit
                  globtrk_h.nok = globtrk_h.nok + 1
                  globtrk_aux_h.nok = globtrk_aux_h.nok + 1

                  iglob = globtrk_h.nok
                  globtrk(iglob).id = iglob
                  globtrk(iglob).iflag = (sign(1.0,float
     +                 (tptrack(itrk).flag)))*100
     +                 +tptrack(itrk).flag

                  globtrk(iglob).det_id = 1
                  globtrk(iglob).n_point = tptrack(itrk).nrec
                  globtrk(iglob).n_max_point = 0
                  globtrk(iglob).n_fit_point = tptrack(itrk).nfit          
                  globtrk(iglob).icharge = tptrack(itrk).q
                  if( abs(globtrk(iglob).icharge) .ne. 1) then
                     globtrk(iglob).iflag = 
     +                    -1.0*abs(globtrk(iglob).iflag+20)
                  endif
                  globtrk(iglob).x0 = tptrack(itrk).r0*
     +                 cos(tptrack(itrk).phi0*C_RAD_PER_DEG)
                  globtrk(iglob).y0 = tptrack(itrk).r0*
     +                 sin(tptrack(itrk).phi0*C_RAD_PER_DEG)
                  globtrk(iglob).z0 = tptrack(itrk).z0
                  globtrk(iglob).psi = tptrack(itrk).psi
                  globtrk(iglob).tanl = tptrack(itrk).tanl
                  globtrk(iglob).invpt = tptrack(itrk).invp
                  globtrk(iglob).covar_diag(1) = tptrack(itrk).cov(1)
                  globtrk(iglob).covar_diag(2) = tptrack(itrk).cov(3)
                  globtrk(iglob).covar_diag(3) = tptrack(itrk).cov(6)
                  globtrk(iglob).covar_diag(4) = tptrack(itrk).cov(10)
                  globtrk(iglob).covar_diag(5) = tptrack(itrk).cov(15)
                  globtrk(iglob).chisq(1) = tptrack(itrk).chisq(1)
                  globtrk(iglob).chisq(2) = tptrack(itrk).chisq(2)
C     Now find first point on track.
                  if (tptrack(itrk).id.gt.0.and.
     +                 tptrack(itrk).id.lt.mxtrack) then
                     ifirst = gtrk(tptrack(itrk).id).ntpc
                     ifirst = 
     +              gtrk(tptrack(itrk).id).ipnt(ifirst)
                  else
                     write (errmsg,'(A60)')
     +                    'EGR_REFITTER-W: lasthit track id error'
                     call message(errmsg,1,imsg4)
                  endif
                  globtrk(iglob).x_first(1) = tphit(ifirst).x
                  globtrk(iglob).x_first(2) = tphit(ifirst).y
                  globtrk(iglob).x_first(3) = tphit(ifirst).z

C     Now find last point on track and calculate track length.
                  if (tptrack(itrk).id.gt.0.and.
     +                 tptrack(itrk).id.lt.mxtrack) then
                     lasthit = gtrk(tptrack(itrk).id).ipnt(1)
                  else
                     write (errmsg,'(A60)')
     +                    'EGR_REFITTER-W: lasthit track id error'
                     call message(errmsg,1,imsg4)
                  endif
                  globtrk(iglob).x_last(1) = tphit(lasthit).x
                  globtrk(iglob).x_last(2) = tphit(lasthit).y
                  globtrk(iglob).x_last(3) = tphit(lasthit).z
                  globtrk(iglob).length = sqrt((globtrk(iglob).x_last(1)
     +                 -globtrk(iglob).x_first(1))**2 +
     +                 (globtrk(iglob).x_last(2)
     +                 -globtrk(iglob).x_first(2))**2)
                  if (globtrk(iglob).length.gt.99999.9) then
                     globtrk(iglob).length=99999.9
                  endif


                  globtrk(iglob).ndegf = (2*tptrack(itrk).nfit) -5
                  globtrk(iglob).id_global_pid = 0
                  globtrk(iglob).id_hypo_pid = 0
                  globtrk(iglob).id_start_vertex= 0
                  globtrk(iglob).id_stop_vertex = 0
                  globtrk(iglob).id_emc = 0
                  globtrk(iglob).id_smd = 0 

               endif            ! End of egrpar(1).perfect .ne. 1
               do ient = 1, gtrk(itrk).nfit
                  if( ient .le .mxpnt_tpc) then
                     ithit = gtrk(itrk).ipnt(ient)
                     tphit(ithit).id_globtrk = globtrk_h.nok
                  endif
               enddo
               tptrack(itrk).id_globtrk = globtrk_h.nok
            endif               ! End of test if good tpc track
         enddo                  ! End of tpc track loop
      endif                     ! End of if egrpar(1).usetpc .eq. 1
      
C     Now loop over unmatched SVT tracks.  Straight svt_track copy.
      
      if( egrpar(1).usesvt .gt. 0 ) then
         itpct = 0
         igtrk = tptrack_h.nok
         do itrk = 1,svt_track_h.nok
            if (svt_track(itrk).id_globtrk.eq.0) then
               if (egrpar(1).svtchicut.eq.0.or.
     +              svt_track(itrk).chisq(1).le.
     +              egrpar(1).svtchicut) then ! only copy good SVT tracks
                  if( egrpar(1).usesvt .eq. 1) then

                     globtrk_h.nok = globtrk_h.nok + 1
                     globtrk_aux_h.nok = globtrk_aux_h.nok + 1
                     iglob = globtrk_h.nok
                     rlast = 0.
                     rfirst= 10000.
                     do ishit = 1,svt_group_h.nok
                        if( svt_group(ishit).id1 .eq.
     +                       svt_track(itrk).id) then
                           ient = svt_group(ishit).id2
                           svt_spt(ient).id_globtrk = iglob
                           r = (svt_spt(ient).x(1)**2+svt_spt(ient).x(2)**2+
     +                      svt_spt(ient).x(3)**2)
                           if( r .gt. rlast) then
                              rlast = r
                              lasthit = ient
                           endif
                           if( r .lt. rfirst) then
                              rfirst = r
                              ifirst = ient 
                           endif   
                        endif
                     enddo

                     globtrk(iglob).id = iglob
                     globtrk(iglob).iflag = (sign(1.0,float(
     +                    svt_track(itrk).flag)))*200.
     +                    +svt_track(itrk).flag
                     globtrk(iglob).det_id = 2
                     globtrk(iglob).n_point = svt_track(itrk).nspt
                     globtrk(iglob).n_max_point = 3
                     globtrk(iglob).n_fit_point =  svt_track(itrk).nspt
                     globtrk(iglob).icharge = 
     +                    sign(1.0,svt_track(itrk).invpt)
                     globtrk(iglob).x0 = svt_track(itrk).r0*
     +                    cos(svt_track(itrk).phi0*C_RAD_PER_DEG)
                     globtrk(iglob).y0 = svt_track(itrk).r0*
     +                    sin(svt_track(itrk).phi0*C_RAD_PER_DEG)
                     globtrk(iglob).z0 = svt_track(itrk).z0
                     globtrk(iglob).psi = svt_track(itrk).psi
                     globtrk(iglob).tanl = svt_track(itrk).tanl
                     globtrk(iglob).invpt = abs(svt_track(itrk).invpt)
                     globtrk(iglob).covar_diag(1) = 
     +                       svt_track(itrk).cov(1)
                    globtrk(iglob).covar_diag(2) = 
     +                       svt_track(itrk).cov(3)
                    globtrk(iglob).covar_diag(3) = 
     +                       svt_track(itrk).cov(6)
                    globtrk(iglob).covar_diag(4) = 
     +                       svt_track(itrk).cov(10)
                    globtrk(iglob).covar_diag(5) = 
     +                       svt_track(itrk).cov(15)
                    globtrk(iglob).chisq(1) = svt_track(itrk).chisq(1)
                    globtrk(iglob).chisq(2) = svt_track(itrk).chisq(2)
                    globtrk(iglob).x_first(1) = svt_spt(ifirst).x(1)
                    globtrk(iglob).x_first(2) = svt_spt(ifirst).x(2)
                    globtrk(iglob).x_first(3) = svt_spt(ifirst).x(3)
                    globtrk(iglob).x_last(1) = svt_spt(lasthit).x(1)
                    globtrk(iglob).x_last(2) = svt_spt(lasthit).x(2)
                    globtrk(iglob).x_last(3) = svt_spt(lasthit).x(3)
                    globtrk(iglob).length = sqrt((
     +                   globtrk(iglob).x_last(1) - 
     +                   globtrk(iglob).x_first(1))**2 +
     +                   (globtrk(iglob).x_last(2) - 
     +                   globtrk(iglob).x_first(2))**2)
                    globtrk(iglob).impact = 0
                    globtrk(iglob).ndegf = (2*globtrk(iglob).n_point) - 5 
                    globtrk(iglob).id_global_pid = 0
                    globtrk(iglob).id_hypo_pid = 0
                    globtrk(iglob).id_start_vertex = 0
                    globtrk(iglob).id_stop_vertex = 0
                    globtrk(iglob).id_emc = 0
                    globtrk(iglob).id_smd = 0
                    
                    globtrk_aux(globtrk_aux_h.nok).id_track = iglob
                    globtrk_aux(globtrk_aux_h.nok).residuals(1) = 0
                    globtrk_aux(globtrk_aux_h.nok).residuals(2) = 0
                    globtrk_aux(globtrk_aux_h.nok).covar_off_diag(1) = 
     +                   svt_track(itrk).cov(2)
                    globtrk_aux(globtrk_aux_h.nok).covar_off_diag(2) = 
     +                   svt_track(itrk).cov(4)
                    globtrk_aux(globtrk_aux_h.nok).covar_off_diag(3) = 
     +                   svt_track(itrk).cov(5)
                    globtrk_aux(globtrk_aux_h.nok).covar_off_diag(4) = 
     +                   svt_track(itrk).cov(7)
                    globtrk_aux(globtrk_aux_h.nok).covar_off_diag(5) = 
     +                   svt_track(itrk).cov(8)
                    globtrk_aux(globtrk_aux_h.nok).covar_off_diag(6) = 
     +                   svt_track(itrk).cov(9)
                    globtrk_aux(globtrk_aux_h.nok).covar_off_diag(7) = 
     +                   svt_track(itrk).cov(11)
                    globtrk_aux(globtrk_aux_h.nok).covar_off_diag(8) = 
     +                    svt_track(itrk).cov(12)
                    globtrk_aux(globtrk_aux_h.nok).covar_off_diag(9) = 
     +                   svt_track(itrk).cov(13)
                    globtrk_aux(globtrk_aux_h.nok).covar_off_diag(10) = 
     +                   svt_track(itrk).cov(14)
                 else
                    igtrk = igtrk + 1
                    iret = egr_refit_track(igtrk,itpct,itrk,ipriv,
     +                   gtrk,tphit_h,tphit,privert_h,privert,
     +                   svt_spt_h,svt_spt,svt_track_h,svt_track,
     +                   svt_group_h,svt_group,egrpar_h,egrpar,
     +                   evt_match_h,evt_match,covar)
                    
                    iret = egr_load_globtrk(globtrk_h,globtrk,
     +                   globtrk_aux_h,globtrk_aux,igtrk,gtrk,tphit_h,
     +                   tphit,svt_spt_h,svt_spt,covar,itpct,isvtt,ipriv)
                    
                    do ishit = 1, gtrk(igtrk).nfit
                       ient = gtrk(igtrk).ipnt(ishit)
                       svt_spt(ient).id_globtrk = globtrk_h.nok
                    enddo
                    
                 endif          ! End of if usesvt=1
                 
                 svt_track(itrk).id_globtrk = globtrk_h.nok
                 
              endif             ! End of if egrpar(1).svtchicut
           endif                ! End of if svt_track(itrk).id_globtrk
        enddo                   ! End of do 1,svt_track_h.nok
      endif                     ! End of if egrpar(1).usesvt .gt. 0
      
Cvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
C     This section handles scenarios where egr is first run without fitting
C     the vertex point and then run with fitting the vertex point on the
C     second pass.  This situation is specified with egrpar(1).usevert=2.
      
      if (egrpar(1).usevert.eq.2) egrpar(1).usevert=-2
      
Cvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
      
CCC  Read thru globtrk and check for invpt values out of range (e.g. positive infinity)
CCC  and for such cases set globtrk.invpt = -999999.
CCC  L.Ray - 9/16/98

      if(globtrk_h.nok .gt. 0) then
         do i = 1,globtrk_h.nok
            if(globtrk(i).invpt .ge. -999999. .and. globtrk(i).invpt .le. 999999.) then
               continue 
            else 
               write(6,998) i
998   Format(5x,'BAD Value of globtrk(i=',I7,').invpt detected - set to -999999')
               globtrk(i).invpt = -999999.
            end if  
         end do
      end if
             
      egr_fitter = STAFCV_OK
      
      return
      end
      




