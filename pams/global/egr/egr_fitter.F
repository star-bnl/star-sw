      INTEGER*4 FUNCTION EGR_FITTER(
     1             tphit_h,             tphit ,
     2           privert_h,           privert ,
     3           tptrack_h,           tptrack ,
     4            tpeval_h,            tpeval ,
     5           svt_spt_h,           svt_spt ,
     6            egrpar_h,            egrpar ,
     7         svt_track_h,         svt_track ,
     8         svt_group_h,        svt_group ,
     9         evt_match_h,         evt_match , 
     9           globtrk_h,           globtrk ) 
C>----------------------------------------------------------------------
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
C     globtrk_h =       header for the globtrk table (global track information)
C     globtrk   =       rows of the globtrk table
C<----------------------------------------------------------------------
      IMPLICIT NONE
#include "egr_fitter.inc"
#include "egr_track_pointers.inc"
C ---------------------------------------------------------------------

      RECORD  /track_pointers/ gtrk(mxtrack)

      INTEGER i,j,jj,iret,itrk,ient,imatch
      INTEGER igtrk,itpct,isvtt,ipriv,iglob,ishit,ithit
      INTEGER tpcmc(mxtrack),icov,lastrow,lasthit,lastsec
      INTEGER*4 egr_refit_track,egr_load_globtrk
      INTEGER svt(mxtrack),tpc(mxtrack)
      INTEGER imsg,imsgw
      INTEGER imsg1,imsg2,imsg3,imsg4,imsg5,imsg6,imsg7,imsg8
      CHARACTER*132 errmsg,msg
      REAL lambda,covar(3,3)
      DATA imsg,imsgw,imsg1,imsg2,imsg3 /5*0/
      DATA imsg4,imsg5,imsg6,imsg7,imsg8 /5*0/

C ---------------------------------------------------------------------

      write(msg,'(A30)') ' EGR_REFITTER-I: started. '
      call message(msg,1,imsg)

C  Initialize number of refit tracks

      globtrk_h.nok = 0
c      write(6,*) 'EGR: ',tphit_h.nok,tptrack_h.nok,tpeval_h.nok,globtrk_h.nok

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
            tphit(i).id_globtrk = 0
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
                  igtrk = globtrk_h.nok
                  do i=1,15
                     globtrk(igtrk).cov(i)=svt_track(svt(isvtt)).cov(i)
                  enddo
                  globtrk(igtrk).cov(10) = svt_track(svt(isvtt)).nspt +
     +                 tptrack(itpct).nfit 
                  globtrk(igtrk).chisq=svt_track(svt(isvtt)).chisq(1)+
     1                 svt_track(svt(isvtt)).chisq(2)
                  globtrk(igtrk).id= igtrk
                  globtrk(igtrk).icharge=tptrack(itpct).q ! tpc q
                  globtrk(igtrk).r0=svt_track(svt(isvtt)).r0
                  globtrk(igtrk).phi0=svt_track(svt(isvtt)).phi0
                  globtrk(igtrk).z0=svt_track(svt(isvtt)).z0
                  globtrk(igtrk).invpt=abs(tptrack(itpct).invp) ! tpc pt
                  globtrk(igtrk).psi=svt_track(svt(isvtt)).psi
                  globtrk(igtrk).tanl=svt_track(svt(isvtt)).tanl
                  globtrk(igtrk).ndegf = svt_track(svt(isvtt)).nspt +
     +                 tptrack(itpct).nfit
                  globtrk(igtrk).id_emc = 0
                  globtrk(igtrk).id_hypo_pid = 0
                  globtrk(igtrk).id_global_pid = 0
                  globtrk(igtrk).id_part = 0
                  globtrk(igtrk).id_pver = 0
                  globtrk(igtrk).id_tof = 0
                  
                  globtrk(igtrk).sflag = svt_track(svt(isvtt)).flag*10
     +                 +(sign(1.0,float(tptrack(itrk).flag)))*600
     +                 +tptrack(itrk).flag
                  if( abs(globtrk(igtrk).icharge) .ne. 1) then
                     globtrk(igtrk).sflag = -1.0*globtrk(igtrk).sflag+20
                  endif
C     Now find last point on track and calculate track length.
                  
                  if (tptrack(itpct).id.gt.0.and.
     +                 tptrack(itpct).id.lt.mxtrack) then
                     lasthit = gtrk(tptrack(itpct).id).ipnt(1)
                  else
                     write (errmsg,'(A60)')
     +                    'EGR_REFITTER-W: lasthit trk id error'
                     call message(errmsg,1,imsg4)
                  endif
                  
                  lastsec = tphit(lasthit).row/100
                  lastrow = tphit(lasthit).row - lastsec*100
                  globtrk(igtrk).last_row = lastrow
                  globtrk(igtrk).xlast(1) = tphit(lasthit).x
                  globtrk(igtrk).xlast(2) = tphit(lasthit).y
                  globtrk(igtrk).xlast(3) = tphit(lasthit).z
                  lambda = atan(globtrk(igtrk).tanl)
                  globtrk(igtrk).length = (globtrk(igtrk).xlast(3)
     +                 -globtrk(igtrk).z0/sin(lambda))
                  if (globtrk(igtrk).length.gt.999.9) then
                     globtrk(igtrk).length=999.9
                  endif
                  
               else
                  iret = egr_refit_track(tptrack(itpct).id,tptrack(itpct).id,
     +                 svt(isvtt),ipriv,gtrk,
     +                 tphit_h,tphit,privert_h,privert,svt_spt_h,svt_spt,
     +                 svt_track_h,svt_track,svt_group_h,svt_group,
     +                 egrpar_h,egrpar,evt_match_h,evt_match,covar)
                  
                  iret = egr_load_globtrk(globtrk_h,globtrk,tptrack(itpct).id,gtrk,
     +                 tphit_h,tphit,covar,itpct)
               endif
               tptrack(itpct).id_globtrk = globtrk_h.nok
               svt_track(svt(isvtt)).id_globtrk = globtrk_h.nok  
            endif               ! isvtt>0
c     mark spts as used
            do ient = 1, gtrk(itpct).nfit
               if( gtrk(itpct).det(ient) .eq. 1) then
                  ithit = gtrk(itpct).ipnt(ient)
                  tphit(ithit).id_globtrk = (globtrk_h.nok)*1000+ient
               elseif( gtrk(itpct).det(ient) .eq. 2)  then     
                  svt_spt(gtrk(itpct).ipnt(ient)).id_globtrk =  
     +                 (globtrk_h.nok)*1000+ient
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
               
               iret = egr_load_globtrk(globtrk_h,globtrk,igtrk,gtrk,
     +              tphit_h,tphit,covar,itpct)
               
c     mark spts as used
               
               do ient = 1, gtrk(itpct).nfit
                  if( gtrk(itpct).det(ient) .eq. 1) then
                     ithit = gtrk(itpct).ipnt(ient)
                     tphit(ithit).id_globtrk = (globtrk_h.nok)*1000
     +                    +ient
                  elseif( gtrk(itpct).det(ient) .eq. 2)  then   
                     svt_spt(gtrk(itpct).ipnt(ient)).id_globtrk =  
     +                    (globtrk_h.nok)*1000+ient
                  endif
               enddo
               
            else                ! egrpar(1).useglobal=3
               
               globtrk_h.nok = globtrk_h.nok + 1
               igtrk = globtrk_h.nok
               do i=1,15
                  globtrk(igtrk).cov(i)=svt_track(svt(isvtt)).cov(i)
               enddo
               globtrk(igtrk).cov(10) = svt_track(svt(isvtt)).nspt +
     +              tptrack(tpc(itpct)).nfit 
               globtrk(igtrk).chisq=svt_track(svt(isvtt)).chisq(1)+
     1              svt_track(svt(isvtt)).chisq(2)
               globtrk(igtrk).id=igtrk
               globtrk(igtrk).icharge=tptrack(tpc(itpct)).q ! tpc q
               globtrk(igtrk).r0=svt_track(svt(isvtt)).r0
               globtrk(igtrk).phi0=svt_track(svt(isvtt)).phi0
               globtrk(igtrk).z0=svt_track(svt(isvtt)).z0
               globtrk(igtrk).invpt=abs(tptrack(tpc(itpct)).invp) ! tpc pt
               globtrk(igtrk).psi=svt_track(svt(isvtt)).psi
               globtrk(igtrk).tanl=svt_track(svt(isvtt)).tanl
               globtrk(igtrk).ndegf = svt_track(svt(isvtt)).nspt +
     +              tptrack(tpc(itpct)).nfit
               globtrk(igtrk).id_emc = 0
               globtrk(igtrk).id_hypo_pid = 0
               globtrk(igtrk).id_global_pid = 0
               globtrk(igtrk).id_part = 0
               globtrk(igtrk).id_pver = 0
               globtrk(igtrk).id_tof = 0

               globtrk(igtrk).sflag = svt_track(svt(isvtt)).flag*10
     +                 +(sign(1.0,float(tptrack(tpc(itpct)).flag)))*600
     +                 +tptrack(tpc(itpct)).flag

C     Now find last point on track and calculate track length.

                  if (tptrack(tpc(itpct)).id.gt.0.and.
     +                 tptrack(tpc(itpct)).id.lt.mxtrack) then
                     lasthit = gtrk(tptrack(tpc(itpct)).id).ipnt(1)
                  else
                     write (errmsg,'(A60)')
     +                'EGR_REFITTER-W: lasthit trk id error'
                     call message(errmsg,1,imsg4)
                  endif

                  lastsec = tphit(lasthit).row/100
                  lastrow = tphit(lasthit).row - lastsec*100
                  globtrk(igtrk).last_row = lastrow
                  globtrk(igtrk).xlast(1) = tphit(lasthit).x
                  globtrk(igtrk).xlast(2) = tphit(lasthit).y
                  globtrk(igtrk).xlast(3) = tphit(lasthit).z
                  lambda = atan(globtrk(igtrk).tanl)
                  globtrk(igtrk).length = (globtrk(igtrk).xlast(3)
     +                 -globtrk(igtrk).z0/sin(lambda))
                  if (globtrk(igtrk).length.gt.999.9) then
                     globtrk(igtrk).length=999.9
                  endif

c     mark spts as used

               do ient = 1, gtrk(itpct).ntpc
                  if( gtrk(itpct).det(ient) .eq. 1) then
                     ithit = gtrk(itpct).ipnt(ient)
                     tphit(ithit).id_globtrk = (igtrk)*1000
     +                    +ient
                  endif
               enddo

               ient = gtrk(itpct).ntpc
               do ishit = 1, svt_group_h.nok
                  if( svt_group(ishit).id1 .eq. 
     +                 svt_track(svt(isvtt)).id) then
                     
                     ient = ient+1
                     ithit = svt_group(ishit).id2
                     svt_spt(ithit).id_globtrk = 
     +                    (igtrk)*1000+ient
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
     +           tptrack(itrk).flag .gt. 0)    then
               
               if( egrpar(1).usetpc .gt. 1) then
                  
                  iret = egr_refit_track(tptrack(itrk).id,
     +                 tptrack(itrk).id,isvtt,ipriv,gtrk,tphit_h,
     +                 tphit,privert_h,privert,svt_spt_h,svt_spt,
     +                 svt_track_h,svt_track,svt_group_h,svt_group,
     +                 egrpar_h,egrpar,evt_match_h,evt_match,covar)
                  iret = egr_load_globtrk(globtrk_h,globtrk,
     +                 tptrack(itrk).id,gtrk,tphit_h,tphit,covar,itrk)
               else
                  gtrk(itrk).nfit = tptrack(itrk).nfit
                  globtrk_h.nok = globtrk_h.nok + 1
                  iglob = globtrk_h.nok
                  globtrk(iglob).icharge = tptrack(itrk).q
                  globtrk(iglob).id = iglob
                  globtrk(iglob).ndegf = tptrack(itrk).nfit
                  globtrk(iglob).sflag = (sign(1.0,float
     +                 (tptrack(itrk).flag)))*100
     +                 +tptrack(itrk).flag
                  if( abs(globtrk(iglob).icharge) .ne. 1) then
                     globtrk(iglob).sflag = -1.0*globtrk(iglob).sflag+20
                  endif
                  globtrk(iglob).chisq = tptrack(itrk).chisq(1)
                  do icov = 1,15
                     globtrk(iglob).cov(icov) = tptrack(itrk).cov(icov)
                  enddo
                  globtrk(iglob).cov(10) = tptrack(itrk).nrec
                  globtrk(iglob).invpt = tptrack(itrk).invp
                  globtrk(iglob).phi0 = tptrack(itrk).phi0
                  globtrk(iglob).psi = tptrack(itrk).psi
                  globtrk(iglob).r0 = tptrack(itrk).r0
                  globtrk(iglob).tanl = tptrack(itrk).tanl
                  globtrk(iglob).z0 = tptrack(itrk).z0
                  globtrk(iglob).id_emc = 0
                  globtrk(iglob).id_hypo_pid = 0
                  globtrk(iglob).id_global_pid = 0
                  globtrk(iglob).id_part = 0
                  globtrk(iglob).id_pver = 0
                  globtrk(iglob).id_tof = 0
C     Now find last point on track and calculate track length.
                  if (tptrack(itrk).id.gt.0.and.
     +                 tptrack(itrk).id.lt.mxtrack) then
                     lasthit = gtrk(tptrack(itrk).id).ipnt(1)
                  else
                     write (errmsg,'(A60)')
     +                    'EGR_REFITTER-W: lasthit track id error'
                     call message(errmsg,1,imsg4)
                  endif
                  lastsec = tphit(lasthit).row/100
                  lastrow = tphit(lasthit).row - lastsec*100
                  globtrk(iglob).last_row = lastrow
                  globtrk(iglob).xlast(1) = tphit(lasthit).x
                  globtrk(iglob).xlast(2) = tphit(lasthit).y
                  globtrk(iglob).xlast(3) = tphit(lasthit).z
                  lambda = atan(globtrk(iglob).tanl)
                  globtrk(iglob).length = (globtrk(iglob).xlast(3)
     +                 -globtrk(iglob).z0/sin(lambda))
                  if (globtrk(iglob).length.gt.999.9) then
                     globtrk(iglob).length=999.9
                  endif
               endif            ! End of egrpar(1).perfect .ne. 1
               do ient = 1, gtrk(itrk).nfit
                  if( ient .le .mxpnt_tpc) then
                     ithit = gtrk(itrk).ipnt(ient)
                     tphit(ithit).id_globtrk = (globtrk_h.nok)*1000+ient
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
                     iglob = globtrk_h.nok
                     do ishit = 1,svt_group_h.nok
                        if( svt_group(ishit).id1 .eq.
     +                       svt_track(itrk).id) then
                           ient = svt_group(ishit).id2
                           svt_spt(ient).id_globtrk = 
     +                          (iglob)*1000+ishit
                        endif
                     enddo
                     globtrk(iglob).icharge = 
     +                    sign(1.0,svt_track(itrk).invpt)
                     globtrk(iglob).id = iglob
                     globtrk(iglob).ndegf = svt_track(itrk).nspt
                     globtrk(iglob).sflag = (sign(1.0,float(
     +                    svt_track(itrk).flag)))*200.
     +                    +svt_track(itrk).flag
                     globtrk(iglob).chisq = svt_track(itrk).chisq(1)
                     do icov = 1,15
                        globtrk(iglob).cov(icov) = 
     +                       svt_track(itrk).cov(icov)
                     enddo
                     globtrk(iglob).cov(10) = svt_track(itrk).nspt
                     globtrk(iglob).invpt = abs(svt_track(itrk).invpt)
                     globtrk(iglob).phi0 = svt_track(itrk).phi0
                     globtrk(iglob).psi = svt_track(itrk).psi
                     globtrk(iglob).r0 = svt_track(itrk).r0
                     globtrk(iglob).tanl = svt_track(itrk).tanl
                     globtrk(iglob).z0 = svt_track(itrk).z0
                     globtrk(iglob).last_row = 0
                     globtrk(iglob).length = 0
                     globtrk(iglob).xlast(1) = 0.0
                     globtrk(iglob).xlast(2) = 0.0
                     globtrk(iglob).xlast(3) = 0.0
                     globtrk(iglob).id_emc = 0
                     globtrk(iglob).id_hypo_pid = 0
                     globtrk(iglob).id_global_pid = 0
                     globtrk(iglob).id_part = 0
                     globtrk(iglob).id_pver = 0
                     globtrk(iglob).id_tof = 0
                  else
                     igtrk = igtrk + 1
                     iret = egr_refit_track(igtrk,itpct,itrk,ipriv,
     +                    gtrk,tphit_h,tphit,privert_h,privert,
     +                    svt_spt_h,svt_spt,svt_track_h,svt_track,
     +                    svt_group_h,svt_group,egrpar_h,egrpar,
     +                    evt_match_h,evt_match,covar)
                     
                     iret = egr_load_globtrk(globtrk_h,globtrk,igtrk
     +                    ,gtrk,tphit_h,tphit,covar,itpct)

                     do ishit = 1, gtrk(igtrk).nfit
                        ient = gtrk(igtrk).ipnt(ishit)
                        svt_spt(ient).id_globtrk = 
     +                       (globtrk_h.nok*1000)+ishit
                     enddo
  
                  endif         ! End of if usesvt=1

                  svt_track(itrk).id_globtrk = globtrk_h.nok

               endif            ! End of if egrpar(1).svtchicut
            endif               ! End of if svt_track(itrk).id_globtrk
         enddo                  ! End of do 1,svt_track_h.nok
      endif                     ! End of if egrpar(1).usesvt .gt. 0
      
Cvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
C     This section handles scenarios where egr is first run without fitting
C     the vertex point and then run with fitting the vertex point on the
C     second pass.  This situation is specified with egrpar(1).usevert=2.
      
      if (egrpar(1).usevert.eq.2) egrpar(1).usevert=-2
      
Cvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
      
      egr_fitter = STAFCV_OK
      
      return
      end
      




