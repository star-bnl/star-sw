      INTEGER*4 FUNCTION EGR_FITTER(
     1             tphit_h,             tphit ,
     2           privert_h,           privert ,
     3           tptrack_h,           tptrack ,
     3         tpc_group_h,         tpc_group ,
     4           svt_spt_h,           svt_spt ,
     5            egrpar_h,            egrpar ,
     6         svt_track_h,         svt_track ,
     7         svt_group_h,         svt_group ,
     8         evt_match_h,         evt_match , 
     9           globtrk_h,           globtrk) 
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
C ***  9/9/97 -  CAINES - Updated the SVT track-hit  identification   **
C ***  12/2/97 - CAINES - Added message routine as official one now  **
C ***                     unmanaged                                  **
C ***  6/5/98 -  CAINES - Changed globtrk to dst_track and dst_track_aux*
C ***  9/29/99 - CAINES - Removed scenario setting,now just usetpc etc *
C ***  2/18/00 - CAINES - Removed MC abilities                         *
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
C<----------------------------------------------------------------------
      IMPLICIT NONE
#include "egr_fitter.inc"
#include "egr_track_pointers.inc"
#include "math_constants.inc"
#include "phys_constants.inc"
#include "StDetectorId.inc"
#include "StTrackMethod.inc"

C ---------------------------------------------------------------------

      RECORD  /track_pointers/ gtrk(1)

      INTEGER i,j,jj,iret,itrk,ient,imatch
      INTEGER igtrk,itpct,isvtt,ipriv,iglob,ishit,ithit
      INTEGER lasthit,ifirst
      INTEGER*4 egr_refit_track,egr_load_globtrk
      INTEGER*4 egr_refit_track_kal, egr_load_globtrk_kal
      INTEGER svt(1),tpc(1),mxtrack
      INTEGER malloc
      INTEGER imsg,imsgw
      INTEGER imsg1,imsg2,imsg3,imsg4,imsg5,imsg6,imsg7,imsg8
      INTEGER nqwe,locf,ifull
      CHARACTER*132 errmsg,msg
      REAL rfirst,rlast,r
      double precision param(5),covar(5,5)
      real x(3),bfield(3),cov(3,3)
      DATA imsg,imsgw,imsg1,imsg2,imsg3 /5*0/
      DATA imsg4,imsg5,imsg6,imsg7,imsg8 /5*0/

C--->>>>>>>>>> 
      REAL gseed, gtanl, ginvpt, glength

C ---------------------------------------------------------------------


      pointer (gtrk_p, gtrk)
      pointer (svt_p, svt)
      pointer (tpc_p,tpc)

      write(msg,'(A30)') ' EGR_REFITTER-I: started. '
      call message(msg,1,imsg)

C  Initialize number of refit tracks

      globtrk_h.nok = 0
c      write(6,*) 'EGR: ',tphit_h.nok,tptrack_h.nok,globtrk_h.nok

CCC  Initialize the local track-pointer array structure to zero:
CCC  L. Ray, 9/16/98

      write(6,997) 
997   format(2x,'Initializing gtrk in egr_fitter.F')


      mxtrack = svt_track_h.nok + tptrack_h.nok
      i = abs(loc(gtrk(2))-loc(gtrk(1)))
      gtrk_p = malloc(mxtrack*i)
      call vzero(gtrk(1),(mxtrack*i)/4)
      svt_p = malloc(mxtrack*4)
      tpc_p = malloc(mxtrack*4)

      x(1) = 0
      x(2) = 0.
      x(3) = 0.
      call gufld(x,bfield)

      write(6,*) 'Using a field of',bfield(3),' KGauss'  


C Set whether to use primary vertex in fit or not

      if (egrpar(1).usevert.eq.1.or.egrpar(1).usevert.eq.-2) then
         ipriv=1
      else
         ipriv = 0
      endif
C *************************************************************************
C  Fill the gtrk refitting array here.
C  First work on the TPC hit information.  Set up hit assignments.
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
C** Spiros put this after Helen suggested it!
            gtrk(tptrack(i).id).nmax = tptrack(i).npos
C**
            tpc(tptrack(i).id) = i
*--- reset foreign keys that are set here SM ---------*

            tptrack(i).id_globtrk = -1*tptrack(i).id_globtrk
            
         end do
         do ithit=1,tpc_group_h.nok ! Loop over number of hits
            itrk = tpc_group(ithit).id1/1000
            if (itrk.ge.1.and.itrk.le.tptrack_h.nok) then !!!*VP*
               gtrk(itrk).nhit = gtrk(itrk).nhit + 1
               gtrk(itrk).ntpc = gtrk(itrk).ntpc + 1
               ient = tpc_group(ithit).id1 - itrk*1000
               if (ient.ge.1.and.ient.le.mxpnt_tpc) then  
                  if ( gtrk(itrk).nhit .lt. mxpnt_tpc) then !!*VP*
                     gtrk(itrk).pos(ient) = ient
                     gtrk(itrk).det(ient) = 1
                     if( tpc_group(ithit).ident .eq. 0) then     
                        gtrk(itrk).ipnt(ient) = tpc_group(ithit).id2
                     else
                        gtrk(itrk).ipnt(ient) = 0
                     endif
                  endif
                  
               else
                  if( ient .eq. mxpnt_tpc+1) then 
                     write (errmsg,'(A45,I5)')
     +                    ' EGR_REFITTER-W: Too many hits on track', itrk
                     call message(errmsg,1,imsg2)
                  endif
               endif
            else
*     VP*           if (itrk.gt.mxtrack) then
               if (itrk.gt.tptrack_h.nok) then !!*VP*
                  write (errmsg,'(A45,I8)')
     +                 ' EGR_REFITTER-W: track id out of range',
     +                 tpc_group(ithit).id1
                  call message(errmsg,1,imsg3)
               endif
            endif
         enddo                  ! Loop over ithit
      endif                     ! usetpc.gt.0
         
    
      do i=1,svt_track_h.nok
         if( svt_track(i).id .le. mxtrack) then
            svt(svt_track(i).id) = i
         endif
         svt_track(i).id_globtrk = -1*svt_track(i).id_globtrk
      enddo
      
         
      if( egrpar(1).useglobal .gt. 1) then ! Use svms matching
         
         do imatch=1,evt_match_h.nok ! Loop over evt_match table
            itpct = evt_match(imatch).idtpc
            isvtt = evt_match(imatch).idsvt
            if( egrpar(1).useglobal .eq. 2) then ! Standard refit
               
               igtrk = itpct  
               
               iret = egr_refit_track(igtrk,itpct,svt(isvtt),ipriv,gtrk,
     +              tphit_h,tphit,privert_h,privert,svt_spt_h,svt_spt,
     +              svt_track_h,svt_track,svt_group_h,svt_group,
     +              egrpar_h,egrpar,evt_match_h,evt_match,cov)
               
               iret = egr_load_globtrk(globtrk_h,globtrk,
     +              igtrk,gtrk,tphit_h,tphit,svt_spt_h,
     +              svt_spt,cov,itpct,isvtt,ipriv,bfield(3))
               
c     mark spts as used if not using primary vertex
               do ient = 1, gtrk(itpct).nhit
                  ithit = gtrk(itpct).ipnt(ient)
                  if( gtrk(itpct).det(ient) .eq. 1 .and. ithit .ne. 0) then
                     
                     if( gtrk(itpct).pos(ient) .lt. 0) then
                        tpc_group(ithit).ident = -1
                     endif
                     if( ipriv .eq. 0) then
                        tphit(ithit).id_globtrk = globtrk_h.nok
                     endif
                  elseif( gtrk(itpct).det(ient) .eq. 2 .and. ithit .ne. 0)  then  
                     if( gtrk(itpct).pos(ient) .lt. 0) then
                        
                        do ishit = 1, svt_group_h.nok
                           if( svt_group(ishit).id2 .eq. 
     +                          svt_spt(gtrk(itpct).ipnt(ient)).id) then
                              svt_group(ishit).ident=-1
                           endif
                        enddo
                     endif
                     if( ipriv .eq.0 ) then
                        svt_spt(gtrk(itpct).ipnt(ient)).id_globtrk =  
     +                       globtrk_h.nok
                     endif
                     if( gtrk(itpct).pos(ient) .gt. 0) then
                        globtrk(globtrk_h.nok).n_fit_point = 
     +                       globtrk(globtrk_h.nok).n_fit_point-1+1000
                     endif
                     if(svt_spt(gtrk(itpct).ipnt(ient)).id_wafer 
     +                    .gt.7000) then
                        globtrk(globtrk_h.nok).n_point = 
     +                       globtrk(globtrk_h.nok).n_point-1000+10000
                        if( gtrk(itpct).pos(ient) .gt. 0) then
                           globtrk(globtrk_h.nok).n_fit_point = 
     +                          globtrk(globtrk_h.nok).n_fit_point-1000+
     +                          10000
                        endif
                     endif
                     
                  endif
               enddo
            elseif( egrpar(1).useglobal.eq.4) then ! Kalman refit
               igtrk = itpct
               
               param(1) = svt_track(svt(isvtt)).phi0*C_RAD_PER_DEG
               param(2) = svt_track(svt(isvtt)).z0
               param(3) = svt_track(svt(isvtt)).psi*C_RAD_PER_DEG 
               param(4) = svt_track(svt(isvtt)).tanl
               param(5) = svt_track(svt(isvtt)).invpt
               
               
               iret = egr_refit_track_kal(param, igtrk,itpct,svt(isvtt),
     +              ipriv,gtrk,
     +              tphit_h,tphit,privert_h,privert,svt_spt_h,svt_spt,
     +              svt_track_h,svt_track,svt_group_h,svt_group,
     +              egrpar_h,egrpar,evt_match_h,evt_match,covar)
               
               iret = egr_load_globtrk_kal(globtrk_h,globtrk
     +              ,igtrk,gtrk,tphit_h,
     +              tphit,svt_spt_h,svt_spt,covar,itpct,isvtt,
     +              ipriv,bfield(3))
               
c     mark spts as used if not using primary vertex
               do ient = 1, gtrk(itpct).nhit
                  ithit = gtrk(itpct).ipnt(ient)
                  if( gtrk(itpct).det(ient) .eq. 1 .and. ithit .ne. 0) then
                     
                     if( gtrk(itpct).pos(ient) .lt. 0) then
                        tpc_group(ithit).ident = -1
                     endif
                     if( ipriv .eq. 0) then
                        tphit(ithit).id_globtrk = globtrk_h.nok
                     endif
                  elseif( gtrk(itpct).det(ient) .eq. 2 .and. ithit .ne. 0)  then 
                     if( gtrk(itpct).pos(ient) .lt. 0) then
                        
                        do ishit = 1, svt_group_h.nok
                           if( svt_group(ishit).id2 .eq. 
     +                          svt_spt(gtrk(itpct).ipnt(ient)).id) then
                              svt_group(ishit).ident=-1
                           endif
                        enddo
                     endif
                     if( ipriv .eq. 0) then
                        svt_spt(gtrk(itpct).ipnt(ient)).id_globtrk =  
     +                       globtrk_h.nok
                     endif
                     if( gtrk(itpct).pos(ient) .gt. 0) then
                        globtrk(globtrk_h.nok).n_fit_point = 
     +                       globtrk(globtrk_h.nok).n_fit_point-1+1000
                     endif
                     if(svt_spt(gtrk(itpct).ipnt(ient)).id_wafer 
     +                    .gt.7000) then
                        globtrk(globtrk_h.nok).n_point = 
     +                       globtrk(globtrk_h.nok).n_point-1000+10000
                        if( gtrk(itpct).pos(ient) .gt. 0) then
                           globtrk(globtrk_h.nok).n_fit_point = 
     +                          globtrk(globtrk_h.nok).n_fit_point-1000
     +                          +10000
                        endif
                     endif
                  endif
               enddo 
            endif
         
         
         if( ipriv .eq. 0) then
            tptrack(tpc(itpct)).id_globtrk = globtrk_h.nok
            svt_track(svt(isvtt)).id_globtrk = globtrk_h.nok
         else
            tptrack(tpc(itpct)).id_globtrk =
     +           -1*tptrack(tpc(itpct)).id_globtrk
         endif
         
      enddo                     ! imatch
      endif                     ! if useglobal>0
      
C     Loop through unmatched TPC tracks here.  Straight copy of tptrack.
      
      if( egrpar(1).usetpc .gt. 0) then
         isvtt = 0
         do itrk = 1,tptrack_h.nok

            if( tptrack(itrk).id_globtrk.le.0 .and.
     +           tptrack(itrk).invp .ne. 0.0   .and.
     +           tptrack(itrk).flag .gt. 0) then
C     +           tptrack(itrk).nrec .gt. 10 )    then
               
               
               if( egrpar(1).usetpc .eq. 2) then
                  
                  iret = egr_refit_track(tptrack(itrk).id,
     +                 tptrack(itrk).id,isvtt,ipriv,gtrk,tphit_h,
     +                 tphit,privert_h,privert,svt_spt_h,svt_spt,
     +                 svt_track_h,svt_track,svt_group_h,svt_group,
     +                 egrpar_h,egrpar,evt_match_h,evt_match,cov)
                  iret = egr_load_globtrk(globtrk_h,globtrk
     +                 ,tptrack(itrk).id,gtrk
     +                 ,tphit_h,tphit,svt_spt_h,svt_spt,cov,itrk
     +                 ,isvtt,ipriv,bfield(3))

               elseif( egrpar(1).usetpc .eq. 4) then

                    param(1) = tptrack(itrk).phi0*C_RAD_PER_DEG
                    param(2) = tptrack(itrk).z0
                    param(3) = tptrack(itrk).psi*C_RAD_PER_DEG 
                    param(4) = tptrack(itrk).tanl
                    param(5) = tptrack(itrk).invp*tptrack(itrk).q

                  iret = egr_refit_track_kal(param,tptrack(itrk).id,
     +                 tptrack(itrk).id,isvtt,ipriv,gtrk,tphit_h,
     +                 tphit,privert_h,privert,svt_spt_h,svt_spt,
     +                 svt_track_h,svt_track,svt_group_h,svt_group,
     +                 egrpar_h,egrpar,evt_match_h,evt_match,covar)
                  iret = egr_load_globtrk_kal(globtrk_h,globtrk,
     +                 tptrack(itrk).id,gtrk
     +                 ,tphit_h,tphit,svt_spt_h,svt_spt,covar,itrk,isvtt,
     +                 ipriv,bfield(3))
               else


                  gtrk(itrk).nfit = tptrack(itrk).nfit
		  ifull = 1
		  if (globtrk_h.nok .ge. globtrk_h.maxlen) go to 1313
                  globtrk_h.nok = globtrk_h.nok + 1
		  ifull = 2


                  iglob = globtrk_h.nok
                  globtrk(iglob).id = iglob
                  globtrk(iglob).iflag = (sign(1.0,float
     +                 (tptrack(itrk).flag)))*100
     +                 +tptrack(itrk).flag

                  globtrk(iglob).det_id = kTpcId
                  globtrk(iglob).n_point = tptrack(itrk).nrec
                  globtrk(iglob).n_max_point = tptrack(itrk).npos
                  globtrk(iglob).n_fit_point = tptrack(itrk).nfit          
                  globtrk(iglob).icharge = tptrack(itrk).q
                  if( abs(globtrk(iglob).icharge) .ne. 1) then
                     globtrk(iglob).iflag = 
     +                    -1.0*abs(globtrk(iglob).iflag+20)
                  endif
                  globtrk(iglob).r0 = tptrack(itrk).r0
                  globtrk(iglob).phi0 = tptrack(itrk).phi0
                  globtrk(iglob).z0 = tptrack(itrk).z0
                  globtrk(iglob).psi = tptrack(itrk).psi
                  globtrk(iglob).tanl = tptrack(itrk).tanl
                  globtrk(iglob).invpt = tptrack(itrk).invp
                  globtrk(iglob).covar(1) = tptrack(itrk).cov(3)
                  globtrk(iglob).covar(2) = 0.
                  globtrk(iglob).covar(3) = 0.
                  globtrk(iglob).covar(4) = 0.
                  globtrk(iglob).covar(5) = 0.
                  globtrk(iglob).covar(6) = tptrack(itrk).cov(15)
                  globtrk(iglob).covar(7) = 0.
                  globtrk(iglob).covar(8) = 0.
                  globtrk(iglob).covar(9) = 0.
                  globtrk(iglob).covar(10) = tptrack(itrk).cov(10)
                  globtrk(iglob).covar(11) = 0.
                  globtrk(iglob).covar(12) = 0.
                  globtrk(iglob).covar(13) = tptrack(itrk).cov(6)
                  globtrk(iglob).covar(14) = 0.
                  globtrk(iglob).covar(15) = tptrack(itrk).cov(1)

                  globtrk(iglob).chisq(1) = tptrack(itrk).chisq(1)/
     +            real(globtrk(iglob).n_fit_point-3)
                  globtrk(iglob).chisq(2) = tptrack(itrk).chisq(2)/
     +                 real(globtrk(iglob).n_fit_point-2)
C     Now find first point on track.
                  if (tptrack(itrk).id.gt.0.and.
     +                 tptrack(itrk).id.le.mxtrack) then
                     ifirst = gtrk(tptrack(itrk).id).ntpc
                     lasthit= ifirst
                     ifirst = 
     +                    gtrk(tptrack(itrk).id).ipnt(ifirst)
                     do while(ifirst .eq.0 .and. lasthit .gt.0)
                        lasthit = lasthit-1
                        ifirst = 
     +                       gtrk(tptrack(itrk).id).ipnt(lasthit)
                    enddo
                  else
                     write (errmsg,'(A60)')
     +                    'EGR_REFITTER-W: lasthit track id error'
                     call message(errmsg,1,imsg4)
                  endif
                  if( ifirst .ne. 0) then
                     globtrk(iglob).x_first(1) = tphit(ifirst).x
                     globtrk(iglob).x_first(2) = tphit(ifirst).y
                     globtrk(iglob).x_first(3) = tphit(ifirst).z
                  else
                     globtrk(iglob).x_first(1) = -999.
                     globtrk(iglob).x_first(2) = -999.
                     globtrk(iglob).x_first(3) = -999.
                  endif
                     


C     Now find last point on track 
                  if (tptrack(itrk).id.gt.0.and.
     +                 tptrack(itrk).id.le.mxtrack) then
                     lasthit = gtrk(tptrack(itrk).id).ipnt(1)
                     ifirst = 1
                     do while( lasthit .eq. 0 .and.
     +                    ifirst .le.  gtrk(tptrack(itrk).id).ntpc)
                        ifirst = ifirst+1
                        lasthit = gtrk(tptrack(itrk).id).ipnt(ifirst)
                     enddo
                     
                 else
                     write (errmsg,'(A60)')
     +                    'EGR_REFITTER-W: lasthit track id error'
                     call message(errmsg,1,imsg4)
                  endif
                  if( lasthit .ne. 0) then
                     globtrk(iglob).x_last(1) = tphit(lasthit).x
                     globtrk(iglob).x_last(2) = tphit(lasthit).y
                     globtrk(iglob).x_last(3) = tphit(lasthit).z
                  else
                     globtrk(iglob).x_last(1) = -999.
                     globtrk(iglob).x_last(2) = -999.
                     globtrk(iglob).x_last(3) = -999.
                  endif
                     

                  globtrk(iglob).length = tptrack(itrk).length
CSM               
                  globtrk(iglob).id_start_vertex= 0
                  globtrk(iglob).curvature = tptrack(itrk).curvature
                  globtrk(iglob).method = 2**8+2**kHelix2StepId
                  globtrk(iglob).map(1) = 0
                  globtrk(iglob).map(2) = 0
                  globtrk(iglob).pid = 0


               endif            ! End of what type of tpc copying to do
               

               do ient = 1, gtrk(itrk).ntpc
                  if( ient .le .mxpnt_tpc) then
                     ithit = gtrk(itrk).ipnt(ient)
                     if (ithit.ne.-999 .and. ithit .ne. 0) then
                        if( gtrk(itrk).pos(ient) .lt. 0) then
                           tpc_group(ithit).ident = -1
                        else if( ipriv .eq. 0) then
                           tphit(ithit).id_globtrk 
     +                          = globtrk_h.nok
                        endif
                     endif
                  endif
               enddo
               if( ipriv .eq. 0) then
                  tptrack(itrk).id_globtrk = globtrk_h.nok
               else
                  tptrack(itrk).id_globtrk = -1*tptrack(itrk).id_globtrk 
               endif
            endif               ! End of test if good tpc track
         enddo                  ! End of tpc track loop
      endif                     ! End of if egrpar(1).usetpc .eq. 1
      
C     Now loop over unmatched SVT tracks.  Straight svt_track copy.
      
      if( egrpar(1).usesvt .gt. 0 ) then
         itpct = 0
         igtrk = tptrack_h.nok
         do itrk = 1,svt_track_h.nok
            if (svt_track(itrk).id_globtrk.le.0) then
               if (egrpar(1).svtchicut.eq.0.or.
     +              svt_track(itrk).chisq(1).le.
     +              egrpar(1).svtchicut) then ! only copy good SVT tracks
                  if( egrpar(1).usesvt .eq. 1) then

		     ifull = 1
		     if (globtrk_h.nok .ge. globtrk_h.maxlen) go to 1313
                     globtrk_h.nok = globtrk_h.nok + 1
		     ifull = 2
                     iglob = globtrk_h.nok
                     rlast = 0.
                     globtrk(iglob).det_id = kSvtIdentifier
C     Adjust for ssd later
                     globtrk(iglob).n_point = 1000*svt_track(itrk).nspt
                     globtrk(iglob).n_max_point = 0
                     globtrk(iglob).n_fit_point =  1000*svt_track(itrk).nspt
                     rfirst= 10000.

                     do ishit = 1,svt_group_h.nok
                        if( svt_group(ishit).id1 .eq.
     +                       svt_track(itrk).id) then
                           ient = svt_group(ishit).id2

                           if( svt_spt(ient).id_wafer .gt. 7000 ) then
                              globtrk(iglob).det_id = kSsdSvtIdentifier
                              globtrk(iglob).n_point =
     +                         globtrk(iglob).n_point-1000+10000
                              globtrk(iglob).n_fit_point = 
     +                         globtrk(iglob).n_fit_point-1000+10000
                           endif
                           if( ipriv .eq. 0) then
                              svt_spt(ient).id_globtrk = iglob
                           endif
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
                     globtrk(iglob).icharge = 
     +                    sign(1.0,svt_track(itrk).invpt)
                     globtrk(iglob).r0 = svt_track(itrk).r0
                     globtrk(iglob).phi0 = svt_track(itrk).phi0
                     globtrk(iglob).z0 = svt_track(itrk).z0
                     globtrk(iglob).psi = svt_track(itrk).psi
                     globtrk(iglob).tanl = svt_track(itrk).tanl
                     globtrk(iglob).invpt = abs(svt_track(itrk).invpt)
                     globtrk(iglob).covar(1) = 
     +                       svt_track(itrk).cov(1)
                    globtrk(iglob).covar(2) = 
     +                       svt_track(itrk).cov(2)
                    globtrk(iglob).covar(3) = 
     +                       svt_track(itrk).cov(3)
                    globtrk(iglob).covar(4) = 
     +                       svt_track(itrk).cov(4)
                    globtrk(iglob).covar(5) = 
     +                       svt_track(itrk).cov(5)
                    globtrk(iglob).covar(6) = 
     +                       svt_track(itrk).cov(6)
                    globtrk(iglob).covar(7) = 
     +                       svt_track(itrk).cov(7)
                    globtrk(iglob).covar(8) = 
     +                       svt_track(itrk).cov(8)
                    globtrk(iglob).covar(9) = 
     +                       svt_track(itrk).cov(9)
                    globtrk(iglob).covar(10) = 
     +                       svt_track(itrk).cov(10)
                    globtrk(iglob).covar(11) = 
     +                       svt_track(itrk).cov(11)
                    globtrk(iglob).covar(12) = 
     +                       svt_track(itrk).cov(12)
                    globtrk(iglob).covar(13) = 
     +                       svt_track(itrk).cov(13)
                    globtrk(iglob).covar(14) = 
     +                       svt_track(itrk).cov(14)
                    globtrk(iglob).covar(15) = 
     +                       svt_track(itrk).cov(15)

                    globtrk(iglob).chisq(1) = svt_track(itrk).chisq(1)
                    globtrk(iglob).chisq(2) = svt_track(itrk).chisq(2)
                    globtrk(iglob).x_first(1) = svt_spt(ifirst).x(1)
                    globtrk(iglob).x_first(2) = svt_spt(ifirst).x(2)
                    globtrk(iglob).x_first(3) = svt_spt(ifirst).x(3)
                    globtrk(iglob).x_last(1) = svt_spt(lasthit).x(1)
                    globtrk(iglob).x_last(2) = svt_spt(lasthit).x(2)
                    globtrk(iglob).x_last(3) = svt_spt(lasthit).x(3)
C----->>>>  new calculation of .length   S.Margetis
                  gseed = sqrt((globtrk(iglob).x_last(1) 
     +                - globtrk(iglob).x_first(1))**2 +
     +                 (globtrk(iglob).x_last(2) 
     +                - globtrk(iglob).x_first(2))**2)

                  gtanl = globtrk(iglob).tanl
                  ginvpt= globtrk(iglob).invpt
            call egr_trk_length(gseed,gtanl,ginvpt,bfield(3),glength)
                  globtrk(iglob).length=glength
C---<<<<<<<
                  if (globtrk(iglob).length.gt.99999.9) then
                     globtrk(iglob).length=99999.9
                  endif
                    globtrk(iglob).impact = 0
CSM                    globtrk(iglob).id_start_vertex = 0
                    globtrk(iglob).curvature = abs(C_D_CURVATURE*bfield(3)*
     +                   globtrk(iglob).invpt)
                    globtrk(iglob).method = 2**kHelix2StepId+2**7
                    globtrk(iglob).map(1) = 0
                    globtrk(iglob).map(2) = 0
                    globtrk(iglob).pid = 0

                    
                 elseif( egrpar(1).usesvt .eq.2)then
                    igtrk = igtrk + 1
                    iret = egr_refit_track(igtrk,itpct,itrk,ipriv,
     +                   gtrk,tphit_h,tphit,privert_h,privert,
     +                   svt_spt_h,svt_spt,svt_track_h,svt_track,
     +                   svt_group_h,svt_group,egrpar_h,egrpar,
     +                   evt_match_h,evt_match,cov)
                    
                    iret = egr_load_globtrk(globtrk_h,globtrk,
     +                   igtrk,gtrk,tphit_h,
     +                   tphit,svt_spt_h,svt_spt,cov,itpct,isvtt
     +                   ,ipriv,bfield(3))

                    if( ipriv .eq.0) then
                       do ishit = 1, gtrk(igtrk).nhit
                          if( gtrk(igtrk).det(ishit) .eq. 2) then
                             ient = gtrk(igtrk).ipnt(ishit)
                             if( ient .ne. 0) then
                                svt_spt(ient).id_globtrk = globtrk_h.nok
                             endif
                          endif
                       enddo
                    endif
                 elseIF( egrpar(1).usesvt .eq. 4) then
                    igtrk = igtrk + 1

                    param(1) = svt_track(itrk).phi0*C_RAD_PER_DEG
                    param(2) = svt_track(itrk).z0
                    param(3) = svt_track(itrk).psi*C_RAD_PER_DEG
                    param(4) = svt_track(itrk).tanl
                    param(5) = svt_track(itrk).invpt

                    iret = egr_refit_track_kal(param,igtrk,itpct,itrk,ipriv,
     +                   gtrk,tphit_h,tphit,privert_h,privert,
     +                   svt_spt_h,svt_spt,svt_track_h,svt_track,
     +                   svt_group_h,svt_group,egrpar_h,egrpar,
     +                   evt_match_h,evt_match,covar)
                    
                    iret = egr_load_globtrk_kal(globtrk_h,globtrk,
     +                   igtrk,gtrk,tphit_h,
     +                   tphit,svt_spt_h,svt_spt,covar,itpct,isvtt,ipriv,
     +                   bfield(3))
                    
                    do ishit = 1, gtrk(igtrk).nhit
                       if( gtrk(igtrk).det(ishit) .eq. 2) then
                          ient = gtrk(igtrk).ipnt(ishit)
                          if( ient .ne. 0) then
                             if( ipriv .eq. 0 ) then
                                svt_spt(ient).id_globtrk = globtrk_h.nok
                             endif
                             if( gtrk(igtrk).pos(ishit) .gt. 0) then
                                globtrk(globtrk_h.nok).n_fit_point = 
     +                               globtrk(globtrk_h.nok).n_fit_point-1+1000
                             else 
                                do ient=1, svt_group_h.nok
                                   
                                   if( svt_group(ient).id2 .eq. 
     +                                  svt_spt(gtrk(igtrk).ipnt(ishit)).id) then
                                      svt_group(ient).ident=-1
                                   endif
                                enddo
                             endif
                             if( svt_spt(ient).id_wafer .gt. 7000) then
                                globtrk(globtrk_h.nok).n_point = 
     +                               globtrk(globtrk_h.nok).n_point-1000+10000
                                if( gtrk(igtrk).pos(ishit) .gt. 0) then
                                   globtrk(globtrk_h.nok).n_fit_point = 
     +                                  globtrk(globtrk_h.nok).n_fit_point-1000+10000
                                endif
                             endif
                          endif
                       endif
                    enddo
                    
                 endif          ! End of if usesvt=1
                 
                 

               if( ipriv .eq.0) then
                 svt_track(itrk).id_globtrk = globtrk_h.nok
              else
                svt_track(itrk).id_globtrk = -1*svt_track(itrk).id_globtrk
              endif
                 
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
            if(globtrk(i).invpt .ge. -999999. .and. 
     +         globtrk(i).invpt .le. 999999.) then
               continue 
            else 
C     write(6,998) i
C 998  Format(5X,'BAD Value of globtrk(i=',I7,').invpt detected - set to -999999')
      globtrk(i).invpt = -999999.
CSM preserve the detector id information. Instead of -999 put -x99. Spiros
CSM        globtrk(i).iflag = -999
      globtrk(i).iflag = -1.*(abs(globtrk(i).iflag/100)*100+99)
            end if  
        end do
      end if
             
      egr_fitter = STAFCV_OK

 999  call free(gtrk_p)    
      call free(svt_p)
      call free(tpc_p)

      
      return
1313  write(*,*) '***Error*** GLOBTRK(',globtrk_h.maxlen,') OVERFULL'
      egr_fitter = STAFCV_BAD
      goto 999
      end
      
