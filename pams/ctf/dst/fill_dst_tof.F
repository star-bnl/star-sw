c
c	WJL, feb-11-99
c
      INTEGER*4 FUNCTION FILL_DST_TOF(
     1       g2t_ctf_hit_h,       g2t_ctf_hit ,
     1         g2t_track_h,         g2t_track ,
     3         tpt_track_h,         tpt_track ,
     4         tte_mctrk_h,         tte_mctrk ,
     5        dst_vertex_h,        dst_vertex ,
     5           ctg_geo_h,           ctg_geo ,
     5          ctg_slat_h,          ctg_slat ,
     5      ctg_slat_phi_h,      ctg_slat_phi ,
     5      ctg_slat_eta_h,      ctg_slat_eta ,
     6         cts_mpara_h,         cts_mpara ,
     6         cts_mslat_h,         cts_mslat ,
     9       dst_tof_trk_h,       dst_tof_trk ,
     9       dst_tof_evt_h,       dst_tof_evt )
      IMPLICIT NONE
#include "fill_dst_tof.inc"
      LOGICAL   first_call/.TRUE./
      INTEGER   itptrk2ttetrk(0:15000)
      INTEGER   ig2ttrk2ctfhit(0:50000)
      INTEGER   ietaphi2mslat(20,300)
      INTEGER   nopoint_ttetrk,nopoint_g2ttrk,nopoint_ctfhit
      INTEGER   itptrk,imctrk,igetrk,ittetrk,ig2ttrk,ictfhit,imslat
      INTEGER   i,j,k,l,m,n,icall,itrk,itr,ieb,ll,idet
      INTEGER   iout,iout1,iout2,iout3,iout4,iout5
      INTEGER   eta_bin,tray_bin,slat_bin,int_yn,ivtx
      INTEGER   n_exttrk,n_toftrk,trk_npts_min,i_eta,kk,kk_eta
      REAL      b_field,deg_to_rad,pi,rad_to_deg
      REAL      p_input(6),p_gen,p_trk,p_tof,dx,dy,dr,phi1,phi2
      REAL      trk_qual,trk_qual_cut,trkchar,rval,zmin,zmax,pid,c,r,s_trk
      REAL      xc,yc,crad,eta_hit,phi_tof,phi_trk
      REAL      ctg_zmin(18),ctg_zmax(18),len,trkz
      REAL      prtramat,prtrkmat,prhitmat
      REAL      pt,pz,p_tot,x_strt,y_strt,cap_phi0,dedx,theta
      REAL      ang_int,td_ang,cosphi_int,z_int,dirvec(3)
      REAL      xprivert(2),p_privert(6),xp(2),xcc(2)
      REAL      xinner(2),xouter(2),drinner
      REAL      xend(2),p_end(6),xtrktof,ytrktof
      REAL      privert_r_cut
      REAL      x_privert,y_privert,z_privert
      INTEGER   Z_to_IETA
      LOGICAL   KEEPIT

c---- start executable statements...
c---- initialize...
      if(first_call)then
       first_call   = .FALSE.
       if(ctg_slat_eta_h.nok.eq.4)then
        write(*,*)' FILL_DST_TOF: the geometry tables say CTB data was passed...'
        write(*,*)' FILL_DST_TOF: this routine handles TOFp data only...'
        goto 1000
       elseif(ctg_slat_eta_h.nok.ne.18)then
        write(*,*)' FILL_DST_TOF: the geometry tables are empty or incorrect...'
        goto 1000
       end if
       icall        = 0
       do i         = 1,18
        ctg_zmin(i) = ctg_slat_eta(i).z_min
        ctg_zmax(i) = ctg_slat_eta(i).z_max
cTEST        write(*,*)i,ctg_zmin(i),ctg_zmax(i)
       end do
      end if       ! end first_call code...
c
      write(*,*)' FILL_DST_TOF: ~~~~~~~~~~~~ Starting ~~~~~~~~~~~~~~'
      icall = icall + 1
      do i                 = 0,15000
       itptrk2ttetrk(i)    = 0
      end do
      do i                 = 1,50000
       ig2ttrk2ctfhit(i)   = 0
      end do
      do i                 = 1,300
       do j                = 1,20
        ietaphi2mslat(j,i) = 0
       end do
      end do
      n_exttrk             = 0
      n_toftrk             = 0

      kk            = 0
      iout          = 0
      iout1         = 0
      iout2         = 0
      iout3         = 0
      iout4         = 0
      iout5         = 0
      pi            = ACOS(-1.)
      deg_to_rad    = pi/180.
      rad_to_deg    = 1./deg_to_rad
      zmin          =  13.56  - 2.0 !
      zmax          = 227.346 + 2.0 ! was 241.49
      b_field       = 4.982340      ! should be a parameter...
      trk_qual_cut  = 10.   ! OPEN CUT because data volume so small, and cut vars are in output
      trk_npts_min  = 10    ! OPEN CUT because data volume so small, and cut vars are in output
      privert_r_cut =  4.0  ! OPEN CUT because data volume so small, and cut vars are in output

c---- privert setup
      x_privert = 0.
      y_privert = 0.
      z_privert = 0.
      DO ivtx=1,dst_vertex_h.nok
         IF( (dst_vertex(ivtx).vtx_id .eq. 1).AND.
     +       (dst_vertex(ivtx).iflag  .eq. 1)) THEN
            x_privert = dst_vertex(ivtx).x
            y_privert = dst_vertex(ivtx).y
            z_privert = dst_vertex(ivtx).z
         ENDIF
      ENDDO
      write(*,'(1x,a36,3f9.3)')' FILL_DST_TOF: primary vertex posn =',
     1         x_privert,y_privert,z_privert

c---- set up lookup from tptrack table to tte_mctrk table...
      if(tpt_track_h.nok.gt.15000)then
       write(*,*)' TOFANA -- internal array too small: itptrk2ttetrk'
       goto 1000
      end if
      do ittetrk = 1,tte_mctrk_h.nok
       itptrk    = tte_mctrk(ittetrk).recid
       if(itptrk.gt.0.and.itptrk.le.tpt_track_h.nok)then
        itptrk2ttetrk(itptrk) = ittetrk
       end if
      end do
c---- set up lookup from g2t_track table to g2t_ctf_hit table...
      if(g2t_track_h.nok.gt.50000)then
       write(*,*)' TOFANA -- internal array too small: ig2ttrk2ctfhit'
       goto 1000
      end if
      do ictfhit = 1,g2t_ctf_hit_h.nok
       ig2ttrk   = g2t_ctf_hit(ictfhit).track_p
       if(ig2ttrk.gt.0.and.ig2ttrk.le.g2t_track_h.nok)then
        ig2ttrk2ctfhit(ig2ttrk) = ictfhit
       else
        write(*,*)'pointer in g2t_ctf_hit to row in g2t_track too large',
     1             ictfhit,ig2ttrk,g2t_track_h.nok
       end if
      end do
c---- set up lookup from i_eta and i_phi to cts_mslat table...
      do i       = 1,cts_mslat_h.nok
       j         = cts_mslat(i).i_eta
       k         = cts_mslat(i).i_phi
cTEST        write(*,*)' cts ',cts_mslat(i).i_eta,cts_mslat(i).i_phi,
cTEST     1         cts_mslat(i).z_hit,cts_mslat(i).ptot,cts_mslat(i).s_length
       if((j.ge.1.and.j.le. 18).AND.
     1    (k.ge.1.and.k.le.300))then
        if(k.lt.251.or.k.gt.255)then
         write(*,*)' FILL_DST_TOF: the geometry seen via cts_mslat is inappropriate for mdc2'
         write(*,*)'             : cts_mslat(i).i_phi =',k
         write(*,*)'             : exiting with error...'
         goto 1000
        end if
        if(j.lt.10.or.j.gt.18)then
         write(*,*)' FILL_DST_TOF: the geometry seen via cts_mslat is inappropriate for mdc2'
         write(*,*)'             : cts_mslat(i).i_eta =',j
         write(*,*)'             : exiting with error...'
         goto 1000
        end if
        ietaphi2mslat(j,k) = i   ! get pointer to row in cts_mslat...
       else
        write(*,*)' FILL_DST_TOF: mslat problem -- ',i,j,k
        write(*,*)'             : exiting with error...'
        goto 1000
       end if
      end do
      nopoint_ttetrk = 0
      nopoint_g2ttrk = 0
      nopoint_ctfhit = 0

c---- start loop over reconstructed tracks...
      do itptrk   = 1,tpt_track_h.nok
        KEEPIT    = .FALSE.

c---- get all pointers (don't bother extrapolating otherwise)
        eta_bin  = 0
        tray_bin = 0
        slat_bin = 0
        ittetrk  = -1
        ig2ttrk  = -1
        ictfhit  = -1
        ittetrk  = itptrk2ttetrk(itptrk)
        if(ittetrk.le.0.OR.
     1     ittetrk.gt.tte_mctrk_h.nok)then
         nopoint_ttetrk = nopoint_ttetrk + 1
         goto 201
        end if
        ig2ttrk  = tte_mctrk(ittetrk).mcid
        if(ig2ttrk.le.0.OR.
     1     ig2ttrk.gt.g2t_track_h.nok)then
         nopoint_g2ttrk = nopoint_g2ttrk + 1
         goto 201
        end if

c---- this is nonzero only for those g2t tracks that are associated with tofp hits...
c---- this is only ~10-15 tracks/central venus event
        iout     = iout + 1
        ictfhit  = ig2ttrk2ctfhit(ig2ttrk)
        if(ictfhit.le.0.OR.
     1     ictfhit.gt.g2t_ctf_hit_h.nok)then
         nopoint_ctfhit = nopoint_ctfhit + 1
c         goto 201    ! leave commented to get geometrical efficiency for tracks in acceptance correct!!!
        else 
         iout1   = iout1 + 1
        end if

c---- extrapolate to TOF...
        trk_qual  =       tpt_track(itptrk).chisq(1)
     1            / FLOAT(tpt_track(itptrk).nfit)
        if( trk_qual              .le.trk_qual_cut .AND.
     1      tpt_track(itptrk).nfit.ge.trk_npts_min )then
          n_exttrk= n_exttrk + 1
c---- decent track, so get parameters...
          trkchar = FLOAT(tpt_track(itptrk).q)
          pt      = 1.0/tpt_track(itptrk).invp
          pz      = pt*tpt_track(itptrk).tanl
          p_tot   = sqrt(pt**2 + pz**2)
          crad    = ABS(trkchar)*pt/(0.0003*b_field)
          x_strt  = tpt_track(itptrk).r0
     1            * cos(tpt_track(itptrk).phi0*deg_to_rad)
          y_strt  = tpt_track(itptrk).r0
     1            * sin(tpt_track(itptrk).phi0*deg_to_rad)
          cap_phi0= tpt_track(itptrk).psi*deg_to_rad + trkchar*0.5*pi
          xc      = x_strt - crad*cos(cap_phi0)
          yc      = y_strt - crad*sin(cap_phi0)
          p_input(1) = xc
          p_input(2) = yc
          p_input(3) = tpt_track(itptrk).z0
          p_input(4) = crad
          p_input(5) = tpt_track(itptrk).tanl
          p_input(6) = cap_phi0
c---- extrapolate to z-dependent radius of tofp slats...
          int_yn  = 1
          trkz    = 213.*pz/pt
          kk_eta  = 0
          if(ABS(trkz).gt.zmin.AND.ABS(trkz).lt.zmax)then
           if(trkz.gt.0)then                             ! acceptable ONLY for TOFp design (mdc2)!!!!
            kk_eta  = Z_TO_IETA(ctg_zmin,ctg_zmax,trkz)  !!!! ! estimate i_eta from momenta
           end if
          end if
          if(kk_eta.gt.0)then
           len     = ABS(trkz - ctg_zmin(kk_eta))
           rval    = 214.35   - len*SIND(8.0)
           call EUT_EXTRA_RAD(p_input,rval,trkchar,ang_int,td_ang,
     +                        cosphi_int,z_int,int_yn,dirvec)
           if (ABS(z_int).lt.zmin) goto 201     ! skip track...
           if (ABS(z_int).gt.zmax) goto 201     ! skip track...
          else
           goto 201
          end if
c---- track extrapolated to the TOF radius within the Z-extrema...
           if(ictfhit.gt.0)then
            phi1     = ATAN2(g2t_ctf_hit(ictfhit).x(1),
     1                       g2t_ctf_hit(ictfhit).x(2))
           else 
            phi1     = -99.
           end if
           phi2      = -ang_int
           if(phi2.gt.1.4)phi1 = phi1 + 2.*pi
           phi2      = phi2 + pi/2.
           if(phi1.gt.-90..and.phi1.lt.-pi)then
            phi1     = phi1 + 2.*pi
            phi2     = phi2 + 2.*pi
           end if
           if(phi1.ge.pi)then
            phi1     = phi1 - 2.*pi
            phi2     = phi2 - 2.*pi
           end if
           if(phi2.lt.-pi)phi2 = phi2 + 2.*pi
           if(phi2.ge. pi)phi2 = phi2 - 2.*pi
           phi_tof   = phi1
           phi_trk   = phi2
c---- assign eta,tray,slat bin numbers
           n_toftrk = n_toftrk + 1
c            eta_hit  = -1.0*LOG(TAN(ATAN2(rval,z_int)/2.0))
           eta_bin  = Z_TO_IETA(ctg_zmin,ctg_zmax,z_int)
           tray_bin = IFIX((phi_trk+pi)/ 0.10472    ) + 1
           if(tray_bin.lt.  01)tray_bin = tray_bin + 60
           if(tray_bin.gt.  60)tray_bin = tray_bin - 60
           if(ang_int.ge.-3.*deg_to_rad)then
             slat_bin = IFIX((ang_int+3.*deg_to_rad)/(0.10472/5.)) + 1
           else
             slat_bin = IFIX((ang_int+3.*deg_to_rad)/(0.10472/5.)) + 300
           endif
c!!!!!!!!!!!!!!!!!!!!!
c!!!!!!!!!!!!!!!!!!!!!           slat_bin   = slat_bin + 5  ! needed for geant output made with g2t...
c!!!!!!!!!!!!!!!!!!!!!  unnecesary for g2r data 
c!!!!!!!!!!!!!!!!!!!!!
           if(slat_bin.lt.  01)slat_bin = slat_bin + 300
           if(slat_bin.gt. 300)slat_bin = slat_bin - 300

c---- extrapolate track parameters to primary vertex...
           xcc(1) = xc
           xcc(2) = yc
           xp(1)  = x_privert
           xp(2)  = y_privert
           call EV0_PROJECT_TRACK(xcc,crad,xp,xprivert)

c---- extrapolate track parameters to tof wall...
           xp(1) = (rval)*COS(ang_int)
           xp(2) = (rval)*SIN(ang_int)
           call EV0_PROJECT_TRACK(xcc,crad,xp,xend)

c---- calculate path length...
           xinner(1) = xprivert(1)
           xinner(2) = xprivert(2)
           xouter(1) = xend(1)
           xouter(2) = xend(2)
           drinner   = SQRT(xinner(1)**2+xinner(2)**2)
           if(drinner.lt.privert_r_cut)then          !<--<<< dca cut for "primary-like"
             c       = SQRT((xouter(1)-xinner(1))**2
     1               +      (xouter(2)-xinner(2))**2)
             r       = crad
             theta   = ATAN2(rval,z_int)
             s_trk   = SQRT((2.*r*ASIN(c/2./r))**2 + (z_int-z_privert)**2)
     1               - SIN(theta) + pi/2. + 0.5
             if(s_trk.gt.200.and.             ! <----------<<<<<<<
     1          p_tot.le.5.0)then             ! <----------<<<<<<<
              KEEPIT    = .TRUE.              ! <----------<<<<<<<
             end if                           ! <----------<<<<<<<
           else
            s_trk    = 0.
           end if                             ! end drinner check...

c           if(eta_bin .ge.1.and.eta_bin .le. 18.and.       ! sanity check...
c     1        slat_bin.ge.1.and.slat_bin.le.300)then       ! sanity check...
           iout2     = iout2 + 1
           if(eta_bin .ge. 10.and.eta_bin .le. 18.and.      ! does it extrapolate to TOFp?
     1        slat_bin.ge.251.and.slat_bin.le.255)then      ! does it extrapolate to TOFp?
              imslat = ietaphi2mslat(eta_bin,slat_bin)      ! yes, but does it match a struck slat?
              iout3  = iout3 + 1
              if(imslat.le.0)then
cTEST              write(*,*)'track hits tray but not slat ',eta_bin,slat_bin
               idet  = 0
              else
               iout4 = iout4 + 1 
               idet  = (eta_bin-10)*5 + (slat_bin-250)      ! appropriate mdc2 tofp only!!!
              end if
           else
cTEST              write(*,*)'track not in TOFp acceptance ',eta_bin,slat_bin
              KEEPIT = .FALSE.
              imslat = 0
           end if

cTEST        if(imslat.gt.0)then
cTEST         write(*,*)'match? ',eta_bin,slat_bin,z_int,p_tot,s_trk,' .. ',
cTEST     1     imslat,cts_mslat(imslat).z_hit,cts_mslat(imslat).ptot,
cTEST     1     cts_mslat(imslat).s_length
cTEST        elseif(eta_bin.ge.10.and.(slat_bin.ge.241.and.slat_bin.le.265))then
cTEST         write(*,*)'no mat ',eta_bin,slat_bin,z_int,p_tot,s_trk,' .. ',
cTEST     1     imslat,0.0,0.0,0.0
cTEST        end if

c---- fill dst_tof_trk...
          if(KEEPIT)then
           kk                       = kk + 1
           dst_tof_trk(kk).det_id   = idet
           dst_tof_trk(kk).itptrk   = itptrk
           dst_tof_trk(kk).ittetrk  = ittetrk
           dst_tof_trk(kk).ictfhit  = ictfhit
           dst_tof_trk(kk).ig2ttrk  = ig2ttrk
           dst_tof_trk(kk).ptot     = p_tot
           dst_tof_trk(kk).nfit     = tpt_track(itptrk).nfit
           dst_tof_trk(kk).redchi2  = trk_qual
           dst_tof_trk(kk).drinner  = drinner
           dst_tof_trk(kk).x_trk    = rval*SIN(phi_trk)    !!!!!!interchanged!!!!!!!
           dst_tof_trk(kk).y_trk    = rval*COS(phi_trk)    !!!!!!interchanged!!!!!!!
           dst_tof_trk(kk).z_trk    = z_int
           dst_tof_trk(kk).s_trk    = s_trk
           dst_tof_trk(kk).phi_trk  = phi_trk
           if(ictfhit.gt.0)then
            dst_tof_trk(kk).x_tof   = g2t_ctf_hit(ictfhit).x(1)
            dst_tof_trk(kk).y_tof   = g2t_ctf_hit(ictfhit).x(2)
            dst_tof_trk(kk).z_tof   = g2t_ctf_hit(ictfhit).x(3)
            dst_tof_trk(kk).s_tof   = g2t_ctf_hit(ictfhit).s_track
            dst_tof_trk(kk).phi_tof = phi_tof
           else
            dst_tof_trk(kk).x_tof   = 0.
            dst_tof_trk(kk).y_tof   = 0.
            dst_tof_trk(kk).z_tof   = 0.
            dst_tof_trk(kk).s_tof   = 0.
            dst_tof_trk(kk).phi_tof = 0.
           end if
           dst_tof_trk(kk).charge   = FLOAT(tpt_track(itptrk).q)
           dst_tof_trk(kk).ge_pid   = g2t_track(ig2ttrk).ge_pid
           dst_tof_trk(kk).dedx     = tte_mctrk(ittetrk).dedxr
           if(imslat.gt.0)then
            dst_tof_trk(kk).imslat    = imslat
            dst_tof_trk(kk).iflag     = 1
            dst_tof_trk(kk).pm_len    = cts_mslat(imslat).pm_length
            dst_tof_trk(kk).n_hits    = cts_mslat(imslat).n_hits
            dst_tof_trk(kk).adc       = cts_mslat(imslat).adc
            dst_tof_trk(kk).tof       = 1.0E9*cts_mslat(imslat).tof
            dst_tof_trk(kk).atime     = 1.0E9*cts_mslat(imslat).time
            dst_tof_trk(kk).mtime     = 1.0E9*cts_mslat(imslat).mtime
            dst_tof_trk(kk).mtimel    = 1.0E9*cts_mslat(imslat).mtimel
            dst_tof_trk(kk).ts_mtime  = 30.*1.0E9*cts_mslat(imslat).mtime /s_trk
            dst_tof_trk(kk).ts_mtime  = 30.*1.0E9*cts_mslat(imslat).mtime /s_trk
            dst_tof_trk(kk).ts_mtimel = 30.*1.0E9*cts_mslat(imslat).mtimel/s_trk
           else
            dst_tof_trk(kk).imslat    = 0
            dst_tof_trk(kk).iflag     = 0
            dst_tof_trk(kk).pm_len    = 0.
            dst_tof_trk(kk).n_hits    = 0.
            dst_tof_trk(kk).adc       = 0.
            dst_tof_trk(kk).tof       = 0.
            dst_tof_trk(kk).atime     = 0.
            dst_tof_trk(kk).mtime     = 0.
            dst_tof_trk(kk).mtimel    = 0.
            dst_tof_trk(kk).ts_mtime  = 0.
            dst_tof_trk(kk).ts_mtimel = 0.
           end if            ! end slat lookup check...
          end if             ! end KEEPIT check for dst_tof_trk fill...
        else
cTEST         write(*,*)trk_qual,trk_qual_cut,tpt_track(itptrk).nfit,trk_npts_min
        end if                 ! end track quality check...
201     continue               ! jump to here if necessary to skip track.
      enddo                    ! End loop over input tracks...

      dst_tof_trk_h.nok  = kk      ! update number of entries for this event...
c      write(*,*)' FILL_DST_TOF: no point in tte_mctrk   for tpt_track =',nopoint_ttetrk
c      write(*,*)' FILL_DST_TOF: no point in g2t_track   for tte_mctrk =',nopoint_g2ttrk
c      write(*,*)' FILL_DST_TOF: no point in g2t_ctf_hit for g2t_track =',nopoint_ctfhit
c      write(*,*)'                 g2t_track nok =',g2t_track_h.nok
c      write(*,*)'               g2t_ctf_hit nok =',g2t_ctf_hit_h.nok
c      write(*,*)'                 tpt_track nok =',tpt_track_h.nok
c      write(*,*)'                 tte_mctrk nok =',tte_mctrk_h.nok
      write(*,*)' FILL_DST_TOF:        n_exttrk =',n_exttrk
      write(*,*)' FILL_DST_TOF:        n_toftrk =',n_toftrk
      write(*,*)' FILL_DST_TOF: dst_tof_trk nok =',dst_tof_trk_h.nok
c      write(*,*)iout,iout1,iout2,iout3,iout4
      if(iout.gt.0)then
       prhitmat = FLOAT(iout1)/FLOAT(iout)
       write(*,*)' FILL_DST_TOF:  prob tpt/tte/g2t trk has ctf hit        = ',prhitmat
      end if
      if(iout2.gt.0)then
       prtramat = FLOAT(iout3)/FLOAT(iout2)
       write(*,*)' FILL_DST_TOF:  prob decent trk extrap.s to tray        = ',prtramat
       prtrkmat = FLOAT(iout4)/FLOAT(iout2)
       write(*,*)' FILL_DST_TOF:  prob decent trk extrap.s to struck slat = ',prtrkmat
       prhitmat = FLOAT(iout4)/FLOAT(iout3)
       write(*,*)' FILL_DST_TOF:  prob intray trk extrap.s to struck slat = ',prhitmat
      end if
      dst_tof_evt_h.nok           = 1
      dst_tof_evt(1).icall        = icall
      dst_tof_evt(1).n_tpttrk     = tpt_track_h.nok
      dst_tof_evt(1).n_ttetrk     = tte_mctrk_h.nok
      dst_tof_evt(1).n_g2ttrk     = g2t_track_h.nok
      dst_tof_evt(1).n_ctfhit     = g2t_ctf_hit_h.nok
      dst_tof_evt(1).n_exttrk     = n_exttrk
      dst_tof_evt(1).n_toftrk     = n_toftrk
      dst_tof_evt(1).ntrks        = iout
      dst_tof_evt(1).ntrks_hit    = iout1
      dst_tof_evt(1).ntrks_kee    = iout2
      dst_tof_evt(1).ntrks_tra    = iout3
      dst_tof_evt(1).ntrks_mat    = iout4
      dst_tof_evt(1).prtramat     = prtramat
      dst_tof_evt(1).prtrkmat     = prtrkmat
      dst_tof_evt(1).prhitmat     = prhitmat   
    
999   FILL_DST_TOF=STAFCV_OK
      WRITE(*,*)' FILL_DST_TOF: Exiting, no errors...'
      RETURN
1000  FILL_DST_TOF=STAFCV_BAD
      WRITE(*,*)' FILL_DST_TOF: Exiting with error!!!'
      RETURN
      END

c______________________________________________________________________
      INTEGER*4 FUNCTION Z_TO_IETA(zmin,zmax,z_int)
      IMPLICIT NONE
      integer i,j1,j2,eta_bin
      real*4  z_int,zmin(18),zmax(18),dzcheck1,dzcheck2,dz

            eta_bin   = 0
            j1        = 0
            j2        = 0
            dzcheck1  = 250.
            dzcheck2  = 250.
            do i      = 1,18
             if(z_int.gt.0)then
              if ( z_int.ge.zmin(i)  .and.
     1             z_int.le.zmax(i) ) then
               eta_bin = i
               goto 101
              end if
             else
              if ( z_int.le.zmin(i)  .and.
     1             z_int.ge.zmax(i) ) then
               eta_bin = i
               goto 101
              end if
             endif
             dz       = ABS(z_int - zmin(i))
             if(dz.lt.dzcheck1)then
              j1       = i
              dzcheck1 = dz
             end if
             dz       = ABS(z_int - zmax(i))
             if(dz.lt.dzcheck2)then
              j2       = i
              dzcheck2 = dz
             end if
            end do
101         if(eta_bin.gt.0)then
             Z_TO_IETA  = eta_bin 
            else
             if(dzcheck1.lt.dzcheck2)then
              Z_TO_IETA = j1
             else
              Z_TO_IETA = j2
             end if
            end if
            return
            end
