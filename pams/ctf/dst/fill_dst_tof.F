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
     9           dst_tof_h,           dst_tof )
      IMPLICIT NONE
#include "fill_dst_tof.inc"
      LOGICAL   first_call/.TRUE./
      INTEGER   itptrk2ttetrk(0:12500)
      INTEGER   ig2ttrk2ctftrk(0:32000)
      INTEGER   ietaphi2mslat(18,300)
      INTEGER   nopoint_ttetrk,nopoint_g2ttrk,nopoint_ctftrk
      INTEGER   itptrk,imctrk,igetrk,ittetrk,ig2ttrk,ictftrk,imslat
      INTEGER   i,j,k,l,m,n,icall,itrk,itr,ieb,kk,kk2,ll
      INTEGER   eta_bin,tray_bin,slat_bin,int_yn,islat
      INTEGER   n_exttrk,n_toftrk,trk_npts_min,i_eta,kk_eta
      REAL      b_field,deg_to_rad,pi,rad_to_deg
      REAL      p_input(6),p_gen,p_trk,p_tof,dx,dy,dr,phi1,phi2
      REAL      trk_qual,trk_qual_cut,trkchar,rval,zval,pid,c,r,s_trk
      REAL      xc,yc,crad,eta_hit,phi_tof,phi_trk
      REAL      ctg_zmin(18),ctg_zmax(18),len,trkz
      REAL      pt,pz,p_tot,x_strt,y_strt,cap_phi0,dedx,theta
      REAL      ang_int,td_ang,cosphi_int,z_int,dirvec(3)
      REAL      xprivert(2),p_privert(6),xp(2),xcc(2)
      REAL      xinner(2),xouter(2),dxinner,dxouter
      REAL      xend(2),p_end(6),xtrktof,ytrktof
      REAL      privert_x_cut,privert_y_cut
      REAL      x_privert,y_privert,z_privert
      INTEGER   Z_to_IETA
      LOGICAL   KEEPIT

c---- start executable statements...
      write(*,*)' FILL_DST_TOF: >>>----> Starting...'

c---- initialize...
      if(first_call)then
       first_call   = .FALSE.
       icall        = 0
       do i         = 1,18
        ctg_zmin(i) = ctg_slat_eta(i).z_min
        ctg_zmax(i) = ctg_slat_eta(i).z_max
cTEST        write(*,*)i,ctg_zmin(i),ctg_zmax(i)
       end do
      end if       ! end first_call code...
      icall        = icall + 1
      do i                 = 0,12500
       itptrk2ttetrk(i)    = 0
      end do
      do i                 = 1,32000
       ig2ttrk2ctftrk(i)   = 0
      end do
      do i                 = 1,300
       do j                = 1,18
        ietaphi2mslat(j,i) = 0
       end do
      end do
      n_exttrk             = 0
      n_toftrk             = 0

      kk            = 0
      kk2           = 0
      pi            = ACOS(-1.)
      deg_to_rad    = pi/180.
      rad_to_deg    = 1./deg_to_rad
      zval          = 228.          ! was 241.49
      b_field       = 5.            ! should be a parameter...
      trk_qual_cut  = 2.5           ! should be a parameter...
      trk_npts_min  = 10            ! should be a parameter...
      privert_x_cut = 2.            ! should be a parameter...
      privert_y_cut = 2.            ! should be a parameter...

c---- privert setup
      x_privert = dst_vertex(1).x
      y_privert = dst_vertex(1).y
      z_privert = dst_vertex(1).z
      write(*,'(1x,a36,3f9.3)')' FILL_DST_TOF: primary vertex posn =',
     1         x_privert,y_privert,z_privert

c---- set up lookup from tptrack table to tte_mctrk table...
      write(*,*)'Indexing...'
      if(tpt_track_h.nok.gt.12500)then
       write(*,*)' TOFANA -- internal array too small: itptrk2ttetrk'
       stop
      end if
      do ittetrk = 1,tte_mctrk_h.nok
       itptrk    = tte_mctrk(ittetrk).recid
       if(itptrk.gt.0.and.itptrk.le.tpt_track_h.nok)then
        itptrk2ttetrk(itptrk) = ittetrk
       end if
      end do
c---- set up lookup from g2t_track table to g2t_ctf_hit table...
      if(g2t_track_h.nok.gt.32000)then
       write(*,*)' TOFANA -- internal array too small: ig2ttrk2ctftrk'
       stop
      end if
      do ictftrk = 1,g2t_ctf_hit_h.nok
       ig2ttrk   = g2t_ctf_hit(ictftrk).track_p
       if(ig2ttrk.gt.0.and.ig2ttrk.le.g2t_track_h.nok)then
        ig2ttrk2ctftrk(ig2ttrk) = ictftrk
       else
        write(*,*)'pointer in g2t_ctf_hit to row in g2t_track too large',
     1             ictftrk,ig2ttrk,g2t_track_h.nok
       end if
      end do
c---- set up lookup from i_eta and i_phi to cts_mslat table...
      do i       = 1,cts_mslat_h.nok
       j         = cts_mslat(i).i_eta
       k         = cts_mslat(i).i_phi
       if((j.ge.1.and.j.le. 18).AND.
     1    (k.ge.1.and.k.le.300))then
          ietaphi2mslat(j,k) = i
       else
        write(*,*)'MSLAT ERROR -- ',i,j,k
       end if
      end do
      nopoint_ttetrk = 0
      nopoint_g2ttrk = 0
      nopoint_ctftrk = 0

c---- start loop over reconstructed tracks...
      do itptrk   = 1,tpt_track_h.nok
        KEEPIT    = .FALSE.

c---- get all pointers (don't bother extrapolating otherwise)
        ittetrk  = -1
        ig2ttrk  = -1
        ictftrk  = -1
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
        ictftrk  = ig2ttrk2ctftrk(ig2ttrk)
        if(ictftrk.le.0.OR.
     1     ictftrk.gt.g2t_ctf_hit_h.nok)then
         nopoint_ctftrk = nopoint_ctftrk + 1
         goto 201
        end if

c---- extrapolate to TOF...
        trk_qual  =       tpt_track(itptrk).chisq(1)
     1            / FLOAT(tpt_track(itptrk).nfit)
        if( trk_qual              .le.trk_qual_cut .AND.
     1      tpt_track(itptrk).nfit.ge.trk_npts_min )then
          n_exttrk= n_exttrk + 1
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
          int_yn  = 1
          trkz    = 213.*pz/pt
          kk_eta  = Z_TO_IETA(ctg_zmin,ctg_zmax,trkz)
          if(kk_eta.gt.0)then
           len     = ABS(trkz - ctg_zmin(kk_eta))
           rval    = 214.35 - len*SIND(8.0)
           call EUT_EXTRA_RAD(p_input,rval,trkchar,ang_int,td_ang,
     +                        cosphi_int,z_int,int_yn,dirvec)
          end if
          if((int_yn.eq.0).AND.
     1       (ABS(z_int).le.zval))then
           phi1      = ATAN2(g2t_ctf_hit(ictftrk).x(1),
     1                       g2t_ctf_hit(ictftrk).x(2))
           phi2      = -ang_int
           if(phi2.gt.1.4)phi1 = phi1 + 2.*pi
           phi2      = phi2 + pi/2.
           if(phi1.lt.-pi)then
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
          end if
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
          if(int_yn.eq.0.and.
     1       dxinner.lt.privert_x_cut.and.
     1       dxouter.lt.privert_y_cut)then
           xinner(1) = xprivert(1)
           xinner(2) = xprivert(2)
           xouter(1) = xend(1)
           xouter(2) = xend(2)
           dxinner   = SQRT(xinner(1)**2+xinner(2)**2)
           dxouter   = SQRT((xouter(1)-xp(1))**2
     1               +      (xouter(2)-xp(2))**2)
           c         = SQRT((xouter(1)-xinner(1))**2
     1               +      (xouter(2)-xinner(2))**2)
           r         = crad
           theta     = ATAN2(rval,z_int)
           s_trk     = SQRT((2.*r*ASIN(c/2./r))**2 + (z_int-z_privert)**2)
     1               - SIN(theta) + pi/2.
           if(s_trk.gt.200.)then
            KEEPIT    = .TRUE.             ! <----------<<<<<<<
           end if
          else
           s_trk     = 0.
           dxinner   = 0.
           dxouter   = 0.
          end if
c---- assign eta,tray,slat bin numbers
          theta      = 0.
          eta_bin    = 0
          tray_bin   = 0
          slat_bin   = 0
          if(int_yn.eq.0.and.ABS(z_int).le.zval)then        ! Trk hits TOF
            n_toftrk = n_toftrk + 1
            eta_hit  = -1.0*LOG(TAN(ATAN2(rval,z_int)/2.0))
            theta    = ATAN2(rval,z_int)
            eta_bin  = Z_TO_IETA(ctg_zmin,ctg_zmax,z_int)
            tray_bin = IFIX((phi_trk+pi)/ 0.10472    ) + 1
            slat_bin = IFIX((phi_trk+pi)/(0.10472/5.)) + 1
            if(tray_bin.lt.  01)tray_bin = tray_bin + 60
            if(tray_bin.gt.  60)tray_bin = tray_bin - 60
            if(slat_bin.lt.   1)slat_bin = slat_bin + 300
            if(tray_bin.gt. 300)slat_bin = slat_bin - 300

c---- fill dst_tof...
            if(KEEPIT)then
             kk                   = kk + 1
             dst_tof(kk).itptrk   = itptrk
             dst_tof(kk).ictftrk  = ictftrk
             dst_tof(kk).ig2ttrk  = ig2ttrk
             dst_tof(kk).ptot     = p_tot
             dst_tof(kk).x_trk    = rval*COS(phi_trk)
             dst_tof(kk).y_trk    = rval*SIN(phi_trk)
             dst_tof(kk).z_trk    = z_int
             dst_tof(kk).s_trk    = s_trk
             dst_tof(kk).charge   = FLOAT(tpt_track(itptrk).q)
             if(ang_int.ge.-3.*deg_to_rad)then
               islat = IFIX((ang_int+3.*deg_to_rad)/(0.10472/5.)) + 1
             else
               islat = IFIX((ang_int+3.*deg_to_rad)/(0.10472/5.)) + 300
             endif
             if(eta_bin.ge.1.and.eta_bin.le. 18.and.
     1          islat  .ge.1.and.islat  .le.300)then
              imslat = ietaphi2mslat(eta_bin,islat)
             else
              imslat = 0
             end if
             if(imslat.gt.0)then
              if(cts_mslat(imslat).n_hits.ge.1)then ! for evaluation only... i.e.
               kk2                = kk2 + 1         ! was chosen slat actually struck?
              end if
              dst_tof(kk).imslat = imslat
              dst_tof(kk).iflag  = 1
              dst_tof(kk).tof    = 1.0E9*cts_mslat(imslat).tof
              dst_tof(kk).atime  = 1.0E9*cts_mslat(imslat).time
              dst_tof(kk).mtime  = 1.0E9*cts_mslat(imslat).mtime
              dst_tof(kk).mtimel = 1.0E9*cts_mslat(imslat).mtimel
              dst_tof(kk).n_hits = cts_mslat(imslat).n_hits
              dst_tof(kk).adc    = cts_mslat(imslat).adc
              dst_tof(kk).pm_len = cts_mslat(imslat).pm_length
             else
              dst_tof(kk).imslat = 0
              dst_tof(kk).iflag  = 0
              dst_tof(kk).tof    = 0.
              dst_tof(kk).atime  = 0.
              dst_tof(kk).mtime  = 0.
              dst_tof(kk).mtimel = 0.
              dst_tof(kk).pm_len = 0.
              dst_tof(kk).n_hits = 0.
              dst_tof(kk).adc    = 0.
c              dst_tof(kk).    = 0.
c              dst_tof(kk).    = 0.
             end if            ! end slat lookup check...
            end if             ! end KEEPIT check for dst_tof fill...
          end if               ! end track hits tof check...
         end if                ! end track quality check...
201     continue               ! pointer lookup problem jump...
      enddo                    ! End loop over input tracks...

      dst_tof_h.nok  = kk      ! update number of entries for this event...
      write(*,*)' FILL_DST_TOF: no point in tte_mctrk   for tpt_track =',nopoint_ttetrk
      write(*,*)' FILL_DST_TOF: no point in g2t_track   for tte_mctrk =',nopoint_g2ttrk
      write(*,*)' FILL_DST_TOF: no point in g2t_ctf_hit for g2t_track =',nopoint_ctftrk
c      write(*,*)'               g2t_track_h.nok =',g2t_track_h.nok
c      write(*,*)'             g2t_ctf_hit_h.nok =',g2t_ctf_hit_h.nok
c      write(*,*)'               tpt_track_h.nok =',tpt_track_h.nok
c      write(*,*)'               tte_mctrk_h.nok =',tte_mctrk_h.nok
      write(*,*)' FILL_DST_TOF:        n_exttrk =',n_exttrk
      write(*,*)' FILL_DST_TOF:        n_toftrk =',n_toftrk
      write(*,*)' FILL_DST_TOF:   dst_tof_h.nok =',dst_tof_h.nok
      if(kk.gt.0)then
       write(*,*)' FILL_DST_TOF:      match prob =',FLOAT(kk2)/FLOAT(kk)
      end if

999   FILL_DST_TOF=STAFCV_OK
      RETURN
      END

c______________________________________________________________________
      INTEGER*4 FUNCTION Z_TO_IETA(zmin,zmax,z_int)
      IMPLICIT NONE
      integer i,j,eta_bin
      real*4  z_int,zmin(18),zmax(18),dzcheck,dz

            eta_bin   = 0
            dzcheck   = 250.
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
             if(dz.lt.dzcheck)then
              j       = i
              dzcheck = dz
             end if
            end do
101         if(eta_bin.gt.0)then
             Z_TO_IETA = eta_bin
            else
             Z_TO_IETA = j
            end if
            return
            end
