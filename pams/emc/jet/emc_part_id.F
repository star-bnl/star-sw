c:>------------------------------------------------------------------
C:FILE:         emc_part_id.F
C:DESCRIPTION:  EMC Analysis Module, package ems, STAF compliant.
c:AUTHOR:       K. Shestermanov, IHEP Protvino
C:BUGS:         None known.
C:HISTORY:      11/23/99 - Creation.
c:>------------------------------------------------------------------
      INTEGER*4 FUNCTION emc_part_id(
     1       emc_cluster_h,       emc_cluster ,
     2 emc_cluster_index_h, emc_cluster_index ,
     3    emc_hits_bsmde_h,    emc_hits_bsmde ,
     4    emc_hits_bsmdp_h,    emc_hits_bsmdp ,
     5       ems_control_h,       ems_control ,
     6   emc_part_id_par_h,   emc_part_id_par ,
     7         calb_calg_h,         calb_calg ,
     8         calb_calr_h,         calb_calr ,
     9         dst_track_h,         dst_track ,  
     A           emc_gam_h,           emc_gam ,
     1         emc_elect_h,         emc_elect )
c:>------------------------------------------------------------------
C:ROUTINE:      INTEGER*4 EMS_ELECT
C:DESCRIPTION:  looks for cluster in barrel EMC
C:ARGUMENTS:    emc_hits_bsmde emc_hits_bsmdp,emc_energy,ems_control
C:ARGUMENTS:    emc_cl_par,emc_cluster
C:RETURN VALUE: STAFCV_OK on successful completion
c:>------------------------------------------------------------------
      IMPLICIT NONE

#include "emc_part_id.inc"
#include "emc_def.h"
#include "math_constants.inc"
#include "phys_constants.inc"
c
c  %L

      integer nsh,nstrip_e,nstrip_p,np_e,np_p      
      parameter (nsh=250,nstrip_e=150,nstrip_p=15,np_e=4,np_p=4) 
      integer n_cluster,ne,ncluster,ns_cells,etab,phib
      integer n_shower,cells,nstr
      real p_cluster,et_tot,et_tot_sim,pt_sum_out,eta,phi,x_local(5)
      real s_eta,s_phi
      real energy,s_energy,rmin,rmax,isol,hi2_z(np_e),hi2_rphi(np_p)
      real eta_w(np_e),s_eta_w(np_e),phi_w(np_p),s_phi_w(np_p)
      real zrphi,z_point(np_e)
      real phi_smd_tower,z_rphi
      integer i,j,k,ihit,n_tower_phi,n_tower_eta
      REAL zr_point,sweit,width
      real strip_e(nstrip_e),strip_ew(nstrip_e)
      real strip_p(nstrip_p),strip_pw(nstrip_p)
      real edep_smd(np_e),width_sh_e(np_e),width_sh_p(np_p),phi0,xcor
      real pdep_smd(np_p),detadz,dphidx
      INTEGER phi_smd,eta_smd,eta_s_min,eta_s_max,phi_s_min,phi_s_max
      INTEGER phi_bin,eta_bin,out_num,smd_p_min,smd_p_max
      integer str_e_max(np_e),str_p_max(np_p),n_show,itwr
      integer det,module,sub,p_bemc,p_bsmde,p_bsmdp
      integer p_nst_e(nstrip_e),p_nst_p(nstrip_p),nmod,det_id(10)
      real weight_s,sigma_shower_shape,shower_shape,shape,s_shape
      real phi_corr,dphim,dphir
c---                    table ems_control and ems_electpar
      integer n_shower_e,n_shower_p
      integer num_str_n_e,num_str_w_e,str_eta_p,str_phi_p
      real str_width_e(2),str_width_p,bemc_max_eta,c_rphi
      real rad_smd_e,rad_smd_p
      real energy_cut,et_seed,r_shower,r_isol,x_edge,hi2_z_cut,hi2_rphi_cut
      real e_smd_cut,wid_z_cut,wid_rphi_cut,str_shift(2)
      real trk_qual_cut,delta_rphi,delta_z 
      logical first /.true./
c---
      real w0/3.5/
      real e_smd_s,e_smd_sum(np_e),p_smd_sum(np_p),weight
c---
c---                            for charged track 
      integer eta_b_trk,phi_b_trk,int_yn
      real b_field,trk_qual,r_int,emin,emax,phi_trk,eta_trk
      real trkchar,pt,pz,p_tot,crad,x_strt,y_strt
      real cap_phi0,xc,yc,p_input(6),z_int,rval
      real zval,ang_int,td_ang,cosphi_int,dirvec(3)
      real trak(6),trk1(6),xc1(2),r1,xp(2),xn1(2)
      real x_em,y_em,z_em,e_ov_p,e_p_min,e_p_max
      real x_trk,y_trk,eta_vtx,phi_vtx
      real pos(3) /0.0, 0.0, 0.0/
      real mfld(3)
      real psi
      integer itrk
c---
c---                             for pi0

      real cxx(np_e),cyy(np_e),czz(np_e),egam1,egam2,ep(np_e,np_p),
      real egam(np_e),sig(np_e),sig12 
      integer k_eta,k_phi,ngam,str_efp_min,str_efp_max,im,jm
      integer n_sho_eta,n_sho_phi,iep(np_e),max_epi,lvmin,
      real sampl_smde,sampl_smdp,ep_smd_sum,s12
c
      real smin
      integer mode /1/
      integer iw(10,10)
      integer idw /10/
c
      integer dist(4,4),k1,k2,lvsimi,id_min,idist,iqu(10),n_iter,iassndx,ir
      real a1,a2,b1,b2,c1,c2,a12,norm,sigma,e1,e2
c
      save str_width_e,str_width_p,rad_smd_e,rad_smd_p,c_rphi
      save bemc_max_eta,num_str_n_e,num_str_w_e,str_shift
      save n_shower_e,n_shower_p,energy_cut,et_seed,r_shower,r_isol
      save x_edge,e_smd_cut,wid_z_cut,wid_rphi_cut,hi2_z_cut,hi2_rphi_cut
      save e_p_min,e_p_max,trk_qual_cut,delta_rphi,delta_z,first
c
c---- start executable statements...
      write(*,*) 'emc_part_id:  starting...'
      emc_part_id = STAFCV_OK
c---                           for charged track
      call gufld(pos,mfld)
      b_field=mfld(3)
c
      if(first) then
        str_width_e(1)=2.*(calb_calg(1).seta1wdh+calb_calg(1).set12wdh)
        str_width_e(2)=2.*(calb_calg(1).seta2wdh+calb_calg(1).set12wdh)
        rad_smd_e=calb_calr(1).rsmd1
        rad_smd_p=calb_calr(1).rsmd2
        str_width_p=2.*(calb_calg(1).sphiwdh+calb_calg(1).sphidwdh)
        c_rphi=2.*(calb_calg(1).sphiwdh*calb_calg(1).nphistr/2.+
     *         calb_calg(1).sphidwdh*(calb_calg(1).nphistr/2))
        bemc_max_eta = calb_calg(1).etacut
        num_str_n_e=calb_calg(1).netfirst
        num_str_w_e=calb_calg(1).netsecon
        str_shift(1)=2.*calb_calg(1).SmetaWdh
        str_shift(2)=str_shift(1)+str_width_e(1)*num_str_n_e
c---str_width_e(2)
        n_shower_e=emc_part_id_par(1).n_shower_e 
        n_shower_p=emc_part_id_par(1).n_shower_p
        r_shower=emc_part_id_par(1).r_shower
        x_edge=emc_part_id_par(1).x_edge
        e_smd_cut=emc_part_id_par(1).e_smd_cut
        wid_z_cut=emc_part_id_par(1).wid_z_cut
        wid_rphi_cut=emc_part_id_par(1).wid_rphi_cut
        hi2_z_cut=emc_part_id_par(1).hi2_z_cut
        hi2_rphi_cut=emc_part_id_par(1).hi2_rphi_cut
        trk_qual_cut=emc_part_id_par(1).trk_qual_cut
        delta_rphi=emc_part_id_par(1).delta_rphi
        delta_z=emc_part_id_par(1).delta_z
        e_p_min=emc_part_id_par(1).e_p_min
        e_p_max=emc_part_id_par(1).e_p_max        
        first=.false.
      endif
c
c---- Initialize nok counter and variable arrays
c
      p_bsmde=-1
      p_bsmdp=-1
c---point of cluster interaction in SMD
c
      n_shower=emc_cluster_h.nok
      if(n_shower.le.0) return
      emc_gam_h.nok=0
      do ne=1,n_shower 
        energy=emc_cluster(ne).energy
        s_energy=emc_cluster(ne).s_energy
        eta=emc_cluster(ne).eta
        phi=emc_cluster(ne).phi
        eta_bin=n_tower_eta(eta)
        phi_bin=n_tower_phi(phi) 
        s_eta=emc_cluster(ne).sigma_eta
        call vzero(strip_e,nstrip_e)
        call vzero(strip_ew,nstrip_e)
        call vzero(strip_p,nstrip_p)
        call vzero(strip_pw,nstrip_p)
c
c---find eta(z) from strip table
c
        if(eta_bin.le.20) then 
          eta_smd=10-((eta_bin-1)/2)
        else 
          eta_smd=((eta_bin-21)/2)+1
        endif
        phi_smd=((phi_bin-1)/2)+1
        do ihit=1,emc_hits_bsmde_h.nok
          module=emc_hits_bsmde(ihit).module
          if(nmod(module).eq.phi_smd) then
             nstr=int(emc_hits_bsmde(ihit).eta)
             strip_e(nstr)=strip_e(nstr)+emc_hits_bsmde(ihit).energy
             p_nst_e(nstr)=ihit
          endif
        enddo  
        call smd_st_sum(abs(eta)-0.48*r_shower,str_efp_min)
        call smd_st_sum(abs(eta)+0.48*r_shower,str_efp_max)
c        call smd_st_sum(abs(eta)-3.*s_eta,str_efp_min)
c        call smd_st_sum(abs(eta)+3.*s_eta,str_efp_max)
        str_efp_min=max(1,str_efp_min)
        str_efp_min=min(str_efp_min,nstrip_e)
        edep_smd(1)=0.
        do ihit=str_efp_min,str_efp_max
          edep_smd(1)=edep_smd(1)+strip_e(ihit)
        enddo
        e_smd_s=sampl_smde(edep_smd(1))        
        do ihit=str_efp_min,str_efp_max
c          strip_e(ihit)=strip_e(ihit)*energy(ne)/edep_smd(1)
          strip_e(ihit)=strip_e(ihit)*e_smd_s/edep_smd(1)
          strip_ew(ihit)=strip_e(ihit)
        enddo
        n_sho_eta=(str_efp_max-str_efp_min)/(n_shower_e*2+1)+1        
        k_eta=0
        call vzero(width_sh_e,np_e)
        call vzero(eta_w,np_e)
        call vzero(s_eta_w,np_e)
        call vzero(det_id,10)
        call vzero(hi2_z,np_e)
        call vzero(z_point,np_e)
        call vzero(edep_smd,np_e)
        call vzero(str_e_max,np_e)
        call vzero(iqu,np_e)
        call vzero(e_smd_sum,np_e)
c        do i=1,np_e
c          iep(i)=i
c        enddo
c---
        do k=1,n_sho_eta
          k_eta=k_eta+1
          do ihit=str_efp_min,str_efp_max
            if(strip_e(ihit).gt.edep_smd(k_eta)) then
              edep_smd(k_eta)=strip_e(ihit)
              str_e_max(k_eta)=ihit
            endif
          enddo 
          if(edep_smd(k_eta).le.e_smd_cut*0.5) goto 120
          p_bsmde=p_nst_e(str_e_max(k_eta))
          eta_s_min=max(1,str_efp_min,str_e_max(k_eta)-n_shower_e)
          eta_s_max=min(str_efp_max,str_e_max(k_eta)+n_shower_e,nstrip_e)   
          do ihit=eta_s_min,eta_s_max
            e_smd_sum(k_eta)=e_smd_sum(k_eta)+strip_e(ihit)
          enddo
          if(e_smd_sum(k_eta).lt.e_smd_cut) go to 120
c---shower location in eta (z) from SMD table
          zr_point=0.
          sweit=0
          do ihit=eta_s_min,eta_s_max
            if(strip_e(ihit).gt.0.) then
              nstr=int(ihit/num_str_n_e)+1
              zrphi=(float(ihit-(nstr-1)*num_str_n_e)-0.5)*str_width_e(nstr)
     *             +str_shift(nstr) 
              weight=max(0.,w0+log(strip_e(ihit)/e_smd_sum(k_eta)))
              sweit=sweit+weight
              zr_point=zr_point+zrphi*weight
            endif
          enddo
          zr_point=zr_point/sweit
c---shower width in z direction
          width=0.
          do ihit=eta_s_min,eta_s_max
            if(strip_e(ihit).ne.0.) then
              nstr=int(ihit/num_str_n_e)+1
              zrphi=(float(ihit-(nstr-1)*num_str_n_e)-0.5)*str_width_e(nstr)
     *             +str_shift(nstr)
              weight=max(0.,w0+log(strip_e(ihit)/e_smd_sum(k_eta)))
              width=width+(zrphi-zr_point)**2*weight
            endif
          enddo
          z_point(k_eta)=zr_point
          if(eta.lt.0.) z_point(k_eta)=-z_point(k_eta)
          width=sqrt(width/sweit)
c          if(width.le.0.001) then
c            iqu(k_eta)=-1
c            iep(k_eta)=-1
c            goto 130
c          endif
          if(width.gt.wid_z_cut) then
            iqu(k_eta)=2
            go to 130
          endif
          width_sh_e(k_eta)=width
          eta_w(k_eta)=-log(tan(atan2(rad_smd_e,z_point(k_eta))/2.)) 
          s_eta_w(k_eta)=detadz(rad_smd_e,zr_point)*width
          det_id(k_eta)=det_id(k_eta)+BSMDE*10
c
c---Hi2 for EM shower in eta direction
c
          zr_point=0.
          sweit=0.
          weight_s=0.
          hi2_z(k_eta)=0.
          do ihit=eta_s_min,eta_s_max
            s_shape=sigma_shower_shape(ihit-str_e_max(k_eta),e_smd_sum(k_eta))
            zr_point=zr_point+float(ihit-str_e_max(k_eta))*
     *               strip_e(ihit)/s_shape 
            sweit=sweit+strip_e(ihit)
            weight_s=weight_s+strip_e(ihit)/s_shape
          enddo
          zr_point=zr_point/weight_s
          do ihit=eta_s_min,eta_s_max
            s_shape=sigma_shower_shape(ihit-str_e_max(k_eta),e_smd_sum(k_eta))
            shape=shower_shape(ihit-str_e_max(k_eta),zr_point)
            hi2_z(k_eta)=hi2_z(k_eta)+(strip_e(ihit)/sweit-shape)**2/
     *                   (shape*s_shape)**2
          enddo
          hi2_z(k_eta)=hi2_z(k_eta)/float(eta_s_max-eta_s_min+1)
          if(hi2_z(k_eta).gt.hi2_z_cut) then
            iqu(k_eta)=2
            go to 130          
          endif
          do ihit=eta_s_min,eta_s_max
            strip_e(ihit)=0.
          enddo
          iqu(k_eta)=1
          goto 140
c          go to 130
 120      continue 
          if(k_eta.eq.1) then
            z_point(k_eta)=rad_smd_e*sinh(eta)
            eta_w(k_eta)=eta      
            s_eta_w(k_eta)=emc_cluster(ne).sigma_eta
            width_sh_e(k_eta)=-1.
            hi2_z(k_eta)=-1.
            iep(k_eta)=-1
          else
            k_eta=k_eta-1
          endif
          goto 150
 130      continue
          norm=edep_smd(k_eta)/shower_shape(0,0.)          
          do ihit=eta_s_min,eta_s_max
            strip_e(ihit)=strip_e(ihit)-norm*
     *                    shower_shape(ihit-str_e_max(k_eta),0.)
          enddo
c          if(k.eq.n_sho_eta) go to 120
c          k_eta=k_eta-1
 140    enddo
 150    continue
c
        call fit_sho(k_eta,str_e_max,e_smd_sum,iqu,strip_ew,n_shower_e,e1,e2,ir)
c
c---find phi from strip in SMD table
c
        do ihit=1,emc_hits_bsmdp_h.nok
          module=emc_hits_bsmdp(ihit).module
          if(nmod(module).eq.phi_smd) then
            if(emc_hits_bsmdp(ihit).eta.eq.eta_smd) then
              nstr=emc_hits_bsmdp(ihit).sub
              strip_p(nstr)=emc_hits_bsmdp(ihit).energy
              p_nst_p(nstr)=ihit
            endif 
          endif
        enddo
        phi0=phi_smd_tower(phi_smd)
        if(phi-phi0.ge.6.23082543) phi0=C_2PI
c
        xcor=c_rphi-rad_smd_p*tan(phi0-phi)
        str_phi_p=int(xcor/str_width_p)+1
c
        k_phi=0
        pdep_smd(1)=0.
c        str_efp_min=1                !           temporary ????
c        str_efp_max=15               !
        str_efp_min=max(1,str_phi_p-2*n_shower_p)
        str_efp_max=min(str_phi_p+2*n_shower_p,nstrip_p)
c
        n_sho_phi=(str_efp_max-str_efp_min)/(n_shower_p*2+1)+1
c
        do ihit=str_efp_min,str_efp_max
          pdep_smd(1)=pdep_smd(1)+strip_p(ihit)
        enddo
        e_smd_s=sampl_smde(pdep_smd(1))        
        do ihit=str_efp_min,str_efp_max
          strip_p(ihit)=strip_p(ihit)*e_smd_s/pdep_smd(1)
          strip_pw(ihit)=strip_p(ihit)
        enddo
        call vzero(p_smd_sum,np_p)
        call vzero(x_local,np_p)
        call vzero(width_sh_p,np_p)
        call vzero(det_id,10)
        call vzero(hi2_rphi,np_p)
        call vzero(phi_w,np_p)
        call vzero(s_phi_w,np_p)
        call vzero(str_p_max,np_p)
        call vzero(pdep_smd,np_p)
        call vzero(iqu,np_p)
        n_show=n_shower_p
c----
        do k=1,n_sho_phi
          k_phi=k_phi+1
          do ihit=str_efp_min,str_efp_max
            if(strip_p(ihit).gt.pdep_smd(k_phi)) then
              pdep_smd(k_phi)=strip_p(ihit)
              str_p_max(k_phi)=ihit
            endif
          enddo
          if(pdep_smd(k_phi).le.e_smd_cut*0.5) goto 155
          p_bsmdp=p_nst_p(str_p_max(k_phi))
          phi_s_min=max(1,str_p_max(k_phi)-n_shower_p)
          phi_s_max=min(str_p_max(k_phi)+n_shower_p,nstrip_p)          
          if(str_p_max(k_phi).lt.3) then
            n_show=str_p_max(k_phi)-1
          elseif(str_p_max(k_phi).gt.13) then
            n_show=15-str_p_max(k_phi)
          endif
          n_show=min(n_show,n_shower_p)
          phi_s_min=max(1,str_p_max(k_phi)-n_show)
          phi_s_max=min(str_p_max(k_phi)+n_show,nstrip_p)          
          do ihit=phi_s_min,phi_s_max
            p_smd_sum(k_phi)=p_smd_sum(k_phi)+strip_p(ihit)    
          enddo
          if(p_smd_sum(k_phi).lt.e_smd_cut) go to 155
c---shower location in r*phi direction
          zr_point=0.
          sweit=0.
          do ihit=phi_s_min,phi_s_max
            if(strip_p(ihit).ne.0.) then
              zrphi=(float(ihit)-0.5)*str_width_p
              weight=max(0.,w0+log(strip_p(ihit)/p_smd_sum(k_phi)))
              sweit=sweit+weight
              zr_point=zr_point+zrphi*weight
            endif
          enddo
          zr_point=zr_point/sweit
          if(zr_point.lt.x_edge) go to 160
          if(abs(c_rphi*2.-zr_point).lt.x_edge) go to 160
          phi_w(k_phi)=phi_smd_tower(phi_smd)-atan((c_rphi-zr_point)/rad_smd_p)
          x_local(k_phi)=c_rphi-zr_point
c---shower width in r*phi direction
          width=0.
          do ihit=phi_s_min,phi_s_max
            if(strip_p(ihit).ne.0.) then 
              zrphi=(float(ihit)-0.5)*str_width_p
              weight=max(0.,w0+log(strip_p(ihit)/p_smd_sum(k_phi)))
              width=width+(zrphi-zr_point)**2*weight
            endif
          enddo
          width=sqrt(width/sweit)
c          if(width.le.0.001) then
c            iqu(k_phi)=-1
c            goto 160
c          endif 
          if(width.gt.wid_rphi_cut) then
            iqu(k_phi)=2            
            goto 160
          endif
          s_phi_w(k_phi)=dphidx(rad_smd_p,c_rphi,zr_point)*width
          width_sh_p(k_phi)=width
          det_id(k_phi)=det_id(k_phi)+BSMDP
c
c---Hi2 for EM shower in phi direction
c
          zr_point=0.
          sweit=0.
          weight_s=0.
          hi2_rphi(k_phi)=0.
          do ihit=phi_s_min,phi_s_max
            s_shape=sigma_shower_shape(ihit-str_p_max(k_phi),p_smd_sum(k_phi))
            zr_point=zr_point+float(ihit-str_p_max(k_phi))*
     *               strip_p(ihit)/s_shape
            sweit=sweit+strip_p(ihit)
            weight_s=weight_s+strip_p(ihit)/s_shape
          enddo
          zr_point=zr_point/weight_s
          do ihit=phi_s_min,phi_s_max
            s_shape=sigma_shower_shape(ihit-str_p_max(k_phi),p_smd_sum(k_phi))
            shape=shower_shape(ihit-str_p_max(k_phi),zr_point)
            hi2_rphi(k_phi)=hi2_rphi(k_phi)+(strip_p(ihit)/sweit-shape)**2/
     *                      (shape*s_shape)**2
          enddo
          hi2_rphi(k_phi)=hi2_rphi(k_phi)/float(phi_s_max-phi_s_min+1)
          if(hi2_rphi(k_phi).gt.hi2_rphi_cut) then
            iqu(k_phi)=2
            go to 160
          endif
          do ihit=phi_s_min,phi_s_max   
            strip_p(ihit)=0.
          enddo
          iqu(k_eta)=1
c
          goto 170
c          go to 160
 155      continue 
          if(k_phi.eq.1) then
            x_local(k_phi)=c_rphi-xcor
            phi_w(k_phi)=phi      
            s_phi_w(k_phi)=emc_cluster(ne).sigma_phi
            width_sh_p(k_phi)=-1.
            hi2_rphi(k_phi)=-1.
          else
            k_phi=k_phi-1
          endif
          goto 180
 160      continue
          norm=pdep_smd(k_phi)/shower_shape(0,0.)          
          do ihit=phi_s_min,phi_s_max
            strip_p(ihit)=strip_p(ihit)-norm*
     *                    shower_shape(ihit-str_p_max(k_phi),0.)
          enddo
c          if(k.eq.n_sho_phi) go to 155 
c          k_phi=k_phi-1
 170    enddo
 180    continue
c
        call fit_sho(k_phi,str_p_max,p_smd_sum,iqu,strip_pw,
     *               n_shower_p,e1,e2,ir)
c
c---
c        if(k_eta.eq.1.and.k_phi.gt.1) then
c          if(e_smd_sum(1).gt.p_smd_sum(1)+p_smd_sum(2)) then
c            k_eta=2
c            ep_smd_sum=e_smd_sum(1)
c            e_smd_sum(1)=p_smd_sum(1)*ep_smd_sum/(p_smd_sum(1)+p_smd_sum(2))
c            e_smd_sum(2)=p_smd_sum(2)*ep_smd_sum/(p_smd_sum(1)+p_smd_sum(2))
c            eta_w(2)=eta_w(1)
c          endif
c        endif
c        if(k_phi.eq.1.and.k_eta.gt.1) then
c          if(p_smd_sum(1).gt.e_smd_sum(1)+e_smd_sum(2)) then
c            k_phi=2
c            ep_smd_sum=p_smd_sum(1)
c            p_smd_sum(1)=e_smd_sum(1)*ep_smd_sum/(e_smd_sum(1)+e_smd_sum(2))
c            p_smd_sum(2)=e_smd_sum(2)*ep_smd_sum/(e_smd_sum(1)+e_smd_sum(2))
c            phi_w(2)=phi_w(1)
c          endif
c        endif
c---
        do i=k_phi+1,np_p
          iep(i)=0
        enddo
        iassndx=0
        do i=1,k_eta
          if(iep(k_eta).eq.-1) goto 191
          do j=1,k_phi
            ep(i,j)=abs(e_smd_sum(i)-p_smd_sum(j))/(e_smd_sum(i)+p_smd_sum(j))
c            ep(i,j)=abs(log(e_smd_sum(i)/p_smd_sum(j)))
            iassndx=1          
          enddo
 191    enddo
        if(iassndx.eq.1) then
          call assndx(mode,ep,k_eta,k_phi,np_e,iep,smin,iw,idw)
        endif
c
c------------------------  Charged track  ----------------------------------
c
        write(*,*)'EMC_part_id : No. tracks in =',dst_track_h.nok
        itrk=0
        if(dst_track_h.nok.eq.0) go to 211
        do ihit = 1, dst_track_h.nok       ! Loop over input tracks
          trk_qual=(dst_track(ihit).chisq(1)+
     +             dst_track(ihit).chisq(2))/2.0
          if(trk_qual.le.trk_qual_cut)then
            trkchar = float(dst_track(ihit).icharge)
            pt      = 1.0/dst_track(ihit).invpt
            pz      = pt*dst_track(ihit).tanl
            p_tot   = sqrt(pt**2 + pz**2)
            crad    = pt/abs(trkchar*C_D_CURVATURE*b_field)
            x_strt  = dst_track(ihit).r0*sin(dst_track(ihit).phi0*
     *                C_RAD_PER_DEG)
            y_strt  = dst_track(ihit).r0*cos(dst_track(ihit).phi0*
     *                C_RAD_PER_DEG)
            psi=dst_track(ihit).psi*C_RAD_PER_DEG
            cap_phi0= dst_track(ihit).psi*C_RAD_PER_DEG+trkchar*C_PI_2
            xc      = x_strt - crad*cos(cap_phi0)
            yc      = y_strt - crad*sin(cap_phi0)
            p_input(1) = xc
            p_input(2) = yc
            p_input(3) = dst_track(ihit).z0
            p_input(4) = crad
            p_input(5) = dst_track(ihit).tanl 
            p_input(6) = cap_phi0
            z_int = 10000.
c*-Check if it hits Barrel
            rval  = ems_control(1).bemc_inner_r
            zval  = ems_control(1).bemc_max_z
            call eut_extra_rad(p_input,zval,trkchar,ang_int,
     +              td_ang,cosphi_int,z_int,int_yn,dirvec)          
            if(int_yn.eq.0.and.abs(z_int).le.zval)then ! Trk hits EMC Barrel
c              eta_trk = -1.0*log(tan(atan2(rval,z_int)/2.0))
c              eta_b_trk=n_tower_eta(eta_trk)
c              phi_b_trk=n_tower_phi(ang_int)
c              write(*,*)'eta_t,ang,eta_b,phi_b',ang_int,eta_b_trk,phi_b_trk,eta_bin,phi_bin
c---
c              if(eta_b_trk.lt.eta_bin-1.or.
c     *           eta_b_trk.gt.eta_bin+1) goto 320
c              if(phi_b_trk.lt.phi_bin-1.or.
c     *           phi_b_trk.gt.phi_bin+1) goto 320
              e_ov_p=energy/p_tot
              rmin=(energy-e_p_min*s_energy)/p_tot
              rmax=(energy+e_p_max*s_energy)/p_tot
              write(*,*)'e_ov_p,p_tot,rmin,rmax',e_ov_p,p_tot,rmin,rmax
              if(e_ov_p.lt.rmin) goto 320
              if(e_ov_p.gt.rmax) goto 320
c---calculate the point of hit from gtr
              call eut_extra_rad(p_input,rad_smd_e,trkchar,
     +                ang_int,td_ang,cosphi_int,z_int,int_yn,dirvec)
              eta_trk = -1.0*log(tan(atan2(rad_smd_e,z_int)/2.0))
              x_trk=rad_smd_e*cos(ang_int)
              y_trk=rad_smd_e*sin(ang_int) 
c---point of interactions from shower from SMD
              do i=1,k_eta
                j=iep(i)
                if(j.eq.-1) j=1
                x_em=rad_smd_e*cos(phi_w(j))            
                y_em=rad_smd_e*sin(phi_w(j))
                z_em=z_point(i)
c
                z_rphi=sqrt((x_em-x_trk)**2+(y_em-y_trk)**2)
                if(z_rphi.le.delta_rphi) go to 200
                z_rphi=z_em-z_int
                if(abs(z_rphi).le.delta_z) go to 200
                goto 210
 200            iep(i)=0
 210         enddo
C*-Project Tracks back to the primary Vertex.
              trak(1)  = dst_track(ihit).r0
              trak(2)  = trak(1)*dst_track(ihit).phi0*C_RAD_PER_DEG
              trak(3)  = dst_track(ihit).z0 !z0
              trak(4)  = dst_track(ihit).psi*C_RAD_PER_DEG !psi
              trak(5)  = dst_track(ihit).tanl !tanl
              trak(6)  = FLOAT(dst_track(ihit).icharge)
     +                  * (0.2997*5.)*dst_track(ihit).invpt/1000.        !q/R

C*-At present assume that primary vertex is at X = Y = Z = 0

              xp(1) = 0.0
              xp(2) = 0.0
            
              call emc_circle_param_r(trak,xc1,r1)
              call emc_project_track(xc1,r1,xp,xn1)
              call emc_update_track_param(xc1,r1,xn1,trak,trk1) 
C*-Calculate (phi,eta) bin for Track at vertex            
              eta_vtx=-1.0*ALOG(TAN(ATAN2(pt,pz)/2.0))
              phi_vtx=trk1(4)
              do while(phi_vtx .ge. C_2PI)
                phi_vtx=phi_vtx-C_2PI
              enddo
              do while(phi_vtx .lt. 0.0)
                phi_vtx=phi_vtx+C_2PI
              enddo
              itrk=1
            endif
          endif  
 320      enddo
 211    continue
c-------------------------------------------------------
*     fill output table
c-------------------------------------------------------
        ngam=0
        do i=1,k_eta
          if(iep(i).le.0) go to 220 
          j=iep(i)
          if(iqu(j).eq.-1) go to 220
          ngam=ngam+1
          sig(ngam)=(e_smd_sum(i)+p_smd_sum(j))/2.
          cxx(ngam)=cos(phi_w(j))/cosh(eta_w(i))
          cyy(ngam)=sin(phi_w(j))/cosh(eta_w(i))
          czz(ngam)=tanh(eta_w(i))
          egam(ngam)=(e_smd_sum(i)+p_smd_sum(j))/2.
          emc_gam_h.nok = emc_gam_h.nok + 1
          out_num =  emc_gam_h.nok
          emc_gam(out_num).p_cluster = ne
          emc_gam(out_num).p_bsmde = p_bsmde
          emc_gam(out_num).p_bsmdp = p_bsmdp
          emc_gam(out_num).det_id  = det_id(i)
          emc_gam(out_num).eta = eta_w(i)
          emc_gam(out_num).sigma_eta = s_eta_w(i) 
          emc_gam(out_num).phi = phi_w(j)
          emc_gam(out_num).sigma_phi = s_phi_w(i)
          emc_gam(out_num).x_cl(1) = rad_smd_e*cos(phi_w(j))
          emc_gam(out_num).x_cl(2) = rad_smd_e*sin(phi_w(j))
          emc_gam(out_num).x_cl(3) = z_point(i)
          emc_gam(out_num).x_p_loc = x_local(j)
          emc_gam(out_num).r_isol = emc_cluster(ne).ratio_isol
          emc_gam(out_num).width_z = width_sh_e(i)
          emc_gam(out_num).width_rphi = width_sh_p(j)
          emc_gam(out_num).hi2_z = hi2_z(i)
          emc_gam(out_num).hi2_rphi = hi2_rphi(j)
 220    enddo
        if(ngam.eq.1) then
          emc_gam(out_num).energy = energy
          emc_gam(out_num).s_energy = s_energy
          emc_gam(out_num).p_cl(1) = energy*cxx(ngam)
          emc_gam(out_num).p_cl(2) = energy*cyy(ngam)
          emc_gam(out_num).p_cl(3) = energy*czz(ngam)
        else
          sig12=emc_cluster(ne).s_energy
          do i=1,ngam
            do j=i,ngam
              if(j.eq.i) goto 230
              
              egam1=((sig12*sig(j)-sig(j)**2)*egam(i)+sig(i)*sig(j)*
     *           (egam(j)-energy))/sig(j)/(sig12-sig(j)-sig(i))
              s12=(sig12+sig(i))*(sig12+sig(j))-sig(i)*sig(j)
              emc_gam(out_num-i).energy = egam1
              emc_gam(out_num-i).s_energy = sqrt((sig12+sig(i))/s12)
              emc_gam(out_num-i).p_cl(1) = egam1*cxx(i)
              emc_gam(out_num-i).p_cl(2) = egam1*cyy(i)
              emc_gam(out_num-i).p_cl(3) = egam1*czz(i)
              egam2=((sig12*sig(i)-sig(i)**2)*egam(j)+sig(i)*sig(j)*
     *           (egam(i)-energy))/sig(i)/(sig12-sig(i)-sig(j))      
              emc_gam(out_num).energy = egam2
              emc_gam(out_num).s_energy = sqrt((sig12+sig(j))/s12)
              emc_gam(out_num).p_cl(1) = egam2*cxx(j)
              emc_gam(out_num).p_cl(2) = egam2*cyy(j)
              emc_gam(out_num).p_cl(3) = egam2*czz(j)
              write(*,*),'out_num,ngam,i,j,egam2,s',out_num,ngam,i,j,egam2,
     *        emc_gam(out_num).s_energy
 230        enddo
          enddo
        endif
c
 235  enddo                     ! end loop over shower
      emc_part_id=STAFCV_OK
999   RETURN
      END
c
      subroutine fit_sho(k,str_max,smd_sum,iqu,strip,n_shower,e1,e2,iret)
c
      implicit none
c
      integer n_dim,nmax,iter
      parameter (n_dim=4,nmax=100,iter=10)
      integer k,str_max(n_dim),n_shower,dist(n_dim,n_dim),i,j,iqu(n_dim)
      integer k1,k2,idist,s_min,s_max,ihit,n_iter,iret
      real strip(*),smd_sum(n_dim),e1,e2,e_old,a1,a2,b1,b2,c1,c2,a12
      real sigma,sigma_shower_shape,shower_shape
c
      iret=1
      call vfill(dist,n_dim*n_dim,nmax)
        k1=0
        k2=0
        if(k.eq.1) goto 50
        idist=100
        do i=1,k
          if(iqu(i).eq.-1) goto 20
          do j=i,k
            if(i.eq.j) goto 10
            dist(i,j)=abs(str_max(i)-str_max(j))
            if(dist(i,j).lt.idist) then
              k1=i
              k2=j
              idist=dist(i,j)
            endif
 10       enddo
 20     enddo
c        id_min=lvsimi(dist,8,1)
c        k1=id_min/4+1
c        k2=id_min-4*(k1-1)
        if(k1-k2.eq.0) goto 50
        if(abs(k1-k2).lt.2*n_shower+1) then
          a1=0.
          a2=0.
          b1=0.
          b2=0.
          c1=0.
          c2=0.
          s_min=min(str_max(k1),str_max(k2))
          s_max=max(str_max(k1),str_max(k2))
          s_min=max(1,s_min-n_shower)
          s_max=min(150,s_max+n_shower)
          e1=smd_sum(k1)
          e2=smd_sum(k2)
          n_iter=0
 30       continue
          e_old=e1
          n_iter=n_iter+1
          do ihit=s_min,s_max
            if(strip(ihit).eq.0) go to 40
            i=ihit-str_max(k1)
            j=ihit-str_max(k2)
            if(abs(i).le.2.and.abs(j).le.2) then
              sigma=sigma_shower_shape(i,e1)**2+sigma_shower_shape(j,e2)**2
              sigma=sigma*strip(ihit)
            elseif(abs(i).le.2.and.abs(j).gt.2) then
              sigma=sigma_shower_shape(i,e1)**2*strip(ihit)
            elseif(abs(i).gt.2.and.abs(j).le.2) then
              sigma=sigma_shower_shape(j,e2)**2*strip(ihit)    
            endif
            a1=a1+shower_shape(ihit-str_max(k1),0.)**2/sigma
            a2=a2+shower_shape(ihit-str_max(k1),0.)*
     *         shower_shape(ihit-str_max(k2),0.)/sigma   
            b2=b2+shower_shape(ihit-str_max(k2),0.)**2/sigma   
            c1=c1+strip(ihit)*shower_shape(ihit-str_max(k1),0.)/sigma
            c2=c2+strip(ihit)*shower_shape(ihit-str_max(k2),0.)/sigma
 40     enddo
          b1=a2
          a12=a1*b2-b1*a2
          e1=(c1*b2-c2*b1)/a12          
          e2=(a1*c2-a2*c1)/a12
        endif
        if(e1.le.0..or.e2.le.0.) return
        if(abs(e_old-e1)/e1.gt.0.05.and.n_iter.lt.iter) goto 30
        iret=0
        smd_sum(k1)=e1
        smd_sum(k2)=e2
 50     continue
c
        end
c
      real function sampl_smde(x)

      implicit none
c 
      real x
c      real p(3) /0.097194,0.88268,0.11036/
      real p(3) /0.29645,1.2800,0.091063/
c
      sampl_smde=p(1)+p(2)*x+p(3)*x*x
c
      end
c
      real function sampl_smdp(x)
c
      implicit none
c 
      real x
c      real p(3) /0.17460,0.8944,0.071163/
      real p(3) /0.44463,1.0085,0.089567/
c
      sampl_smdp=p(1)+p(2)*x+p(3)*x*x
c
      end
c
      real function sigma_shower_shape(i,e)
c---fluctuation of lateral shower shape in SMD over shower_shape 
c---vs number of strips (-2,-1,0,1,2), max in 0
      implicit none
      real sigma(-2:2),e,e_old
      real p0(3)/0.20987,0.058035,0.31281/
      real p1(3)/0.84555,0.62491,0.84314/
      real p2(3)/1.5412,1.3534,0.60289/
      integer i
      save e_old,sigma
c
      if(e.ne.e_old) then
        sigma(-2)=p2(1)+p2(2)/e**p2(3)
        sigma(-1)=p1(1)+p1(2)/e**p1(3)
        sigma(0) =p0(1)+p0(2)/e**p0(3)
        sigma(1) =sigma(-1)
        sigma(2) =sigma(-2)
        e_old=e
      endif
      sigma_shower_shape=sigma(i)
c
      end
c
      real function shower_shape(i,mpoint)
c---relative lateral shower shower shape
      implicit none
      integer i
      real p(4)/0.62215,0.52124,0.06036,1.8960/
      real point,mpoint
c
      point=abs(float(i)-mpoint)
      shower_shape=p(1)*exp(-point/p(2))+p(3)*exp(-point/p(4))
c
      end    
      real function detadz(r,x)
c
      implicit none
c
      real r,x,w

c
      if(x.ne.0.) then
        w=atan2(r,x)/2.
        detadz=r/tan(w)/cos(w)**2/2./(1.+(r/x)**2)/x**2
      else
        detadz=0.
      endif
c
      end
c
      real function dphidx(r,c,x)
c
      implicit none
c
      real r,c,x
c
      dphidx=1./r/(1.+(c-x)**2/r**2)
c
      end
c





      
      






















