c:>------------------------------------------------------------------
C:FILE:         emc_cl_finder.F
C:DESCRIPTION:  EMC Analysis Module, package ems, STAF compliant.
c:AUTHOR:       K. Shestermanov, IHEP Protvino
C:BUGS:         None known.
C:HISTORY:      31jul96-v1.0-wjl- Creation.
C:                     30oct96 T. LeCompte added some bounds checking
c:              create working variant for gstaf 23March97 K. Shestermanov
c:>------------------------------------------------------------------
      INTEGER*4 FUNCTION EMC_CL_FINDER(
     1        emc_energy_h,             emc_energy ,
     2       ems_control_h,            ems_control ,
     3   emc_cluster_par_h,        emc_cluster_par ,
     4         calb_calg_h,              calb_calg ,
     5         calb_calr_h,              calb_calr ,
     6       emc_cluster_h,             emc_cluster,
     7      emc_cluster_index_h,  emc_cluster_index )
c:>------------------------------------------------------------------
C:ROUTINE:      INTEGER*4 emc_cl_finder
C:DESCRIPTION:  looks for cluster in barrel EMC only
C:ARGUMENTS:    emc_energy,ems_control,emc_cluster_par,calb_calg,calb_calr
C:ARGUMENTS:    emc_cluster,emc_cluster_index
C:RETURN VALUE: STAFCV_OK on successful completion
c:>------------------------------------------------------------------
      IMPLICIT NONE

#include "emc_cl_finder.inc"
#include "emc_def.h"
#include "math_constants.inc"
#include "phys_constants.inc"
c
      integer nsh
      parameter (nsh=250)
      common/cluster/ncluster, ns_cells(nsh), p_cluster(nsh,5), 
     & et_tot(120,40),pt_sum_out(nsh),et_tot_sim(120,40)
c
      integer ncluster, ns_cells
      integer nc, eta_b, phi_b,itwr,phi_smd
c----------------------------------------------------------
      real energy,energy_emc,s_energy,isol,phil,phis,dphi
      real p_cluster,pt_sum_out
      real e_corr,c_rphi,phi_corr
      real eta,eta0,phi,et_tot,et_tot_sim,phi0,dphim,dphir,rad_smd_p
      real s_phi,s_eta,detam,detar,eta_corr,sigma_eta,sigma_phi
      integer n_tower_eta,n_tower_phi,det,sub,module,out_num,i,p_bemc
      real bemc_max_eta,energy_cut,et_seed,r_shower,r_isol
      real phi_smd_tower,xcor,emc_res_a,emc_res_b,emc_res_c
      logical first /.true./
      integer iprin,ISLFLAG
c----------------------------------------------------------------------
      save energy_cut,et_seed,r_shower,r_isol,bemc_max_eta,rad_smd_p
      save c_rphi,emc_res_a,emc_res_b,emc_res_c
c
c---- start executable statements..
      write(*,*) 'emc_cl_funder:starting...'
      emc_cl_finder = STAFCV_BAD
c
      iprin=ISLFLAG('EMCC','PRIN')
      if(first) then
        energy_cut=emc_cluster_par(1).energy_cut
        et_seed=emc_cluster_par(1).et_seed
        r_shower=emc_cluster_par(1).r_shower
        r_isol=emc_cluster_par(1).r_isol
        emc_res_a=emc_cluster_par(1).emc_res_a
        emc_res_b=emc_cluster_par(1).emc_res_b
        emc_res_c=emc_cluster_par(1).emc_res_c
c
        bemc_max_eta = calb_calg(1).etacut
        rad_smd_p = calb_calr(1).rsmd2
        c_rphi=2.*(calb_calg(1).sphiwdh*calb_calg(1).nphistr/2.+
     *         calb_calg(1).sphidwdh*(calb_calg(1).nphistr/2))
        first=.false.
      endif
      ncluster = 0
      call vzero(et_tot,120*40)
      call vzero(et_tot_sim,120*40)
c
c---- Load Neutral Energy and total Energy
c
      do itwr = 1,emc_energy_h.nok
        if(emc_energy(itwr).energy.ne.0.) then
          eta=emc_energy(itwr).eta
          phi=emc_energy(itwr).phi
          eta_b=n_tower_eta(eta)
          phi_b=n_tower_phi(phi) 
          et_tot(phi_b,eta_b) = emc_energy(itwr).energy_twr/cosh(eta)
          et_tot_sim(phi_b,eta_b) =emc_energy(itwr).energy/cosh(eta)
        endif
      enddo 
c
      ncluster = 0
      call efind(energy_cut,et_seed,r_shower,r_isol)
c
c----------------------------------------------------------------
      if(ncluster .le. 0) go to 999
c----------------------------------------------------------------
c
      emc_cluster_index_h.nok=0
      emc_cluster_h.nok=0
      do i=1,ncluster
        eta = p_cluster(i,3)                           ! weighted eta
        phi = p_cluster(i,4)                           ! weighted phi
        if(phi.lt.0.) phi=C_2PI+phi
        if(abs(eta).lt.bemc_max_eta.and. 
     *     p_cluster(i,5)*cosh(eta).gt.energy_cut) then
          eta_b=n_tower_eta(eta)
          phi_b=n_tower_phi(phi)
          phi_smd=((phi_b-1)/2)+1
          phi0=phi_smd_tower(phi_smd)
          if(phi-phi0.ge.6.23082543) phi0=C_2PI
          dphim=phi-phi0
          dphir=phi_corr(abs(dphim))
          phi=phi0+sign(dphir,dphim)
          xcor=rad_smd_p*tan(phi0-phi)
          s_phi=sigma_phi(abs(dphim))
          eta0=p_cluster(i,1)-0.025    
          detam=eta-eta0
          detar=eta_corr(abs(detam))
          eta=eta0+sign(detar,detam)
          s_eta=sigma_eta(abs(detam))
c
          energy_emc = p_cluster(i,5)*cosh(eta)
          s_energy =emc_res_a*energy_emc+emc_res_b+emc_res_c*sqrt(energy_emc)
          energy=e_corr(energy_emc,abs(eta),abs(xcor))
          s_energy=s_energy*energy/energy_emc
          isol = (pt_sum_out(i)-p_cluster(i,5))/p_cluster(i,5)
          call find_bin_emc(ems_control,phi,eta,det,module,eta_b,sub)
          if(det.eq.BEMC) then
            do itwr = 1,emc_energy_h.nok
              if(emc_energy(itwr).module.eq.module.and.
     *           emc_energy(itwr).ebin.eq.eta_b.and.
     *           emc_energy(itwr).sub.eq.sub) then
                p_bemc=itwr
                go to 10
              endif
            enddo
          endif 
 10       continue
c
c---fill out the emc_cluster table
c 
          emc_cluster_index_h.nok = emc_cluster_index_h.nok + 1
          emc_cluster_h.nok = emc_cluster_h.nok + 1
c          
          out_num =  emc_cluster_h.nok
          if(emc_cluster_index_h.maxlen.le.out_num.or.
     *       emc_cluster_h.maxlen.le.out_num) then
            return
          endif
          emc_cluster_index(out_num).det = BEMC
          emc_cluster_index(out_num).hit_id = p_bemc
          emc_cluster_index(out_num).cluster_id = out_num
          emc_cluster(out_num).det = BEMC
          emc_cluster(out_num).nhits = ns_cells(i)
          emc_cluster(out_num).eta = eta
          emc_cluster(out_num).sigma_eta = s_eta
          emc_cluster(out_num).phi = phi
          emc_cluster(out_num).sigma_phi = s_phi
          emc_cluster(out_num).energy_emc = energy_emc
          emc_cluster(out_num).energy = energy
          emc_cluster(out_num).s_energy = s_energy
          emc_cluster(out_num).ratio_isol = isol
c
        endif
      enddo
 999  continue
      emc_cl_finder = STAFCV_OK
      end
c=====================================================================
      subroutine efind(energy_cut,et_seed,r_shower,r_isol)
c---------------------------------------------------------------------
      implicit none
#include "math_constants.inc"
      integer nsh,dim
      parameter (nsh=250,dim=10000)
      integer ncluster,ns_cells
      real p_cluster,et_tot,pt_sum_out,et_tot_sim
      common/cluster/ncluster, ns_cells(nsh), p_cluster(nsh,5),et_tot(120,40),
     &            pt_sum_out(nsh),et_tot_sim(120,40) 
c
      integer ij,ic, nj,icmax,ijmax,i_cluster,ietph
      integer i,j,n,nc
      integer nce2
      integer k(dim,2)
      integer n_iter,n_r_int
      real p(dim,5)
      real p_sim(dim)
      real etmax,eta,phi,dphia,phic,e_cell
      real prec,r_int,eta_old,phi_old,eta_b,phi_b,esum_out
      real eta_tower,phi_tower
      data prec/0.05/
c
      real energy_cut,et_seed,r_shower,r_isol
      integer nphi_bemc,neta_bemc
      data nphi_bemc,neta_bemc/120,40/
c
c-------------------------------------------------------------------
c   to work below we should get now:
c            n_hit_cells - number of hit cells
c   for each hit cell:
c            p(i,5)=sum(p_T)    K(i,1) - packed cell number
c            p(i,1)=eta_i       K(i,2) - number of hits in a cell
c            p(i,2)=phi_i
c=====================================================================
c            p(i,1),p(i,2) - (eta,phi) position of center
c            of cluster initiator, i.e. geometrical center of cluster.
c            p(i,3),p(i,4) - (eta,phi) position of E_T weighted
c            center of cluster, i.e. center of gravity of cluster;
c            p(i,5) = sum E_T of cluster.
c----------------------------------------------------------------------------
c  p(i,5), i= :   1..........n..(n+1).......nc..(nc+1)..........nj
c  k(i,1), i= :   particles      hit_cells        cluster seeds
c----------------------------------------------------------------------
      n = 4800
      ncluster = 0
      n_iter=-1
c
      call vzero(p_sim,dim)
      call vzero(k,dim*2)
      call vzero(p,dim*5)
      call vzero(ns_cells,nsh)
      call vzero(pt_sum_out,nsh)
c
      n_iter=n_iter+1
      nc = n
      do j=1,120
        do i=1,40
          nc = nc + 1
          ietph = nphi_bemc*i + j
          k(nc,1) = ietph
          eta=eta_tower(i)
          phi=phi_tower(j)
          p(nc,5) = et_tot(j,i)
          p(nc,1) = eta
          p(nc,2) = phi
          p_sim(nc) = et_tot_sim(j,i) 
        enddo
      enddo
c---------------------------------------------------------------
      if(nc .eq. n) return
c---------------------------------------------------------------
      nce2=2*120*40
      ncluster = 0
c...find initiator cell, the one with highest pT of not yet used ones
      nj=nc
  140 etmax=0.
      do 150 ic=n+1,nc
        if(k(ic,1).eq.0.or.k(ic,1).gt.nce2) goto 150
        if(p(ic,5).le.etmax) goto 150
        icmax=ic
        eta=p(ic,1)
        phi=p(ic,2)
        etmax=p(ic,5)
  150 continue
c
      if(etmax.lt.et_seed) goto 210 ! Less than min. seed parameter
c
      k(icmax,1)=k(icmax,1)+nce2
      nj=nj+1
      k(nj,1)=1
      k(nj,2)=0
      p(nj,1)=eta
      p(nj,2)=phi
      p(nj,3)=0.
      p(nj,4)=0.
      p(nj,5)=0.
c
      eta_b=eta
      phi_b=phi
c  
c-weighted eta and phi
c
      n_r_int=0
 151  eta_old=eta
      phi_old=phi
      n_r_int=n_r_int+1
      if(n_r_int.gt.10) then
        eta=eta_b
        phi=phi_b
        go to 153
      endif 
      do 152 ic=n+1,nc
        if(k(ic,1).eq.0) goto 152
        if(abs(p(ic,1)-eta).gt.r_shower) goto 152 !Outside cone radius
        dphia=abs(p(ic,2)-phi)
        if(dphia.gt.r_shower.and.dphia.lt.C_2PI-r_shower) goto 152
        phic=p(ic,2)
c        if(dphia.gt.C_PI) phic=phic+sign(C_2PI,phi)
c***
c        if(dphia.gt.C_PI) then
c          phic=phic-sign(C_2PI,phi)
c          write(*,884)phic,dphia
c 884      format(1x,'phic-3,dphia'2f9.5)
c        endif
c***
        if(dphia.ge.C_2PI-r_shower) then
          if(phic.le.r_shower) then
            phic=phic+sign(C_2PI,phi)
          else
            phic=phic-sign(C_2PI,phi)
          endif
        endif
c***
        if((p(ic,1)-eta)**2+(phic-phi)**2.gt.r_shower**2) goto 152
        p(nj,3)=p(nj,3)+p(ic,5)*p(ic,1)
        p(nj,4)=p(nj,4)+p(ic,5)*phic
        p(nj,5)=p(nj,5)+p(ic,5)
  152 continue
c
      if(p(nj,5)*cosh(p(nj,3)/p(nj,5)).lt.energy_cut) go to 153
      eta=p(nj,3)/p(nj,5)
      phi=p(nj,4)/p(nj,5)
      if(abs(phi).gt.C_PI) phi=phi-sign(C_2PI,phi)
      r_int=sqrt((eta_old-eta)**2+(phi_old-phi)**2)
      if(r_int.ge.0.5*r_shower) then
        eta=eta_b
        phi=phi_b
        go to 153
      endif 
      p(nj,3)=0.
      p(nj,4)=0.
      p(nj,5)=0.
      if(r_int.gt.prec*r_shower) go to 151
 153  continue 
c...    sum up unused cells within required distance of initiator
      p(nj,3)=0.
      p(nj,4)=0.
      p(nj,5)=0.
c
      ns_cells(nj-nc)=0 
      esum_out=0.
      do 160 ic=n+1,nc
        if(k(ic,1).eq.0) goto 160
        if(abs(p(ic,1)-eta).gt.r_shower) goto 154 ! Outside cone radius
        dphia=abs(p(ic,2)-phi)
        if(dphia.gt.r_shower.and.dphia.lt.C_2PI-r_shower) goto 154
        phic=p(ic,2)
c        if(dphia.gt.C_PI) phic=phic+sign(C_2PI,phi)
c***
c        if(dphia.gt.C_PI) phic=phic-sign(C_2PI,phi)
c***
        if(dphia.ge.C_2PI-r_shower) then
          if(phic.le.r_shower) then
            phic=phic+sign(C_2PI,phi)
          else
            phic=phic-sign(C_2PI,phi)
          endif
        endif
c
        if((p(ic,1)-eta)**2+(phic-phi)**2.gt.r_shower**2) goto 154
c        if(p(ic,5)*cosh(p(ic,1)).gt.0.01*energy_cut) then
        if(p(ic,5)*cosh(p(ic,1)).gt.0.0) then
          ns_cells(nj-nc)=ns_cells(nj-nc)+1
        endif
        k(ic,1)=-k(ic,1)
        k(nj,2)=k(nj,2)+k(ic,2)
        p(nj,3)=p(nj,3)+p(ic,5)*p(ic,1)
        p(nj,4)=p(nj,4)+p(ic,5)*phic
        p(nj,5)=p(nj,5)+p(ic,5)
        pt_sum_out(nj-nc)=pt_sum_out(nj-nc)+p(ic,5)
        go to 160
 154    continue
        if(abs(p(ic,1)-eta).gt.r_isol) goto 160  ! Outside cone radius
        dphia=abs(p(ic,2)-phi)
        if(dphia.gt.r_isol.and.dphia.lt.C_2PI-r_isol) goto 160
        phic=p(ic,2)
        if(dphia.gt.C_PI) phic=phic+sign(C_2PI,phi)
        if((p(ic,1)-eta)**2+(phic-phi)**2.gt.r_isol**2) goto 160
        pt_sum_out(nj-nc)=pt_sum_out(nj-nc)+p_sim(ic)
 160  continue
c
c... reject cluster below minimum Ej_min, else accept
c
      if(p(nj,5)*cosh(p(nj,3)/p(nj,5)).lt.energy_cut) then
        nj=nj-1
c
        do 170 ic=n+1,nc
 170    if(k(ic,1).lt.0) k(ic,1)=-k(ic,1)
      else
        p(nj,3)=p(nj,3)/p(nj,5)
        p(nj,4)=p(nj,4)/p(nj,5)
        if(abs(p(nj,4)).gt.C_PI) p(nj,4)=p(nj,4)-sign(C_2PI,p(nj,4))
        do 180 ic=n+1,nc
 180    if(k(ic,1).lt.0) k(ic,1)=0
c        do 190 j=1,4
c 190    p(nj,j)=0.
c        do 200 ic=n+1,nc
c          if(k(ic,1).ge.0) goto 200
c          p(nj,1)=p(nj,1)+p(ic,5)*cos(p(ic,2))
c          p(nj,2)=p(nj,2)+p(ic,5)*sin(p(ic,2))
c          p(nj,3)=p(nj,3)+p(ic,5)*sinh(p(ic,1))
c          p(nj,4)=p(nj,4)+p(ic,5)*cosh(p(ic,1))
c          k(ic,1)=0
c 200    continue
      endif
      goto 140
c...  arrange clusters in falling ET sequence
c
 210  do 230 i=1,nj-nc
        etmax=0.
        do 220 ij=nc+1,nj
          if(k(ij,1).eq.0) goto 220
          if(p(ij,5).lt.etmax) goto 220
          ijmax=ij
          etmax=p(ij,5)
 220      continue
        k(ijmax,1)=0
        k(n+i,1) = i
        k(n+i,2) = k(ijmax,2)
        do 230 j=1,5
 230  p(n+i,j) = p(ijmax,j)
      ncluster = nj - nc
c      ncluster=ncluster-7
      if(ncluster .eq. 0) return
      do i_cluster=1,ncluster
        i = nc + i_cluster
        p_cluster(i_cluster,1) = p(i,1)
        p_cluster(i_cluster,2) = p(i,2)
        p_cluster(i_cluster,3) = p(i,3)
        p_cluster(i_cluster,4) = p(i,4)
        p_cluster(i_cluster,5) = p(i,5)
      enddo
      return
      end
c---
      integer function nmod(i)
c---function find number of module for eta < 0
c
      integer i
c
      nmod=i
      if(i.gt.60.and.i.le.115) then
        nmod=116-i
      elseif(i.gt.115) then
        nmod=176-i
      endif
c
      end
c---        
      real function phi_smd_tower(phi_bin)
c---function find angle phi(MARS) the center of the tower
      implicit none
c
      integer phi_bin,i
      real phi(60)
      logical first/.true./
      save phi
c
      if(first) then
        do i=1,60
         if(i.lt.14) then
           phi(i)=1.361356817-0.104719755*float(i)
         else
           phi(i)=7.644542124-0.104719755*float(i)
         endif
        enddo
        first=.false.
      endif
        phi_smd_tower=phi(phi_bin)
      end
c
      subroutine smd_st_sum(eta,num)
c---find first and last strip number for given eta-number of tower
      implicit none
c
      integer num
      integer num_str_n_e /75/
      real rad_smd_e /231.32/
      real wstr(2) /1.5367,1.9609/
      real smetawdh /0.9806/
      real eta,z,shift(2)
c
      eta=max(eta,-0.95)
      eta=min(eta,0.95)
      z=rad_smd_e*sinh(abs(eta))
      shift(1)=2.*smetawdh
      shift(2)=2.*smetawdh+wstr(1)*num_str_n_e
      if(z.le.shift(2)) then
        num=int((z-shift(1))/wstr(1))+1
      else 
        num=int((z-shift(2))/wstr(2))+num_str_n_e+1 
      endif
        if(num.gt.150) num=150
      end
c
      real function phi_tower(phi_bin)
c---function find angle phi(MARS) the center of the tower
      implicit none
c
      integer phi_bin,i
      real phi(120)
      logical first/.true./
      save phi
c
      if(first) then
        do i=1,120
         if(i.lt.26) then
           phi(i)=1.335176878-0.052359877*float(i)
         else
           phi(i)=7.618362185-0.052359877*float(i)
         endif
        enddo
        first=.false.
      endif
        phi_tower=phi(phi_bin)
      end
      real function eta_tower(eta_bin)
c
      integer eta_bin
c
      eta_tower=float(eta_bin)*0.05-1.025
      end
c
      integer function n_tower_eta(eta)
c
      real eta
      integer eta_bin
c
      if(eta.gt.0) then               ! Positive eta's in bins 21 -> 40.
        eta_bin  = INT(20.0*eta) + 21
        if(eta_bin.gt.40) eta_bin = 40
      else                            ! Negative eta's in bins 01 -> 20
        eta_bin  = 20 - INT(20.0*abs(eta))
      endif
      n_tower_eta=eta_bin
      end
c
      integer function n_tower_phi(phi)
c---calculate number of tower as function of angle phi
      implicit none
c
      real phi,phi_s
c
      phi_s=phi
      if(phi.lt.0.) phi_s=phi+6.283185307
      phi_s=phi_s-1.308996939
      if(phi_s.gt.0.) then
        n_tower_phi=120-int(phi_s/0.05235988) 
      else
        n_tower_phi=int(abs(phi_s)/0.05235988)+1
      endif
      end
c
      real function e_corr(e,x,y)
c--- correction function for energy e or gamma with r=0.055
      implicit none
      real e,x,y,par1,par2,par3
      real p(6) /1.0643,-0.017877,-0.0023002,0.45487e-03,0.27629e-04,
     *           -0.17464e-05/
      real par4 /1.0027/
      real par5 /-0.225e-02/ 
      real pp1/14.975/
      real pp2/1.3111/
      real ps1/14.69/
      real ps2/-0.1022/
      real ps3/0.7484/
      real pfc/0.96/
      real rps
c
      rps = (pp1+pp2*x*x)/(ps1+ps2*x+ps3*x*x)
      par1=p(1)+p(2)*x
      par2=p(3)+p(4)*x
      par3=p(5)+p(6)*x

      e_corr=pfc*rps*e*(par1+par2*y**2+par3*y**4)*(par4+par5*e)
c
      end
c
      real function phi_corr(phim)
c---correction phi, from EMC. NIM 140(1977)441
      implicit none     
      real p(4) /0.430728e-03,0.689429,0.0220683,1.06557/
      real phim
c
      if(phim.le.0.02617994) then
        phi_corr=p(1)*phim**p(2)/(p(3)-phim**p(4))
      else
        phim=0.05235988-phim
        phi_corr=0.05235988-p(1)*phim**p(2)/(p(3)-phim**p(4))
      endif
c
      end
c
      real function eta_corr(etam)
c---correction eta, from EMC. NIM 140(1977)441
      implicit none     
      real p(4) /0.32635E-03,0.72803,0.19179E-01,1.0848/
      real etam
c
      if(etam.le.0.025) then
        eta_corr=p(1)*etam**p(2)/(p(3)-etam**p(4))
      else
        etam=0.05-etam
        eta_corr=0.05-p(1)*etam**p(2)/(p(3)-etam**p(4))
      endif
c
      end
c
      real function sigma_eta(x)
c
      implicit none
c
      real p(3)/-10.325,-0.39066E-01,0.59388E-02/ 
      real s/0.00004/
      real s1/2.5/
      real s2/0.025/
      real a,x,eta_corr
c
      if(x.le.0.025) then
        a=p(1)*x**2-p(2)*x+p(3)
      else
        x=0.05-x
        a=p(1)*x**2-p(2)*x+p(3)
      endif
      a=a*(eta_corr(x+s)-eta_corr(x))/s*s1
      if(a.gt.s2) a=s2
      sigma_eta=a
c
      end
c
      real function sigma_phi(x)
c
      implicit none
c
      real p(3)/-9.325,-0.39066E-01,0.59388E-02/ 
      real s/0.00008/
      real s1/2.8/
      real s2/0.025/
      real a,x,phi_corr
c
      if(x.le.0.02617994 ) then
        a=p(1)*x**2-p(2)*x+p(3)
      else
        x=0.05235988-x
        a=p(1)*x**2-p(2)*x+p(3)
      endif
      a=a*(phi_corr(x+s)-phi_corr(x))/s*s1
      if(a.ge.s2) a=s2
      sigma_phi=a
c
      end









      
      






















