      subroutine UA1(sum_down,cone_rad,et_seed,ej_min,et_min,ierror)
c:>------------------------------------------------------------------
C:FILE:         subroutine part of ems_erj.F
C:DESCRIPTION:  EMC Analysis Module, package ems, STAF compliant.
c:AUTHOR:       W.J. Llope, Rice University (TAS Version by B. Christie)
C:BUGS:         None known.
C:HISTORY:      31jul96-v1.0-wjl- Creation.
c:>------------------------------------------------------------------
c:>------------------------------------------------------------------
C:ROUTINE:      subroutine UA1
C:DESCRIPTION:  UA1 jet algorithm from LUND JETSET called from ems_erj
C:ARGUMENTS:    sum_down,cone_rad,et_seed,ej_min,et_min,ierror
C:ARGUMENTS:    jet info returned to ems_erj via commons
C:RETURN VALUE: ierror=1 on error
c:>------------------------------------------------------------------
      integer i,j,ierror
      common/hadr/xyz(10000,3),p_hadr(10000,5),de(5,5),q_hadr(10000),
     &           m_hadr(10000),absp0,px0,py0,pz0,eta0,ph0,th0
      common/jets/njet, ns_cells(100), p_jet(100,5),et_tot(120,40)
      real et_min
      integer ij,ic, nj,njet,icmax,ijmax,i_jet,ietph,ns_cells
      integer n,nc,njet_bar,i_eta,i_phi
      integer nce2,mste24,mste25,mste26
      integer k(10000,2)
      integer n_sum_cell,n_iter,n_r_int
      real p(10000,5),p_hadr,xyz,q_hadr,m_hadr
      real p_jet
      real pare35,pare36,pare37,pare38
      real pi,twopi
      real etmax,eta,phi,phi2,dphia,phic,e_cell
      real calib,et_tot
      real prec,r_int,eta_old,phi_old,eta_b,phi_b
      real cone_rad,et_seed,ej_min
      real absp0,px0,py0,pz0,ph0,eta0,th0,de
      real et_sum_tot,et_sum_cell,et_sum_old,et_prec
      logical sum_down

      pi    = ACOS(-1.0)
      twopi = 2.0*pi
      if(sum_down)then
       mste24 = 20
       mste25 = 60
       prec   = 0.05
      else
       mste24 = 40
       mste25 = 120
       prec   = 0.025
      end if
      mste26  = 1
      pare35  = 1.0
      ierror  = 0

c-------------------------------------------------------------------
c   to work below we should get now:
c            n_hit_cells - number of hit cells
c   for each hit cell:
c            p(i,5)=sum(p_T)    K(i,1) - packed cell number
c            p(i,1)=eta_i       K(i,2) - number of hits in a cell
c            p(i,2)=phi_i
c=====================================================================
c                           if MSTE26 = 1 :
c                         p(i,1),p(i,2) - (eta,phi) position of center
c                         of jet initiator, i.e. geometrical center of jet.
c                         p(i,3),p(i,4) - (eta,phi) position of E_T weighted
c                         center of jet, i.e. center of gravity of jet;
c                         p(i,5) = sum E_T of jet.
c                           if MSTE26 = 2 :
c                         p(i,1)...p(i,5) = jet momentum vector constructed
c                         of summed E_T and (eta,phi)  of E_T weighted
c                         center of jet: (px,py,pz,e,M)=E_T*(cos phi,sin phi,
c                         sinh eta, cosh eta, 0.).
c                           if MSTE26 = 3 :
c                         p(i,1)...p(i,5) = jet momentum vector constructed
c                         by adding vectorially momentum of each cell in jet.
c                         Jet MASS in p(i,5)=E2-px2-py2-pz2.
c----------------------------------------------------------------------------
c                         p(i,5) = sum of E_T of jet.
c----------------------------------------------------------------------------
C      pare35 = 1.0     !  max /eta/  for detector                     (1.0)
c----------------------------------------------------------------------
c  p(i,5), i= :   1..........n..(n+1).......nc..(nc+1)..........nj
c  k(i,1), i= :   particles      hit_cells        jet seeds
c----------------------------------------------------------------------
      n = 4800
      if(sum_down)n = 1200
      njet = 0
      njet_bar = 0
      n_iter=-1
      do i=1,10000
        k(i,1)=0
        k(i,2)=0
        do j=1,5
          p(i,j)=0.0
        enddo
      enddo
      do i=1,100
        ns_cells(i)=0
      enddo
      et_sum_tot=0. 
      do j=1,120
        do i=1,40
          e_cell = et_tot(j,i)
          et_sum_tot=et_sum_tot+e_cell 
        enddo
      enddo
      et_sum_cell=et_sum_tot/float(n)
      et_sum_old=et_sum_cell
 130  continue
      n_iter=n_iter+1
      nc = n
      do j=1,120
        do i=1,40
          e_cell = et_tot(j,i)
          nc = nc + 1
          ietph = mste25*i + j
          k(nc,1) = ietph
          if((sum_down.and.(j.le.30)).OR.
     1       ((.not.sum_down).and.(j.le.60)))then
            phi =(   1.5+(float(j)- 1.)*3.0)*twopi/360.
            phi2=(   3.0+(float(j)- 1.)*6.0)*twopi/360.
          else
            phi =(-178.5+(float(j)-61.)*3.0)*twopi/360.
            phi2=(-177.0+(float(j)-31.)*6.0)*twopi/360.
          endif
          if(sum_down)then
           eta =  -0.950 + (float(i)-1.)*0.10
           phi = phi2
          else
           eta =  -0.975 + (float(i)-1.)*0.05
          end if
          p(nc,5) = e_cell-et_sum_cell 
          p(nc,1) = eta
          p(nc,2) = phi
        enddo
      enddo
c---------------------------------------------------------------
      if(nc .eq. n) return
c---------------------------------------------------------------
      nce2=2*mste24*mste25

      njet = 0
      nj=nc

c...find initiator cell, the one with highest pT of not yet used ones
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
      if(etmax.lt.et_seed) goto 209 ! Less than min. seed parameter
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

      n_r_int=0
 151  eta_old=eta
      phi_old=phi
      n_r_int=n_r_int+1
      if(n_r_int.gt.10) then
        eta=eta_b
        phi=phi_b
        go to 159
      endif 
      do 158 ic=n+1,nc
        if(k(ic,1).eq.0) goto 158
        if(abs(p(ic,1)-eta).gt.cone_rad) goto 158  ! Outside cone radius
        dphia=abs(p(ic,2)-phi)
        if(dphia.gt.cone_rad.and.dphia.lt.twopi-cone_rad) goto 158
        phic=p(ic,2)
        if(dphia.gt.pi) phic=phic+sign(twopi,phi)
        if((p(ic,1)-eta)**2+(phic-phi)**2.gt.cone_rad**2) goto 158
        p(nj,3)=p(nj,3)+p(ic,5)*p(ic,1)
        p(nj,4)=p(nj,4)+p(ic,5)*phic
        p(nj,5)=p(nj,5)+p(ic,5)
  158 continue
c
      if(p(nj,5)*cosh(p(nj,3)/p(nj,5)).lt.ej_min) go to 159
      eta=p(nj,3)/p(nj,5)
      phi=p(nj,4)/p(nj,5)
      if(abs(phi).gt.pi) phi=phi-sign(twopi,phi)
      r_int=sqrt((eta_old-eta)**2+(phi_old-phi)**2)
      if(r_int.ge.0.5*cone_rad) then
        eta=eta_b
        phi=phi_b
        go to 159
      endif 
      p(nj,3)=0.
      p(nj,4)=0.
      p(nj,5)=0.
      if(r_int.gt.prec*cone_rad) go to 151
 159  continue 
c...    sum up unused cells within required distance of initiator
      p(nj,3)=0.
      p(nj,4)=0.
      p(nj,5)=0.
c
      ns_cells(nj-nc)=0 
      do 160 ic=n+1,nc
        if(k(ic,1).eq.0) goto 160
        if(abs(p(ic,1)-eta).gt.cone_rad) goto 160  ! Outside cone radius
        dphia=abs(p(ic,2)-phi)
        if(dphia.gt.cone_rad.and.dphia.lt.twopi-cone_rad) goto 160
        phic=p(ic,2)
        if(dphia.gt.pi) phic=phic+sign(twopi,phi)
        if((p(ic,1)-eta)**2+(phic-phi)**2.gt.cone_rad**2) goto 160
        if(p(ic,5).gt.0.) then
          ns_cells(nj-nc)=ns_cells(nj-nc)+1
        endif
        k(ic,1)=-k(ic,1)
        k(nj,2)=k(nj,2)+k(ic,2)
        p(nj,3)=p(nj,3)+p(ic,5)*p(ic,1)
        p(nj,4)=p(nj,4)+p(ic,5)*phic
        p(nj,5)=p(nj,5)+p(ic,5)
  160 continue
c
c... reject cluster below minimum Ej_min, else accept
c
      if(p(nj,5)*cosh(p(nj,3)/p(nj,5)).lt.ej_min) then     
        nj=nj-1

        do 170 ic=n+1,nc
  170     if(k(ic,1).lt.0) k(ic,1)=-k(ic,1)
      elseif(mste26.le.2) then
        p(nj,3)=p(nj,3)/p(nj,5)
        p(nj,4)=p(nj,4)/p(nj,5)
        if(abs(p(nj,4)).gt.pi) p(nj,4)=p(nj,4)-sign(twopi,
     &  p(nj,4))

        do 180 ic=n+1,nc
  180     if(k(ic,1).lt.0) k(ic,1)=0
      else
        do 190 j=1,4
  190     p(nj,j)=0.

        do 200 ic=n+1,nc
          if(k(ic,1).ge.0) goto 200
          p(nj,1)=p(nj,1)+p(ic,5)*cos(p(ic,2))
          p(nj,2)=p(nj,2)+p(ic,5)*sin(p(ic,2))
          p(nj,3)=p(nj,3)+p(ic,5)*sinh(p(ic,1))
          p(nj,4)=p(nj,4)+p(ic,5)*cosh(p(ic,1))
          k(ic,1)=0
  200   continue
      endif
      goto 140
c
  209 continue
      et_sum_tot=0.
      nc = n
      n_sum_cell=0
      do j=1,120
        do i=1,40
          i_eta = i
          i_phi = j
          if(sum_down)then
           i_eta = ((i_eta-1)/2) + 1
           i_phi = ((i_phi-1)/2) + 1
          end if
          e_cell = et_tot(i_phi,i_eta)
          nc = nc + 1
          if(k(nc,1).gt.0) then   
            et_sum_tot = et_sum_tot + e_cell 
            n_sum_cell = n_sum_cell + 1
          endif 
        enddo
      enddo
      et_sum_cell=et_sum_tot/float(n_sum_cell)
      et_prec=abs((et_sum_cell-et_sum_old)/et_sum_cell)   
      if(et_prec.gt.prec.and.n_iter.le.40) then
        et_sum_old=et_sum_cell               
        do i=1,10000
          k(i,1)=0
          k(i,2)=0
          do j=1,5
            p(i,j)=0.0
          enddo
        enddo            
        go to 130
      endif

c...  arrange clusters in falling ET sequence
  210 do 230 i=1,nj-nc
        etmax=0.
        do 220 ij=nc+1,nj
          if(k(ij,1).eq.0) goto 220
          if(p(ij,5).lt.etmax) goto 220
          ijmax=ij
          etmax=p(ij,5)
  220   continue
        k(ijmax,1) = 0
        k(n+i,1)   = i
        k(n+i,2)   = k(ijmax,2)

        do 230 j=1,5
  230     p(n+i,j) = p(ijmax,j)

        njet = nj - nc

c.. convert to massless or massive four-vectors
      if(mste26.eq.2) then
        do 240 i = n+1,n+njet
          eta = p(i,3)
          p(i,1)=p(i,5)*cos(p(i,4))
          p(i,2)=p(i,5)*sin(p(i,4))
          p(i,3)=p(i,5)*sinh(eta)
          p(i,4)=p(i,5)*cosh(eta)
  240   p(i,5)=0.
      elseif(mste26.ge.3) then
        do 250 i=n+1,n+njet
  250     p(i,5)=sqrt(max(0.,p(i,4)**2-p(i,1)**2-p(i,2)**2-p(i,3)**2))
      endif

c.. move  p(i,5)  to  p_jet(100,5)
      if(njet .eq. 0) return
      if(njet.gt.100)then
       write(*,*)'UA1 -- Problem: More than 100 jets found!'
       ierror = 1
       return
      end if

      do i_jet=1,njet
        i = nc + i_jet
        p_jet(i_jet,1) = p(i,1)
        phi = p(i,2)
        if(phi .lt. 0.0) phi=phi+twopi
        p_jet(i_jet,2) = phi
        p_jet(i_jet,3) = p(i,3)
        phi = p(i,4)
        if(phi .lt. 0.0) phi=phi+twopi
        p_jet(i_jet,4) = p(i,4)
        p_jet(i_jet,5) = p(i,5)
      enddo

      return
      end
