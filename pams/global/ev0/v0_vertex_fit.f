c==============================================================
c                                                             |               
c   This file should contain the following subroutines:       |              
c                                                             |               
c       TRK_V0_MAIN                                           |
c       V0_PTRS                                               |
c       V0_INTERSECT                                          |
c       V0_MASS                                               |
c       V0_REFIT                                              |
c       V0QMIN                                                |
c       V0QCOF                                                |
c       TRK_DUMMY_VTX                                         |
c       V0_PARAM_PROP                                         |
c                                                             |
c==============================================================


        options/extend_source
        SUBROUTINE TRK_V0_MAIN(HITS,TPC_SUM,V0_SW,V0,NOK_V0)

***************************************************************************
***************************************************************************
**                                                                       **
**  Input arguments:                                                     **
**  ----------------                                                     **
**    HITS     =  HITS table                                             **
**    TPC_SUM  =  TPC_SUM table (contains main vertex information)       **
**    V0_SW    =  V0_SW switch table                                     ** 
**                                                                       **
**  Output arguments:                                                    **
**  -----------------                                                    **
**    V0       =   V0 table                                              **
**    NOK_V0   =   number of V0 candidates written to V0 table           **
**                                                                       **
**  Functional Description:                                              **
**  -----------------------                                              **
**    Controls V0  finding. Called by TRK_MAIN.                          **
**                                                                       **
**  Created:    7-Mar-94                                                 **
**  Author:     M. Justice                                               **
**                                                                       **
**                                                                       **
**  PCHAIN = Helix parameters of fitted track at the most upstream       **
**           hit on the chain as determined from FLDMAP_FIT routine.     **
**           Passed through TRK_CHAINS the common block.                 **
**             1. x                                                      **
**             2. y                                                      **
**             3. sinp                                                   **
**             4. h/r                                                    **
**             5. tanl                                                   **
**             6. chisq                                                  **
**                                                                       **
**  RPB    = Postion and rigidity vectors corresponding to the PCHAIN    **
**           helix parameters. Passed through TRK_TRACKS common block.   **
**             1. x                                                      **
**             2. y                                                      **
**             3. z                                                      **
**             4. rx                                                     **
**             5. ry                                                     **
**             6. rz                                                     **
**                                                                       **
***************************************************************************
***************************************************************************

        IMPLICIT NONE

	INCLUDE 'tas_incl_path:tas_structures.inc'
	INCLUDE 'tas_incl_path:tas_table_pars.inc'
	INCLUDE 'tas_incl_path:tas_table_st.inc'

        INCLUDE 'tas_incl_path:trk_tracks.inc'
        INCLUDE 'tas_incl_path:trk_chains.inc'
 
        RECORD   /hits_row_st     / hits(hits_len)
        RECORD   /tpc_sum_row_st  / tpc_sum
        RECORD   /v0_row_st       / v0(v0_len)
        RECORD   /v0_sw_row_st    / v0_sw

        INTEGER nok_v0

        REAL dca
        COMMON /trk_dist/ dca(mxtrk)

	INTEGER ktype,ltype
	PARAMETER (ltype = 1, ktype = 2)

	REAL mp,mpi
	PARAMETER (mp = .93827, mpi = .13957)

	REAL qp,qpip,qpim
	PARAMETER (qp = +1.0, qpip = +1.0, qpim = -1.0)

	INTEGER i,j,k,l,itrk,jtrk,ierr
        INTEGER npim,npip,np
	INTEGER pim_ptr(mxtrk),pip_ptr(mxtrk),p_ptr(mxtrk)

 	REAL par(6,2),xv(3)
	REAL p1b(6),p2b(6),p1v(6),p2v(6)
	REAL p(3,2),spath,chi2,zdk
	REAL mass,pv0(3),xdk(3),xv0(3)
	REAL spxz,spyz,sbxz
	REAL dist,b,dely

c----------------------- Begin Execution -----------------------

        nok_v0  = 0

* First set up pointers to TRK_TRACKS common

	call v0_ptrs(ngtrk,ia,iz,npim,npip,np,pim_ptr,pip_ptr,p_ptr)

* Check to see if there is at least 1 pi minus

	if(npim.le.0) return

* Main vertex from TRK_VTX routines

	xv(1) = tpc_sum.xv
	xv(2) = tpc_sum.yv
	xv(3) = tpc_sum.zv

************************ Search for Lambda's ********************************

        do 20 i = 1,np                         ! loop over protons

          itrk  = p_ptr(i)
	  do k = 1,5
	    par(k,1) = pchain(k,itrk)          ! helix param's at 1st hit
	  enddo
          par(6,1) = rpb(3,itrk)               ! z at 1st hit on track

	  do 10 j = 1,npim                     ! loop over pi minus's

            jtrk    = pim_ptr(j)

* Cut on distance of closest approach

	    if(dca(jtrk) .gt. v0_sw.dca_cut) goto 10

	    do k = 1,5
	       par(k,2) = pchain(k,jtrk)
	    enddo
            par(6,2) = rpb(3,jtrk)        

* Find intersection of two trajectories (IERR .ne. 0 if they don't intersect)

	    call v0_intersect(par,v0_sw,xdk,dely,ierr)
	    if(ierr .ne. 0) goto 10

* Get mass and 4-vector momentum

	    if(v0_sw.refit .eq. 0)then
	      do k = 1,6
	        p1b(k) = rpb(k,itrk)           ! from TRK_TRACKS common
	        p2b(k) = rpb(k,jtrk)
	      enddo

* Propagate momentum vectors to z plane of vertex

	      call zplane_proj(p1b,xdk(3),qp,p1v,spath)
	      call zplane_proj(p2b,xdk(3),qpim,p2v,spath)

	      do k = 1,3
	       p(k,1) = p1v(k+3)
	       p(k,2) = p2v(k+3)
	      enddo

	    else              ! do 9 parameter fit
	      
	      zdk = xdk(3)
	      call v0_refit(hits,itrk,jtrk,zdk,xdk,p,chi2,ierr)
	      if(ierr .ne. 0) goto 10

	    endif

           if(xdk(3) .le. xv(3)) goto 10

* Cut on distance from main vertex

	    dist = sqrt((xdk(1)-xv(1))**2 + (xdk(2)-xv(2))**2 + 
     +               (xdk(3)-xv(3))**2)
	    if(dist .lt. v0_sw.dvtx) goto 10

* Get invariant mass and vector momentum

	    call v0_mass(p,mp,mpi,mass,pv0)

	    spxz = pv0(1)/pv0(3)
	    spyz = pv0(2)/pv0(3)

	    sbxz = -1./spxz

* Point of closest approach to main vertex

	    xv0(3) = (xdk(1)-xv(1)+sbxz*xv(3)-spxz*xdk(3))/(sbxz-spxz)
	    xv0(1) = xdk(1) + spxz*(xv0(3) - xdk(3))
	    xv0(2) = xdk(2) + spyz*(xv0(3) - xdk(3))

* Impact parameter

	    b = sqrt((xv0(1)-xv(1))**2 + (xv0(2)-xv(2))**2 + 
     +               (xv0(3)-xv(3))**2)

	    if(b .le. v0_sw.bmax) then           ! fill table
	       nok_v0 = nok_v0 + 1
	       v0(nok_v0).type     = ltype
	       v0(nok_v0).mass     = mass
	       v0(nok_v0).b        = b
	       v0(nok_v0).dist     = dist
	       v0(nok_v0).dely     = dely
               v0(nok_v0).trkid(1) = itrk
               v0(nok_v0).trkid(2) = jtrk
               v0(nok_v0).dca(1)   = dca(itrk)
               v0(nok_v0).dca(2)   = dca(jtrk)
	       do l = 1,3
	         v0(nok_v0).p(l)  = pv0(l)
	         v0(nok_v0).x(l)  = xdk(l)
	         v0(nok_v0).x0(l) = xv0(l)
	       enddo
	       v0(nok_v0).chi2    = chi2
               if(nok_v0 .eq. v0_len) return
	    endif	    

cmj	    print*,' '
cmj	    print*,'Type = ',ltype
cmj	    print*,'Mass = ',mass
cmj	    print*,'b    =',b
cmj	    print*,'dist =',dist
cmj	    print*,' '


 10	  enddo

 20	enddo


************************** Search for K0's **********************************


* Check to see if there is at least 1 pi plus

	if(npip.le.0) return

        do 40 i = 1,npip                       ! loop over pi plus's

          itrk       = pip_ptr(i)

* Cut on distance of closest approach

          if(dca(itrk) .gt. v0_sw.dca_cut) goto 40

	  do k = 1,5
	    par(k,1) = pchain(k,itrk)          ! helix param's at 1st hit
	  enddo
          par(6,1) = rpb(3,itrk)               ! z at 1st hit on track


	  do 30 j = 1,npim                     ! loop over pi minus's

            jtrk    = pim_ptr(j)

* Cut on distance of closest approach

	    if(dca(jtrk) .gt. v0_sw.dca_cut) goto 30

	    do k = 1,5
	       par(k,2) = pchain(k,jtrk)
	    enddo
            par(6,2) = rpb(3,jtrk)        ! z at target plane

* Find intersection of two trajectories (IERR .ne. 0 if they don't intersect)

	    call v0_intersect(par,v0_sw,xdk,dely,ierr)
	    if(ierr .ne. 0) goto 30

* Get mass and 4-vector momentum

	    if(v0_sw.refit .eq. 0)then
	      do k = 1,6
	        p1b(k) = rpb(k,itrk)           ! from TRK_TRACKS common
	        p2b(k) = rpb(k,jtrk)
	      enddo

* Propagate momentum vectors to z plane of vertex

	      call zplane_proj(p1b,xdk(3),qpip,p1v,spath)
	      call zplane_proj(p2b,xdk(3),qpim,p2v,spath)

	      do k = 1,3
	       p(k,1) = p1v(k+3)
	       p(k,2) = p2v(k+3)
	      enddo

	    else              ! do 9 parameter fit
	      
	      zdk = xdk(3)
	      call v0_refit(hits,itrk,jtrk,zdk,xdk,p,chi2,ierr)
	      if(ierr .ne. 0) goto 30

	    endif

            if(xdk(3) .le. xv(3)) goto 30

* Cut on distance from main vertex again

	    dist = sqrt((xdk(1)-xv(1))**2 + (xdk(2)-xv(2))**2 + 
     +               (xdk(3)-xv(3))**2)
	    if(dist .lt. v0_sw.dvtx) goto 30

* Get invariant mass and vector momentum

	    call v0_mass(p,mpi,mpi,mass,pv0)   

	    spxz = pv0(1)/pv0(3)
	    spyz = pv0(2)/pv0(3)

	    sbxz = -1./spxz

* Point of closest approach to main vertex

	    xv0(3) = (xdk(1)-xv(1)+sbxz*xv(3)-spxz*xdk(3))/(sbxz-spxz)
	    xv0(1) = xdk(1) + spxz*(xv0(3) - xdk(3))
	    xv0(2) = xdk(2) + spyz*(xv0(3) - xdk(3))

* Impact parameter

	    b = sqrt((xv0(1)-xv(1))**2 + (xv0(2)-xv(2))**2 + 
     +               (xv0(3)-xv(3))**2)

	    if(b .le. v0_sw.bmax) then           ! fill table
	       nok_v0 = nok_v0 + 1
	       v0(nok_v0).type     = ktype
	       v0(nok_v0).mass     = mass
	       v0(nok_v0).b        = b
	       v0(nok_v0).dist     = dist
	       v0(nok_v0).dely     = dely
               v0(nok_v0).trkid(1) = itrk
               v0(nok_v0).trkid(2) = jtrk
               v0(nok_v0).dca(1)   = dca(itrk)
               v0(nok_v0).dca(2)   = dca(jtrk)
	       do l = 1,3
	         v0(nok_v0).p(l)  = pv0(l)
	         v0(nok_v0).x(l)  = xdk(l)
	         v0(nok_v0).x0(l) = xv0(l)
	       enddo
	       v0(nok_v0).chi2    = chi2
               if(nok_v0 .eq. v0_len) return
	    endif	    

 30	  enddo

 40	enddo

	return
	end




        options/extend_source
	SUBROUTINE V0_PTRS(N,A,Z,NPIM,NPIP,NP,PIM_PTR,PIP_PTR,P_PTR)


***************************************************************************
***************************************************************************
**                                                                       **
**  Input arguments:                                                     **
**  ----------------                                                     **
**    A     =   atomic masses                                            **
**    Z     =   charges                                                  **
**                                                                       **
**  Output arguments:                                                    **
**  -----------------                                                    **
**    N         =   number of found tracks                               **
**    NPIM      =   number of pi-'s                                      **
**    NPIP      =   number of pi+'s                                      **
**    NP        =   number of protons                                    **
**    PIM_PTR   =   pi- pointer array                                    **
**    PIP_PTR   =   pi+ pointer array                                    **
**    P_PTR     =   proton pointer array                                 **
**                                                                       **
**  Functional Description:                                              **
**  -----------------------                                              **
**    Counts number of pi-'s, pip's, and protons. Sets up pointers to    **
**  TRK_CHAINS and TRK_TRACKS common blocks. Called by TRK_V0_MAIN.      ** 
**                                                                       **
**  Created:    7-Mar-94                                                 **
**  Author:     M. Justice                                               **
**                                                                       **
***************************************************************************
***************************************************************************

        IMPLICIT NONE

	INTEGER mxtrk
	PARAMETER (mxtrk = 500)

        INTEGER n,a(mxtrk),z(mxtrk),npim,npip,np,itrk
	INTEGER pim_ptr(mxtrk),pip_ptr(mxtrk),p_ptr(mxtrk)


c----------------------- Begin Execution -----------------------

	npim = 0
	npip = 0
	np   = 0

	do itrk = 1,n

	  if(a(itrk) .eq. 0)then

             if(z(itrk) .eq. -1)then
               npim          = npim + 1
               pim_ptr(npim) = itrk
             elseif(z(itrk) .eq. +1)then
               npip          = npip + 1
               pip_ptr(npip) = itrk
             endif

	  elseif(a(itrk) .eq. 1)then

             if(z(itrk) .eq. +1)then
               np          = np + 1
               p_ptr(np) = itrk
             endif

	  endif

	enddo

	return
	end



        options/extend_source
	SUBROUTINE V0_INTERSECT(PAR,V0_SW,XDK,DY,IERR)

***************************************************************************
***************************************************************************
**                                                                       **
**  Input arguments:                                                     **
**  ----------------                                                     **
**    V0_SW    = V0 switch table                                         **
**    PAR      = Helix parameters                                        **
**                 1. x_0                                                **
**                 2. y_0                                                **
**                 3. sin(phi_0)                                         **
**                 4. h/rho_0                                            **
**                 5. tanl_0                                             **
**                 6. z_0                                                **
**                                                                       **
**  Output arguments:                                                    **
**  -----------------                                                    **
**    XDK  =  x,y,z coordinates of intersection                          **
**    DY   =  delta y of helices at circle intersection                  **
**    IERR =  0 if routine successfully finds an intersection            **
**                                                                       **
**  Functional Description:                                              **
**  -----------------------                                              **
**                                                                       **
**                                                                       **
***************************************************************************
***************************************************************************

        IMPLICIT NONE

	INCLUDE 'tas_incl_path:tas_structures.inc'
	INCLUDE 'tas_incl_path:tas_table_pars.inc'
	INCLUDE 'tas_incl_path:tas_table_st.inc'

        RECORD   /v0_sw_row_st / v0_sw

	INTEGER i,ierr,jint

 	REAL    par(6,2)
	REAL    h(2),y0(2),rho(2),phi0(2),sinp0(2),cosp0(2),tanl(2)
	REAL    xc(2),zc(2),dist,phi_c
	REAL    dphi,cos_dphi,phi2,sinp2
	REAL    phi(2),xint(2),zint(2),y1(2),y2(2),dely(2)
	REAL    xdk(3),dy,delta_r

        REAL cpi,c2pi,cpi2
        COMMON /cnsts/ cpi,c2pi,cpi2


c----------------------- Begin Execution -----------------------

	ierr = 0

	do i = 1,2
	  h(i)     = sign(1.,par(4,i))
          y0(i)    = par(2,i)
	  rho(i)   = abs(1./par(4,i))
	  sinp0(i) = par(3,i)
	  cosp0(i) = sqrt(1. - sinp0(i)**2)
          phi0(i)  = asin(sinp0(i))
	  tanl(i)  = par(5,i)
          xc(i)    = par(1,i) - h(i)*rho(i)*cosp0(i)
          zc(i)    = par(6,i) - rho(i)*par(3,i)
	enddo

* Distance between circle centers

	dist = sqrt((xc(1)-xc(2))**2 + (zc(1)-zc(2))**2)

* Do circles overlap within criteria ?

	if((rho(1)+rho(2)) .lt. (dist - v0_sw.ctol))then
          ierr = 1
	  return
	endif

	if(abs((rho(1)-rho(2))) .gt. (dist + v0_sw.ctol))then
          ierr = 2
	  return
	endif

* Phi angle of line connecting centers ( -pi/2 .le. phi_c .le. pi/2)

	phi_c    = atan2((zc(2)-zc(1)),(xc(2)-xc(1)))

	if(abs(phi_c) .gt. cpi2)then
          ierr = 3
	  return
	endif

	if((rho(1)+rho(2)) .ge. dist) then   ! get intersection points

* Find angle of intersection points with respect to line connecting
* centers

	   cos_dphi = (dist**2 + rho(1)**2 - rho(2)**2)/(2.*rho(1)*dist)
	   if(abs(cos_dphi) .ge. 1)then
	    ierr = 4
	    return
	   endif

	   dphi     = acos(cos_dphi)

* Phi angles of two intersection points

	   phi(1) = phi_c + dphi
	   if(phi(1) .gt. cpi2)phi(1) = cpi - phi(1)

	   phi(2) = phi_c - dphi
	   if(phi(2) .lt. -cpi2)phi(2) = -cpi - phi(2)

	   do i = 1,2
	     xint(i)  = xc(1) + h(1)*rho(1)*abs(cos(phi(i)))
	     zint(i)  = zc(1) + rho(1)*sin(phi(i))
             y1(i)    = y0(1) + rho(1)*tanl(1)*(phi(i)-phi0(1))
             sinp2    = (zint(i) - zc(2))/rho(2)
	     if(abs(sinp2).ge.1.0)then
	      ierr = 5
	      return
	     endif
	     phi2     = asin(sinp2)
             y2(i)    = y0(2) + rho(2)*tanl(2)*(phi2-phi0(2))
	     dely(i)  = abs(y2(i)-y1(i))
	   enddo

	   if(dely(1) .lt. dely(2))then
             jint = 1
	   else
	     jint = 2
	   endif
          
	   if(dely(jint) .gt. v0_sw.dely)then
	     ierr = 5
	     return
	   endif

	else      ! circles don't quite touch

           delta_r  = 0.5*(dist - (rho(1) + rho(2)))
	   xint(1)  = xc(1) + h(1)*(rho(1)+delta_r)*abs(cos(phi_c))
	   zint(1)  = zc(1) + (rho(1)+delta_r)*sin(phi_c)
           y1(1)    = y0(1) + (rho(1)+delta_r)*tanl(1)*(phi_c-phi0(1))
           sinp2    = (zint(1) - zc(2))/(rho(2) + delta_r)
	   if(abs(sinp2).ge.1.0)then
	      ierr = 5
	      return
	   endif
	   phi2     = asin(sinp2)
           y2(1)    = y0(2) + (rho(2)+delta_r)*tanl(2)*(phi2-phi0(2))
           dely(1)  = abs(y2(1) - y1(1))

	   if(dely(1) .gt. v0_sw.dely)then
	     ierr = 5
	     return
	   endif

           jint = 1

	endif

	xdk(1) = xint(jint)
	xdk(2) = 0.5*(y1(jint) + y2(jint))
	xdk(3) = zint(jint)
	dy     = dely(jint)           

	return
	end




        options/extend_source
	SUBROUTINE V0_MASS(PV,M1,M2,MASS,PV0)

***************************************************************************
***************************************************************************
**                                                                       **
**  Input arguments:                                                     **
**  ----------------                                                     **
**    PV     = momenta of 2 tracks at decay point                        **
**    M1,M2  = masses of particles 1 and 2                               **
**                                                                       **
**  Output arguments:                                                    **
**  -----------------                                                    **
**    MASS = invariant mass                                              **
**    PV0  = 3-momentum of V0 in lab frame                               **
**                                                                       **
**  Functional Description:                                              **
**  -----------------------                                              **
**                                                                       **
**                                                                       **
***************************************************************************
***************************************************************************

        IMPLICIT NONE

	REAL m1,m2,e1,e2,pv(3,2)
	REAL mass,pv0(3),ev0

c
	pv0(1) = pv(1,1) + pv(1,2)
	pv0(2) = pv(2,1) + pv(2,2)
	pv0(3) = pv(3,1) + pv(3,2)

	e1     = sqrt(m1**2 + pv(1,1)**2 + pv(2,1)**2 + pv(3,1)**2)
	e2     = sqrt(m2**2 + pv(1,2)**2 + pv(2,2)**2 + pv(3,2)**2)
	ev0    = e1 + e2

	mass   = sqrt(ev0**2 - pv0(1)**2 - pv0(2)**2 - pv0(3)**2)

	return
	end
	


        options/extend_source
	SUBROUTINE  V0_REFIT(HITS,ICH,JCH,ZDK,XV0,PV,CHISQ,IERR)

***************************************************************************
***************************************************************************
**                                                                       **
**  Input arguments:                                                     **
**  ----------------                                                     **
**    HITS    = HITS table                                               **
**    ICH     = pointer to TRK_CHAINS common                             **
**    JCH     = pointer to TRK_CHAINS common                             **
**    ZDK     = initial guess for z coordinate at V0 decay point         **
**                                                                       **
**  Output arguments:                                                    **
**  -----------------                                                    **
**    XV0     = coordinates of fitted decay point                        **
**    PV   = fitted momentum vectors at fitted decay point               **
**    IERR    =   0 if fit successful                                    **  
**           .ne. 0 if fit fails                                         **
**                                                                       **
**  Functional Description:                                              **
**  -----------------------                                              **
**                                                                       **
**                                                                       **
**  Created:    15-Mar-94                                                **
**  Author:     M. Justice                                               **
**                                                                       **
**  Reference:  Numerical Recipes (FORTRAN version)                      **
**              W.H. Press et al.                                        **
**              Cambridge University Press (1989)                        **
**              pp. 523-528                                              **
**                                                                       **
***************************************************************************
***************************************************************************


        IMPLICIT NONE
 
        INCLUDE 'tas_incl_path:tas_structures.inc'
        INCLUDE 'tas_incl_path:tas_table_pars.inc'
        INCLUDE 'tas_incl_path:tas_table_st.inc'
 
        INCLUDE 'tas_incl_path:trk_hit_ptrs.inc'
	INCLUDE 'tas_incl_path:trk_chains.inc'
 
        RECORD /hits_row_st/ hits(hits_len)
 
	INTEGER npar
        PARAMETER (npar = 9)

        REAL chi_min
        PARAMETER (chi_min = 0.5)

        REAL chi_max
        COMMON /chi_cut/ chi_max

	REAL chk_min
	PARAMETER (chk_min = 0.01)

	REAL alamda_max
	PARAMETER (alamda_max = 1.0e+5)

	REAL cnst 
	PARAMETER (cnst =  0.2998e-3)

        REAL a(npar),covar(npar,npar),alamda
	REAL x(mxrow,2),y(mxrow,2),z(mxrow,2),dx(mxrow,2),dy(mxrow,2)
	REAL par_b(5,2),par_0(5,2)
	REAL zdk,xv0(3),pv(3,2)
	REAL chisq,chisq_ndf,chisq_old,chk
	REAL bfld(3),sinp,rho,tanl,h,pxz

	INTEGER npads(mxrow,2)
	INTEGER ipts,jpts,npts(2),niter
	INTEGER ich,jch
	INTEGER i,j,ih,jh
	INTEGER ierr


c----------------------- Begin Execution -----------------------
        
	ierr   = 0

	ipts  = 0
	do i = nhch(ich),1,-1
	  ipts          = ipts + 1
	  ih            = ihch(i,ich)
	  x(ipts,1)     = hits(ih).x
	  y(ipts,1)     = hits(ih).y
	  z(ipts,1)     = hits(ih).z
	  dx(ipts,1)    = hits(ih).dx
	  dy(ipts,1)    = hits(ih).dy
          npads(ipts,1) = hits(ih).npads
 	enddo

	jpts  = 0
	do j = nhch(jch),1,-1
	  jpts          = jpts + 1
	  jh            = ihch(j,jch)
	  x(jpts,2)     = hits(jh).x
	  y(jpts,2)     = hits(jh).y
	  z(jpts,2)     = hits(jh).z
	  dx(jpts,2)    = hits(jh).dx
	  dy(jpts,2)    = hits(jh).dy
          npads(jpts,2) = hits(jh).npads
 	enddo

	npts(1) = ipts 
	npts(2) = jpts 

* Parameters at 1st hit on track

	do i = 1,5
          par_b(i,1) = pchain(i,ich)	
          par_b(i,2) = pchain(i,jch)	
	enddo

* Propagate parameters to Z = ZDK for initial guess

	do i = 1,2
	  call v0_param_prop(z(1,i),zdk,par_b(1,i),par_0(1,i),ierr)
          if(ierr .ne. 0)return
	enddo

* Initial parameters

	a(1) = 0.5*(par_0(1,1) + par_0(1,2))    ! x
	a(2) = 0.5*(par_0(2,1) + par_0(2,2))    ! y
	a(3) = zdk
	a(4) = par_0(3,1)
	a(5) = par_0(4,1)
	a(6) = par_0(5,1)
	a(7) = par_0(3,2)
	a(8) = par_0(4,2)
	a(9) = par_0(5,2)

        alamda = -1.
        call v0qmin(npts,x,y,z,dx,dy,npads,a,covar,alamda,chisq,ierr)
	if(ierr.ne.0)return

        chisq_old = chisq

	niter = 0

        do j = 1,30

          niter = niter+1
          call v0qmin(npts,x,y,z,dx,dy,npads,a,covar,alamda,chisq,ierr)

          if(ierr.ne.0) return

          chisq_ndf = chisq/(2*(npts(1)+npts(2)) - npar)


*** The convergence criteria here could use a lot more thought  -MJ 3-Mar-94

	  if(chisq_ndf.le.chi_min)goto 150

          chk = (chisq_old-chisq)/chisq_old
	  if(chk.eq.0.0)then
	    if(alamda .gt. alamda_max)goto 150
	  elseif(chisq_ndf .le. chi_max)then
            if(chk .le. chk_min)goto 150
	  endif  

          chisq_old = chisq

        enddo


 150    continue

	alamda = 0.0
        call v0qmin(npts,x,y,z,dx,dy,npads,a,covar,alamda,chisq,ierr)
	
	if(ierr.ne.0)return

	call mfld(a(1),a(2),a(3),bfld)
	if(bfld(2) .eq. 0) then
	  ierr = -10
	  return
	endif

	xv0(1) = a(1)
	xv0(2) = a(2)
	xv0(3) = a(3)

	do i = 1,2
	  j    = 3*i + 1
          sinp = a(j)
	  rho  = abs(1./a(j+1))
	  tanl    = a(j+2)
	  h       = sign(1.,a(j+1))
	  pxz     = cnst*bfld(2)*rho
	  pv(1,i) = -h*pxz*sinp
          pv(2,i) = pxz*tanl
	  pv(3,i) = pxz*sqrt(1.-sinp**2)
	enddo

        chisq     = chisq/(2*(npts(1)+npts(2)) - npar)

	return
	end




	options/extend_source
      	SUBROUTINE V0QMIN(NPTS,X,Y,Z,DX,DY,NPADS,A,COV,ALAM,CHISQ,IERR)

*******************************************************************************
*******************************************************************************
**                                                                           **
**  Input arguments:                                                         **
**  ----------------                                                         **
**    IPTS    = number of hits on 1st track                                  **
**    JPTS    = number of hits on 2nd track                                  **
**    X,Y,Z   = vectors containing space points from HITS table              **
**    DX,DY   = errors from HITS table                                       **
**    NPADS   = vector of number of pads from HITD table                     **
**    A       = vector of 9 helix parameters                                 **
**    COV     = 9x9 covariance matrix associated with A                      **
**    ALAM    = parameter used by Levenberg-Marquardt algorithm              **
**                                                                           **
**  Output arguments:                                                        **
**  -----------------                                                        **
**    CHISQ   =   chi-square                                                 **
**    IERR    =   0 on successful return                                     **
**           .ne. 0 otherwise                                                **
**                                                                           **
**  Functional Description:                                                  **
**  -----------------------                                                  **
**    Called by V0_REFIT. Part of Levenbegr-Marquardt algorithm.             **
**    See reference cited above.                                             **
**                                                                           **
**  Created:    12-Mar-94                                                    **
**  Author:     M. Justice                                                   **
**                                                                           **
*******************************************************************************
*******************************************************************************

	IMPLICIT NONE

	INTEGER npar
      	PARAMETER (npar = 9)

	INTEGER mxrow
	PARAMETER (mxrow = 128)

        INTEGER npts(2),npads(mxrow,2)

	REAL alam,chisq,ochisq
        REAL x(mxrow,2),y(mxrow,2),z(mxrow,2),dx(mxrow,2),dy(mxrow,2)
        REAL a(npar),cov(npar,npar),alpha(npar,npar),atry(npar)
        REAL beta(npar),da(npar)

	INTEGER j,k,ierr

C***
        if(alam.lt.0.) then
          alam = .001
          call v0qcof(npts,x,y,z,dx,dy,npads,a,alpha,beta,chisq,ierr)
          if(ierr.ne.0)return
          ochisq = chisq
        endif

        do j = 1,npar
          do k =1,j-1
           cov(j,k) = alpha(j,k)
	   cov(k,j) = cov(j,k)
          end do
          cov(j,j) = alpha(j,j)*(1.0 + alam)
          da(j)    = beta(j)
        end do

        if(alam.eq.0.)then
   	  call rsinv_mj(npar,cov,npar,ierr)    ! modified CERNLIB routine
	  return                               ! inverts COV
        endif

        call rseqn_mj(npar,cov,npar,ierr,1,da) ! modified CERNLIB routine
                                             ! for solving systems of
                                             ! linear equations
        if(ierr.ne.0)return                  

        do j = 1,npar
         atry(j) = a(j) + da(j)
        end do

        call v0qcof(npts,x,y,z,dx,dy,npads,atry,cov,da,chisq,ierr)
        if(ierr.ne.0)return

        if(chisq.lt.ochisq)then

          if(alam.gt.1.e-10)alam = 0.1*alam
          ochisq = chisq

          do j = 1,npar
            do k = 1,j
              alpha(j,k) = cov(j,k)
            end do
            beta(j) = da(j)
            a(j) = atry(j)
          end do

        else

          alam   = 10.*alam
          chisq  = ochisq

        end if

       	return
        end




	options/extend_source
	SUBROUTINE V0QCOF(NPTS,X,Y,Z,DX,DY,NPADS,A,ALPHA,BETA,CHISQ,IERR)

*******************************************************************************
*******************************************************************************
**                                                                           **
**  Input arguments:                                                         **
**  ----------------                                                         **
**    IPTS    = number of hits on 1st track                                  **
**    JPTS    = number of hits on 2nd track                                  **
**    X,Y,Z   = vectors containing space points from HITS table              **
**    DX,DY   = errors from HITS table                                       **
**    NPADS   = vector of number of pads from HITD table                     **
**    A       = vector of 9 helix parameters                                 **
**                                                                           **
**  Output arguments:                                                        **
**  -----------------                                                        **
**    ALPHA   =   inverse of covariance matrix                               **
**    BETA    =   vector of derivatives of chisq w/respect to 5 parameters   **
**    CHISQ   =   chi-square                                                 **
**    IERR    =   0 on successful return                                     **
**           .ne. 0 otherwise                                                **
**                                                                           **
**  Functional Description:                                                  **
**  -----------------------                                                  **
**    Called by V0QMIN. Part of Levenberg-Marquardt algorithm. Steps two     **
**  trajectories through B field for simultaneous V0 fit. Calculates inverse **
**  of covariance matrix and derivatives of the chi-squared with respect to  **
**  the 9 helix parameters.                                                  **
**                                                                           **
**  Created:    12-Mar-94                                                    **
**  Author:     M. Justice                                                   **
**                                                                           **
**    A  = 9 helix parameters                                                **
**            1. x_0                                                         **
**            2. y_0                                                         **
**            3. z_0                                                         **
**            4. sinp(1)                                                     **
**            5. h(1)/rho(1)                                                 **
**            6. tanl(1)                                                     **
**            7. sinp(2)                                                     **
**            8. h(2)/rho(2)                                                 **
**            9. tanl(2)                                                     **
**                                                                           **
*******************************************************************************
*******************************************************************************

	IMPLICIT NONE

	INTEGER npar
      	PARAMETER (npar = 9)

	INTEGER mxrow
	PARAMETER (mxrow = 128)

	INTEGER mxch
	PARAMETER (mxch = 500)

        INTEGER npts(2),npads(mxrow,2)

        REAL x(mxrow,2),y(mxrow,2),z(mxrow,2)
	REAL dx(mxrow,2),dy(mxrow,2)
	REAL alpha(npar,npar),beta(npar)
	REAL a(npar),dxda(npar),dyda(npar)
	REAL b1(3),b2(3)

	REAL curvmin
	PARAMETER (curvmin = 1.e-10)

	REAL eps
	PARAMETER (eps = 1.e-3)

	INTEGER i,j,k,l,m,n,itrk,ihit,ierr,istp,nstps

	REAL wx,wy,wt_x,wt_y,chisq,dz
	REAL sinp,cosp,tanp,phi,r
	REAL x_old,y_old,z_old,sinp_old,cosp_old,tanp_old,phi_old,r_old
	REAL xp,yp,zp,del_x,del_y
	REAL x_0,y_0,z_0,cosp_0,tanp_0,phi_0
	REAL h(2),r_0(2),sinp_0(2),tanl(2)


c------------------------ Begin Execution ----------------------------
 
	ierr  = 0
	chisq = 0.0

	x_0  = a(1)
	y_0  = a(2)
	z_0  = a(3)

	sinp_0(1) = a(4)
	if(abs(sinp_0(1)).ge.1.)then
	 ierr = -1
	 return
	endif

	sinp_0(2) = a(7)
	if(abs(sinp_0(2)).ge.1.)then
	 ierr = -1
	 return
	endif

	if(abs(a(5)).lt.curvmin)a(5) =  sign(curvmin,a(4))
	r_0(1)    = 1/abs(a(5))

	if(abs(a(8)).lt.curvmin)a(8) =  sign(curvmin,a(8))
	r_0(2)    = 1/abs(a(8))

	h(1)      = sign(1.,a(5))
	h(2)      = sign(1.,a(8))
	tanl(1)   = a(6)
	tanl(2)   = a(9)	

	do i = 1,npar
	  beta(i) = 0.
	  dxda(i) = 0.
	  dyda(i) = 0.
	  do j = 1,i
	    alpha(i,j) = 0.
	  enddo
	enddo

	do itrk = 1,2
	  m        = 3*itrk + 1
	  n        = 7 - 3*(itrk-1)

	  do k = n,n+2            ! this loop really only necessary for
	    dxda(k) = 0.0         ! itrk=2
	    dyda(k) = 0.0
	  enddo

	  cosp_0   = sqrt(1. - sinp_0(itrk)**2)
	  tanp_0   = sinp_0(itrk)/cosp_0
	  phi_0    = asin(sinp_0(itrk))

	  x_old    = x_0
	  y_old    = y_0
	  z_old    = z_0
	  r_old    = r_0(itrk)
	  sinp_old = sinp_0(itrk)
	  cosp_old = cosp_0
	  tanp_old = tanp_0
	  phi_old  = phi_0

	  do ihit = 1,npts(itrk)
	    wx = 1./dx(ihit,itrk)**2
	    wy = 1./dy(ihit,itrk)**2
            dz = z(ihit,itrk) - z_old
	    if(dz.gt.1.6) then
             
	      nstps = int(dz/1.2)       
	      dz    = dz/float(nstps)

	      do istp = 1,nstps
                sinp = sinp_old + dz/r_old
                if(abs(sinp).ge.1.)then
                  ierr = -3
                  return
                endif

                cosp = sqrt(1. - sinp**2.)
	        phi  = asin(sinp)
	        tanp = sinp/cosp
        
                xp   = x_old + h(itrk)*r_old*(cosp - cosp_old)
                yp   = y_old + r_old*tanl(itrk)*(phi - phi_old)
                zp   = z_old + dz

          	call mfld(x_old,y_old,z_old,b1)
                call mfld(xp,yp,zp,b2)
	        if(b1(2).le.eps .or. b2(2).le.eps)then
                  r = r_old
                else
                  r = r_old*sqrt(b1(1)**2 + b1(2)**2 + b1(3)**2)/
     +                      sqrt(b2(1)**2 + b2(2)**2 + b2(3)**2)
	        endif

* Now get derivatives
 
	       dxda(3)   = dxda(3) + h(itrk)*(tanp-tanp_old)*
     +                               r_old/r_0(itrk)
	       dyda(3)   = dyda(3) - (1./cosp - 1./cosp_old)*
     +                               tanl(itrk)*r_old/r_0(itrk)

 	       dxda(m)   = dxda(m) + h(itrk)*r_old*(tanp_old - tanp)
	       dyda(m)   = dyda(m) - tanl(itrk)*r_old*(1./cosp_old - 1./cosp)

	       dxda(m+1) = dxda(m+1) - r_0(itrk)*r_old*(cosp - cosp_old 
     +                               - tanp_old*(sinp_old - sinp_0(itrk))
     +                               + tanp*(sinp -sinp_0(itrk)))
	       dyda(m+1) = dyda(m+1) - h(itrk)*tanl(itrk)*r_0(itrk)*r_old*
     +                                 (phi - phi_old 
     +                               + (sinp_0(itrk) - sinp)/cosp 
     +                               - (sinp_0(itrk) - sinp_old)/cosp_old)

	       x_old    = xp
	       y_old    = yp
	       z_old    = zp
               r_old    = r
	       sinp_old = sinp
	       cosp_old = cosp
	       phi_old  = phi
               tanp_old = tanp
	      enddo                     ! close loop over istp
	      dz = z(ihit,itrk) - zp

	    endif                       ! if dz.gt.1.6

            sinp = sinp_old + dz/r_old
            if(abs(sinp).ge.1.)then
              ierr = -4
              return
            endif
            cosp = sqrt(1. - sinp**2.)
	    phi  = asin(sinp)
	    tanp = sinp/cosp

            xp   = x_old + h(itrk)*r_old*(cosp - cosp_old)
            yp   = y_old + r_old*tanl(itrk)*(phi - phi_old)
            zp   = z_old + dz

            call mfld(x_old,y_old,z_old,b1)
            call mfld(xp,yp,zp,b2)
	    if(b1(2).le.eps .or. b2(2).le.eps)then
               r = r_old
            else
               r = r_old*sqrt(b1(1)**2 + b1(2)**2 + b1(3)**2)/
     +                   sqrt(b2(1)**2 + b2(2)**2 + b2(3)**2)
	    endif

* Now get derivatives
	
	    dxda(1)   =  1.
	    dyda(2)   =  1.

	    dxda(3)   = dxda(3) + h(itrk)*(tanp-tanp_old)*
     +                            r_old/r_0(itrk)
	    dyda(3)   = dyda(3) - (1./cosp - 1./cosp_old)*
     +                            tanl(itrk)*r_old/r_0(itrk)

	    dxda(m)   = dxda(m) + h(itrk)*r_old*(tanp_old - tanp)
	    dyda(m)   = dyda(m) - tanl(itrk)*r_old*(1./cosp_old - 1./cosp)

	    dxda(m+1) = dxda(m+1) - r_0(itrk)*r_old*(cosp - cosp_old 
     +                            - tanp_old*(sinp_old - sinp_0(itrk))
     +                            + tanp*(sinp -sinp_0(itrk)))
	    dyda(m+1) = dyda(m+1) - h(itrk)*tanl(itrk)*r_0(itrk)*r_old*
     +                              (phi - phi_old
     +                            + (sinp_0(itrk) - sinp)/cosp 
     +                            - (sinp_0(itrk) - sinp_old)/cosp_old)

	    dyda(m+2) = (yp - y_0)/tanl(itrk)

	    del_x     = x(ihit,itrk) - xp
	    del_y     = y(ihit,itrk) - yp
	    chisq     = chisq + wx*del_x**2 + wy*del_y**2

            do k = 1,9
	      wt_x    = wx*dxda(k)
	      wt_y    = wy*dyda(k)
	      beta(k) = beta(k) + wt_x*del_x + wt_y*del_y
	      do l = 1,k
	       alpha(k,l) = alpha(k,l) + wt_x*dxda(l) + wt_y*dyda(l)
	      enddo
	    enddo

	    x_old    = xp
	    y_old    = yp
	    z_old    = zp
	    r_old    = r
	    sinp_old = sinp
	    cosp_old = cosp
	    phi_old  = phi
	    tanp_old = tanp

	  enddo                           ! close loop over ihit

	enddo                             ! close loop over itrk

	return
	end


        options/extend_source
        SUBROUTINE TRK_DUMMY_VTX(TRK_SW,TPC_SUM,NOK_TPC_SUM)

***************************************************************************
***************************************************************************
**                                                                       **
**  Input arguments:                                                     **
**  ----------------                                                     **
**    TRK_SW      =   TRK_SW table                                       **
**                                                                       **
**  Output arguments:                                                    **
**  -----------------                                                    **
**    TPC_SUM     =   TPC_SUM table                                      **
**    NOK_TPC_SUM =   event.tpc_sum.h.nok                                **
**                                                                       **
**  Functional Description:                                              **
**  -----------------------                                              **
**    Called by TRK_MAIN. Puts TRK)SW.XTGT,YTGT,ZTGT into TPC_SUM        **
**  table then calculates distances of closest approach. Useful for      **
**  reconstruction of V0 only simulated events.                          **
**                                                                       **
**  Created:    15-Mar-94                                                **
**  Author:     M. Justice                                               **
**                                                                       **
***************************************************************************
***************************************************************************
 
        IMPLICIT NONE
 
        INCLUDE 'tas_incl_path:tas_structures.inc'
        INCLUDE 'tas_incl_path:tas_table_pars.inc'
        INCLUDE 'tas_incl_path:tas_table_st.inc'

        INCLUDE 'tas_incl_path:trk_tracks.inc'
 
        RECORD /trk_sw_row_st  / trk_sw
        RECORD /tpc_sum_row_st / tpc_sum
 
        REAL dca
        COMMON /trk_dist/ dca(mxtrk)

	INTEGER nok_tpc_sum,j
 
	REAL xv,yv,zv,x0,y0,z0
	REAL px,py,pz,pxz
	REAL psi,sinv,cosv,sinl,cosl,tanl
	REAL h(3)


c----------------------- Begin Execution -----------------------
 
	xv = trk_sw.xtgt
	yv = trk_sw.ytgt
	zv = trk_sw.ztgt

        do j = 1,ngtrk
          dca(j) = 100.
          x0     = rpv(1,j)   
          y0     = rpv(2,j)
          z0     = rpv(3,j)
	  px     = rpv(4,j)
	  py     = rpv(5,j)
	  pz     = rpv(6,j)
          pxz    = sqrt(px*px + pz*pz)
          psi    = atan2(px,pz)
	  tanl   = py/pxz
          sinv   = sin(psi)
          cosv   = cos(psi)
          cosl   = 1.0/sqrt(1.0+tanl**2)
          sinl   = tanl*cosl
          H(1)   = (yv-y0)*cosl*cosv - (zv-z0)*sinl       
          H(2)   = (zv-z0)*cosl*sinv - (xv-x0)*cosl*cosv  
          H(3)   = (xv-x0)*sinl - (yv-y0)*cosl*sinv
          dca(j) = sqrt(H(1)*H(1)+H(2)*H(2)+H(3)*H(3))
        enddo
 
	tpc_sum.xv  = xv
	tpc_sum.yv  = yv
	tpc_sum.zv  = zv
	nok_tpc_sum = 1

	return
	end




	options/extend_source
	SUBROUTINE V0_PARAM_PROP(ZBEG,ZEND,PAR_B,PAR_E,IERR)

*******************************************************************************
*******************************************************************************
**                                                                           **
**  Input arguments:                                                         **
**  ----------------                                                         **
**    ZBEG     = starting Z                                                  **
**    ZEND     = Z to project to                                             **
**    PAR_B    = 5 helix parameters at Z = ZBEG                              **
**                 1. x                                                      **
**                 2. y                                                      **
**                 3. sin(phi)                                               **
**                 4. h/r                                                    **
**	           5. tanl                                                   **
**                                                                           **
**  Output arguments:                                                        **
**  -----------------                                                        **
**    PAR_E = extrapolated helix parameters at Z = ZEND                      **
**                                                                           **
**  Functional Description:                                                  **
**  -----------------------                                                  **
**    Called by TRK_V0_MAIN.  Propagates helix parameters from ZBEG          **
**  to ZEND.                                                                 **
**                                                                           **
**  Created:    15-Mar-94                                                    **
**  Author:     M. Justice                                                   **
**                                                                           **
*******************************************************************************
*******************************************************************************

	IMPLICIT NONE

	REAL cpi,c2pi,cpi2
      	COMMON /cnsts/ cpi,c2pi,cpi2
	
	REAL par_b(5),par_e(5)

	REAL b1(3),b2(3)

	REAL dz_max
	PARAMETER (dz_max = 1.0)

	REAL dz_tol
	PARAMETER (dz_tol = 2.0)

	REAL eps
	PARAMETER (eps = 1.e-3)

	REAL bmag1
	REAL zbeg,zend
	REAL dz,delta_phi
	REAL xp,yp,zp,sinp,cosp,phi,r,tanl,h
	REAL x_old,y_old,z_old,sinp_old,cosp_old,phi_old,r_old

	INTEGER ierr,nstps,istp


c----------------------- Begin Execution -----------------------

	if((zbeg + dz_tol) .lt. zend)then
	  ierr = -1
	  return
	endif

	x_old    = par_b(1)
	y_old    = par_b(2)
	z_old    = zbeg
        sinp_old = par_b(3)
	cosp_old = sqrt(1.-sinp_old**2)
	phi_old  = asin(sinp_old)
	r_old    = 1./abs(par_b(4))
	h        = sign(1.,par_b(4))
	tanl     = par_b(5)

	dz       = zend - zbeg
	if(abs(dz) .gt. dz_max) then
          nstps = int(abs(dz)/dz_max)
	  dz    = dz/float(nstps)

	   do istp = 1,nstps
             sinp = sinp_old + dz/r_old
	     if(abs(sinp).ge.1.)then
               ierr = -2
	       return
	     endif
	     cosp = sqrt(1. - sinp**2.)
	     phi = asin(sinp)

	     delta_phi = phi - phi_old   
	
	     xp = x_old + h*r_old*(cosp - cosp_old)
	     yp = y_old + r_old*delta_phi*tanl
             zp = z_old + dz

             call mfld(x_old,y_old,z_old,b1)
             bmag1 = sqrt(b1(1)**2+b1(2)**2+b1(3)**2)
             call mfld(xp,yp,zp,b2)
 
             if(b1(2).le.eps .or. b2(2).le.eps) then
               r = r_old
             else
               r = r_old*bmag1/sqrt(b2(1)**2 + b2(2)**2 + b2(3)**2)
             endif

* updated parameters

	     x_old    = xp
	     y_old    = yp
	     z_old    = zp
	     sinp_old = sinp
	     cosp_old = cosp
	     phi_old  = phi
	     r_old    = r
	   enddo

	   dz = zend - zp

	endif

        sinp = sinp_old + dz/r_old
	if(abs(sinp).ge.1.)then
           ierr = -2
	   return
	endif
	cosp = sqrt(1. - sinp**2.)
	phi  = asin(sinp)

	delta_phi = phi - phi_old   
	
	xp = x_old + h*r_old*(cosp - cosp_old)
	yp = y_old + r_old*delta_phi*tanl
        zp = z_old + dz

        call mfld(x_old,y_old,z_old,b1)
        bmag1 = sqrt(b1(1)**2+b1(2)**2+b1(3)**2)
        call mfld(xp,yp,zp,b2)
 
        if(b1(2).le.eps .or. b2(2).le.eps) then
           r = r_old
        else
           r = r_old*bmag1/sqrt(b2(1)**2 + b2(2)**2 + b2(3)**2)
        endif

	par_e(1) = xp
	par_e(2) = yp
	par_e(3) = sinp
	par_e(4) = h/r
	par_e(5) = tanl

	return
	end	
