C--------------------------------------------------------------------
C
C ... Common blocks for UrQMD 3.3.1
C
C--------------------------------------------------------------------

      Function address_of_energies()
      use iso_c_binding
      Type(C_Ptr) :: address_of_energies
      real*8,target:: Ekinbar, Ekinmes, ESky2, ESky3, EYuk, ECb, EPau
      common /energies/ Ekinbar, Ekinmes, ESky2, ESky3, EYuk, ECb, EPau
      address_of_energies = c_loc( Ekinbar )
      return
      End

      Function address_of_sys()
      use iso_c_binding
      Type(C_Ptr) :: address_of_sys
      INTEGER,target:: npart, nbar, nmew, ctag, nsteps, uid_cnt, ranseed,
     & event, Ap, At, Zp, Zt, eos, dectag, NHardRes, NSoftRes,
     & NDecRes, NElColl, NBlColl
       common /sys/ npart, nbar, nmes, ctag,nsteps,uid_cnt,
     &             ranseed,event,Ap,At,Zp,Zt,eos,dectag,
     &             NHardRes,NSoftRes,NDecRes,NElColl,NBlColl
      address_of_sys = c_loc ( npart )
      return
      End

      Function address_of_rsys()
      use iso_c_binding
      Type(C_Ptr) :: address_of_rsys
      real*8,target::  time,  acttime, bdist, ebeam, bimp, bmin, ecm
      common /rsys/ time,acttime,bdist,bimp,bmin,ebeam,ecm
      address_of_rsys = c_loc ( time )
      return
      End

      Function address_of_cuts()
      use iso_c_binding
      Type(C_Ptr) :: address_of_cuts
      real*8,target:: cutmax, cutPau, cutCb, cutYuk, cutSky, cutdww
      common /cuts/ cutmax, cutPau, cutCb, cutYuk, cutSky, cutdww
      address_of_cuts = c_loc ( cutmax )
      return
      End

      Function address_of_spdata()
      use iso_c_binding
      type(c_ptr):: address_of_spdata
      integer nspl
C This next was taken from coms.inc, if it changes in the main code 
C it will have to change here too
      parameter (nspl = 500)  ! dimension of spline arrays
      real*8,target:: spx(nspl), spPauy(nspl), outPau(nspl),
     &                spCby(nspl),  outCb(nspl),
     &                spYuky(nspl), outYuk(nspl),
     &                spSkyy(nspl), outSky(nspl),
     &                spdwwy(nspl), outdww(nspl)
      common /spdata/ spx, spPauy, outPau, spCby,  outCb,
     &                     spYuky, outYuk, spSkyy, outSky,
     &                     spdwwy, outdww
      address_of_spdata = c_loc ( spx(1) )
      return
      End

      Function address_of_isys()
      use iso_c_binding
      type(c_ptr):: address_of_isys
      integer nmax
C This next was taken from coms.inc, if it changes in the main code
C it will have to change here too
      parameter (nmax = 40000) ! maximum number of particles
      integer,target:: spin(nmax),ncoll(nmax),charge(nmax),strid(nmax),
     &        ityp(nmax),lstcoll(nmax),iso3(nmax),origin(nmax),uid(nmax)
      common/isys/spin,ncoll,charge,ityp,lstcoll,iso3,origin,strid,
     &            uid
      address_of_isys = c_loc ( spin(1) )
      return
      End

      Function address_of_coor()
      use iso_c_binding
      type(c_ptr)::address_of_coor
      integer nmax
C This next was taken from coms.inc, if it changes in the main code
C it will have to change here too
      parameter (nmax = 40000) ! maximum number of particles
      real*8,target::
     &     r0(nmax), rx(nmax), ry(nmax), rz(nmax),
     &     p0(nmax), px(nmax), py(nmax), pz(nmax),
     &     fmass(nmax), rww(nmax), dectime(nmax)
      common /coor/ r0, rx, ry, rz, p0, px, py, pz, fmass, rww, dectime
      address_of_coor = c_loc ( r0(1) )
      return
      End
      
      Function address_of_frag()
      use iso_c_binding
      type(c_ptr)::address_of_frag
      integer nmax
C This next was taken from coms.inc, if it changes in the main code
C it will have to change here too
      parameter (nmax = 40000) ! maximum number of particles
      real*8,target:: tform(nmax), xtotfac(nmax)
      common /frag/ tform, xtotfac 
      address_of_frag = c_loc ( tform(1) )
      return
      End

      Function address_of_aios()
      use iso_c_binding
      type(c_ptr):: address_of_aios
      integer nmax
C This next was taken from coms.inc, if it changes in the main code
C it will have to change here too
      parameter (nmax = 40000) ! maximum number of particles
      real*8 ,target::airx(nmax), airy(nmax), airz(nmax),
     &     aipx(nmax), aipy(nmax), aipz(nmax),
     &     aorx(nmax,4), aory(nmax,4), aorz(nmax,4),
     &     aopx(nmax,4), aopy(nmax,4), aopz(nmax,4)
      common /aios/ airx, airy, airz, aipx, aipy, aipz,
     &              aorx, aory, aorz, aopx, aopy, aopz
      address_of_aios = c_loc ( airx(1) )
      return
      End

      Function address_of_pots()
      use iso_c_binding
      type(c_ptr):: address_of_pots
      real*8 ,target::
     &     gw, sgw, delr, fdel, dt,da, db, Cb0, Yuk0, Pau0, Sky20,
     &     Sky30, gamSky, gamYuk, drPau, dpPau, dtimestep
      common /pots/ Cb0, Yuk0, Pau0, Sky20, Sky30, gamSky,
     &              gamYuk, drPau, dpPau, gw, sgw, delr, fdel,
     &              dt,da, db,dtimestep
      address_of_pots = c_loc ( Cb0 )
      return
      End

      Function address_of_scoor()
      use iso_c_binding
      type(c_ptr):: address_of_scoor
      integer smax
C This next was taken from coms.inc, if it changes in the main code
C it will have to change here too
      parameter(smax=500)  ! maximum number of spectators
      real*8,target:: r0s(smax), rxs(smax), rys(smax), rzs(smax),
     &         p0s(smax), pxs(smax), pys(smax), pzs(smax),
     &         sfmass(smax)
      common /scoor/ r0s, rxs, rys, rzs, p0s, pxs ,pys, pzs, sfmass
      address_of_scoor = c_loc ( r0s(1) )
      return
      End

      Function address_of_sisys()
      use iso_c_binding
      type(c_ptr)::address_of_sisys
      integer smax
C This next was taken from coms.inc, if it changes in the main code
C it will have to change here too
      parameter(smax=500)  ! maximum number of spectators
      integer,target:: sspin(smax), scharge(smax), sityp(smax), siso3(smax),
     &          suid(smax)
      common /sisys/ sspin, scharge, sityp, siso3, suid
      address_of_sisys = c_loc ( sspin(1) )
      return
      End

      Function address_of_ssys()
      use iso_c_binding
      type(c_ptr):: address_of_ssys
      integer,target:: nspec
      common /ssys/ nspec
      address_of_ssys = c_loc ( nspec )
      return
      End

      Function address_of_rtdelay()
      use iso_c_binding
      type(c_ptr):: address_of_rtdelay
      integer ::nmax
C This next was taken from coms.inc, if it changes in the main code
C it will have to change here too
      parameter (nmax = 40000) ! maximum number of particles
      real*8,target:: p0td(2,nmax),pxtd(2,nmax),pytd(2,nmax),pztd(2,nmax),
     &         fmasstd(2,nmax)
      common /rtdelay/p0td,pxtd,pytd,pztd,fmasstd
      address_of_rtdelay = c_loc ( p0td(1,1) )
      return
      End

      Function address_of_itdelay()
      use iso_c_binding
      type(c_ptr):: address_of_itdelay
      integer :: nmax
C This next was taken from coms.inc, if it changes in the main code
C it will have to change here too
      parameter (nmax = 40000) ! maximum number of particles
      integer,target:: ityptd(2,nmax),iso3td(2,nmax)
      common /itdelay/ityptd,iso3td
      address_of_itdelay = c_loc ( ityptd(1,1) )
      return
      End

      Function address_of_svinfo()
      use iso_c_binding
      type(c_ptr):: address_of_svinfo
      integer,target:: itypt(2),uidt(2),origint(2),iso3t(2)
      common /svinfo/itypt,uidt,origint,iso3t
      address_of_svinfo = c_loc ( itypt(1) )
      return
      End

      Function address_of_ffermi()
      use iso_c_binding
      type(c_ptr):: address_of_ffermi
      integer nmax
C This next was taken from coms.inc, if it changes in the main code
C it will have to change here too
      parameter (nmax = 40000) ! maximum number of particles
      real*8,target:: ffermpx(nmax), ffermpy(nmax), ffermpz(nmax)
      common /ffermi/ ffermpx, ffermpy, ffermpz
      address_of_ffermi = c_loc ( ffermpx(1) )
      return
      End

      Function address_of_peq()
      use iso_c_binding
      type(c_ptr):: address_of_peq
      real*8,target:: peq1, peq2
      common /peq/ peq1,peq2
      address_of_peq = c_loc ( peq1 )
      return
      End

C      Function address_of_()
C      Integer address_of_
C
C      address_of_ = c_loc (  )
C      return
C      End
