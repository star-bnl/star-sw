C--------------------------------------------------------------------
C
C ... Common blocks for UrQMD 3.3.1
C
C--------------------------------------------------------------------

      Function address_of_energies()
      Integer address_of_energies, loc
      real*8 Ekinbar, Ekinmes, ESky2, ESky3, EYuk, ECb, EPau
      common /energies/ Ekinbar, Ekinmes, ESky2, ESky3, EYuk, ECb, EPau
      address_of_energies = loc( Ekinbar )
      return
      End

      Function address_of_sys()
      Integer address_of_sys, loc
      INTEGER npart, nbar, nmew, ctag, nsteps, uid_cnt, ranseed,
     & event, Ap, At, Zp, Zt, eos, dectag, NHardRes, NSoftRes,
     & NDecRes, NElColl, NBlColl
       common /sys/ npart, nbar, nmes, ctag,nsteps,uid_cnt,
     &             ranseed,event,Ap,At,Zp,Zt,eos,dectag,
     &             NHardRes,NSoftRes,NDecRes,NElColl,NBlColl
      address_of_sys = loc ( npart )
      return
      End

      Function address_of_rsys()
      Integer address_of_rsys, loc
      real*8  time,  acttime, bdist, ebeam, bimp, bmin, ecm
      common /rsys/ time,acttime,bdist,bimp,bmin,ebeam,ecm
      address_of_ = loc ( time )
      return
      End

      Function address_of_cuts()
      Integer address_of_cuts, loc
      real*8 cutmax, cutPau, cutCb, cutYuk, cutSky, cutdww
      common /cuts/ cutmax, cutPau, cutCb, cutYuk, cutSky, cutdww
      address_of_ = loc ( cutmax )
      return
      End

      Function address_of_spdata()
      Integer address_of_spdata, loc, nspl
C This next was taken from coms.inc, if it changes in the main code 
C it will have to change here too
      parameter (nspl = 500)  ! dimension of spline arrays
      real*8 spx(nspl), spPauy(nspl), outPau(nspl),
     &                spCby(nspl),  outCb(nspl),
     &                spYuky(nspl), outYuk(nspl),
     &                spSkyy(nspl), outSky(nspl),
     &                spdwwy(nspl), outdww(nspl)
      common /spdata/ spx, spPauy, outPau, spCby,  outCb,
     &                     spYuky, outYuk, spSkyy, outSky,
     &                     spdwwy, outdww
      address_of_spdata = loc ( spx(1) )
      return
      End

      Function address_of_isys()
      Integer address_of_isys, loc, nmax
C This next was taken from coms.inc, if it changes in the main code
C it will have to change here too
      parameter (nmax = 40000) ! maximum number of particles
      integer spin(nmax),ncoll(nmax),charge(nmax),strid(nmax),
     &        ityp(nmax),lstcoll(nmax),iso3(nmax),origin(nmax),uid(nmax)
      common/isys/spin,ncoll,charge,ityp,lstcoll,iso3,origin,strid,
     &            uid
      address_of_isys = loc ( spin(1) )
      return
      End

      Function address_of_coor()
      Integer address_of_coor, loc, nmax
C This next was taken from coms.inc, if it changes in the main code
C it will have to change here too
      parameter (nmax = 40000) ! maximum number of particles
      real*8
     &     r0(nmax), rx(nmax), ry(nmax), rz(nmax),
     &     p0(nmax), px(nmax), py(nmax), pz(nmax),
     &     fmass(nmax), rww(nmax), dectime(nmax)
      common /coor/ r0, rx, ry, rz, p0, px, py, pz, fmass, rww, dectime
      address_of_coor = loc ( r0(1) )
      return
      End
      
      Function address_of_frag()
      Integer address_of_frag, loc, nmax
C This next was taken from coms.inc, if it changes in the main code
C it will have to change here too
      parameter (nmax = 40000) ! maximum number of particles
      real*8 tform(nmax), xtotfac(nmax)
      common /frag/ tform, xtotfac 
      address_of_frag = loc ( tform(1) )
      return
      End

      Function address_of_aios()
      Integer address_of_aios, loc, nmax
C This next was taken from coms.inc, if it changes in the main code
C it will have to change here too
      parameter (nmax = 40000) ! maximum number of particles
      real*8 airx(nmax), airy(nmax), airz(nmax),
     &     aipx(nmax), aipy(nmax), aipz(nmax),
     &     aorx(nmax,4), aory(nmax,4), aorz(nmax,4),
     &     aopx(nmax,4), aopy(nmax,4), aopz(nmax,4)
      common /aios/ airx, airy, airz, aipx, aipy, aipz,
     &              aorx, aory, aorz, aopx, aopy, aopz
      address_of_aios = loc ( airx(1) )
      return
      End

      Function address_of_pots()
      Integer address_of_pots, loc
      real*8 
     &     gw, sgw, delr, fdel, dt,da, db, Cb0, Yuk0, Pau0, Sky20,
     &     Sky30, gamSky, gamYuk, drPau, dpPau, dtimestep
      common /pots/ Cb0, Yuk0, Pau0, Sky20, Sky30, gamSky,
     &              gamYuk, drPau, dpPau, gw, sgw, delr, fdel,
     &              dt,da, db,dtimestep
      address_of_pots = loc ( Cb0 )
      return
      End

      Function address_of_scoor()
      Integer address_of_scoor, loc, smax
C This next was taken from coms.inc, if it changes in the main code
C it will have to change here too
      parameter(smax=500)  ! maximum number of spectators
      real*8 r0s(smax), rxs(smax), rys(smax), rzs(smax),
     &         p0s(smax), pxs(smax), pys(smax), pzs(smax),
     &         sfmass(smax)
      common /scoor/ r0s, rxs, rys, rzs, p0s, pxs ,pys, pzs, sfmass
      address_of_scoor = loc ( r0s(1) )
      return
      End

      Function address_of_sisys()
      Integer address_of_sisys, loc, smax
C This next was taken from coms.inc, if it changes in the main code
C it will have to change here too
      parameter(smax=500)  ! maximum number of spectators
      integer sspin(smax), scharge(smax), sityp(smax), siso3(smax),
     &          suid(smax)
      common /sisys/ sspin, scharge, sityp, siso3, suid
      address_of_sisys = loc ( sspin(1) )
      return
      End

      Function address_of_ssys()
      Integer address_of_ssys, loc
      integer nspec
      common /ssys/ nspec
      address_of_ssys = loc ( nspec )
      return
      End

      Function address_of_rtdelay()
      Integer address_of_rtdelay, loc, nmax
C This next was taken from coms.inc, if it changes in the main code
C it will have to change here too
      parameter (nmax = 40000) ! maximum number of particles
      real*8 p0td(2,nmax),pxtd(2,nmax),pytd(2,nmax),pztd(2,nmax),
     &         fmasstd(2,nmax)
      common /rtdelay/p0td,pxtd,pytd,pztd,fmasstd
      address_of_rtdelay = loc ( p0td(1,1) )
      return
      End

      Function address_of_itdelay()
      Integer address_of_itdelay, loc, nmax
C This next was taken from coms.inc, if it changes in the main code
C it will have to change here too
      parameter (nmax = 40000) ! maximum number of particles
      integer ityptd(2,nmax),iso3td(2,nmax)
      common /itdelay/ityptd,iso3td
      address_of_itdelay = loc ( ityptd(1,1) )
      return
      End

      Function address_of_svinfo()
      Integer address_of_svinfo, loc
      integer itypt(2),uidt(2),origint(2),iso3t(2)
      common /svinfo/itypt,uidt,origint,iso3t
      address_of_svinfo = loc ( itypt(1) )
      return
      End

      Function address_of_ffermi()
      Integer address_of_ffermi, loc, nmax
C This next was taken from coms.inc, if it changes in the main code
C it will have to change here too
      parameter (nmax = 40000) ! maximum number of particles
      real*8 ffermpx(nmax), ffermpy(nmax), ffermpz(nmax)
      common /ffermi/ ffermpx, ffermpy, ffermpz
      address_of_ffermi = loc ( ffermpx(1) )
      return
      End

      Function address_of_peq()
      Integer address_of_peq, loc
      real*8 peq1, peq2
      common /peq/ peq1,peq2
      address_of_peq = loc ( peq1 )
      return
      End

C      Function address_of_()
C      Integer address_of_, loc
C
C      address_of_ = loc (  )
C      return
C      End
