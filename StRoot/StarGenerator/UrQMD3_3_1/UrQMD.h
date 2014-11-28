#ifndef __UrQMD3_3_1__
#define __UrQMD3_3_1__

#include "StarCallf77.h"
#include <string>
using namespace std;

#include "TObject.h"

//
// Interface to common Blocks
//

#define energies F77_NAME(energies,ENERGIES)
struct ENERGIES_t {
      /*real*8 Ekinbar, Ekinmes, ESky2, ESky3, EYuk, ECb, EPau
      common /energies/ Ekinbar, Ekinmes, ESky2, ESky3, EYuk, ECb, EPau*/
  Double_t  Ekinbar;
  Double_t  Ekinmes;
  Double_t  ESky2;
  Double_t  ESky3;
  Double_t  EYuk;
  Double_t  ECb;
  Double_t  EPau;
};
extern "C" ENERGIES_t *address_of_energies();

#define sys F77_NAME(sys,SYS)
struct SYS_t {
    /*INTEGER npart, nbar, nmew, ctag, nsteps, uid_cnt, ranseed,
     & event, Ap, At, Zp, Zt, eos, dectag, NHardRes, NSoftRes,
     & NDecRes, NElColl, NBlColl
       common /sys/ npart, nbar, nmes, ctag,nsteps,uid_cnt,
     &             ranseed,event,Ap,At,Zp,Zt,eos,dectag,
     &             NHardRes,NSoftRes,NDecRes,NElColl,NBlColl*/
  Int_t  npart;
  Int_t  nbar;
  Int_t  nmew;
  Int_t  ctag;
  Int_t  nspteps;
  Int_t  uid_cnt;
  Int_t  ranseed;
  Int_t  event;
  Int_t  Ap;
  Int_t  At;
  Int_t  Zp;
  Int_t  Zt;
  Int_t  eos;
  Int_t  dectag;
  Int_t  NHardRes;
  Int_t  NSoftRes;
  Int_t  NDecRes;
  Int_t  NElColl;
  Int_t  NBlColl; 
};
extern "C" SYS_t *address_of_sys();

#define rsys F77_NAME(rsys,RSYS)
struct RSYS_t {
    /*real*8  time,  acttime, bdist, ebeam, bimp, bmin, ecm
      common /rsys/ time,acttime,bdist,bimp,bmin,ebeam,ecm*/
  Double_t  time;
  Double_t  acttime;
  Double_t  bdist;
  Double_t  ebeam;
  Double_t  bimp;
  Double_t  bmin;
  Double_t  ecm;
};
extern "C" RSYS_t *address_of_rsys();

#define cuts F77_NAME(cuts,CUTS)
struct CUTS_t {
    /*real*8 cutmax, cutPau, cutCb, cutYuk, cutSky, cutdww
      common /cuts/ cutmax, cutPau, cutCb, cutYuk, cutSky, cutdww*/
  Double_t  cutmax;
  Double_t  cutPau;
  Double_t  cutCb;
  Double_t  cutYuk;
  Double_t  cutSky;
  Double_t  cutdww;
};
extern "C" CUTS_t *address_of_cuts();

#define spdata F77_NAME(spdata,SPDATA)
struct SPDATA_t {
   /* parameter (nspl = 500)  ! dimension of spline arrays
      real*8 spx(nspl), spPauy(nspl), outPau(nspl),
     &                spCby(nspl),  outCb(nspl),
     &                spYuky(nspl), outYuk(nspl),
     &                spSkyy(nspl), outSky(nspl),
     &                spdwwy(nspl), outdww(nspl)
      common /spdata/ spx, spPauy, outPau, spCby,  outCb,
     &                     spYuky, outYuk, spSkyy, outSky,
     &                     spdwwy, outdww*/
  Double_t  _spx[500];
  Double_t  _spPauy[500];
  Double_t  _outPau[500];
  Double_t  _spCby[500];
  Double_t  _outCby[500];
  Double_t  _spYuky[500];
  Double_t  _outYuk[500];
  Double_t  _spSkyy[500];
  Double_t  _outSky[500];
  Double_t  _spdwwy[500];
  Double_t  _outdww[500];
  Double_t  &spx( Int_t i ){return _spx[i-1]; }
  Double_t  &spPauy( Int_t i ){return _spPauy[i-1]; }
  Double_t  &outPau( Int_t i ){return _outPau[i-1]; }
  Double_t  &spCby( Int_t i ){return _spCby[i-1]; }
  Double_t  &outCby( Int_t i ){return _outCby[i-1]; }
  Double_t  &spYuky( Int_t i ){return _spYuky[i-1]; }
  Double_t  &outYuk( Int_t i ){return _outYuk[i-1]; }
  Double_t  &spSkyy( Int_t i ){return _spSkyy[i-1]; }
  Double_t  &outSky( Int_t i ){return _outSky[i-1]; }
  Double_t  &spdwwy( Int_t i ){return _spdwwy[i-1]; }
  Double_t  &outdww( Int_t i ){return _outdww[i-1]; }
};
extern "C" SPDATA_t *address_of_spdata();

#define isys F77_NAME(isys,ISYS)
struct ISYS_t {
   /* parameter (nmax = 40000) ! maximum number of particles
      integer spin(nmax),ncoll(nmax),charge(nmax),strid(nmax),
     &        ityp(nmax),lstcoll(nmax),iso3(nmax),origin(nmax),uid(nmax)
      common/isys/spin,ncoll,charge,ityp,lstcoll,iso3,origin,strid,
     &            uid*/
  Int_t  _spin[40000];
  Int_t  _ncoll[40000];
  Int_t  _charge[40000];
  Int_t  _ityp[40000];
  Int_t  _lstcoll[40000];
  Int_t  _iso3[40000];
  Int_t  _origin[40000];
  Int_t  _strid[40000];
  Int_t  _uid[40000];
  Int_t  &spin( Int_t i ){return _spin[i-1]; }
  Int_t  &ncoll( Int_t i ){return _ncoll[i-1]; }
  Int_t  &ityp( Int_t i ){return _ityp[i-1]; }
  Int_t  &lstcoll( Int_t i ){return _lstcoll[i-1]; }
  Int_t  &iso3( Int_t i ){return _iso3[i-1]; }
  Int_t  &origin( Int_t i ){return _origin[i-1]; }
  Int_t  &strid( Int_t i ){return _strid[i-1]; }
  Int_t  &uid( Int_t i ){return _uid[i-1]; }
};
extern "C" ISYS_t *address_of_isys();

#define coor F77_NAME(coor,COOR)
struct COOR_t {
   /* parameter (nmax = 40000) ! maximum number of particles
      real*8
     &     r0(nmax), rx(nmax), ry(nmax), rz(nmax),
     &     p0(nmax), px(nmax), py(nmax), pz(nmax),
     &     fmass(nmax), rww(nmax), dectime(nmax)
      common /coor/ r0, rx, ry, rz, p0, px, py, pz, fmass, rww, dectime*/
  Double_t  _r0[40000];
  Double_t  _rx[40000];
  Double_t  _ry[40000];
  Double_t  _rz[40000];
  Double_t  _p0[40000];
  Double_t  _px[40000];
  Double_t  _py[40000];
  Double_t  _pz[40000];
  Double_t  _fmass[40000];
  Double_t  _dectime[40000];
  Double_t  &r0( Int_t i ){return _r0[i-1]; }
  Double_t  &rx( Int_t i ){return _rx[i-1]; }
  Double_t  &ry( Int_t i ){return _ry[i-1]; }
  Double_t  &rz( Int_t i ){return _rz[i-1]; }
  Double_t  &p0( Int_t i ){return _p0[i-1]; }
  Double_t  &px( Int_t i ){return _px[i-1]; }
  Double_t  &py( Int_t i ){return _py[i-1]; }
  Double_t  &pz( Int_t i ){return _pz[i-1]; }
  Double_t  &fmass( Int_t i ){return _fmass[i-1]; }
  Double_t  &dectime( Int_t i ){return _dectime[i-1]; }
};
extern "C" COOR_t *address_of_coor();

#define frag F77_NAME(frag,FRAG)
struct FRAG_t {
   /* parameter (nmax = 40000) ! maximum number of particles
      real*8 tform(nmax), xtotfac(nmax)
      common /frag/ tform, xtotfac*/
  Double_t  _tform[40000];
  Double_t  _xtotfrac[40000];
  Double_t  &tform( Int_t i ){return _tform[i-1]; }
  Double_t  &xtotfrac( Int_t i ){return _xtotfrac[i-1]; }
};
extern "C" FRAG_t *address_of_frag();

#define aios F77_NAME(aios,AIOS)
struct AIOS_t {
   /* parameter (nmax = 40000) ! maximum number of particles
      real*8 airx(nmax), airy(nmax), airz(nmax),
     &     aipx(nmax), aipy(nmax), aipz(nmax),
     &     aorx(nmax,4), aory(nmax,4), aorz(nmax,4),
     &     aopx(nmax,4), aopy(nmax,4), aopz(nmax,4)
      common /aios/ airx, airy, airz, aipx, aipy, aipz,
     &              aorx, aory, aorz, aopx, aopy, aopz*/
  Double_t  _airx[40000];
  Double_t  _airy[40000];
  Double_t  _airz[40000];
  Double_t  _aipx[40000];
  Double_t  _aipy[40000];
  Double_t  _aipz[40000];
  Double_t  _aorx[4][40000];
  Double_t  _aory[4][40000];
  Double_t  _aorz[4][40000];
  Double_t  _aopx[4][40000];
  Double_t  _aopy[4][40000];
  Double_t  _aopz[4][40000];
  Double_t  &airx( Int_t i ){return _airx[i-1]; }
  Double_t  &airy( Int_t i ){return _airy[i-1]; }
  Double_t  &airz( Int_t i ){return _airz[i-1]; }
  Double_t  &aipx( Int_t i ){return _aipx[i-1]; }
  Double_t  &aipy( Int_t i ){return _aipy[i-1]; }
  Double_t  &aipz( Int_t i ){return _aipz[i-1]; }
  Double_t  &aorx( Int_t i, Int_t j ){return _aorx[j-1][i-1]; }
  Double_t  &aory( Int_t i, Int_t j ){return _aory[j-1][i-1]; }
  Double_t  &aorz( Int_t i, Int_t j ){return _aorz[j-1][i-1]; }
  Double_t  &aopx( Int_t i, Int_t j ){return _aopx[j-1][i-1]; }
  Double_t  &aopy( Int_t i, Int_t j ){return _aopy[j-1][i-1]; }
  Double_t  &aopz( Int_t i, Int_t j ){return _aopz[j-1][i-1]; }
};
extern "C" AIOS_t *address_of_aios();

#define pots F77_NAME(pots,POTS)
struct POTS_t {
    /*real*8
     &     gw, sgw, delr, fdel, dt,da, db, Cb0, Yuk0, Pau0, Sky20,
     &     Sky30, gamSky, gamYuk, drPau, dpPau, dtimestep
      common /pots/ Cb0, Yuk0, Pau0, Sky20, Sky30, gamSky,
     &              gamYuk, drPau, dpPau, gw, sgw, delr, fdel,
     &              dt,da, db,dtimestep*/
  Double_t  Cb0;
  Double_t  Yuk0;
  Double_t  Pau0;
  Double_t  Sky20;
  Double_t  Sky30;
  Double_t  gamSky;
  Double_t  gamYuk;
  Double_t  drPau;
  Double_t  dpPau;
  Double_t  gw;
  Double_t  sgw;
  Double_t  delr;
  Double_t  fdel;
  Double_t  dt;
  Double_t  da;
  Double_t  db;
  Double_t  dtimestep;
};
extern "C" POTS_t *address_of_pots();

#define scoor F77_NAME(scoor,SCOOR)
struct SCOOR_t {
   /* parameter(smax=500)  ! maximum number of spectators
      real*8 r0s(smax), rxs(smax), rys(smax), rzs(smax),
     &         p0s(smax), pxs(smax), pys(smax), pzs(smax),
     &         sfmass(smax)
      common /scoor/ r0s, rxs, rys, rzs, p0s, pxs ,pys, pzs, sfmass*/
  Double_t  _r0s[500];
  Double_t  _rxs[500];
  Double_t  _rys[500];
  Double_t  _rzs[500];
  Double_t  _p0s[500];
  Double_t  _pxs[500];
  Double_t  _pys[500];
  Double_t  _pzs[500];
  Double_t  _sfmass[500];
  Double_t  &r0s( Int_t i ){return _r0s[i-1]; }
  Double_t  &rxs( Int_t i ){return _rxs[i-1]; }
  Double_t  &rys( Int_t i ){return _rys[i-1]; }
  Double_t  &rzs( Int_t i ){return _rzs[i-1]; }
  Double_t  &p0s( Int_t i ){return _p0s[i-1]; }
  Double_t  &pxs( Int_t i ){return _pxs[i-1]; }
  Double_t  &pys( Int_t i ){return _pys[i-1]; }
  Double_t  &pzs( Int_t i ){return _pzs[i-1]; }
  Double_t  &sfmass( Int_t i ){return _sfmass[i-1]; }
};
extern "C" SCOOR_t *address_of_scoor();

#define sisys F77_NAME(sisys,SISYS)
struct SISYS_t {
   /* parameter(smax=500)  ! maximum number of spectators
      integer sspin(smax), scharge(smax), sityp(smax), siso3(smax),
     &          suid(smax)
      common /sisys/ sspin, scharge, sityp, siso3, suid*/
  Int_t  _sspin[500];
  Int_t  _scharge[500];
  Int_t  _sityp[500];
  Int_t  _siso3[500];
  Int_t  _suid[500];
  Int_t  &sspin( Int_t i ){return _sspin[i-1]; }
  Int_t  &scharge( Int_t i ){return _scharge[i-1]; }
  Int_t  &sityp( Int_t i ){return _sityp[i-1]; }
  Int_t  &siso3( Int_t i ){return _siso3[i-1]; }
  Int_t  &suid( Int_t i ){return _suid[i-1]; }
};
extern "C" SISYS_t *address_of_sisys();

#define ssys F77_NAME(ssys,SSYS)
struct SSYS_t {
    /*integer nspec
      common /ssys/ nspec*/
  Int_t npsec;
};
extern "C" SSYS_t *address_of_ssys();

#define rtdelay F77_NAME(rtdelay,RTDELAY)
struct RTDELAY_t {
   /* parameter (nmax = 40000) ! maximum number of particles
      real*8 p0td(2,nmax),pxtd(2,nmax),pytd(2,nmax),pztd(2,nmax),
     &         fmasstd(2,nmax)
      common /rtdelay/p0td,pxtd,pytd,pztd,fmasstd*/
  Double_t  _p0td[40000][2];
  Double_t  _pxtd[40000][2];
  Double_t  _pytd[40000][2];
  Double_t  _pztd[40000][2];
  Double_t  _fmasstd[40000][2];
  Double_t  &p0td( Int_t i, Int_t j ){return _p0td[j-1][i-1]; }
  Double_t  &pxtd( Int_t i, Int_t j ){return _pxtd[j-1][i-1]; }
  Double_t  &pytd( Int_t i, Int_t j ){return _pytd[j-1][i-1]; }
  Double_t  &pztd( Int_t i, Int_t j ){return _pztd[j-1][i-1]; }
  Double_t  &fmasstd( Int_t i, Int_t j ){return _fmasstd[j-1][i-1]; }
};
extern "C" RTDELAY_t *address_of_rtdelay();

#define itdelay F77_NAME(itdelay,ITDELAY)
struct ITDELAY_t {
   /* parameter (nmax = 40000) ! maximum number of particles
      integer ityptd(2,nmax),iso3td(2,nmax)
      common /itdelay/ityptd,iso3td*/
  Int_t  _ityptd[40000][2];
  Int_t  _iso3td[40000][2];
  Int_t  &ityptd( Int_t i, Int_t j ){return _ityptd[j-1][i-1]; }
  Int_t  &iso3td( Int_t i, Int_t j ){return _iso3td[j-1][i-1]; }
};
extern "C" ITDELAY_t *address_of_itdelay();

#define svinfo F77_NAME(svinfo,SVINFO)
struct SVINFO_t {
    /*integer itypt(2),uidt(2),origint(2),iso3t(2)
      common /svinfo/itypt,uidt,origint,iso3t*/
  Int_t  _itypt[2];
  Int_t  _uidt[2];
  Int_t  _origint[2];
  Int_t  _iso3t[2];
  Int_t  &itypt( Int_t i ){return _itypt[i-1]; }
  Int_t  &uidt( Int_t i ){return _uidt[i-1]; }
  Int_t  &origint( Int_t i ){return _origint[i-1]; }
  Int_t  &iso3t( Int_t i ){return _iso3t[i-1]; }
};
extern "C" SVINFO_t *address_of_svinfo();

#define ffermi F77_NAME(ffermi,FFERMI)
struct FFERMI_t {
  /* parameter (nmax = 40000) ! maximum number of particles
      real*8 ffermpx(nmax), ffermpy(nmax), ffermpz(nmax)
      common /ffermi/ ffermpx, ffermpy, ffermpz*/
  Double_t  _ffermpx[40000];
  Double_t  _ffermpy[40000];
  Double_t  _ffermpz[40000];
  Double_t  &ffermpx( Int_t i ){return _ffermpx[i-1]; }
  Double_t  &ffermpy( Int_t i ){return _ffermpy[i-1]; }
  Double_t  &ffermpz( Int_t i ){return _ffermpz[i-1]; }
};
extern "C" FFERMI_t *address_of_ffermi();

#define peq F77_NAME(peq,PEQ)
struct PEQ_t {
    /*real*8 peq1, peq2
      common /peq/ peq1,peq2*/
  Double_t  peq1;
  Double_t  peq2;
};
extern "C" PEQ_t *address_of_peq();

void iurqmd( void );
void genevt( void );

/// Global Parameters which are needed in UrQMD.* and StarUrQMD.*
///JFN 11/20/12 12:05pm- I am moving these to StarUrQMD.h
/*std::vector< vector <string> > InputParameters;
map<TString,Int_t> InputParametersInt;
map<TString,Double_t> InputParametersDouble;
map<TString,TString> InputParametersString;*/

#endif
