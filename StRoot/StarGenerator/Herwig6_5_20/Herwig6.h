#ifndef __Herwig6__
#define __Herwig6__

#include "StarCallf77.h"
#include <string>
using namespace std;

#include "TObject.h"
/**
/// Generate a Herwig event
void HerEvnt();
// Print end of run statistics
void HerStat( Int_t stat );
/// List particles
void HerList( Int_t list );
/// Setup Herwig parameters
void HerSet();
/// Copy particles in *Some* common block
void HerSome( Int_t mode );
/// Initialize Herwig
void HerInit( string fram, string blue, string yellow, Double_t energy );
**/

//
// Interface to common Blocks
//

#define address_of_hepeup F77_NAME( address_of_hepeup, ADDRESS_OF_HEPEUP )
struct HEPEUP_t {
  /*  PARAMETER (MAXNUP=500)
      INTEGER NUP,IDPRUP,IDUP,ISTUP,MOTHUP,ICOLUP
      DOUBLE PRECISION XWGTUP,SCALUP,AQEDUP,AQCDUP,PUP,VTIMUP,SPINUP
      COMMON/HEPEUP/NUP,IDPRUP,XWGTUP,SCALUP,AQEDUP,AQCDUP,
     &              IDUP(MAXNUP),ISTUP(MAXNUP),MOTHUP(2,MAXNUP),
     &              ICOLUP(2,MAXNUP),PUP(5,MAXNUP),VTIMUP(MAXNUP),
     &              SPINUP(MAXNUP)*/
  /* Layout of the memory. */
  Int_t    nup;
  Int_t    idprup;
  Double_t xwgtup;
  Double_t scalup;
  Double_t aqedup;
  Double_t aqcdup;
  Int_t    _idup[500];
  Int_t    _istup[500];
  Int_t    _mothup[500][2];
  Int_t    _icolup[500][2];
  Double_t _pup[500][5];
  Double_t _vtimup[500];
  Double_t _spinup[500]; 
  /* Add access methods which mimic fortran arrays */
  Int_t    &idup( Int_t i ){return _idup[i-1]; }
  Int_t    &istup( Int_t i ){return _istup[i-1]; }
  Int_t    &mothup( Int_t i, Int_t j ){return _mothup[j-1][i-1]; }
  Int_t    &icolup( Int_t i, Int_t j ){return _icolup[j-1][i-1]; }
  Double_t &pup( Int_t i, Int_t j ){return _pup[j-1][i-1]; }
  Double_t &vtimup( Int_t i ){return _vtimup[i-1]; }
  Double_t &spinup( Int_t i ){return _spinup[i-1]; }
};
extern "C" HEPEUP_t *address_of_hepeup();

#define address_of_hwgup F77_NAME( adress_of_hwgup, ADDRESS_OF_HWGUP )
struct HWGUP_t {
  /*  INTEGER MAXNUP,ILOC,JLOC
      PARAMETER (MAXNUP=500,NMXHEP=4000)
      COMMON /HWGUP/ILOC(NMXHEP),JLOC(MAXNUP)*/
  Int_t    _iloc[4000];
  Int_t    _jloc[500];
  /* Add access methods which mimic fortran arrays */
  Int_t    &iloc( Int_t i ){return _iloc[i-1]; }
  Int_t    &jloc( Int_t i ){return _jloc[i-1]; }
};
extern "C" HWGUP_t *address_of_hwgup();

#define address_of_hepevt F77_NAME( address_of_hepevt, ADDRESS_OF_HEPEVT )
struct HEPEVT_t {
  /*  PARAMETER (NMXHEP=4000)
      INTEGER NEVHEP,NHEP,ISTHEP,IDHEP,JMOHEP,JDAHEP
      DOUBLE PRECISION PHEP,VHEP
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     & JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
     address_of_hepevt = loc ( nevhep )*/
  /* Layout of the memory */
  Int_t    nevhep;
  Int_t    nhep;
  Int_t    _isthep[4000];
  Int_t    _idhep[4000];
  Int_t    _jmohep[4000][2];
  Int_t    _jdahep[4000][2];
  Double_t _phep[4000][2];
  Double_t _vhep[4000][4];
  /* Add access methods which mimic fortran arrays */
  Int_t    &isthep( Int_t i ){return _isthep[i-1]; }
  Int_t    &idhep( Int_t i ){return _idhep[i-1]; }
  Int_t    &jmohep( Int_t i, Int_t j ){return _jmohep[j-1][i-1]; }
  Int_t    &jdahep( Int_t i, Int_t j ){return _jdahep[j-1][i-1]; }
  Double_t &phep( Int_t i, Int_t j ){return _phep[j-1][i-1]; }
  Double_t &vhep( Int_t i, Int_t j ){return _vhep[j-1][i-1]; }
};
extern "C" HEPEVT_t *address_of_hepevt();

#define address_of_hwbeam F77_NAME( address_of_hwbeam, ADDRESS_OF_HWBEAM )
struct HWBEAM_t {
  /*  INTEGER IPART1,IPART2
      COMMON/HWBEAM/IPART1,IPART2*/
  /* Layout of the memory */
  Int_t    ipart1;
  Int_t    ipart2;
};
extern "C" HWBEAM_t *address_of_hwbeam();

#define address_of_hwproc F77_NAME( address_of_hwproc, ADDRESS_OF_HWPROC )
struct HWPROC_t {
  /*  DOUBLE PRECISION EBEAM1,EBEAM2,PBEAM1,PBEAM2
      INTEGER IPROC,MAXEV
      COMMON/HWPROC/EBEAM1,EBEAM2,PBEAM1,PBEAM2,IPROC,MAXEV*/
  Double_t ebeam1;
  Double_t ebeam2;
  Double_t pbeam1;
  Double_t pbeam2;
  Int_t    iproc;
  Int_t    maxev;
};
extern "C" HWPROC_t *address_of_hwproc();

#define address_of_hwpram F77_NAME( address_of_hwpram, ADDRESS_OF_HWPRAM )
struct HWPRAM_t {
  /*  DOUBLE PRECISION AFCH,ALPHEM,B1LIM,BETAF,BTCLM,CAFAC,CFFAC,
     & CLMAX,CLPOW,CLSMR,CSPEED,ENSOF,ETAMIX,F0MIX,F1MIX,F2MIX,GAMH,
     & GAMW,GAMZ,GAMZP,GEV2NB,H1MIX,PDIQK,PGSMX,PGSPL,PHIMIX,PIFAC,
     & PRSOF,PSPLT,PTRMS,PXRMS,QCDL3,QCDL5,QCDLAM,QDIQK,QFCH,QG,
     & QSPAC,QV,SCABI,SWEIN,TMTOP,VFCH,VCKM,VGCUT,VQCUT,
     & VPCUT,ZBINM,EFFMIN,OMHMIX,ET2MIX,PH3MIX,GCUTME
      INTEGER IOPREM,IPRINT,ISPAC,LRSUD,LWSUD,MODPDF,NBTRY,NCOLO,
     & NCTRY,NDTRY,NETRY,NFLAV,NGSPL,NSTRU,NSTRY,NZBIN,IOP4JT,
     & NPRFMT,CLDIR
      LOGICAL AZSOFT,AZSPIN,HARDME,NOSPAC,PRNDEC,PRVTX,SOFTME,ZPRIME,
     & PRNDEF,PRNTEX,PRNWEB
      COMMON/HWPRAM/AFCH(16,2),ALPHEM,B1LIM,BETAF,BTCLM,CAFAC,CFFAC,
     & CLMAX,CLPOW,CLSMR(2),CSPEED,ENSOF,ETAMIX,F0MIX,F1MIX,F2MIX,GAMH,
     & GAMW,GAMZ,GAMZP,GEV2NB,H1MIX,PDIQK,PGSMX,PGSPL(4),PHIMIX,PIFAC,
     & PRSOF,PSPLT(2),PTRMS,PXRMS,QCDL3,QCDL5,QCDLAM,QDIQK,QFCH(16),QG,
     & QSPAC,QV,SCABI,SWEIN,TMTOP,VFCH(16,2),VCKM(3,3),VGCUT,VQCUT,
     & VPCUT,ZBINM,EFFMIN,OMHMIX,ET2MIX,PH3MIX,GCUTME,
     & IOPREM,IPRINT,ISPAC,LRSUD,LWSUD,MODPDF(2),NBTRY,NCOLO,NCTRY,
     & NDTRY,NETRY,NFLAV,NGSPL,NSTRU,NSTRY,NZBIN,IOP4JT(2),NPRFMT,
     & AZSOFT,AZSPIN,CLDIR(2),HARDME,NOSPAC,PRNDEC,PRVTX,SOFTME,ZPRIME,
     & PRNDEF,PRNTEX,PRNWEB */
  Double_t  _afch[2][16];
  Double_t  alphem;
  Double_t  b1lim;
  Double_t  betaf;
  Double_t  btclm;
  Double_t  cafac;
  Double_t  cffac;
  Double_t  clmax;
  Double_t  clpow;
  Double_t  _clsmr[2];
  Double_t  cspeed;
  Double_t  ensof;
  Double_t  etamix;
  Double_t  f0mix;
  Double_t  f1mix;
  Double_t  f2mix;
  Double_t  gamh;
  Double_t  gamw;
  Double_t  gamz;
  Double_t  gamzp;
  Double_t  gev2nb;
  Double_t  h1mix;
  Double_t  pdiqk;
  Double_t  pgsmx;
  Double_t  _pgspl[4];
  Double_t  phimix;
  Double_t  pifac;
  Double_t  prsof;
  Double_t  _psplt[2];
  Double_t  ptrms;
  Double_t  pxrms;
  Double_t  qcdl3;
  Double_t  qcdl5;
  Double_t  qcdlam;
  Double_t  qdiqk;
  Double_t  _qfch[16];
  Double_t  qg;
  Double_t  qspac;
  Double_t  qv;
  Double_t  scabi;
  Double_t  swein;
  Double_t  tmtop;
  Double_t  _vfch[2][16];
  Double_t  _vckm[3][3];
  Double_t  vgcut;
  Double_t  vqcut;
  Double_t  vpcut;
  Double_t  zbinm;
  Double_t  effmin;
  Double_t  omhmix;
  Double_t  et2mix;
  Double_t  ph3mix;
  Double_t  gcutme;
  Int_t     ioprem;
  Int_t     iprint;
  Int_t     ispac;
  Int_t     lrsud;
  Int_t     lwsud;
  Int_t     _modpdf[2];
  Int_t     nbtry;
  Int_t     ncolo;
  Int_t     nctry;
  Int_t     ndtry;
  Int_t     netry;
  Int_t     nflav;
  Int_t     ngspl;
  Int_t     nstru;
  Int_t     nstry;
  Int_t     nzbin;
  Int_t     _iop4jt[2];
  Int_t     nprfmt;
  Bool_t    azsoft;
  Bool_t    azspin;
  Int_t     _cldir[2];
  Bool_t    hardme;
  Bool_t    nospac;
  Bool_t    prndec;
  Bool_t    prvtx;
  Bool_t    softme;
  Bool_t    zprime;
  Bool_t    prndef;
  Bool_t    prntex;
  Bool_t    prnweb;
  Double_t &afch( Int_t i, Int_t j ){return _afch[j-1][i-1]; }
  Double_t &clsmr( Int_t i ){return _clsmr[i-1]; }
  Double_t &psplt( Int_t i ){return _psplt[i-1]; }
  Double_t &qfch( Int_t i ){return _qfch[i-1]; }
  Double_t &vfch( Int_t i, Int_t j ){return _vfch[j-1][i-1]; }
  Double_t &vckm( Int_t i, Int_t j ){return _vckm[j-1][i-1]; }
  Int_t    &modpdf( Int_t i ){return _modpdf[i-1]; }
  Int_t    &iop4jt( Int_t i ){return _iop4jt[i-1]; }
  Int_t    &cldir( Int_t i ){return _cldir[i-1]; }
};
extern "C" HWPRAM_t *address_of_hwpram();

#define address_of_hwbrch F77_NAME( address_of_hwbrch, ADDRESS_OF_HWBRCH )
struct HWBRCH_t {
  /*  INTEGER INHAD,JNHAD
      DOUBLE PRECISION ANOMSC,HARDST,PTINT,XFACT
      COMMON/HWBRCH/ANOMSC(2,2),HARDST,PTINT(3,2),XFACT,INHAD,JNHAD,
      & NSPAC(7),ISLENT,BREIT,FROST,USECMF*/
  Double_t _anomsc[2][2];
  Double_t hardst;
  Double_t _ptint[2][3];
  Double_t xfact;
  Int_t    inhad;
  Int_t    jnhad;
  Double_t &anomsc( Int_t i, Int_t j ){return _anomsc[j-1][i-1]; }
  Double_t &ptint( Int_t i, Int_t j ){return _ptint[j-1][i-1]; }
};
extern "C" HWBRCH_t *address_of_hwbrch();

#define address_of_hwevnt F77_NAME( address_of_hwevnt, ADDRESS_OF_HWEVNT )
struct HWEVNT_t {
  /*
       DOUBLE PRECISION AVWGT,EVWGT,GAMWT,TLOUT,WBIGST,WGTMAX,WGTSUM,
     & WSQSUM
      INTEGER IDHW,IERROR,ISTAT,LWEVT,MAXER,MAXPR,NRN,NUMER,NUMERU,
     & NWGTS
      LOGICAL NOWGT,GENSOF
      PARAMETER (NMXHEP=4000)
      COMMON/HWEVNT/AVWGT,EVWGT,GAMWT,TLOUT,WBIGST,WGTMAX,WGTSUM,WSQSUM,
     & IDHW(NMXHEP),IERROR,ISTAT,LWEVT,MAXER,MAXPR,NOWGT,NRN(2),NUMER,
     & NUMERU,NWGTS,GENSOF*/
  Double_t avwgt;
  Double_t evwgt;
  Double_t gamwt;
  Double_t tlout;
  Double_t wbigst;
  Double_t wgtmax;
  Double_t wgtsum;
  Double_t wsqsum;
  Int_t    _idhw[4000];
  Int_t    ierror;
  Int_t    istat;
  Int_t    lwevt;
  Int_t    maxer;
  Int_t    maxpr;
  Bool_t   nowgt;
  Int_t    _nrn[2];
  Int_t    numer;
  Int_t    nwgts;
  Bool_t   gensof;
  Int_t    &idhw( Int_t i ){return _idhw[i-1]; }
  Int_t    &nrn( Int_t i ){return _nrn[i-1]; }
};
extern "C" HWEVNT_t *address_of_hwevnt();

#define address_of_hwhard F77_NAME( address_of_hwhard, ADDRESS_OF_HWHARD )
struct HWHARD_t {
  /*  DOUBLE PRECISION ASFIXD,CLQ,COSS,COSTH,CTMAX,DISF,EMLST,EMMAX,
     & EMMIN,EMPOW,EMSCA,EPOLN,GCOEF,GPOLN,OMEGA0,PHOMAS,PPOLN,
     & PTMAX,PTMIN,PTPOW,Q2MAX,Q2MIN,Q2POW,Q2WWMN,Q2WWMX,QLIM,SINS,THMAX,
     & Y4JT,TMNISR,TQWT,XX,XLMIN,XXMIN,YBMAX,YBMIN,YJMAX,YJMIN,YWWMAX,
     & YWWMIN,WHMIN,ZJMAX,ZMXISR
      INTEGER IAPHIG,IBRN,IBSH,ICO,IDCMF,IDN,IFLMAX,IFLMIN,IHPRO,IPRO,
     & MAPQ,MAXFL
      LOGICAL BGSHAT,COLISR,FSTEVT,FSTWGT,GENEV,HVFCEN,TPOL,DURHAM
      COMMON/HWHARD/ASFIXD,CLQ(7,6),COSS,COSTH,CTMAX,DISF(13,2),EMLST,
     & EMMAX,EMMIN,EMPOW,EMSCA,EPOLN(3),GCOEF(7),GPOLN,OMEGA0,PHOMAS,
     & PPOLN(3),PTMAX,PTMIN,PTPOW,Q2MAX,Q2MIN,Q2POW,Q2WWMN,Q2WWMX,QLIM,
     & SINS,THMAX,Y4JT,TMNISR,TQWT,XX(2),XLMIN,XXMIN,YBMAX,YBMIN,YJMAX,
     & YJMIN,YWWMAX,YWWMIN,WHMIN,ZJMAX,ZMXISR,IAPHIG,IBRN(2),IBSH,
     & ICO(10),IDCMF,IDN(10),IFLMAX,IFLMIN,IHPRO,IPRO,MAPQ(6),MAXFL,
     & BGSHAT,COLISR,FSTEVT,FSTWGT,GENEV,HVFCEN,TPOL,DURHAM*/
  Double_t asfixd;
  Double_t _clq[6][7];
  Double_t coss;
  Double_t costh;
  Double_t ctmax;
  Double_t _disf[2][13];
  Double_t emlst;
  Double_t emmax;
  Double_t emmin;
  Double_t empow;
  Double_t emsca;
  Double_t _epoln[3];
  Double_t _gcoef[7];
  Double_t gpoln;
  Double_t omega0;
  Double_t phomas;
  Double_t _ppoln[3];
  Double_t ptmax;
  Double_t ptmin;
  Double_t ptpow;
  Double_t q2max;
  Double_t q2min;
  Double_t q2pow;
  Double_t q2wwmn;
  Double_t q2wwmx;
  Double_t qlim;
  Double_t sins;
  Double_t thmax;
  Double_t y4jt;
  Double_t tmnisr;
  Double_t tqwt;
  Double_t _xx[2];
  Double_t xlmin;
  Double_t xxmin;
  Double_t ybmax;
  Double_t ybmin;
  Double_t yjmax;
  Double_t yjmin;
  Double_t ywwmax;
  Double_t ywwmin;
  Double_t whmin;
  Double_t zjmax;
  Double_t zmxisr;
  Int_t    iaphig;
  Int_t    _ibrn[2];
  Int_t    ibsh;
  Int_t    _ico[10];
  Int_t    idcmf;
  Int_t    _idn[10];
  Int_t    iflmax;
  Int_t    iflmin;
  Int_t    ihpro;
  Int_t    ipro;
  Int_t    _mapq[6];
  Int_t    maxfl;
  Bool_t   bgshat;
  Bool_t   colisr;
  Bool_t   fstevt;
  Bool_t   fstwgt;
  Bool_t   genev;
  Bool_t   hvfcen;
  Bool_t   tpol;
  Bool_t   durham;
  Double_t &clq( Int_t i, Int_t j ){return _clq[j-1][i-1]; }
  Double_t &disf( Int_t i, Int_t j){return _disf[j-1][i-1]; }
  Double_t &epoln( Int_t i ){return _epoln[i-1]; }
  Double_t &gcoef( Int_t i ){return _gcoef[i-1]; }
  Double_t &ppoln( Int_t i ){return _ppoln[i-1]; }
  Double_t &xx( Int_t i ){return _xx[i-1]; }
  Int_t    &ibrn( Int_t i ){return _ibrn[i-1]; }
  Int_t    &ico( Int_t i ){return _ico[i-1]; }
  Int_t    &idn( Int_t i ){return _idn[i-1]; }
  Int_t    &mapq( Int_t i ){return _mapq[i-1]; }
};
extern "C" HWHARD_t *address_of_hwhard();

#define address_of_custom F77_NAME( address_of_custom, ADDRESS_OF_CUSTOM )
struct CUSTOM_t {
  Double_t hwmans;
  Double_t hwmant;
  Double_t hwmanu;
};
extern "C" CUSTOM_t *address_of_custom();

void HWSetBeams( string part1, string part2 );

void InitializeEvent( vector<string> particles );

void GenerateEvent();

void HWEFIN();

#endif
