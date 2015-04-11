#ifndef __G3Commons_h__
#define __G3Commons_h__

#include <string>
#include <iostream>
using namespace std;


#define setter( tag, var ) if ( #tag == cut ) { var = val; return; }

// IMported from TGiant3.h

//----------QUEST 
//      COMMON/QUEST/IQUEST(100) 
typedef struct { 
  Int_t    iquest[100]; 
} Quest_t; 
 
//----------GCBANK
//      COMMON/GCBANK/NZEBRA,GVERSN,ZVERSN,IXSTOR,IXDIV,IXCONS,FENDQ(16)
//     +             ,LMAIN,LR1,WS(KWBANK)
typedef struct {
  Int_t nzebra;
  Float_t gversn;
  Float_t zversn;
  Int_t ixstor;
  Int_t ixdiv;
  Int_t ixcons;
  Float_t fendq[16];
  Int_t lmain;
  Int_t lr1;
} Gcbank_t;

//----------GCLINK 
//      COMMON/GCLINK/JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART 
//     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX 
//     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT 
typedef struct { 
  Int_t    jdigi; 
  Int_t    jdraw; 
  Int_t    jhead; 
  Int_t    jhits; 
  Int_t    jkine; 
  Int_t    jmate; 
  Int_t    jpart; 
  Int_t    jrotm; 
  Int_t    jrung; 
  Int_t    jset; 
  Int_t    jstak; 
  Int_t    jgstat; 
  Int_t    jtmed; 
  Int_t    jtrack; 
  Int_t    jvertx; 
  Int_t    jvolum; 
  Int_t    jxyz; 
  Int_t    jgpar; 
  Int_t    jgpar2; 
  Int_t    jsklt; 
} Gclink_t; 
 
 
//----------GCFLAG 
//      COMMON/GCFLAG/IDEBUG,IDEMIN,IDEMAX,ITEST,IDRUN,IDEVT,IEORUN 
//     +        ,IEOTRI,IEVENT,ISWIT(10),IFINIT(20),NEVENT,NRNDM(2) 
typedef struct { 
  Int_t    idebug; 
  Int_t    idemin; 
  Int_t    idemax; 
  Int_t    itest; 
  Int_t    idrun; 
  Int_t    idevt; 
  Int_t    ieorun; 
  Int_t    ieotri; 
  Int_t    ievent; 
  Int_t    iswit[10]; 
  Int_t    ifinit[20]; 
  Int_t    nevent; 
  Int_t    nrndm[2]; 
} Gcflag_t; 
 
//----------GCKINE 
//      COMMON/GCKINE/IKINE,PKINE(10),ITRA,ISTAK,IVERT,IPART,ITRTYP 
//     +      ,NAPART(5),AMASS,CHARGE,TLIFE,VERT(3),PVERT(4),IPAOLD 
typedef struct { 
  Int_t    ikine; 
  Float_t  pkine[10]; 
  Int_t    itra; 
  Int_t    istak; 
  Int_t    ivert; 
  Int_t    ipart; 
  Int_t    itrtyp; 
  Int_t    napart[5]; 
  Float_t  amass; 
  Float_t  charge; 
  Float_t  tlife; 
  Float_t  vert[3]; 
  Float_t  pvert[4]; 
  Int_t    ipaold; 
} Gckine_t; 
 
//----------GCKING 
//      COMMON/GCKING/KCASE,NGKINE,GKIN(5,MXGKIN), 
//     +                           TOFD(MXGKIN),IFLGK(MXGKIN) 
#define MXGKIN 100 
typedef struct  { 
  Int_t    kcase; 
  Int_t    ngkine; 
  Float_t  gkin[MXGKIN][5]; 
  Int_t    tofd[MXGKIN]; 
  Int_t    iflgk[MXGKIN]; 
} Gcking_t; 

//----------GCKIN2
//      COMMON/GCKIN2/NGPHOT,XPHOT(11,MXPHOT)
#define MXPHOT 800
typedef struct {
  Int_t ngphot;
  Float_t xphot[MXPHOT][11];
} Gckin2_t;

//----------GCKIN3 
//      COMMON/GCKIN3/GPOS(3,MXGKIN)
typedef struct {
  Float_t gpos[MXGKIN][3];
} Gckin3_t;

//----------GCMATE 
//      COMMON/GCMATE/NMAT,NAMATE(5),A,Z,DENS,RADL,ABSL 
typedef struct { 
  Int_t    nmat; 
  Int_t    namate[5]; 
  Float_t  a; 
  Float_t  z; 
  Float_t  dens; 
  Float_t  radl; 
  Float_t  absl; 
} Gcmate_t; 
 
//----------GCTMED 
//      COMMON/GCTMED/NUMED,NATMED(5),ISVOL,IFIELD,FIELDM,TMAXFD,STEMAX 
//     +      ,DEEMAX,EPSIL,STMIN,CFIELD,PREC,IUPD,ISTPAR,NUMOLD 
typedef struct { 
  Int_t    numed; 
  Int_t    natmed[5]; 
  Int_t    isvol; 
  Int_t    ifield; 
  Float_t  fieldm; 
  Float_t  tmaxfd; 
  Float_t  stemax; 
  Float_t  deemax; 
  Float_t  epsil; 
  Float_t  stmin; 
  Float_t  cfield; 
  Float_t  prec; 
  Int_t    iupd; 
  Int_t    istpar; 
  Int_t    numold; 
} Gctmed_t; 
 
//----------GCTRAK 
#define MAXMEC 30 
//      PARAMETER (MAXMEC=30) 
//      COMMON/GCTRAK/VECT(7),GETOT,GEKIN,VOUT(7),NMEC,LMEC(MAXMEC) 
//     + ,NAMEC(MAXMEC),NSTEP ,MAXNST,DESTEP,DESTEL,SAFETY,SLENG 
//     + ,STEP  ,SNEXT ,SFIELD,TOFG  ,GEKRAT,UPWGHT,IGNEXT,INWVOL 
//     + ,ISTOP ,IGAUTO,IEKBIN, ILOSL, IMULL,INGOTO,NLDOWN,NLEVIN 
//     + ,NLVSAV,ISTORY 
typedef struct { 
  Float_t  vect[7]; 
  Float_t  getot; 
  Float_t  gekin; 
  Int_t    vout[7]; 
  Int_t    nmec; 
  Int_t    lmec[MAXMEC]; 
  Int_t    namec[MAXMEC]; 
  Int_t    nstep; 
  Int_t    maxnst; 
  Float_t  destep; 
  Float_t  destel; 
  Float_t  safety; 
  Float_t  sleng; 
  Float_t  step; 
  Float_t  snext; 
  Float_t  sfield; 
  Float_t  tofg; 
  Float_t  gekrat; 
  Float_t  upwght; 
  Int_t    ignext; 
  Int_t    inwvol; 
  Int_t    istop; 
  Int_t    igauto; 
  Int_t    iekbin; 
  Int_t    ilosl; 
  Int_t    imull; 
  Int_t    ingoto; 
  Int_t    nldown; 
  Int_t    nlevin; 
  Int_t    nlsav; 
  Int_t    istory; 
} Gctrak_t; 
 
//----------GCVOLU 
//      COMMON/GCVOLU/NLEVEL,NAMES(15),NUMBER(15), 
//     +LVOLUM(15),LINDEX(15),INFROM,NLEVMX,NLDEV(15),LINMX(15), 
//     +GTRAN(3,15),GRMAT(10,15),GONLY(15),GLX(3) 
typedef struct { 
  Int_t    nlevel; 
  Int_t    names[15]; 
  Int_t    number[15]; 
  Int_t    lvolum[15]; 
  Int_t    lindex[15]; 
  Int_t    infrom; 
  Int_t    nlevmx; 
  Int_t    nldev[15]; 
  Int_t    linmx[15]; 
  Float_t  gtran[15][3]; 
  Float_t  grmat[15][10]; 
  Float_t  gonly[15]; 
  Float_t  glx[3]; 
} Gcvolu_t; 
 
//----------GCSETS 
//  COMMON/GCSETS/IHSET,IHDET,ISET,IDET,IDTYPE,NVNAME,NUMBV(20) 
typedef struct { 
  Int_t    ihset; 
  Int_t    ihdet; 
  Int_t    iset; 
  Int_t    idet; 
  Int_t    idtype; 
  Int_t    nvname; 
  Int_t    numbv[20]; 
} Gcsets_t; 
 
//----------GCNUM 
//   COMMON/GCNUM/NMATE ,NVOLUM,NROTM,NTMED,NTMULT,NTRACK,NPART 
//  +            ,NSTMAX,NVERTX,NHEAD,NBIT 
typedef struct { 
  Int_t    nmate; 
  Int_t    nvolum; 
  Int_t    nrotm; 
  Int_t    ntmed; 
  Int_t    ntmult; 
  Int_t    ntrack; 
  Int_t    npart; 
  Int_t    nstmax; 
  Int_t    nvertx; 
  Int_t    nhead; 
  Int_t    nbit; 
} Gcnum_t; 
 
//----------GCCUTS 
//  COMMON/GCCUTS/CUTGAM,CUTELE,CUTNEU,CUTHAD,CUTMUO,BCUTE,BCUTM 
//   +             ,DCUTE ,DCUTM ,PPCUTM,TOFMAX,GCUTS(5) 
struct Gccuts_t { 
  Float_t cutgam; 
  Float_t cutele; 
  Float_t cutneu; 
  Float_t cuthad; 
  Float_t cutmuo; 
  Float_t bcute; 
  Float_t bcutm; 
  Float_t dcute; 
  Float_t dcutm; 
  Float_t ppcutm; 
  Float_t tofmax; 
  Float_t gcuts[5];
  void set( string cut, float val ) { 
    if ( "cutgam" == cut ) cutgam = val;
    if ( "cutele" == cut ) cutele = val;
    if ( "cutneu" == cut ) cutneu = val;
    if ( "cuthad" == cut ) cuthad = val;
    if ( "cutmuo" == cut ) cutmuo = val;
    if ( "bcute"  == cut ) bcute  = val;
    if ( "bcutm"  == cut ) bcutm  = val;
    if ( "dcute"  == cut ) dcute  = val; 
    if ( "dcutm"  == cut ) dcutm  = val;
    if ( "ppcutm" == cut ) ppcutm = val;
    if ( "tofmax" == cut ) tofmax = val;
  };
  // ctor initializes default values
  Gccuts_t() : cutgam( 0.001 ), 
	       cutele( 0.001 ),
	       cutneu( 0.001 ),
	       cuthad( 0.001 ),
	       cutmuo( 0.001 ),
	       bcute ( -1.0 ),
	       bcutm ( -1.0 ),
	       dcute ( -1.0 ),
	       dcutm ( -1.0 ),
	       ppcutm( 0.01 ),
	       tofmax( 1E+10 )
  {
    for (Int_t i = 0; i < 5; i++) gcuts[i] = 0;
  };
};

//----------GCMULO
//      COMMON/GCMULO/SINMUL(101),COSMUL(101),SQRMUL(101),OMCMOL,CHCMOL
//     +  ,EKMIN,EKMAX,NEKBIN,NEK1,EKINV,GEKA,GEKB,EKBIN(200),ELOW(200)
typedef struct {
  Float_t sinmul[101];
  Float_t cosmul[101];
  Float_t sqrmul[101];
  Float_t omcmol;
  Float_t chcmol;
  Float_t ekmin;
  Float_t ekmax;
  Int_t   nekbin;
  Int_t   nek1;
  Float_t ekinv;
  Float_t geka;
  Float_t gekb;
  Float_t ekbin[200];
  Float_t elow[200];
} Gcmulo_t;

//----------GCPHYS
//      COMMON/GCPHYS/IPAIR,SPAIR,SLPAIR,ZINTPA,STEPPA
//     +             ,ICOMP,SCOMP,SLCOMP,ZINTCO,STEPCO
//     +             ,IPHOT,SPHOT,SLPHOT,ZINTPH,STEPPH
//     +             ,IPFIS,SPFIS,SLPFIS,ZINTPF,STEPPF
//     +             ,IDRAY,SDRAY,SLDRAY,ZINTDR,STEPDR
//     +             ,IANNI,SANNI,SLANNI,ZINTAN,STEPAN
//     +             ,IBREM,SBREM,SLBREM,ZINTBR,STEPBR
//     +             ,IHADR,SHADR,SLHADR,ZINTHA,STEPHA
//     +             ,IMUNU,SMUNU,SLMUNU,ZINTMU,STEPMU
//     +             ,IDCAY,SDCAY,SLIFE ,SUMLIF,DPHYS1
//     +             ,ILOSS,SLOSS,SOLOSS,STLOSS,DPHYS2
//     +             ,IMULS,SMULS,SOMULS,STMULS,DPHYS3
//     +             ,IRAYL,SRAYL,SLRAYL,ZINTRA,STEPRA
//      COMMON/GCPHLT/ILABS,SLABS,SLLABS,ZINTLA,STEPLA
//     +             ,ISYNC
//     +             ,ISTRA

struct Gcbirk_t { // addition: birk's law parameterization
  Float_t birk[4];
  Gcbirk_t() {for (Int_t i = 0; i < 4; i++) birk[i] = 0; }
  void set( string cut, float val ) { 
    setter( birk1, birk[0] );
    setter( birk2, birk[1] );
    setter( birk3, birk[2] );
    setter( birk4, birk[3] );
  };
};

struct Gcphlt_t {
  Int_t   ilabs;
  Float_t slabs;
  Float_t sllabs;
  Float_t zintla;
  Float_t stepla;
  Int_t isync;
  Int_t istra;
  Gcphlt_t() : ilabs(1), slabs(0), sllabs(0), zintla(0), stepla(0), isync(1), istra(1)
  {      };
  void set( string cut, float val ) { 
    setter( labs, ilabs );
    setter( sync, isync );
    setter( stra, istra );
    setter( labs, slabs );
  }
};

struct Gcphys_t {
  Int_t    ipair;
  Float_t  spair;
  Float_t  slpair;
  Float_t  zintpa;
  Float_t  steppa;
  Int_t    icomp;
  Float_t  scomp;
  Float_t  slcomp;
  Float_t  zintco;
  Float_t  stepco;
  Int_t    iphot;
  Float_t  sphot;
  Float_t  slphot;
  Float_t  zintph;
  Float_t  stepph;
  Int_t    ipfis;
  Float_t  spfis;
  Float_t  slpfis;
  Float_t  zintpf;
  Float_t  steppf;
  Int_t    idray;
  Float_t  sdray;
  Float_t  sldray;
  Float_t  zintdr;
  Float_t  stepdr;
  Int_t    ianni;
  Float_t  sanni;
  Float_t  slanni;
  Float_t  zintan;
  Float_t  stepan;
  Int_t    ibrem;
  Float_t  sbrem;
  Float_t  slbrem;
  Float_t  zintbr;
  Float_t  stepbr;
  Int_t    ihadr;
  Float_t  shadr;
  Float_t  slhadr;
  Float_t  zintha;
  Float_t  stepha;
  Int_t    imunu;
  Float_t  smunu;
  Float_t  slmunu;
  Float_t  zintmu;
  Float_t  stepmu;
  Int_t    idcay;
  Float_t  sdcay;
  Float_t  slife;
  Float_t  sumlif;
  Float_t  dphys1;
  Int_t    iloss;
  Float_t  sloss;
  Float_t  soloss;
  Float_t  stloss;
  Float_t  dphys2;
  Int_t    imuls;
  Float_t  smuls;
  Float_t  somuls;
  Float_t  stmuls;
  Float_t  dphys3;
  Int_t    irayl;
  Float_t  srayl;
  Float_t  slrayl;
  Float_t  zintra;
  Float_t  stepra;
  Gcphys_t() :
    ipair(0),
    spair(0),
    slpair(0),
    zintpa(0),
    steppa(0),
    icomp(0),
    scomp(0),
    slcomp(0),
    zintco(0),
    stepco(0),
    iphot(0),
    sphot(0),
    slphot(0),
    zintph(0),
    stepph(0),
    ipfis(0),
    spfis(0),
    slpfis(0),
    zintpf(0),
    steppf(0),
    idray(0),
    sdray(0),
    sldray(0),
    zintdr(0),
    stepdr(0),
    ianni(0),
    sanni(0),
    slanni(0),
    zintan(0),
    stepan(0),
    ibrem(0),
    sbrem(0),
    slbrem(0),
    zintbr(0),
    stepbr(0),
    ihadr(0),
    shadr(0),
    slhadr(0),
    zintha(0),
    stepha(0),
    imunu(0),
    smunu(0),
    slmunu(0),
    zintmu(0),
    stepmu(0),
    idcay(0),
    sdcay(0),
    slife(0),
    sumlif(0),
    dphys1(0),
    iloss(0),
    sloss(0),
    soloss(0),
    stloss(0),
    dphys2(0),
    imuls(0),
    smuls(0),
    somuls(0),
    stmuls(0),
    dphys3(0),
    irayl(0),
    srayl(0),
    slrayl(0),
    zintra(0),
    stepra(0)  {    };
  void set( string cut, float val ) { 
    setter( pair, ipair );
    setter( comp, icomp );
    setter( phot, iphot );
    setter( pfis, ipfis );
    setter( dray, idray );
    setter( anni, ianni );
    setter( brem, ibrem );
    setter( hadr, ihadr );
    setter( munu, imunu );
    setter( dcay, idcay );
    setter( loss, iloss );
    setter( muls, imuls ); 
    setter( rayl, irayl );
    
  };
}; 

//----------GCOPTI 
//      COMMON/GCOPTI/IOPTIM
typedef struct { 
  Int_t   ioptim;
} Gcopti_t; 
 
//----------GCTLIT 
//      COMMON/GCTLIT/THRIND,PMIN,DP,DNDL,JMIN,ITCKOV,IMCKOV,NPCKOV
typedef struct { 
  Float_t   thrind;
  Float_t   pmin;
  Float_t   dp;
  Float_t   dndl;
  Int_t     jmin;
  Int_t     itckov;
  Int_t     imckov;
  Int_t     npckov;
} Gctlit_t; 
 
//----------GCVDMA 
//      COMMON/GCVDMA/NVMANY,MANYLE(20),MANYNA(20,15),
//     +MANYNU(20,15),NFMANY,MYCOUN,IMYSE,RAYTRA,VECCOS(3)
typedef struct { 
  Int_t     vdma[624];
  Float_t   raytra;
  Float_t   veccos[3];
} Gcvdma_t; 
 
//----------GCTPOL 
#define MAXME1 30 
//      COMMON/GCTPOL/POLAR(3), NAMEC1(MAXME1) 
typedef struct { 
  Float_t polar[3]; 
  Int_t   namec1[MAXME1]; 
} Gctpol_t; 

/************************************************************************
 *                                                                      *
 *      Commons for GEANE                                               *
 *                                                                      *
 ************************************************************************/

//------------ERTRIO
//    INTEGER          MXPRED
//    PARAMETER (MXPRED = 10)
//    DOUBLE PRECISION ERDTRP
//    REAL             ERRIN, ERROUT, ERTRSP, ERXIN, ERXOUT, ERPIN,
//   +                 ERPOUT
//    INTEGER          NEPRED, INLIST, ILPRED, IEPRED
//    COMMON /ERTRIO/  ERDTRP(5,5,MXPRED), ERRIN(15), ERROUT(15,MXPRED),
//   +                 ERTRSP(5,5,MXPRED), ERXIN( 3), ERXOUT( 3,MXPRED),
//   +                 ERPIN(3), ERPOUT(3,MXPRED), NEPRED,INLIST,ILPRED,
//   +                 IEPRED(MXPRED)
//

#define MXPRED 10
typedef struct {
  Double_t erdtrp[MXPRED*5*5];
  Float_t  errin[5];
  Float_t  errout[MXPRED*15];
  Float_t  ertrsp[MXPRED*5*5];
  Float_t  erxin[3];
  Float_t  erxout[MXPRED*3];
  Float_t  erpin[3];
  Float_t  erpout[MXPRED*3];
  Int_t    nepred;
  Int_t    inlist;
  Int_t    ilpred;
  Int_t    iepred;
} Ertrio_t;

//-----------EROTPS
//    CHARACTER*8     CHOPTI
//    LOGICAL         LEEXAC, LELENG, LEONLY, LEPLAN, LEPOIN, LEVOLU
//    REAL            ERPLI, ERPLO, ERLENG
//    INTEGER         NAMEER, NUMVER, IOVLER
//    COMMON /EROPTS/ ERPLI(3,2), ERPLO(3,4,MXPRED), ERLENG(MXPRED),
//   +                NAMEER(MXPRED), NUMVER(MXPRED), IOVLER(MXPRED),
//   +                LEEXAC, LELENG, LEONLY, LEPLAN, LEPOIN, LEVOLU
//    COMMON /EROPTC/CHOPTI

typedef struct {
  Float_t   erpli[3*2];
  Float_t   erplo[MXPRED*3*4];
  Float_t   erleng[MXPRED];
  Int_t     nameer[MXPRED];
  Int_t     numver[MXPRED];
  Int_t     iovler[MXPRED];
  Bool_t    leexac;
  Bool_t    leleng;
  Bool_t    leonly;
  Bool_t    leplan;
  Bool_t    lepoin;
  Bool_t    levolu;
  Bool_t    levmix;
} Eropts_t;

typedef struct {
  char chopti[8];
} Eroptc_t;

//-------ERWORK
//    DOUBLE PRECISION EI, EF, ASDSC
//    COMMON /ERWORK/ EI(15), EF(15), ASDSC(5,5),
//   +                   XI(3), PPI(3), HI(9),
//   +                   XF(3), PF(3),  HF(9),
//   +                   CHTR, DEDX2, BACKTR, CUTEK, TLGCM2, TLRAD

typedef struct {
  Double_t  ei[15];
  Double_t  ef[15];
  Double_t  asdsc[5*5];
  Float_t   xi[3];
  Float_t   ppi[3];
  Float_t   hi[9];
  Float_t   xf[3];
  Float_t   pf[3];
  Float_t   hf[9];
  Float_t   chtr;
  Float_t   dedx2;
  Float_t   backtr;
  Float_t   cutek;
  Float_t   tlgcm2;
  Float_t   tlrad;
} Erwork_t;


#endif
