#ifndef TGeant3_H 
#define TGeant3_H 
//////////////////////////////////////////////// 
//  C++ interface to Geant3 basic routines    // 
//////////////////////////////////////////////// 
 
#include "TNamed.h" 
  
class TGeant3 : public TNamed { 

private:
  Int_t fNextVol;    // Iterator for GeomIter
  
public: 
   TGeant3(); 
   TGeant3(const char *name, const char *title, Int_t *gcbank, Int_t nwgeant=3000000, Int_t nwpaw=100000); 
   TGeant3(Int_t *gcbank, const char *name, const char *title); 
   virtual ~TGeant3(); 
   virtual void LoadAddress(); 
 
   virtual Int_t  GetSetNumber(const char *name); 

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//                                                                                                               //
//                                                                                                               //
//     Here are the service routines from the geometry which could be implemented also in other geometries       //
//                                                                                                               //
//                                                                                                               //
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  void  GeomIter();
  Int_t NextVolUp(Text_t *name, Int_t &copy);
  Int_t CurrentVol(Text_t *name, Int_t &copy) const;
  Int_t CurrentVolOff(Int_t off, Text_t *name, Int_t &copy) const;
  Int_t VolId(Text_t *name) const;
  void  TrackPosition(Float_t *xyz) const;
  Float_t TrackCharge() const;
  Bool_t TrackInside();
  Bool_t TrackEntering();
  Bool_t TrackExiting();
  Bool_t TrackOut();

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//                                                                                                               //
//                                                                                                               //
//     Here are the interface functions with GEANT3.21                                                           //
//                                                                                                               //
//                                                                                                               //
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

 
      // functions from GBASE 
   virtual  void  Gpcxyz(); 
   virtual  void  Ggclos(); 
   virtual  void  Gfile(const char *filename, const char *option="I"); 
   virtual  void  Glast(); 
   virtual  void  Gprint(const char *name); 
   virtual  void  Grun(); 
   virtual  void  Gtrig(); 
   virtual  void  Gtrigc(); 
   virtual  void  Gtrigi(); 
   virtual  void  Gwork(Int_t nwork); 
   virtual  void  Gzinit(); 
 
      // functions from GCONS 
   virtual  void  Gfmate(Int_t imat, char *name, Float_t &a, Float_t &z, Float_t &dens, 
                         Float_t &radl, Float_t &absl); 
   virtual  void  Gfpart(Int_t ipart, char *name, Int_t &itrtyp,  
                         Float_t &amass, Float_t &charge, Float_t &tlife); 
   virtual  void  Gftmed(Int_t numed, char *name, Int_t &nmat, Int_t &isvol,  
                         Int_t &ifield, Float_t &fieldm, Float_t &tmaxfd, 
                         Float_t &stemax, Float_t &deemax, Float_t &epsil, 
                         Float_t &stmin); 
   virtual  void  Gmate(); 
   virtual  void  Gpart(); 
   virtual  void  Gsdk(Int_t ipart, Float_t *bratio, Int_t *mode); 
   virtual  void  Gsmate(Int_t imat, const char *name, Float_t a, Float_t z,  
                         Float_t dens, Float_t radl, Float_t absl); 
   virtual  void  Gsmixt(Int_t imat, const char *name, Float_t a, Float_t z,  
                         Float_t dens, Int_t nlmat, Float_t *wmat); 
   virtual  void  Gspart(Int_t ipart, const char *name, Int_t itrtyp,  
                         Float_t amass, Float_t charge, Float_t tlife); 
   virtual  void  Gstmed(Int_t numed, const char *name, Int_t nmat, Int_t isvol,  
                         Int_t ifield, Float_t fieldm, Float_t tmaxfd, 
                         Float_t stemax, Float_t deemax, Float_t epsil, 
                         Float_t stmin); 
   virtual  void  Gstpar(Int_t itmed, const char *param, Float_t parval); 
 
      // functions from GKINE 
   virtual  void  Gfkine(Int_t itra, Float_t *vert, Float_t *pvert, 
                         Int_t &ipart, Int_t &nvert); 
   virtual  void  Gfvert(Int_t nvtx, Float_t *v, Int_t &ntbeam, Int_t &nttarg, Float_t &tofg); 
   virtual  Int_t Gskine(Float_t *plab, Int_t ipart, Int_t nv, Float_t *ubuf=0, Int_t nwbuf=0); 
   virtual  Int_t Gsvert(Float_t *v, Int_t ntbeam, Int_t nttarg, Float_t *ubuf=0, Int_t nwbuf=0); 
 
      // functions from GPHYS 
   virtual  void  Gphysi(); 
 
      // functions from GTRAK 
   virtual  void  Gdebug(); 
   virtual  void  Gekbin(); 
   virtual  void  Gfinds(); 
   virtual  void  Gsking(Int_t igk); 
   virtual  void  Gsstak(Int_t iflag); 
   virtual  void  Gsxyz(); 
   virtual  void  Gtrack(); 
   virtual  void  Gtreve(); 
 
      // functions from GGEOM 
   virtual  void  Gdtom(Float_t *xd, Float_t *xm, Int_t iflag); 
   virtual  void  Glmoth(Int_t iudet, Int_t iunum, Int_t &nlev, 
                         Int_t *lvols, Int_t *lindx); 
   virtual  void  Gmedia(Float_t *x, Int_t &numed); 
   virtual  void  Gmtod(Float_t *xm, Float_t *xd, Int_t iflag); 
   virtual  void  Gsdvn(const char *name, const char *mother, Int_t ndiv, Int_t iaxis); 
   virtual  void  Gsdvn2(const char *name, const char *mother, Int_t ndiv, Int_t iaxis, Float_t c0i, Int_t numed); 
   virtual  void  Gsdvs(const char *name, const char *mother, Float_t step, Int_t iaxis, Int_t numed); 
   virtual  void  Gsdvs2(const char *name, const char *mother, Float_t step, Int_t iaxis, Float_t c0, Int_t numed); 
   virtual  void  Gsdvt(const char *name, const char *mother, Float_t step, Int_t iaxis, Int_t numed, Int_t ndvmx); 
   virtual  void  Gsord(const char *name, Int_t iax); 
   virtual  void  Gspos(const char *name, Int_t nr, const char *mother,  
                         Float_t x, Float_t y, Float_t z, Int_t irot, const char *konly="ONLY"); 
   virtual  void  Gsposp(const char *name, Int_t nr, const char *mother,  
                         Float_t x, Float_t y, Float_t z, Int_t irot, const char *konly, Float_t *upar, Int_t np); 
   virtual  void  Gsrotm(Int_t nmat, Float_t theta1, Float_t phi1, Float_t theta2, Float_t phi2, 
                         Float_t theta3, Float_t phi3); 
   virtual  void  Gprotm(Int_t nmat=0); 
   virtual  Int_t Gsvolu(const char *name, const char *shape, Int_t nmed,  
                         Float_t *upar, Int_t np); 
 
   ClassDef(TGeant3,1)  //C++ interface to Geant basic routines 
}; 
 
EXTERN TGeant3 *geant; 
 
//--------------Declarations for ZEBRA--------------------- 
EXTERN Int_t *z_iq, *z_lq; 
EXTERN Float_t *z_q; 
 
//----------QUEST 
//      COMMON/QUEST/IQUEST(100) 
typedef struct { 
  Int_t    iquest[100]; 
} common_quest; 
 
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
} common_gclink; 
 
 
//----------GCCUTS 
//      COMMON/GCCUTS/CUTGAM,CUTELE,CUTNEU,CUTHAD,CUTMUO,BCUTE,BCUTM 
//     +             ,DCUTE ,DCUTM ,PPCUTM,TOFMAX,GCUTS(5) 
typedef struct { 
  Float_t  cutgam; 
  Float_t  cutele; 
  Float_t  cutneu; 
  Float_t  cuthad; 
  Float_t  cutmuo; 
  Float_t  bcute; 
  Float_t  bcutm; 
  Float_t  dcute; 
  Float_t  dcutm; 
  Float_t  ppcutm; 
  Float_t  tofmax; 
  Float_t  gcuts[5]; 
} common_gcuts; 
 
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
} common_gcflag; 
 
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
} common_gckine; 
 
//----------GCKING 
//      COMMON/GCKING/KCASE,NGKINE,GKIN(5,MXGKIN), 
//     +                           TOFD(MXGKIN),IFLGK(MXGKIN) 
#define MXGKIN 100 
typedef struct  { 
  Int_t    kcase; 
  Int_t    ngkine; 
  Int_t    gkin[MXGKIN][5]; 
  Int_t    tofd[MXGKIN]; 
  Int_t    iflgk[MXGKIN]; 
} common_gcking; 
 
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
} common_gcmate; 
 
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
} common_gctmed; 
 
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
} common_gctrak; 
 
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
} common_gcvolu; 
 
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
} common_gcsets; 
 
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
} common_gcnum; 
 
//----------GCCUTS 
//  COMMON/GCCUTS/CUTGAM,CUTELE,CUTNEU,CUTHAD,CUTMUO,BCUTE,BCUTM 
//   +             ,DCUTE ,DCUTM ,PPCUTM,TOFMAX,GCUTS(5) 
typedef struct { 
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
} common_gccuts; 
 
//----------GCTPOL 
#define MAXME1 30 
//      COMMON/GCTPOL/POLAR(3), NAMEC1(MAXME1) 
typedef struct { 
  Float_t polar[3]; 
  Int_t   namec1[MAXME1]; 
} common_gctpol; 
 
 
EXTERN common_quest  *cquest; 
EXTERN common_gclink *clink; 
EXTERN common_gccuts *ccuts; 
EXTERN common_gcflag *cflag; 
EXTERN common_gckine *ckine; 
EXTERN common_gcking *cking; 
EXTERN common_gcmate *cmate; 
EXTERN common_gctmed *ctmed; 
EXTERN common_gctrak *ctrak; 
EXTERN common_gctpol *ctpol; 
EXTERN common_gcvolu *cvolu; 
EXTERN common_gcnum  *cnum; 
EXTERN common_gcsets *csets; 
 
#endif 
