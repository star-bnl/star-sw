#ifndef TGeant3_H 
#define TGeant3_H 
//////////////////////////////////////////////// 
//  C++ interface to Geant3 basic routines    // 
//////////////////////////////////////////////// 
 
#include "TNamed.h" 
#include "St_Node.h"
  
class TGeant3 : public TNamed { 

private:
  Int_t fNextVol;    // Iterator for GeomIter
  
public: 
   TGeant3(); 
   TGeant3(const char *name, const char *title, Int_t nwgeant=2000000, Int_t nwpaw=0, Int_t iwtype=0); 
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

//////////////////////////////////////////////////////////
//                                                      //
//     Here are the interface functions with GEANT3.21  //
//                                                      //
//                                                      //
//////////////////////////////////////////////////////////

 
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
   virtual  Int_t Glvolu(const Int_t Nlev, Int_t *Lnam, Int_t *Lnum);  
   ClassDef(TGeant3,1)  //C++ interface to Geant basic routines 
}; 
 
EXTERN TGeant3 *geant; 
 
//----------GCBANK
//      PARAMETER (KWBANK=69000,KWWORK=5200)
//      COMMON/GCBANK/NZEBRA,GVERSN,ZVERSN,IXSTOR,IXDIV,IXCONS,FENDQ(16)
//     +             ,LMAIN,LR1,WS(KWBANK)
//      DIMENSION IQ(2),Q(2),LQ(8000),IWS(2)
//      EQUIVALENCE (Q(1),IQ(1),LQ(9)),(LQ(1),LMAIN),(IWS(1),WS(1))  
typedef struct {
  Int_t   nzebra;



  Float_t gversion,zversn;
  Int_t   ixstor,ixdiv,ixcons;
  Float_t fendq[15];
  Int_t   lq[8],iq[1];
} common_gcbank;
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
#ifndef __CINT__
#if never
EXTERN common_gcbank *cbank;
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
//--------------Declarations for ZEBRA--------------------- 
EXTERN Int_t *z_iq, *z_lq; 
EXTERN Float_t *z_q; 
#endif
#ifndef WIN32 
# define hlimit  hlimit_ 
# define gzebra  gzebra_ 
# define grfile  grfile_ 
# define gpcxyz  gpcxyz_ 
# define ggclos  ggclos_ 
# define glast   glast_ 
# define ginit   ginit_ 
# define grun    grun_ 
# define gtrig   gtrig_ 
# define gtrigc  gtrigc_ 
# define gtrigi  gtrigi_ 
# define gwork   gwork_ 
# define gzinit  gzinit_ 
# define gfmate  gfmate_ 
# define gfpart  gfpart_ 
# define gftmed  gftmed_ 
# define gmate   gmate_ 
# define gpart   gpart_ 
# define gsdk    gsdk_ 
# define gsmate  gsmate_ 
# define gsmixt  gsmixt_ 
# define gspart  gspart_ 
# define gstmed  gstmed_ 
# define gstpar  gstpar_ 
# define gfkine  gfkine_ 
# define gfvert  gfvert_ 
# define gskine  gskine_ 
# define gsvert  gsvert_ 
# define gphysi  gphysi_ 
# define gdebug  gdebug_ 
# define gekbin  gekbin_ 
# define gfinds  gfinds_ 
# define gsking  gsking_ 
# define gsstak  gsstak_ 
# define gsxyz   gsxyz_ 
# define gtrack  gtrack_ 
# define gtreve  gtreve_ 
# define gdtom   gdtom_ 
# define glmoth  glmoth_ 
# define gmedia  gmedia_ 
# define gmtod   gmtod_ 
# define gsdvn   gsdvn_ 
# define gsdvn2  gsdvn2_ 
# define gsdvs   gsdvs_ 
# define gsdvs2  gsdvs2_ 
# define gsdvt   gsdvt_ 
# define gsord   gsord_ 
# define gspos   gspos_ 
# define gsposp  gsposp_ 
# define gsrotm  gsrotm_ 
# define gprotm  gprotm_ 
# define gsvolu  gsvolu_ 
# define glvolu  glvolu_ 
# define gprint  gprint_ 
# define dzshow  dzshow_ 
# define agmain  agmain_ 
# define agxuser agxuser_
# define agxinit agxinit_
# define geometry geometry_
# define agvolume agvolume_
# define agstroot agstroot_
# define kuexel  kuexel_
# define set_kupatl set_kupatl_
# define dzddiv  dzddiv_
# define gfrotm  gfrotm_
# define gfxzrm  gfxzrm_

#ifndef Geant3Dummy
# define type_of_call 
# define DEFCHARD     const char* 
# define DEFCHARL   , const int 
# define PASSCHARD(string) string 
# define PASSCHARL(string) , strlen(string) 
#endif /* not Geant3Dummy */
#else 
# define hlimit  HLIMIT 
# define gzebra  GZEBRA 
# define grfile  GRFILE 
# define gpcxyz  GPCXYZ 
# define ggclos  GGCLOS 
# define glast   GLAST 
# define ginit   GINIT 
# define grun    GRUN 
# define gtrig   GTRIG 
# define gtrigc  GTRIGC 
# define gtrigi  GTRIGI 
# define gwork   GWORK 
# define gzinit  GZINIT 
# define gfmate  GFMATE 
# define gfpart  GFPART 
# define gftmed  GFTMED 
# define gmate   GMATE 
# define gpart   GPART 
# define gsdk    GSDK 
# define gsmate  GSMATE 
# define gsmixt  GSMIXT 
# define gspart  GSPART 
# define gstmed  GSTMED 
# define gstpar  GSTPAR 
# define gfkine  GFKINE 
# define gfvert  GFVERT 
# define gskine  GSKINE 
# define gsvert  GSVERT 
# define gphysi  GPHYSI 
# define gdebug  GDEBUG 
# define gekbin  GEKBIN 
# define gfinds  GFINDS 
# define gsking  GSKING 
# define gsstak  GSSTAK 
# define gsxyz   GSXYZ 
# define gtrack  GTRACK 
# define gtreve  GTREVE 
# define gdtom   GDTOM 
# define glmoth  GLMOTH 
# define gmedia  GMEDIA 
# define gmtod   GMTOD 
# define gsdvn   GSDVN 
# define gsdvn2  GSDVN2 
# define gsdvs   GSDVS 
# define gsdvs2  GSDVS2 
# define gsdvt   GSDVT 
# define gsord   GSORD 
# define gspos   GSPOS 
# define gsposp  GSPOSP 
#ifndef Geant3Dummy
# define gsrotm  GSROTM 
#endif /* not Geant3Dummy */
# define gprotm  GPROTM 
# define gsvolu  GSVOLU 
# define glvolu  GLVOLU 
# define gprint  GPRINT 
# define dzshow  DZSHOW 
# define agmain  AGMAIN
# define agxuser AGXUSER
# define agxinit AGXINIT
# define geometry GEOMETRY
# define agvolume AGVOLUME
# define agstroot AGSTROOT
# define kuexel  KUEXEL
# define set_kupatl SET_KUPATL
# define dzddiv  DZDDIV
# define gfrotm  GFROTM
# define gfxzrm  GFXZRM


#ifndef Geant3Dummy
 
# define type_of_call  _stdcall 
# define DEFCHARD   const char* , const int        
# define DEFCHARL          
# define PASSCHARD(string) string, strlen(string) 
# define PASSCHARL(string) 
#endif /* not Geant3Dummy */
#endif 
 
#ifndef Geant3Dummy
extern "C" void type_of_call hlimit(const int&); 
extern "C" void type_of_call gzebra(const int&); 
extern "C" void type_of_call gpcxyz(); 
extern "C" void type_of_call ggclos(); 
extern "C" void type_of_call glast(); 
extern "C" void type_of_call ginit(); 
extern "C" void type_of_call grun(); 
extern "C" void type_of_call gtrig(); 
extern "C" void type_of_call gtrigc(); 
extern "C" void type_of_call gtrigi(); 
extern "C" void type_of_call gwork(const int&); 
extern "C" void type_of_call gzinit(); 
extern "C" void type_of_call gmate(); 
extern "C" void type_of_call gpart(); 
extern "C" void type_of_call gsdk(Int_t &, Float_t *, Int_t *); 
extern "C" void type_of_call gfkine(Int_t &, Float_t *, Float_t *, Int_t &, Int_t &, Float_t *, Int_t &); 
extern "C" void type_of_call gfvert(Int_t &, Float_t *, Int_t &, Int_t &, Float_t &, Float_t *, Int_t &); 
extern "C" void type_of_call gskine(Float_t *,Int_t &, Int_t &, Float_t *, Int_t &, Int_t &); 
extern "C" void type_of_call gsvert(Float_t *,Int_t &, Int_t &, Float_t *, Int_t &, Int_t &); 
extern "C" void type_of_call gphysi(); 
extern "C" void type_of_call gdebug(); 
extern "C" void type_of_call gekbin(); 
extern "C" void type_of_call gfinds(); 
extern "C" void type_of_call gsking(Int_t &); 
extern "C" void type_of_call gsstak(Int_t &); 
extern "C" void type_of_call gsxyz(); 
extern "C" void type_of_call gtrack(); 
extern "C" void type_of_call gtreve(); 
extern "C" void type_of_call gdtom(Float_t *, Float_t *, Int_t &); 
extern "C" void type_of_call glmoth(Int_t &, Int_t &, Int_t &, Int_t *, Int_t *, Int_t *); 
extern "C" void type_of_call gmedia(Float_t *, Int_t &); 
extern "C" void type_of_call gmtod(Float_t *, Float_t *, Int_t &); 
extern "C" void type_of_call gsrotm(const Int_t &, const Float_t &, const Float_t &, const Float_t &, const Float_t &, const Float_t &, const Float_t &); 
extern "C" void type_of_call gprotm(const Int_t &); 
extern "C" void type_of_call grfile(const Int_t&, DEFCHARD, DEFCHARD DEFCHARL DEFCHARL); 
extern "C" void type_of_call gfmate(const Int_t&, DEFCHARD, Float_t &, Float_t &, Float_t &, Float_t &, Float_t &, Float_t *, Int_t & DEFCHARL); 
extern "C" void type_of_call gfpart(const Int_t&, DEFCHARD, Int_t &, Float_t &, Float_t &, Float_t &, Float_t *, Int_t & DEFCHARL); 
extern "C" void type_of_call gftmed(const Int_t&, DEFCHARD, Int_t &, Int_t &, Int_t &, Float_t &, Float_t &, Float_t &, Float_t &, Float_t &, Float_t &, Float_t *, Int_t & DEFCHARL); 
extern "C" void type_of_call gsmate(const Int_t&, DEFCHARD, Float_t &, Float_t &, Float_t &, Float_t &, Float_t &, Float_t *, Int_t & DEFCHARL); 
extern "C" void type_of_call gsmixt(const Int_t&, DEFCHARD, Float_t &, Float_t &, Float_t &, Int_t &, Float_t * DEFCHARL); 
extern "C" void type_of_call gspart(const Int_t&, DEFCHARD, Int_t &, Float_t &, Float_t &, Float_t &, Float_t *, Int_t & DEFCHARL); 
extern "C" void type_of_call gstmed(const Int_t&, DEFCHARD, Int_t &, Int_t &, Int_t &, Float_t &, Float_t &, Float_t &, Float_t &, Float_t &, Float_t &, Float_t *, Int_t & DEFCHARL); 
extern "C" void type_of_call gstpar(const Int_t&, DEFCHARD, Float_t & DEFCHARL); 
extern "C" void type_of_call gsdvn(DEFCHARD,DEFCHARD, Int_t &, Int_t & DEFCHARL DEFCHARL); 
extern "C" void type_of_call gsdvn2(DEFCHARD,DEFCHARD, Int_t &, Int_t &, Float_t &, Int_t & DEFCHARL DEFCHARL); 
extern "C" void type_of_call gsdvs(DEFCHARD,DEFCHARD, Float_t &, Int_t &, Int_t & DEFCHARL DEFCHARL); 
extern "C" void type_of_call gsdvs2(DEFCHARD,DEFCHARD, Float_t &, Int_t &, Float_t &, Int_t & DEFCHARL DEFCHARL); 
extern "C" void type_of_call gsdvt(DEFCHARD,DEFCHARD, Float_t &, Int_t &, Int_t &, Int_t & DEFCHARL DEFCHARL); 
extern "C" void type_of_call gsord(DEFCHARD, Int_t & DEFCHARL);
extern "C" void type_of_call gspos(DEFCHARD, Int_t &, DEFCHARD, Float_t &, Float_t &, Float_t &, Int_t &, DEFCHARD DEFCHARL DEFCHARL DEFCHARL); 
extern "C" void type_of_call gsposp(DEFCHARD, Int_t &, DEFCHARD, Float_t &, Float_t &, Float_t &, Int_t &, DEFCHARD,  
				    Float_t *, Int_t & DEFCHARL DEFCHARL DEFCHARL); 
extern "C" void type_of_call gsvolu(DEFCHARD, DEFCHARD, Int_t &, Float_t *, Int_t &, Int_t & DEFCHARL DEFCHARL); 
extern "C" void type_of_call glvolu(Int_t *, Int_t *, Int_t *, Int_t *);
extern "C" void type_of_call gprint(DEFCHARD,const int& DEFCHARL); 
extern "C" void type_of_call dzshow(DEFCHARD,const int&,const int&,DEFCHARD,const int&, const int&, const int&, const int& DEFCHARL DEFCHARL);
extern "C" void type_of_call agmain(Int_t*,Int_t*,Int_t*);
extern "C" void type_of_call agxuser();
extern "C" void type_of_call agxinit();
extern "C" void type_of_call geometry();
extern "C" Int_t  type_of_call agvolume(St_Node**,Float_t**,Float_t**,Float_t**);
extern "C" Int_t  type_of_call agstroot();
extern "C" void type_of_call kuexel   (const Char_t*,Int_t);
extern "C" void type_of_call set_kupatl(const Char_t*,Int_t*,Int_t);
extern "C" void type_of_call dzddiv   (Int_t*,Int_t*,Char_t*,Char_t*,Int_t*,Int_t*,Int_t*,Int_t*,Int_t,Int_t);
extern "C" void type_of_call gfrotm   (Int_t*,Float_t*,Float_t*,Float_t*,Float_t*,Float_t*,Float_t*);
extern "C" void type_of_call gfxzrm   (Int_t*,Float_t*,Float_t*,Float_t*,Float_t*,Float_t*,Float_t*,Float_t*,Float_t*,Float_t*,Float_t*);
 
#else  /* Geant3Dummy */
#endif
 // Geant3 common blocks mapped to C structures 
#ifndef WIN32 
//#  define pawc   pawc_
#  define gcbank gcbank_
#  define quest  quest_ 
#  define gclink gclink_ 
#  define gccuts gccuts_ 
#  define gcflag gcflag_ 
#  define gckine gckine_ 
#  define gcking gcking_ 
#  define gcmate gcmate_ 
#  define gctmed gctmed_ 
#  define gctrak gctrak_ 
#  define gctpol gctpol_ 
#  define gcvolu gcvolu_ 
#  define gcstak gcstak_ 
#  define gcnum  gcnum_ 
#  define gcsets gcsets_ 
#else 
//#  define pawc   PAWC
#  define gcbank GCBANK
#  define quest  QUEST 
#  define gclink GCLINK 
#  define gccuts GCCUTS 
#  define gcflag GCFLAG 
#  define gckine GCKINE 
#  define gcking GCKING 
#  define gcmate GCMATE 
#  define gctmed GCTMED 
#  define gctrak GCTRAK 
#  define gctpol GCTPOL 
#  define gcvolu GCVOLU 
#  define gcstak GCSTAK 
#  define gcnum  GCNUM 
#  define gcsets GCSETS 
#endif 
 
#ifndef Geant3Dummy
extern "C" common_gcbank gcbank;
extern "C" common_quest  quest; 
extern "C" common_gclink gclink; 
extern "C" common_gccuts gccuts; 
extern "C" common_gcflag gcflag; 
extern "C" common_gckine gckine; 
extern "C" common_gcking gcking; 
extern "C" common_gcmate gcmate; 
extern "C" common_gctmed gctmed; 
extern "C" common_gctrak gctrak; 
extern "C" common_gctpol gctpol; 
extern "C" common_gcvolu gcvolu; 
extern "C" common_gcnum  gcnum; 
extern "C" common_gcsets gcsets; 
#else /* Geant3Dummy */
Int_t  gcbank;
Int_t  quest;
Int_t  gclink;
Int_t  gccuts;
Int_t  gcflag;
Int_t  gckine;
Int_t  gcking;
Int_t  gcmate;
Int_t  gctmed;
Int_t  gctrak;
Int_t  gctpol;
Int_t  gcvolu;
Int_t  gcnum ;
Int_t  gcsets;
#endif /* Geant3Dummy */
//--------------Declarations for ZEBRA--------------------- 
 
#endif /* __CINT__ */ 
#endif 
