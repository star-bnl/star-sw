#include "G3Bridge.h"

 G3Bridge gBrd;
//_____________________________________________________________________________
 
extern "C" void  gmedia_ (float *x, int *numed, int *check);
extern "C" void  g3media_(float *x, int *numed, int *check);
extern "C" void  gudcay_ ();
extern "C" void  gudigi_ ();
extern "C" float gudtim_ (void* DETREP,void* HITREP,void* IW1,void* DIS );
extern "C" void  gufld_  (float* X,float *F );
extern "C" void  guhadr_ ();
extern "C" void  guiget_ (int *NMENU,int *NCOMD,int *NPAR);
extern "C" void  guinme_ (float *X,int *SH,float *P,int *IYES);
extern "C" void  guinti_ ();
extern "C" void  gukine_ ();
extern "C" void  gunear_ (int *ISEARC, int *ICALL, float *X, int *JNEAR);
extern "C" void  guout_ ();
extern "C" void  gupara_ ();
extern "C" void  guphad_ ();
extern "C" float guplsh_ (int *MED0,int *MED1);
extern "C" void  guskip_ (int *ISKIP);
extern "C" void  gustep_ ();
extern "C" void  guswim_ (int *CHARGE,float *STEP,float *VECT,float *VOUT);
extern "C" void  gutrak_ ();
extern "C" void  gutrev_ ();
extern "C" void  guview_ (void *a1,void *a2,void *a3,void *a4,void *a5);
extern "C" void  guxcs_  ();
extern "C" void  grndm_  (float *r, int *n);
extern "C" void  grndmq_ (int *is1, int *is2, int *is3, void *c1, void *c2);
extern "C" void  rxgtrak_ (int *mtrack, int *ipart, float *pmom, float *e, float *vpos, float *polar, float *tof); 
extern "C" void  rxouth_ ();
extern "C" void  rxinh_  ();
extern "C" void  ginvol_ (float*, int*);
extern "C" void  g3invol_(float*, int*);
extern "C" void  gtmedi_ (float *x, int *n);
extern "C" void  g3tmedi_(float *x, int *n);
extern "C" void  gtmany_ (int *lev);
extern "C" void  g3tmany_(int *lev);
extern "C" void  gtonly_ (int *isonly);
extern "C" void  glvolu_ (int *nlev, int *lnam,int *lnum, int *ier);
extern "C" void  g3lvolu(int *nlev, int *lnam,int *lnum, int *ier);
extern "C" void  gtnext_ ();
extern "C" void  g3tnext_();
extern "C" void  ggperp_ (float*, float*, int*);
extern "C" void  g3gperp_(float*, float*, int*);

//_____________________________________________________________________________
void gudcay_()
{  if (gBrd.m_gudcay) (*(Sub0)gBrd.m_gudcay)(); }


//_____________________________________________________________________________
void gudigi_()
{  if (gBrd.m_gudigi) (*(Sub0)gBrd.m_gudigi)(); }

//_____________________________________________________________________________
float gudtim_(void* DETREP,void* HITREP,void* IW1,void* DIS )
{  
  if (!gBrd.m_gudtim) return 0;
  return (*(Fun4)gBrd.m_gudtim)(DETREP,HITREP,IW1,DIS );
}

//_____________________________________________________________________________
void gufld_(float* X,float *F )
{  
  if (gBrd.m_gufld) {(*(Sub2)gBrd.m_gufld)(X,F);return;}
  F[0]=0; F[1]=0; F[2]=0;
}

//_____________________________________________________________________________
extern "C" void gheish_();
void guhadr_()
{  
  if (gBrd.m_guhadr) {(*(Sub0)gBrd.m_guhadr)();return;}
  gheish_(); 
}

//_____________________________________________________________________________
void guiget_(int *NMENU,int *NCOMD,int *NPAR)
{  
  if (gBrd.m_guiget) (*(Sub3)gBrd.m_guiget)(NMENU,NCOMD,NPAR);
}

//_____________________________________________________________________________
void guinme_(float *X,int *SH,float *P,int *IYES)
{  
  *IYES=-1;
  if (gBrd.m_guinme) (*(Sub4)gBrd.m_guinme)(X,SH,P,IYES);
}
//_____________________________________________________________________________
void guinti_()
{  
  if (gBrd.m_guinti) (*(Sub0)gBrd.m_guinti)();
}

//_____________________________________________________________________________
void gukine_()
{  
  if (gBrd.m_gukine) (*(Sub0)gBrd.m_gukine)();
}

//_____________________________________________________________________________
void gunear_(int *ISEARC, int *ICALL, float *X, int *JNEAR)
{  
  if (gBrd.m_gunear) (*(Sub4)gBrd.m_gunear)(ISEARC, ICALL, X, JNEAR);
}

//_____________________________________________________________________________
void guout_()
{  
  if (gBrd.m_guout) (*(Sub0)gBrd.m_guout)();
}
//_____________________________________________________________________________
void gupara_()
{  
  if (gBrd.m_gupara) (*(Sub0)gBrd.m_gupara)();
}

//_____________________________________________________________________________
extern "C" void gpghei_();
void guphad_()
{  
  if (gBrd.m_guphad) {(*(Sub0)gBrd.m_guphad)();return;}
  gpghei_();
}
//_____________________________________________________________________________
float guplsh_(int *MED0,int *MED1)
{  
  if (gBrd.m_guplsh) return (*(Fun2)gBrd.m_guplsh)(MED0,MED1);
  return 1;
}
// 
//_____________________________________________________________________________
void guskip_(int *ISKIP)
{  
  *ISKIP = 0;
  if (gBrd.m_guskip) (*(Sub1)gBrd.m_guskip)(ISKIP);
}

//_____________________________________________________________________________
void gustep_()
{  
  if (gBrd.m_gustep) (*(Sub0)gBrd.m_gustep)();
}

//_____________________________________________________________________________
extern "C" void g3swim_(int *CHARGE,float *STEP,float *VECT,float *VOUT);
void guswim_(int *CHARGE,float *STEP,float *VECT,float *VOUT)
{  
  if (gBrd.m_guswim) {(*(Sub4)gBrd.m_guswim)(CHARGE,STEP,VECT,VOUT);return;}
  g3swim_(CHARGE,STEP,VECT,VOUT);
}

//_____________________________________________________________________________
extern "C" void g3track_();
void gutrak_()
{  
  if (gBrd.m_gutrak) {(*(Sub0)gBrd.m_gutrak)();return;}
  g3track_();
}
//_____________________________________________________________________________
extern "C" void g3treve_();
void gutrev_()
{  
  if (gBrd.m_gutrev) {(*(Sub0)gBrd.m_gutrev)();return;}
}

//_____________________________________________________________________________
void guview_(void *a1,void *a2,void *a3,void *a4,void *a5)
//  5 arguments due to additional argument len of char string somewhere
{  
  if (gBrd.m_guview) {(*(Sub5)gBrd.m_guview)(a1,a2,a3,a4,a5);return;}
  gustep_();
}
//_____________________________________________________________________________
void guxcs_(){}

//_____________________________________________________________________________
void grndm_(float *r, int *n)
{(*(Sub2)gBrd.m_grndm)(r,n);}

//_____________________________________________________________________________
void grndmq_(int *is1, int *is2, int *is3, void *c1, void*c2)
{
  if (!gBrd.m_grndmq) return;
  (*(Sub5)gBrd.m_grndmq)(is1,is2,is3,c1,c2);
}

//_____________________________________________________________________________
void rxgtrak_(int *mtrack, int *ipart, float *pmom, float *e, float *vpos, float *polar, float *tof) 
{(*(Sub7)gBrd.m_rxgtrak)(mtrack, ipart, pmom, e, vpos, polar, tof);}

//_____________________________________________________________________________
void rxouth_()
{(*(Sub0)gBrd.m_rxouth)();}

//_____________________________________________________________________________
void rxinh_()
{(*(Sub0)gBrd.m_rxinh)();}

//_____________________________________________________________________________
void ginvol_(float *x, int *i)
{
  if (gBrd.m_ginvol) {(*(Sub2)gBrd.m_ginvol)(x,i); return;}
  g3invol_(x,i);
}

//_____________________________________________________________________________
void gtmedi_(float *x, int *i)
{
  if (gBrd.m_gtmedi) {(*(Sub2)gBrd.m_gtmedi)(x,i); return;}
  g3tmedi_(x,i);
}

//_____________________________________________________________________________
void gtmany_(int *lev)
{
  if (gBrd.m_gtmany) {(*(Sub1)gBrd.m_gtmany)(lev); return;}
  g3tmany_(lev);
}

//_____________________________________________________________________________
void gtonly_(int *isonly)
{(*(Sub1)gBrd.m_gtonly)(isonly);}

//_____________________________________________________________________________
void glvolu_(int *nlev, int *lnam,int *lnum, int *ier)
{
  if (gBrd.m_glvolu) {(*(Sub4)gBrd.m_glvolu)(nlev,lnam,lnum,ier); return;}
  g3lvolu(nlev,lnam,lnum,ier);
}

//_____________________________________________________________________________
void gtnext_()
{
  if (gBrd.m_gtnext) {(*(Sub0)gBrd.m_gtnext)(); return;}
  g3tnext_();
}


//_____________________________________________________________________________
void ggperp_(float *x, float *u, int *ierr)
{
  if (gBrd.m_ggperp) {(*(Sub3)gBrd.m_ggperp)(x,u,ierr); return;}
  g3gperp_(x,u,ierr);
}

//_____________________________________________________________________________
void gmedia_(float *x, int *numed, int *check)
{
  if (gBrd.m_gmedia) {(*(Sub3)gBrd.m_gmedia)(x,numed,check); return;}
  g3media_(x,numed,check);
}



