#ifndef Geant3Dummy
//////////////////////////////////////////////// 
//  C++ interface to Geant3 basic routines    // 
//////////////////////////////////////////////// 
#else /* Geant3Dummy */
//////////////////////////////////////////////////////
//  C++ dummy interface to Geant3 basic routines    //
//////////////////////////////////////////////////////
#endif /* Geant3Dummy */
 
#include "TGeant3.h" 
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
#if 0
common_gcbank *cbank;
common_quest  *cquest; 
common_gclink *clink; 
common_gccuts *ccuts; 
common_gcflag *cflag; 
common_gckine *ckine; 
common_gcking *cking;
common_gcmate *cmate; 
common_gctmed *ctmed; 
common_gctrak *ctrak; 
common_gctpol *ctpol; 
common_gcvolu *cvolu; 
common_gcnum  *cnum; 
common_gcsets *csets; 
Int_t *z_iq, *z_lq; 
Float_t *z_q; 
#endif 
//--------------------------------------------------------- 
 
TGeant3 *geant = 0; 
 
ClassImp(TGeant3) 
 
//___________________________________________ 
TGeant3::TGeant3() 
{ 
} 
 
//___________________________________________ 
TGeant3::TGeant3(const char *name, const char *title, Int_t nwgeant, Int_t nwpaw, Int_t iwtype) 
       :TNamed(name,title) 
{ 
#ifndef Geant3Dummy
  //   gzebra(nwgeant); 
  //   hlimit(-nwpaw); 
  //   ginit(); 
  printf (" calling agmain \n");
  agmain_(&nwgeant,&nwpaw,&iwtype); 
  //   gzinit(); 
#endif /* not Geant3Dummy */
   geant = this; 
 
   // set some Zebra pointers 
#ifndef Geant3Dummy
   z_lq      = &(gcbank.lq[0]); //&gcbank[21]; 
   z_iq      = &(gcbank.iq[0]); //&gcbank[29];   
   void *qq  = z_iq; 
   z_q       = (float*)qq; 
#else /* Geant3Dummy */
  // z_lq      = &gcbank[21];
  // z_iq      = &gcbank[29];
  // void *qq  = z_iq;
  // z_q       = (float*)qq;
#endif /* Geant3Dummy */
 
   LoadAddress(); 
} 
 
//___________________________________________ 
TGeant3::~TGeant3() 
{ 
} 
void TGeant3::LoadAddress() { 
#ifndef Geant3Dummy
  cbank =&gcbank;
  cquest=&quest; 
  clink=&gclink; 
  ccuts=&gccuts; 
  cflag=&gcflag; 
  ckine=&gckine; 
  cking=&gcking; 
  cmate=&gcmate; 
  ctmed=&gctmed; 
  ctrak=&gctrak; 
  ctpol=&gctpol; 
  cvolu=&gcvolu; 
  cnum=&gcnum; 
  csets=&gcsets; 
} 

void TGeant3::GeomIter()
{
  // Geometry iterator for going UP the three
  fNextVol=cvolu->nlevel;
}
 
Int_t TGeant3::NextVolUp(Text_t *name, Int_t &copy)
{
  // Iterator for GEANT volume tree
  Int_t i, gname;
  fNextVol--;
  if(fNextVol>=0) {
    gname=cvolu->names[fNextVol];
    strncpy(name,(char *) &gname, 4);
    name[4]='\0';
    copy=cvolu->number[fNextVol];
    i=cvolu->lvolum[i];
    if(gname == z_iq[clink->jvolum+i]) return i;
    else printf("GeomTree: Volume %s not found in bank\n",name);
  }
  return 0;
}

Int_t TGeant3::CurrentVol(Text_t *name, Int_t &copy) const
{
  // Returns the current volume minus offset
  Int_t i, gname;
  if( (i=cvolu->nlevel-1) < 0 ) {
    printf("CurrentVol: stack depth %d\n",cvolu->nlevel);
  } else {
    gname=cvolu->names[i];
    if(name) {
      strncpy(name,(char *) &gname, 4);
      name[4]='\0';
    }
    copy=cvolu->number[i];
    i=cvolu->lvolum[i];
    if(gname == z_iq[clink->jvolum+i]) return i;
    else printf("CurrentVol: Volume %s not found in bank\n",name);
  }
  return 0;
}

Int_t TGeant3::CurrentVolOff(Int_t off, Text_t *name, Int_t &copy) const
{
  // Returns the current volume minus offset
  Int_t i, gname;
  if( (i=cvolu->nlevel-off-1) < 0 ) {
    printf("CurrentVolOff: Offset requested %d but stack depth %d\n",off,cvolu->nlevel);
  } else {
    gname=cvolu->names[i];
    if(name) {
      strncpy(name,(char *) &gname, 4);
      name[4]='\0';
    }
    copy=cvolu->number[i];
    i=cvolu->lvolum[i];
    if(gname == z_iq[clink->jvolum+i]) return i;
    else printf("CurrentVolOff: Volume %s not found in bank\n",name);
  }
  return 0;
}

Int_t TGeant3::VolId(Text_t *name) const
{
  Int_t gname, i;
  strncpy((char *) &gname, name, 4);
  for(i=1; i<=cnum->nvolum; i++)
    if(gname == z_iq[clink->jvolum+i]) return i;
  printf("VolId: Volume %s not found\n",name);
  return 0;
}

void TGeant3::TrackPosition(Float_t *xyz) const
{
  xyz[0]=ctrak->vect[0];
  xyz[1]=ctrak->vect[1];
  xyz[2]=ctrak->vect[2];
}

Float_t TGeant3::TrackCharge() const
{
  return ckine->charge;
}

Bool_t TGeant3::TrackInside()
{
  return (ctrak->inwvol==0);
}

Bool_t TGeant3::TrackEntering()
{
  return (ctrak->inwvol==1);
}

Bool_t TGeant3::TrackExiting()
{
  return (ctrak->inwvol==2);
}

Bool_t TGeant3::TrackOut()
{
  return (ctrak->inwvol==3);
#else /* Geant3Dummy */
   cbank  = (common_gcbanl *) &gcbank;
   cquest = (common_quest  *) &quest;
   clink  = (common_gclink *) &gclink;
   ccuts  = (common_gccuts *) &gccuts;
   cflag  = (common_gcflag *) &gcflag;
   ckine  = (common_gckine *) &gckine;
   cking  = (common_gcking *) &gcking;
   cmate  = (common_gcmate *) &gcmate;
   ctmed  = (common_gctmed *) &gctmed;
   ctrak  = (common_gctrak *) &gctrak;
   ctpol  = (common_gctpol *) &gctpol;
   cvolu  = (common_gcvolu *) &gcvolu;
   cnum   = (common_gcnum  *) &gcnum;
   csets  = (common_gcsets *) &gcsets;
#endif /* Geant3Dummy */
}

//=======================functions from GBASE 
 
//___________________________________________ 
#ifndef Geant3Dummy
void  TGeant3::Gfile(const char *filename, const char *option) 
{ 
   grfile(21, PASSCHARD(filename), PASSCHARD(option) PASSCHARL(filename) PASSCHARL(option)); 
} 
 
//___________________________________________ 
Int_t TGeant3::GetSetNumber(const char *name) 
{ 
   Int_t hname[2]; 
   Int_t jset = clink->jset; 
   if (!jset) return 0; 
   Int_t nsets = z_iq[jset-1]; 
   Int_t nch = strlen(name); 
   for (Int_t i=1;i<=nsets;i++) { 
      hname[0] = z_iq[jset+i]; 
      hname[1] = 0; 
      char *iuset = (char*)hname; 
      if (strncmp(iuset,name,nch) == 0) return i; 
   } 
   return 0; 
} 
 
//___________________________________________ 
void  TGeant3::Gpcxyz() 
{ 
   gpcxyz(); 
} 
 
//___________________________________________ 
void  TGeant3::Ggclos() 
{ 
   ggclos(); 
} 
 
//___________________________________________ 
void  TGeant3::Glast() 
{ 
   glast(); 
} 
 
//___________________________________________ 
void  TGeant3::Gprint(const char *name) 
{ 
   gprint(PASSCHARD(name),0 PASSCHARL(name)); 
} 
 
//___________________________________________ 
void  TGeant3::Grun() 
{ 
   grun(); 
} 
 
//___________________________________________ 
void  TGeant3::Gtrig() 
{ 
   gtrig(); 
} 
 
//___________________________________________ 
void  TGeant3::Gtrigc() 
{ 
   gtrigc(); 
} 
 
//___________________________________________ 
void  TGeant3::Gtrigi() 
{ 
   gtrigi(); 
} 
 
//___________________________________________ 
void  TGeant3::Gwork(Int_t nwork) 
{ 
   gwork(nwork); 
} 
 
//___________________________________________ 
void  TGeant3::Gzinit() 
{ 
   gzinit(); 
} 
#else /* Geant3Dummy */
void  TGeant3::Gfile(const char *filename, const char *option) {}
Int_t TGeant3::GetSetNumber(const char *name) {return 0;}
void  TGeant3::GeomIter() {}
Int_t TGeant3::NextVolUp(Text_t *name, Int_t &copy) {return 0;}
Int_t TGeant3::CurrentVol(Text_t *name, Int_t &copy) const {return 0;}
Int_t TGeant3::CurrentVolOff(Int_t off, Text_t *name, Int_t &copy) const {return 0;}
void  TGeant3::TrackPosition(Float_t *xyz) const {}
Int_t TGeant3::VolId(Text_t *name) const {return 0;}
Float_t TGeant3::TrackCharge() const {return 0;}
Bool_t TGeant3::TrackInside() {return 0;}
Bool_t TGeant3::TrackEntering() {return 0;}
Bool_t TGeant3::TrackExiting() {return 0;}
Bool_t TGeant3::TrackOut() {return 0;}
void  TGeant3::Gpcxyz() {}
void  TGeant3::Ggclos() {}
void  TGeant3::Glast() {}
void  TGeant3::Gprint(const char *name) {}
void  TGeant3::Grun() {}
void  TGeant3::Gtrig() {}
void  TGeant3::Gtrigc() {}
void  TGeant3::Gtrigi() {}
void  TGeant3::Gwork(Int_t nwork) {}
void  TGeant3::Gzinit() {}
#endif /* Geant3Dummy */
 
 
//=======================functions from GCONS 
//___________________________________________ 
void  TGeant3::Gfmate(Int_t imat, char *name, Float_t &a, Float_t &z,  
		   Float_t &dens, Float_t &radl, Float_t &absl) 
{ 
#ifndef Geant3Dummy
  Float_t *ubuf=0; 
  Int_t nbuf; 
  gfmate(imat, PASSCHARD(name), a, z, dens, radl, absl, ubuf, nbuf PASSCHARL(name)); 
#endif /* not Geant3Dummy */
} 
 
//___________________________________________ 
void  TGeant3::Gfpart(Int_t ipart, char *name, Int_t &itrtyp,  
		   Float_t &amass, Float_t &charge, Float_t &tlife) 
{ 
#ifndef Geant3Dummy
  Float_t *ubuf=0; 
  Int_t   nbuf; 
  gfpart(ipart, PASSCHARD(name), itrtyp, amass, charge, tlife, ubuf, nbuf PASSCHARL(name)); 
#endif /* not Geant3Dummy */
} 
 
//___________________________________________ 
void  TGeant3::Gftmed(Int_t numed, char *name, Int_t &nmat, Int_t &isvol,  
		   Int_t &ifield, Float_t &fieldm, Float_t &tmaxfd, 
		    Float_t &stemax, Float_t &deemax, Float_t &epsil, 
		    Float_t &stmin) 
{ 
#ifndef Geant3Dummy
  Float_t *ubuf=0; 
  Int_t   nbuf; 
  gftmed(numed, PASSCHARD(name), nmat, isvol, ifield, fieldm, tmaxfd, stemax,  
         deemax, epsil, stmin, ubuf, nbuf PASSCHARL(name)); 
} 
 
//___________________________________________ 
void  TGeant3::Gmate() 
{ 
   gmate(); 
} 
 
//___________________________________________ 
void  TGeant3::Gpart() 
{ 
   gpart(); 
} 
 
//___________________________________________ 
void  TGeant3::Gsdk(Int_t ipart, Float_t *bratio, Int_t *mode) 
{ 
   gsdk(ipart,bratio,mode); 
#endif /* not Geant3Dummy */
} 
 
//___________________________________________ 
#ifdef Geant3Dummy
void  TGeant3::Gmate() {}
void  TGeant3::Gpart() {}
void  TGeant3::Gsdk(Int_t ipart, Float_t *bratio, Int_t *mode) {}
#endif /* Geant3Dummy */
void  TGeant3::Gsmate(Int_t imat, const char *name, Float_t a, Float_t z,  
#ifndef Geant3Dummy
		   Float_t dens, Float_t radl, Float_t absl) 
{ 
   Float_t *ubuf=0; 
   Int_t   nbuf=0; 
   gsmate(imat,PASSCHARD(name), a, z, dens, radl, absl, ubuf, nbuf PASSCHARL(name)); 
} 
 
//___________________________________________ 
#else /* Geant3Dummy */
                   Float_t dens, Float_t radl, Float_t absl) {}
#endif /* Geant3Dummy */
void  TGeant3::Gsmixt(Int_t imat, const char *name, Float_t a, Float_t z,  
#ifndef Geant3Dummy
		   Float_t dens, Int_t nlmat, Float_t *wmat) 
{ 
   gsmixt(imat,PASSCHARD(name), a, z,dens, nlmat,wmat PASSCHARL(name)); 
} 
 
//___________________________________________ 
#else /* Geant3Dummy */
                   Float_t dens, Int_t nlmat, Float_t *wmat) {}
#endif /* Geant3Dummy */
void  TGeant3::Gspart(Int_t ipart, const char *name, Int_t itrtyp,  
#ifndef Geant3Dummy
		   Float_t amass, Float_t charge, Float_t tlife) 
{ 
   Float_t *ubuf=0; 
   Int_t   nbuf=0; 
   gspart(ipart,PASSCHARD(name), itrtyp, amass, charge, tlife, ubuf, nbuf PASSCHARL(name)); 
} 
 
//___________________________________________ 
#else /* Geant3Dummy */
                   Float_t amass, Float_t charge, Float_t tlife) {}
#endif /* Geant3Dummy */
void  TGeant3::Gstmed(Int_t numed, const char *name, Int_t nmat, Int_t isvol,  
		   Int_t ifield, Float_t fieldm, Float_t tmaxfd, 
#ifndef Geant3Dummy
		   Float_t stemax, Float_t deemax, Float_t epsil, Float_t stmin) 
{ 
   Float_t *ubuf=0; 
   Int_t   nbuf=0; 
   gstmed(numed,PASSCHARD(name), nmat, isvol, ifield, fieldm, tmaxfd, stemax, deemax, epsil, stmin, ubuf, nbuf PASSCHARL(name)); 
} 
 
//___________________________________________ 
void  TGeant3::Gstpar(Int_t itmed, const char *param, Float_t parval) 
{ 
   gstpar(itmed,PASSCHARD(param), parval PASSCHARL(param)); 
} 
#else /* Geant3Dummy */
                   Float_t stemax, Float_t deemax, Float_t epsil, Float_t stmin) {}
void  TGeant3::Gstpar(Int_t itmed, const char *param, Float_t parval) {}
#endif /* Geant3Dummy */
 
 
//=======================functions from GKINE 
//___________________________________________ 
#ifndef Geant3Dummy
void  TGeant3::Gfkine(Int_t itra, Float_t *vert, Float_t *pvert, Int_t &ipart, Int_t &nvert) 
{ 
  Float_t *ubuf=0; 
  Int_t   nbuf; 
  gfkine(itra,vert,pvert,ipart,nvert,ubuf,nbuf); 
} 
 
//___________________________________________ 
void  TGeant3::Gfvert(Int_t nvtx, Float_t *v, Int_t &ntbeam, Int_t &nttarg, Float_t &tofg) 
{ 
  Float_t *ubuf=0; 
  Int_t   nbuf; 
  gfvert(nvtx,v,ntbeam,nttarg,tofg,ubuf,nbuf); 
} 
 
//___________________________________________ 
#else /* Geant3Dummy */
void  TGeant3::Gfkine(Int_t itra, Float_t *vert, Float_t *pvert, Int_t &ipart, Int_t &nvert) {}
void  TGeant3::Gfvert(Int_t nvtx, Float_t *v, Int_t &ntbeam, Int_t &nttarg, Float_t &tofg) {}
#endif /* Geant3Dummy */
Int_t TGeant3::Gskine(Float_t *plab, Int_t ipart, Int_t nv, Float_t *buf, Int_t nwbuf) 
{ 
#ifndef Geant3Dummy
   Int_t nt = 0; 
   gskine(plab, ipart, nv, buf, nwbuf, nt); 
   return nt; 
#else /* Geant3Dummy */
   return 0;
#endif /* Geant3Dummy */
} 
 
//___________________________________________ 
Int_t TGeant3::Gsvert(Float_t *v, Int_t ntbeam, Int_t nttarg, Float_t *ubuf, Int_t nwbuf) 
{ 
#ifndef Geant3Dummy
   Int_t nwtx = 0; 
   gsvert(v, ntbeam, nttarg, ubuf, nwbuf, nwtx); 
   return nwtx; 
#else /* Geant3Dummy */
   return 0;
#endif /* Geant3Dummy */
} 
 
//=======================functions from GPHYS 
//___________________________________________ 
#ifndef Geant3Dummy
void  TGeant3::Gphysi() 
{ 
   gphysi(); 
} 
#else /* Geant3Dummy */
void  TGeant3::Gphysi() {}
#endif /* Geant3Dummy */
 
 
//=======================functions from GTRAK 
//___________________________________________ 
#ifndef Geant3Dummy
void  TGeant3::Gdebug() 
{ 
   gdebug(); 
} 
#else /* Geant3Dummy */
void  TGeant3::Gdebug() {}
void  TGeant3::Gekbin() {}
void  TGeant3::Gfinds() {}
void  TGeant3::Gsking(Int_t igk) {}
void  TGeant3::Gsstak(Int_t iflag) {}
void  TGeant3::Gsxyz() {}
void  TGeant3::Gtrack() {}
void  TGeant3::Gtreve() {}
#endif /* Geant3Dummy */
 
#ifdef Geant3Dummy
//=======================functions from GGEOM
#endif /* Geant3Dummy */
//___________________________________________ 
#ifndef Geant3Dummy
void  TGeant3::Gekbin() 
#else /* Geant3Dummy */
void  TGeant3::Gdtom(Float_t *xd, Float_t *xm, Int_t iflag) {}
void  TGeant3::Glmoth(Int_t iudet, Int_t iunum, Int_t &nlev, Int_t *lvols, Int_t *lindx) {}
void  TGeant3::Gmedia(Float_t *x, Int_t &numed) {}
void  TGeant3::Gmtod(Float_t *xm, Float_t *xd, Int_t iflag) {}
void  TGeant3::Gsdvn(const char *name, const char *mother, Int_t ndiv, Int_t iaxis) {}
void  TGeant3::Gsdvn2(const char *name, const char *mother, Int_t ndiv, Int_t iaxis, Float_t c0i, Int_t numed) {}
void  TGeant3::Gsdvs(const char *name, const char *mother, Float_t step, Int_t iaxis, Int_t numed) {}
void  TGeant3::Gsdvs2(const char *name, const char *mother, Float_t step, Int_t iaxis, Float_t c0, Int_t numed) {}
void  TGeant3::Gsdvt(const char *name, const char *mother, Float_t step, Int_t iaxis, Int_t numed, Int_t ndvmx) {}
void  TGeant3::Gsord(const char *name, Int_t iax) {}
void  TGeant3::Gspos(const char *name, Int_t nr, const char *mother, 
                  Float_t x, Float_t y, Float_t z, Int_t irot, const char *konly) {}
void  TGeant3::Gsposp(const char *name, Int_t nr, const char *mother, 
                   Float_t x, Float_t y, Float_t z, Int_t irot, const char *konly, Float_t *upar, Int_t np ) {}
void  TGeant3::Gsrotm(Int_t nmat, Float_t theta1, Float_t phi1, Float_t theta2, Float_t phi2,
                   Float_t theta3, Float_t phi3)  {}
void  TGeant3::Gprotm(Int_t n)  {}
Int_t TGeant3::Gsvolu(const char *name, const char *shape, Int_t nmed, 
                   Float_t *upar, Int_t np)
Int_t TGeant3::Glvolu(const Nlev, Int_t *Lnam, Int_t *Lnum)

#endif /* Geant3Dummy */
{ 
#ifndef Geant3Dummy
   gekbin(); 
#else /* Geant3Dummy */
   return 0;
#endif /* Geant3Dummy */
} 
 
#ifndef Geant3Dummy
//___________________________________________ 
void  TGeant3::Gfinds() 
{ 
   gfinds(); 
} 
//___________________________________________ 
void  TGeant3::Gsking(Int_t igk) 
{ 
   gsking(igk); 
}
//___________________________________________ 
void  TGeant3::Gsstak(Int_t iflag) 
{ 
   gsstak(iflag); 
} 
//___________________________________________ 
void  TGeant3::Gsxyz() 
{ 
   gsxyz(); 
} 
//___________________________________________ 
void  TGeant3::Gtrack() 
{ 
   gtrack(); 
} 
//___________________________________________ 
void  TGeant3::Gtreve() 
{ 
   gtreve(); 
} 
//=======================functions from GGEOM 
//___________________________________________ 
void  TGeant3::Gdtom(Float_t *xd, Float_t *xm, Int_t iflag) 
{ 
   gdtom(xd, xm, iflag); 
} 
//___________________________________________ 
void  TGeant3::Glmoth(Int_t iudet, Int_t iunum, Int_t &nlev, Int_t *lvols, Int_t *lindx) 
{ 
   Int_t *idum=0; 
   glmoth(iudet, iunum, nlev, lvols, lindx, idum); 
} 
//___________________________________________ 
void  TGeant3::Gmedia(Float_t *x, Int_t &numed) 
{ 
  gmedia(x,numed); 
} 
//___________________________________________ 
void  TGeant3::Gmtod(Float_t *xm, Float_t *xd, Int_t iflag) 
{ 
   gmtod(xm, xd, iflag); 
} 
//___________________________________________ 
void  TGeant3::Gsdvn(const char *name, const char *mother, Int_t ndiv, Int_t iaxis) 
{ 
   gsdvn(PASSCHARD(name), PASSCHARD(mother), ndiv, iaxis PASSCHARL(name) PASSCHARL(mother)); 
} 
//___________________________________________ 
void  TGeant3::Gsdvn2(const char *name, const char *mother, Int_t ndiv, Int_t iaxis, Float_t c0i, Int_t numed) 
{ 
   gsdvn2(PASSCHARD(name), PASSCHARD(mother), ndiv, iaxis, c0i, numed PASSCHARL(name) PASSCHARL(mother)); 
} 
//___________________________________________ 
void  TGeant3::Gsdvs(const char *name, const char *mother, Float_t step, Int_t iaxis, Int_t numed) 
{ 
   gsdvs(PASSCHARD(name), PASSCHARD(mother), step, iaxis, numed PASSCHARL(name) PASSCHARL(mother)); 
} 
//___________________________________________ 
void  TGeant3::Gsdvs2(const char *name, const char *mother, Float_t step, Int_t iaxis, Float_t c0, Int_t numed) 
{ 
   gsdvs2(PASSCHARD(name), PASSCHARD(mother), step, iaxis, c0, numed PASSCHARL(name) PASSCHARL(mother)); 
} 
//___________________________________________ 
void  TGeant3::Gsdvt(const char *name, const char *mother, Float_t step, Int_t iaxis, Int_t numed, Int_t ndvmx) 
{ 
   gsdvt(PASSCHARD(name), PASSCHARD(mother), step, iaxis, numed, ndvmx PASSCHARL(name) PASSCHARL(mother)); 
} 
//___________________________________________ 
void  TGeant3::Gsord(const char *name, Int_t iax) 
{ 
   gsord(PASSCHARD(name), iax PASSCHARL(name)); 
} 
//___________________________________________ 
void  TGeant3::Gspos(const char *name, Int_t nr, const char *mother,  
		  Float_t x, Float_t y, Float_t z, Int_t irot, const char *konly) 
{ 
   gspos(PASSCHARD(name), nr, PASSCHARD(mother), x, y, z, irot, PASSCHARD(konly) PASSCHARL(name) PASSCHARL(mother) PASSCHARL(konly)); 
} 
//___________________________________________ 
void  TGeant3::Gsposp(const char *name, Int_t nr, const char *mother,  
		   Float_t x, Float_t y, Float_t z, Int_t irot, const char *konly, Float_t *upar, Int_t np ) 
{ 
   gsposp(PASSCHARD(name), nr, PASSCHARD(mother), x, y, z, irot, PASSCHARD(konly), upar, np PASSCHARL(name) PASSCHARL(mother) PASSCHARL(konly)); 
} 
//___________________________________________ 
void  TGeant3::Gsrotm(Int_t nmat, Float_t theta1, Float_t phi1, Float_t theta2, Float_t phi2, 
		   Float_t theta3, Float_t phi3) 
{ 
   gsrotm(nmat, theta1, phi1, theta2, phi2, theta3, phi3); 
} 
//___________________________________________ 
void  TGeant3::Gprotm(Int_t nmat) 
{ 
   gprotm(nmat); 
} 
//___________________________________________ 
Int_t TGeant3::Gsvolu(const char *name, const char *shape, Int_t nmed,  
		   Float_t *upar, Int_t np) 
{ 
   Int_t ivolu = 0; 
   gsvolu(PASSCHARD(name), PASSCHARD(shape), nmed, upar, np, ivolu PASSCHARL(name) PASSCHARL(shape)); 
   return ivolu; 
} 
//___________________________________________ 
Int_t TGeant3::Glvolu(const Int_t Nlev, Int_t *Lnam, Int_t *Lnum)
{ 
   Int_t Ierr = 0; 
   glvolu(&Nlev, Lnam, Lnum, &Ierr);
   return Ierr; 
} 
//___________________________________________ 
Float_t* TGeant3::Gufld(Float_t *x, Float_t *bf)
{ 
   gufld(x,bf);
   return bf; 
} 
#endif /* Geant3Dummy */
 
 
