//
// Created 14-mar-2001 for testing method StEmcGeom::getBinEnv by PAI
// Corr. 15-mar =>use only Jose's scheme 
//
#if !defined(__CINT__)
#include <TROOT.h>
#include <TSystem.h>
#include "StEmcUtil/StEmcGeom.h"
#endif

void  tgeom(const Int_t det=1);
Int_t tg1(const Int_t id);
Int_t tg2(const Int_t det=1, const Int_t id=0);
void  tg3();
void  tg4(const Int_t det=1, const Int_t m1=1,const Int_t m2=120);
//void  tg4(const Int_t det, const Int_t m) {tg4(det,m,m);}

class StEmcGeom; StEmcGeom *geom = 0;
Int_t m,e,s;

void 
tgeom(const Int_t det)
{
  if(strlen(gSystem->GetLibraries("*StEmcUtil.so","D")) == 0){
    gROOT->ProcessLine(".x Load.C");
    gSystem->Load("StEmcUtil.so");
  }
  if(det>=1&&det<=4) { 
    //  if(geom) delete geom;
    geom = new StEmcGeom(det);
  }
}

Int_t 
tg1(const Int_t id)
{
  tgeom();

  Int_t ret;
  ret =  geom->getBin(id,m,e,s);

  if(ret == 0) {
    printf(" id %4i => m %3i e %2i s %i \n", id, m, e, s);
    return ret;
  }
  else return 1;
}

Int_t 
tg2(const Int_t det, const Int_t id)
{
  // Cross checking for getBin and getId
  tgeom(det);

  Int_t m,e,s, idW, id1, id2; 
  //  det=geom->Detector();

  id1 = id; id2=id; // Default
  if(id == 0) {
    switch (det){  
    case 1:
    case 2: {id1=1; id2=4800; break;}
    case 3:
    case 4: {id1=1; id2=18000; break;}
    default: return 0;
    }
  }

  Int_t ibad = 0; // bad case
  for(Int_t i=id1; i<=id2; i++){
    Int_t ret =  geom->getBin(i,m,e,s);
    if(ret == 0) {
      Int_t retW = geom->getId(m,e,s,idW);
      if(retW == 0){
        if(i != idW) {
          ibad++;
          printf(" bad case %i | id %4i : %4i => m %3i e %2i s %i \n" 
         ,ibad, i, idW, m, e, s);
        }
      }
      else {printf(" Something wrong \n"); return 0;}
    }
    else return 1;
  }

  if(ibad) printf(" Detector %i Bad case %i \n", det, ibad);
  else     printf(" Detector %i => No Errors \n", det);

  return 10;
}

void 
tg3()
{
  // 
  tgeom(0);
  Int_t det, id=1;
  for(det=1; det<=4; det++){
    geom = new StEmcGeom(det);
    Int_t ret =  geom->getBin(id,m,e,s);
    if(ret == 0) printf(" det %i id %i => m %i e %i s %i \n",
    det,id, m,e,s);
  }
}

void 
tg4(const Int_t det,const Int_t m1, const Int_t m2)
{
  //
  // For checking getBin(phi,eta,m,e,s) - 26-jul-2001
  // 
  tgeom(det);
  Float_t phi,eta;
  Int_t mw, ew, sw, ier1=0, ier2=0;
  
  for(Int_t m=m1; m<=m2; m++){
    for(Int_t e=1; e<=geom->NEta(); e++){
      for(Int_t s=1; s<=geom->NSub(); s++){
        if(geom->getEta(m,e,eta)==0&&geom->getPhi(m,s,phi)==0){
           geom->getBin(phi,eta, mw,ew,sw);
           if(m!=mw || e!=ew || s!=sw) {
             ier2++;
             printf("<E> %5i | m %i=>%i e %i=>%i s %i=>%i phi %f eta %f \n",
             ier2,  m,mw, e,ew, s,sw, phi,eta);
           }
        }
        else {
          ier1++;
          printf("<E> %5i | something wrong m %i e %i s %i\n", ier1, m,e,s);
        }
      }
    }
  }
  printf(" For Detector %i ",det);
  if(ier1==0 && ier2==0) printf(" => No Errors \n");
  else printf(" #Error1 %i #Error2 %i\n", ier1, ier2); 
}

