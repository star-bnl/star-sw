//
// Created 14-mar-2001 for testing method StEmcGeom::getBinEnv by PAI
// Corr. 15-mar =>use only Jose's scheme 
//
#if !defined(__CINT__)
#include <TROOT.h>
#include <TSystem.h>
#include "StEmcUtil/StEmcGeom.h"
#endif

void  tGeom(const Int_t det=1);
Int_t tg1(const Int_t id);
Int_t tg2(const Int_t det=1, const Int_t id=0);
Int_t tg3();

class StEmcGeom; StEmcGeom *geom = 0;
Int_t m,e,s;

void 
tGeom(const Int_t det)
{
   if(strlen(gSystem->GetLibraries("*StEmcUtil.so","D")) == 0){
    gROOT->ProcessLine(".x Load.C");
    gSystem->Load("StEmcUtil.so");
  }
  if(det>=1&&det<=4) geom = new StEmcGeom(det);
}

Int_t 
tg1(const Int_t id)
{
  tGeom();

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
  tGeom(det);

  Int_t m,e,s, idW, id1, id2, det=geom->Detector();

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

Int_t 
tg3()
{
  // 
  tGeom(0);
  Int_t det, id=1;
  for(det=1; det<=4; det++){
    geom = new StEmcGeom(det);
    Int_t ret =  geom->getBin(id,m,e,s);
    if(ret == 0) printf(" det %i id %i => m %i e %i s %i \n",
    det,id, m,e,s);
  }
}
