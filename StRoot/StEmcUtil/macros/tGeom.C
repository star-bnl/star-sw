//
// Created 14-mar-2001 for testing method StEmcGeom::getBinEnv by PAI
// Corr. 15-mar =>use only Jose's scheme 
//
#if !defined(__CINT__)
#include <TROOT.h>
#include <TSystem.h>
#include "StEmcUtil/StEmcGeom.h"
#endif

void  tGeom();
Int_t tg1(const Int_t id);
Int_t tg2(const Int_t id);

class StEmcGeom; StEmcGeom *geom = 0;
Int_t m,e,s;

void tGeom()
{
   if(strlen(gSystem->GetLibraries("*StEmcUtil.so","D")) == 0){
    gROOT->ProcessLine(".x Load.C");
    gSystem->Load("StEmcUtil.so");
  }
  if(geom == 0) geom = new StEmcGeom(1);
}

Int_t tg1(const Int_t id)
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

Int_t tg2(const Int_t id)
{
  // Cross checking for getBin and getId
  tGeom();

  Int_t m,e,s, idW, id1, id2;

  id1 = id; id2=id; // Default
  if(id == 0) {id1=1; id2=4800;}

  for(Int_t i=id1; i<=id2; i++){
    Int_t ret =  geom->getBin(i,m,e,s);
    if(ret == 0) {
      Int_t retW = geom->getId(m,e,s,idW);
      if(retW == 0){
        printf(" id %4i : %4i => m %3i e %2i s %i \n", i, idW, m, e, s);
        if(id1 == id2) return ret;
      }
      else {printf(" Something wrong \n"); return 0;}
    }
    else return 1;
  }
  return 10;
}
