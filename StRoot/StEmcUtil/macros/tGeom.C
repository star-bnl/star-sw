//
// Created 14-mar-2001 for testing method StEmcGeom::getBinEnv by PAI 
//
#if !defined(__CINT__)
#include <TROOT.h>
#include <TSystem.h>
#include "StEmcUtil/StEmcGeom.h"
#endif

void  tGeom();
Int_t tg1(const Int_t idEnv);
Int_t tg2(const Int_t idEnv);

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

Int_t tg1(const Int_t idEnv)
{
  tGeom();

  Int_t ret, idOfl;
  ret =  geom->getBinEnv(idEnv,m,e,s);

  if(ret == 0) {
    geom->getId(m,e,s, idOfl);
    printf(" idEnv %4i idOfl %4i => m %3i e %2i s %i \n", idEnv, idOfl, m, e, s);
    return ret;
  }
  else return 1;
}

Int_t tg2(const Int_t idEnv)
{
  // Cross checking for getBinEnv and getIdEnv
  tGeom();

  Int_t m,e,s, idEnvW;
  Int_t ret =  geom->getBinEnv(idEnv,m,e,s);
  if(ret == 0) {
    Int_t retW = geom->getIdEnv(m,e,s,idEnvW);
    if(retW == 0){
      printf(" idEnv %4i : %4i => m %3i e %2i s %i \n", idEnv, idEnvW, m, e, s);
      return ret;
    }
    else {printf(" Something wrong \n"); return 0;}
  }
  else return 1;
}
