//
// 9-mar-2001 for testing StEmcMath 
//
//.include "$STAR/StRoot "
//gDebug=5 
#if !defined(__CINT__)
#include <TROOT.h>
#include <TSystem.h>
#include "StarClassLibrary/StThreeVectorF.hh"
#include "StEvent/StMeasuredPoint.h"
#include "StEvent/StHit.h"
#include "StEmcUtil/StEmcMath.h"
#endif

void tEmcMath();

class StMeasuredPoint; StMeasuredPoint *hit1, *hit2;

void tEmcMath()
{
  if(strlen(gSystem->GetLibraries("*StEmcUtil.so","D")) == 0){
    gROOT->ProcessLine(".x Load.C");
    gSystem->Load("StEmcUtil.so");
  }

  StThreeVectorF  v1(230., 0.0, 0.0);
  StThreeVectorF  ve(2,2,2); // always the same
  printf(" For vector => eta %f phi %f\n",v1.pseudoRapidity(),v1.phi());
  hit1 = new  StHit(v1,ve,0,0,0);

  Double_t eta,phi;
  for(Int_t z=-50; z<=50; z+=10){
    StThreeVectorF  vzero(0,0,z);
    hit2 = new  StHit(vzero,ve,0,0,0);
    Bool_t tmp = StEmcMath::etaPhi(hit1,hit2, eta,phi);
    printf(" For StEmcMath : z %5.1f eta %f phi %f => %i \n", z, eta, phi,tmp);
    delete hit2;
  }
}
