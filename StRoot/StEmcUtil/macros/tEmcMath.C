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
#include "StEmcUtil/emcInternalDef.h"
#include "StDetectorId.h"

void tEmcLoad();
void tEmcMath(); 
void tEmcId();

class StMeasuredPoint; StMeasuredPoint *hit1, *hit2;

void tEmcLoad()
{
  if(strlen(gSystem->GetLibraries("*StEmcUtil.so","D")) == 0){
    gROOT->ProcessLine(".x Load.C");
    gSystem->Load("StEmcUtil.so");
  }
}

void tEmcMath()
{
  tEmcLoad();
   
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

void tEmcId()
{
  tEmcLoad();
  UInt_t id, idw; StDetectorId stId=0;
  cout<<" Check local to STAR \n";
  for(id=0; id<=9; id++){
     stId = StEmcMath::detectorId(id);
    if(stId) cout<<" Local Emc Id "<<id<<" => Star Id "<<UInt_t(stId)<<endl;
    else cout<<" Wrong local Emc Id "<<id<<endl;
  }

  cout<<"\n Check STAR to local\n";
  for(idw=7; idw<=17; idw++){
    stId = StDetectorId(idw);
    id = StEmcMath::detectorId(stId);
    if(id) cout<<" Star Id "<<UInt_t(stId)<<" => Local Emc Id "<<id<<endl;
    else cout<<" Wrong Star Id "<<UInt_t(stId)<<endl;
  }
}
