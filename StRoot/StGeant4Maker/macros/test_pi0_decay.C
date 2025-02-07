#ifndef __CLING__
#include <StBFChain.h>
#include <StChain.h>
#endif

void test_pi0_decay() {

  double ptmn=1.0; 
  double ptmx=10.0;
  double etamn=-1.0; 
  double etamx=2.0;

  double cutgam = 999.0;
  double cutele = 999.0;
  double dcute  = 999.0;
  double dcutm  = 999.0;
  double bcute  = 999.0;
  double bcutm  = 999.0;

  StMaker* _geant4 = chain->GetMaker("geant4");
  
  _geant4 -> SetAttr("CUTGAM",cutgam); 
  _geant4 -> SetAttr("CUTELE",cutele); 
  _geant4 -> SetAttr("DCUTE",dcute); 
  _geant4 -> SetAttr("DCUTM",dcutm); 
  _geant4 -> SetAttr("BCUTE",bcute); 
  _geant4 -> SetAttr("BCUTM",bcutm); 

  int number[] = { 1, 10, 100, 1000, 10000 };

  for ( auto n : number ) {   
    particleGun(n,"pi0",ptmn,ptmx,etamn,etamx);
    gROOT->ProcessLine("chain->Clear();");
  }

};
