#ifndef __CLING__
#include <StBFChain.h>
#include <StChain.h>
#include <StGeant4Maker/StMCParticleStack.h>
#include <TVirtualMC.h>
#include <StMessMgr.h>
#endif

void test_forward(int n=1,const char* particles="pi+,pi-") {

  double ptmn =  1.0; 
  double ptmx = 10.0;
  double etamn= -2.8; 
  double etamx=  3.8;

  // Configure G4 maker 
  StMaker* _geant4 = chain->GetMaker("geant4");

  // Run particle gun simulation
  particleGun(n,particles,ptmn,ptmx,etamn,etamx);


};
