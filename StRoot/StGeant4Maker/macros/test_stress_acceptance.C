#ifndef __CLING__
#include <StBFChain.h>
#include <StChain.h>
#include <StGeant4Maker/StMCParticleStack.h>
#include <TVirtualMC.h>
#include <StMessMgr.h>
#endif

void test_stress_acceptance(int nevents=10, int ntracks=10,const char* particles="pi+,pi-,K+,K-,mu+,mu-,e+,e-,proton,antiproton,pi0,gamma", const bool verbose=false ) {

  double ptmn =  0.100; 
  double ptmx = 10.000;
  double etamn= -2.0; 
  double etamx= +4.0;

  StMaker*           _geant4 = chain->GetMaker("geant4star");

  // Run particle gun simulation
  for ( int i=0;i<nevents;i++ ) {

    particleGun(ntracks,particles,ptmn,ptmx,etamn,etamx);
    if ( false == verbose ) continue;

    // Get the particle stack for inspection
    StMCParticleStack* _stack = (StMCParticleStack*)TVirtualMC::GetMC()->GetStack();

    for ( auto vert : _stack->GetVertexTable() ) {
      LOG_INFO << *vert << endm;
    }

    for ( auto part : _stack->GetParticleTable() ) {
      LOG_INFO << *part << endm;
    }

  }


};
