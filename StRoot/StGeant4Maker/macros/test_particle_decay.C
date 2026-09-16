#ifndef __CLING__
#include <StBFChain.h>
#include <StChain.h>
#include <StGeant4Maker/StMCParticleStack.h>
#include <TVirtualMC.h>
#include <StMessMgr.h>
#endif

void test_particle_decay(int n=1,const char* particles="pi0") {

  double ptmn =  1.0; 
  double ptmx = 10.0;
  double etamn= -1.0; 
  double etamx=  2.0;

  // Configure G4 maker 
  StMaker* _geant4 = chain->GetMaker("geant4");

  // Run particle gun simulation
  particleGun(n,particles,ptmn,ptmx,etamn,etamx);

  //  gROOT->ProcessLine("chain->Clear();");
#if 0
  StMCParticleStack* stack = (StMCParticleStack *)TVirtualMC::GetMC()->GetStack();

  LOG_INFO << "Stack: ntrack   = " << stack->GetNtrack() << endm;
  LOG_INFO << "Stack: nprimary = " << stack->GetNprimary() << endm;

  int index = 0;
  LOG_INFO << "StarMCParticle table" << endm;  
  for ( auto p : stack->GetParticleTable() ) {
    LOG_INFO << Form("[%04i] ",index++) << *p << " nhits=" << p->numberOfHits() << endm;
  }

  index = 0;
  LOG_INFO << "StarMCVertex table" << endm;
  for ( auto v : stack -> GetVertexTable() ) {
    LOG_INFO << Form("[%04i] ",index++) << *v << endm; 
    if ( v->parent() ) {
      LOG_INFO << "      " << *(v->parent()) << " --> " << endm; 
      //LOG_INFO << Form("    %p", v->parent()) << endm;
    }
    for ( auto d : v->daughters() ) {
        LOG_INFO << "       " << *d << endm; 
    } 
  }
#endif

};
