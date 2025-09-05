//
// Run G4 through standard BFC 
//
#include "StRoot/macros/bfc.C"

#include "StarGenerator/BASE/StarPrimaryMaker.h"

#include "StChain.h"

void bfcGeant4() 
{

  auto* topchain = new StChain("simu");
  topchain->cd();

  // Event generation chain
  const char* evchain = "stargen stargen:stubs stargen:mk kinematics:mk nodefault sdt20230201";
  bfc(-1,evchain);
  auto* chain1 = chain; 
  chain1->SetName("generator");
  topchain->cd();

  auto* kine   = chain1->GetMaker("StarKine");
  kine->SetAttr("type","FlatPT");
  kine->SetAttr("ntrack",int(10));
  kine->SetAttr("particles","pi+,pi0,pi-");
  kine->SetAttr("ptlow",0.1);
  kine->SetAttr("pthigh",10.0);
  kine->SetAttr("ylow",-1.5);
  kine->SetAttr("yhigh",+1.5);

  auto* pmk = chain1->GetMaker("PrimaryMaker");
  pmk->AddMaker( kine );
  pmk->SetAttr("debug",1);

#if 1
  // Geant4 chain  
  const char* g4chain = "ry2023a agml geant4 geant4vmc geant4mk:mk geant4out -emc_t -ftpcT mysql nodefault";
  bfc(-1, g4chain );
  auto* chain2 = chain; 
  chain2->SetName("physicssim");

  topchain->cd();
  auto* geant4mk = chain->GetMaker("geant4star"); 
  geant4mk->SetAttr("application:engine","G4");
#endif

#if 1
  topchain->Init();
  chain1->ls(4);


  assert(pmk);
  pmk->SetAttr("verbose",1);

  //  topchain->EventLoop(100);
  for ( int i=0;i<100;i++ ) {
    topchain->Clear();
    topchain->Make();
    kine->Print();

    ////    pmk->Print();
  };


  topchain->Finish();
#endif



};
