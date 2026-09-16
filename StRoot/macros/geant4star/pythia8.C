class StBFChain;        
class StMessMgr;

#include <string>
#include <TString.h>

// Load sufficient libraries to bootstrap the StBFChain framework
#pragma cling load("libTree.so")
#pragma cling load("StarRoot")
#pragma cling load("St_base")
#pragma cling load("StChain")
#pragma cling load("StUtilities")
#pragma cling load("StBFChain")
#pragma cling load("liblog4cxx")
#pragma cling load("StStarLogger.so")
#pragma cling load("StarClassLibrary.so")
#pragma cling load("libmysqlclient.so")
//#pragma cling load("libStarMiniCern.so")

#if !(defined(__CINT__) || defined(__CLING__)) || defined(__MAKECINT__)

#include "Stiostream.h"
#include "TSystem.h"
#include "TClassTable.h"
#include "TApplication.h"
#include "TInterpreter.h"
#include "StBFChain.h"
#include "StMessMgr.h"
#include "TROOT.h"
#include "TAttr.h"
#include "Rtypes.h"

#endif


// Defined w/in the StBFChain library
extern StBFChain* chain;

StBFChain* top = new StBFChain("physicssim");

// Minimal setup to load libraries
std::string chainopts="nodefault y2021a sdt20210215 agml stargen:mk pythia8.1.86 pythia8:mk g4star:mk noinput geant4out ";

StMaker* g4star = 0;

void pythia8( int nevents=1, 
		  const char* outname="pythia8.geant.root",
		  std::string physlist="FTFP_BERT"		  
		  ) {
  
  top->SetDebug(1);
  top->SetFlags(chainopts.c_str());
  top->Set_IO_Files(0, outname);
  top->Load();
  top->Instantiate();


  auto* prim = top->Maker("PrimaryMaker");

  //
  // Setup pythia8 for qqbar --> W --> e+/- nu(bar) production
  //
  auto* pythia8 = top->Maker("Pythia8");
  pythia8->SetAttr("FRAME", "CMS");
  pythia8->SetAttr("Ecms",  200.0);
  pythia8->SetAttr("BLUE", "proton");
  pythia8->SetAttr("YELL", "proton");
  pythia8->SetAttr("SET",  "HardQCD:all=on" );
  prim->AddMaker( pythia8 );
  prim->SetAttr("verbose",111);


  //
  // Geant4STAR configuration
  //
  //  const char* g4commands =
  //    "/process/eLoss/maxKinEnergy 1000.0 GeV" 
  //    ;
  g4star = top->Maker("geant4star");
  g4star->SetAttr("G4VmcOpt",physlist.c_str());

  g4star->SetAttr(
      "G4UI:PREINIT", 
      "/process/eLoss/maxKinEnergy 250.0 GeV"   ";" 
      "/mcCrossSection/setMaxKinE  250.0 GeV"   ";"
      "/process/verbose" ";"
  );
  //  g4star->SetAttr(
  //      "G4UI:INIT",
  //      "/mcCrossSection/setMinKinE 1 keV "       ";"
  //      "/mcCrossSection/setMaxKinE 250 GeV "     ";"
  //      "/mcCrossSection/setMinMomentum 10 keV "  ";"
  //      "/mcCrossSection/setMaxMomentum 250 GeV " ";"
  //  );

  //  g4star->SetAttr(
  //		  "G4UI:PRETRIG",
  //		  "prompt"
  //		  );

  //  g4star->SetAttr(
  //		  "G4UI:POSTTRIG",
  //		  "prompt"
  //		  );


  //  g4star -> SetAttr("G4VmcOpt:Phys",  "FTFP_BERT");
  //  g4star -> SetAttr("G4VmcOpt:Phys",  "QGSP_BERT");



  top->ls(5);
  
  top->Init();

  gSystem->SetFPEMask( kNoneMask );
  top->EventLoop(nevents, top->Maker("outputStream"));
  top->Finish();

};



