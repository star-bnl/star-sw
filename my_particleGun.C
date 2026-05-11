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
// #pragma cling load("libStarMiniCern.so")

#if !(defined(__CINT__) || defined(__CLING__)) || defined(__MAKECINT__)

#include "Stiostream.h"
#include "TSystem.h"
#include "TClassTable.h"
#include "TApplication.h"
#include "TInterpreter.h"
#include "StBFChain.h"
#include "StMessMgr.h"
#include "TROOT.h"
#include "TError.h" // Added for gErrorIgnoreLevel
#include "TAttr.h"
#include "Rtypes.h"
#include "TRandom.h"
#include <random>
#include <algorithm>

#endif



#include <vector>
#include <iostream>
#include <fstream>
#include <map>
#include <TTable.h>

#include "g2t_emc_hit.h"
#include "g2t_track.h"
#include "g2t_vertex.h"

// Defined w/in the StBFChain library
extern StBFChain* chain;

StBFChain* top = new StBFChain("physicssim");

StMaker *pmk = nullptr;
StMaker *kine = nullptr;
StMaker *g4mk = nullptr;

// Minimal setup to load libraries
std::string chainopts="nodefault y2023a sdt20230722.042300 agml stargen:mk kinematics:mk g4star:mk noinput geant4out ";

StMaker* g4star = 0;



void my_particleGun( 
      int nevents=10, 
		  const char* outname="particleGun.geant.root",
		  int ntracks=1,
		  double ptmn=2.0,
		  double ptmx=2.0000000001,
		  double etamn=2.1,
		  double etamx=5.1,
		  double phimn=0.0,
		  double phimx=TMath::TwoPi(),
      int pid  = 5,
		  std::string distribution="FlatPT",
		  std::string physlist="FTFP_BERT"
		  ) {
  
  std::cout << "--- Initializing Chain ---" << std::endl;
  std::string chainopts = "noinput nodefault dev2021 sdt20210215 agml stargen:mk kinematics:mk g4star:mk geant4out";

  top->SetDebug(1);
  top->SetFlags(chainopts.c_str());
  top->Set_IO_Files(0, outname);
  top->Load();
  top->Instantiate();

  pmk  = top->Maker("PrimaryMaker");
  kine = top->Maker("StarKine");
  g4mk = top->Maker("geant4star");

  g4mk->SetAttr("application:engine", "G3");
  g4mk->SetAttr("hcal:engine", "G3");
  g4mk->SetAttr("wcal:engine", "G3");
  g4mk->SetAttr("runnumber", 24203001 );
  pmk->AddMaker(kine);
  pmk->SetAttr("verbose", 0);
  
  gInterpreter->ProcessLine("{Geometry* __hack = new Geometry(); delete __hack;}");
  kine->SetAttr("mode",    distribution.c_str() );
  kine->SetAttr("ptlow",   ptmn     );
  kine->SetAttr("pthigh",  ptmx     );
  kine->SetAttr("etalow",  etamn    );
  kine->SetAttr("etahigh", etamx    );
  kine->SetAttr("phimn",   phimn    );
  kine->SetAttr("phimx",   phimx    );
  kine->SetAttr("pid",     pid      );
  kine->SetAttr("ntrack",  ntracks  );

  top->ls("*");
  
  top->Init();
  gSystem->SetFPEMask(kNoneMask);

  top->EventLoop(nevents, top->Maker("outputStream"));
  


  top->Finish();


};
