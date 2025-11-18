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

#include "StarSimOpts.h"


// Defined w/in the StBFChain library
extern StBFChain* chain;

StBFChain* top = new StBFChain("physicssim");

// Minimal setup to load libraries
std::string chainopts="nodefault sdt20210215 agml stargen:mk genreader:mk g4star:mk noinput geant4out ";

StMaker* g4star = 0;
// rcf25000_y2023a_AuAu200_G4_hijing1p383_minbias_v001_job0.genevents.root
void run_geant4star( int nevents=10, 
             std::string jobtag="rcf25000:y2023a:AuAu200:G4:hijing1.383:minbias:v001",
	     const int index=0,
	     std::string engine="G4",
	     std::string physlist="FTFP_BERT",
	     std::string inpname_="TAG"
	     ) {

  setupProductionJobs();

  auto job=GetStarSimOpts( jobtag );
  std::cout << job.simopts << std::endl;

  std::string inpname = jobtag;
  std::string outname;

  std::replace( inpname.begin(), inpname.end(), ':', '_' );
  std::replace( inpname.begin(), inpname.end(), '.', 'p' );
  inpname += "_job";
  inpname += std::to_string(index);

  outname = inpname + ".geant.root";
  inpname = inpname + ".genevents.root";

  if ( inpname_ != "TAG" ) {
    inpname = inpname_;
  }

  chainopts = job.simopts;
  chainopts += " ";
  chainopts += job.timestamps[0];

  std::cout << "chain: " << job.simopts << std::endl;
  std::cout << "name:  " << job.name << std::endl;
  std::cout << "in:    " << inpname << std::endl;
  std::cout << "out:   " << outname << std::endl;

  top->SetDebug(1);
  top->SetFlags(chainopts.c_str());
  top->Set_IO_Files(0, outname.c_str());
  top->Load();
  top->Instantiate();

  auto* prim = top->Maker("PrimaryMaker");
  auto* read = top->Maker("vmcreader");
  read->SetAttr("input",inpname.c_str());
  read->SetAttr("debug",1);
  prim->AddMaker( read );
  prim->SetAttr("debug",1);

  assert(read);
  assert(prim);

  // Apply primary attributes
  for ( const auto dattr : job.primDAttr ) { prim->SetAttr( dattr.first.c_str(), dattr.second );  } // defaults

  //
  // Geant4STAR configuration
  //
  //  const char* g4commands =
  //    "/process/eLoss/maxKinEnergy 1000.0 GeV" 
  //    ;
  g4star = top->Maker("geant4star");
  //  g4star->SetAttr("application:engine", engine.c_str() );

  // ignored if G3...
  g4star->SetAttr("G4VmcOpt",physlist.c_str());
  g4star->SetAttr(
      "G4UI:PREINIT", 
      "/process/eLoss/maxKinEnergy 250.0 GeV"   ";" 
      "/mcCrossSection/setMaxKinE  250.0 GeV"   ";"
  );
  g4star->SetAttr(
      "G4UI:INIT",
      "/mcCrossSection/setMinKinE 1 keV "       ";"
      "/mcCrossSection/setMaxKinE 250 GeV "     ";"
      "/mcCrossSection/setMinMomentum 10 keV "  ";"
      "/mcCrossSection/setMaxMomentum 250 GeV " ";"
  );


  // Apply string attributes
  for ( const auto sattr : job.simSAttr ) {
    g4star->SetAttr( sattr.first.c_str(), sattr.second.c_str() );
  }

  // Apply double attributes
  for ( const auto dattr : job.simDAttr ) {
    g4star->SetAttr( dattr.first.c_str(), dattr.second );
  }  

  // Apply integer flags
  for ( const auto dattr : job.simIAttr ) {
    g4star->SetAttr( dattr.first.c_str(), dattr.second );
  }  

  g4star->SetAttr("runnumber", job.runnumbers[0] );


  top->ls(5);


  gInterpreter->ProcessLine("{Geometry* __hack = new Geometry(); delete __hack;}");

  top->Init();


  gSystem->SetFPEMask( kNoneMask );
  top->EventLoop(nevents, top->Maker("outputStream"));
  top->Finish();

};



