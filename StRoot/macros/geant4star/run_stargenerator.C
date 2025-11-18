#pragma cling add_include_path("StRoot/macros/geant4star/")

class StBFChain;        
class StMessMgr;

#include <string>
#include <TString.h>
#include "StarSimOpts.h"

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
#include <random>
#include <algorithm>

#endif


// Defined w/in the StBFChain library
extern StBFChain* chain;

StBFChain* top = new StBFChain("physicssim");

// Minimal setup to load libraries
std::string chainopts;//="nodefault y2021a sdt20210215 agml stargen:stubs stargen:mk hijing1.383 hijing:mk noinput nooutput ";

std::map<std::string, double> primDAttr = {
  {"PTMIN",   0.01}, {"PTMAX", 250.0 },
  {"ETAMIN",  -3.0}, {"ETAMX", 6.0 },
  {"ZMIN", -999.0},  {"ZMAX", 3999.0 },
  {"XVERTEX", 0.1},  {"XSIGMA", 0.01},
  {"YVERTEX",-0.1},  {"YSIGMA", 0.01},
  {"ZVERTEX", 0.0},  {"ZSIGMA", 15.0}
};

void run_stargenerator( 
		       const int nevents=10, 
		       std::string jobtag="rcf25000:y2025a:AuAu200:G3:hijing1.383:v001",
		       const int index=0
			) {

  std::mt19937 rng(index+1000); // Seed the Mersenne Twister engine
  setupProductionJobs();

  if ( 0==nevents ) {
    ListStarSimOpts();
    return;
  };


  auto job = GetStarSimOpts( jobtag );
  chainopts = job.genopts;



  std::cout << "Chain opts = " << chainopts << std::endl;

  std::string outname = jobtag;
  std::replace( outname.begin(), outname.end(), ':', '_' );
  std::replace( outname.begin(), outname.end(), '.', 'p' );
  outname += "_job";
  outname += std::to_string(index);
  outname = outname + ".genevents.root";
  std::cout << outname << std::endl;

  // Randomly select a timestamp
  std::shuffle( job.timestamps.begin(), job.timestamps.end(), rng );  
  chainopts += " ";
  chainopts += job.timestamps[0];
  
  top->SetDebug(1);
  top->SetFlags(chainopts.c_str());
  //  top->Set_IO_Files(0, outname);
  top->Load();
  top->Instantiate();

  // Configure primary maker
  auto* prim = top->Maker("PrimaryMaker");
  prim->SetAttr("output",outname.c_str());

  // Apply string attributes
  for ( const auto dattr : primDAttr ) { prim->SetAttr( dattr.first.c_str(), dattr.second );  } // defaults



  // Configure event generator
  auto* generator = top->Maker( job.generator.c_str() );

  // Apply string attributes
  for ( const auto sattr : job.genSAttr ) { generator->SetAttr( sattr.first.c_str(), sattr.second.c_str() );  }

  // Apply double attributes
  for ( const auto dattr : job.genDAttr ) { generator->SetAttr( dattr.first.c_str(), dattr.second );  }

  // Apply int attributes
  for ( const auto iattr : job.genIAttr ) { generator->SetAttr( iattr.first.c_str(), iattr.second );  }

  prim->AddMaker( generator );
  prim->SetAttr("verbose",111);

  top->ls(5);
  top->Init();

  top->EventLoop(nevents);

  top->Finish();

};



