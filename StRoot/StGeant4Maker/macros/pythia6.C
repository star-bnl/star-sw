#ifndef __CLING__
#include "StChain/StMaker.h"

#include "StarGenerator/BASE/StarPrimaryMaker.h"
#include "StarGenerator/UTIL/StarParticleData.h"

#include "StarGenerator/Pythia6_4_28/StarPythia6.h"

#include "StMessMgr.h"
#include "TList.h"
#include "TAttr.h"
#include "TString.h"
#include <vector>

#include <string>
#endif

StarParticleData& particleData = StarParticleData::instance();
StarPrimaryMaker* primaryMk = 0;
StarPythia6*      pythia6mk = 0;

int nevents = 0;

extern "C" {

  // Interface to the pythia configuration command
  void pygive_( const char* give, int ngive );

};

void PyGive( std::string give ) {
  pygive_( give.c_str(), give.size() );
}

//___________________________________________________________________
void init();       // initialize the chain
void eventloop();
void drellyan();
//___________________________________________________________________
void pythia6()
{
  
  init();
  eventloop();

}
//___________________________________________________________________
void init() {

  primaryMk   = dynamic_cast<StarPrimaryMaker*>( chain->GetMaker("PrimaryMaker") );   assert(primaryMk);
  auto* geant4mk    = dynamic_cast<StGeant4Maker*>   (chain->GetMaker("geant4star") );      assert(geant4mk);

  assert(&particleData);

  nevents = geant4mk->IAttr("nevents");
  LOG_INFO << "Number of events to process" << nevents << endm;
  LOG_INFO << "Create and register pythia6 maker to primaryMk = " << primaryMk << endm;

  std::string blue = geant4mk->SAttr("blue"); if ( blue == "" ) blue="proton";
  std::string yell = geant4mk->SAttr("yell"); if ( yell == "" ) yell="proton";
  double Ecms = geant4mk->DAttr("Ecms"); if ( Ecms <= 0.0 ) Ecms = 510.0;

  LOG_INFO << "Blue beam " << blue.c_str() << endm;
  LOG_INFO << "Yellow beam " << yell.c_str() << endm;
  LOG_INFO << "Ecms = " << Ecms << " GeV " << endm;
    
  pythia6mk = new StarPythia6();
  pythia6mk->SetFrame("CMS", Ecms );
  pythia6mk->SetBlue( blue.c_str() );
  pythia6mk->SetYell( yell.c_str() );

  int setcount = 0;
  auto* setlist = dynamic_cast<const TAttr*>(geant4mk->GetAttr());
  setlist->Print();
  TIter Next(setlist);
  TNamed* obj = 0;
  // ROOT's interface is a bit annoying here.  It has reveresed the order of the
  // arguements stuffed into the TList.  And I don't see an easy (read syntatically
  // clear and concise) way to reverse iterate over a TList... so...
  std::vector<TString> mylist;
  std::vector<TString> closeDecays;
  std::vector<TString> openDecays;
  while (( obj = dynamic_cast<TNamed*>(Next()) )) {
    TString str = obj->GetName();
    TString tit = obj->GetTitle();
    if ( str.Contains("pythia6:set:") ) {
      str.ReplaceAll("pythia6:set:","");
      TString cmd = str + "=";
      if ( tit.IsDigit() ) { // integer digits
	cmd += tit;
      }
      else if ( tit.IsFloat() ) { // floating point
	// keep to five decimal places
	cmd += Form( "%.5f", tit.Atof() );
      }
      else {
	cmd += tit;
      }
      mylist.push_back(cmd);
    }
    if ( str.Contains("pythia6:tune") ) {
      int tune = pythia6mk->IAttr("pythia6:tune");
      pythia6mk->PyTune(tune); // set tune
    }
    // if ( str.Contains("pythia6:drellyan:ee") ) {
    //   // initialize DY --> ee
    //   drellyan();
    // }
    if ( str.Contains("pythia6:closedecay") ) {
      TString ex = Form("pythia6mk->CloseDecays(%s);", tit.Data());
      //LOG_INFO<< ex.Data() << endm;
      //gROOT->ProcessLine(ex.Data());
      closeDecays.push_back(ex);
      
    }
    if ( str.Contains("pythia6:opendecay") ) {
      TString ex = Form("pythia6mk->OpenDecay(%s);", tit.Data());
      //      LOG_INFO<< ex.Data() << endm;
      //      gROOT->ProcessLine(ex.Data());
      openDecays.push_back(ex);
    }
  }

  for ( auto iter=mylist.rbegin(); iter!=mylist.rend(); iter++ ) {
    //    TString cmd=*iter;
    std::string cmd = (*iter).Data();
    LOG_INFO << "execute pythia6 command " << cmd.c_str() << endm;
    //    pythia6mk->PyGive(cmd.Data());
    PyGive(cmd);
    setcount++;
  }

  for ( auto iter=closeDecays.begin(); iter != closeDecays.end();iter++ ) {
    gROOT->ProcessLine( (*iter) );
  }
  for ( auto iter=openDecays.begin(); iter!= openDecays.end();iter++ ) {
    gROOT->ProcessLine( (*iter) );
  }
  
  // Always default to nondiffractive (i.e. as-close-to-minbias-as-documented)
  primaryMk->AddGenerator(pythia6mk);

  chain->Init();

}
//___________________________________________________________________
void eventloop() {

  for ( int event = 0; event < nevents; event++ ) {
    LOG_INFO << "[geant4star running event " << event << "/" << nevents << "]" << endm;
    chain->Clear();
    chain->Make();
    primaryMk->event()->Print();
  }

}
//___________________________________________________________________
void drellyan() {

  auto& pyjets = pythia6mk->pyjets();
  auto& pysubs = pythia6mk->pysubs();
  auto& pydat3 = pythia6mk->pydat3();
  auto& pypars = pythia6mk->pypars();
  auto& pyint5 = pythia6mk->pyint5();

  LOG_INFO << "DRELL YAN Z0/GAMMA* e+e-" << endm;

  pysubs.msel    = 0; // select specific processes
  pysubs.msub(1) = 1; // ffbar --> Z0/gamma*
  // others??  15? 19?

  // close decays for the Z (and I believe the gamma* as well)
  pythia6mk->CloseDecays(23);
  pythia6mk->OpenDecay( 23, 182, 2 ); // limit decay to electrons

}
