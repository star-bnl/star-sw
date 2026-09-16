#ifndef __CLING__
#include "StChain/StMaker.h"

#include "StarGenerator/BASE/StarPrimaryMaker.h"
#include "StarGenerator/UTIL/StarParticleData.h"

#include "StarGenerator/Hijing1_383/StarHijing.h"

#include "StMessMgr.h"
#include "TList.h"
#include "TAttr.h"
#include "TString.h"
#include <vector>

#include <string>
#endif

StarParticleData& particleData = StarParticleData::instance();
StarPrimaryMaker* primaryMk = 0;
StarHijing*      hijingmk = 0;

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
void hijing()
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
  LOG_INFO << "Create and register hijing maker to primaryMk = " << primaryMk << endm;

  std::string blue = geant4mk->SAttr("blue"); if ( blue == "" ) blue="Au";
  std::string yell = geant4mk->SAttr("yell"); if ( yell == "" ) yell="Au";
  double Ecms = geant4mk->DAttr("Ecms"); if ( Ecms <= 0.0 ) Ecms = 200.0;

  LOG_INFO << "Blue beam " << blue.c_str() << endm;
  LOG_INFO << "Yellow beam " << yell.c_str() << endm;
  LOG_INFO << "Ecms = " << Ecms << " GeV " << endm;
    
  hijingmk = new StarHijing();
  hijingmk->SetFrame("CMS", Ecms );
  hijingmk->SetBlue( blue.c_str() );
  hijingmk->SetYell( yell.c_str() );
  hijingmk->SetImpact( 0.0, 30.0 );

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
    if ( str.Contains("hijing:set:") ) {
      str.ReplaceAll("hijing:set:","");
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
    //    if ( str.Contains("hijing:tune") ) {
    //      int tune = hijingmk->IAttr("hijing:tune");
    //      hijingmk->PyTune(tune); // set tune
    //    }
    // if ( str.Contains("hijing:drellyan:ee") ) {
    //   // initialize DY --> ee
    //   drellyan();
    // }
    //    if ( str.Contains("hijing:closedecay") ) {
    //      TString ex = Form("hijingmk->CloseDecays(%s);", tit.Data());
    //      //LOG_INFO<< ex.Data() << endm;
    //      //gROOT->ProcessLine(ex.Data());
    //      closeDecays.push_back(ex);
    //      
    //    }
    //    if ( str.Contains("hijing:opendecay") ) {
    //      TString ex = Form("hijingmk->OpenDecay(%s);", tit.Data());
    //      //      LOG_INFO<< ex.Data() << endm;
    //      //      gROOT->ProcessLine(ex.Data());
    //      openDecays.push_back(ex);
    //    }
  }

  for ( auto iter=mylist.rbegin(); iter!=mylist.rend(); iter++ ) {
    //    TString cmd=*iter;
    std::string cmd = (*iter).Data();
    LOG_INFO << "execute hijing command " << cmd.c_str() << endm;
    //    hijingmk->PyGive(cmd.Data());
    PyGive(cmd);
    setcount++;
  }

  //  for ( auto iter=closeDecays.begin(); iter != closeDecays.end();iter++ ) {
  //    gROOT->ProcessLine( (*iter) );
  //  }
  //  for ( auto iter=openDecays.begin(); iter!= openDecays.end();iter++ ) {
  //    gROOT->ProcessLine( (*iter) );
  //  }
  
  // Always default to nondiffractive (i.e. as-close-to-minbias-as-documented)
  primaryMk->AddGenerator(hijingmk);

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
//void drellyan() {
//
//  auto& pyjets = hijingmk->pyjets();
//  auto& pysubs = hijingmk->pysubs();
//  auto& pydat3 = hijingmk->pydat3();
//  auto& pypars = hijingmk->pypars();
//  auto& pyint5 = hijingmk->pyint5();
//
//  LOG_INFO << "DRELL YAN Z0/GAMMA* e+e-" << endm;
//
//  pysubs.msel    = 0; // select specific processes
//  pysubs.msub(1) = 1; // ffbar --> Z0/gamma*
//  // others??  15? 19?
//
//  // close decays for the Z (and I believe the gamma* as well)
//  hijingmk->CloseDecays(23);
//  hijingmk->OpenDecay( 23, 182, 2 ); // limit decay to electrons
///
//}
