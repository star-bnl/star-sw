#ifndef __CLING__

#include "StChain/StMaker.h"

#include "StarGenerator/BASE/StarPrimaryMaker.h"
#include "StarGenerator/UTIL/StarParticleData.h"

#include "StarGenerator/Pythia8_1_86/StarPythia8.h"

#include "StMessMgr.h"
#include "TList.h"
#include "TAttr.h"
#include "TString.h"
#include <vector>

#include <string>
#endif

StarParticleData& particleData = StarParticleData::instance();
StarPrimaryMaker* primaryMk = 0;

int nevents = 0;

//___________________________________________________________________
void init();       // initialize the chain
void eventloop();

//___________________________________________________________________
void pythia8()
{
  
  init();
  eventloop();

  // Finish ... chain has been finished... so wtf?
  chain->Finish();

}
//___________________________________________________________________
void init() {

  primaryMk   = dynamic_cast<StarPrimaryMaker*>( chain->GetMaker("PrimaryMaker") );   assert(primaryMk);
  auto* geant4mk    = dynamic_cast<StGeant4Maker*>   (chain->GetMaker("geant4star") );      assert(geant4mk);

  assert(&particleData);

  nevents = geant4mk->IAttr("nevents");
  LOG_INFO << "Number of events to process" << nevents << endm;
  LOG_INFO << "Create and register pythia8 maker to primaryMk = " << primaryMk << endm;

  std::string blue = geant4mk->SAttr("blue"); if ( blue == "" ) blue="proton";
  std::string yell = geant4mk->SAttr("yell"); if ( yell == "" ) yell="proton";
  double Ecms = geant4mk->DAttr("cms"); if ( Ecms <= 0.0 ) Ecms = 510.0;

  LOG_INFO << "Blue beam " << blue.c_str() << endm;
  LOG_INFO << "Yellow beam " << yell.c_str() << endm;
  LOG_INFO << "Ecms = " << Ecms << " GeV " << endm;
    
  StarPythia8* pythia8 = new StarPythia8();
  pythia8->SetFrame("CMS", Ecms );
  pythia8->SetBlue( blue.c_str() );
  pythia8->SetYell( yell.c_str() );

  int setcount = 0;
  auto* setlist = dynamic_cast<const TAttr*>(geant4mk->GetAttr());
  setlist->Print();
  TIter Next(setlist);
  TNamed* obj = 0;
  // ROOT's interface is a bit annoying here.  It has reveresed the order of the
  // arguements stuffed into the TList.  And I don't see an easy (read syntatically
  // clear and concise) way to reverse iterate over a TList... so...
  std::vector<TString> mylist;
  while (( obj = dynamic_cast<TNamed*>(Next()) )) {
    TString str = obj->GetName();
    TString tit = obj->GetTitle();
    if ( str.Contains("pythia8:set:") ) {
      str.ReplaceAll("pythia8:set:","");
      TString cmd = str + "=" + tit;
      mylist.push_back(cmd);
    }
  }

  for ( auto iter=mylist.rbegin(); iter!=mylist.rend(); iter++ ) {
    TString cmd=*iter;
    LOG_INFO << cmd.Data() << endm;
    pythia8->Set(cmd.Data());
    setcount++;
  }
  

  

  // Always default to nondiffractive (i.e. as-close-to-minbias-as-documented)
  if ( 0==setcount )       pythia8->Set("SoftQCD:nonDiffractive=on");

  primaryMk->AddGenerator(pythia8);

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
