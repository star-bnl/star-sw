// $Id: StHistMaker.cxx,v 2.0 2000/08/25 16:02:40 genevb Exp $
// $Log: StHistMaker.cxx,v $
// Revision 2.0  2000/08/25 16:02:40  genevb
// New revision: new structure, multiplicity classes
//
// Revision 1.2  2000/07/26 19:57:50  lansdell
// new histograms and functionality added (e.g., overlay several histograms, new printlist option qa_shift)
//
// Revision 1.1  2000/06/23 21:16:18  kathy
// code that will collect all histograms that were added together in StHistUtil::AddHists - has to be in a maker in order to write it out
//
//
///////////////////////////////////////////////////////////////////////////
#include <iostream.h>

#include "StHistMaker.h"
#include "StHistUtil.h"

#ifndef ROOT_TH1
#include "TH1.h"
#endif


#include "TList.h"
#include "TString.h"

ClassImp(StHistMaker)
  
//_____________________________________________________________________________
  StHistMaker::StHistMaker(const char *name, const char *title) : StMaker(name){

}
//_____________________________________________________________________________

StHistMaker::~StHistMaker(){

}

//_____________________________________________________________________________

Int_t StHistMaker::Finish() {

  return StMaker::Finish();
}
//_____________________________________________________________________________

Int_t StHistMaker::Init(){

  return StMaker::Init();
}
//_____________________________________________________________________________

Int_t StHistMaker::Make(){

  cout << " StHistMaker::Make  " << endl;
  cout << "This is the array " << mHArray << endl;
  for (int i=0; i<512; i++) {
    AddHist(mHArray[i]);
  }

 return kStOK;

}

//_____________________________________________________________________________

