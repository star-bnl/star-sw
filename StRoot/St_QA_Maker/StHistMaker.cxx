// $Id: StHistMaker.cxx,v 2.3 2003/09/02 17:59:21 perev Exp $
// $Log: StHistMaker.cxx,v $
// Revision 2.3  2003/09/02 17:59:21  perev
// gcc 3.2 updates + WarnOff
//
// Revision 2.2  2002/09/06 02:51:34  genevb
// Remove limit on maximum number of histograms that can be copied
//
// Revision 2.1  2001/05/16 20:57:03  lansdell
// new histograms added for qa_shift printlist; some histogram ranges changed; StMcEvent now used in StEventQA
//
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
#include <Stiostream.h>

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
  for (int i=0; i<mHArraySize; i++) {
    AddHist(mHArray[i]);
  }

 return kStOK;

}

//_____________________________________________________________________________

