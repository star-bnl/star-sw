

#ifndef StAngleCorrMaker_HH
#define StAngleCorrMaker_HH

///////////////////////////////////////////////////////////////////////////////
//
// StAnalysisMaker
//
// Description: 
//  Sample maker to access and analyze StEvent
//
// Environment:
//  Software developed for the STAR Detector at Brookhaven National Laboratory
//
// Author List: 
//  Torre Wenaus, BNL
//
// History:
//
///////////////////////////////////////////////////////////////////////////////
#include "StMaker.h"
#include "tables/HighPtTag.h"

#include <TOrdCollection.h>

class StEvent;
class StRun;
class TH1F;

class StAngleCorrMaker : public StMaker {

private:

  Bool_t drawinit;
  Char_t collectionName[256];

  // Maker generates a container of tracks 
  TOrdCollection* mCollectionOfTracks;
  int mNumberEvents ;

  void MakeHistograms();
  void analyseRealPairs();

protected:
  // maker generates some histograms
  TH1F* mHistPhiNumerator;

public:

  StAngleCorrMaker(const Char_t *name="angle corr", const Char_t *title="angle corr");
  virtual ~StAngleCorrMaker();
  virtual void Clear(Option_t *option="");
  virtual Int_t Init();
  virtual Int_t  Make();
  virtual void   PrintInfo();
  virtual Int_t  Finish();

  ClassDef(StAngleCorrMaker, 1)
};

#endif
