
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
//  Craig Ogilvie MIT
//
// History:
//
///////////////////////////////////////////////////////////////////////////////
#include "StMaker.h"


#include <TOrdCollection.h>
#include <TFile.h>
class StEvent;
class StRun;
class TH1F;

class StAngleCorrMaker : public StMaker {

private:

  // Maker generates a container of tracks 
  TOrdCollection* mCollectionOfTracksA;
  TOrdCollection* mCollectionOfTracksB;
  TOrdCollection* mCollectionOfHighestPt;
  int mNumberEventsInPool ;
  int mTotalEvents ;

  void analyseRealPairs(StEvent&, int);
  void analyseMixedPairs();

protected:
  // maker generates some histograms
  TFile* mOutput;

  TH1F* mPhiNumeratorPtThresh ;
  TH1F* mPhiDenominatorPtThresh ;
  TH1F* mPhiNumeratorPtHigh ; 
  TH1F* mPhiDenominatorPtHigh ;

public:

  StAngleCorrMaker(const Char_t *name="angle corr", const Char_t *title="angle corr");
  virtual ~StAngleCorrMaker();
  virtual Int_t Init();
  virtual Int_t  Make();
  virtual Int_t  Finish();

  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StAngleCorrMaker.h,v 1.6 1999/08/25 14:19:35 ogilvie Exp $ built "__DATE__" "__TIME__ ; return cvs;}

  ClassDef(StAngleCorrMaker, 1)
};

#endif
