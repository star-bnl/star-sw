// $Id: StRareMaker.h,v 1.2 2001/09/06 20:51:23 hardtke Exp $
//
// $Log: StRareMaker.h,v $
// Revision 1.2  2001/09/06 20:51:23  hardtke
// Update
//


#ifndef StRareMaker_HH
#define StRareMaker_HH

///////////////////////////////////////////////////////////////////////////////
//
// StRareMaker
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
#include "SystemOfUnits.h"
#ifndef ROOT_TH1
#include "TH1.h"
#endif
#ifndef ROOT_TH2
#include "TH2.h"
#endif
#include "TTree.h"
#include "TFile.h"

class StEvent;
class StRun;
class StRareEvent;
class StRareEventCut;
class StRareTrackCut;

class StRareMaker : public StMaker {

private:
  int number_of_events_processed;
  TFile* out;

protected:

    TTree   *m_Tree;

public:

  StRareMaker(const Char_t *name="RareParticles");
  StRareMaker(const Char_t *name,StRareEventCut* cut, StRareTrackCut* track);
  virtual ~StRareMaker(){};
  virtual void Clear(Option_t *option="");
  virtual Int_t Init();
   virtual Int_t  Make();
  virtual void   PrintInfo();
  virtual Int_t  Finish();
  virtual void   Report();

private:
  StRareTrackCut* TrackCut;  //!
  StRareEventCut* EventCut;  //!
  StRareEvent* revt;

  ClassDef(StRareMaker, 1)
};

#endif




