// $Id: StRareMaker.h,v 1.6 2002/01/18 19:14:10 struck Exp $
//
// $Log: StRareMaker.h,v $
// Revision 1.6  2002/01/18 19:14:10  struck
// compress track classes, filter only hadronic unbiased/Z=-2 events
//
// Revision 1.5  2001/10/16 01:26:14  struck
// added filename parameter for tree file to constructors
//
// Revision 1.4  2001/10/15 20:20:27  struck
// first version with L3 included
//
// Revision 1.3  2001/09/14 18:00:25  perev
// Removed references to StRun.
//
// Revision 1.2  2001/09/06 20:51:23  hardtke
// Update
//


#ifndef StRareMaker_HH
#define StRareMaker_HH


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
//-tu class StRun;
class StRareEvent;
class StRareEventCut;
class StRareTrackCut;
class StL3RareTrackCut;

class StRareMaker : public StMaker {

private:
  int number_of_events_processed;
  TFile* out;

protected:

    TTree   *m_Tree;

public:

  StRareMaker(const Char_t *name="RareParticles", Char_t* fileName="RareEvent.root");
  StRareMaker(const Char_t *name, Char_t* fileName, StRareEventCut* cut, StRareTrackCut* track);
  StRareMaker(const Char_t *name,
	      Char_t* fileName,
	      StRareEventCut* cut,
	      StRareTrackCut* trackCut,
	      StL3RareTrackCut* l3trackCut);
  virtual ~StRareMaker(){};
  virtual void  Clear(Option_t *option="");
  virtual Int_t  Init();
  virtual Int_t  Make();
  virtual void   PrintInfo();
  virtual Int_t  Finish();
  virtual void   Report();

private:
  StRareTrackCut*   mTrackCut;  //!
  StRareEventCut*   mEventCut;  //!
  StL3RareTrackCut* mL3TrackCut; //!
  StRareEvent*      mRareEvent;

  ClassDef(StRareMaker, 1)
};

#endif




