#ifndef __StarFilterMaker_h__
#define __StarFilterMaker_h__

#include "StMaker.h"
#include "TLorentzVector.h"
#include "TVector3.h"
#include "TClonesArray.h"
#include "TDatabasePDG.h"
//#include "StarParticleStack.h"

#include "StarGenerator/UTIL/StarParticleData.h"
#include "StarGenerator/BASE/StarPrimaryMaker.h"

class StarGenEvent;
class StarPrimaryMaker;

class TTree;
class TFile;

/**
   \class StarFilterMaker
   \author John F. Novak
   \brief Main filter class. Goes anywhere in the chain, filters StarGenEvent objects
*/

/*
   To Do's: JFN 4/22/13

   Should the filter make it's own tree? Probably not, it should probably make a branch of the 'main' tree
      4/23- make a branch on the PrimaryMakers tree
   How do things get passed along the chain? StarGenEvents? or a collection of them? What gets handed to the next link?
      4/23- StMaker has a TDataSet object and functions to access it, but StarPrimaryMaker is also writing things to the tree
*/

/*
   To Do's: JFN 4/23/13

   For each pass of BFC is only one event generated? Is it named mPrimaryEvent, or are their multiple events?
      4/24- Just one, and it is named "PrimaryMaker". It can be retreived from StMaker
   I think for each event I should use some "Event()" function defined on the event generator class, haven't found it yet...
     And how will we handle looping over generators?
      4/24- There is a one primary event per BFC loop and it is StarPrimaryMaker->mPrimaryEvent retreivable via primary->event()
*/

/*
   To Do's: JFN 4/24/13

   Have filter return a kStatus, and then decide what to do with the event (reject, flag, etc) in Make()
   Decide what has to be stored in the TTree: just the event num and status, every track and status, etc...
*/

class StarFilterMaker : public StMaker
{
 public:
  StarFilterMaker( const Char_t *name="" );
  ~StarFilterMaker() { /* nothing to see here */ };

  Int_t Init();
  Int_t Make();
  void  Clear( const Option_t *opts="" );
  Int_t Finish();

  /// The function Filter( StarGenEvent* ) is not defined here in the base class.
  /// Users who write filters (which will inherit from this class) will overwrite it.
  /// @param event Is the event to be filtered.  If no event is provided, then the event 
  ///        which was registered by SetEvent will be used.
  virtual Int_t Filter( StarGenEvent *event=0 ) = 0;

  void SetEvent( StarGenEvent *event ){ mEvent = event; }

  Int_t numberOfEvents(){ return NumberofEvents; }
  Int_t acceptedEvents(){ return AcceptedEvents; }
  Int_t rejectedEvents(){ return RejectedEvents; }
  Int_t rejectedSinceLast(){ return RejectedSinceLast; }

 private:

  Int_t NumberofEvents;
  Int_t AcceptedEvents;
  Int_t RejectedEvents;
  Int_t RejectedSinceLast;

 private:
 protected:
  ClassDef(StarFilterMaker,1);

  StarGenEvent *mEvent;

  //Options:
    Bool_t bFlag; // If false, failed tracks will be flaged but not rejected. Default:false

};
#endif
