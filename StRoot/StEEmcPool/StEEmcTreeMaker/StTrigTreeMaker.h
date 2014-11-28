/*
 * Created by S. Gliske, Aug 2012
 *
 * Description: makes a tree with all the triggers per each event,
 * with the option of cutting on the (hardware) trigger.
 *
 */

#ifndef StTrigTreeMaker_H_
#define StTrigTreeMaker_H_

#include <set>
#include <Rtypes.h>
#include <TTree.h>
#include <TFile.h>

#include "StMaker.h"
#include "StRoot/StTriggerUtilities/StTriggerSimuMaker.h"

class TrigSet;

class StTrigTreeMaker_t : public StMaker {
 public:
   /// constructor
   StTrigTreeMaker_t( const Char_t *myName = "TrigTreeMaker", const Char_t* filenameOut = "trigTree.root", Bool_t isMC = 0, Bool_t useSimu = 1 );

   /// deconstructor
   virtual ~StTrigTreeMaker_t();

   /// Initialize
   Int_t Init();

   /// Build an event
   Int_t Make();

   /// Clear for next event
   void Clear(Option_t *opts="");

   /// Write everything to file
   Int_t Finish();

   void addTrigger( UInt_t trig );
   void removeTrigger( UInt_t trig );

   /// TODO: write copy constructor and equals operator.  Should not
   /// ever be used anyhow, but for completeness should eventually
   /// write them.

 protected:
   /// whether is Monte Carlo or data
   Bool_t mIsMC;

   /// whether to use MuDst (hardware) triggers or simulated (software) triggers
   Bool_t mUseSimuTrg;

   /// filenames
   std::string mFilename;

   /// TFiles/TTrees for writing
   TFile *mFile;
   TTree *mTree;

   // list of triggers for which to write out all triggers
   std::set< UInt_t > mTriggerSet;

   // the triggers to save to the root file
   TrigSet *mTrigSet;

 private:
   // for ROOT
   ClassDef( StTrigTreeMaker_t, 1 );
};

inline void StTrigTreeMaker_t::addTrigger( UInt_t trig ){ mTriggerSet.insert( trig ); };
inline void StTrigTreeMaker_t::removeTrigger( UInt_t trig ){ mTriggerSet.erase( trig ); };

/*
 * $Id: StTrigTreeMaker.h,v 1.1 2012/11/26 19:06:11 sgliske Exp $
 * $Log: StTrigTreeMaker.h,v $
 * Revision 1.1  2012/11/26 19:06:11  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/StEEmcTreeMaker to StRoot/StEEmcPool/StEEmcTreeMaker
 *
 *
 */

#endif
