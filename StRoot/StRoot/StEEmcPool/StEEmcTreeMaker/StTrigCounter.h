/*
 * Created by S. Gliske, May 2012
 *
 * Description: count events matching a single, given trigger (most likely mb).
 *
 */

#ifndef StTrigCounter_H_
#define StTrigCounter_H_

#include <set>
#include <Rtypes.h>
#include "StMaker.h"

class StTrigCounter : public StMaker {
 public:
   /// constructor
   StTrigCounter( const Char_t *myName, Int_t trigID = 117001 );

   /// deconstructor
   virtual ~StTrigCounter();

   /// Initialize
   Int_t Init();

   /// Build an event
   Int_t Make();

   /// Clear for next event
   void Clear(Option_t *opts="");

   /// Write everything to file
   Int_t Finish();

   /// monitering status
   void printStatus();

   /// get values
   UInt_t getTrigID() const;
   Int_t getNumEventsTot() const;
   Int_t getNumEventsInCut() const;

   /// TODO: write copy constructor and equals operator.  Should not
   /// ever be used anyhow, but for completeness should eventually
   /// write them.

 protected:
   UInt_t mTrigID;
   Int_t mNumEvents, mNumEventsInCut;

 private:
   // for ROOT
   ClassDef( StTrigCounter, 1 );
};

#endif

/*
 * $Id: StTrigCounter.h,v 1.2 2013/02/28 23:37:22 sgliske Exp $
 * $Log: StTrigCounter.h,v $
 * Revision 1.2  2013/02/28 23:37:22  sgliske
 * Updated so result of StTrigCounter gets saved in EEmcTree Part1
 * rather than just being output to the console (log file)
 *
 * Revision 1.1  2012/12/17 20:58:56  sgliske
 * first added to official StEEmcPool
 *
 *
 */
