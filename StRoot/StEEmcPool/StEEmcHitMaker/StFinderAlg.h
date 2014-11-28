/*!
 * \class StFinderAlg_t
 * \author Stephen Gliske <sgliske@anl.gov>
 *
 * Abstract base class for various "finder" algorithms.  Note: does
 * not exist any "action" methods such as "find" as these may have
 * different inputs for various children classes.
*/

// ROOT stuff
#include <Rtypes.h>


#ifndef _FINDER_ALG_H_
#define _FINDER_ALG_H_

class StFinderAlg_t {

 public:
    StFinderAlg_t() : mIsReady(0) { /* */ };
   virtual ~StFinderAlg_t() { /* */ };

   /// Check if status is ready for hit finding
   Bool_t isReady() const { return mIsReady; };

   /// clear things before doing the finding for the next event
   virtual void clear() = 0;

   static void setEventNum( Int_t eventNum ){ mEventNum=eventNum; };
   static Int_t getEventNum(){ return mEventNum; };

 protected:
   Bool_t mIsReady;

 private:
   static Int_t mEventNum;  //!

   ClassDef( StFinderAlg_t, 1 );
};

#endif

/*
 * $Id: StFinderAlg.h,v 1.1 2012/11/26 19:05:56 sgliske Exp $ 
 * $Log: StFinderAlg.h,v $
 * Revision 1.1  2012/11/26 19:05:56  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/StEEmcHitMaker to StRoot/StEEmcPool/StEEmcHitMaker
 *
 * 
 */
