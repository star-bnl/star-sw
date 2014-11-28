/*!
  \class StEvtTrigDetSumsMaker
  
  StEvtTrigDetSumsMaker populates the TrigDetSums table from StEvent.
  This allows the table to be filled with the same information when
  reading event.root files that was available when the files were
  created. In particular, database values of RICH scalers are slightly
  different. Using StEvtTrigDetSumsMaker also avoids the unnecessary
  overhead of going to the database for information already available.

  Note that trigDetSums is used by StDetectorDbRichScalers, which in
  turn is used by St_spaceChargeCorC, used by StMagUtilities. It
  should be placed before any use of StMagUtilities in the chain.

*/

#ifndef STAR_StEvtTrigDetSumsMaker
#define STAR_StEvtTrigDetSumsMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif

class StEvtTrigDetSumsMaker : public StMaker {
 public: 
  StEvtTrigDetSumsMaker(const char *name="EvtTrigDetSums") : StMaker(name) {}
  virtual       ~StEvtTrigDetSumsMaker() {}
  virtual Int_t  Make();
  
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StEvtTrigDetSumsMaker.h,v 1.3 2014/08/06 11:43:32 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
  
  ClassDef(StEvtTrigDetSumsMaker,0)
};

#endif

//_____________________________________________________________________________
// $Id: StEvtTrigDetSumsMaker.h,v 1.3 2014/08/06 11:43:32 jeromel Exp $
// $Log: StEvtTrigDetSumsMaker.h,v $
// Revision 1.3  2014/08/06 11:43:32  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.2  2012/10/15 17:38:34  genevb
// Add CVS logging
//
//

