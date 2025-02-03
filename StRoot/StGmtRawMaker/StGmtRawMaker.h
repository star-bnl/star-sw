// \class StGmtRawMaker
// \authors K.S. Engle and Richard Witt (witt@usna.edu)
// based on StFgtRawMaker

#ifndef STAR_StGmtRawMaker_HH
#define STAR_StGmtRawMaker_HH

// StRoot headers
#include "StChain/StRTSBaseMaker.h"

// ROOT headers
#include <cmath>

// Forward declaration
class StGmtCollection;

/**
   This is the raw maker for the GMT data. It makes use of its base class functions to read daq 
   files into the StGmtEvent Data structure.
*/
class StGmtRawMaker : public StRTSBaseMaker {
  public:
    StGmtRawMaker(const Char_t *name = "GmtRaw") : StRTSBaseMaker("adc", name), mGmtCollectionPtr(0) {}
    ~StGmtRawMaker() {}
    Int_t Make();

  protected:
    Int_t fillHits();
    Int_t prepareEnvironment();
    StGmtCollection *mGmtCollectionPtr;

  private:
    ClassDef(StGmtRawMaker, 0)
};
#endif
