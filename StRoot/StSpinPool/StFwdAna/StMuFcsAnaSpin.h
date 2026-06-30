/*
  AUTHOR
  David Kapukchyan

  PURPOSE
  The purpoe of this class is to get and save the spin bit information for the event

  DESCRIPTION
  Grabs the spin bit information from #StSpinDbMaker and saves it to #FcsEventInfo in #StMuFcsAnaData. If #StSpinDbMaker was not created will generate a random spin bit and save that
  
  LOG
  @[January 15, 2026] > First instance where relevant functionality was copied from #StMuFcsTreeMaker

*/


#ifndef STMUFCSANASPIN_HH
#define STMUFCSANASPIN_HH

#include "StMuFcsVirtualAna.h"

class StMuFcsAnaSpin : public StMuFcsVirtualAna
{
public:
  StMuFcsAnaSpin();
  ~StMuFcsAnaSpin();

  virtual UInt_t LoadHists(TFile* file, HistManager* histman, StMuFcsAnaData* anadata);
  virtual Int_t DoMake(StMuFcsAnaData* mufcsdata);
  
private:
  ClassDef(StMuFcsAnaSpin,1)
};

#endif

