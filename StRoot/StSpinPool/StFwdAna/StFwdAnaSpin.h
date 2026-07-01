/*
  AUTHOR
  David Kapukchyan

  PURPOSE
  The purpoe of this class is to get and save the spin bit information for the event

  DESCRIPTION
  Grabs the spin bit information from #StSpinDbMaker and saves it to #FcsEventInfo in #StMuFcsAnaData. If #StSpinDbMaker was not created will generate a random spin bit and save that
  
  LOG
  @[January 15, 2026] > First instance where relevant functionality was copied from #StMuFcsTreeMaker
  @[July 1, 2026] > Changed name from StMuFcsAnaSpin to StFwdAnaSpin

*/


#ifndef STFWDANA_STFWDANASPIN_HH
#define STFWDANA_STFWDANASPIN_HH

#include "StFwdAnaVirtual.h"

class StFwdAnaSpin : public StFwdAnaVirtual
{
public:
  StFwdAnaSpin();
  ~StFwdAnaSpin();

  virtual UInt_t LoadHists(TFile* file, HistManager* histman, StFwdAnaData* anadata);
  virtual Int_t DoMake(StFwdAnaData* anadata);
  
private:
  ClassDef(StFwdAnaSpin,1)
};

#endif

