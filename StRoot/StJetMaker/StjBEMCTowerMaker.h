// -*- mode: c++;-*-
// $Id: StjBEMCTowerMaker.h,v 1.2 2008/08/02 19:22:26 tai Exp $
#ifndef STJETBEMCTOWERMAKER_HH
#define STJETBEMCTOWERMAKER_HH

#include "StMaker.h"
#include <Rtypes.h>

class StjTowerEnergyListWriter;

class TDirectory;
class TTree;

class StMuDstMaker;

namespace StSpinJet {
  class StjBEMC;
  class StjTowerEnergyListCut;
}

class StjBEMCTowerMaker : public StMaker {

public:

  StjBEMCTowerMaker(const Char_t *name, TDirectory* file, StMuDstMaker* uDstMaker);
  virtual ~StjBEMCTowerMaker() { }

  Int_t Init();
  Int_t Make();
  Int_t Finish();
    
  const char* GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StjBEMCTowerMaker.h,v 1.2 2008/08/02 19:22:26 tai Exp $ built "__DATE__" "__TIME__; return cvs;}

private:

  TDirectory* _file;

  StMuDstMaker* _uDstMaker;

  StSpinJet::StjBEMC* _bemc;
  StSpinJet::StjTowerEnergyListCut* _bemcCut;

  StjTowerEnergyListWriter* _writer;

  ClassDef(StjBEMCTowerMaker, 0)

};

#endif // STJETBEMCTOWERMAKER_HH
