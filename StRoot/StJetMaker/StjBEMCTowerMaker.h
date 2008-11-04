// -*- mode: c++;-*-
// $Id: StjBEMCTowerMaker.h,v 1.5 2008/11/04 05:54:35 tai Exp $
#ifndef STJBEMCTOWERMAKER_H
#define STJBEMCTOWERMAKER_H

#include "StMaker.h"
#include <Rtypes.h>

#include <StjTowerEnergyListCut.h>

class StjTowerEnergyListWriter;

class TDirectory;
class TTree;

class StMuDstMaker;

class StjBEMC;

class StjBEMCTowerMaker : public StMaker {

public:

  StjBEMCTowerMaker(const Char_t *name, TDirectory* file, StMuDstMaker* uDstMaker);
  virtual ~StjBEMCTowerMaker() { }

  Int_t Init();
  Int_t Make();
  Int_t Finish();
    
  const char* GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StjBEMCTowerMaker.h,v 1.5 2008/11/04 05:54:35 tai Exp $ built "__DATE__" "__TIME__; return cvs;}

private:

  TDirectory* _file;

  StMuDstMaker* _uDstMaker;

  StjBEMC* _bemc;
  StjTowerEnergyListCut* _bemcCut;

  StjTowerEnergyListWriter* _writer;

  ClassDef(StjBEMCTowerMaker, 0)

};

#endif // STJBEMCTOWERMAKER_H
