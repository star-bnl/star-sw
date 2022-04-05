#include <StMaker.h>

#include "StjBEMCMuDst.h"

#include "StjTowerEnergyListCut.h"
#include "StjTowerEnergyCut2003BemcTower.h"
#include "StjTowerEnergyCutBemcWestOnly.h"
#include "StjTowerEnergyCutEnergy.h"
#include "StjTowerEnergyCutBemcStatus.h"
#include "StjTowerEnergyCutAdc.h"
#include "StjTowerEnergyCutTowerId.h"

#include "StjTowerEnergyListWriter.h"

#include <TDirectory.h>

class StjWestBEMCTowerEnergyListMaker : public StMaker {

public:
  
  StjWestBEMCTowerEnergyListMaker(const Char_t *name, TDirectory* file, StMuDstMaker* uDstMaker)
  : StMaker(name), _file(file), _uDstMaker(uDstMaker) { }
  virtual ~StjWestBEMCTowerEnergyListMaker() { }

  const char* GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StjWestBEMCTowerEnergyListMaker.C,v 1.4 2014/08/06 11:43:24 jeromel Exp $ built " __DATE__ " " __TIME__; return cvs;}

private:

  TDirectory* _file;

  StMuDstMaker* _uDstMaker;

  StjBEMC* _bemc;
  StjTowerEnergyListCut _bemcCut;

  StjTowerEnergyListWriter* _writer;

public:

  Int_t Init()
  {
    _bemc = new StjBEMCMuDst(_uDstMaker, true);

    _bemcCut.addCut(new StjTowerEnergyCutBemcWestOnly());
    _bemcCut.addCut(new StjTowerEnergyCutEnergy(0.0));
    _bemcCut.addCut(new StjTowerEnergyCutBemcStatus(1));
    _bemcCut.addCut(new StjTowerEnergyCutAdc(0, 2.0));
    _bemcCut.addCut(new StjTowerEnergyCutTowerId(1048));

    _writer = new StjTowerEnergyListWriter("bemcTowers", _file);

    return kStOk;
  }

  Int_t Make()
  {
    StjTowerEnergyList energyList = _bemc->getEnergyList();

    energyList = _bemcCut(energyList);

    _writer->Fill(energyList);

    return kStOk;
  }

  Int_t Finish()
  {
    _writer->Finish();

    return kStOk;
  }

  ClassDef(StjWestBEMCTowerEnergyListMaker, 1)
};
