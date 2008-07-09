// -*- mode: c++;-*-
// $Id: StBET4pMaker.h,v 1.34 2008/07/09 23:53:36 tai Exp $
#ifndef STBET4PMAKER_HH
#define STBET4PMAKER_HH

#include "StFourPMaker.h"

#include "CorrectTowerEnergyForTracks.h"
#include "StJetEEMCMuDst.h"

class StMuDstMaker;
class StBemcTables;
class StBET4pMakerImp;

namespace StSpinJet {

class CollectChargedTracksFromTPC;
class BemcEnergySumCalculator;

class StJetTPC;
class StJetTPCTrackCut;
class StJetBEMC;
class StJetBEMCEnergyCut;
class StJetEEMC;

}

class StBET4pMaker : public StFourPMaker {

public:
    
  StBET4pMaker(const char* name, StMuDstMaker* uDstMaker, bool doTowerSwapFix = true);
    
  virtual ~StBET4pMaker() {};
    
  Int_t Init();    
  Int_t Make();
    
  void Clear(Option_t* opt);

  Int_t InitRun(Int_t runId);

  FourList &getTracks();

  void setUseEndcap(bool v);
  void setUse2003Cuts(bool v);
  void setUse2005Cuts(bool v);
  void setUse2006Cuts(bool v);

  int nDylanPoints() const;
  double sumEmcEt() const;

  bool bemcCorrupt() const { return isBemcCorrupted(); }


  StBET4pMakerImp* GetImp() { return _imp; }
  StSpinJet::StJetBEMCEnergyCut* getBEMCEnergyCut() { return _bemcCut; }
  StSpinJet::StJetTPCTrackCut* getTPCTrackCut() { return _tpcCut; }

private:

  StSpinJet::StJetTPC* _tpc;
  StSpinJet::StJetTPCTrackCut* _tpcCut;

  StSpinJet::StJetEEMCMuDst* _eemc;
  StBemcTables* _bemcTables;

  StSpinJet::StJetBEMC* _bemc;
  StSpinJet::StJetBEMCEnergyCut* _bemcCut;

  StSpinJet::CorrectTowerEnergyForTracks* _correctTowerEnergyForTracks;
  StBET4pMakerImp* _imp;

  StSpinJet::BemcEnergySumCalculator* _bemcEnergySumCalculator;

  

  bool isBemcCorrupted() const;

  ClassDef(StBET4pMaker,1)
};

#endif // STBET4PMAKER_HH
