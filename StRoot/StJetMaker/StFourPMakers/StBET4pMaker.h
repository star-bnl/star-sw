// -*- mode: c++;-*-
// $Id: StBET4pMaker.h,v 1.37 2008/07/10 03:09:36 tai Exp $
#ifndef STBET4PMAKER_HH
#define STBET4PMAKER_HH

#include "StFourPMaker.h"

#include "CorrectTowerEnergyForTracks.h"

class StMuDstMaker;
class StBET4pMakerImp;

namespace StSpinJet {

class CollectChargedTracksFromTPC;
class BemcEnergySumCalculator;

class StJetTPC;
class StJetBEMC;
class StJetEEMC;

class StJetTPCTrackCut;
class StJetBEMCEnergyCut;

}

class StBET4pMaker : public StFourPMaker {

public:
    
  StBET4pMaker(const char* name, StMuDstMaker* uDstMaker, bool doTowerSwapFix = true);
    
  virtual ~StBET4pMaker() {};
    
  Int_t Init();    
  Int_t Make();
    
  void Clear(Option_t* opt = "");

  FourList &getTracks();

  void setUseTPC(bool v)      { _useTPC      = v; }
  void setUseBEMC(bool v)     { _useBEMC     = v; }
  void setUseEndcap(bool v)   { _useEndcap   = v; }
  void setUse2003Cuts(bool v) { _use2003Cuts = v; }
  void setUse2005Cuts(bool v) { _use2005Cuts = v; }
  void setUse2006Cuts(bool v) { _use2006Cuts = v; }

  int nDylanPoints() const;
  double sumEmcEt() const;

  bool bemcCorrupt() const { return isBemcCorrupted(); }


  StBET4pMakerImp* GetImp() { return _imp; }
  StSpinJet::StJetBEMCEnergyCut* getBEMCEnergyCut() { return _bemcCut; }
  StSpinJet::StJetTPCTrackCut* getTPCTrackCut() { return _tpcCut; }

  const char* GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StBET4pMaker.h,v 1.37 2008/07/10 03:09:36 tai Exp $ built "__DATE__" "__TIME__; return cvs;}

private:

  StMuDstMaker* _uDstMaker;
  bool _doTowerSwapFix;

  bool _useTPC;
  bool _useBEMC;
  bool _useEndcap;
  bool _use2003Cuts;
  bool _use2005Cuts;
  bool _use2006Cuts;

  StSpinJet::StJetTPC* _tpc;
  StSpinJet::StJetBEMC* _bemc;
  StSpinJet::StJetEEMC* _eemc;

  StSpinJet::StJetTPCTrackCut* _tpcCut;
  StSpinJet::StJetBEMCEnergyCut* _bemcCut;

  StSpinJet::CorrectTowerEnergyForTracks* _correctTowerEnergyForTracks;
  StBET4pMakerImp* _imp;

  StSpinJet::BemcEnergySumCalculator* _bemcEnergySumCalculator;

  

  bool isBemcCorrupted() const;

  ClassDef(StBET4pMaker,1)
};

#endif // STBET4PMAKER_HH
