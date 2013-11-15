// -*- mode: c++;-*-
// $Id: StBET4pMaker.h,v 1.12 2009/09/03 23:36:09 pibero Exp $
#ifndef STBET4PMAKER_H
#define STBET4PMAKER_H

#include "StFourPMaker.h"

class StMuDstMaker;
class StjTreeEntryMaker;
class StBET4pMakerImp;

class StjeBemcEnergySumCalculator;

class StjTrackListCut;
class StjTowerEnergyListCut;
class StjeTrackListToStMuTrackFourVecList;
class StjeTowerEnergyListToStMuTrackFourVecList;

class StjAbstractTowerEnergyCorrectionForTracks;

class StBET4pMaker : public StFourPMaker {

public:
    
  StBET4pMaker(const char* name, StMuDstMaker* maker, bool doTowerSwapFix = true, StjAbstractTowerEnergyCorrectionForTracks* correctTowerEnergyForTracks = 0);
    
  virtual ~StBET4pMaker() {};
    
  Int_t Init();    
  Int_t Make();
    
  void Clear(Option_t* opt = "");

  FourList &getTracks();

  void setUseTPC(bool v = true)      { _useTPC      = v; }
  void setUseBEMC(bool v = true)     { _useBEMC     = v; }
  void setUseEndcap(bool v = true)   { _useEEMC   = v; }
  void setUse2003Cuts(bool v = true) { _use2003Cuts = v; }
  void setUse2005Cuts(bool v = true) { _use2005Cuts = v; }
  void setUse2006Cuts(bool v = true) { _use2006Cuts = v; }
  void setUseBEMCEnergySum(bool v = true) { _useBEMCEnergySum = v; }
  void setUseBEMCEnergyVariation(bool v = true) { _useBEMCEnergyVariation = v; }
  void setBEMCEnergyVariationRatio(double ratio) {  _bemcEnergyVariationRatio= ratio; }

  int nDylanPoints() const;
  double sumEmcEt() const;

  bool bemcCorrupt() const { return isBemcCorrupted(); }

  StBET4pMakerImp* GetImp() { return _imp; }

  const char* GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StBET4pMaker.h,v 1.12 2009/09/03 23:36:09 pibero Exp $ built "__DATE__" "__TIME__; return cvs;}

private:

  StjTreeEntryMaker* _entryMaker;

  StMuDstMaker* _uDstMaker;
  bool _doTowerSwapFix;
  StjAbstractTowerEnergyCorrectionForTracks* _correctTowerEnergyForTracks;

  bool _useTPC;
  bool _useBEMC;
  bool _useEEMC;
  bool _use2003Cuts;
  bool _use2005Cuts;
  bool _use2006Cuts;
  bool _useBEMCEnergySum;
  bool _useBEMCEnergyVariation;
  double _bemcEnergyVariationRatio;


  StBET4pMakerImp* _imp;

  StjeBemcEnergySumCalculator* _bemcEnergySumCalculator;

  StjeTrackListToStMuTrackFourVecList& _track2four;
  StjeTowerEnergyListToStMuTrackFourVecList& _energy2four;
  FourList _tracks;

  bool isBemcCorrupted() const;

  ClassDef(StBET4pMaker,1)
};

#endif // STBET4PMAKER_H
