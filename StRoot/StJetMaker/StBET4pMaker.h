// -*- mode: c++;-*-
// $Id: StBET4pMaker.h,v 1.16 2010/05/22 13:43:20 pibero Exp $
#ifndef STBET4PMAKER_H
#define STBET4PMAKER_H

class StMuDstMaker;
class StMuPrimaryVertex;
class StjTreeEntryMaker;
class StBET4pMakerImp;

class StjeBemcEnergySumCalculator;

class StjTrackListCut;
class StjTowerEnergyListCut;
class StjeTrackListToStMuTrackFourVecList;
class StjeTowerEnergyListToStMuTrackFourVecList;
class StjAbstractTowerEnergyCorrectionForTracks;

#include "StFourPMaker.h"

class StBET4pMaker : public StFourPMaker {

public:
    
  StBET4pMaker(const char* name, StMuDstMaker* maker, bool doTowerSwapFix = true, StjAbstractTowerEnergyCorrectionForTracks* correctTowerEnergyForTracks = 0);

  virtual ~StBET4pMaker() {}

  Int_t Init();
  Int_t Make();

  void Clear(Option_t* opt = "");

  const vector<VertexNode>& getVertexNodes() const { return _vertexNodes; }

  void setUseTPC(bool v = true)      { _useTPC      = v; }
  void setUseBEMC(bool v = true)     { _useBEMC     = v; }
  void setUseEndcap(bool v = true)   { _useEEMC   = v; }
  void setUse2003Cuts(bool v = true) { _use2003Cuts = v; }
  void setUse2005Cuts(bool v = true) { _use2005Cuts = v; }
  void setUse2006Cuts(bool v = true) { _use2006Cuts = v; }
  void setUse2009Cuts(bool v = true) { _use2009Cuts = v; }
  void setUseBEMCEnergySum(bool v = true) { _useBEMCEnergySum = v; }
  void setUseBEMCEnergyVariation(bool v = true) { _useBEMCEnergyVariation = v; }
  void setBEMCEnergyVariationRatio(double ratio) {  _bemcEnergyVariationRatio= ratio; }

  int nDylanPoints() const;
  double sumEmcEt() const;

  bool bemcCorrupt() const { return isBemcCorrupted(); }

  StBET4pMakerImp* GetImp() { return _imp; }

  const char* GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StBET4pMaker.h,v 1.16 2010/05/22 13:43:20 pibero Exp $ built "__DATE__" "__TIME__; return cvs;}

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
  bool _use2009Cuts;
  bool _useBEMCEnergySum;
  bool _useBEMCEnergyVariation;
  double _bemcEnergyVariationRatio;

  StBET4pMakerImp* _imp;
  StjeBemcEnergySumCalculator* _bemcEnergySumCalculator;
  StjeTrackListToStMuTrackFourVecList& _track2four;
  StjeTowerEnergyListToStMuTrackFourVecList& _energy2four;

  bool isBemcCorrupted() const;

  ClassDef(StBET4pMaker,0)
};

#endif // STBET4PMAKER_H
