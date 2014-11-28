// -*- mode: c++;-*-
// $Id: StBET4pMaker.h,v 1.19 2014/08/06 11:43:22 jeromel Exp $
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

  //    This method tells the maker to use a random selector when collecting
  //    tracks.
  //    v   Specifies whether or not the random selector should be used.  If
  //        true, then the random selector should be used.  If false, the
  //        random selector should not be used.
  void setUseRandomSelector(bool v = true) { _useRandomSelector = v; }

  //    This method specifies how the random selector should be built.
  //    newProb     The probability of randomly selecting a track for
  //                inclusion.  This is a ratio in the range of [0,1].
  //    newAt       Whether or not the specified probability should be an
  //                absolute threshold, so that exactly that ratio of tracks
  //                are selected.  If true, then the probability represents an
  //                absolute threshold.
  //    newSeed     The seed for the random number generator used in the
  //                RandomSelector.  Zero specifies that a random seed should
  //                be generated.  Keep in mind that if random seeds are
  //                generated too quickly by specifying "zero" that seeds may
  //                be identical.  Seeds should be generated using this method
  //                at second intervals.
  void setRandomSelector(
			 double newProb = 1.0,
			 bool newAt = false, 
    unsigned int newSeed = 0
			 )
  {
    _randomSelectorProb = newProb;
    _randomSelectorAt = newAt;
    _randomSelectorSeed = newSeed;
  }

  int nDylanPoints() const;
  double sumEmcEt() const;

  bool bemcCorrupt() const { return isBemcCorrupted(); }

  StBET4pMakerImp* GetImp() { return _imp; }

  const char* GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StBET4pMaker.h,v 1.19 2014/08/06 11:43:22 jeromel Exp $ built " __DATE__ " " __TIME__; return cvs;}

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
  bool _useRandomSelector;
  bool _useBEMCEnergyVariation;
  double _bemcEnergyVariationRatio;

  StBET4pMakerImp* _imp;
  StjeBemcEnergySumCalculator* _bemcEnergySumCalculator;
  StjeTrackListToStMuTrackFourVecList& _track2four;
  StjeTowerEnergyListToStMuTrackFourVecList& _energy2four;

  double _randomSelectorProb;
  bool _randomSelectorAt;
  unsigned int _randomSelectorSeed;

  bool isBemcCorrupted() const;

  ClassDef(StBET4pMaker,0)
};

#endif // STBET4PMAKER_H
