// -*- mode: c++;-*-
// $Id: StBET4pMaker.h,v 1.39 2008/07/10 09:36:00 tai Exp $
#ifndef STBET4PMAKER_HH
#define STBET4PMAKER_HH

#include "StFourPMaker.h"

class StMuDstMaker;
class StBET4pMakerImp;

namespace StSpinJet {

class BemcEnergySumCalculator;

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
  void setUseEndcap(bool v)   { _useEEMC   = v; }
  void setUse2003Cuts(bool v) { _use2003Cuts = v; }
  void setUse2005Cuts(bool v) { _use2005Cuts = v; }
  void setUse2006Cuts(bool v) { _use2006Cuts = v; }

  int nDylanPoints() const;
  double sumEmcEt() const;

  bool bemcCorrupt() const { return isBemcCorrupted(); }


  StBET4pMakerImp* GetImp() { return _imp; }

  const char* GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StBET4pMaker.h,v 1.39 2008/07/10 09:36:00 tai Exp $ built "__DATE__" "__TIME__; return cvs;}

private:

  StMuDstMaker* _uDstMaker;
  bool _doTowerSwapFix;

  bool _useTPC;
  bool _useBEMC;
  bool _useEEMC;
  bool _use2003Cuts;
  bool _use2005Cuts;
  bool _use2006Cuts;

  StBET4pMakerImp* _imp;

  StSpinJet::BemcEnergySumCalculator* _bemcEnergySumCalculator;

  

  bool isBemcCorrupted() const;

  ClassDef(StBET4pMaker,1)
};

#endif // STBET4PMAKER_HH
