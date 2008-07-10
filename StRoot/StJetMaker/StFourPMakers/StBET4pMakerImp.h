// -*- mode: c++;-*-
// $Id: StBET4pMakerImp.h,v 1.57 2008/07/10 19:35:30 tai Exp $
#ifndef STBET4PMAKERIMP_HH
#define STBET4PMAKERIMP_HH


#include "CorrectTowerEnergyForTracks.h"

#include "StJetFinder/AbstractFourVec.h"

#include <Rtypes.h>
#include <TVector3.h>
#include <TLorentzVector.h>

#include <map>


namespace StSpinJet {
  class StMuTrackEmu;
  class StJetTPC;
  class StJetBEMC;
  class StJetEEMC;
  class StJetTPCTrackCut;
  class StJetBEMCEnergyCut;
  class TrackListToFourList;
  class EnergyListToFourList;
}


typedef std::vector<AbstractFourVec*> FourList;

class StBET4pMakerImp {

public:
    
  StBET4pMakerImp(StSpinJet::StJetTPC* tpc,
		  StSpinJet::StJetTPCTrackCut* tpcCut,
		  StSpinJet::StJetBEMC* bemc,
		  StSpinJet::StJetBEMCEnergyCut* bemcCut,
		  StSpinJet::CorrectTowerEnergyForTracks* correctTowerEnergyForTracks,
		  StSpinJet::StJetEEMC* eemc);
    
  virtual ~StBET4pMakerImp() { };

  void Init();
  void Make();
    
  void Clear(Option_t* opt = "");

  FourList &getTracks() { return _tracks; };
  Int_t numTracks(void) { return _tracks.size(); };


  bool UseTPC()  const;
  bool UseBEMC() const;
  bool UseEEMC() const;

  StSpinJet::StJetTPCTrackCut* getTPCTrackCut()     { return _tpcCut; }
  StSpinJet::StJetBEMCEnergyCut* getBEMCEnergyCut() { return _bemcCut; }

private:

  typedef std::vector<StSpinJet::StMuTrackEmu*> TrackList;

  FourList _tracks;

  StSpinJet::StJetTPC*  _tpc;
  StSpinJet::StJetBEMC* _bemc;
  StSpinJet::StJetEEMC* _eemc;

  StSpinJet::StJetTPCTrackCut* _tpcCut;
  StSpinJet::StJetBEMCEnergyCut* _bemcCut;

  StSpinJet::CorrectTowerEnergyForTracks* _correctTowerEnergyForTracks;

  StSpinJet::TrackListToFourList& _track2four;
  StSpinJet::EnergyListToFourList& _energy2four;
};

#endif // STBET4PMAKERIMP_HH
