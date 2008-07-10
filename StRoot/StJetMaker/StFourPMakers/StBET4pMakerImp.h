// -*- mode: c++;-*-
// $Id: StBET4pMakerImp.h,v 1.50 2008/07/10 01:56:08 tai Exp $
#ifndef STBET4PMAKERIMP_HH
#define STBET4PMAKERIMP_HH


#include <Rtypes.h>

#include "StJetFinder/AbstractFourVec.h"

#include "CollectChargedTracksFromTPC.h"

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

#include "CollectEnergyDepositsFromBEMC.h"
#include "CorrectTowerEnergyForTracks.h"

typedef std::vector<AbstractFourVec*> FourList;

class StBET4pMakerImp {

public:
    
  StBET4pMakerImp(
		  StSpinJet::CollectChargedTracksFromTPC* collectChargedTracksFromTPC,
		  StSpinJet::CollectEnergyDepositsFromBEMC* collectEnergyDepositsFromBEMC,
		  StSpinJet::CorrectTowerEnergyForTracks* correctTowerEnergyForTracks,
		  StSpinJet::StJetEEMC* eemc
		  );
    
  StBET4pMakerImp(
		  StSpinJet::StJetTPC* tpc,
		  StSpinJet::StJetTPCTrackCut* tpcCut,
		  StSpinJet::StJetBEMC* bemc,
		  StSpinJet::StJetBEMCEnergyCut* bemcCut,
		  StSpinJet::CorrectTowerEnergyForTracks* correctTowerEnergyForTracks,
		  StSpinJet::StJetEEMC* eemc
		  );
    
  virtual ~StBET4pMakerImp() {};
    
  void Make();
    
  void Clear(Option_t* opt = "");

  void setUseEndcap(bool v) { mUseEndcap = v; }
  void setUseBEMC(bool v) { mUseBEMC = v; }

  FourList &getTracks() { return _tracks; };
  Int_t numTracks(void) { return _tracks.size(); };


  bool UseEEMC() const { return mUseEndcap; }

private:

  typedef std::vector<StSpinJet::StMuTrackEmu*> TrackList;

  FourList _tracks;

  bool mUseEndcap;
  bool mUseBEMC;

  StSpinJet::CollectChargedTracksFromTPC *_collectChargedTracksFromTPC;
  StSpinJet::CollectEnergyDepositsFromBEMC *_collectEnergyDepositsFromBEMC;
  StSpinJet::StJetEEMC* _eemc;

  StSpinJet::CorrectTowerEnergyForTracks* _correctTowerEnergyForTracks;

  StSpinJet::TrackListToFourList& _track2four;
  StSpinJet::EnergyListToFourList& _energy2four;
};

#endif // STBET4PMAKERIMP_HH
