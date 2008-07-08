// -*- mode: c++;-*-
// $Id: StBET4pMakerImp.h,v 1.44 2008/07/08 11:21:57 tai Exp $
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
}

#include "CollectEnergyDepositsFromBEMC.h"
#include "CollectEnergyDepositsFromEEMC.h"
#include "CorrectTowerEnergyForTracks.h"

typedef std::vector<AbstractFourVec*> FourList;

class StBET4pMakerImp {

public:
    
  StBET4pMakerImp(
		  StSpinJet::CollectChargedTracksFromTPC* collectChargedTracksFromTPC,
		  StSpinJet::CollectEnergyDepositsFromBEMC* collectEnergyDepositsFromBEMC,
		  StSpinJet::CollectEnergyDepositsFromEEMC* collectEnergyDepositsFromEEMC,
		  StSpinJet::CorrectTowerEnergyForTracks* correctTowerEnergyForTracks
		  );
    
  virtual ~StBET4pMakerImp() {};
    
  void Make();
    
  void Clear(Option_t* opt);

  void setUseEndcap(bool v) { mUseEndcap = v; }
  void setUseBEMC(bool v) { mUseBEMC = v; }

  FourList &getTracks() { return _tracks; };
  Int_t numTracks(void) { return _tracks.size(); };


private:

  typedef std::vector<StSpinJet::StMuTrackEmu*> TrackList;

  FourList constructFourMomentumListFrom(const TrackList& trackList);

  FourList constructFourMomentumListFrom(const StSpinJet::TowerEnergyDepositList& energyDepositList);


  TLorentzVector constructFourMomentum(const StSpinJet::TowerEnergyDeposit& deposit);

  FourList _tracks;

  bool mUseEndcap;
  bool mUseBEMC;

  StSpinJet::CollectChargedTracksFromTPC *_collectChargedTracksFromTPC;
  StSpinJet::CollectEnergyDepositsFromBEMC *_collectEnergyDepositsFromBEMC;
  StSpinJet::CollectEnergyDepositsFromEEMC *_collectEnergyDepositsFromEEMC;

  StSpinJet::CorrectTowerEnergyForTracks* _correctTowerEnergyForTracks;

};

#endif // STBET4PMAKERIMP_HH
