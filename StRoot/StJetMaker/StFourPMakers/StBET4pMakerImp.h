// -*- mode: c++;-*-
// $Id: StBET4pMakerImp.h,v 1.41 2008/07/07 20:35:17 tai Exp $
#ifndef STBET4PMAKERIMP_HH
#define STBET4PMAKERIMP_HH


#include <Rtypes.h>

#include "../StMuTrackFourVec.h"
#include "StJetFinder/AbstractFourVec.h"

#include "CollectChargedTracksFromTPC.h"

#include <TVector3.h>
#include <TLorentzVector.h>

#include <map>

class StMuTrack;
class StMuTrackFourVec;
class StMuDstMaker;

namespace StSpinJet {
  class StMuTrackEmu;
}

#include "CollectEnergyDepositsFromBEMC.h"
#include "CollectEnergyDepositsFromEEMC.h"
#include "CorrectTowerEnergyForTracks.h"

typedef std::vector<AbstractFourVec*> FourList;

class StBET4pMakerImp {

public:
    
  StBET4pMakerImp(StMuDstMaker* uDstMaker,
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


  TVector3 getVertex();

  TLorentzVector constructFourMomentum(const TVector3& towerLocation, double energy);

  FourList _tracks;

  bool mUseEndcap;
  bool mUseBEMC;

  StMuDstMaker* mMuDstMaker;

  StSpinJet::CollectChargedTracksFromTPC *_collectChargedTracksFromTPC;
  StSpinJet::CollectEnergyDepositsFromBEMC *_collectEnergyDepositsFromBEMC;
  StSpinJet::CollectEnergyDepositsFromEEMC *_collectEnergyDepositsFromEEMC;

  StSpinJet::CorrectTowerEnergyForTracks* _correctTowerEnergyForTracks;

};

#endif // STBET4PMAKERIMP_HH
