// -*- mode: c++;-*-
// $Id: StBET4pMakerImp.h,v 1.37 2008/06/10 08:31:08 tai Exp $
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

#include "CollectEnergyDepositsFromBEMC.h"
#include "CollectEnergyDepositsFromEEMC.h"

typedef std::vector<AbstractFourVec*> FourList;

class StBET4pMakerImp {

public:
    
  StBET4pMakerImp(StMuDstMaker* uDstMaker,
		  StSpinJet::CollectChargedTracksFromTPC* collectChargedTracksFromTPC,
		  StSpinJet::CollectEnergyDepositsFromBEMC *collectEnergyDepositsFromBEMC,
		  StSpinJet::CollectEnergyDepositsFromEEMC *collectEnergyDepositsFromEEMC
		  );
    
  virtual ~StBET4pMakerImp() {};
    
  void Make();
    
  void Clear(Option_t* opt);

  void setUseEndcap(bool v) { mUseEndcap = v; }

  FourList &getTracks() { return _tracks; };
  Int_t numTracks(void) { return _tracks.size(); };


private:

  typedef std::vector<std::pair<const StMuTrack*, int> > TrackList;

  FourList constructFourMomentumListFrom(const TrackList& trackList);

  void countTracksOnBemcTower(const StMuTrack& track);

  StSpinJet::TowerEnergyDepositList correctBemcTowerEnergyForTracks(const StSpinJet::TowerEnergyDepositList &energyDepositList, const TrackList& trackList);
  double correctBemcTowerEnergyForTracks_(double energy, int bemcTowerId);

  FourList constructFourMomentumListFrom(const StSpinJet::TowerEnergyDepositList& energyDepositList);


  TVector3 getVertex();

  TLorentzVector constructFourMomentum(const TVector3& towerLocation, double energy);

  FourList _tracks;

  bool mUseEndcap;

  //these arrays are used to correlate tracks w/ towers
  static const int mNOfBemcTowers = 4800;

  int mNtracksOnTower[mNOfBemcTowers + 1]; // indexed form [1,4800] (number of tracks incident on this tower)

  StMuDstMaker* mMuDstMaker;

  StSpinJet::CollectChargedTracksFromTPC *_collectChargedTracksFromTPC;
  StSpinJet::CollectEnergyDepositsFromBEMC *_collectEnergyDepositsFromBEMC;
  StSpinJet::CollectEnergyDepositsFromEEMC *_collectEnergyDepositsFromEEMC;

};

#endif // STBET4PMAKERIMP_HH
