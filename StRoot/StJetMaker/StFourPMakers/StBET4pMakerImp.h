// -*- mode: c++;-*-
// $Id: StBET4pMakerImp.h,v 1.34 2008/06/10 06:08:22 tai Exp $
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
class EEmcGeomSimple;
class StEEmcDbMaker;
class StEvent;

#include "CollectEnergyDepositsFromBEMC.h"


typedef std::vector<AbstractFourVec*> FourList;

class StBET4pMakerImp {

public:
    
  StBET4pMakerImp(StMuDstMaker* uDstMaker, StSpinJet::CollectChargedTracksFromTPC* collectChargedTracksFromTPC,  StSpinJet::CollectEnergyDepositsFromBEMC *collectEnergyDepositsFromBEMC);
    
  virtual ~StBET4pMakerImp() {};
    
  void Init(StEEmcDbMaker* eedb);
  void Make();
    
  void Clear(Option_t* opt);

  void setUseEndcap(bool v) { mUseEndcap = v; }
  //  void setUse2003Cuts(bool v);
  //  void setUse2005Cuts(bool v);
  //  void setUse2006Cuts(bool v);

  FourList &getTracks() { return _tracks; };
  Int_t numTracks(void) { return _tracks.size(); };


private:

  typedef std::vector<std::pair<const StMuTrack*, int> > TrackList;

  FourList constructFourMomentumListFrom(const TrackList& trackList);

  void countTracksOnBemcTower(const StMuTrack& track);

  StSpinJet::TowerEnergyDepositList correctBemcTowerEnergyForTracks(const StSpinJet::TowerEnergyDepositList &energyDepositList, const TrackList& trackList);
  double correctBemcTowerEnergyForTracks_(double energy, int bemcTowerId);

  FourList constructFourMomentumListFrom(const StSpinJet::TowerEnergyDepositList& energyDepositList);

  StSpinJet::TowerEnergyDepositList collectEnergyFromEEMC();


  TVector3 getVertex();

  TLorentzVector constructFourMomentum(const TVector3& towerLocation, double energy);

  FourList _tracks;

  bool mUseEndcap;

  //these arrays are used to correlate tracks w/ towers
  static const int mNOfBemcTowers = 4800;

  int mNtracksOnTower[mNOfBemcTowers + 1]; // indexed form [1,4800] (number of tracks incident on this tower)

  StMuDstMaker* mMuDstMaker;

  EEmcGeomSimple* mEeGeom;
  StEEmcDbMaker* mEeDb;

  StSpinJet::CollectChargedTracksFromTPC *_collectChargedTracksFromTPC;
  StSpinJet::CollectEnergyDepositsFromBEMC *_collectEnergyDepositsFromBEMC;

};

#endif // STBET4PMAKERIMP_HH
