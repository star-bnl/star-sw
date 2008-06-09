// -*- mode: c++;-*-
// $Id: StBET4pMakerImp.h,v 1.29 2008/06/09 23:00:51 tai Exp $
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
class StEmcCollection;
class StMuTrackFourVec;
class StMuDstMaker;
class StEmcRawHit;
class EEmcGeomSimple;
class StBemcTables;
class StEEmcDbMaker;
class StEvent;

typedef std::vector<AbstractFourVec*> FourList;

class StBET4pMakerImp {

public:
    
  StBET4pMakerImp(StMuDstMaker* uDstMaker, StBemcTables* bemcTables);
    
  virtual ~StBET4pMakerImp() {};
    
  void Init(StEEmcDbMaker* eedb);
  void Make();
    
  void Clear(Option_t* opt);

  void setUseEndcap(bool v) { mUseEndcap = v; }
  void setUse2003Cuts(bool v) { mUse2003Cuts = v; }
  void setUse2005Cuts(bool v) { mUse2005Cuts = v; }
  void setUse2006Cuts(bool v);

  int nDylanPoints() const { return mDylanPoints; }
  double sumEmcEt() const { return mSumEmcEt; }

  FourList &getTracks() { return _tracks; };
  Int_t numTracks(void) { return _tracks.size(); };


private:

  typedef std::vector<std::pair<const StMuTrack*, int> > TrackList;

  FourList constructFourMomentumListFrom(const TrackList& trackList);

  struct TowerEnergyDeposit {
    StDetectorId detectorId;
    int towerId;
    TVector3 towerLocation;
    double energy;
  };

  typedef std::vector<TowerEnergyDeposit> TowerEnergyDepositList;

  typedef int BemcTowerID;
  typedef std::map<BemcTowerID, const StEmcRawHit*> BemcTowerIdHitMap;

  BemcTowerIdHitMap getTowerHitsFromBEMC();
  BemcTowerIdHitMap selectBemcTowerHits(const BemcTowerIdHitMap &bemcTowerHits);

  bool shouldKeepThisBemcHit(const StEmcRawHit* theRawHit, int bemcTowerID);

  void countTracksOnBemcTower(const StMuTrack& track);

  double sumEnergyOverBemcTowers(double minE, const TowerEnergyDepositList &energyDepositList);
  int numberOfBemcTowersWithEnergyAbove(double minE, const TowerEnergyDepositList &energyDepositList);


  TowerEnergyDepositList readBemcTowerEnergy(const BemcTowerIdHitMap &bemcTowerHits);
  TowerEnergyDepositList correctBemcTowerEnergyForTracks(const TowerEnergyDepositList &energyDepositList, const TrackList& trackList);
  double correctBemcTowerEnergyForTracks_(double energy, int bemcTowerId);

  FourList constructFourMomentumListFrom(const TowerEnergyDepositList& energyDepositList);

  TowerEnergyDepositList collectEnergyFromEEMC();

  bool accept2003Tower(int id);

  TVector3 getBemcTowerLocation(int bemcTowerId);
  TVector3 getVertex();

  TLorentzVector constructFourMomentum(const TVector3& towerLocation, double energy);

  FourList _tracks;

  bool mUseEndcap;

  //these arrays are used to correlate tracks w/ towers
  static const int mNOfBemcTowers = 4800;

  int mNtracksOnTower[mNOfBemcTowers + 1]; // indexed form [1,4800] (number of tracks incident on this tower)

  StMuDstMaker* mMuDstMaker;
  StBemcTables* _bemcTables;

  bool mUse2003Cuts;
  bool mUse2005Cuts;
  bool mUse2006Cuts;
  int mDylanPoints;
  double mSumEmcEt;
        
  EEmcGeomSimple* mEeGeom;
  StEEmcDbMaker* mEeDb;

  StSpinJet::CollectChargedTracksFromTPC *_collectChargedTracksFromTPC;
  
};

#endif // STBET4PMAKERIMP_HH
