/*!
  \class StBET4pMaker
  \author M.L. Miller (MIT Software)

  StBET4pMaker is used to fill the list of 4-momenta that is then passed to a
  StJetFinder instance.  StBET4pMaker simply instantiates an object of type
  StBETMuTrackFourVec for every final state particle in the event.
*/

#ifndef STBET4PMAKER_HH
#define STBET4PMAKER_HH

#include "StFourPMaker.h"

class StEmcCollection;
class StMuTrackFourVec;
class StMuDstMaker;
class StEmcRawHit;
class EEmcGeomSimple;
class StBemcTables;
class StEEmcDbMaker;


class StBET4pMaker : public StFourPMaker {

public:
    
  StBET4pMaker(const char* name, StMuDstMaker* uDstMaker, bool doTowerSwapFix = true);
    
  virtual ~StBET4pMaker() {};
    
  Int_t Init();    
  Int_t Make();
    
  void Clear(Option_t* opt);

  Int_t InitRun(Int_t runId);

  void setUseEndcap(bool v) { mUseEndcap = v; }
  void setUse2003Cuts(bool v) { mUse2003Cuts = v; }
  void setUse2005Cuts(bool v) { mUse2005Cuts = v; }
  void setUse2006Cuts(bool v) { mUse2006Cuts = v; }

  int nDylanPoints() const { return mDylanPoints; }
  double sumEmcEt() const { return mSumEmcEt; }
    
private:

  void collectChargedTracksFromTPC();
  bool isUsableTrack(const StMuTrack& track) const;
  void countTracksOnBemcTower(const StMuTrack& track);

  void collectEnergyFromBEMC();
  double correctBemcTowerEnergyForTracks(double energy, int bemcTowerId);

  void collectEnergyFromEEMC();

  StEmcCollection *find_StEmCCollection();
  bool isCorrupted();

  void fillBemcTowerHits();
  double sumEnergyOverBemcTowers(double minE);
  int numberOfBemcTowersWithEnergyAbove(double minE);

  bool shouldKeepThisBemcHit(StEmcRawHit* theRawHit, int bemcTowerID);

  bool mCorrupt;
  bool mUseEndcap;

  //these arrays are used to correlate tracks w/ towers
  static const int mNOfBemcTowers = 4800;

  StEmcRawHit* mBTowHits[mNOfBemcTowers + 1]; // indexed from [1,4800]
  int mNtracksOnTower[mNOfBemcTowers + 1]; // indexed form [1,4800] (number of tracks incident on this tower)

  StMuDstMaker* mMuDstMaker;
  StBemcTables* mTables;

  bool mUse2003Cuts;
  bool mUse2005Cuts;
  bool mUse2006Cuts;
  int mDylanPoints;
  double mSumEmcEt;
        
  EEmcGeomSimple* mEeGeom;
  StEEmcDbMaker* mEeDb;
  
  ClassDef(StBET4pMaker,1)
};

#endif // STBET4PMAKER_HH
