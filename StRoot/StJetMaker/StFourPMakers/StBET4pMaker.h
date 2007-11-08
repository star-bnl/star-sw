/*!
  \class StBET4pMaker
  \author M.L. Miller (MIT Software)

  StBET4pMaker is used to fill the list of 4-momenta that is then passed to a
  StJetFinder instance.  StBET4pMaker simply instantiates an object of type
  StBETMuTrackFourVec for every final state particle in the event.
*/

#ifndef StBET4pMaker_HH
#define StPYthia4pMaker_HH

#include <vector>
using namespace std;

class StMuTrackFourVec;
class StMuDstMaker;
class StEmcADCtoEMaker;
class StEmcRawHit;
class StMuEmcPosition;
class EEmcGeomSimple;
class StBemcTables;
class StEEmcDbMaker;

#include "StJetMaker/StFourPMakers/StFourPMaker.h"

class StBET4pMaker : public StFourPMaker
{
public:
    
    ///Require StMuDstMaker pointer at instantiation
    //StBET4pMaker(const char* name, StMuDstMaker* uDstMaker, StEmcADCtoEMaker* adc2e);
    StBET4pMaker(const char* name, StMuDstMaker* uDstMaker, bool doTowerSwapFix=true);
    
    ///Default destructor
    virtual ~StBET4pMaker() {};
    
    ///Fill the lists
    virtual Int_t Make();
    
    ///Clear the lists
    virtual void Clear(Option_t* opt);

    ///Initialize
    virtual Int_t Init();    
    Int_t InitRun(Int_t runId);


    void setUseEndcap(bool v) {mUseEndcap=v;}
    void setUse2003Cuts(bool v) {mUse2003Cuts=v;}
    void setUse2005Cuts(bool v) {mUse2005Cuts=v;}

    int nDylanPoints() const {return mDylanPoints;}
    double sumEmcEt() const {return mSumEmcEt;}
    
protected:
    void fillBarrelHits();

    typedef vector<StMuTrackFourVec*> BET4Vec;
    BET4Vec mVec;
    bool mCorrupt;
    bool mUseEndcap;
    double mField;

    //these arrays are used to correlate tracks w/ towers
    StEmcRawHit* mBTowHits[4801]; //!indexed from [1,4800]
    int mNtracksOnTower[4801]; //!indexed form [1,4800] (number of tracks incident on this tower)

    //utility used for track-> towe rprojection
    StMuEmcPosition*  mMuPosition; //!

    StMuDstMaker* mMuDstMaker; //!
    //StEmcADCtoEMaker* mAdc2E; //!
    StBemcTables* mTables;//!

    bool mUse2003Cuts; //!
    bool mUse2005Cuts; //!
    int mDylanPoints; //!
    double mSumEmcEt; //!
        
    EEmcGeomSimple* mEeGeom;
    StEEmcDbMaker* mEeDb;


    ClassDef(StBET4pMaker,1)
	};


#endif
