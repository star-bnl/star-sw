/*!\class StEmcOldFinder
\author Alexandre A. P. Suaide
 
This is the original BEMC cluster finder
algorithm developed by Subhasis and Aleksei
Pavlinov. It was ported to this new framework.
 
*/
#ifndef STAR_StEmcOldFinder
#define STAR_StEmcOldFinder

#include "StEmcVirtualFinder.h"
#include "TList.h"
#include "TArray.h"
#include "StEmcPreClusterCollection.h"
#include "StEmcUtil/others/emcInternalDef.h"
#include "StEmcRawMaker/defines.h"

class StEvent;
class StEmcDetector;
class StEmcModule;

class StEmcOldFinder : public StEmcVirtualFinder
{
private:

protected:
    Float_t        mEnergySeed[MAXDETBARREL];
    Float_t        mEnergyAdd[MAXDETBARREL];
    Float_t        mEnergyThresholdAll[MAXDETBARREL];
    Int_t          mSizeMax[MAXDETBARREL];

    TArrayF        mEnergy;
    TArrayI        mEW;
    TArrayI        mSW;
    TArrayI        mUsed;
    TArrayI        mHitsId;
    Int_t          mNHit;
    Int_t          mNH;
    Int_t          mFirst;
    Int_t          mLast;
    Int_t          mKeyEta;
    Int_t          mKeyPhi;
    Int_t          mEtaFirst;
    Int_t          mEtaLast;
    Int_t          mPhiFirst;
    Int_t          mPhiLast;
    Int_t          mSecond;
    Int_t          mEtaSeed;
    Int_t          mKeyDir;
    Int_t          mOverlapFlag;
    Float_t        mEnergyLast;
    Float_t        mEnergyFirst;

    Bool_t         findClustersInDetector(StEmcDetector*); ///< finds clusters in a given detector
    Bool_t         findClustersInModule(Int_t, StEmcModule*); ///< finds clusters in a BEMC module
    Bool_t         testOnNeighbor(Int_t, Int_t); ///< test for hits in neighbor strips

public:
    StEmcOldFinder();
    virtual        ~StEmcOldFinder();

    virtual Bool_t findClusters(StEvent*);

    void   setEnergySeed(Int_t det, Float_t a)
    {
        mEnergySeed[det-1] = a;
    } ///< sets the energy seed. Energy seed is the minimum energy a hit should have to start looking for a cluster in that region
    void   setEnergyAdd(Int_t det, Float_t a)
    {
        mEnergyAdd[det-1] = a;
    } ///< sets the energy add. Energy add is the minimum energy a hit should have to be include in a cluster
    void   setEnergyThresholdAll(Int_t det, Float_t a)
    {
        mEnergyThresholdAll[det-1] = a;
    } ///< sets the energy threshold. Energy threshold is the minimum energy a cluster should have to be saved
    void   setSizeMax(Int_t det, Int_t a)
    {
        mSizeMax[det-1] = a;
    }///< sets the size max. Size max is the maximum number of hits a cluster can have

    Float_t energySeed(Int_t det)
    {
        return mEnergySeed[det-1];
    } ///< returns the energy seed value for a given detector
    Float_t energyAdd(Int_t det)
    {
        return mEnergyAdd[det-1];
    } ///< returns the energy add value for a given detector
    Float_t energyThresholdAll(Int_t det)
    {
        return mEnergyThresholdAll[det-1];
    } ///< returns the energy threshold for a given detector
    Int_t   sizeMax(Int_t det)
    {
        return mSizeMax[det-1];
    } ///< returns the size max for a given detector



    ClassDef(StEmcOldFinder,1)
};

#endif
