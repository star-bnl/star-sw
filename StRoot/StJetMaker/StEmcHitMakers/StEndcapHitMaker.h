//StEndcapHitMaker.h
//R. Fatemi (IUCF)
//9/04

#ifndef StEndcapHitMaker_HH
#define StEndcapHitMaker_HH

#include "StMaker.h"
#include "StJetMaker/StEmcHitMakers/EemcHit.h"
#include <vector>
#include <cmath>
using namespace std;

class StMuDstMaker;
class StEEmcDbMaker;
class StMuEmcCollection;
class EEmcGeomSimple;
class StMuDstMaker;

class StEndcapHitMaker : public StMaker
{
public:
    StEndcapHitMaker(const char* name, StMuDstMaker*);
    virtual ~StEndcapHitMaker() {};

    virtual Int_t Init();
    virtual void Clear(Option_t*);
    virtual Int_t Make();
    virtual Int_t Finish();

    typedef vector<EemcHit> EemcHitVec;
    EemcHitVec& hits() {return mHits;}
    
protected:
    StMuDstMaker* mMuDst;
    StEEmcDbMaker* mEeDb;
    StMuEmcCollection* mMuEmcCol;
    EEmcGeomSimple* mEeGeom;
    
    double mSumEEMC;//total energy in EEMC
    double mHitEnergy;//energy tower must have to be considered a hit
    int mNumHitTow;//number of towers above hit threshold
    bool mPrint;
    bool mSimu;
    EemcHitVec mHits;
    
protected:
    void findHits();
    double getTheta(double eta) const;
    double getEta(double theta) const;
    double getZVertex();

    ClassDef(StEndcapHitMaker,1)
	};

//inlines
inline double StEndcapHitMaker::getTheta(double eta) const
{
    return 2*atan(exp(-eta));
}
    
inline double StEndcapHitMaker::getEta(double theta) const
{
    return -log(tan(theta/2));
}


#endif
