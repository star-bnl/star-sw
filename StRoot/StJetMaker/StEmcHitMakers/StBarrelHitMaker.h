//StBarrelHitMaker.h
//R. Fatemi (IUCF)
//9/04

#ifndef StBarrelHitMaker_HH
#define StBarrelHitMaker_HH

#include "StMaker.h"
#include "StJetMaker/StEmcHitMakers/BemcHit.h"
#include <vector>
#include <cmath>
using namespace std;

class StMuDstMaker;
class StEEmcDbMaker;
class StMuEmcCollection;
class EEmcGeomSimple;
class StMuDstMaker;

class StBarrelHitMaker : public StMaker
{
public:
    StBarrelHitMaker(const char* name);
    virtual ~StBarrelHitMaker() {};

    virtual Int_t Init();
    //!virtual Int_t InitRun(Int_t);
    virtual void Clear(Option_t*);
    virtual Int_t Make();
    virtual Int_t Finish();

    typedef vector<BemcHit> BemcHitVec;
    BemcHitVec& hits() {return mHits;}
    
protected:
    StMuDstMaker* mMuDst;
    
    double mSumEEMC;//total energy in EMC
    double mHitEnergy;//energy tower must have to be considered a hit
    int mNumHitTow;//number of towers above hit threshold
    bool mPrint;
    BemcHitVec mHits;
    
protected:
    void findHits();

    ClassDef(StBarrelHitMaker,1)
	};

#endif
