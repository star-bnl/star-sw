//StJetSkimEvent.cxx
//M.L. Miller (MIT)
//12/06

#include <iostream>

//local
#include "StPythiaEvent.h"
#include "StJetSkimEvent.h"

ClassImp(StJetSkimTrigHeader)
ClassImp(StJetSkimTrig)
ClassImp(StJetSkimVert)
ClassImp(StJetSkimEvent)

StJetSkimTrigHeader::StJetSkimTrigHeader() {
    init();
}

StJetSkimTrigHeader::StJetSkimTrigHeader(const StJetSkimTrigHeader & t) {
    this->runId                            = t.runId;
    this->trigId                           = t.trigId;
    this->prescale                         = t.prescale;
    this->eastBarrelTowerThreshold         = t.eastBarrelTowerThreshold;
    this->eastBarrelTriggerPatchThreshold  = t.eastBarrelTriggerPatchThreshold;
    this->eastBarrelJetPatchThreshold      = t.eastBarrelJetPatchThreshold;
    this->westBarrelTowerThreshold         = t.westBarrelTowerThreshold;
    this->westBarrelTriggerPatchThreshold  = t.westBarrelTriggerPatchThreshold;
    this->westBarrelJetPatchThreshold      = t.westBarrelJetPatchThreshold;
    this->endcapTowerThreshold             = t.endcapTowerThreshold;
    this->endcapTriggerPatchThreshold      = t.endcapTriggerPatchThreshold;
    this->endcapJetPatchThreshold          = t.endcapJetPatchThreshold;
    this->totalEnergyThreshold             = t.totalEnergyThreshold;
}

StJetSkimTrigHeader::~StJetSkimTrigHeader() { /*no-op*/ }

StJetSkimTrigHeader& StJetSkimTrigHeader::operator=(const StJetSkimTrigHeader & rhs) {
    if(this != &rhs) {
        TObject::operator=(rhs);
        
        this->runId                            = rhs.runId;
        this->trigId                           = rhs.trigId;
        this->prescale                         = rhs.prescale;
        this->eastBarrelTowerThreshold         = rhs.eastBarrelTowerThreshold;
        this->eastBarrelTriggerPatchThreshold  = rhs.eastBarrelTriggerPatchThreshold;
        this->eastBarrelJetPatchThreshold      = rhs.eastBarrelJetPatchThreshold;
        this->westBarrelTowerThreshold         = rhs.westBarrelTowerThreshold;
        this->westBarrelTriggerPatchThreshold  = rhs.westBarrelTriggerPatchThreshold;
        this->westBarrelJetPatchThreshold      = rhs.westBarrelJetPatchThreshold;
        this->endcapTowerThreshold             = rhs.endcapTowerThreshold;
        this->endcapTriggerPatchThreshold      = rhs.endcapTriggerPatchThreshold;
        this->endcapJetPatchThreshold          = rhs.endcapJetPatchThreshold;
        this->totalEnergyThreshold             = rhs.totalEnergyThreshold;
    }
    
    return *this;
}

void StJetSkimTrigHeader::Clear(const char *option) {
    TObject::Clear(option);
    init();
}

void StJetSkimTrigHeader::init() {
    this->runId                            = -1;
    this->trigId                           = -1;
    this->prescale                         = 0.;
    this->eastBarrelTowerThreshold         = -1;
    this->eastBarrelTriggerPatchThreshold  = -1;
    this->eastBarrelJetPatchThreshold      = -1;
    this->westBarrelTowerThreshold         = -1;
    this->westBarrelTriggerPatchThreshold  = -1;
    this->westBarrelJetPatchThreshold      = -1;
    this->endcapTowerThreshold             = -1;
    this->endcapTriggerPatchThreshold      = -1;
    this->endcapJetPatchThreshold          = -1;
    this->totalEnergyThreshold             = -1;
}

StJetSkimTrig::StJetSkimTrig() : TObject()
{
    init();
}

StJetSkimTrig::StJetSkimTrig(const StJetSkimTrig& t) : TObject()
{
    this->mTrigId                   = t.trigId();
    this->mDidFire                  = t.didFire();
    this->mShouldFire               = t.shouldFire();
    this->mShouldFireBBC            = t.shouldFireBBC();
    this->mShouldFireBemc           = t.shouldFireBemc();
    this->mShouldFireEemc           = t.shouldFireEemc();
    this->mShouldFireL2             = t.shouldFireL2();
    this->mTowers                   = t.mTowers;
    this->mTriggerPatches           = t.mTriggerPatches;
    this->mJetPatches               = t.mJetPatches;
    this->mTotalEnergy              = t.totalEnergy();

    memcpy(this->mL2ResultEmulated,t.mL2ResultEmulated,sizeof(this->mL2ResultEmulated));
}

StJetSkimTrig::~StJetSkimTrig() { /* no-op */ }

StJetSkimTrig& StJetSkimTrig::operator=(const StJetSkimTrig& rhs)
{
    if(this != &rhs) {
        TObject::operator=(rhs);
        
        this->mTrigId                   = rhs.trigId();
        this->mDidFire                  = rhs.didFire();
        this->mShouldFire               = rhs.shouldFire();
        this->mShouldFireBBC            = rhs.shouldFireBBC();
        this->mShouldFireBemc           = rhs.shouldFireBemc();
        this->mShouldFireEemc           = rhs.shouldFireEemc();
        this->mShouldFireL2             = rhs.shouldFireL2();
        this->mTowers                   = rhs.mTowers;
        this->mTriggerPatches           = rhs.mTriggerPatches;
        this->mJetPatches               = rhs.mJetPatches;
        this->mTotalEnergy              = rhs.totalEnergy();

	memcpy(this->mL2ResultEmulated,rhs.mL2ResultEmulated,sizeof(this->mL2ResultEmulated));
    }
        
    return *this;
}

void StJetSkimTrig::init() {
    this->mTrigId                       = -1;
    this->mDidFire                      = -1;
    this->mShouldFire                   = -1;
    this->mShouldFireBBC                = -1;
    this->mShouldFireBemc               = -1;
    this->mShouldFireEemc               = -1;
    this->mShouldFireL2                 = -1;
    this->mTotalEnergy                  = -1;
    this->mTowers.clear();
    this->mTriggerPatches.clear();
    this->mJetPatches.clear();

    memset(this->mL2ResultEmulated,0,sizeof(this->mL2ResultEmulated));
}   

void StJetSkimTrig::clear() {
    this->Clear();
}

void StJetSkimTrig::Clear(const char *option) {
    TObject::Clear(option);
    init();
}

map<int,int>& StJetSkimTrig::towersAboveThreshold(int detector) const{
    map<int,int> *theMap = new map<int,int>;
    for(map<int,int>::const_iterator it=mTowers.begin(); it!=mTowers.end(); it++) {
        if( (detector==0) && (it->first > 0) ) {
            (*theMap)[it->first] = it->second;
        }
        else if( (detector==1) && (it->first <= 0) ) {
            (*theMap)[TMath::Abs(it->first)] = it->second;
        }
    }
    return (*theMap);
}

map<int,int>& StJetSkimTrig::triggerPatchesAboveThreshold(int detector) const{
    map<int,int> *theMap = new map<int,int>;
    for(map<int,int>::const_iterator it=mTriggerPatches.begin(); it!=mTriggerPatches.end(); it++) {
        if( (detector==0) && (it->first > 0) ) {
            (*theMap)[it->first - 1] = it->second;
        }
        else if( (detector==1) && (it->first <= 0) ) {
            (*theMap)[TMath::Abs(it->first)] = it->second;
        }
    }
    return (*theMap);
}

map<int,int>& StJetSkimTrig::jetPatchesAboveThreshold(int detector) const{
    map<int,int> *theMap = new map<int,int>;
    for(map<int,int>::const_iterator it=mJetPatches.begin(); it!=mJetPatches.end(); it++) {
        if( (detector==0) && (it->first > 0) ) {
            (*theMap)[it->first - 1] = it->second;
        }
        else if( (detector==1) && (it->first <= 0) ) {
            (*theMap)[TMath::Abs(it->first)] = it->second;
        }
    }
    return (*theMap);
}

void StJetSkimTrig::addTowerAboveThreshold(int detector, int aID, int aADC) {
    pair<int,int> p1(aID,aADC);
    if(detector == 1) p1.first = (-1)*aID;
    pair<map<int,int>::iterator,bool> p2 = mTowers.insert(p1);
    if(!p2.second) {
        cerr << "Error!  towersAboveThreshold map already contains key == " << p1.first << endl;
    }
}

void StJetSkimTrig::addTriggerPatchAboveThreshold(int detector, int aID, int aADC) {
    pair<int,int> p1(aID+1,aADC);
    if(detector == 1) p1.first = (-1)*aID;
    pair<map<int,int>::iterator,bool> p2 = mTriggerPatches.insert(p1);
    if(!p2.second) {
        cerr << "Error!  triggerPatchesAboveThreshold map already contains key == " << p1.first << endl;
    }
}

void StJetSkimTrig::addJetPatchAboveThreshold(int detector, int aID, int aADC) {
    pair<int,int> p1(aID+1,aADC);
    if(detector == 1) p1.first = (-1)*aID;
    pair<map<int,int>::iterator,bool> p2 = mJetPatches.insert(p1);
    if(!p2.second) {
        cerr << "Error!  jetPatchesAboveThreshold map already contains key == " << p1.first << endl;
    }
}

void StJetSkimTrig::setL2ResultEmulated(const int* rhs) {
  memcpy(mL2ResultEmulated,rhs,sizeof(mL2ResultEmulated));
}

StJetSkimVert::StJetSkimVert()
{
    for (int i=0; i<3; ++i) {
        mPosition[i] = 0.;
        mPosError[i] = 0.;
    }
    mVertexFinderId  = mNTracksUsed = mNBTOFMatch = mNCTBMatch = mNBEMCMatch = mNEEMCMatch = mNCrossCentralMembrane = mRefMultNeg = mRefMultPos = mRefMultFtpcWest = mRefMultFtpcEast = 0;
    mRanking =  mSumTrackPt = mMeanDip = mChiSquared = 0.;
}

void StJetSkimVert::setPosition(float* x)
{
    mPosition[0] = x[0];
    mPosition[1] = x[1];
    mPosition[2] = x[2];
}
void StJetSkimVert::setError(float* x)
{
    mPosError[0] = x[0];
    mPosError[1] = x[1];
    mPosError[2] = x[2];
}



void StJetSkimVert::clear()
{
    for (int i=0; i<3; ++i) {
        mPosition[i] = 0.;
        mPosError[i] = 0.;
    }   
    mVertexFinderId  = mNTracksUsed = mNBTOFMatch = mNCTBMatch = mNBEMCMatch = mNEEMCMatch = mNCrossCentralMembrane = mRefMultNeg = mRefMultPos = mRefMultFtpcWest = mRefMultFtpcEast = 0;
    mRanking =  mSumTrackPt = mMeanDip = mChiSquared = 0.;
}

void StJetSkimVert::Clear(const char *option) {
    this->clear();
    TObject::Clear(option);
}

bool StJetSkimVert::operator==(const StJetSkimVert &rhs) const {
    if (this == &rhs) return true;
    
    for(int i=0; i<3; i++) {
        if(mPosition[i] != rhs.position()[i]) return false;
        if(mPosError[i] != rhs.posError()[i]) return false;
    }
    
    if(mRanking != rhs.ranking())               return false;
    if(mNTracksUsed != rhs.nTracksUsed())       return false;
    if(mNBTOFMatch != rhs.nBTOFMatch())         return false;
    if(mNCTBMatch != rhs.nCTBMatch())           return false;
    if(mNBEMCMatch != rhs.nBEMCMatch())         return false;
    if(mNEEMCMatch != rhs.nEEMCMatch())         return false;
    if(mNCrossCentralMembrane != rhs.nCrossCentralMembrane())   return false;
    if(mSumTrackPt != rhs.sumTrackPt())         return false;
    if(mMeanDip != rhs.meanDip())               return false;
    if(mChiSquared != rhs.chiSquared())         return false;
    if(mRefMultNeg != rhs.refMultNeg())         return false;
    if(mRefMultPos != rhs.refMultPos())         return false;
    if(mRefMultFtpcWest != rhs.refMultFtpcWest())   return false;
    if(mRefMultFtpcEast != rhs.refMultFtpcEast())   return false;
    
    return true;
}

StJetSkimEvent::StJetSkimEvent() : TObject(), mTriggers(new TClonesArray("StJetSkimTrig",100)), mVertices(new TClonesArray("StJetSkimVert",100)), mBestVert(NULL), mMcEvent(NULL)
{
    mMudstFileName = "Undefined";
    mBestVertRef = NULL;
    mFill = mRunId = mEventId = mbx7 = mbx48 = mSpinBits = 0;
    mEbbc = mWbbc = mBbcTimeBin = 0;
    mVpdTdiff = mVpdTstart = mVpdZvertex = -999.;
    mVpdEastHits = mVpdWestHits = -1;
    mZdcWestRate = 0;
    mZdcEastRate = 0;
    mZdcCoincidenceRate = 0;
    mBbcWestRate = 0;
    mBbcEastRate = 0;
    mBbcCoincidenceRate = 0;
    mIsValid = mIsPolLong = mIsPolTrans = mIsMaskedUsingBx48 = mOffsetBx48minusBX7 = mSpin4usingBx48 = 0;

    memset(mBarrelJetPatchTh,0,sizeof(mBarrelJetPatchTh));
    memset(mEndcapJetPatchTh,0,sizeof(mEndcapJetPatchTh));
    memset(mOverlapJetPatchTh,0,sizeof(mOverlapJetPatchTh));

    memset(mBarrelHighTowerTh,0,sizeof(mBarrelHighTowerTh));
    memset(mEndcapHighTowerTh,0,sizeof(mEndcapHighTowerTh));

    memset(mBarrelJetPatchAdc,0,sizeof(mBarrelJetPatchAdc));
    memset(mEndcapJetPatchAdc,0,sizeof(mEndcapJetPatchAdc));
    memset(mOverlapJetPatchAdc,0,sizeof(mOverlapJetPatchAdc));

    mEmcLayer2 = 0;
}

StJetSkimEvent::StJetSkimEvent(const StJetSkimEvent &other) : TObject()
{
    this->mFill         = other.fill();
    this->mRunId        = other.runId();
    this->mEventId      = other.eventId();
    this->mMudstFileName= other.mudstFileName();
    
    mTriggers = new TClonesArray("StJetSkimTrig",100);
    for(int i=0; i<other.triggers()->GetEntries(); i++) {
        this->setTrig(*(StJetSkimTrig*)other.triggers()->At(i));
    }
    
    mVertices = new TClonesArray("StJetSkimTrig",100);
    for(int i=0; i<other.vertices()->GetEntries(); i++) {
        this->setVert(*(StJetSkimVert*)other.vertices()->At(i));
    }
    
    mBestVert = other.bestVert();
    if(mBestVert) this->setBestVert(*mBestVert);
    mBestVert = NULL;
    
    this->mbx7          = other.bx7();
    this->mbx48         = other.bx48();
    this->mSpinBits     = other.spinBits();
    this->mEbbc         = other.eBbc();
    this->mWbbc         = other.wBbc();
    this->mBbcTimeBin   = other.bbcTimeBin();

    this->mVpdTdiff   = other.vpdTdiff();
    this->mVpdTstart   = other.vpdTstart();
    this->mVpdZvertex   = other.vpdZvertex();
    this->mVpdEastHits   = other.vpdEastHits();
    this->mVpdWestHits   = other.vpdWestHits();

    mZdcWestRate = other.mZdcWestRate;
    mZdcEastRate = other.mZdcEastRate;
    mZdcCoincidenceRate = other.mZdcCoincidenceRate;
    mBbcWestRate = other.mBbcWestRate;
    mBbcEastRate = other.mBbcEastRate;
    mBbcCoincidenceRate = other.mBbcCoincidenceRate;
    
    this->mIsValid      = other.isValid();
    this->mIsPolLong    = other.isPolLong();
    this->mIsPolTrans   = other.isPolTrans();
    this->mIsMaskedUsingBx48 = other.isMaskedUsingBx48();
    this->mOffsetBx48minusBX7 = other.offsetBx48minusBX7();
    this->mSpin4usingBx48 = other.spin4usingBx48();

    memcpy(mL2Result,other.mL2Result,sizeof(mL2Result));

    memcpy(mBarrelJetPatchTh,other.mBarrelJetPatchTh,sizeof(mBarrelJetPatchTh));
    memcpy(mEndcapJetPatchTh,other.mEndcapJetPatchTh,sizeof(mEndcapJetPatchTh));
    memcpy(mOverlapJetPatchTh,other.mOverlapJetPatchTh,sizeof(mOverlapJetPatchTh));

    memcpy(mBarrelHighTowerTh,other.mBarrelHighTowerTh,sizeof(mBarrelHighTowerTh));
    memcpy(mEndcapHighTowerTh,other.mEndcapHighTowerTh,sizeof(mEndcapHighTowerTh));

    memcpy(mBarrelJetPatchAdc,other.mBarrelJetPatchAdc,sizeof(mBarrelJetPatchAdc));
    memcpy(mEndcapJetPatchAdc,other.mEndcapJetPatchAdc,sizeof(mEndcapJetPatchAdc));
    memcpy(mOverlapJetPatchAdc,other.mOverlapJetPatchAdc,sizeof(mOverlapJetPatchAdc));

    mEmcLayer2 = other.mEmcLayer2;
}

StJetSkimEvent::~StJetSkimEvent()
{
    if(mTriggers) mTriggers->Delete();
    if(mVertices) mVertices->Delete();
}

StJetSkimEvent& StJetSkimEvent::operator=(const StJetSkimEvent &rhs) 
{
    if(this != &rhs) {
        TObject::operator=(rhs);
        
        this->mFill         = rhs.fill();
        this->mRunId        = rhs.runId();
        this->mEventId      = rhs.eventId();
        this->mMudstFileName= rhs.mudstFileName();
            
        mTriggers->Delete();
        for(int i=0; i<rhs.triggers()->GetEntries(); i++) {
            this->setTrig(*(StJetSkimTrig*)rhs.triggers()->At(i));
        }
        
        mVertices->Delete();
        for(int i=0; i<rhs.vertices()->GetEntries(); i++) {
            this->setVert(*(StJetSkimVert*)rhs.vertices()->At(i));
        }
        
        mBestVert = rhs.bestVert();
        if(mBestVert) this->setBestVert(*mBestVert);
        mBestVert = NULL;
                        
        this->mbx7          = rhs.bx7();
        this->mbx48         = rhs.bx48();
        this->mSpinBits     = rhs.spinBits();
        this->mEbbc         = rhs.eBbc();
        this->mWbbc         = rhs.wBbc();
        this->mBbcTimeBin   = rhs.bbcTimeBin();

        this->mVpdTdiff   = rhs.vpdTdiff();
        this->mVpdTstart   = rhs.vpdTstart();
        this->mVpdZvertex   = rhs.vpdZvertex();
        this->mVpdEastHits   = rhs.vpdEastHits();
        this->mVpdWestHits   = rhs.vpdWestHits();

	mZdcWestRate = rhs.mZdcWestRate;
	mZdcEastRate = rhs.mZdcEastRate;
	mZdcCoincidenceRate = rhs.mZdcCoincidenceRate;
	mBbcWestRate = rhs.mBbcWestRate;
	mBbcEastRate = rhs.mBbcEastRate;
	mBbcCoincidenceRate = rhs.mBbcCoincidenceRate;
        
        this->mIsValid      = rhs.isValid();
        this->mIsPolLong    = rhs.isPolLong();
        this->mIsPolTrans   = rhs.isPolTrans();
        this->mIsMaskedUsingBx48 = rhs.isMaskedUsingBx48();
        this->mOffsetBx48minusBX7 = rhs.offsetBx48minusBX7();
        this->mSpin4usingBx48 = rhs.spin4usingBx48();

	memcpy(mL2Result,rhs.mL2Result,sizeof(mL2Result));

	memcpy(mBarrelJetPatchTh,rhs.mBarrelJetPatchTh,sizeof(mBarrelJetPatchTh));
	memcpy(mEndcapJetPatchTh,rhs.mEndcapJetPatchTh,sizeof(mEndcapJetPatchTh));
	memcpy(mOverlapJetPatchTh,rhs.mOverlapJetPatchTh,sizeof(mOverlapJetPatchTh));

	memcpy(mBarrelHighTowerTh,rhs.mBarrelHighTowerTh,sizeof(mBarrelHighTowerTh));
	memcpy(mEndcapHighTowerTh,rhs.mEndcapHighTowerTh,sizeof(mEndcapHighTowerTh));

	memcpy(mBarrelJetPatchAdc,rhs.mBarrelJetPatchAdc,sizeof(mBarrelJetPatchAdc));
	memcpy(mEndcapJetPatchAdc,rhs.mEndcapJetPatchAdc,sizeof(mEndcapJetPatchAdc));
	memcpy(mOverlapJetPatchAdc,rhs.mOverlapJetPatchAdc,sizeof(mOverlapJetPatchAdc));

	mEmcLayer2 = rhs.mEmcLayer2;
    }
    
    return *this;
}

void StJetSkimEvent::clear()
{
    mMudstFileName = "Undefined";
    mFill = mRunId = mEventId =  mbx7 = mbx48 = mSpinBits = 0;
    mEbbc = mWbbc = mBbcTimeBin = 0;
    mVpdTdiff = mVpdTstart = mVpdZvertex = -999.;
    mVpdEastHits = mVpdWestHits = -1;
    mZdcWestRate = 0;
    mZdcEastRate = 0;
    mZdcCoincidenceRate = 0;
    mBbcWestRate = 0;
    mBbcEastRate = 0;
    mBbcCoincidenceRate = 0;
    mIsValid = mIsPolLong = mIsPolTrans = mIsMaskedUsingBx48 = mOffsetBx48minusBX7 = mSpin4usingBx48 = 0;
    mTriggers->Clear("c");
    mVertices->Clear("c");
    mBestVert = NULL;
    mBestVertRef.Clear();
    mTrigHeaderArrayRef.Clear();

    memset(mBarrelJetPatchTh,0,sizeof(mBarrelJetPatchTh));
    memset(mEndcapJetPatchTh,0,sizeof(mEndcapJetPatchTh));
    memset(mOverlapJetPatchTh,0,sizeof(mOverlapJetPatchTh));

    memset(mBarrelHighTowerTh,0,sizeof(mBarrelHighTowerTh));
    memset(mEndcapHighTowerTh,0,sizeof(mEndcapHighTowerTh));

    memset(mBarrelJetPatchAdc,0,sizeof(mBarrelJetPatchAdc));
    memset(mEndcapJetPatchAdc,0,sizeof(mEndcapJetPatchAdc));
    memset(mOverlapJetPatchAdc,0,sizeof(mOverlapJetPatchAdc));

    mEmcLayer2 = 0;
}

void StJetSkimEvent::Clear(const char *option) {
    this->clear();
    TObject::Clear(option);
}

StJetSkimVert* StJetSkimEvent::bestVert() const {
    return (mBestVertRef.GetObject() ? (StJetSkimVert*)mBestVertRef.GetObject() : mBestVert);
}

void StJetSkimEvent::setBestVert(const StJetSkimVert & v) {
    for(int i=0; i<mVertices->GetEntries(); i++) {
        if(v == *(StJetSkimVert*)(mVertices->At(i))) {
            this->setBestVert(i);
            break;
        }
    }
    
    if(this->bestVert() == NULL) {
        cerr << "StJetSkimEvent::setBestVert -- Error! Couldn't set best vertex" << endl;
        cerr << "StJetSkimEvent::setBestVert -- Make sure the vertex is already in the new clones array" << endl;
    }
}

void StJetSkimEvent::setBestVert(int clonesArrayIndex) {
    mBestVertRef = (StJetSkimVert*)mVertices->At(clonesArrayIndex);
}

void StJetSkimEvent::setL2Result(const int* rhs) {
    memcpy(mL2Result,rhs,sizeof(mL2Result));
}

void StJetSkimEvent::setTrig(const StJetSkimTrig& t)
{
    int addAt = mTriggers->GetLast()+1;
    
    //and here's the crazy syntax
    TClonesArray& arrayRef = *(mTriggers);
    new ( arrayRef[addAt]) StJetSkimTrig( t );
}

void StJetSkimEvent::setVert(const StJetSkimVert& t)
{
    int addAt = mVertices->GetLast()+1;
    
    //and here's the crazy syntax
    TClonesArray& arrayRef = *(mVertices);
    new ( arrayRef[addAt]) StJetSkimVert( t );
}

StJetSkimTrigHeader* StJetSkimEvent::trigHeader(int trigId) {
    TClonesArray *tmp = this->trigHeaders();
    if(!tmp) {
        cout << "ERROR LOADING TRIGGER HEADERS!" << endl;
        return NULL;
    }
    for(int i=0; i<tmp->GetEntries(); i++) {
        StJetSkimTrigHeader* header = (StJetSkimTrigHeader*)tmp->At(i);
        if(header->trigId == trigId) return header;
    }
    return NULL;
}

StJetSkimTrig* StJetSkimEvent::trigger(int trigId) {
    TClonesArray *tmp = this->triggers();
    for(int i=0; i<tmp->GetEntries(); i++) {
        StJetSkimTrig* trigger = (StJetSkimTrig*)tmp->At(i);
        if(trigger->trigId() == trigId) return trigger;
    }
    return NULL;
}

map<int,int> StJetSkimEvent::barrelJetPatchesAboveTh(int i) const
{
  map<int,int> m;
  for (int jp = 0; jp < 18; ++jp) {
    int adc = barrelJetPatchAdc(jp);
    if (adc > barrelJetPatchTh(i)) m.insert(make_pair(jp,adc));
  }
  return m;
}

map<int,int> StJetSkimEvent::endcapJetPatchesAboveTh(int i) const
{
  map<int,int> m;
  for (int jp = 0; jp < 6; ++jp) {
    int adc = endcapJetPatchAdc(jp);
    if (adc > endcapJetPatchTh(i)) m.insert(make_pair(jp,adc));
  }
  return m;
}

map<int,int> StJetSkimEvent::overlapJetPatchesAboveTh(int i) const
{
  map<int,int> m;
  for (int jp = 0; jp < 6; ++jp) {
    int adc = overlapJetPatchAdc(jp);
    if (adc > overlapJetPatchTh(i)) m.insert(make_pair(jp,adc));
  }
  return m;
}
