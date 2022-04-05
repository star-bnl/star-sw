/*
 * StEmcOfflineCalibrationEvent.cxx
 * J. Kevin Adkins, University of Kentucky
 * June 15, 2014
 *
 * Access methods and tree creating algorithm for
 * BEMC calibration trees. This was updated from 
 * the previous method to be a bit more streamlined
 * and user friendly. Also added independent trigger
 * class easier definition of triggered events
 */

#include "TClonesArray.h"
#include "StEmcOfflineCalibrationEvent.h"
#include <map>
#include <algorithm>

ClassImp(StEmcOfflineCalibrationCluster)
ClassImp(StEmcOfflineCalibrationTrack)
ClassImp(StEmcOfflineCalibrationEvent)
ClassImp(StEmcOfflineCalibrationTrigger)
ClassImp(StEmcOfflineCalibrationVertex)
//----------------------------------------------------------------------------------
StEmcOfflineCalibrationTrack::StEmcOfflineCalibrationTrack()
{
  Clear();
}

StEmcOfflineCalibrationTrack::~StEmcOfflineCalibrationTrack(){/** Nothing **/}

void StEmcOfflineCalibrationTrack::Clear(Option_t* option)
{
  fill(mTowerId, mTowerId+9, -1);
  fill(mTowerAdc, mTowerAdc+9, -1);
  fill(mTowerStatus, mTowerStatus+9, -1);
  fill(mTowerPed, mTowerPed+9, -1.);
  fill(mTowerPedRms, mTowerPedRms+9, -1.);

  fill(mPreshowerAdc, mPreshowerAdc+9, -1);
  fill(mPreshowerStatus, mPreshowerStatus+9, -1);
  fill(mPreshowerPed, mPreshowerPed+9, -1.);
  fill(mPreshowerPedRms, mPreshowerPedRms+9, -1.);

  fill(mSmdeId, mSmdeId+11, -1);
  fill(mSmdeAdc, mSmdeAdc+11, -1);
  fill(mSmdeStatus, mSmdeStatus+11, -1);
  fill(mSmdePed, mSmdePed+11, -1.);
  fill(mSmdePedRms, mSmdePedRms+11, -1.);

  fill(mSmdpId, mSmdpId+11, -1);
  fill(mSmdpAdc, mSmdpAdc+11, -1);
  fill(mSmdpStatus, mSmdpStatus+11, -1);
  fill(mSmdpPed, mSmdpPed+11, -1.);
  fill(mSmdpPedRms, mSmdpPedRms+11, -1.);

  mTowerExitId = -1;
  mVertexIndex = -1;
  mFlag = -10;
  mBad = -10;
  mNHits = -1;
  mNFitPoints = -1;
  mNDedxPoints = -1;
  mNHitsPossible = -1;
  mCharge = -10;

  mP = -99.;
  mEta = -99.;
  mPhi = -99.;
  mDeDx = -99.;
  mNSigmaElectron = -99.;
  mNSigmaPion = -99.;
  mNSigmaKaon = -99.;
  mNSigmaProton = -99.;
  mHighestNeighbor = -99.;
  mDeta = -99.;
  mDphi = -99.;
  mMomentum.setX(-99.); mMomentum.setY(-99.); mMomentum.setZ(-99.);

  mTofMatchedFlag = -10;
  mVpdVz = -999.;
  mDcaGlobal = -1.;
  mTofTime = -1.;
  mTofBeta = -10.;
  mTofPathLength = -10.;
  mTofSigmaElectron = -100.;
  mTofProbElectron = -100.;
}

void StEmcOfflineCalibrationTrack::setTowerId(Int_t id, Int_t aTid) { mTowerId[id] = aTid; }
void StEmcOfflineCalibrationTrack::setTowerAdc(Int_t id, Int_t aAdc) { mTowerAdc[id] = aAdc; }
void StEmcOfflineCalibrationTrack::setTowerStatus(Int_t id, Int_t aStat) { mTowerStatus[id] = aStat; }
void StEmcOfflineCalibrationTrack::setTowerPedestal(Int_t id, Float_t aPed) { mTowerPed[id] = aPed; }
void StEmcOfflineCalibrationTrack::setTowerPedestalRms(Int_t id, Float_t aRms) { mTowerPedRms[id] = aRms; }
void StEmcOfflineCalibrationTrack::setPreshowerAdc(Int_t id, Int_t bAdc) { mPreshowerAdc[id] = bAdc; }
void StEmcOfflineCalibrationTrack::setPreshowerStatus(Int_t id, Int_t bStat) { mPreshowerStatus[id] = bStat; }
void StEmcOfflineCalibrationTrack::setPreshowerPedestal(Int_t id, Float_t bPed) { mPreshowerPed[id] = bPed; }
void StEmcOfflineCalibrationTrack::setPreshowerPedestalRms(Int_t id, Float_t bRms) { mPreshowerPedRms[id] = bRms; }
void StEmcOfflineCalibrationTrack::setPreshowerCap(Int_t id, UChar_t bCap) { mPreshowerCap[id] = bCap; }

void StEmcOfflineCalibrationTrack::setSmdeId(Int_t id, Int_t smdeId) { mSmdeId[id] = smdeId; }
void StEmcOfflineCalibrationTrack::setSmdeAdc(Int_t id, Int_t smdeAdc) { mSmdeAdc[id] = smdeAdc; }
void StEmcOfflineCalibrationTrack::setSmdeStatus(Int_t id, Int_t smdeStat) { mSmdeStatus[id] = smdeStat; }
void StEmcOfflineCalibrationTrack::setSmdePedestal(Int_t id, Float_t smdePed) { mSmdePed[id] = smdePed; }
void StEmcOfflineCalibrationTrack::setSmdePedestalRms(Int_t id, Float_t smdeRms) { mSmdePedRms[id] = smdeRms; }
void StEmcOfflineCalibrationTrack::setSmdpId(Int_t id, Int_t smdpId) { mSmdpId[id] = smdpId; }
void StEmcOfflineCalibrationTrack::setSmdpAdc(Int_t id, Int_t smdpAdc) { mSmdpAdc[id] = smdpAdc; }
void StEmcOfflineCalibrationTrack::setSmdpStatus(Int_t id, Int_t smdpStat) { mSmdpStatus[id] = smdpStat; }
void StEmcOfflineCalibrationTrack::setSmdpPedestal(Int_t id, Float_t smdpPed) { mSmdpPed[id] = smdpPed; }
void StEmcOfflineCalibrationTrack::setSmdpPedestalRms(Int_t id, Float_t smdpRms) { mSmdpPedRms[id] = smdpRms; }

void StEmcOfflineCalibrationTrack::setTowerExitId(Int_t aTowExitId) { mTowerExitId = aTowExitId; }
void StEmcOfflineCalibrationTrack::setVertexIndex(Int_t aVertInd) { mVertexIndex = aVertInd; }
void StEmcOfflineCalibrationTrack::setFlag(Int_t aFlag) { mFlag = aFlag; }
void StEmcOfflineCalibrationTrack::setBad(Int_t aBad) { mBad = aBad; }
void StEmcOfflineCalibrationTrack::setNHits(Int_t aHits) { mNHits = aHits; }
void StEmcOfflineCalibrationTrack::setNFitPoints(Int_t aFitPts) { mNFitPoints = aFitPts; }
void StEmcOfflineCalibrationTrack::setNDedxPoints(Int_t aDeDxPts) { mNDedxPoints = aDeDxPts; }
void StEmcOfflineCalibrationTrack::setNHitsPossible(Int_t aHitsPoss) { mNHitsPossible = aHitsPoss; }
void StEmcOfflineCalibrationTrack::setCharge(Int_t aCharge) { mCharge = aCharge; }
void StEmcOfflineCalibrationTrack::setP(Double_t aP) { mP = aP; }
void StEmcOfflineCalibrationTrack::setEta(Double_t aEta) { mEta = aEta; }
void StEmcOfflineCalibrationTrack::setPhi(Double_t aPhi) { mPhi = aPhi; }
void StEmcOfflineCalibrationTrack::setDeDx(Double_t aDeDx) { mDeDx = aDeDx; }
void StEmcOfflineCalibrationTrack::setNSigmaElectron(Double_t aNSElec) { mNSigmaElectron = aNSElec; }
void StEmcOfflineCalibrationTrack::setNSigmaPion(Double_t aNSPion) { mNSigmaPion = aNSPion; }
void StEmcOfflineCalibrationTrack::setNSigmaKaon(Double_t aNSKaon) { mNSigmaKaon = aNSKaon; }
void StEmcOfflineCalibrationTrack::setNSigmaProton(Double_t aNSProton) { mNSigmaProton = aNSProton; }
void StEmcOfflineCalibrationTrack::setHighestNeighbor(Float_t aHN) { mHighestNeighbor = aHN; }
void StEmcOfflineCalibrationTrack::setDeta(Float_t aDeta) { mDeta = aDeta; }
void StEmcOfflineCalibrationTrack::setDphi(Float_t aDphi) { mDphi = aDphi; }
void StEmcOfflineCalibrationTrack::setMomentum(StThreeVectorF aMom) { mMomentum = aMom; }
void StEmcOfflineCalibrationTrack::setTofMatchedFlag(Int_t aTofFlag) { mTofMatchedFlag = aTofFlag; }
void StEmcOfflineCalibrationTrack::setVpdVz(Float_t aVpdVz) { mVpdVz = aVpdVz; }
void StEmcOfflineCalibrationTrack::setDcaGlobal(Float_t aDca) { mDcaGlobal = aDca; }
void StEmcOfflineCalibrationTrack::setTofTime(Float_t aTofTime) { mTofTime = aTofTime; }
void StEmcOfflineCalibrationTrack::setTofBeta(Float_t aTofBeta) { mTofBeta = aTofBeta; }
void StEmcOfflineCalibrationTrack::setTofPathlength(Float_t aTofPL) { mTofPathLength = aTofPL; }
void StEmcOfflineCalibrationTrack::setTofSigmaElectron(Float_t aTofSE) { mTofSigmaElectron = aTofSE; }
void StEmcOfflineCalibrationTrack::setTofProbElectron(Float_t aTofPE) { mTofProbElectron = aTofPE; }
//----------------------------------------------------------------------------------
StEmcOfflineCalibrationEvent::StEmcOfflineCalibrationEvent()
{
  mTracks = new TClonesArray("StEmcOfflineCalibrationTrack",150);
  mTriggers = new TClonesArray("StEmcOfflineCalibrationTrigger",20);
  mVertices = new TClonesArray("StEmcOfflineCalibrationVertex",10);
  Clear();
}

StEmcOfflineCalibrationEvent::~StEmcOfflineCalibrationEvent()
{
  Clear();
  mTracks->Delete();
  mVertices->Delete();
  mTriggers->Delete();
  
  if(mTracks) delete mTracks;
  if(mVertices) delete mVertices;
  if(mTriggers) delete mTriggers;
}

void StEmcOfflineCalibrationEvent::Clear(Option_t* option)
{
  mTracks->Clear(option);
  mTriggers->Clear(option);
  mVertices->Clear(option);

  mFillNum = 0;
  mDate = 0;
  mTime = 0;
  mRunNum = 0;
  mEventId = 0;

  fill(mHighTowerTh, mHighTowerTh+4, 0);
  fill(mHighTowerAdc, mHighTowerAdc+4800, 0);
}

Int_t StEmcOfflineCalibrationEvent::nVertices() const { return mVertices->GetEntriesFast(); }
Int_t StEmcOfflineCalibrationEvent::nTriggers() const { return mTriggers->GetEntriesFast(); }
Int_t StEmcOfflineCalibrationEvent::nTracks() const { return mTracks->GetEntriesFast(); }

StEmcOfflineCalibrationVertex* StEmcOfflineCalibrationEvent::vertex(Int_t iVert) const { return (StEmcOfflineCalibrationVertex*)mVertices->At(iVert); }
StEmcOfflineCalibrationTrack* StEmcOfflineCalibrationEvent::track(Int_t iTrack) const { return (StEmcOfflineCalibrationTrack*)mTracks->At(iTrack); }
StEmcOfflineCalibrationTrigger* StEmcOfflineCalibrationEvent::trigger(Int_t trigId) const {
    TClonesArray *tmp = this->triggers();
    for(int i=0; i<tmp->GetEntries(); i++) {
        StEmcOfflineCalibrationTrigger* trigger = (StEmcOfflineCalibrationTrigger*)tmp->At(i);
        if(trigger->trigId() == trigId) return trigger;
    }
    return NULL;
}

map<Int_t, Int_t> StEmcOfflineCalibrationEvent::towersAboveHighTowerTh(Int_t th) const
{
  map<Int_t, Int_t> aMap;
  for (Int_t iTow = 0; iTow < 4800; ++iTow){
    Int_t softId = iTow+1;
    Int_t mAdc = highTowerAdc(softId);
    if (mAdc > highTowerTh(th))
      aMap.insert(make_pair(softId,mAdc));
  }
  return aMap;
}

void StEmcOfflineCalibrationEvent::setFill(Int_t aFill) { mFillNum = aFill; }
void StEmcOfflineCalibrationEvent::setRunNum(Int_t aRunNum) { mRunNum = aRunNum; }
void StEmcOfflineCalibrationEvent::setEventId(Int_t aEventId) { mEventId = aEventId; }
void StEmcOfflineCalibrationEvent::setDate(Int_t aDate) { mDate = aDate; }
void StEmcOfflineCalibrationEvent::setTime(Int_t aTime) { mTime = aTime; }
void StEmcOfflineCalibrationEvent::setHighTowerTh(Int_t level, Int_t thresh) { mHighTowerTh[level] = thresh; }
void StEmcOfflineCalibrationEvent::setHighTowerAdc(Int_t softId, Int_t adc) { mHighTowerAdc[softId-1] = adc; }

// Adding functions
StEmcOfflineCalibrationTrack* StEmcOfflineCalibrationEvent::addTrack(){
  return new ( (*mTracks)[mTracks->GetEntriesFast()] ) StEmcOfflineCalibrationTrack;
}

StEmcOfflineCalibrationVertex* StEmcOfflineCalibrationEvent::addVertex(){
  return new ( (*mVertices)[mVertices->GetEntriesFast()] ) StEmcOfflineCalibrationVertex;
}

StEmcOfflineCalibrationTrigger* StEmcOfflineCalibrationEvent::addTrigger(){
  return new ( (*mTriggers)[mTriggers->GetEntriesFast()] ) StEmcOfflineCalibrationTrigger;
}

//----------------------------------------------------------------------------------

StEmcOfflineCalibrationTrigger::StEmcOfflineCalibrationTrigger()
{
  InitTrigValues();
}

StEmcOfflineCalibrationTrigger::~StEmcOfflineCalibrationTrigger(){ /** Nothing **/ }

void StEmcOfflineCalibrationTrigger::InitTrigValues()
{
  mTrigId = -1;
  mShouldFire = false;
  mDidFire = false;
}

void StEmcOfflineCalibrationTrigger::ClearTrigValues()
{
  InitTrigValues();
}

void StEmcOfflineCalibrationTrigger::setTrigId(Int_t aTrigId) { mTrigId = aTrigId; }
void StEmcOfflineCalibrationTrigger::setDidFire(Bool_t aDidFire) { mDidFire = aDidFire; }
void StEmcOfflineCalibrationTrigger::setShouldFire(Bool_t aShouldFire) { mShouldFire = aShouldFire; }
//----------------------------------------------------------------------------------
StEmcOfflineCalibrationVertex::StEmcOfflineCalibrationVertex()
{
  mTracks.Clear();
  mRanking = -999.;
  mX = mY = mZ = -999.;
}
StEmcOfflineCalibrationVertex::~StEmcOfflineCalibrationVertex(){ /** Nothing **/ }

void StEmcOfflineCalibrationVertex::setRanking(Double_t aRanking) { mRanking = aRanking; }
void StEmcOfflineCalibrationVertex::setX(Double_t aX) { mX = aX; }
void StEmcOfflineCalibrationVertex::setY(Double_t aY) { mY = aY; }
void StEmcOfflineCalibrationVertex::setZ(Double_t aZ) { mZ = aZ; }
//----------------------------------------------------------------------------------
StEmcOfflineCalibrationCluster::StEmcOfflineCalibrationCluster(){ Clear(); }
StEmcOfflineCalibrationCluster::~StEmcOfflineCalibrationCluster(){ /** Nothing **/ }
void StEmcOfflineCalibrationCluster::setCentralTrack(StEmcOfflineCalibrationTrack *tk) { mCentralTrack = tk; }
void StEmcOfflineCalibrationCluster::Clear(Option_t *option){ mNeighborTracks.Clear(); mCentralTrack = 0; }
//----------------------------------------------------------------------------------
