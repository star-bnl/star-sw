/*
 * StEmcOfflineCalibrationEvent.h
 * J. Kevin Adkins, University of Kentucky
 * June 15, 2014
 */


#ifndef STAR_StEmcOfflineCalibrationEvent
#define STAR_StEmcOfflineCalibrationEvent

#include "TObject.h"
#include "TClonesArray.h"
#include "TVector3.h"
#include "StThreeVectorF.hh"
#include "TRefArray.h"

#include <vector>
#include <algorithm>
#include <map>

class StEmcOfflineCalibrationVertex;
class StEmcOfflineCalibrationTrigger;
class StEmcOfflineCalibrationTrack;

using namespace std;
//----------------------------------------------------------------------
class StEmcOfflineCalibrationTrack : public TObject
{
public:
  StEmcOfflineCalibrationTrack();
  ~StEmcOfflineCalibrationTrack();  
  void Clear(Option_t* option="");

  // 0 < id <= 8. 0 is central tower in 3x3 cluster, 1-8 are surrounding towers
  Int_t towerId(Int_t id) const { return mTowerId[id]; }
  Int_t towerAdc(Int_t id) const { return mTowerAdc[id]; }
  Int_t towerStatus(Int_t id) const { return mTowerStatus[id]; }
  Float_t towerPedestal(Int_t id) const { return mTowerPed[id]; }
  Float_t towerPedestalRms(Int_t id) const { return mTowerPedRms[id]; }
  Int_t preshowerAdc(Int_t id) const { return mPreshowerAdc[id]; }
  Int_t preshowerStatus(Int_t id) const { return mPreshowerStatus[id]; }
  Float_t preshowerPedestal(Int_t id) const{ return mPreshowerPed[id]; }
  Float_t preshowerPedestalRms(Int_t id) const{ return mPreshowerPedRms[id]; }
  UChar_t preshowerCap(Int_t id) const{ return mPreshowerCap[id]; }
  
  // 0 << id <= 10 for SMD
  Int_t smdeId(Int_t id) const { return mSmdeId[id]; }
  Int_t smdeAdc(Int_t id) const { return mSmdeAdc[id]; }
  Int_t smdeStatus(Int_t id) const { return mSmdeStatus[id]; }
  Float_t smdePedestal(Int_t id) const { return mSmdePed[id]; }
  Float_t smdePedestalRms(Int_t id) const { return mSmdePedRms[id]; }

  Int_t smdpId(Int_t id) const { return mSmdpId[id]; }
  Int_t smdpAdc(Int_t id) const { return mSmdpAdc[id]; }
  Int_t smdpStatus(Int_t id) const { return mSmdpStatus[id]; }
  Float_t smdpPedestal(Int_t id) const { return mSmdpPed[id]; }
  Float_t smdpPedestalRms(Int_t id) const { return mSmdpPedRms[id]; }

  Int_t towerExitId() const { return mTowerExitId; }
  Int_t vertexIndex() const { return mVertexIndex; }
  Int_t flag() const { return mFlag; }
  Int_t bad() const { return mBad; }
  Int_t nHits() const { return mNHits; }
  Int_t nFitPoints() const { return mNFitPoints; }
  Int_t nDedxPoints() const { return mNDedxPoints; }
  Int_t nHitsPossible() const { return mNHitsPossible; }
  Int_t charge() const { return mCharge; }

  Double_t p() const { return mP; }
  Double_t eta() const { return mEta; }
  Double_t phi() const { return mPhi; }
  Double_t dEdx() const { return mDeDx; }
  Double_t nSigmaElectron() const { return mNSigmaElectron; }
  Double_t nSigmaKaon() const { return mNSigmaKaon; }
  Double_t nSigmaPion() const { return mNSigmaPion; }
  Double_t nSigmaProton() const { return mNSigmaProton; }
  Float_t highestNeighbor() const { return mHighestNeighbor; }
  Float_t deta() const { return mDeta; }
  Float_t dphi() const { return mDphi; }
  StThreeVectorF momentum() const { return mMomentum; }
  
  // information from TOF
  Int_t tofMatchedFlag() const { return mTofMatchedFlag; }
  Float_t vpdVz() const { return mVpdVz; }
  Float_t dcaGlobal() const { return mDcaGlobal; }
  Float_t tofTime() const { return mTofTime; }
  Float_t tofBeta() const { return mTofBeta; }
  Float_t tofPathlength() const { return mTofPathLength; }
  Float_t tofSigmaElectron() const { return mTofSigmaElectron; }
  Float_t tofProbElectron() const { return mTofProbElectron; }

  void setTowerId(Int_t,Int_t);
  void setTowerAdc(Int_t,Int_t);
  void setTowerStatus(Int_t,Int_t);
  void setTowerPedestal(Int_t, Float_t);
  void setTowerPedestalRms(Int_t, Float_t);
  void setPreshowerAdc(Int_t, Int_t);
  void setPreshowerStatus(Int_t, Int_t);
  void setPreshowerPedestal(Int_t, Float_t);
  void setPreshowerPedestalRms(Int_t, Float_t);
  void setPreshowerCap(Int_t, UChar_t);
  void setSmdeId(Int_t, Int_t);
  void setSmdeAdc(Int_t, Int_t);
  void setSmdeStatus(Int_t, Int_t);
  void setSmdePedestal(Int_t, Float_t);
  void setSmdePedestalRms(Int_t, Float_t);
  void setSmdpId(Int_t, Int_t);
  void setSmdpAdc(Int_t, Int_t);
  void setSmdpStatus(Int_t, Int_t);
  void setSmdpPedestal(Int_t, Float_t);
  void setSmdpPedestalRms(Int_t, Float_t);
  void setTowerExitId(Int_t);
  void setVertexIndex(Int_t);
  void setFlag(Int_t);
  void setBad(Int_t);
  void setNHits(Int_t);
  void setNFitPoints(Int_t);
  void setNDedxPoints(Int_t);
  void setNHitsPossible(Int_t);
  void setCharge(Int_t);
  void setP(Double_t);
  void setEta(Double_t);
  void setPhi(Double_t);
  void setDeDx(Double_t);
  void setNSigmaElectron(Double_t);
  void setNSigmaPion(Double_t);
  void setNSigmaKaon(Double_t);
  void setNSigmaProton(Double_t);
  void setHighestNeighbor(Float_t);
  void setDeta(Float_t);
  void setDphi(Float_t);
  void setMomentum(StThreeVectorF);
  void setTofMatchedFlag(Int_t);
  void setVpdVz(Float_t);
  void setDcaGlobal(Float_t);
  void setTofTime(Float_t);
  void setTofBeta(Float_t);
  void setTofPathlength(Float_t);
  void setTofSigmaElectron(Float_t);
  void setTofProbElectron(Float_t);

 private:
  Int_t mTowerId[9];
  Int_t mTowerAdc[9];
  Int_t mTowerStatus[9];
  Float_t mTowerPed[9];
  Float_t mTowerPedRms[9];

  Int_t mPreshowerAdc[9];
  Int_t mPreshowerStatus[9];
  Float_t mPreshowerPed[9];
  Float_t mPreshowerPedRms[9];
  UChar_t mPreshowerCap[9];

  Int_t mSmdeId[11];
  Int_t mSmdeAdc[11];
  Int_t mSmdeStatus[11];
  Float_t mSmdePed[11];
  Float_t mSmdePedRms[11];

  Int_t mSmdpId[11];
  Int_t mSmdpAdc[11];
  Int_t mSmdpStatus[11];
  Float_t mSmdpPed[11];
  Float_t mSmdpPedRms[11];

  Int_t mTowerExitId;
  Int_t mVertexIndex;
  Int_t mFlag;
  Int_t mBad;
  Int_t mNHits;
  Int_t mNFitPoints;
  Int_t mNDedxPoints;
  Int_t mNHitsPossible;
  Int_t mCharge;

  Double_t mP;
  Double_t mEta;
  Double_t mPhi;
  Double_t mDeDx;
  Double_t mNSigmaElectron;
  Double_t mNSigmaPion;
  Double_t mNSigmaKaon;
  Double_t mNSigmaProton;
  Float_t mHighestNeighbor;
  Float_t mDeta;
  Float_t mDphi;
  StThreeVectorF mMomentum;

  Int_t mTofMatchedFlag;
  Float_t mVpdVz;
  Float_t mDcaGlobal;
  Float_t mTofTime;
  Float_t mTofBeta;
  Float_t mTofPathLength;
  Float_t mTofSigmaElectron;
  Float_t mTofProbElectron;

  ClassDef(StEmcOfflineCalibrationTrack, 9);
};
//----------------------------------------------------------------------
class StEmcOfflineCalibrationEvent : public TObject
{
 public:
  StEmcOfflineCalibrationEvent();
  ~StEmcOfflineCalibrationEvent();
  
  void Clear(Option_t* option="");
  
  //event info
  Int_t fillNum() const { return mFillNum; }
  Int_t runNum() const { return mRunNum; }
  Int_t eventId() const { return mEventId; }
  Int_t date() const { return mDate; }
  Int_t time() const { return mTime; }
  
  // Tracks
  TClonesArray *tracks() const { return mTracks; }
  Int_t nTracks() const;
  StEmcOfflineCalibrationTrack *track(Int_t) const;

  // Triggers
  TClonesArray *triggers() const { return mTriggers; }
  Int_t nTriggers() const;
  StEmcOfflineCalibrationTrigger *trigger(Int_t) const;

  //Vertices
  TClonesArray *vertices() const { return mVertices; }
  Int_t nVertices() const;
  StEmcOfflineCalibrationVertex *vertex(Int_t) const;

  Int_t highTowerTh(Int_t th) const { return mHighTowerTh[th]; }
  Int_t highTowerAdc(Int_t softId) const { return mHighTowerAdc[softId-1]; }

  map<Int_t,Int_t> towersAboveHighTowerTh(Int_t) const;

  // Adding functions
  StEmcOfflineCalibrationTrack* addTrack();
  StEmcOfflineCalibrationVertex* addVertex();
  StEmcOfflineCalibrationTrigger* addTrigger();

  // Setting functions
  void setFill(Int_t aFill);
  void setRunNum(Int_t aRunNum);
  void setEventId(Int_t aEventId);
  void setDate(Int_t aDate);
  void setTime(Int_t aTime);
  void setHighTowerTh(Int_t,Int_t);
  void setHighTowerAdc(Int_t,Int_t);

 private:
  Int_t mFillNum;
  Int_t mRunNum;
  Int_t mEventId;
  Int_t mDate;
  Int_t mTime;

  Int_t mHighTowerAdc[4800];
  Int_t mHighTowerTh[4];
  
  TClonesArray *mTracks;
  TClonesArray *mTriggers;
  TClonesArray *mVertices;

  ClassDef(StEmcOfflineCalibrationEvent,6);
};
//----------------------------------------------------------------------
class StEmcOfflineCalibrationTrigger : public TObject {
 public:
  StEmcOfflineCalibrationTrigger();
  ~StEmcOfflineCalibrationTrigger();

  void InitTrigValues();
  void ClearTrigValues();

  Int_t trigId() const { return mTrigId; }
  Bool_t shouldFire() const { return mShouldFire; }
  Bool_t didFire() const { return mDidFire; }

  // Setters
  void setTrigId (Int_t aTrigId);
  void setDidFire (Bool_t aDidFire);
  void setShouldFire( Bool_t aShouldFire);

 private:
  Int_t mTrigId;
  Bool_t mShouldFire;
  Bool_t mDidFire;

  ClassDef(StEmcOfflineCalibrationTrigger,1);
};
//----------------------------------------------------------------------
class StEmcOfflineCalibrationVertex : public TObject {
 public:
  StEmcOfflineCalibrationVertex();
  ~StEmcOfflineCalibrationVertex();
  
  Double_t x() const { return mX; }
  Double_t y() const { return mY; }
  Double_t z() const { return mZ; }
  Double_t ranking() const { return mRanking; }

  // Tracks for this vertex
  const TRefArray& tracks() const { return mTracks; }
  Int_t nTracks() const { return mTracks.GetEntriesFast(); }
  StEmcOfflineCalibrationTrack *track(Int_t i) const { return (StEmcOfflineCalibrationTrack*)mTracks.At(i); }
  StEmcOfflineCalibrationTrack* addTrack(StEmcOfflineCalibrationTrack *trk) { mTracks.Add((TObject*)trk); return (StEmcOfflineCalibrationTrack*)mTracks.Last(); }  

  void setRanking(Double_t aRanking);
  void setX(Double_t aX);
  void setY(Double_t aY);
  void setZ(Double_t aZ);

 private:
  Double_t mRanking;
  Double_t mX;
  Double_t mY;
  Double_t mZ;
  TRefArray mTracks;

  ClassDef(StEmcOfflineCalibrationVertex,1);
};
//----------------------------------------------------------------------
class StEmcOfflineCalibrationCluster : public TObject
{
 public:
  StEmcOfflineCalibrationCluster();
  ~StEmcOfflineCalibrationCluster();
  void Clear(Option_t *option="");

  const TRefArray& tracks() const { return mNeighborTracks; }  
  Int_t numberOfNeighborTracks() const { return mNeighborTracks.GetEntriesFast(); }
  StEmcOfflineCalibrationTrack* neighborTrack(Int_t i) const { return (StEmcOfflineCalibrationTrack*)mNeighborTracks.At(i); } 
  StEmcOfflineCalibrationTrack* centralTrack() const { return mCentralTrack; }
  StEmcOfflineCalibrationTrack* addNeighborTrack(StEmcOfflineCalibrationTrack* trk) { mNeighborTracks.Add((TObject*)trk); return (StEmcOfflineCalibrationTrack*)mNeighborTracks.Last(); }

  void setCentralTrack(StEmcOfflineCalibrationTrack*);

 private:
  TRefArray mNeighborTracks;
  StEmcOfflineCalibrationTrack *mCentralTrack;

  ClassDef(StEmcOfflineCalibrationCluster,4);  
};
#endif
