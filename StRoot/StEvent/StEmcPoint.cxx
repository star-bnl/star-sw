/***************************************************************************
 *
 * $Id: StEmcPoint.cxx,v 2.1 2000/03/23 22:24:07 akio Exp $
 *
 * Author: Akio Ogawa, Jan 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEmcPoint.cxx,v $
 * Revision 2.1  2000/03/23 22:24:07  akio
 * Initial version of Emc Point, and Inclusion of track pointers
 *
 *
 **************************************************************************/
#include "StEmcPoint.h"
#include <iostream.h>

static const char rcsid[] = "$Id: StEmcPoint.cxx,v 2.1 2000/03/23 22:24:07 akio Exp $";

ClassImp(StEmcPoint)

StEmcPoint::StEmcPoint() { /* noop */ }

StEmcPoint::StEmcPoint(const StThreeVectorF& p,
		       const StThreeVectorF& e,
		       const StThreeVectorF& s,
		       ULong_t hp, Float_t q, 
		       Float_t energy, Float_t csq,
		       UChar_t c)
  : StHit(p,e,hp,q,c),mEnergy(energy), mChiSquare(csq), mSize(s)
{ /* noop */ }

StEmcPoint::~StEmcPoint() {/* noop */}

Float_t
StEmcPoint::energy()    const {return mEnergy;};

void
StEmcPoint::setEnergy(const Float_t e) {mEnergy = e;};

Float_t
StEmcPoint::chiSquare() const {return mChiSquare;};

void
StEmcPoint::setChiSquare(const Float_t c) {mChiSquare = c;};

StThreeVectorF
StEmcPoint::size() const {return mSize;};

void
StEmcPoint::setSize(const StThreeVectorF& s) {mSize = s;};

Int_t 
StEmcPoint::getDetId(const StDetectorId id) const{
  if(id>=kBarrelEmcTowerId && id<=kBarrelSmdPhiStripId){
    return id-kBarrelEmcTowerId;
  }else if(id>=kEndcapEmcTowerId && id<=kEndcapSmdPhiStripId){
    return id-kEndcapEmcTowerId;
  }else{
    cout<<"***Error at StEmcPoint:::getDetId, Invalid  StDetectorId"<<endl;
    return 0;
  }
}

Float_t
StEmcPoint::energyInDetector(const StDetectorId id) const{
  int i = getDetId(id);
  if(i==-1) return 0.0;
  return mEnergyInDetector[i];
}

void
StEmcPoint::setEnergyInDetector(const StDetectorId id, const Float_t e){
  int i = getDetId(id);
  if(i>=0) mEnergyInDetector[i]=e;
}

Float_t
StEmcPoint::sizeAtDetector(const StDetectorId id) const{
  int i = getDetId(id);
  if(i==-1) return 0.0;
  return mSizeAtDetector[i];
}

void
StEmcPoint::setSizeAtDetector(const StDetectorId id, const Float_t s){
  int i = getDetId(id);
  if(i>=0) mSizeAtDetector[i]=s;
}

StPtrVecEmcCluster& 
StEmcPoint::cluster(const StDetectorId id){
  int i = getDetId(id);
  return mCluster[i];
}
  
const StPtrVecEmcCluster& 
StEmcPoint::cluster(const StDetectorId id) const{
  int i = getDetId(id);
  return mCluster[i];
}
  
void 
StEmcPoint::addCluster(const StDetectorId id, const StEmcCluster* c){
  int i = getDetId(id);
  if(i>=0) mCluster[i].push_back(c);
}

StPtrVecEmcPoint&
StEmcPoint::neighbor(){
 return mNeighbors;
}

const StPtrVecEmcPoint&
StEmcPoint::neighbor()const {
 return mNeighbors;
}
  
void
StEmcPoint::addNeighbor(const StEmcPoint* p){
  mNeighbors.push_back(p);
}

Int_t
StEmcPoint::nTracks() const {return mTracks.size();}

StPtrVecTrack&
StEmcPoint::track() {return mTracks;}

const StPtrVecTrack&
StEmcPoint::track() const {return mTracks;}

void
StEmcPoint::addTrack(StTrack* track) {mTracks.push_back(track);}

StObject* StEmcPoint::clone() { return new StEmcPoint(*this); };



