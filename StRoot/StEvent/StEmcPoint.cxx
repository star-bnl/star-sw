/***************************************************************************
 *
 * $Id: StEmcPoint.cxx,v 2.12 2013/01/15 23:21:05 fisyak Exp $
 *
 * Author: Akio Ogawa, Jan 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEmcPoint.cxx,v $
 * Revision 2.12  2013/01/15 23:21:05  fisyak
 * improve printouts
 *
 * Revision 2.11  2012/10/23 20:18:33  fisyak
 * Add/modify print outs
 *
 * Revision 2.10  2005/07/19 21:31:45  perev
 * IdTruth
 *
 * Revision 2.9  2004/07/20 17:07:49  perev
 * Pavlinov corrs for TBrowser
 *
 * Revision 2.8  2004/07/15 16:36:24  ullrich
 * Removed all clone() declerations and definitions. Use StObject::clone() only.
 *
 * Revision 2.7  2004/02/17 21:43:41  ullrich
 * Added code to the constructor (was empty).
 *
 * Revision 2.6  2003/09/02 17:58:05  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 2.5  2001/04/05 04:00:48  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.4  2001/03/24 03:34:45  perev
 * clone() -> clone() const
 *
 * Revision 2.3  2000/07/28 19:49:27  akio
 * Change in Detector Id for Endcap SMD
 *
 * Revision 2.2  2000/05/22 19:21:53  akio
 * Bug fix, add delta into EMcPoint, wider bits for Eta in RawHit
 *
 * Revision 2.1  2000/03/23 22:24:07  akio
 * Initial version of Emc Point, and Inclusion of track pointers
 *
 *
 **************************************************************************/
#include <Stiostream.h>
#include "StEmcPoint.h"
#include "StEmcCluster.h"
static const char rcsid[] = "$Id: StEmcPoint.cxx,v 2.12 2013/01/15 23:21:05 fisyak Exp $";

ClassImp(StEmcPoint)

StEmcPoint::StEmcPoint() { 

    mEnergy = 0;
    mChiSquare = 0;
    for(int i=0;i<4;i++) {
	mEnergyInDetector[i]=0;
	mSizeAtDetector[i]=0;
    }
    mDelta[0]=0;
    mDelta[1]=0;
    myQuality =0;
}

StEmcPoint::StEmcPoint(const StThreeVectorF& p,
                       const StThreeVectorF& e,
                       const StThreeVectorF& s,
                       unsigned int hp, float q,
                       float energy, float csq,
                       unsigned char c)
  : StHit(p,e,hp,q,c),mEnergy(energy), mChiSquare(csq), mSize(s)
{     
  myQuality =0;
}

StEmcPoint::~StEmcPoint() {/* noop */}

float
StEmcPoint::energy()    const {return mEnergy;};

void
StEmcPoint::setEnergy(const float e) {mEnergy = e;};

float
StEmcPoint::chiSquare() const {return mChiSquare;};

void
StEmcPoint::setChiSquare(const float c) {mChiSquare = c;};

StThreeVectorF
StEmcPoint::size() const {return mSize;};

void
StEmcPoint::setSize(const StThreeVectorF& s) {mSize = s;};

int
StEmcPoint::getDetId(const StDetectorId id) const{
  if(id>=kBarrelEmcTowerId && id<=kBarrelSmdPhiStripId){
    return id-kBarrelEmcTowerId;
  }else if(id>=kEndcapEmcTowerId && id<=kEndcapSmdVStripId){
    return id-kEndcapEmcTowerId;
  }else{
    cout<<"***Error at StEmcPoint:::getDetId, Invalid  StDetectorId"<<endl;
    return 0;
  }
}
int
StEmcPoint::getDetId(Int_t id) const{
  if(id>=kBarrelEmcTowerId && id<=kBarrelSmdPhiStripId){
    return id-kBarrelEmcTowerId;
  }else if(id>=kEndcapEmcTowerId && id<=kEndcapSmdVStripId){
    return id-kEndcapEmcTowerId;
  }else{
    cout<<"***Error at StEmcPoint:::getDetId, Invalid  StDetectorId"<<endl;
    return 0;
  }
}

float
StEmcPoint::energyInDetector(const StDetectorId id) const{
  int i = getDetId(id);
  if(i==-1) return 0.0;
  return mEnergyInDetector[i];
}

void
StEmcPoint::setEnergyInDetector(const StDetectorId id, const float e){
  int i = getDetId(id);
  if(i>=0) mEnergyInDetector[i]=e;
}

float
StEmcPoint::sizeAtDetector(const StDetectorId id) const{
  int i = getDetId(id);
  if(i==-1) return 0.0;
  return mSizeAtDetector[i];
}

void
StEmcPoint::setSizeAtDetector(const StDetectorId id, const float s){
  int i = getDetId(id);
  if(i>=0) mSizeAtDetector[i]=s;
}

float StEmcPoint::deltaEta() const{return mDelta[0];}
float StEmcPoint::deltaPhi() const{return mDelta[1];}
float StEmcPoint::deltaU()   const{return mDelta[0];}
float StEmcPoint::deltaV()   const{return mDelta[1];}
void StEmcPoint::setDeltaEta(const float d){mDelta[0]=d;}
void StEmcPoint::setDeltaPhi(const float d){mDelta[1]=d;}
void StEmcPoint::setDeltaU(const float d){mDelta[0]=d;}
void StEmcPoint::setDeltaV(const float d){mDelta[1]=d;}

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
  
StPtrVecEmcCluster&
StEmcPoint::cluster(Int_t id){
  int i = getDetId(id);
  return mCluster[i];
}
  
const StPtrVecEmcCluster&
StEmcPoint::cluster(Int_t id) const{
  int i = getDetId(id);
  return mCluster[i];
}
  
void
StEmcPoint::addCluster(const StDetectorId id, const StEmcCluster* c){
  if(!c) return; // 29-oct-2003 
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

int
StEmcPoint::nTracks() const {return mTracks.size();}

StPtrVecTrack&
StEmcPoint::track() {return mTracks;}

const StPtrVecTrack&
StEmcPoint::track() const {return mTracks;}

void
StEmcPoint::addTrack(StTrack* track) 
{
  if(track) mTracks.push_back(track); // 29-oct-03 - eliminate zero pointer
}

// 11-nov-03 by PAI
void StEmcPoint::print()
{ // for debugging 
  cout << "Point energy " << mEnergy << endl;
  cout << "size         " << mSize   << endl;
  cout << "#tracks " << mTracks.size() << endl;
  for(int det=0; det<4; det++){
    cout << " det " << det << " #clusters " << mCluster[det].size()<<endl;
  }
}

ostream&
operator<<(ostream &os, const StEmcPoint& pnt)
{
  os << "Point Energy " << pnt.energy() 
     << " size "    << pnt.size() << " #tracks " << pnt.nTracks() << endl; 
  Int_t Ids[4] = {kBarrelEmcTowerId, kBarrelEmcPreShowerId, kEndcapEmcTowerId, kEndcapEmcPreShowerId};
  for (Int_t k = 0; k < 4; k++) {
    const StPtrVecEmcCluster &cl = pnt.cluster(Ids[k]);
    Int_t ncl = (Int_t) cl.size();
    for (Int_t i = 0; i < ncl; i++) {
      cout << *cl[i] << endl;
    }
  }
  os << *((StHit *)&pnt);
  return os;
}
void   StEmcPoint::Print(Option_t *option) const {cout << *this << endl;}

