/***************************************************************************
 *
 * $Id: StEmcPoint.h,v 2.2 2000/05/22 19:21:54 akio Exp $
 *
 * Author: Akio Ogawa, Mar 2000
 ***************************************************************************
 *
 * Description: Base class for electromagnetic calorimeter Point
 *
 ***************************************************************************
 *
 * $Log: StEmcPoint.h,v $
 * Revision 2.2  2000/05/22 19:21:54  akio
 * Bug fix, add delta into EMcPoint, wider bits for Eta in RawHit
 *
 * Revision 2.1  2000/03/23 22:24:07  akio
 * Initial version of Emc Point, and Inclusion of track pointers
 *
 *
 **************************************************************************/
#ifndef StEmcPoint_hh
#define StEmcPoint_hh

#include "StHit.h"
#include "StEnumerations.h"

class StEmcPoint : public StHit {
public:
  StEmcPoint();
  StEmcPoint(const StThreeVectorF&,
 	     const StThreeVectorF&,
 	     const StThreeVectorF&,
  	     ULong_t, Float_t, 
	     Float_t, Float_t,
	     UChar_t = 0);
  ~StEmcPoint();
  
  Float_t   energy() const;
  Float_t   chiSquare() const;
  void setEnergy(const Float_t);
  void setChiSquare(const Float_t);
  StThreeVectorF size() const;
  void setSize(const StThreeVectorF&);

  Float_t   energyInDetector(const StDetectorId) const;
  Float_t   sizeAtDetector(const StDetectorId) const;
  void setEnergyInDetector(const StDetectorId, const Float_t);
  void setSizeAtDetector(const StDetectorId, const Float_t);

  Float_t deltaEta() const;
  Float_t deltaPhi() const;
  Float_t deltaU() const;
  Float_t deltaV() const;
  void setDeltaEta(const Float_t);
  void setDeltaPhi(const Float_t);
  void setDeltaU(const Float_t);
  void setDeltaV(const Float_t);

  StPtrVecEmcCluster& cluster(const StDetectorId);
  const StPtrVecEmcCluster& cluster(const StDetectorId) const;
  void addCluster(const StDetectorId, const StEmcCluster*);

  StPtrVecEmcPoint& neighbor();
  const StPtrVecEmcPoint& neighbor() const;
  void addNeighbor(const StEmcPoint*);

  Int_t   nTracks() const;
  StPtrVecTrack&  track();
  const StPtrVecTrack& track() const;
  void addTrack(StTrack*);

protected:
  Float_t            mEnergy;
  Float_t            mChiSquare;
  StThreeVectorF     mSize;
  Float_t            mEnergyInDetector[4];
  Float_t            mSizeAtDetector[4];
  Float_t            mDelta[2];
  StPtrVecEmcCluster mCluster[4];
  StPtrVecEmcPoint   mNeighbors;
  StPtrVecTrack      mTracks;

  Int_t getDetId(const StDetectorId) const;
  StObject* clone();
  ClassDef(StEmcPoint,1)  
};
#endif


