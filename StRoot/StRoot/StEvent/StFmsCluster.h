/****************************************************************************
 *
 * $Id: StFmsCluster.h,v 2.2 2015/08/26 16:51:59 ullrich Exp $
 *
 * Author: Thomas Burton, Yuxi Pan, 2014
 ****************************************************************************
 *
 * Description: Implementation of StFmsCluster, a group of
 *              adjacent FMS towers.
 * A cluster is formed by the energy depositions (hits) of one or more
 * particles over a group of FMS towers.
 ****************************************************************************
 *
 * $Log: StFmsCluster.h,v $
 * Revision 2.2  2015/08/26 16:51:59  ullrich
 * Added print out fct and operator.
 *
 * Revision 2.1  2015/02/14 18:56:00  ullrich
 * Initial Revision.
 *
 *
 ****************************************************************************/
#ifndef STFMSCLUSTER_H
#define STFMSCLUSTER_H

#include "StLorentzVectorF.hh"

#include "StObject.h"

#include "StContainers.h"  // For StPtrVecFmsHit, StPtrVecFmsPoint
#include "StEnumerations.h"


class StFmsCluster : public StObject {
public:
    StFmsCluster();
    ~StFmsCluster();
    
    unsigned short detectorId() const;
    int category() const;
    int nTowers() const;
    int nPhotons() const;
    float energy() const;
    float x() const;  // Mean x ("center of gravity") in local grid coordinate (1st moment).
    float y() const;  // Mean y ("center of gravity") in local grid coordinate (1st moment).
    float sigmaMax() const; // Maximum 2nd moment (along major axis).
    float sigmaMin() const; // Minimum 2nd moment.
    float chi2Ndf1Photon() const; // chi^2/ndf for 1-photon fit to the cluster.
    float chi2Ndf2Photon() const; // chi^2/ndf for 2-photon fit to the cluster.
    int id() const; // Cluster ID
    const StLorentzVectorF& fourMomentum() const; // Cluster four-momentum (px, py, pz, E)
    void setDetectorId(unsigned short detector);
    void setCategory(int catag);
    void setNTowers(int numbTower);
    void setEnergy(float energy);
    void setX(float x0);
    void setY(float y0);
    void setSigmaMin(float sigmaMax);
    void setSigmaMax(float sigmaMax);
    void setChi2Ndf1Photon(float chi2ndfph1);
    void setChi2Ndf2Photon(float chi2ndfph2);
    void setId(float cluid);
    void setFourMomentum(StLorentzVectorF p4);
    StPtrVecFmsHit& hits();
    const StPtrVecFmsHit& hits() const;
    StPtrVecFmsPoint& points();
    const StPtrVecFmsPoint& points() const;
   
    void print(Option_t *option="") const;

private:
    UShort_t mDetectorId;  // Detector starts from 1
    Int_t mCategory;  // Category of cluster (see StFmsClusterCategory)
    Int_t mNTowers;  // Number of non-zero-energy tower hits in the cluster
    Float_t mEnergy;  // Total energy contained in this cluster (0th moment)
    Float_t mX;  // Mean x ("center of gravity") in local grid coordinate (1st moment)
    Float_t mY;  // Mean y ("center of gravity") in local grid coordinate (1st moment)
    Float_t mSigmaMin;  // Minimum 2nd moment
    Float_t mSigmaMax;  // Maximum 2nd moment (along major axis)
    Float_t mChi2Ndf1Photon;  // &chi;<sup>2</sup> / ndf for 1-photon fit
    Float_t mChi2Ndf2Photon;  // &chi;<sup>2</sup> / ndf for 2-photon fit
    Int_t mId;  // Eventwise cluster ID
    StLorentzVectorF mFourMomentum;  // Cluster four momentum
    StPtrVecFmsPoint mPhotons; // Fitted points (photons) in the cluster
    StPtrVecFmsHit mHits;  // Tower hits of the current cluster

    ClassDef(StFmsCluster, 1)
};

ostream& operator<<(ostream&, const StFmsCluster&);

//
//   Inline functions
//
inline unsigned short StFmsCluster::detectorId() const { return mDetectorId; }
inline int StFmsCluster::category() const { return mCategory; }
inline int StFmsCluster::nTowers() const { return mNTowers; }
inline int StFmsCluster::nPhotons() const { return mPhotons.size(); }
inline float StFmsCluster::energy() const { return mEnergy; }
inline float StFmsCluster::x() const { return mX; } // Mean x ("center of gravity") in local grid coordinate (1st moment).
inline float StFmsCluster::y() const { return mY; } // Mean y ("center of gravity") in local grid coordinate (1st moment).
inline float StFmsCluster::sigmaMax() const { return mSigmaMax; } // Maximum 2nd moment (along major axis).
inline float StFmsCluster::sigmaMin() const { return mSigmaMin; } // Minimum 2nd moment.
inline float StFmsCluster::chi2Ndf1Photon() const { return mChi2Ndf1Photon; } // chi^2/ndf for 1-photon fit to the cluster.
inline float StFmsCluster::chi2Ndf2Photon() const { return mChi2Ndf2Photon; } // chi^2/ndf for 2-photon fit to the cluster.
inline int StFmsCluster::id() const { return mId; } // Cluster ID
inline const StLorentzVectorF& StFmsCluster::fourMomentum() const { return mFourMomentum; } // Cluster four-momentum (px, py, pz, E)
inline void StFmsCluster::setDetectorId(unsigned short detector) { mDetectorId = detector; }
inline void StFmsCluster::setCategory(int catag) { mCategory = catag; }
inline void StFmsCluster::setNTowers(int numbTower) { mNTowers = numbTower; }
inline void StFmsCluster::setEnergy(float energy) { mEnergy = energy; }
inline void StFmsCluster::setX(float x0) { mX = x0; }
inline void StFmsCluster::setY(float y0) { mY = y0; }
inline void StFmsCluster::setSigmaMin(float sigmaMax) { mSigmaMin = sigmaMax; }
inline void StFmsCluster::setSigmaMax(float sigmaMax) { mSigmaMax = sigmaMax; }
inline void StFmsCluster::setChi2Ndf1Photon(float chi2ndfph1) { mChi2Ndf1Photon = chi2ndfph1; }
inline void StFmsCluster::setChi2Ndf2Photon(float chi2ndfph2) { mChi2Ndf2Photon = chi2ndfph2; }
inline void StFmsCluster::setId(float cluid) { mId = cluid; }
inline void StFmsCluster::setFourMomentum(StLorentzVectorF p4) { mFourMomentum = p4; }
inline StPtrVecFmsHit& StFmsCluster::hits() { return mHits; }
inline const StPtrVecFmsHit& StFmsCluster::hits() const { return mHits; }
inline StPtrVecFmsPoint& StFmsCluster::points() { return mPhotons; }
inline const StPtrVecFmsPoint& StFmsCluster::points() const { return mPhotons; }


#endif  // STFMSCLUSTER_H
