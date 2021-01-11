/****************************************************************************
 *
 * $Id: StFcsCluster.h,v 2.1 2021/01/11 20:25:37 ullrich Exp $
 *
 * Author: Akio Ogawa 2018
 ****************************************************************************
 *
 * Description: Implementation of StFcsCluster, a group ofadjacent FCS towers.
 * A cluster is formed by the energy depositions (hits) of one or more
 * particles over a group of FCS towers.
 ****************************************************************************
 *
 * $Log: StFcsCluster.h,v $
 * Revision 2.1  2021/01/11 20:25:37  ullrich
 * Initial Revision
 *
 ****************************************************************************/
#ifndef STFCSCLUSTER_H
#define STFCSCLUSTER_H

#include "StLorentzVectorD.hh"

#include "StObject.h"

#include "StContainers.h"  // For StPtrVecFcsHit, StPtrVecFcsPoint
#include "StEnumerations.h"

class StFcsPoint;

class StFcsCluster : public StObject {
public:
    StFcsCluster();
    ~StFcsCluster();
    
    int id() const; // Cluster ID
    unsigned short detectorId() const;
    int category() const;
    int nTowers() const;
    int nNeighbor() const;
    int nPoints() const;
    float energy() const;
    float x() const;  // Mean x ("center of gravity") in local grid coordinate (1st moment).
    float y() const;  // Mean y ("center of gravity") in local grid coordinate (1st moment).
    float sigmaMax() const; // Maximum 2nd moment (along major axis).
    float sigmaMin() const; // Minimum 2nd moment.
    float theta() const;    // Angle in x-y plane that defines the direction of least-2nd-sigma
    float chi2Ndf1Photon() const; // chi^2/ndf for 1-photon fit to the cluster.
    float chi2Ndf2Photon() const; // chi^2/ndf for 2-photon fit to the cluster.
    const StLorentzVectorD& fourMomentum() const; // Cluster four-momentum (px, py, pz, E)

    void setId(float cluid);
    void setDetectorId(unsigned short detector);
    void setCategory(int catag);
    void setNTowers(int numbTower);
    void setEnergy(float energy);
    void setX(float x0);
    void setY(float y0);
    void setSigmaMin(float sigmaMax);
    void setSigmaMax(float sigmaMax);
    void setTheta(float theta);
    void setChi2Ndf1Photon(float chi2ndfph1);
    void setChi2Ndf2Photon(float chi2ndfph2);
    void setFourMomentum(StLorentzVectorD p4);
    StPtrVecFcsHit& hits();
    const StPtrVecFcsHit& hits() const;
    void addNeighbor(StFcsCluster* neighbor);
    StPtrVecFcsCluster& neighbor();
    const StPtrVecFcsCluster& neighbor() const;    
    StPtrVecFcsPoint& points();
    const StPtrVecFcsPoint& points() const;
    void addPoint(StFcsPoint* p);
    void addPoint(StFcsPoint* p1, StFcsPoint* p2);
    void print(Option_t *option="") const;

private:
    Int_t mId=-1;             // Eventwise cluster ID
    UShort_t mDetectorId=0;   // Detector starts from 1
    Int_t mCategory=0;        // Category of cluster (see StFcsClusterCategory)
    Int_t mNTowers=0;         // Number of non-zero-energy tower hits in the cluster
    Float_t mEnergy=0.0;      // Total energy contained in this cluster (0th moment)
    Float_t mX=0.0;  // Mean x ("center of gravity") in local grid coordinate (1st moment)
    Float_t mY=0.0;  // Mean y ("center of gravity") in local grid coordinate (1st moment)
    Float_t mSigmaMin=0.0;        // Minimum 2nd moment
    Float_t mSigmaMax=0.0;        // Maximum 2nd moment (along major axis)
    Float_t mTheta=0.0;           //Angle in x-y plane that defines the direction of least-2nd-sigma
    Float_t mChi2Ndf1Photon=0.0;  // &chi;<sup>2</sup> / ndf for 1-photon fit
    Float_t mChi2Ndf2Photon=0.0;  // &chi;<sup>2</sup> / ndf for 2-photon fit
    StLorentzVectorD mFourMomentum;  // Cluster four momentum
    StPtrVecFcsHit mHits;            // Tower hits of the current cluster
    StPtrVecFcsCluster mNeighbor;    // Neighbor clusters
    StPtrVecFcsPoint mPoints;        // Fitted points (photons) in the cluster

    ClassDef(StFcsCluster, 2)
};


inline int StFcsCluster::id() const { return mId; } // Cluster ID
inline unsigned short StFcsCluster::detectorId() const { return mDetectorId; }
inline int StFcsCluster::category() const { return mCategory; }
inline int StFcsCluster::nTowers() const { return mNTowers; }
inline int StFcsCluster::nNeighbor() const { return mNeighbor.size(); }
inline int StFcsCluster::nPoints() const { return mPoints.size(); }
inline float StFcsCluster::energy() const { return mEnergy; }
inline float StFcsCluster::x() const { return mX; } // Mean x ("center of gravity") in local grid coordinate (1st moment).
inline float StFcsCluster::y() const { return mY; } // Mean y ("center of gravity") in local grid coordinate (1st moment).
inline float StFcsCluster::sigmaMax() const { return mSigmaMax; } // Maximum 2nd moment (along major axis).
inline float StFcsCluster::sigmaMin() const { return mSigmaMin; } // Minimum 2nd moment.
inline float StFcsCluster::theta() const { return mTheta; } // Angle in x-y plane that defines the direction of least-2nd-sigma
inline float StFcsCluster::chi2Ndf1Photon() const { return mChi2Ndf1Photon; } // chi^2/ndf for 1-photon fit to the cluster.
inline float StFcsCluster::chi2Ndf2Photon() const { return mChi2Ndf2Photon; } // chi^2/ndf for 2-photon fit to the cluster.
inline const StLorentzVectorD& StFcsCluster::fourMomentum() const { return mFourMomentum; } // Cluster four-momentum (px, py, pz, E)
inline void StFcsCluster::setDetectorId(unsigned short detector) { mDetectorId = detector; }
inline void StFcsCluster::setCategory(int catag) { mCategory = catag; }
inline void StFcsCluster::setNTowers(int numbTower) { mNTowers = numbTower; }
inline void StFcsCluster::setEnergy(float energy) { mEnergy = energy; }
inline void StFcsCluster::setX(float x0) { mX = x0; }
inline void StFcsCluster::setY(float y0) { mY = y0; }
inline void StFcsCluster::setSigmaMin(float sigmaMax) { mSigmaMin = sigmaMax; }
inline void StFcsCluster::setSigmaMax(float sigmaMax) { mSigmaMax = sigmaMax; }
inline void StFcsCluster::setTheta(float theta) { mTheta = theta; }
inline void StFcsCluster::setChi2Ndf1Photon(float chi2ndfph1) { mChi2Ndf1Photon = chi2ndfph1; }
inline void StFcsCluster::setChi2Ndf2Photon(float chi2ndfph2) { mChi2Ndf2Photon = chi2ndfph2; }
inline void StFcsCluster::setId(float cluid) { mId = cluid; }
inline void StFcsCluster::setFourMomentum(StLorentzVectorD p4) { mFourMomentum = p4; }
inline StPtrVecFcsHit& StFcsCluster::hits() { return mHits; }
inline const StPtrVecFcsHit& StFcsCluster::hits() const { return mHits; }
inline StPtrVecFcsPoint& StFcsCluster::points() { return mPoints; }
inline const StPtrVecFcsPoint& StFcsCluster::points() const { return mPoints; }

#endif  // STFCSCLUSTER_H
