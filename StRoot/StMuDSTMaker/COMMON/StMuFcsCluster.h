/*************************************************************************
 *
 * $Id: StMuFcsCluster.h,v 1.2 2021/11/17 22:09:58 jdb Exp $
 *
 * Author: Daniel Brandenburg, 2021
 *************************************************************************
 *
 * Description: Declaration of StMuFcsCluster, the MuDST FCS cluster class
 *
 *************************************************************************/

#ifndef StMuFcsCluster_hh
#define StMuFcsCluster_hh

#include <TObject.h>
#include <TRefArray.h>
#include <TLorentzVector.h>

class StFcsCluster;  // Equivalent class in StEvent
class StMuFcsPoint;
class StMuFcsHit;

/**
 */
class StMuFcsCluster : public TObject {
 public:
    StMuFcsCluster();
    ~StMuFcsCluster();
    
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
    const TLorentzVector& fourMomentum() const; // Cluster four-momentum (px, py, pz, E)

    void setId(int cluid);
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
    void setFourMomentum(TLorentzVector p4);

    // void  addHit( StMuFcsHit* hit ){ mHits.Add( hit ); }
    TRefArray* hits();
    const TRefArray* hits() const;
    void addNeighbor(StMuFcsCluster* neighbor);
    TRefArray* neighbor();
    const TRefArray* neighbor() const;    
    TRefArray* points();
    const TRefArray* points() const;
    void addPoint(StMuFcsPoint* p);
    void addPoint(StMuFcsPoint* p1, StMuFcsPoint* p2);
    void print(Option_t *option="") const;

private:
    Int_t mId=-1;             // Eventwise cluster ID
    UShort_t mDetectorId=0;   // Detector starts from 1
    Int_t mCategory=0;        // Category of cluster (see StMuFcsClusterCategory)
    Int_t mNTowers=0;         // Number of non-zero-energy tower hits in the cluster
    Float_t mEnergy=0.0;      // Total energy contained in this cluster (0th moment)
    Float_t mX=0.0;  // Mean x ("center of gravity") in local grid coordinate (1st moment)
    Float_t mY=0.0;  // Mean y ("center of gravity") in local grid coordinate (1st moment)
    Float_t mSigmaMin=0.0;        // Minimum 2nd moment
    Float_t mSigmaMax=0.0;        // Maximum 2nd moment (along major axis)
    Float_t mTheta=0.0;           //Angle in x-y plane that defines the direction of least-2nd-sigma
    Float_t mChi2Ndf1Photon=0.0;  // &chi;<sup>2</sup> / ndf for 1-photon fit
    Float_t mChi2Ndf2Photon=0.0;  // &chi;<sup>2</sup> / ndf for 2-photon fit
    TLorentzVector mFourMomentum;  // Cluster four momentum
    TRefArray mHits;            // Tower hits of the current cluster
    TRefArray mNeighbor;        // Neighbor clusters
    TRefArray mPoints;          // Fitted points (photons) in the cluster

  ClassDef(StMuFcsCluster, 1)
};

inline int StMuFcsCluster::id() const { return mId; } // Cluster ID
inline unsigned short StMuFcsCluster::detectorId() const { return mDetectorId; }
inline int StMuFcsCluster::category() const { return mCategory; }
inline int StMuFcsCluster::nTowers() const { return mNTowers; }
inline int StMuFcsCluster::nNeighbor() const { return mNeighbor.GetSize(); }
inline int StMuFcsCluster::nPoints() const { return mPoints.GetSize(); }
inline float StMuFcsCluster::energy() const { return mEnergy; }
inline float StMuFcsCluster::x() const { return mX; } // Mean x ("center of gravity") in local grid coordinate (1st moment).
inline float StMuFcsCluster::y() const { return mY; } // Mean y ("center of gravity") in local grid coordinate (1st moment).
inline float StMuFcsCluster::sigmaMax() const { return mSigmaMax; } // Maximum 2nd moment (along major axis).
inline float StMuFcsCluster::sigmaMin() const { return mSigmaMin; } // Minimum 2nd moment.
inline float StMuFcsCluster::theta() const { return mTheta; } // Angle in x-y plane that defines the direction of least-2nd-sigma
inline float StMuFcsCluster::chi2Ndf1Photon() const { return mChi2Ndf1Photon; } // chi^2/ndf for 1-photon fit to the cluster.
inline float StMuFcsCluster::chi2Ndf2Photon() const { return mChi2Ndf2Photon; } // chi^2/ndf for 2-photon fit to the cluster.
inline const TLorentzVector& StMuFcsCluster::fourMomentum() const { return mFourMomentum; } // Cluster four-momentum (px, py, pz, E)
inline void StMuFcsCluster::setDetectorId(unsigned short detector) { mDetectorId = detector; }
inline void StMuFcsCluster::setCategory(int catag) { mCategory = catag; }
inline void StMuFcsCluster::setNTowers(int numbTower) { mNTowers = numbTower; }
inline void StMuFcsCluster::setEnergy(float energy) { mEnergy = energy; }
inline void StMuFcsCluster::setX(float x0) { mX = x0; }
inline void StMuFcsCluster::setY(float y0) { mY = y0; }
inline void StMuFcsCluster::setSigmaMin(float sigmaMax) { mSigmaMin = sigmaMax; }
inline void StMuFcsCluster::setSigmaMax(float sigmaMax) { mSigmaMax = sigmaMax; }
inline void StMuFcsCluster::setTheta(float theta) { mTheta = theta; }
inline void StMuFcsCluster::setChi2Ndf1Photon(float chi2ndfph1) { mChi2Ndf1Photon = chi2ndfph1; }
inline void StMuFcsCluster::setChi2Ndf2Photon(float chi2ndfph2) { mChi2Ndf2Photon = chi2ndfph2; }
inline void StMuFcsCluster::setId(int cluid) { mId = cluid; }
inline void StMuFcsCluster::setFourMomentum(TLorentzVector p4) { mFourMomentum = p4; }
inline TRefArray* StMuFcsCluster::hits() { return &mHits; }
inline const TRefArray* StMuFcsCluster::hits() const { return &mHits; }
inline TRefArray* StMuFcsCluster::neighbor() { return &mNeighbor; }
inline const TRefArray* StMuFcsCluster::neighbor() const { return &mNeighbor; }
inline TRefArray* StMuFcsCluster::points() { return &mPoints; }
inline const TRefArray* StMuFcsCluster::points() const { return &mPoints; }


#endif  // StMuFcsCluster_hh
