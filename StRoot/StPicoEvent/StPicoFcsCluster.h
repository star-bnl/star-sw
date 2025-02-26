/***************************************************************************
 *
 * Author: jdb, Feb 2022
 ***************************************************************************
 *
 * Description: StPicoFcsCluster stores the Fcs Clusters
 *
 **************************************************************************/
#ifndef StPicoFcsCluster_hh
#define StPicoFcsCluster_hh

#include <TObject.h>
#include <vector>
#include "TVector3.h"
#include "TLorentzVector.h"


class StPicoFcsCluster : public TObject {

public:
    /// Constructor
    StPicoFcsCluster(  );
    /// Copy constructor
    StPicoFcsCluster(const StPicoFcsCluster &fwdTrack);
    /// Destructor
    virtual ~StPicoFcsCluster();

    virtual void Print(const Char_t *option = "") const;
    UShort_t  index() const              { return mIndex; }
    /// Return unique Id of the track
    UShort_t   id() const              { return mId; }
    unsigned short detectorId() const { return mDetectorId; }
    int category() const { return mCategory; }
    int nTowers() const { return mNTowers; }
    float energy() const { return mFourMomentumT; } // Energy
    float x() const { return mX; }  // Mean x ("center of gravity") in local grid coordinate (1st moment).
    float y() const { return mY; }  // Mean y ("center of gravity") in local grid coordinate (1st moment).
    float sigmaMax() const { return mSigmaMax; } // Maximum 2nd moment (along major axis).
    float sigmaMin() const { return mSigmaMin; } // Minimum 2nd moment.
    float theta() const { return mTheta; }    // Angle in x-y plane that defines the direction of least-2nd-sigma
    float chi2Ndf1Photon() const { return mChi2Ndf1Photon; } // chi^2/ndf for 1-photon fit to the cluster.
    float chi2Ndf2Photon() const { return mChi2Ndf2Photon; } // chi^2/ndf for 2-photon fit to the cluster.
    const TLorentzVector fourMomentum() const { return TLorentzVector( mFourMomentumX, mFourMomentumY, mFourMomentumZ, mFourMomentumT ); } // Cluster four-momentum (px, py, pz, E)

    void setIndex(int index) { mIndex = (UShort_t)index; }
    void setId(int cluid) { mId = (UShort_t)cluid; }
    void setDetectorId(unsigned short detector) { mDetectorId=(UShort_t)detector; }
    void setCategory(int catag) { mCategory = catag; }
    void setNTowers(int numbTower) { mNTowers = (UShort_t)numbTower; }
    void setEnergy(float energy) { mFourMomentumT = energy; }
    void setX(float x0) { mX = x0; }
    void setY(float y0) { mY = y0; }
    void setSigmaMin(float sigmaMin) { mSigmaMin = sigmaMin; }
    void setSigmaMax(float sigmaMax) { mSigmaMax = sigmaMax; }
    void setTheta(float theta) { mTheta = theta; }
    void setChi2Ndf1Photon(float chi2ndfph1) { mChi2Ndf1Photon = chi2ndfph1; }
    void setChi2Ndf2Photon(float chi2ndfph2) { mChi2Ndf2Photon = chi2ndfph2;}
    void setFourMomentum(float px, float py, float pz, float e) { mFourMomentumX = px; mFourMomentumY = py; mFourMomentumZ = pz; mFourMomentumT = e; }
    void setFourMomentum(TLorentzVector p4) { mFourMomentumX = p4.X(); mFourMomentumY = p4.Y(); mFourMomentumZ = p4.Z(); mFourMomentumT = p4.T(); }

protected:
    UShort_t mIndex=0;         // Eventwise cluster Index (unique across all detectors)
    UShort_t mId=0;             // Eventwise cluster ID
    UShort_t mDetectorId=0;   // Detector starts from 1
    Int_t mCategory=0;        // Category of cluster (see StMuFcsClusterCategory)
    UShort_t mNTowers=0;         // Number of non-zero-energy tower hits in the cluster
    Float_t mX=0.0;  // Mean x ("center of gravity") in local grid coordinate (1st moment)
    Float_t mY=0.0;  // Mean y ("center of gravity") in local grid coordinate (1st moment)
    Float_t mSigmaMin=0.0;        // Minimum 2nd moment
    Float_t mSigmaMax=0.0;        // Maximum 2nd moment (along major axis)
    Float_t mTheta=0.0;           //Angle in x-y plane that defines the direction of least-2nd-sigma
    Float_t mChi2Ndf1Photon=0.0;  // &chi;<sup>2</sup> / ndf for 1-photon fit
    Float_t mChi2Ndf2Photon=0.0;  // &chi;<sup>2</sup> / ndf for 2-photon fit
    Float_t mFourMomentumX=0.0;
    Float_t mFourMomentumY=0.0;
    Float_t mFourMomentumZ=0.0;
    Float_t mFourMomentumT=0.0;

    ClassDef(StPicoFcsCluster, 2)
};

#endif

