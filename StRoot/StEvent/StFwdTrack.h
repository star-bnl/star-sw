/***************************************************************************
 *
 * $Id: StFwdTrack.h,v 2.1 2021/01/11 20:25:37 ullrich Exp $
 *
 * Author: jdb, Feb 2022
 ***************************************************************************
 *
 * Description: StFwdTrack stores the Forward tracks built from Fst and Ftt
 *
 ***************************************************************************
 *
 * $Log: StFwdTrack.h,v $
 * Revision 2.1  2021/01/11 20:25:37  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StFwdTrack_hh
#define StFwdTrack_hh

#include "Stiostream.h"
#include "StObject.h"
#include <vector>
#include "StThreeVectorD.hh"
#include "StContainers.h"
#include <climits>

class StFcsCluster;


struct StFwdTrackProjection : public StObject {
    StFwdTrackProjection() {}
    StFwdTrackProjection ( const StFwdTrackProjection & other) {
        mXYZ = other.mXYZ;
        mMom = other.mMom;
        mDetId = other.mDetId;
        memcpy( mCov, other.mCov, sizeof( mCov ) );
    }
    StFwdTrackProjection(   unsigned short detId, 
                            StThreeVectorD xyz, 
                            StThreeVectorD mom, 
                            float c[9] ) {
        set( detId, xyz, mom, c );
    }

    void set(   unsigned short detId, 
                StThreeVectorD xyz, 
                StThreeVectorD mom, 
                float c[9]) {
        mDetId = detId;
        mXYZ = xyz;
		mMom = mom;
        memcpy( mCov, c, sizeof(mCov) ); 
    }
    void set(const StFwdTrackProjection &other ){
        mDetId = other.mDetId;
        mXYZ   = other.mXYZ;
        mMom   = other.mMom;
        memcpy( mCov, other.mCov, sizeof(mCov) ); 
    }
    StThreeVectorD mXYZ;
    StThreeVectorD mMom;
    unsigned char mDetId;
    float mCov[9];

    float dx(){
        return sqrt( mCov[0] );
    }
    float dy(){
        return sqrt( mCov[4] );
    }
    float dz(){
        return sqrt( mCov[8] );
    }

    ClassDef(StFwdTrackProjection, 1)
};

struct StFwdTrackSeedPoint : public StObject {
    StFwdTrackSeedPoint() {}
    StFwdTrackSeedPoint(    StThreeVectorD xyz, 
                            short detsec, 
                            unsigned short trackId, 
                            float cov[9] ){
        mXYZ = xyz;
        mSector = detsec;
        mTrackId = trackId;
        memcpy( mCov, cov, sizeof( mCov ));
    }

    short detectorId() const { return mSector / 10; }
    short sector() const { return mSector % 10; }
    
    StThreeVectorD mXYZ;
    unsigned short mTrackId;
    short mSector; // = detId * 10 + sector
    float mCov[9];
    
    ClassDef(StFwdTrackSeedPoint, 1)
};

class StFwdTrack : public StObject {

public:
    StFwdTrack(  );
    // dtor needed for releasing associations
    ~StFwdTrack(  );

    vector<StFwdTrackProjection> mProjections;
    vector<StFwdTrackSeedPoint> mFTTPoints;
    vector<StFwdTrackSeedPoint> mFSTPoints;

    StFwdTrackProjection getProjectionFor(  int detectorId, 
                            size_t index = 0 );

    StThreeVectorD momentum() const;
    StThreeVectorD momentumAt(size_t _id = 0) const;
    char charge() const;


     // Quality of the fit
    bool   didFitConverge() const;
    bool   didFitConvergeFully() const;
    short    numberOfFailedPoints() const;
    double chi2() const;
    double ndf() const;
    double pval() const;

    // error on pT upper and lower 1-sigma values
    // add access to cov matrix

    // Number of fit points used by GenFit
    short   numberOfFitPoints() const;
    
    // Number of points used in the track seed step
    short   numberOfSeedPoints() const;
    UShort_t idTruth() const { return mIdTruth; }
    UShort_t qaTruth() const { return mQATruth; }
    StThreeVectorD dca() const { return StThreeVectorD( mDCA[0], mDCA[1], mDCA[2] ); }
    UChar_t vertexIndex() const {
        // extract bits 7…2:
        return (mVtxIndex >> 2) & 0x3F;
    }
    UChar_t trackType() const {
        // extract bits 1…0:
        return mVtxIndex & 0x03;
    }
    UChar_t vertexIndexRaw() const { return mVtxIndex; }
    UShort_t globalTrackIndex() const { return mGlobalTrackIndex; }

    bool isGlobalTrack() const { return (trackType() == StFwdTrack::kGlobal); }
    bool isBeamLineConstrainedTrack() const { return (trackType() == StFwdTrack::kBeamlineConstrained); }
    bool isPrimaryTrack() const { return (trackType() == StFwdTrack::kPrimaryVertexConstrained); }
    bool isFwdVertexConstrainedTrack() const { return (trackType() == StFwdTrack::kForwardVertexConstrained); }

    void setPrimaryMomentum( StThreeVectorD mom ) { mPrimaryMomentum = mom; }
    void setDidFitConverge( bool lDidFitConverge ) { mDidFitConverge = lDidFitConverge; }
    void setDidFitConvergeFully( bool lDidFitConvergeFully ) { mDidFitConvergeFully = lDidFitConvergeFully;}
    void setNumberOfFailedPoints( short lNumberOfFailedPoints ) { mNumberOfFailedPoints = lNumberOfFailedPoints;}
    void setNumberOfSeedPoints( short lNumberOfSeedPoints ) { mNumberOfSeedPoints = lNumberOfSeedPoints;}
    void setNumberOfFitPoints( short lNumberOfFitPoints ) { mNumberOfFitPoints = lNumberOfFitPoints;}
    void setChi2( float lChi2 ) { mChi2 = lChi2;}
    void setNDF( float lNDF ) { mNDF = lNDF;}
    void setPval( float lPval ) { mPval = lPval;}
    void setCharge( short  lCharge ) { mCharge = lCharge;}
    void setMc( UShort_t idt, UShort_t qual ) { mIdTruth = idt; mQATruth = qual; }
    void setDCA( StThreeVectorD dca ) { mDCA[0] = dca.x(); mDCA[1] = dca.y(); mDCA[2] = dca.z(); }
    void setDCA( float dcaX, float dcaY, float dcaZ ) { mDCA[0] = dcaX; mDCA[1] = dcaY; mDCA[2] = dcaZ; }
    void setVtxIndex( UChar_t vtxIndex ) { mVtxIndex = pack6and2( vtxIndex, trackType() ); }
    void setTrackType( UChar_t trackType ) { mVtxIndex = pack6and2( vertexIndex(), trackType ); }
    void setVtxIndexAndTrackType( UChar_t vtxIndex, UChar_t trackType ) { mVtxIndex = pack6and2( vtxIndex, trackType ); }
    void setGlobalTrackIndex( UShort_t index ) { mGlobalTrackIndex = index; }

    // ECAL clusters
    StPtrVecFcsCluster& ecalClusters();
    const StPtrVecFcsCluster& ecalClusters() const;
    void addEcalCluster(StFcsCluster* p);
    void sortEcalClusterByET();
    // HCAL clusters
    StPtrVecFcsCluster& hcalClusters();
    const StPtrVecFcsCluster& hcalClusters() const;
    void addHcalCluster(StFcsCluster* p);
    void sortHcalClusterByET();

    enum StFwdTrackType { kGlobal=0, kBeamlineConstrained=1, kPrimaryVertexConstrained=2, kForwardVertexConstrained=3 };

    static unsigned char inline pack6and2(unsigned int A, unsigned int B) {
        // mask to ensure they fit:
        A &= 0x3F;       // 0x3F = 0b00111111
        B &= 0x03;       // 0x03 = 0b00000011

        // put A in the **high** 6 bits, B in the **low** 2 bits
        return static_cast<unsigned char>((A << 2) | B);
    }

protected:

    // Track quality and convergence
    bool mDidFitConverge; // did the fit converge
    bool mDidFitConvergeFully; // did the fit converge fully (fwd and bkw)
    short mNumberOfFailedPoints; // number of points that failed to converge
    short mNumberOfSeedPoints; // number of points used in the seed step
    short mNumberOfFitPoints; // number of points used in the fit (seed + vertex)
    float mChi2; // chi2 of the fit
    float mNDF; // number of degrees of freedom
    float mPval; // p-value of the fit
    short mCharge; // charge of the track
    StThreeVectorD mPrimaryMomentum; // momentum at the primary vertex
    StPtrVecFcsCluster mEcalClusters; // ECAL clusters
    StPtrVecFcsCluster mHcalClusters; // HCAL clusters
    
    UShort_t mIdTruth; // MC track id
    UShort_t mQATruth; // MC track quality (percentage of hits coming from corresponding MC track)

    float mDCA[3]; // DCA to the primary vertex
    // vtx index is used to pack the vertex index and the track type 
    UChar_t mVtxIndex;
    UShort_t mGlobalTrackIndex;
    
    ClassDef(StFwdTrack,4)
};

#endif

