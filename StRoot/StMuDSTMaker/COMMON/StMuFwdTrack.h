/***************************************************************************
 *
 * Author: jdb, Feb 2022
 ***************************************************************************
 *
 * Description: StMuFwdTrack stores the Forward tracks built from Fst and Ftt
 *
 **************************************************************************/
#ifndef StMuFwdTrack_hh
#define StMuFwdTrack_hh

#include <TObject.h>
#include <vector>
#include "TVector3.h"
#include "TRefArray.h"

#include "StMuFcsCluster.h"

class StFwdTrack;
struct StMuFwdTrackProjection : public TObject {
    StMuFwdTrackProjection() {}
    StMuFwdTrackProjection ( const StMuFwdTrackProjection & other) {
        mXYZ = other.mXYZ;
        mMom = other.mMom;
        mDetId = other.mDetId;
        memcpy( mCov, other.mCov, sizeof( mCov ) );
    }
    StMuFwdTrackProjection(   unsigned short detId, 
                            TVector3 xyz, 
                            TVector3 mom, 
                            float c[9] ) {
        set( detId, xyz, mom, c );
    }

    void set(   unsigned short detId, 
                TVector3 xyz, 
                TVector3 mom, 
                float c[9]) {
        mDetId = detId;
        mXYZ = xyz;
        mMom = mom;
        memcpy( mCov, c, sizeof(mCov) ); 
    }
    void set(   StMuFwdTrackProjection &other ){
        mDetId = other.mDetId;
        mXYZ   = other.mXYZ;
        mMom   = other.mMom;
        memcpy( mCov, other.mCov, sizeof(mCov) ); 
    }
    TVector3 mXYZ;
	TVector3 mMom;
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

    ClassDef(StMuFwdTrackProjection, 1)
};

class StFwdTrackSeedPoint;
struct StMuFwdTrackSeedPoint : public TObject{
    StMuFwdTrackSeedPoint() {}
    StMuFwdTrackSeedPoint(    TVector3 xyz, 
                            short sec, 
                            unsigned short trackId, 
                            float cov[9] ){
        set( xyz, sec, trackId, cov );
    }
    StMuFwdTrackSeedPoint( StFwdTrackSeedPoint *sp ){
        set( sp );
    }

    // setter
    void set(  TVector3 xyz, 
                short sec, 
                unsigned short trackId, 
                float cov[9] ){
        mXYZ = xyz;
        mSector = sec;
        mTrackId = trackId;
        memcpy( mCov, cov, sizeof( mCov ));
    }
    // setter from StFwdTrackSeedPoint
    void set( StFwdTrackSeedPoint *sp );

    // copy ctor
    StMuFwdTrackSeedPoint( const StMuFwdTrackSeedPoint & other ){
        mXYZ = other.mXYZ;
        mSector = other.mSector;
        mTrackId = other.mTrackId;
        memcpy( mCov, other.mCov, sizeof( mCov ) );
    }
    
    TVector3 mXYZ;
    unsigned short mTrackId;
    short mSector;
    float mCov[9];
    ClassDef(StMuFwdTrackSeedPoint,1)
};

class StMuFwdTrack : public TObject {

public:
    StMuFwdTrack(  );

    void set( StFwdTrack* );

    std::vector<StMuFwdTrackProjection> mProjections;
    std::vector<StMuFwdTrackSeedPoint> mFTTPoints;
    std::vector<StMuFwdTrackSeedPoint> mFSTPoints;

    bool getProjectionFor(  int detectorId, 
                            StMuFwdTrackProjection &rProj, 
                            size_t index = 0 );

    TVector3 momentum() const;
    TVector3 momentumAt(size_t _id = 0) const;
    char charge() const;


     // Quality of the fit
    bool   didFitConverge() const;
    bool   didFitConvergeFully() const;
    short  numberOfFailedPoints() const;
    double chi2() const;
    double ndf() const;
    double pval() const;

    // Number of fit points used by GenFit
    short   numberOfFitPoints() const;
    // unsigned int   numberOfPossibleFitPoints() const;

    // Number of points used in the track seed step
    short   numberOfSeedPoints() const;
    UShort_t idTruth() const { return mIdTruth; }
    UShort_t qaTruth() const { return mQATruth; }
    TVector3 dca() const { return TVector3( mDCAXY, mDCAXY, mDCAZ ); }


    void setPrimaryMomentum( TVector3 mom ) { mPrimaryMomentum = mom; }
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
    void setDCA( float dca[3] ) { mDCAXY = sqrt(dca[0]*dca[0]+dca[1]*dca[1]); mDCAZ = dca[2]; }
    void setDCA( TVector3 dca ) { mDCAXY = sqrt(dca.X()*dca.X() + dca.Y()*dca.Y()); mDCAZ = dca.Z(); }

    void addEcalCluster( StMuFcsCluster* clu);
    void addHcalCluster( StMuFcsCluster* clu);

    // FCS Cluster matches
    TRefArray mEcalClusters;
    TRefArray mHcalClusters;
    
protected:

    // Track quality and convergence
    bool mDidFitConverge;   // == 0 if the fit did not converge
    bool mDidFitConvergeFully; // == 0 if the fit did not converge fully
    short mNumberOfFailedPoints; // Number of points that failed to be fitted
    short mNumberOfSeedPoints; // Number of points used in the track seed step
    short mNumberOfFitPoints; // Number of fit points used by GenFit
    float mChi2; // Chi^2 of the fit
    float mNDF; // Number of degrees of freedom of the fit
    float mPval; // P-value of the fit
    short mCharge; // Charge of the track
    TVector3 mPrimaryMomentum; // Momentum of the track at the primary vertex
    
    /// MC track id
    UShort_t mIdTruth;
    /// MC track quality (percentage of hits coming from corresponding MC track)
    UShort_t mQATruth;

    float mDCAXY; // DCA XY to the primary vertex
    float mDCAZ; // DCA Z to the primary vertex
    UChar_t mVtxIndex; // Index of the vertex in the event

    ClassDef(StMuFwdTrack,3)
};

#endif

