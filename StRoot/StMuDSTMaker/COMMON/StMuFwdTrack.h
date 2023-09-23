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

struct StMuFwdTrackSeedPoint : public TObject{
    StMuFwdTrackSeedPoint() {}
    StMuFwdTrackSeedPoint(    TVector3 xyz, 
                            short sec, 
                            unsigned short trackId, 
                            float cov[9] ){
        mXYZ = xyz;
        mSector = sec;
        mTrackId = trackId;
        memcpy( mCov, cov, sizeof( mCov ));
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

    // ECAL clusters
    // StPtrVecFcsCluster& ecalClusters();
    // const StPtrVecFcsCluster& ecalClusters() const;
    // void addEcalCluster(StFcsCluster* p);
    // void sortEcalClusterByET();
    // // HCAL clusters
    // StPtrVecFcsCluster& hcalClusters();
    // const StPtrVecFcsCluster& hcalClusters() const;
    // void addHcalCluster(StFcsCluster* p);
    // void sortHcalClusterByET();

    // vector<StMuFcsCluster*> 

    void addEcalCluster( StMuFcsCluster* clu);
    void addHcalCluster( StMuFcsCluster* clu);

    TRefArray mEcalClusters;
    TRefArray mHcalClusters;
    
protected:

    // Track quality and convergence
    bool mDidFitConverge;
    bool mDidFitConvergeFully;
    short mNumberOfFailedPoints;
    short mNumberOfSeedPoints;
    short mNumberOfFitPoints;
    float mChi2;
    float mNDF;
    float mPval;
    short mCharge;
    TVector3 mPrimaryMomentum;
    

    // StPtrVecFcsCluster mEcalClusters;
    // StPtrVecFcsCluster mHcalClusters;
    
    ClassDef(StMuFwdTrack,2)

};

#endif

