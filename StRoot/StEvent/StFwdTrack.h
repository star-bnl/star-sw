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

class StFcsCluster;


struct StFwdTrackProjection {
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
    void set(   StFwdTrackProjection &other ){
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

    enum {
        PrimaryVertex = 0,
        FST = 1,
        FTT = 2,
        EPD = 3,
        ECAL = 4, 
        HCAL = 5
    } DetectorId;
};

struct StFwdTrackSeedPoint {
    StFwdTrackSeedPoint() {}
    StFwdTrackSeedPoint(    StThreeVectorD xyz, 
                            short sec, 
                            unsigned short trackId, 
                            float cov[9] ){
        mXYZ = xyz;
        mSector = sec;
        mTrackId = trackId;
        memcpy( mCov, cov, sizeof( mCov ));
    }
    
    StThreeVectorD mXYZ;
    unsigned short mTrackId;
    short mSector;
    float mCov[9];
};

class StFwdTrack : public StObject {

public:
    StFwdTrack(  );

    vector<StFwdTrackProjection> mProjections;
    vector<StFwdTrackSeedPoint> mFTTPoints;
    vector<StFwdTrackSeedPoint> mFSTPoints;

    bool getProjectionFor(  int detectorId, 
                            StFwdTrackProjection &rProj, 
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
    // unsigned int   numberOfPossibleFitPoints() const;

    // Number of points used in the track seed step
    short   numberOfSeedPoints() const;


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
    StThreeVectorD mPrimaryMomentum;
    

    StPtrVecFcsCluster mEcalClusters;
    StPtrVecFcsCluster mHcalClusters;
    
    ClassDef(StFwdTrack,1)

};

#endif

