/**************************************************************************
 *
 * $Id: StFmsPoint.h,v 2.2 2015/08/19 19:22:35 ullrich Exp $
 *
 * Author: Thomas Burton, Yuxi Pan, 2014
 **************************************************************************
 *
 * Description: Declaration of StFmsPoint, the StEvent FMS photon structure
 * Represents a "point" (photon etc) fitted to a cluster of FMS towers.
 *
 **************************************************************************
 *
 * $Log: StFmsPoint.h,v $
 * Revision 2.2  2015/08/19 19:22:35  ullrich
 * Major update (PID) by Akio.
 *
 *
 **************************************************************************/
#ifndef StFmsPoint_h
#define StFmsPoint_h

#include "StLorentzVectorF.hh"
#include "StThreeVectorF.hh"
#include "StObject.h"
#include "StFmsCluster.h"

class StFmsPoint : public StObject {
public:
    StFmsPoint();
    ~StFmsPoint();

    enum StFmsPointPidFlag {
        kFpsPidBad=0,         // hit bad slat or no slat
        kFpsPidGamma1=10,     // L1==0 L2==0 L3==0    gamma which did not convert
        kFpsPidGamma2=11,     // L1==0 L2==0 L3>=1    golden gamma
        kFpsPidGamma3=12,     // L1>=1 L2==0 L3==0    gamma with extra hit in layer1
        kFpsPidGamma4=13,     // L1==0 L2>=1 L3==0    gamma with extra hit in layer2
        kFpsPidGamma5=14,     // L1>=1 L2==0 L3>=1    gamma with extra hit in layer1
        kFpsPidGamma6=15,     // L1==0 L2>=1 L3>=1    gamma with extra hit in layer2
        kFpsPidGamma7=16,     // L1>=2 L2>=2 L3>=5    gamma converted to e+e-
        kFpsPidMip=20,        // L1==1 L2==1 L3==1    MIP
        kFpsPidElectron1=30,  // L1==1 L2==1 L3>=5    golden electron
        kFpsPidElectron2=31,  // L1==1 L2>=2 L3>=5    electron
        kFpsPidElectron3=32,  // L1>=2 L2==1 L3>=5    electron
        kFpsPidUnknown=90     // L1>=1 L2>=1 L3==0    not sure what to do
    };

    unsigned short detectorId() const;
    float energy() const;
    float x() const;  // x position in cm at which point intersects the sub-detector in local coordinate
    float y() const;  // y position in cm at which point intersects the sub-detector in local coordinate
    int id() const;   // ID of the point in the current event.
    StFmsCluster* cluster(); //  Parent cluster of the photon.
    const StFmsCluster* cluster() const; //  Parent cluster of the photon.
    int parentClusterId() const; // ID of the parent cluster containing this point.
    int nParentClusterPhotons() const; // Number of points in the parent cluster.
    const StThreeVectorF& XYZ() const; // XYZ position in global STAR coordinate
    const StLorentzVectorF& fourMomentum() const;
    void setDetectorId(unsigned short detector);
    void setEnergy(float energy);
    void setX(float xpos);
    void setY(float ypos);
    void setId(int phid);
    void setCluster(StFmsCluster* cluster);
    void setParentClusterId(int cluid);
    void setNParentClusterPhotons(int nclph);
    void setXYZ(const StThreeVectorF& p3);
    void setFourMomentum(const StLorentzVectorF& p4);
    
    int   fpsPid();                                //PID see enum above
    float fpsMip(int layer, int candidate=0);
    int   fpsSlatid(int layer, int candidate=0);
    float fpsDistance(int layer, int candidate=0); // distance from edge to projected
                                                   // location (negative means inside, positive outside)
    void  setFpsPid(int v);
    void  setFps(int layer, int candidate, float mip, int slatid, float dist);
    void  resetFps();
    void  orderFpsCandidates(); //order Fps hit candidates from near to far
    
private:
    UShort_t mDetectorId;  ///< Detector starts from 1
    Float_t  mEnergy;  ///< Fitted energy
    Float_t  mX;  ///< Fitted x-position in local coordinate
    Float_t  mY;  ///< Fitted y-position in local coordinate
    Int_t    mId;  ///< Photon ID within event
    Int_t    mParentClusterId;  ///< ID of the parent cluster within event
    Int_t    mNParentClusterPhotons;  ///< Number of photons in the parent cluster
#ifdef __CINT__
    StObjLink             mCluster;   // Parent cluster of this photon
#else
    StLink<StFmsCluster>  mCluster;   // Parent cluster of this photon
#endif //__CINT__
    StLorentzVectorF mFourMomentum;  ///< Photon 4-momentum
    StThreeVectorF   mXYZ;  //Photon position in STAR coordinate
    
    //for FPS 3 layers and 2 candidates
    static const int NLayer=3, NCandidate=2;
    Int_t   mFpsPid;                           // see enum above
    Float_t mFpsMip[NLayer][NCandidate];       // # of MIPs
    Int_t   mFpsSlatid[NLayer][NCandidate];    // slatid
    Float_t mFpsDistance[NLayer][NCandidate];  // distance from edge to projected location
                                               // (negative means inside, positive outside)
    ClassDef(StFmsPoint, 3)
};

inline unsigned short StFmsPoint::detectorId() const { return mDetectorId; }
inline float StFmsPoint::energy() const { return mEnergy; }
inline float StFmsPoint::x() const { return mX; } // x position in cm at which point intersects the sub-detector.
inline float StFmsPoint::y() const { return mY; } // y position in cm at which point intersects the sub-detector.
inline int StFmsPoint::id() const { return mId; } // ID of the point in the current event.
inline StFmsCluster* StFmsPoint::cluster() { return cluster(); } //  Parent cluster of the photon.
inline const StFmsCluster* StFmsPoint::cluster() const { return cluster(); }
inline int StFmsPoint::parentClusterId() const { return mParentClusterId; } // ID of the parent cluster
                                                                            // containing this point.
inline int StFmsPoint::nParentClusterPhotons() const { return mNParentClusterPhotons; } // Number of points
                                                                                        // in the parent cluster.
inline const StThreeVectorF& StFmsPoint::XYZ() const { return mXYZ; }
inline const StLorentzVectorF& StFmsPoint::fourMomentum() const { return mFourMomentum; }
inline void StFmsPoint::setDetectorId(unsigned short detector) { mDetectorId = detector; }
inline void StFmsPoint::setEnergy(float energy) { mEnergy = energy; }
inline void StFmsPoint::setX(float xpos) { mX = xpos; }
inline void StFmsPoint::setY(float ypos) { mY = ypos; }
inline void StFmsPoint::setId(int phid) { mId = phid; }
inline void StFmsPoint::setCluster(StFmsCluster* cluster) { mCluster = cluster; }
inline void StFmsPoint::setParentClusterId(int cluid) { mParentClusterId = cluid; }
inline void StFmsPoint::setNParentClusterPhotons(int nclph) { mNParentClusterPhotons = nclph; }
inline void StFmsPoint::setXYZ(const StThreeVectorF& p3) { mXYZ = p3; }
inline void StFmsPoint::setFourMomentum(const StLorentzVectorF& p4) { mFourMomentum = p4; }
inline int  StFmsPoint::fpsPid() {return mFpsPid;}
inline void StFmsPoint::setFpsPid(int v) {mFpsPid=v;}
inline float StFmsPoint::fpsMip(int layer, int candidate) {
    if(layer>=1 && layer<=3) {
        return mFpsMip[layer-1][candidate];
    }
    return -1;
}
inline int StFmsPoint::fpsSlatid(int layer, int candidate) {
    if(layer>=1 && layer<=3) {
        return mFpsSlatid[layer-1][candidate];
    }
    return -1;
}
inline float StFmsPoint::fpsDistance(int layer, int candidate) {
    if(layer>=1 && layer<=3) {
        return mFpsDistance[layer-1][candidate];
    }
    return -1;
}
inline void StFmsPoint::setFps(int layer, int candidate, float mip, int slatid, float d) {
    if(layer>=1 && layer<=NLayer && candidate>=0 && candidate<NCandidate){
        mFpsMip[layer-1][candidate] = mip;
        mFpsSlatid[layer-1][candidate] = slatid;
        mFpsDistance[layer-1][candidate] = d;
    }
}
inline void StFmsPoint::resetFps() {
    mFpsPid = 0;
    for(int l=0; l<NLayer; l++){
        for(int c=0; c<NCandidate; c++){
            mFpsMip[l][c] = -2.0;
            mFpsSlatid[l][c] = -1;
            mFpsDistance[l][c] = 999.0;
        }
    }
}
inline void StFmsPoint::orderFpsCandidates() {
    for(int l=0; l<NLayer; l++){
        if(mFpsDistance[l][0]>mFpsDistance[l][1]){
            float mip = mFpsMip[l][0];
            float slatid = mFpsSlatid[l][0];
            float d = mFpsDistance[l][0];
            mFpsMip[l][0] = mFpsMip[l][1];
            mFpsSlatid[l][0] = mFpsSlatid[l][1];
            mFpsDistance[l][0] = mFpsDistance[l][1];
            mFpsMip[l][1] = mip;
            mFpsSlatid[l][1] = slatid;
            mFpsDistance[l][1] = d;
        }
    }
}
#endif  // StFmsPoint_h

