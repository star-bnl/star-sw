/**************************************************************************
 *
 * $Id: StFmsPoint.h,v 2.7 2015/10/21 14:46:48 ullrich Exp $
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
 * Revision 2.7  2015/10/21 14:46:48  ullrich
 * Improved comments.
 *
 * Revision 2.6  2015/09/14 16:59:22  ullrich
 * Added comments and modified print out.
 *
 * Revision 2.5  2015/09/01 21:01:47  ullrich
 * Minor changes to format of print statments and \nchange to naming of data member.
 *
 * Revision 2.4  2015/09/01 18:29:01  ullrich
 * Changes due to adding StFpsSlat and interconnection between slats and points.
 *
 * Revision 2.3  2015/08/26 16:51:25  ullrich
 * Fixed bug in cluster() and added print out fct and operator.
 *
 * Revision 2.2  2015/08/19 19:22:35  ullrich
 * Major update (PID) by Akio.
 **************************************************************************/
#ifndef StFmsPoint_h
#define StFmsPoint_h

#include "StLorentzVectorF.hh"
#include "StThreeVectorF.hh"
#include "StObject.h"
#include "StFmsCluster.h"
#include "StEnumerations.h"

class StFmsPoint : public StObject {
public:
    StFmsPoint();
    ~StFmsPoint();

    enum StFmsPointPidFlag {
        kFpsPidNoFps=0,       // hit no slat
        kFpsPidBad=1,         // hit status bad slat
        kFpsPidGamma1=10,     // L1==0 L2==0 L3==0    gamma which did not convert
        kFpsPidGamma2=11,     // L1==0 L2==0 L3>=1    golden gamma
        kFpsPidGamma3=12,     // L1>=1 L2==0 L3==0    gamma with extra hit in layer1
        kFpsPidGamma4=13,     // L1==0 L2>=1 L3==0    gamma with extra hit in layer2
        kFpsPidGamma5=14,     // L1>=1 L2==0 L3>=1    gamma with extra hit in layer1
        kFpsPidGamma6=15,     // L1==0 L2>=1 L3>=1    gamma with extra hit in layer2
        kFpsPidGamma7=16,     // L1>=2 L2>=2 L3>=5    gamma converted to e+e-
        kFpsPidMip=20,        // L1==1 L2==1 L3==1    MIP (charged hadron or muon)
        kFpsPidElectron1=30,  // L1==1 L2==1 L3>=5    golden electron/positron
        kFpsPidElectron2=31,  // L1==1 L2>=2 L3>=5    electron
        kFpsPidElectron3=32,  // L1>=2 L2==1 L3>=5    electron
        kFpsPidUnknown=40     // L1>=1 L2>=1 L3==0    not sure what to do
    };
    
    const char* pidName(int i);
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
    int   fpsNCandidate(int layer);
    float fpsMip(int layer, int candidate=0);
    int   fpsSlatId(int layer, int candidate=0);
    float fpsDistance(int layer, int candidate=0); // distance from edge to projected
                                                   // location (negative means inside, positive outside)
    void  setFpsPid(int v);
    void  setFps(int layer, float mip, int slatid, float dist);
    void  resetFps();
    void  orderFpsCandidates(int layer=0); //order Fps hit candidates from near to far (layer=0 for sorting all layers)
    
    void print(int option=0);

private:
    UShort_t mDetectorId;  ///< Detector starts from 1
    Float_t  mEnergy;      ///< Fitted energy
    Float_t  mX;           ///< Fitted x-position in local coordinate
    Float_t  mY;           ///< Fitted y-position in local coordinate
    Int_t    mId;          ///< Photon ID within event
    Int_t    mParentClusterId;  ///< ID of the parent cluster within event
    Int_t    mNParentClusterPhotons;  ///< Number of photons in the parent cluster
#ifdef __CINT__
    StObjLink             mCluster;   // Parent cluster of this photon
#else
    StLink<StFmsCluster>  mCluster;
#endif //__CINT__
    StLorentzVectorF mFourMomentum;  ///< Photon 4-momentum
    StThreeVectorF   mXYZ;           //Photon position in STAR coordinate

    Int_t   mFpsPid;                                  // see enum above
    Int_t   mFpsNCandidate[kFpsNLayer];               // # of possible slat related to this point
    Float_t mFpsMip[kFpsNLayer][kFpsNCandidate];      // # of MIPs
    Int_t   mFpsSlatId[kFpsNLayer][kFpsNCandidate];   // slatid
    Float_t mFpsDistance[kFpsNLayer][kFpsNCandidate]; // distance from edge to projected location
                                                      // (negative means inside, positive outside)
    ClassDef(StFmsPoint, 3)
};

inline unsigned short StFmsPoint::detectorId() const { return mDetectorId; }
inline float StFmsPoint::energy() const { return mEnergy; }
inline float StFmsPoint::x() const { return mX; } // x position in cm at which point intersects the sub-detector.
inline float StFmsPoint::y() const { return mY; } // y position in cm at which point intersects the sub-detector.
inline int StFmsPoint::id() const { return mId; } // ID of the point in the current event.
inline StFmsCluster* StFmsPoint::cluster() { return mCluster; } //  Parent cluster of the photon.
inline const StFmsCluster* StFmsPoint::cluster() const { return mCluster; }
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
inline const char* StFmsPoint::pidName(int i) {
    switch(i){
        case kFpsPidNoFps:     return "NoDet  ";
        case kFpsPidBad:       return "Bad    ";
        case kFpsPidGamma1:    return "GNC    ";
        case kFpsPidGamma2:    return "GC     ";
        case kFpsPidGamma3:    return "GNCwL1H";
        case kFpsPidGamma4:    return "GNCwL2H";
        case kFpsPidGamma5:    return "GCwL1H ";
        case kFpsPidGamma6:    return "GCwL2H ";
        case kFpsPidGamma7:    return "G2e+e- ";
        case kFpsPidMip:       return "MIP    ";
        case kFpsPidElectron1: return "Egolden";
        case kFpsPidElectron2: return "EwL1H  ";
        case kFpsPidElectron3: return "EwL2H  ";
        case kFpsPidUnknown:   return "NOL3   ";
        default:               return "????   ";
    }
}


#endif  // StFmsPoint_h

