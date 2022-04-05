/**************************************************************************
 *
 * $Id: StFcsPoint.h,v 2.1 2021/01/11 20:25:37 ullrich Exp $
 *
 * Author: Akio Ogawa 2018
 **************************************************************************
 *
 * Description: Declaration of StFcsPoint, the StEvent FCS photon structure
 * Represents a "point" (photon etc) fitted to a cluster of FCS towers.
 *
 **************************************************************************
 *
 * $Log: StFcsPoint.h,v $
 * Revision 2.1  2021/01/11 20:25:37  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StFcsPoint_h
#define StFcsPoint_h

#include "StLorentzVectorD.hh"
#include "StThreeVectorD.hh"
#include "StObject.h"
#include "StFcsCluster.h"
#include "StEnumerations.h"

class StFcsPoint : public StObject {
public:
    StFcsPoint();
    ~StFcsPoint();

    unsigned short detectorId() const;
    float energy() const;
    float x() const;  // x position in cell unit at which point intersects the sub-detector in local coordinate
    float y() const;  // y position in cell unit at which point intersects the sub-detector in local coordinate
    unsigned int parentClusterId() const; //parent cluster Id
    StFcsCluster* cluster(); //  Parent cluster of the photon.
    int nParentClusterPhotons()   const; // Number of points in the parent cluster.
    const StThreeVectorD& xyz()   const; // XYZ position in global STAR coordinate
    const StLorentzVectorD& fourMomentum() const;

    void setDetectorId(unsigned short detectorId);
    void setEnergy(float energy);
    void setX(float x);
    void setY(float y);
    void setCluster(StFcsCluster* cluster);
    void setNParentClusterPhotons(int nclph);
    void setXYZ(const StThreeVectorD& p3);
    void setFourMomentum(const StLorentzVectorD& p4);
    
    void print(int option=0);

private:
    UShort_t mDetectorId=0;  ///  North=0, South=1
    Float_t  mEnergy=0;      ///  Fitted energy
    Float_t  mX=0.0;         ///  Fitted x-position in local coordinate
    Float_t  mY=0.0;         ///  Fitted y-position in local coordinate
    Int_t    mNParentClusterPhotons=0;  ///< Number of photons in the parent cluster
    StFcsCluster* mCluster=0;
    StLorentzVectorD mFourMomentum;  ///< Photon 4-momentum
    StThreeVectorD   mXYZ;           //Photon position in STAR coordinate

    ClassDef(StFcsPoint, 1)
};

inline unsigned short StFcsPoint::detectorId() const { return mDetectorId; }
inline float StFcsPoint::energy() const { return mEnergy; }
inline float StFcsPoint::x() const { return mX; } // x position in cm at which point intersects the sub-detector.
inline float StFcsPoint::y() const { return mY; } // y position in cm at which point intersects the sub-detector.
inline unsigned int StFcsPoint::parentClusterId() const { return mCluster->id(); } //parent cluster Id
inline StFcsCluster* StFcsPoint::cluster() { return mCluster; } //  Parent cluster of the photon.
inline int StFcsPoint::nParentClusterPhotons() const { return mNParentClusterPhotons; } // Number of points in parent cluster
inline const StThreeVectorD& StFcsPoint::xyz() const { return mXYZ; }
inline const StLorentzVectorD& StFcsPoint::fourMomentum() const { return mFourMomentum; }
inline void StFcsPoint::setDetectorId(unsigned short det) { mDetectorId = det; }
inline void StFcsPoint::setEnergy(float energy) { mEnergy = energy; }
inline void StFcsPoint::setX(float xpos) { mX = xpos; }
inline void StFcsPoint::setY(float ypos) { mY = ypos; }
inline void StFcsPoint::setCluster(StFcsCluster* cluster) { mCluster = cluster; }
inline void StFcsPoint::setNParentClusterPhotons(int nclph) { mNParentClusterPhotons = nclph; }
inline void StFcsPoint::setXYZ(const StThreeVectorD& p3) { mXYZ = p3; }
inline void StFcsPoint::setFourMomentum(const StLorentzVectorD& p4) { mFourMomentum = p4; }

#endif  // StFcsPoint_h

