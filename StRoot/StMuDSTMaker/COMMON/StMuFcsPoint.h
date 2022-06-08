/*****************************************************************************
 * 
 * $Id: StMuFcsPoint.h,v 1.0 2021/11/17 18:13:28 jdb Exp $
 *
 * Author: Daniel Brandenburg, 2021
 *****************************************************************************
 *
 * Description: Declaration of StMuFcsPoint, the MuDST FCS "point" class
 *
 *****************************************************************************
 *  
 *
 *****************************************************************************/
#ifndef StMuFcsPoint_hh
#define StMuFcsPoint_hh

#include "StLorentzVectorF.hh"
#include "StThreeVectorF.hh"
#include <TObject.h>
#include <TRef.h>
#include <TVector3.h>
#include <TLorentzVector.h>

class StFcsPoint;  // The equivalent point structure in StEvent
class StMuFcsCluster;

/**
 Micro-DST Fcs "point" class.

 Describes a "point" - the energy deposited by a single particle in a cluster.
 One or more points may be form a cluster of adjacent towers in the FCS.
 */
class StMuFcsPoint : public TObject {
    public:
    StMuFcsPoint();
    ~StMuFcsPoint();

    unsigned short detectorId() const;
    float energy() const;
    float x() const;  // x position in cell unit at which point intersects the sub-detector in local coordinate
    float y() const;  // y position in cell unit at which point intersects the sub-detector in local coordinate
    unsigned int parentClusterId() const; //parent cluster Id
    StMuFcsCluster* cluster(); //  Parent cluster of the photon.
    int nParentClusterPhotons()   const; // Number of points in the parent cluster.
    const TVector3& xyz()   const; // XYZ position in global STAR coordinate
    const TLorentzVector& fourMomentum() const;

    void setDetectorId(unsigned short detectorId);
    void setEnergy(float energy);
    void setX(float x);
    void setY(float y);
    void setCluster(StMuFcsCluster* cluster);
    void setNParentClusterPhotons(int nclph);
    void setXYZ(const TVector3& p3);
    void setFourMomentum(const TLorentzVector& p4);
    
    void print(int option=0);

private:
    UShort_t mDetectorId=0;  ///  North=0, South=1
    Float_t  mEnergy=0;      ///  Fitted energy
    Float_t  mX=0.0;         ///  Fitted x-position in local coordinate
    Float_t  mY=0.0;         ///  Fitted y-position in local coordinate
    Int_t    mNParentClusterPhotons=0;  ///< Number of photons in the parent cluster
    TRef mCluster;
    TLorentzVector mFourMomentum;  ///< Photon 4-momentum
    TVector3   mXYZ;           //Photon position in STAR coordinate

  ClassDef(StMuFcsPoint, 2)
};

inline unsigned short StMuFcsPoint::detectorId() const { return mDetectorId; }
inline float StMuFcsPoint::energy() const { return mEnergy; }
inline float StMuFcsPoint::x() const { return mX; } // x position in cm at which point intersects the sub-detector.
inline float StMuFcsPoint::y() const { return mY; } // y position in cm at which point intersects the sub-detector.
inline int StMuFcsPoint::nParentClusterPhotons() const { return mNParentClusterPhotons; } // Number of points in parent cluster
inline const TVector3& StMuFcsPoint::xyz() const { return mXYZ; }
inline const TLorentzVector& StMuFcsPoint::fourMomentum() const { return mFourMomentum; }
inline void StMuFcsPoint::setDetectorId(unsigned short det) { mDetectorId = det; }
inline void StMuFcsPoint::setEnergy(float energy) { mEnergy = energy; }
inline void StMuFcsPoint::setX(float xpos) { mX = xpos; }
inline void StMuFcsPoint::setY(float ypos) { mY = ypos; }
inline void StMuFcsPoint::setNParentClusterPhotons(int nclph) { mNParentClusterPhotons = nclph; }
inline void StMuFcsPoint::setXYZ(const TVector3& p3) { mXYZ = p3; }
inline void StMuFcsPoint::setFourMomentum(const TLorentzVector& p4) { mFourMomentum = p4; }

#endif  // STROOT_STMUDSTMAKER_COMMON_STMUFMSPOINT_H_
