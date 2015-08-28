/*****************************************************************************
 * 
 * $Id: StMuFmsPoint.h,v 1.1 2015/08/28 18:36:04 jdb Exp $
 *
 * Author: Thomas Burton, 2014
 *****************************************************************************
 *
 * Description: Declaration of StMuFmsPoint, the MuDST FMS "point" class
 *
 *****************************************************************************
 *  
 * $Log: StMuFmsPoint.h,v $
 * Revision 1.1  2015/08/28 18:36:04  jdb
 * Added Akios FMS codes
 *
 *
 *****************************************************************************/
#ifndef STROOT_STMUDSTMAKER_COMMON_STMUFMSPOINT_H_
#define STROOT_STMUDSTMAKER_COMMON_STMUFMSPOINT_H_

#include "StLorentzVectorF.hh"
#include <TObject.h>
#include <TRef.h>

class StFmsPoint;  // The equivalent point structure in StEvent
class StMuFmsCluster;

/**
 Micro-DST FMS "point" class.

 Describes a "point" - the energy deposited by a single particle in a cluster.
 One or more points may be form a cluster of adjacent towers in the FMS.

 Maintains a persistent reference to the cluster formed by the point.
 The cluster is owned by the relevant TClonesArray in the micro-DST, not
 StMuFmsPoint, and should not be deleted.
 */
class StMuFmsPoint : public TObject {
 public:
  StMuFmsPoint(int detectorId = 0, float energy = 0.f,
               float x = 0.f, float y = 0.f, float z = 0.f);
  explicit StMuFmsPoint(const StFmsPoint&);
  virtual ~StMuFmsPoint();

  StThreeVectorF momentum(float m = 0.f) const;
  StLorentzVectorF fourMomentum(float m = 0.f) const;
  StMuFmsCluster* cluster();
  const StMuFmsCluster* cluster() const;
  unsigned short detectorId() const;
  float energy() const;
  float x() const; // x "center of gravity" of the point (cm).
  float y() const; // y "center of gravity" of the point (cm).
  float z() const;
  StThreeVectorF xyz() const;
  void setDetectorId(unsigned short detector);
  void setEnergy(float energy);
  void setX(float x);
  void setY(float y);
  void setZ(float z);
  void set(const StFmsPoint&);
  void setCluster(StMuFmsCluster* cluster);

 protected:
  UShort_t mDetectorId;  ///< Detector ID as defined in database
  Float_t mEnergy;  ///< Total energy contained in the point
  Float_t mX;  ///< Mean x ("center of gravity")
  Float_t mY;  ///< Mean y ("center of gravity")
  Float_t mZ;  ///< z at front face of sub-detector
  TRef mCluster;  ///< Parent cluster of this photon

 private:
  /**
   Disallow copy construction.

   Duplication should only be done via Clone().
   */
  StMuFmsPoint(const StMuFmsPoint&);
  /**
   Disallow assignment.

   Duplication should only be done via Clone().
   */
  StMuFmsPoint& operator=(const StMuFmsPoint&);
  ClassDef(StMuFmsPoint, 1)
};

  inline unsigned short StMuFmsPoint::detectorId() const { return mDetectorId; }
  inline float StMuFmsPoint::energy() const { return mEnergy; }
  inline float StMuFmsPoint::x() const { return mX; } // x "center of gravity" of the point (cm).
  inline float StMuFmsPoint::y() const { return mY; } // y "center of gravity" of the point (cm).
  inline float StMuFmsPoint::z() const { return mZ; }
  inline StThreeVectorF StMuFmsPoint::xyz() const { return StThreeVectorF(mX, mY, mZ); }
  inline void StMuFmsPoint::setDetectorId(unsigned short detector) { mDetectorId = detector; } 
  inline void StMuFmsPoint::setEnergy(float energy) { mEnergy = energy; }
  inline void StMuFmsPoint::setX(float x) { mX = x; }
  inline void StMuFmsPoint::setY(float y) { mY = y; }
  inline void StMuFmsPoint::setZ(float z) { mZ = z; }

#endif  // STROOT_STMUDSTMAKER_COMMON_STMUFMSPOINT_H_
