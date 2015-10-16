/*****************************************************************************
 * 
 * $Id: StMuFmsPoint.h,v 1.3 2015/10/16 18:13:28 jdb Exp $
 *
 * Author: Thomas Burton, 2014
 *****************************************************************************
 *
 * Description: Declaration of StMuFmsPoint, the MuDST FMS "point" class
 *
 *****************************************************************************
 *  
 * $Log: StMuFmsPoint.h,v $
 * Revision 1.3  2015/10/16 18:13:28  jdb
 * incremented version # for StMuFmsCollection and StMuFmsPoint
 *
 * Revision 1.2  2015/09/02 22:09:58  jdb
 * Added Akios changes to Fms
 *
 *
 *****************************************************************************/
#ifndef STROOT_STMUDSTMAKER_COMMON_STMUFMSPOINT_H_
#define STROOT_STMUDSTMAKER_COMMON_STMUFMSPOINT_H_

#include "StLorentzVectorF.hh"
#include "StThreeVectorF.hh"
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
               float x = 0.f, float y = 0.f, int id = 0);
  explicit StMuFmsPoint(const StFmsPoint&);
  virtual ~StMuFmsPoint();

  StThreeVectorF momentum(float m = 0.f, float zvertex=0.f) const;
  StLorentzVectorF fourMomentum(float m = 0.f, float zvertex=0.f) const;
  StMuFmsCluster* cluster();
  const StMuFmsCluster* cluster() const;
  unsigned short detectorId() const;
  float energy() const;
  float x() const; // x "center of gravity" of the point (cm) in local coordinate
  float y() const; // y "center of gravity" of the point (cm) in local coordinate
  int id() const;
  StThreeVectorF xyz() const; // XYZ in STAR coordinate
  void setDetectorId(unsigned short detector);
  void setEnergy(float energy);
  void setX(float x);
  void setY(float y);
  void setId(int id);
  void set(const StFmsPoint& point);
  void setXYZ(const StThreeVectorF xyz);
  void setCluster(StMuFmsCluster* cluster);
  
 protected:
  UShort_t mDetectorId;  ///< Detector ID as defined in database
  Float_t mEnergy;  ///< Total energy contained in the point
  Float_t mX;  ///< Mean x ("center of gravity")
  Float_t mY;  ///< Mean y ("center of gravity")
  Int_t mId;   ///point Id
  StThreeVectorF mXYZ;  // STAR coodinate XYZ
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
  ClassDef(StMuFmsPoint, 2)
};

  inline unsigned short StMuFmsPoint::detectorId() const { return mDetectorId; }
  inline float StMuFmsPoint::energy() const { return mEnergy; }
  inline float StMuFmsPoint::x() const { return mX; } // x "center of gravity" of the point (cm).
  inline float StMuFmsPoint::y() const { return mY; } // y "center of gravity" of the point (cm).
  inline int   StMuFmsPoint::id() const { return mId; }
  inline StThreeVectorF StMuFmsPoint::xyz() const { return mXYZ; }
  inline void StMuFmsPoint::setDetectorId(unsigned short detector) { mDetectorId = detector; } 
  inline void StMuFmsPoint::setEnergy(float energy) { mEnergy = energy; }
  inline void StMuFmsPoint::setX(float x) { mX = x; }
  inline void StMuFmsPoint::setY(float y) { mY = y; }
  inline void StMuFmsPoint::setId(int id) { mId = id; }
  inline void StMuFmsPoint::setXYZ(StThreeVectorF xyz) {mXYZ = xyz;}

#endif  // STROOT_STMUDSTMAKER_COMMON_STMUFMSPOINT_H_
