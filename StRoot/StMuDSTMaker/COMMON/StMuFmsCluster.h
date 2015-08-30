/*************************************************************************
 *
 * $Id: StMuFmsCluster.h,v 1.1 2015/08/28 18:36:04 jdb Exp $
 *
 * Author: Thomas Burton, 2014
 *************************************************************************
 *
 * Description: Declaration of StMuFmsCluster, the MuDST FMS cluster class
 *
 *************************************************************************
 *
 * $Log: StMuFmsCluster.h,v $
 * Revision 1.1  2015/08/28 18:36:04  jdb
 * Added Akios FMS codes
 *
 *
 *************************************************************************/  
#ifndef STROOT_STMUDSTMAKER_COMMON_STMUFMSCLUSTER_H_
#define STROOT_STMUDSTMAKER_COMMON_STMUFMSCLUSTER_H_

#include <TObject.h>
#include <TRefArray.h>

class StFmsCluster;  // Equivalent class in StEvent

/**
 Micro-DST FMS cluster class.

 Describes a cluster (collection of adjacent towers) in the FMS.
 May be created by one or more photons.
 It maintains references to the hits that form the cluster, and to photons that
 are fitted to it. However it does not own any of those objects - they are
 owned by the relevant TClonesArrays in the micro-DST.
 */
class StMuFmsCluster : public TObject {
 public:
  StMuFmsCluster(int detectorId = 0, int category = -1, float energy = 0.f,
                 float x = 0.f, float y = 0.f);
  explicit StMuFmsCluster(const StFmsCluster&);
  virtual ~StMuFmsCluster();
  virtual void Clear(Option_t* option = "");
  
  unsigned short detectorId() const;
  unsigned short category() const;
  float energy() const;
  float x() const; // x "center of gravity" of the cluster.
  float y() const; // y "center of gravity" of the cluster.
  TRefArray* hits();
  const TRefArray* hits() const;
  TRefArray* photons();
  const TRefArray* photons() const;
  void setDetectorId(unsigned short detector);
  void setCategory(unsigned short category);
  void setEnergy(float energy);
  void setX(float x);
  void setY(float y);

 protected:
  UShort_t mDetectorId;  ///< Detector ID as defined in database
  UShort_t mCategory;  ///< Category of cluster (see EFmsClusterCategory)
  Float_t mEnergy;  ///< Total energy contained in the cluster
  Float_t mX;  ///< Mean x ("center of gravity")
  Float_t mY;  ///< Mean y ("center of gravity")
  TRefArray mHits;  ///< StMuFmsHits in the current cluster
  TRefArray mPhotons;  ///< StMuFmsPoints in the cluster

 private:
  /**
   Disallow copy construction.

   Duplication should only be done via Clone().
   */
  StMuFmsCluster(const StMuFmsCluster&);
  /**
   Disallow assignment.

   Duplication should only be done via Clone().
   */
  StMuFmsCluster& operator=(const StMuFmsCluster&);
  ClassDef(StMuFmsCluster, 1)
};

  inline unsigned short StMuFmsCluster::detectorId() const { return mDetectorId; }
  inline unsigned short StMuFmsCluster::category() const { return mCategory; }
  inline float StMuFmsCluster::energy() const { return mEnergy; }
  inline float StMuFmsCluster::x() const { return mX; } // x "center of gravity" of the cluster.
  inline float StMuFmsCluster::y() const { return mY; } // y "center of gravity" of the cluster.
  inline TRefArray* StMuFmsCluster::hits() { return &mHits; }
  inline const TRefArray* StMuFmsCluster::hits() const { return &mHits; }
  inline TRefArray* StMuFmsCluster::photons() { return &mPhotons; }
  inline const TRefArray* StMuFmsCluster::photons() const { return &mPhotons; }
  inline void StMuFmsCluster::setDetectorId(unsigned short detector) { mDetectorId = detector; }
  inline void StMuFmsCluster::setCategory(unsigned short category) { mCategory = category; }
  inline void StMuFmsCluster::setEnergy(float energy) { mEnergy = energy; }
  inline void StMuFmsCluster::setX(float x) { mX = x; }
  inline void StMuFmsCluster::setY(float y) { mY = y; }

#endif  // STROOT_STMUDSTMAKER_COMMON_STMUFMSCLUSTER_H_
