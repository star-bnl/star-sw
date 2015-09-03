/*************************************************************************
 *
 * $Id: StMuFmsCluster.h,v 1.2 2015/09/02 22:09:58 jdb Exp $
 *
 * Author: Thomas Burton, 2014
 *************************************************************************
 *
 * Description: Declaration of StMuFmsCluster, the MuDST FMS cluster class
 *
 *************************************************************************
 *
 * $Log: StMuFmsCluster.h,v $
 * Revision 1.2  2015/09/02 22:09:58  jdb
 * Added Akios changes to Fms
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
                 float x = 0.f, float y = 0.f,  float smin=0.f, float smax=0.f,
		 float chi1=0.f, float chi2=0.f, int id=0);
  explicit StMuFmsCluster(const StFmsCluster&);
  virtual ~StMuFmsCluster();
  virtual void Clear(Option_t* option = "");
  
  unsigned short detectorId() const;
  unsigned short category() const;
  float energy() const;
  float x() const; // x "center of gravity" of the cluster.
  float y() const; // y "center of gravity" of the cluster.
  float sigmaMin() const; // Maximum 2nd moment (along major axis).
  float sigmaMax() const; // Minimum 2nd moment.
  float chi2Ndf1Photon() const; // chi^2/ndf for 1-photon fit to the cluster.
  float chi2Ndf2Photon() const; // chi^2/ndf for 2-photon fit to the cluster.
  int id() const; // Cluster ID
  TRefArray* hits();
  const TRefArray* hits() const;
  TRefArray* photons();
  const TRefArray* photons() const;
  void setDetectorId(unsigned short detector);
  void setCategory(unsigned short category);
  void setEnergy(float energy);
  void setX(float x);
  void setY(float y);
  void setSigmaMin(float sigmaMin);
  void setSigmaMax(float sigmaMax);
  void setChi2Ndf1Photon(float chi2ndfph1);
  void setChi2Ndf2Photon(float chi2ndfph2);
  void setId(float cluid);

 protected:
  UShort_t mDetectorId;  ///< Detector ID as defined in database
  UShort_t mCategory;  ///< Category of cluster (see EFmsClusterCategory)
  Float_t mEnergy;  ///< Total energy contained in the cluster
  Float_t mX;  ///< Mean x ("center of gravity")
  Float_t mY;  ///< Mean y ("center of gravity")
  Float_t mSigmaMin;  // Minimum 2nd moment
  Float_t mSigmaMax;  // Maximum 2nd moment (along major axis)
  Float_t mChi2Ndf1Photon;  // &chi;<sup>2</sup> / ndf for 1-photon fit
  Float_t mChi2Ndf2Photon;  // &chi;<sup>2</sup> / ndf for 2-photon fit
  Int_t mId;  // Eventwise cluster ID
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
  inline float StMuFmsCluster::sigmaMin() const { return mSigmaMin; } // Maximum 2nd moment (along major axis).
  inline float StMuFmsCluster::sigmaMax() const { return mSigmaMax; } // Minimum 2nd moment.
  inline float StMuFmsCluster::chi2Ndf1Photon() const { return mChi2Ndf1Photon; } // chi^2/ndf for 1-photon fit to the cluster.
  inline float StMuFmsCluster::chi2Ndf2Photon() const { return mChi2Ndf2Photon; } // chi^2/ndf for 2-photon fit to the cluster.
  inline int StMuFmsCluster::id() const { return mId; } // Cluster ID

  inline TRefArray* StMuFmsCluster::hits() { return &mHits; }
  inline const TRefArray* StMuFmsCluster::hits() const { return &mHits; }
  inline TRefArray* StMuFmsCluster::photons() { return &mPhotons; }
  inline const TRefArray* StMuFmsCluster::photons() const { return &mPhotons; }
  inline void StMuFmsCluster::setDetectorId(unsigned short detector) { mDetectorId = detector; }
  inline void StMuFmsCluster::setCategory(unsigned short category) { mCategory = category; }
  inline void StMuFmsCluster::setEnergy(float energy) { mEnergy = energy; }
  inline void StMuFmsCluster::setX(float x) { mX = x; }
  inline void StMuFmsCluster::setY(float y) { mY = y; }
  inline void StMuFmsCluster::setSigmaMin(float sigmaMin) { mSigmaMin = sigmaMin; }
  inline void StMuFmsCluster::setSigmaMax(float sigmaMax) { mSigmaMax = sigmaMax; }
  inline void StMuFmsCluster::setChi2Ndf1Photon(float chi2ndfph1) { mChi2Ndf1Photon = chi2ndfph1; }
  inline void StMuFmsCluster::setChi2Ndf2Photon(float chi2ndfph2) { mChi2Ndf2Photon = chi2ndfph2; }
  inline void StMuFmsCluster::setId(float cluid) { mId = cluid; }

#endif  // STROOT_STMUDSTMAKER_COMMON_STMUFMSCLUSTER_H_
