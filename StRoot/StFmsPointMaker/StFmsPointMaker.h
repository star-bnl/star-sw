// $Id: StFmsPointMaker.h,v 1.3 2015/09/02 14:52:15 akio Exp $
//
// $Log: StFmsPointMaker.h,v $
// Revision 1.3  2015/09/02 14:52:15  akio
// Adding readMuDst() to give options when reading back from mudst
//
/**
 \file      StFmsPointMaker.h
 \brief     Declaration of StFmsPointMaker, the FMS cluster/photon maker
 \author    Yuxi Pan <yuxipan@physics.ucla.edu>
 \author    Thomas Burton <tpb@bnl.gov>
 \date      2014
 */
#ifndef STROOT_STFMSPOINTMAKER_STFMSPOINTMAKER_H_
#define STROOT_STFMSPOINTMAKER_STFMSPOINTMAKER_H_

#include <map>
#include <vector>

#include "StMaker.h"
//#include "StRoot/StFmsUtil/StFmsGeometry.h"

class StFmsCollection;
class StFmsDbMaker;
class StFmsPoint;

namespace FMSCluster {
class StFmsFittedPhoton;
class StFmsTower;
class StFmsTowerCluster;
}  // namespace FMSCluster

/**
 Finds FMS clusters and fits them with a photon hypothesis.

 A cluster is a collection of adjacent FMS towers with energy depositions.
 "Point" is a generic term for the energy deposited by an individual particle.
 This will typically be a photon, but may be e.g. an electron, or energy left
 by a hadron.

 A single cluster may be formed by more than one point, so clusters are fitted
 with a shower-shape function to disentangle depositions from different
 particles.
 */
class StFmsPointMaker : public StMaker {
 public:
  /** Constructor. */
  explicit StFmsPointMaker(const char* name = "StFmsPointMaker");
  /** Destructor. */
  ~StFmsPointMaker();
  /** Called by StMaker when switching to a new run number. */
  Int_t InitRun(Int_t runNumber);
  /** Called once per event to process the event. */
  Int_t Make();
  /** Called after each event to reset values. */
  void Clear(Option_t* option = "");

  /** Set to read MuDST, then only this maker does is recalc point position using DB values */
  /** and does NOT perform cluster finding nor fitting */
  void SetReadMuDst(int v=1) {mReadMuDst=v;} 
  
 private:
  /** Definition of a collection of towers. */
  typedef std::vector<FMSCluster::StFmsTower> TowerList;
  /** Definition of a TowerList per sub-detector, keyed by detector ID. */
  typedef std::map<int, TowerList> TowerMap;
  /** Disallow copy construction. */
  StFmsPointMaker(const StFmsPointMaker&);
  /** Disallow assignment. */
  StFmsPointMaker& operator=(const StFmsPointMaker&);
  /**
   Get the StFmsCollection from the current StEvent.

   Print messages to LOG_ERROR if StEvent/StFmsCollection cannot be found.
   */
  StFmsCollection* getFmsCollection();
  /**
   Perform photon reconstruction in all sub-detectors for a single event.

   Populate StFmsCollection with the generated clusters and photons.

   Return kStOk upon success, kStErr in case of an error.
   */
  int clusterEvent();
  /**
   Perform photon reconstruction on a single sub-detector.

   Cluster all towers for a sub-detector.
   Update the cluster and photon lists in the provided StFmsCollection with
   the generated clusters and photons.

   Returns standard STAR error codes (kStOk, kStWarn, kStErr).
   */
  int clusterDetector(TowerList* towers, int detectorId);
  /**
   Verify that the sum of tower energies is sensible.

   Returns true if the sum is non-negative and does not exceed the
   center-of-mass energy. Returns false otherwise.
   */
  bool validateTowerEnergySum(const TowerList& towers) const;
  /**
   Process an StFmsTowerCluster and store its StFmsCluster in a collection.

   Pass ownership of the StFmsCluster held by the FMSCluster::StFmsTowerCluster
   to the StFmsCollection.
   Also update StFmsCollection with any photons in the cluster.

   Returns true if the cluster is processed, or false if it is skipped due to
   bad values (e.g. unphysical coordinates). If the current run information
   cannot be accessed for some reason, assumes 500 GeV collisions.
   */
  bool processTowerCluster(FMSCluster::StFmsTowerCluster* towerCluster,
                           int detectorId);
  /** Creates a new StFmsPoint from an StFmsFittedPhoton. */
  StFmsPoint* makeFmsPoint(const FMSCluster::StFmsFittedPhoton& photon,
                           int detectorId);
  /** Reads hits from StEvent and prepare them for clustering. */
  bool populateTowerLists();
  /**
   Tests channel validity.

   Returns true if a detector/row/column combination physically exists.
   Detector values should be as defined as in the database, and row and column
   numbers are in the range [1, N].
  */
  bool isValidChannel(int detector, int row, int col);
  StFmsDbMaker* mFmsDbMaker;  //!< Access to FMS database information
  //FMSCluster::StFmsGeometry mGeometry;  //!< Access to current FMS geometry
  StFmsCollection* mFmsCollection; //!< StFmsCollection as retrieved from StEvent
  TowerMap mTowers;  //!< One for each sub-detector, keyed by detector ID
  int mObjectCount;  //!< Object count in event for use with TRef

  Int_t readMuDst();
  Int_t mReadMuDst;   //! 0= Do clustering and make Fms points
                      //! 1= Just recalc positions based on DB values

  virtual const Char_t *GetCVS() const {static const Char_t cvs[]="Tag $Name:" __DATE__ " " __TIME__ ; return cvs;}
  ClassDef(StFmsPointMaker, 0)
};
#endif  // STROOT_STFMSPOINTMAKER_STFMSPOINTMAKER_H_
