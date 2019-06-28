// $Id: StFmsPointMaker.h,v 1.10 2019/06/26 16:49:43 akio Exp $
//
// $Log: StFmsPointMaker.h,v $
// Revision 1.10  2019/06/26 16:49:43  akio
// shower shape scaling for all shapes
//
// Revision 1.9  2018/03/08 20:07:27  akio
// initialization of mObjectCount and mMaxEnergySum was missing
//
// Revision 1.8  2018/03/02 20:26:44  akio
// Big update from Zhanwen Zhu with new shower shape and six z slices
//
// Revision 1.7  2015/11/05 17:53:09  akio
// Adding setScaleShowerShape() option for scaling up shower shape function for large cell
//
// Revision 1.6  2015/11/02 22:40:04  akio
// adding option for new cluster categorization
//
// Revision 1.5  2015/10/21 15:49:12  akio
// Adding 3 options to control how reconstruction works:
//   setGlobalRefit(int v=1)
//   setMergeSmallToLarge(int v=1)
//   setTry1PhotonFit(int v=1)
//
// Revision 1.4  2015/09/18 18:46:47  akio
// Move energy sum check for killing LED tail event to whole FMS, not each module
// Also make it not dependent on beam energy, so that it runs on simulation as well.
//
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
class StMuDst;

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

  /** Set max energy on sum of all cell to kill tail of LED */
  void setMaxEnergySum(Float_t v) {mMaxEnergySum=v;}

  /** Set to read MuDST, then only this maker does is recalc point position using DB values */
  /** and does NOT perform cluster finding nor fitting */
  void SetReadMuDst(int v=1) {mReadMuDst=v;} 
  
  /* Set this to perform gloal refit of all photons in a detector */
  void setGlobalRefit(int v=1) {mGlobalRefit=v;}

  /* Set this to cluster small and large cell together... experimental for now */
  void setMergeSmallToLarge(int v=1) {mMergeSmallToLarge=v;}

  /* set this to perform 1 photon fit if 2 photon fit failed */
  void setTry1PhotonFit(int v=1) {mTry1PhotonFitWhen2PhotonFitFailed=v;}

  /* set cluster categorization algo */
  void setCategorizationAlgo(int v=1) {mCategorizationAlgo=v;}

  /* set to scale shower shape for large cell, recomended values are*/
  //              large    small
  //old(1slice)    1.6      1.0
  //Zhanwen's      0.8      0.6
  //Yuxi's         1.2      0.8
  void setScaleShowerShape(float large=0.8, float small=0.6) {mScaleShowerShapeLarge=large; mScaleShowerShapeSmall=small;} 

  /* 0=original shower shape, 1=new shower shape with 6 z slices, 2=Yuxi's 6 slices */
  void setShowerShapeWithAngle(int v=1) {mShowerShapeWithAngle=v;} 

  /* 0=no vertex correction, 1=take vertex from Mudst BBC based on run11 calibration */
  void setVertexZ(int v=1) {mVertexZ=v;} 

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
   This is removed and now part of populateTowerLists()
   */
  //bool validateTowerEnergySum(const TowerList& towers) const;

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
  int mObjectCount=0;  //!< Object count in event for use with TRef

  Float_t mMaxEnergySum=255.0; //! max energy cut on sum of all cells
   
  Int_t readMuDst();
  Int_t mReadMuDst=0;  //! 0= Do clustering and make Fms points
                       //! 1= Just recalc positions based on DB values

  Int_t mGlobalRefit=0;       //! if this is none-zero, perform gloab refit of all photon in a module
  Int_t mMergeSmallToLarge=1; //! if this is none-zero, merge small cells to large cells
  Int_t mTry1PhotonFitWhen2PhotonFitFailed=1; //! if this is none-zero, try 1 photon fit if 2 photon fit failed
  Int_t mCategorizationAlgo=1;    //! choose cluster categorization algo
  Float_t mScaleShowerShapeLarge=0.8;  //! scale shower shape for large cell 
  Float_t mScaleShowerShapeSmall=0.6;  //! scale shower shape for small cell
  Int_t mShowerShapeWithAngle=1;  //! incident angle with 6 slices or not
  Int_t mVertexZ=0;               //! use BBC vertex or not
  Double_t vertexz=0.0;           //! vertex position[cm]

  StMuDst* muDst; //!

  virtual const Char_t *GetCVS() const {static const Char_t cvs[]="Tag $Name:" __DATE__ " " __TIME__ ; return cvs;}
  ClassDef(StFmsPointMaker, 0)
};
#endif  // STROOT_STFMSPOINTMAKER_STFMSPOINTMAKER_H_
