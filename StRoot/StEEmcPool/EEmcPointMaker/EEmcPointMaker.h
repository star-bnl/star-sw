/*
 * \class EEmcPointMaker
 * \author Jason C. Webb <jwebb@iucf.indiana.edu>
 *
 * A class for analysing the EEMC shower-maximum detector and generating
 * possible track reconstruction points and/or hit points for photons.
 *
 */

#ifndef __EEmcPointMaker_h__
#define __EEmcPointMaker_h__

#include <vector>

#include "StMaker.h"

#include "EEmcPoint.h"
#include "EEmcSmdPoint.h"

#include "StEEmcPool/EEmcAnalysisMaker/EEmcAnalysisMaker.h"
#include "StEEmcPool/EEmcClusterMaker/EEmcClusterMaker.h"
#include "StEEmcPool/EEmcClusterMaker/EEmcClusterMaker2.h"
#include "StEEmcPool/EEmcSmdClusterMaker/EEmcSmdClusterMaker.h"
#include "EEmcPoint.h" 
#include "TTree.h"

#include <map>

//typedef std::map<EEezCluster*,EEmcSmdPointPtrVec_t> EEmcCluster2SmdMap_t;
//typedef std::map<EEmcSmdPoint*,EEezClusterPtrVec_t> EEmcSmd2ClusterMap_t;

typedef std::map<EEezSmdCluster*,EEmcSmdPointPtrVec_t> EEmcSmdCluster2PointMap_t;
typedef std::map<EEmcCluster*,EEmcSmdPointVec_t> EEmcCluster2SmdPointMap_t;
typedef std::map<EEmcCluster*,EEmcPointVec_t> EEmcCluster2PointMap_t;

/////////////////////////////////////////////////////////////////////////////

class EEmcPointMaker : public StMaker {

 public:

  /// Constructor
  EEmcPointMaker( const Char_t *name = "eemcPointMaker" );
  /// Destructor
  ~EEmcPointMaker(){ /* nada */ };

  /// Initialization
  Int_t Init();
  /// Processing
  Int_t Make();
  /// Clear
  void  Clear( Option_t *opts = "" );

  /// Returns the number of points (tower+SMD)
  Int_t numberOfPoints();
  /// Returns a specific point 
  EEmcPoint point(Int_t i);
  /// Returns the vector of points
  EEmcPointVec_t points(); 

  /// Returns the vector of tower-only points
  EEmcPointVec_t towerpoints();

  /// Returns the number of SMD-only points
  Int_t numberOfSmdPoints();
  /// Returns a specific SMD point
  EEmcSmdPoint smdpoint( Int_t i);
  /// Returns the vector of SMD points
  EEmcSmdPointVec_t smdpoints();


 private:
 protected:

  /// Analysis maker
  EEmcAnalysisMaker   *mEEmcAnalysisMaker;   //!
  /// Tower cluster maker
  EEmcClusterMaker    *mEEmcClusterMaker;    //!
  /// Alternate tower cluster maker
  EEmcClusterMaker2   *mEEmcClusterMaker2;   //!
  /// Smd cluster maker
  EEmcSmdClusterMaker *mEEmcSmdClusterMaker; //!

  /// SMD points
  EEmcSmdPointVec_t mSmdPoints;              //!
  EEmcSmdPointVec_t mMatchedSmdPoints;       //!

  /// SMD clusters ordered by index
  EEezSmdClusterPtrVec_t mOrderedClusters;   //!
  EEezSmdClusterPtrVec_t mOrderedU;          //!
  EEezSmdClusterPtrVec_t mOrderedV;          //!

  /// SMD clusters which don't match anything
  EEezSmdClusterPtrVec_t mOrphans;           //!

  /// Associative arrays mapping SMD clusters to the 
  /// SMD points in which they have been used.
  EEmcSmdCluster2PointMap_t mUcluster2point; //!
  EEmcSmdCluster2PointMap_t mVcluster2point; //!

  /// Local copy of the list of clusters found in 
  /// the event
  EEmcClusterVec_t mClusters;


  /// Associative arrays mapping tower-clusters to
  /// the final set of points
  EEmcCluster2PointMap_t mCluster2point; //!

  /// Array in which we store the final EEmcPoints
  EEmcPointVec_t mPoints; 

  /// Array for storing tower-only points
  EEmcPointVec_t mTowerPoints;


  /// Build an ordered cluster list from which we
  /// will work to construct SMD points.  The ordering
  /// is increasing in index, i.e. from eta=2 corner
  /// to eta=1 corner.
  void OrderClusters();


  /// Populates the associative arrays which connect the
  /// SMD clusters (in mOrderedClusters) to all possible
  /// SMD points.
  void BuildArrays( EEezSmdClusterPtrVec_t u, EEezSmdClusterPtrVec_t v );


  /// Constructs all SMD points by pairing all SMD
  /// clusters (U,V).
  Bool_t BuildSmdPoints();


  /// Removes the specified U and V clusters from their
  /// respective ordered arrays.
  void KillSmdClusters( EEezSmdCluster *u, EEezSmdCluster *v );


  /// Build structures matchin tower clusters to SMD points.
  void MatchClusters();


  /// Match up SMD points to tower clusters, and form
  /// EEmcPoints for use by other makers in the chain.
  void BuildPoints();


  /// Builds tower-only points, essentially just the
  /// list of EEezTowers in new convienient point-
  /// form.
  void BuildTowerPoints();


  /// Loops over all SMD clusters and prints out which points
  /// they match up with.
  void print ( EEezSmdClusterPtrVec_t &clusters );

  void printPoints();
  void printArrays();

  Float_t fraction( EEezSmdCluster *, EEezSmdCluster * );
  Float_t fraction( EEezSmdCluster *, EEezSmdCluster *, EEezSmdCluster * );

  ClassDef(EEmcPointMaker,1);

};


inline EEmcPointVec_t EEmcPointMaker::points(){ return mPoints; } 
inline Int_t          EEmcPointMaker::numberOfPoints(){ return (Int_t)mPoints.size(); }
inline EEmcPoint      EEmcPointMaker::point(Int_t p){ return mPoints[p]; }

inline EEmcPointVec_t EEmcPointMaker::towerpoints(){ return mTowerPoints; }

inline EEmcSmdPointVec_t EEmcPointMaker::smdpoints(){ return mMatchedSmdPoints; }
inline Int_t             EEmcPointMaker::numberOfSmdPoints(){ return (Int_t)mMatchedSmdPoints.size(); }
inline EEmcSmdPoint      EEmcPointMaker::smdpoint(Int_t p){ return mMatchedSmdPoints[p]; }

#endif
