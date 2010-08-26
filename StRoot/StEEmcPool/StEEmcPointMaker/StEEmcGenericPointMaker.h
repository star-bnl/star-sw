#ifndef __StEEmcGenericPointMaker_h__
#define __StEEmcGenericPointMaker_h__

/*!
 *
 * \class StEEmcGenericPointMaker
 * \date 11/21/2006
 * \author Jason C. Webb <Jason.Webb@Valpo.edu>
 *
 * StEEmcGenericPointMaker provides a generic framework for finding points (photon/electron candidates)
 * in the EEMC.  The detailed algorithm is left to the user, who should create a new class which
 * inherits from this one.  An example class is provided:
 *
 * \example StMyPointMaker.h
 *
 * Example macro:
 *
 * \example macros/runEEmcPointMaker.C
 *
 * StEEmcGenericPointMaker is responsible for storing the points found by the user's algorithm.
 * The addPoint(), addSmdPoint(), and addTowerPoint() methods are called in order to store the
 * point found by the algorithm.  Calling these methods allows the maker to assign a unique ID
 * to the point, and to keep track of the association between points and clusters.
 *
 * \section Different types of points
 *
 * There are three types of points defined in this class: points, smd points and tower points.
 * The "smd points" and "tower points" are meant to hold information from only one subsystem of
 * the endcap.  Tower points are points derived from only tower information and smd points are
 * derived only from SMD information.  
 *
 * The third type, "points", make use of both tower and SMD information to form our "best guess"
 * list of photon/electron candidates.
 *
 * Three methods are provided: buildPoints(), buildSmdPoints() and buildTowerPoints().  It is 
 * recommended that the user utilize these methods when building more than one type of point.
 * The addPoint(), addSmdPoint(), and addTowerPoint() methods should always be used to add
 * the points found in the algorithm to the data structures.
 *
 *
 */

#include "StMaker.h"

#include <vector>
#include <map>

#include "StEEmcPool/StEEmcA2EMaker/StEEmcA2EMaker.h"
#include "StEEmcPool/StEEmcClusterMaker/StEEmcGenericClusterMaker.h"

#include "StEEmcPoint.h"
//#include "StEvent/StEmcPoint.h"

#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"
#include "StEEmcUtil/StEEmcSmd/EEmcSmdGeom.h"
#include "StEEmcUtil/EEmcSmdMap/EEmcSmdMap.h"

class TH1F;
class TH2F;

class StEEmcGenericPointMaker : public StMaker
{
public:
  StEEmcGenericPointMaker(const Char_t *name="EEmcPointMaker", const StEEmcA2EMaker *a2e=NULL, const StEEmcGenericClusterMaker *cl=NULL );
  virtual ~StEEmcGenericPointMaker(){ /* nada */ };

  virtual Int_t Init();
  virtual Int_t Make();
  virtual void  Clear(Option_t *opts="");

  /// Return vector of EEmc points
  StEEmcPointVec_t &points() { return mPoints ; }
  const StEEmcPointVec_t &points() const { return mPoints ; }
  /// Return vector of smd-only points
  StEEmcPointVec_t &smdPoints() { return mSmdPoints; }
  const StEEmcPointVec_t &smdPoints() const { return mSmdPoints; }
  /// Return vector of tower-only points
  StEEmcPointVec_t &towerPoints() { return mTowerPoints;} 
  const StEEmcPointVec_t &towerPoints() const { return mTowerPoints;} 

  /// Number of points
  Int_t numberOfPoints() const { return mPoints.size(); }
  /// Number of smd-only points
  Int_t numberOfSmdPoints() const { return mSmdPoints.size(); }
  /// Number of tower-only points
  Int_t numberOfTowerPoints() const { return mTowerPoints.size(); }

  /// Return specific EEmc point
  /// \param ipoint runs from 0 to numberOfPoints()-1
  StEEmcPoint &point( Int_t ipoint ) { return mPoints[ipoint]; }
  const StEEmcPoint &point( Int_t ipoint ) const { return mPoints[ipoint]; }
  /// Return specific smd-only point
  /// \param ipoint runs from 0 to numberOfSmdPoints()-1
  StEEmcPoint &smdPoint( Int_t ipoint ) { return mSmdPoints[ipoint]; }
  const StEEmcPoint &smdPoint( Int_t ipoint ) const { return mSmdPoints[ipoint]; }
  /// Return specific tower-only point
  /// \param ipoint runs from 0 to numberOfTowerPoints()-1
  StEEmcPoint &towerPoint( Int_t ipoint ) { return mTowerPoints[ipoint]; }
  const StEEmcPoint &towerPoint( Int_t ipoint ) const { return mTowerPoints[ipoint]; }

  void addPoint(const StEEmcPoint &point );
  void addSmdPoint(const StEEmcPoint &point );
  void addTowerPoint(const StEEmcPoint &point );

  StEEmcPointVec_t &points(const StEEmcCluster &cluster ) { return (*(mCluster2points.find(cluster.key()))).second; }
  const StEEmcPointVec_t &points(const StEEmcCluster &cluster ) const { return (*(mCluster2points.find(cluster.key()))).second; }
  Int_t numberOfPoints(const StEEmcCluster &c ) const { return (Int_t)(*(mCluster2points.find(c.key()))).second.size(); }

  StEEmcCluster &cluster(const StEEmcPoint &point ){ return (*(mPoint2cluster.find(point.key()))).second; }

protected:

  Int_t mKey;
  Int_t nextPointId(){ return mKey++; }

  const StEEmcA2EMaker            *mEEanalysis;                            /**< adc to energy maker */
  const StEEmcGenericClusterMaker *mEEclusters;                            /**< cluster maker       */

  StEEmcPointVec_t mPoints;                                          /**< vector of final points      */
  StEEmcPointVec_t mSmdPoints;                                       /**< vector of smd-only points   */
  StEEmcPointVec_t mTowerPoints;                                     /**< vector of tower-only points */

  StEEmcPointVec_t buildSmdPoints(Int_t sector, const StEEmcSmdClusterVec_t &u, const StEEmcSmdClusterVec_t &v); /**< builder for smd points   */
  StEEmcPointVec_t buildTowerPoints(Int_t sector, const StEEmcClusterVec_t &c );                           /**< builder for tower points */
  StEEmcPointVec_t buildPoints( const StEEmcClusterVec_t &towerClusters, const StEEmcSmdClusterVec_t &u, const StEEmcSmdClusterVec_t &v );   /**< builder for eemc points  */
    
  const EEmcGeomSimple *mEEtow; /**< tower geometry */
  const EEmcSmdGeom    *mEEsmd; /**< smd geometry */
  const EEmcSmdMap     *mEEmap; /**< smd-to-tower map */

  void fillStEvent();

  TH1F *hNumberOfPoints; 
  TH1F *hEnergyOfPoints;
  TH2F *hDistributionOfPoints;

  TH1F *hTotalEnergy[6];       /**< total energy in each layer (TPQRUV) */
  TH1F *hTotalPointEnergy[6];  /**< total energy associated w/ points in each layer (TPQRUV) */

  std::map< Int_t, StEEmcPointVec_t > mCluster2points;
  std::map< Int_t, StEEmcCluster    > mPoint2cluster;

  ClassDef(StEEmcGenericPointMaker,1);
};

#endif
