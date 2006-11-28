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
  StEEmcGenericPointMaker(const Char_t *name="EEmcPointMaker", StEEmcA2EMaker *a2e=NULL, StEEmcGenericClusterMaker *cl=NULL );
  ~StEEmcGenericPointMaker(){ /* nada */ };

  Int_t Init();
  Int_t Make();
  void  Clear(Option_t *opts="");

  /// Return vector of EEmc points
  StEEmcPointVec_t points();
  /// Return vector of smd-only points
  StEEmcPointVec_t smdPoints();
  /// Return vector of tower-only points
  StEEmcPointVec_t towerPoints();

  /// Number of points
  Int_t numberOfPoints();
  /// Number of smd-only points
  Int_t numberOfSmdPoints();
  /// Number of tower-only points
  Int_t numberOfTowerPoints();

  /// Return specific EEmc point
  /// \param ipoint runs from 0 to numberOfPoints()-1
  StEEmcPoint point( Int_t ipoint );
  /// Return specific smd-only point
  /// \param ipoint runs from 0 to numberOfSmdPoints()-1
  StEEmcPoint smdPoint( Int_t ipoint );
  /// Return specific tower-only point
  /// \param ipoint runs from 0 to numberOfTowerPoints()-1
  StEEmcPoint towerPoint( Int_t ipoint );

  void addPoint( StEEmcPoint &point );
  void addSmdPoint( StEEmcPoint &point );
  void addTowerPoint( StEEmcPoint &point );

  StEEmcPointVec_t points( StEEmcCluster &cluster ) { return mCluster2points[ cluster.key() ]; }
  StEEmcCluster    cluster( StEEmcPoint &point ){ return mPoint2cluster[ point.key() ]; }

 private:
 protected:

  Int_t mKey;
  Int_t nextPointId(){ return mKey++; }

  StEEmcA2EMaker            *mEEanalysis;                            /**< adc to energy maker */
  StEEmcGenericClusterMaker *mEEclusters;                            /**< cluster maker       */

  StEEmcPointVec_t mPoints;                                          /**< vector of final points      */
  StEEmcPointVec_t mSmdPoints;                                       /**< vector of smd-only points   */
  StEEmcPointVec_t mTowerPoints;                                     /**< vector of tower-only points */

  StEEmcPointVec_t buildSmdPoints(Int_t sector, StEEmcSmdClusterVec_t &u, StEEmcSmdClusterVec_t &v); /**< builder for smd points   */
  StEEmcPointVec_t buildTowerPoints(Int_t sector, StEEmcClusterVec_t &c );                           /**< builder for tower points */
  StEEmcPointVec_t buildPoints( StEEmcClusterVec_t &towerClusters, StEEmcSmdClusterVec_t &u, StEEmcSmdClusterVec_t &v );   /**< builder for eemc points  */
    
  EEmcGeomSimple *mEEtow; /**< tower geometry */
  EEmcSmdGeom    *mEEsmd; /**< smd geometry */
  EEmcSmdMap     *mEEmap; /**< smd-to-tower map */

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

inline StEEmcPointVec_t StEEmcGenericPointMaker::points(){ return mPoints ; }
inline Int_t StEEmcGenericPointMaker::numberOfPoints(){ return mPoints.size(); }
inline StEEmcPoint StEEmcGenericPointMaker::point(Int_t ip){ return mPoints[ip]; }

inline StEEmcPointVec_t StEEmcGenericPointMaker::smdPoints(){ return mSmdPoints; }
inline Int_t StEEmcGenericPointMaker::numberOfSmdPoints(){ return mSmdPoints.size(); }
inline StEEmcPoint StEEmcGenericPointMaker::smdPoint(Int_t ip){ return mSmdPoints[ip]; }

inline StEEmcPointVec_t StEEmcGenericPointMaker::towerPoints(){ return mTowerPoints; }
inline Int_t StEEmcGenericPointMaker::numberOfTowerPoints(){ return mTowerPoints.size(); }
inline StEEmcPoint StEEmcGenericPointMaker::towerPoint(Int_t ip){ return mTowerPoints[ip]; }

#endif
