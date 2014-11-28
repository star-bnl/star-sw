#ifndef __StEEmcPointMaker_h__
#define __StEEmcPointMaker_h__

#include "StMaker.h"
#include "TString.h"
#include "StEEmcCluster.h"
#include "StEEmcSmdCluster.h"
#include "StEEmcPoint.h" 

#include "StEEmcPointUtils.h"

#include <map>

class StEEmcA2EMaker;
class StEEmcClusterMaker;

class EEmcGeomSimple;
class EEmcSmdMap;
class EEmcSmdGeom; 

class StEEmcPointMaker : public StMaker, public StEEmcPointUtils {

 public:

  StEEmcPointMaker( const Char_t *name );
  ~StEEmcPointMaker(){ /* nada */ };

  Int_t Init();
  Int_t Make();

  void  Clear(Option_t *opts="");

  void analysis(const Char_t *name);
  void clusters(const Char_t *name);

  /// Sets the miminimum energy for matching a pair of smd
  /// clusters to a tower.  SMD pairs which cross beneath
  /// a tower which falls below the threshold will not form
  /// a point.  Default is zero.
  void towerThreshold( Float_t t) { mTowerThreshold=t; }

  /// Return vector of all points found in endcap 
  StEEmcPointVec_t points();
  /// Return number of points
  Int_t numberOfPoints(); 
  /// Return a specified point
  StEEmcPoint point(Int_t ipoint); 

  ///
  Float_t energySeen(){ return mEseen; }

  /// If called, will look for the presence of StEvent and
  /// fill the StEmcCollection.
  void setFillStEvent(){ mFillStEvent=true; }

  /// Energy sharing mode.
  /// \param mode: 0=smd, 1=tower-shape (iterated).
  void setEnergyMode(Int_t mode){ mEnergyMode=mode; }
  void setLimit(Int_t l){ mLimit=l; }


  /// Given an StEmcPoint, return the StEEmcPoint from
  /// whence it came
  StEEmcPoint point( StEmcPoint *p ){ return mEtoEE[p]; }


 private:
 protected:

  TString mNameAnalysis;
  TString mNameClusters;

  StEEmcA2EMaker     *mEEanalysis; //!
  StEEmcClusterMaker *mEEclusters; //! 

  EEmcGeomSimple *mEEtow; //!
  EEmcSmdGeom    *mEEsmd; //!
  EEmcSmdMap     *mEEmap; //! 

  /// Fills the StEmcPoint collection
  void fillStEvent();
  void verifyStEvent();

  /// All fully reconstructed points
  StEEmcPointVec_t mPoints;  //!

  /// SMD only points
  StEEmcPointVec_t mSmdPoints;  //!

  std::map< StEEmcSmdCluster, StEEmcPointVec_t > mUclusters2points; //!
  std::map< StEEmcSmdCluster, StEEmcPointVec_t > mVclusters2points; //!
  
  /// build smd points and associations between smd points and clusters
  StEEmcPointVec_t buildSmdPoints( Int_t sector, StEEmcSmdClusterVec_t &u, StEEmcSmdClusterVec_t &v );
  /// find points in the endcap
  Bool_t findPoints ( Int_t sector, 
		      StEEmcSmdClusterVec_t u, 
		      StEEmcSmdClusterVec_t v, 
		      StEEmcPointVec_t &p );

  /// Divide energy of eemc towers between identified smd points
  void shareEnergy();
  void shareEnergySimple();
  void shareEnergySmd();
  void countRelatives();

  void removeCluster( StEEmcSmdClusterVec_t &clusters, Int_t key );

  /// return the fraction of energy expected in tower 
  /// due to the presence of a point, based on tower
  /// energy response function
  Float_t  fracp2t( StEEmcPoint &p, StEEmcTower &t );

  Float_t mTowerThreshold;

  Float_t mEseen;

  Bool_t mFillStEvent;
  Int_t mEnergyMode;
  Int_t mLimit;

  /// Map connecting StEEmcPoint to StEmcPoint
  std::map<StEmcPoint *, StEEmcPoint> mEtoEE;    //!


ClassDef(StEEmcPointMaker,1);

};

inline void StEEmcPointMaker::analysis(const Char_t *n){ mNameAnalysis=n; }
inline void StEEmcPointMaker::clusters(const Char_t *n){ mNameClusters=n; }

inline StEEmcPointVec_t StEEmcPointMaker::points(){ return mPoints ; }

inline Int_t StEEmcPointMaker::numberOfPoints(){ return mPoints.size(); } 
inline StEEmcPoint StEEmcPointMaker::point(Int_t ip){ return mPoints[ip]; } 
#endif
