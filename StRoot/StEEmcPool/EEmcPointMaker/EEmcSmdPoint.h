#ifndef __EEmcSmdPoint_h__
#define __EEmcSmdPoint_h__

#include <TObject.h>
#include <TVector3.h>


class EEezTower;
class EEezPatch;
#include "StEEmcPool/EEmcAnalysisMaker/EEezCluster.h"
#include "StEEmcPool/EEmcSmdClusterMaker/EEezSmdCluster.h"

#include <vector>

class EEmcSmdPoint;
typedef std::vector<EEmcSmdPoint*>     EEmcSmdPointPtrVec_t;
typedef EEmcSmdPointPtrVec_t::iterator EEmcSmdPointPtrVecIter_t;

typedef std::vector<EEmcSmdPoint>   EEmcSmdPointVec_t;
typedef EEmcSmdPointVec_t::iterator EEmcSmdPointVecIter_t;

class TH1F;
class EEezTower; 

class EEmcSmdPoint : public TObject {

 public:

  /// Default constructor (don't use, only here for when
  /// I want to create a vector of these points).
  EEmcSmdPoint() { /* nada */ }
  /// Create the smd point from a pair of SMD clusters
  EEmcSmdPoint( EEezSmdCluster *u, EEezSmdCluster *v, Float_t fu=1., Float_t fv=1. );
  
  /// Destructor
  ~EEmcSmdPoint() { /* nada */ };

  /// Get a vector pointing to the intersection point
  TVector3 position();
  /// Get the energy of this point
  Float_t  energy(); 

  /// Return the sector for this point
  Int_t sector();

  /// Get the U cluster
  EEezSmdCluster *clusterU();
  /// Get the V cluster 
  EEezSmdCluster *clusterV(); 

  void setFractions( Float_t fu, Float_t fv );
  void setEnergy( Float_t e );
  
  /// Does this point match a given tower
  Bool_t   match ( EEezTower *tower );
  /// Does this point match a given cluster
  Bool_t   match ( EEezCluster *cluster );
  /// Does this point match a given patch
  Bool_t   match ( EEezPatch *patch );

  /// Sets the tower this cluster sits beneath  
  void setTower( EEezTower *t ); 
  /// Gets the tower this cluster site beneath
  EEezTower *tower(); 
  

 private:
 protected:

  /// Total energy deposited in both planes
  Float_t mEnergy;
  /// SMD cluster in U plane
  EEezSmdCluster *mUcluster;
  /// SMD cluster in V plane
  EEezSmdCluster *mVcluster;
  /// Position vector
  TVector3 mPosition;
  /// Tower which it is nominally sitting beneath 
  EEezTower *mTower; 

  Float_t mFractionU;
  Float_t mFractionV;

  ClassDef(EEmcSmdPoint,1);

};

inline Float_t  EEmcSmdPoint::energy() { return mEnergy; }
inline TVector3 EEmcSmdPoint::position() { return mPosition; }
inline EEezSmdCluster *EEmcSmdPoint::clusterU(){ return mUcluster; }
inline EEezSmdCluster *EEmcSmdPoint::clusterV(){ return mVcluster; } 
inline void EEmcSmdPoint::setTower(EEezTower *tower){ mTower=tower; } 
inline EEezTower *EEmcSmdPoint::tower() { return mTower; } 
inline Int_t EEmcSmdPoint::sector() { return mUcluster->getSector(); }
inline void EEmcSmdPoint::setEnergy(Float_t e){ mEnergy = e; }

#endif
