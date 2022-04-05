#ifndef __StEEmcClusterMaker_h__
#define __StEEmcClusterMaker_h__

#include "StMaker.h"
#include "StEEmcCluster.h"
#include "StEEmcSmdCluster.h"
#include "StEEmcA2EMaker.h"
#include "TString.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"
#include "StEEmcUtil/EEmcSmdMap/EEmcSmdMap.h"
#include "StEEmcUtil/StEEmcSmd/EEmcSmdGeom.h"

#include <map>

class StEEmcClusterMaker : public StMaker {

 public:

  StEEmcClusterMaker( const Char_t *name = "mEEclusters" );
  ~StEEmcClusterMaker(){ /* nada */ };

  Int_t Init();
  Int_t Make();
  void  Clear(Option_t *opts="");

  /// Set the name of the ADC-->E maker
  void analysis ( const Char_t *name );

  /// Set the seed energy for the specified layer
  /// where 0=T, 1=P, 2=Q, 3=R, 4=U, 5=V
  void seedEnergy( Float_t energy, Int_t layer=0 );  

  /// Maximum distance around seed strip to cluster smd strips  
  void setMaxExtent( Int_t m );  

  /// Factor above which an smd strip must exceed the minimum
  /// "floor" energy in order to be considered a seed... 
  void setSeedFloor( Float_t f=2.0 ); 

  /// Return number of clusters for a given sector, layer
  Int_t numberOfClusters(Int_t sec, Int_t layer);
  /// Return number of smd clusters for a given sector, plane
  Int_t numberOfSmdClusters(Int_t sec, Int_t plane);

  /// Return a specific cluster from a given sector, layer
  StEEmcCluster cluster(Int_t sec, Int_t layer, Int_t index);
  /// return a specific cluster from a given sector, plane 
  StEEmcSmdCluster smdcluster(Int_t sec, Int_t plane, Int_t index);


  /// Given a StEmcCluster, return the StEEmcCluster from whence
  /// it came.
  StEEmcCluster cluster( StEmcCluster *cl ){ return mEtoEE[ cl ]; }
  /// Given a StEmcCluster, return the StEEmcSmdCluster from
  /// whence it came.
  StEEmcSmdCluster smdcluster( StEmcCluster *cl ){ return mEtoEEsmd[ cl ]; }

  /// Return a vector of tower clusters
  StEEmcClusterVec_t clusters( Int_t sec, Int_t layer );
  /// Return a vector of smd clusters
  StEEmcSmdClusterVec_t smdclusters( Int_t sec, Int_t plane ); 

  /// If called, will look for the presence of StEvent and
  /// fill the StEmcCollection.
  void setFillStEvent(){ mFillStEvent=true; }

  /// Suppress cluster splitting
  void suppress(){ mSuppress=true; } 

  void print();

 private:
 protected:

  Int_t mClusterId;

  Bool_t mSuppress; 

  /// mSeedTowers[sec][layer] provides a list of tower
  /// elements which exceeded the user-specified seed.
  std::vector< std::vector< StEEmcTowerVec_t > > mSeedTowers;

  /// mTowerClusters[sec][layer] provides list of tower
  /// clusters at specified layer in the given sector,
  /// where the sector is determined by the seed tower.
  std::vector< std::vector< StEEmcClusterVec_t > > mTowerClusters;
  /// mSmdClusters[sec][plane] provides a list of SMD
  /// clusters in the given sector for the given plane
  std::vector< std::vector< StEEmcSmdClusterVec_t > > mSmdClusters;

  /// Counts clusters for full eemc, 0=T, 1=P, 2=Q, 3=R, 4=U, 5=V
  Int_t mNumberOfClusters[6]; 

  /// Seed energy for 0=T, 1=P, 2=Q, 3=R, 4=U, 5=V
  Float_t mSeedEnergy[6];
  /// Maximum distance from SMD seed strips  
  Int_t mMaxExtent; 
  ///
  Float_t mSeedFloor; 

  /// Constructs tower clusters
  Bool_t buildTowerClusters();

  /// Constructs smd clusters
  Bool_t buildSmdClusters();

  /// Fills StEvent cluster collections if the option is selected
  void fillStEvent();

  /// Verify that StEEmcCluster/StEEmcSmdCluster and StEmcCluster
  /// are equivalent
  Bool_t verifyStEvent();

  /// Geometry classes
  EEmcGeomSimple *mEEtow;
  EEmcSmdGeom    *mEEsmd;
  EEmcSmdMap     *mEEmap;

  /// ADC-->E maker name
  TString mAnalysisName;
  /// ADC-->E maker
  StEEmcA2EMaker *mEEanalysis;

  Bool_t mFillStEvent;

  /// Map StEEmcClusters to StEmcClusters
  std::map< StEmcCluster *, StEEmcCluster > mEtoEE;       //!
  // ... and for smd clusters
  std::map< StEmcCluster *, StEEmcSmdCluster > mEtoEEsmd; //!

  ClassDef(StEEmcClusterMaker,1);

};

inline void StEEmcClusterMaker::analysis( const Char_t *name ){ mAnalysisName=name; }
inline void StEEmcClusterMaker::seedEnergy(Float_t energy, Int_t layer){ mSeedEnergy[layer]=energy; }

inline void StEEmcClusterMaker::setMaxExtent(Int_t m){ mMaxExtent=m; } 
inline void StEEmcClusterMaker::setSeedFloor(Float_t f){ mSeedFloor=f; } 

inline Int_t StEEmcClusterMaker::numberOfClusters(Int_t s, Int_t l) 
	{ return (Int_t)mTowerClusters[s][l].size(); }
inline Int_t StEEmcClusterMaker::numberOfSmdClusters(Int_t s, Int_t p) 
	{ return (Int_t)mSmdClusters[s][p].size(); }

inline StEEmcCluster StEEmcClusterMaker::cluster(Int_t s, Int_t l, Int_t i)
	{ return mTowerClusters[s][l][i]; }
inline StEEmcSmdCluster StEEmcClusterMaker::smdcluster(Int_t s,Int_t p,Int_t i)
	{ return mSmdClusters[s][p][i]; }

inline StEEmcClusterVec_t StEEmcClusterMaker::clusters(Int_t s, Int_t l)
    	{ return mTowerClusters[s][l]; }

inline StEEmcSmdClusterVec_t StEEmcClusterMaker::smdclusters(Int_t s, Int_t l)
    	{ return mSmdClusters[s][l]; } 

#endif
