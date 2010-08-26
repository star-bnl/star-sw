#ifndef __StEEmcClusterMaker_h__
#define __StEEmcClusterMaker_h__

#include "StMaker.h"
#include "StEEmcCluster.h"
#include "StEEmcSmdCluster.h"
#include "StEEmcPool/StEEmcA2EMaker/StEEmcA2EMaker.h"
#include "TString.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"
#include "StEEmcUtil/EEmcSmdMap/EEmcSmdMap.h"
#include "StEEmcUtil/StEEmcSmd/EEmcSmdGeom.h"

//#include "StEEmcSimulatorMaker/SlowSimUtil.h" 

#include <map>

class StEEmcClusterMaker : public StMaker {
public:

  StEEmcClusterMaker( const Char_t *name = "mEEclusters" );
  virtual ~StEEmcClusterMaker(){ /* nada */ };

  /// Initialize
  virtual Int_t Init();
  /// Make clusters for this event
  virtual Int_t Make();
  /// Clear clusters for next event
  virtual void  Clear(Option_t *opts="");

  /// Set the name of the ADC-->E maker
  void analysis ( const Char_t *name ) { mAnalysisName=name; }

  /// Set the seed energy for the specified layer
  /// where 0=T, 1=P, 2=Q, 3=R, 4=U, 5=V
  void seedEnergy( Float_t energy, Int_t layer=0 ) { mSeedEnergy[layer]=energy; }

  /// Maximum distance around seed strip to cluster smd strips  
  void setMaxExtent( Int_t m ) { mMaxExtent=m; }

  /// Minimum number of strips to form a cluster
  void setMinStrips( Int_t m ){ mMinStrips=m; }

  /// Factor above which an smd strip must exceed the minimum
  /// "floor" energy in order to be considered a seed... 
  void setSeedFloor( Float_t f=2.0 ) { mSeedFloor=f; } 

  /// Return number of clusters for a given sector, layer
  Int_t numberOfClusters(Int_t sec, Int_t layer) const { return (Int_t)mTowerClusters[sec][layer].size(); }
  /// Return number of smd clusters for a given sector, plane
  Int_t numberOfSmdClusters(Int_t sec, Int_t plane) const { return (Int_t)mSmdClusters[sec][plane].size(); }

  /// Return a specific cluster from a given sector, layer
  StEEmcCluster &cluster(Int_t sec, Int_t layer, Int_t index) { return mTowerClusters[sec][layer][index]; }
  const StEEmcCluster &cluster(Int_t sec, Int_t layer, Int_t index) const { return mTowerClusters[sec][layer][index]; }
  /// return a specific cluster from a given sector, plane 
  StEEmcSmdCluster &smdcluster(Int_t sec, Int_t plane, Int_t index) { return mSmdClusters[sec][plane][index]; }
  const StEEmcSmdCluster &smdcluster(Int_t sec, Int_t plane, Int_t index) const { return mSmdClusters[sec][plane][index]; }

  /// Given a StEmcCluster, return the StEEmcCluster where it came from
  StEEmcCluster &cluster(const StEmcCluster *cl ){ return (*(mEtoEE.find(cl))).second; }
  const StEEmcCluster &cluster(const StEmcCluster *cl ) const { return (*(mEtoEE.find(cl))).second; }
  /// Given a StEmcCluster, return the StEEmcSmdCluster where it came from
  StEEmcSmdCluster &smdcluster(const StEmcCluster *cl ){ return (*(mEtoEEsmd.find(cl))).second; }
  const StEEmcSmdCluster &smdcluster(const StEmcCluster *cl ) const { return (*(mEtoEEsmd.find(cl))).second; }

  /// Return a vector of tower clusters
  StEEmcClusterVec_t &clusters( Int_t sec, Int_t layer ) { return mTowerClusters[sec][layer]; }
  const StEEmcClusterVec_t &clusters( Int_t sec, Int_t layer ) const { return mTowerClusters[sec][layer]; }
  /// Return a vector of smd clusters
  StEEmcSmdClusterVec_t &smdclusters( Int_t sec, Int_t plane ) { return mSmdClusters[sec][plane]; } 
  const StEEmcSmdClusterVec_t &smdclusters( Int_t sec, Int_t plane ) const { return mSmdClusters[sec][plane]; } 

  /// If called, will look for the presence of StEvent and
  /// fill the StEmcCollection.
  void setFillStEvent(){ mFillStEvent=true; }

  /// Suppress seeds in the n strips on either side of an 
  /// already identified smd cluster.  Default = 0.
  void suppress(Int_t n=2){ mSuppress=n; } 

  /// Skips over strips with "fail" bits set, if true
  void skip(Bool_t s=true){ mSkip=s; }

  /// Loose cuts (see code)
  void loose(Bool_t l=true){ mLoose=l; }

  /// Event summary
  void print() const;

protected:

  /// Keep track of clusters
  Int_t mClusterId;
  
  /// Supress seeds adjacent to clusters
  Int_t mSuppress; 
  /// Skip strips if failbit set
  Bool_t mSkip;
  /// Loose cuts option
  Bool_t mLoose;

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
  /// Minimum number of smd strips to form seed
  Int_t mMinStrips;
  /// blah...
  Float_t mSeedFloor; 

  /// Constructs tower clusters
  virtual Bool_t buildTowerClusters();

  /// Constructs smd clusters
  virtual Bool_t buildSmdClusters();

  /// Fills StEvent cluster collections if the option is selected
  void fillStEvent();

  /// Verify that StEEmcCluster/StEEmcSmdCluster and StEmcCluster
  /// are equivalent
  Bool_t verifyStEvent() const;

  // Geometry classes
  const EEmcGeomSimple *mEEtow;  /**<- pointer to tower geometry */
  const EEmcSmdGeom    *mEEsmd;  /**<- pointer to smd geometry */
  const EEmcSmdMap     *mEEmap;  /**<- pointer to tower to smd map */

  /// ADC-->E maker name
  TString mAnalysisName;
  /// ADC-->E maker
  const StEEmcA2EMaker *mEEanalysis;

  /// Option to fill StEvent
  Bool_t mFillStEvent;

  /// Map StEEmcClusters to StEmcClusters
  std::map<const StEmcCluster *, StEEmcCluster > mEtoEE;       //!
  /// ... and for smd clusters
  std::map<const StEmcCluster *, StEEmcSmdCluster > mEtoEEsmd; //!

  ClassDef(StEEmcClusterMaker,1);
};

#endif
