#ifndef __StEEmcIUClusterMaker_h__
#define __StEEmcIUClusterMaker_h__

#include "StMaker.h"
#include "StEEmcIUCluster.h"
#include "StEEmcIUSmdCluster.h"
#include "StEEmcPool/StEEmcA2EMaker/StEEmcA2EMaker.h"
#include "TString.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"
#include "StEEmcUtil/EEmcSmdMap/EEmcSmdMap.h"
#include "StEEmcUtil/StEEmcSmd/EEmcSmdGeom.h"

//#include "StEEmcSimulatorMaker/SlowSimUtil.h" 

#include <map>

class StEEmcIUClusterMaker : public StMaker { //, public SlowSimUtil {

 public:

  StEEmcIUClusterMaker( const Char_t *name = "mEEclusters" );
  ~StEEmcIUClusterMaker(){ /* nada */ };

  /// Initialize
  Int_t Init();
  /// Make clusters for this event
  Int_t Make();
  /// Clear clusters for next event
  void  Clear(Option_t *opts="");

  /// Set the name of the ADC-->E maker
  void analysis ( const Char_t *name );

  /// Set the seed energy for the specified layer
  /// where 0=T, 1=P, 2=Q, 3=R, 4=U, 5=V
  void seedEnergy( Float_t energy, Int_t layer=0 );  

  /// Maximum distance around seed strip to cluster smd strips  
  void setMaxExtent( Int_t m );  

  /// Minimum number of strips to form a cluster
  void setMinStrips( Int_t m ){ mMinStrips=m; }

  /// Factor above which an smd strip must exceed the minimum
  /// "floor" energy in order to be considered a seed... 
  void setSeedFloor( Float_t f=2.0 ); 

  /// Return number of clusters for a given sector, layer
  Int_t numberOfClusters(Int_t sec, Int_t layer);
  /// Return number of smd clusters for a given sector, plane
  Int_t numberOfSmdClusters(Int_t sec, Int_t plane);
  Int_t TnumberOfSmdClusters(Int_t sec, Int_t plane);
  //return the difference of two u clusters' centroid
  //float diffOf2uclusters();
  //float diffOf2vclusters();
  //return the number of seed in u plan all sectors
  int numberOfUseed();
  int numberOfVseed();
  int UStripWidth();
  int VStripWidth();
  float Tenergyoftower();
  float Tenergyofp1();
  float Tenergyofp2();
  float Tenergyofp3();
  float Tenergyofsmdu();
  float Tenergyofsmdv();
  float UAmplitude();
  float VAmplitude();

  /// Return a specific cluster from a given sector, layer
  StEEmcIUCluster cluster(Int_t sec, Int_t layer, Int_t index);
  /// return a specific cluster from a given sector, plane 
  StEEmcIUSmdCluster smdcluster(Int_t sec, Int_t plane, Int_t index);


  /// Given a StEmcCluster, return the StEEmcIUCluster from whence
  /// it came.
  StEEmcIUCluster cluster( StEmcCluster *cl ){ return mEtoEE[ cl ]; }
  /// Given a StEmcCluster, return the StEEmcIUSmdCluster from
  /// whence it came.
  StEEmcIUSmdCluster smdcluster( StEmcCluster *cl ){ return mEtoEEsmd[ cl ]; }

  /// Return a vector of tower clusters
  StEEmcIUClusterVec_t clusters( Int_t sec, Int_t layer );
  /// Return a vector of smd clusters
  StEEmcIUSmdClusterVec_t smdclusters( Int_t sec, Int_t plane ); 

  /// If called, will look for the presence of StEvent and
  /// fill the StEmcCollection.
  void setFillStEvent(){ mFillStEvent=true; }

  /// Suppress seeds in the n strips on either side of an 
  /// already identified smd cluster.  Default = 0.
  void suppress(Int_t n=0){ mSuppress=n; } 

  /// Skips over strips with "fail" bits set, if true
  void skip(Bool_t s=true){ mSkip=s; }

  /// Loose cuts (see code)
  void loose(Bool_t l=true){ mLoose=l; }

  /// Event summary
  void print();

 private:
 protected:

  TH1D        *clusize;
  TH1D        *tclusize;
  TH1D        *cludis;
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
  std::vector< std::vector< StEEmcIUClusterVec_t > > mTowerClusters;
  /// mSmdClusters[sec][plane] provides a list of SMD
  /// clusters in the given sector for the given plane
  std::vector< std::vector< StEEmcIUSmdClusterVec_t > > mSmdClusters;
  std::vector< std::vector< StEEmcIUSmdClusterVec_t > > TmSmdClusters;

  /// Counts clusters for full eemc, 0=T, 1=P, 2=Q, 3=R, 4=U, 5=V
  Int_t mNumberOfClusters[6]; 
  Int_t TmNumberOfClusters[6]; 

  /// Seed energy for 0=T, 1=P, 2=Q, 3=R, 4=U, 5=V
  Float_t mSeedEnergy[6];
  /// Maximum distance from SMD seed strips  
  Int_t mMaxExtent; 
  /// Minimum number of smd strips to form seed
  Int_t mMinStrips;
  /// blah...
  Float_t mSeedFloor;
  //float UMeanclust;
  //float VMeanclust;
  int countUseed;
  int uswidth;
  int vswidth;
  float eeen;
  int countVseed;
  float uamp;
  float vamp;
  float ep1;
  float ep2;
  float ep3;
  float esmdu;
  float esmdv;
  /// Constructs tower clusters
  Bool_t buildTowerClusters();

  /// Constructs smd clusters
  Bool_t buildSmdClusters();

  /// Fills StEvent cluster collections if the option is selected
  void fillStEvent();

  /// Verify that StEEmcIUCluster/StEEmcIUSmdCluster and StEmcCluster
  /// are equivalent
  Bool_t verifyStEvent();

  FILE *fout;
  FILE *fout2;
  
  // Geometry classes
  EEmcGeomSimple *mEEtow;  /**<- pointer to tower geometry */
  EEmcSmdGeom    *mEEsmd;  /**<- pointer to smd geometry */
  EEmcSmdMap     *mEEmap;  /**<- pointer to tower to smd map */

  /// ADC-->E maker name
  TString mAnalysisName;
  /// ADC-->E maker
  StEEmcA2EMaker *mEEanalysis;

  /// Option to fill StEvent
  Bool_t mFillStEvent;

  /// Map StEEmcIUClusters to StEmcClusters
  std::map< StEmcCluster *, StEEmcIUCluster > mEtoEE;       //!
  /// ... and for smd clusters
  std::map< StEmcCluster *, StEEmcIUSmdCluster > mEtoEEsmd; //!

  /// Makes class available to root
  ClassDef(StEEmcIUClusterMaker,1);

};

inline void StEEmcIUClusterMaker::analysis( const Char_t *name ){ mAnalysisName=name; }
inline void StEEmcIUClusterMaker::seedEnergy(Float_t energy, Int_t layer){ mSeedEnergy[layer]=energy; }

inline void StEEmcIUClusterMaker::setMaxExtent(Int_t m){ mMaxExtent=m; } 
inline void StEEmcIUClusterMaker::setSeedFloor(Float_t f){ mSeedFloor=f; } 

inline Int_t StEEmcIUClusterMaker::numberOfClusters(Int_t s, Int_t l) 
	{ return (Int_t)mTowerClusters[s][l].size(); }
inline Int_t StEEmcIUClusterMaker::numberOfSmdClusters(Int_t s, Int_t p) 
	{ return (Int_t)mSmdClusters[s][p].size(); }
inline Int_t StEEmcIUClusterMaker::TnumberOfSmdClusters(Int_t s, Int_t p) 
	{ return (Int_t)TmSmdClusters[s][p].size(); }

inline StEEmcIUCluster StEEmcIUClusterMaker::cluster(Int_t s, Int_t l, Int_t i)
	{ return mTowerClusters[s][l][i]; }
inline StEEmcIUSmdCluster StEEmcIUClusterMaker::smdcluster(Int_t s,Int_t p,Int_t i)
	{ return mSmdClusters[s][p][i]; }

inline StEEmcIUClusterVec_t StEEmcIUClusterMaker::clusters(Int_t s, Int_t l)
    	{ return mTowerClusters[s][l]; }

inline StEEmcIUSmdClusterVec_t StEEmcIUClusterMaker::smdclusters(Int_t s, Int_t l)
    	{ return mSmdClusters[s][l]; } 
//inline float StEEmcIUClusterMaker::diffOf2uclusters() { return (float) abs(UMeanclust); }
//inline float StEEmcIUClusterMaker::diffOf2vclusters() { return (float) abs(VMeanclust); }
inline int StEEmcIUClusterMaker::numberOfUseed() { return (int) countUseed; }
inline int StEEmcIUClusterMaker::numberOfVseed() { return (int) countVseed; }
inline float StEEmcIUClusterMaker::Tenergyoftower() { return (float) eeen; }
inline int StEEmcIUClusterMaker::UStripWidth() { return (int) uswidth; }
inline int StEEmcIUClusterMaker::VStripWidth() { return (int) vswidth; }
inline float StEEmcIUClusterMaker::UAmplitude() { return (float) uamp; }
inline float StEEmcIUClusterMaker::VAmplitude() { return (float) vamp; }
inline float StEEmcIUClusterMaker::Tenergyofp1() { return (float) ep1; }
inline float StEEmcIUClusterMaker::Tenergyofp2() { return (float) ep2; }
inline float StEEmcIUClusterMaker::Tenergyofp3() { return (float) ep3; }
inline float StEEmcIUClusterMaker::Tenergyofsmdu() { return (float) esmdu; }
inline float StEEmcIUClusterMaker::Tenergyofsmdv() { return (float) esmdv; }
#endif
