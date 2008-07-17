#ifndef __StEEmcGenericClusterMaker_h__
#define __StEEmcGenericClusterMaker_h__

/*!
 ******************************************************************************
 *
 * \class StEEmcGenericClusterMaker
 * \date  11/21/2006
 * \author Jason C. Webb <Jason.Webb@Valpo.edu>
 *
 * StEEmcGenericClusterMaker provides a generic framework for finding clusters
 * in the endcap.  The detailed algorithm is left up to the user, who should
 * create a new class which inherits from this class.  An example class, 
 * StMyClusterMaker, is provided:
 *
 * \example StMyClusterMaker.h
 *
 * Example macro:
 *
 * \example macros/rumEEmcClusterMaker.C
 *
 * StEEmcGenericClusterMaker is responsible for storing clusters found by the
 * cluster finding algorithm, forming associations between the clusters found
 * in the different layers of the endcap, for assigning a unique ID ("key")
 * to each cluster.
 *
 * The user builds clusters using the StEEmcCluster (for tower clusters) and 
 * StEEmcSmdCluster (for smd clusters) objects.  The add() method is called
 * for every cluster which is found.  This inserts the cluster into the
 * storage vector, assigns a unique ID to the cluster, and initializes the 
 * data structure which handles the association between clusters in differnt
 * layers.
 *
 * The user should call StEEmcGenericClusterMaker::Make() once all clusters
 * have been found.  This builds the matches between clusters in the different
 * layers.  The user can define how clusters are matched by implementing 
 * a new version of the match() methods in his/her maker.
 *
 * \subsection matching Default Cluster Matching
 *
 * Three methods (match()) are defined which are used by the generic cluster
 * maker to match clusters between layers.  These defaults may be overridden
 * by implementing a new match() function w/in the user's cluster maker.
 *
 * The default methods work as follows:
 *
 * 1. Matching between layers (tower, preshower, postshower)
 *
 * Tower, preshower and postshower clusters will be matched to each other if
 * the seed of the two clusters are in the same or adjacent towers.
 *
 * 2. Matching between smd clusters and tower (preshower/postshower) clusters
 *
 * An SMD cluster is matched to a tower cluster if the mean of the SMD cluster
 * falls within +/- 40 strips (20cm) of the center SMD strip in the seed tower.
 * Matches cannot occur between SMD clusters in one sector and tower clusters
 * with a seed tower in a second sector.
 *
 * 3. Matching between SMD clusters in different planes.
 *
 * SMD clusters must have the same energy w/in +/- 20%, and must both match
 * an existing tower cluster.
 *
 ******************************************************************************
 */

#include "StMaker.h"
#include "StEEmcCluster.h"
#include "StEEmcSmdCluster.h"

class TH1F;
class TH2F;
class StEEmcA2EMaker;

#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"
#include "StEEmcUtil/StEEmcSmd/EEmcSmdGeom.h"
#include "StEEmcUtil/EEmcSmdMap/EEmcSmdMap.h"

#include <map>

#include "TRefArray.h"
#include "TClonesArray.h"

class TH1F;
class TH2F;

//#include "StEEmcClusterCollection.h"

class StMuTrack;
#include "StarClassLibrary/StPhysicalHelixD.hh"

class StEEmcGenericClusterMaker : public StMaker
{

 public:

  StEEmcGenericClusterMaker( const Char_t *name, StEEmcA2EMaker *a2e=NULL );
  ~StEEmcGenericClusterMaker(){ /* nada */ }

  Int_t Init();  

  Int_t Make();

  void  makeHistograms();
  void  makeClusterMap();
  void  makeStEvent();
  void  makeTrackMap();

  void  Clear(Option_t *opts="");

  /// Add a tower (pre/postshower) cluster to the list of clusters
  void add( StEEmcCluster &cluster );

  /// Add a smd cluster to the list of clusters
  void add( StEEmcSmdCluster &cluster );

  /// Searches list of tower (pre/postshower) clusters and removes
  /// the matching cluster.
  void remove( StEEmcCluster &cluster ){ /* needs to be implemented */ }

  /// Searches list of smd clusters and removes the matching cluster.
  void remove( StEEmcSmdCluster &cluster ){ /* needs to be implemented */ }
    
  /// Return a vector of tower clusters
  StEEmcClusterVec_t clusters( Int_t sec, Int_t layer );

  /// Return a vector of smd clusters
  StEEmcSmdClusterVec_t smdclusters( Int_t sec, Int_t plane );

  /// Return a specific cluster from a given sector, layer
  /// @param sector specifies the sector id [0,11]
  /// @param layer specifies which layer 0=T 1=P 2=Q 3=R
  /// @param index specifies which cluster to return
  StEEmcCluster cluster(Int_t sector, Int_t layer, Int_t index);

  /// return a specific cluster from a given sector, plane
  /// @param sector specifies the sector id [0,11]
  /// @param plane specifies which smd plane 0=U 1=V (note difference from "layer" definition)
  /// @param index specifies which cluster to return
  StEEmcSmdCluster smdcluster(Int_t sector, Int_t plane, Int_t index);


  /// returns the number of tracks pointing at the specified cluster
  /// @param cluster
  Int_t numberOfTracks( StEEmcCluster &cluster );
  Int_t numberOfBackgroundTracks( StEEmcCluster &cluster );

  /// return a pointer to a StMuTrack which points at the given
  /// cluster
  /// @param cluster given a cluster, return a pointer to the track which matches it...  yikes this shouldn't be unique...
  /// @param index index of the track pointing to the cluster
  StMuTrack *track( StEEmcCluster &cluster, Int_t index );//{ return mClusterTrackMap[ cluster.key() ]; }
  StMuTrack *backgroundTrack( StEEmcCluster &cluster, Int_t index );//{ return mClusterTrackMap[ cluster.key() ]; }

  /// returns the total number of clusters in a given sector, layer 
  /// @param sector specifies the sector id [0,11]
  /// @param layer specifies which layer 0=T 1=P 2=Q 3=R 4=U 5=V
  Int_t numberOfClusters(Int_t sector, Int_t layer){
    if ( layer < 4 ) 
      return (Int_t)mTowerClusters[ sector ][ layer ].size();
    else if ( layer < 6 )
      return (Int_t)mSmdClusters[ sector ][ layer-4 ].size();
    else
      return -1;
  }
  /// returns the total number of clusters in a given layer
  Int_t numberOfClusters(Int_t layer){ return mNumberOfClusters[layer]; }
  /// returns the total number of clusters summed over all layers
  Int_t numberOfClusters(){ return mClusterId; }

  /// structure describing the matching between tower, preshower, 
  /// postshower and smd clusters
  struct EEmatch 
  {
    StEEmcClusterVec_t  tower;
    StEEmcClusterVec_t  pre1;
    StEEmcClusterVec_t  pre2;
    StEEmcClusterVec_t  post;
    StEEmcSmdClusterVec_t  smdu;
    StEEmcSmdClusterVec_t  smdv;
    StEEmcStripVec_t ustrips;
    StEEmcStripVec_t vstrips;
  };

  /// Returns the clusters which match (see ::match()) the specified 
  /// cluster
  EEmatch clusterMatch( StEEmcCluster &c ) { return mClusterMap[ c.key() ]; }

  /// Returns the current largest cluster ID
  Int_t lastClusterId(){ return mClusterId; }

  /// Returns the number of matching SMD clusters for the given tower cluster and plane
  /// @param cluster a tower cluster
  /// @param plane 0=U 1=V  
  Int_t numberOfMatchingSmdClusters( StEEmcCluster &cluster, Int_t plane );

  /// Returns a specific SMD cluster which matches the tower clster
  /// @param cluster a tower cluster
  /// @param plane 0=U 1=V  
  /// @param index index of the SMD cluster
  StEEmcSmdCluster matchingSmdCluster ( StEEmcCluster &cluster, Int_t plane, Int_t index );

  /// Sets track-cluster matching parameters
  /// @param distance maximum separation in cm from the cluster centroid to the track for a match to be made.
  void setTrackMatching( Float_t distance, Int_t layer ){ mClusterTrackSeparation[layer] = distance; }

  /// extrapolates helix to position z (borrowed from StEEmcPool/TTM)
  Bool_t extrapolateToZ( const StPhysicalHelixD helix, const double z, TVector3 &r);


  /// Builds histograms for SMD clusters matching the specified tower cluster.  Histograms
  /// will be stored in the .hist branch of this maker.  Histograms will follow a naming
  /// convention h[TUV]cluster[key]_[event] where [TUV] ...
  void buildHistograms( StEEmcCluster cluster );

  /// Utility method to provide the "next" cluster id.  It's in public scope to allow 
  /// later makers in the chain (i.e. the point maker) to form new clusters and assign
  /// a unique cluster id (key).
  Int_t nextClusterId(){ return mClusterId++; }

 private:
 protected:

  // EEMC adc to energy
  StEEmcA2EMaker *mEEanalysis;

  //  StEEmcClusterCollection *mCollection;

  // Keeps track of clusters
  Int_t mClusterId;


  // mTowerClusters[sec][layer] provides list of tower
  // clusters at specified layer in the given sector,
  // where the sector is determined by the seed tower.
  std::vector< std::vector< StEEmcClusterVec_t > > mTowerClusters;

  // mSmdClusters[sec][plane] provides a list of SMD
  // clusters in the given sector for the given plane
  std::vector< std::vector< StEEmcSmdClusterVec_t > > mSmdClusters;

  // keeps track of total number of clusters in each layer
  Int_t mNumberOfClusters[6];

  // Pointers to geometry classes
  EEmcGeomSimple *mEEmcGeom;
  EEmcSmdGeom    *mESmdGeom;
  EEmcSmdMap     *mESmdMap;

  /// builder for tower clusters
  Int_t buildTowerClusters(){ return kStOK; }
  /// builder for preshower clusters (both layers)
  Int_t buildPreshowerClusters(){ return kStOK; }
  /// builder for postshower clusters
  Int_t buildPostshowerClusters(){ return kStOK; }
  /// builder for smd clusters
  Int_t buildSmdClusters(){ return kStOK; }

  /// Default methods to determine whether clusters in different layers match.
  /// Tower clusters match pre/postshower clusters if the seed towers are
  /// adjacent.  Smd clusters match tower clusters if they fall within 
  /// +/- mSmdMatchRange strips of the center of the seed tower.
  Bool_t match ( StEEmcCluster &c1, StEEmcCluster &c2 );
  Bool_t match ( StEEmcCluster &c1, StEEmcSmdCluster &c2 );
  Bool_t match ( StEEmcSmdCluster &c1, StEEmcSmdCluster &c2 );

  Bool_t match ( StEEmcCluster &c1, StMuTrack *track );
  Bool_t match ( StEEmcSmdCluster &c1, StMuTrack *track ){ return false; } /* needs to be implemented */  

  Bool_t matchBackgroundTrack ( StEEmcCluster &c1, StMuTrack *track );

  Int_t mSmdMatchRange;

  // hash tables holding relationships between tower clusters and
  // pre/post and/or smd clusters

  // use mClusterMap[ cluster_key ] to grab all clusters which match
  // a given cluster.  All mappings are handled here.
  std::map< Int_t, EEmatch > mClusterMap;

  // use mClusterTrackMap[ cluster_key ] to grab all StMuTracks which
  // are matched to the cluster
  std::map< Int_t, std::vector< StMuTrack* > > mClusterTrackMap;
  std::map< Int_t, std::vector< StMuTrack* > > mBackgroundTrackMap;

  // use mTrackClusterMap[ track->id() ] to grab all clusters 
  // which match the given track (needs implementation)
  std::map< Int_t, EEmatch > mTrackClusterMap;

  // Maximum distance between clusters and tracks where they will
  // be associtated.  TPQRUV
  Float_t mClusterTrackSeparation[6];

  ClassDef(StEEmcGenericClusterMaker,1);
  
};

// returns vector of tower (pre/postshower) clusters
inline StEEmcClusterVec_t StEEmcGenericClusterMaker::clusters(Int_t s, Int_t l) { return mTowerClusters[s][l]; }

// returns vector of smd clusters
inline StEEmcSmdClusterVec_t StEEmcGenericClusterMaker::smdclusters(Int_t s, Int_t l) { return mSmdClusters[s][l]; }

// returns an individual tower (pre/postshower) cluster
inline StEEmcCluster StEEmcGenericClusterMaker::cluster(Int_t s, Int_t l, Int_t i) { return mTowerClusters[s][l][i]; }

// returns an individual smd cluster
inline StEEmcSmdCluster StEEmcGenericClusterMaker::smdcluster(Int_t s,Int_t p,Int_t i) 
{ 
  if ( p < 2 )
    return mSmdClusters[s][p][i];
  else
    assert(0); /* please specify an smd plane 0=U or 1=V */
}

// adds a tower/pre/postshower cluster
//$$$inline void StEEmcGenericClusterMaker::add( StEEmcCluster &c ) { c.key(nextClusterId()); mTowerClusters[ c.tower(0).sector() ][ c.tower(0).layer() ].push_back(c); mNumberOfClusters[c.tower(0).layer()]++; }

// adds an smd cluster
inline void StEEmcGenericClusterMaker::add( StEEmcSmdCluster &c ) { 
  c.key(nextClusterId()); 
  mSmdClusters[ c.sector() ][ c.plane() ].push_back( c ); 
  mNumberOfClusters[c.strip(0).plane()+4]++; 
}

inline StMuTrack *StEEmcGenericClusterMaker::track( StEEmcCluster &cluster, Int_t index ){ return mClusterTrackMap[ cluster.key() ][index]; }
inline StMuTrack *StEEmcGenericClusterMaker::backgroundTrack( StEEmcCluster &cluster, Int_t index ){ return mBackgroundTrackMap[ cluster.key() ][index]; }

inline Int_t StEEmcGenericClusterMaker::numberOfTracks( StEEmcCluster &cluster ){ return (Int_t)mClusterTrackMap[ cluster.key() ].size(); }
inline Int_t StEEmcGenericClusterMaker::numberOfBackgroundTracks( StEEmcCluster &cluster ) { return (Int_t)mBackgroundTrackMap[ cluster.key() ].size(); }
#endif
