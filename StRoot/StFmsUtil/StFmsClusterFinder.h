// $Id: StFmsClusterFinder.h,v 1.5 2015/10/30 21:33:55 akio Exp $
//
// $Log: StFmsClusterFinder.h,v $
// Revision 1.5  2015/10/30 21:33:55  akio
// fix parameter initialization
// adding new cluster categorization method
//
// Revision 1.4  2015/10/29 21:14:55  akio
// increase max number of clusters
// a bug fixes in valley tower association
// removing some debug comments
//
// Revision 1.3  2015/10/21 15:58:04  akio
// Code speed up (~x2) by optimizing minimization fuctions and showershape function
// Add option to merge small cells to large, so that it finds cluster at border
// Add option to perform 1photon fit when 2photon fit faield
// Add option to turn on/off global refit
// Moment analysis done without ECUTOFF when no tower in cluster exceed ECUTOFF=0.5GeV
//
// Revision 1.2  2015/09/02 15:01:32  akio
// Removing StFmsGeometry class, and now it uses StFmsDbMaker to get appropriate parameters.
//
// Revision 1.1  2015/03/10 14:38:53  jeromel
// First version of FmsUtil from Yuxi Pan - reviewd 2015/02
//
/**
 \file      StFmsClusterFinder.h
 \brief     Declaration of StFmsClusterFinder, an FMS tower clustering algorithm
 \author    Steven Heppelmann <steveheppelmann@gmail.com>
 \author    Yuxi Pan <yuxipan@physics.ucla.edu>
 \author    Thomas Burton <tpb@bnl.gov>
 \date      2014
 \copyright Brookhaven National Lab
 */
#ifndef STROOT_STFMSPOINTMAKER_STFMSCLUSTERFINDER_H_
#define STROOT_STFMSPOINTMAKER_STFMSCLUSTERFINDER_H_

#include <list>
#include <memory>  // For std::unique_ptr

#include "Rtypes.h"  // Provides ROOT ClassDef macro
class TObjArray;

namespace FMSCluster {  // $NMSPC
class StFmsTowerCluster;
class StFmsTower;
// Typedef a list of cluster unique_ptrs for convenience.
// Hide it from CINT as it can't handle parsing the header :(
#ifndef __CINT__
typedef std::list<std::unique_ptr<StFmsTowerCluster>> ClusterList;
#endif  // __CINT__
/**
 Form clusters from a collection of FMS towers.

 Generate a list of StFmsTowerCluster from a collection of StFmsTower.
 */
class StFmsClusterFinder {
 public:
  // Typedef StFmsTower pointer list for convenience
  typedef std::list<FMSCluster::StFmsTower*> TowerList;
  /**
   Constructor.

   The argument sets the energy cutoff on towers - towers below this are not
   included in the calculation of the cluster moments (mean and sigma x, y).
   */
  StFmsClusterFinder(double energyCutoff = 0.5);
  // Use default copy constructor and assignment operator.
  /** Destructor */
  virtual ~StFmsClusterFinder();
  /**
   Calculate moments (mean and sigma of tower (x, y) positions) for a cluster.
   
   Also update the cluster with the current number of towers in it.
   */
  void calculateClusterMoments(StFmsTowerCluster* cluster) const;
  /**
   Categorise a cluster based on its energy and tower distribution.
   
   Set the cluster's category field and return that category.
   See StFmsClusterCategory in StEvent/StFmsCluster.h for valid categories.
   */
  int categorise(StFmsTowerCluster* cluster);
  int categorise2(StFmsTowerCluster* cluster);

  /** Return energy cutoff on towers used when calculating cluster moments */
  double momentEnergyCutoff() const { return mEnergyCutoff; }
#ifndef __CINT__  // Hide ClusterList from CINT
  /**
   Find clusters from a collection of input towers.

   Populate the cluster list with the found clusters.
   Arguments:
    - towers: input tower list. Note it may be modified (elements erased or
              reordered) so do not rely on its contents after calling
              findClusters().
    - clusters: output cluster list. Newly found clusters are appended to the
                list.

   Return the number of found clusters.
   */
   int findClusters(TowerList* towers, ClusterList* clusters, int detetorId);
#endif  // __CINT__

 private:
  static const unsigned kMaxNClusters = 25;  ///< We stop looking after this many
#ifndef __CINT__  // Hide ClusterList from CINT
  /**
   Look for cluster seed towers.

   These are the high towers around which the clusters will grow.
   Fill the cluster list with the found seeds, and the neighbor list with
   adjacent, non-seed towers that we will later assign to a cluster.
   */
  unsigned locateClusterSeeds(TowerList* towers, TowerList* neighbors,
                              ClusterList* clusters) const;
  /**
   Associate towers with cluster seeds.

   Go through a list of unassociated neighbor towers and try to associate each
   tower with a cluster.
    - If a neighbor can associate with only a single cluster, add it
      to that cluster and remove it from the neighbor list.
    - If a neighbor could associate with more than one cluster based on
      currently available information, remove it from the neighbor list and add
      it to the valley list. We will work out the association of the valley
      towers later.

   Return the number of neighbors either associated with clusters or placed in
   the valley i.e. the number removed from the neighbor list.
  */
  unsigned associateTowersWithClusters(TowerList* neighbors,
                                       ClusterList* clusters,
                                       TObjArray* valleys) const;
  /**
   Associate valley towers with clusters

   Valleys towers are those that were equidistant between seeds after the first
   round of association. Now that the seeds have some other towers associated
   with them, use a calculation of the cluster center (using all towers) to find
   the tower-cluster distance and associate each valley with its nearest
   cluster.

   Return the number of valley neighbors moved to clusters.
   */
  unsigned associateValleyTowersWithClusters(TowerList* neighbors,
                                             ClusterList* clusters,
                                             TObjArray* valleys) const;
  /**
   Distribute leftover tower to clusters

   These are towers that remain after the valley tower association.

   Return the number of neighbors associated with clusters.
   */
  unsigned associateResidualTowersWithClusters(TowerList* neighbors,
                                               ClusterList* clusters) const;
  /**
   Add "zero" energy towers to the clusters

   These low-energy towers were ignored in all prior clustering steps.
   They serve the purpose of preventing the creation of bogus peaks,
   where there is no energy deposited at the tower
   */
  void associateSubThresholdTowersWithClusters(TowerList* towers,
                                               ClusterList* clusters) const;
  
#endif  // __CINT__
  Double_t mEnergyCutoff;  ///< Tower energy cutoff for cluster moments
  Int_t mNClusts;  ///< Counter for number of found clusters
  Int_t mDetectorId; //current working detectorId

  ClassDef(StFmsClusterFinder, 0)
};  // class StFmsClusterFinder
}  // namespace FMSCluster
#endif  // STROOT_STFMSPOINTMAKER_STFMSCLUSTERFINDER_H_
