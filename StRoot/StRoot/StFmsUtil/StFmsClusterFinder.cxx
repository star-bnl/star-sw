// $Id: StFmsClusterFinder.cxx,v 1.9 2018/03/02 20:27:29 akio Exp $
//
// $Log: StFmsClusterFinder.cxx,v $
// Revision 1.9  2018/03/02 20:27:29  akio
// Big update from	Zhanwen Zhu with new shower shape and six z slices
//
// Revision 1.8  2018/01/04 17:35:44  smirnovd
// [Cosmetic] Remove StRoot/ from include path
//
// $STAR/StRoot is already in the default path search
//
// Revision 1.7  2016/06/08 19:58:33  akio
// Applying Coverity report
//
// Revision 1.6  2016/06/07 15:51:44  akio
// Making code better based on Coverity reports
//
// Revision 1.5  2015/11/02 22:44:49  akio
// Fix photonEnergyInTower()
//
// Revision 1.4  2015/10/30 21:33:55  akio
// fix parameter initialization
// adding new cluster categorization method
//
// Revision 1.3  2015/10/29 21:14:55  akio
// increase max number of clusters
// a bug fixes in valley tower association
// removing some debug comments
//
// Revision 1.2  2015/10/21 15:58:04  akio
// Code speed up (~x2) by optimizing minimization fuctions and showershape function
// Add option to merge small cells to large, so that it finds cluster at border
// Add option to perform 1photon fit when 2photon fit faield
// Add option to turn on/off global refit
// Moment analysis done without ECUTOFF when no tower in cluster exceed ECUTOFF=0.5GeV
//
// Revision 1.1  2015/03/10 14:38:54  jeromel
// First version of FmsUtil from Yuxi Pan - reviewd 2015/02
//
/**
 \file      StFmsClusterFinder.cxx
 \brief     Implementation of StFmsClusterFinder,
            an FMS tower clustering algorithm
 \author    Steven Heppelmann <steveheppelmann@gmail.com>
 \author    Yuxi Pan <yuxipan@physics.ucla.edu>
 \author    Thomas Burton <tpb@bnl.gov>
 \date      2014
 \copyright Brookhaven National Lab
 */
#include "StFmsClusterFinder.h"
#include "StFmsDbMaker/StFmsDbMaker.h"
#include "StFmsDbConfig.h"

#include <algorithm>
#include <cmath>
#include <functional>
#include <memory>  // For unique_ptr

#include "TObjArray.h"

#include "St_base/StMessMgr.h"
#include "StEvent/StFmsCluster.h"
#include "StEvent/StFmsHit.h"

#include "StFmsUtil/StFmsTower.h"
#include "StFmsUtil/StFmsConstant.h"
#include "StFmsUtil/StFmsTowerCluster.h"

namespace {
typedef FMSCluster::StFmsClusterFinder::TowerList TowerList;
typedef TowerList::const_reverse_iterator TowerConstRIter;

using FMSCluster::StFmsTower;

/*
 Test if a tower could be a cluster peak compared to another tower.

 Note returning true does not mean the tower *is* a peak, merely that it *can*
 be (i.e. it is consistent with that hypothesis given this input).
 */
bool couldBePeakTower(const StFmsTower* tower, const StFmsTower* other) {
  return (tower->hit()->energy() >= PEAK_TOWER_FACTOR * other->hit()->energy()) ? true : false;
}

/*
 Test if a tower could be a peak compared to a group of neighbor towers.

 Returns true if the towers passes the peak test with all neighbors, false if
 it fails the test with any.
 */
bool couldBePeakTower(const StFmsTower* tower, TowerList* others) {
  for (auto i = others->begin(); i != others->end(); ++i) {
    if (tower->isNeighbor(**i) && !couldBePeakTower(tower, *i)) {
      return false;
    }  // if
  }  // for
  return true;
}

/** Comparison function to sort towers in order of ascending energy. */
bool ascendingTowerEnergySorter(const StFmsTower* a, const StFmsTower* b) {
  return a->hit()->energy() < b->hit()->energy();
}

/** Comparison function to sort towers in order of descending energy. */
bool descendingTowerEnergySorter(const StFmsTower* a, const StFmsTower* b) {
  return a->hit()->energy() > b->hit()->energy();
}

/* Predicate testing for tower energy above the global cutoff */
bool towerEnergyIsAboveThreshold(const StFmsTower* tower) {
  return !(tower->hit()->energy() < TOWER_E_THRESHOLD);
}

/**
 Predicate determining if a test tower is a neighbour of a reference tower.
 
 A neighbor is defined as the tower immediately above, below or to the side
 i.e not diagonally adjacent cells.
 If the test tower has energy below the global cutoff, always return false,
 even if it is physically a neighbour of the reference tower.
 */
bool towerIsNeighbor(const StFmsTower* test, const StFmsTower* reference) {
  if (towerEnergyIsAboveThreshold(test)) {
    return test->isNeighbor(*reference);
  }  // if
  return false;
}

/*
 Filter out towers below the minimum energy threshold from a list.

 Returns a pointer list of below-threshold towers and erases those towers from
 the input list. The order of towers after filtering is not guaranteed.
 */
TowerList filterTowersBelowEnergyThreshold(TowerList* towers) {
  // Move towers above threshold to the front and those below to the end
  // newEnd marks the end of the above-threshold towers and the beginning of the
  // below-threshold towers
  auto newEnd = std::partition(towers->begin(), towers->end(),
                               std::ptr_fun(&towerEnergyIsAboveThreshold));
  // Store the below-threshold towers in a new list
  TowerList belowThreshold(newEnd, towers->end());
  // Remove the below-threshold towers from the input list
  towers->erase(newEnd, towers->end());
  return belowThreshold;
}

/* There are different ways of calculating a tower-to-cluster distance */
enum ETowerClusterDistance {
  kPeakTower,  // Distance from tower to peak tower in cluster
  kClusterCenter  // Distance from tower to calculated center of cluster
};

/** Sort the towers in an array of clusters in order of descending energy. */
void sortTowersEnergyDescending(FMSCluster::ClusterList* clusters) {
  for (auto i = clusters->begin(); i != clusters->end(); ++i) {
    (*i)->towers().sort(std::ptr_fun(&descendingTowerEnergySorter));
  }  // for
}
}  // unnamed namespace

namespace FMSCluster {
/**
 Association information between a tower and clusters.

 This class is used in determining correct tower-cluster association.
 Stores an StFmsTower, and a list of StFmsTowerCluster with which it could
 potentially be associated.
 This information is used in determining the true cluster with
 which the tower is actually associated.

 Inherits from TObject to allow it to be placed in a ROOT container.
 */ 
class TowerClusterAssociation : public TObject {
 public:
  /**
   Constructor.
   
   Initialise with the StFmsTower of interest.
   */
  explicit TowerClusterAssociation(StFmsTower* tower, int detectorId) : mTower(tower), mDetectorId(detectorId){ }
  /** Returns this tower. */
  StFmsTower* tower() { return mTower; }
  /** \overload */
  const StFmsTower* tower() const { return mTower; }
  /** Returns the list of potential associate clusters */
  std::list<StFmsTowerCluster*>* clusters() { return &mClusters; }
  /**
   Calculate the separation between this tower and another.

   The separation is in row-column coordinates i.e. the distance is in the
   number of towers, not in cm. e.g. a tower at (column=1, row=1) would be
   a distance of 1 from a tower at (1, 2), and sqrt(2) from a tower at (2, 2).
   */
  double separation(const StFmsTower* tower) {
      int det0=mTower->hit()->detectorId();
      int det1=tower->hit()->detectorId();
      //within the same detector
      if(det0==det1){
	  return sqrt(pow(tower->column() - mTower->column(), 2.) +
		      pow(tower->row() - mTower->row(), 2.));
      }
      //different detector (large and small) - make small cell fit to large
      int rowL,colL,rowS,colS;
      if(det0<det1){
	  rowL=mTower->row();    rowS=tower->row();
	  colL=mTower->column(); colS=tower->column();
      }else{
	  rowS=mTower->row();    rowL=tower->row();
	  colS=mTower->column(); colL=tower->column();
      }      
      float rL=rowL-0.5;
      float cL=colL-0.5;
      float rS=(rowS-0.5)/1.5 + 9.0;
      float cS=(colS-0.5)/1.5; 
      return sqrt(pow(rL-rS,2.0)+pow(cL-cS,2.0));
  }
  /**
   Calculate the separation between this tower and a cluster.

   Distances can be calculated in different ways:
    - distance=kPeakTower: distance from this tower to the peak (highest-energy)
                           tower in the cluster.
    - distance=kClusterCenter: distance from this tower to the cluster center,
                               based on cluster moment calculation.
   See separation(const StFmsTower*) for the distance definition.
   */
  double separation(const StFmsTowerCluster* cluster,
                    const ETowerClusterDistance distance) {
    if (kPeakTower == distance) {
      return separation(cluster->towers().front());
    } else {
	// Use calculated cluster center (x0, y0).
	// Subtract 0.5 from tower (column, row) to give tower center.	
	if(mTower->hit()->detectorId() == mDetectorId){ //within the same detector
	    return sqrt(pow(cluster->cluster()->x() - (mTower->column() - 0.5), 2.) +
			pow(cluster->cluster()->y() - (mTower->row() - 0.5), 2.));
	}else{ //different detector (large cell cluster and small cell tower) - make small cell fit to large
	    float col=(mTower->column()-0.5)/1.5; 
	    float row=(mTower->row()-0.5)/1.5+9.0; 
	    return sqrt(pow(cluster->cluster()->x() - col,2.0) +
			pow(cluster->cluster()->y() - row,2.0));
	}  // if	
    }  
  }
  /**
   Returns true if this tower can be associated with a cluster.

   A tower is defined as associable with a cluster if it:
    - has energy less than that of the cluster's highest-energy tower.
    - is physically adjacent to at least one tower in the cluster, so long as...
    - it cannot fulfil the criterion for being a peak w.r.t. to
      the adjacent tower.

   Note this is a "potential" association; a tower may pass the association
   test with more than one cluster, but it will eventually be assigned
   unambiguously to a single cluster.
   */
  bool canAssociate(const StFmsTowerCluster* cluster) {
    const StFmsTowerCluster::Towers& towers = cluster->towers();
    // The peak tower in a cluster is always the first
    const StFmsTower* peak = towers.front();
    // Make sure that this tower has lower energy than the peak, but be careful;
    // because of digitization, it is possible that the "neighbor" tower
    // has the exact same energy as the peak tower, not just less
    if (peak->hit()->energy() < mTower->hit()->energy()) {
      return false;
    }  // if
    // Loop over all towers in this cluster to see if this tower is
    // physically adjacent to any of them.
    for (auto tower = towers.begin(); tower != towers.end(); ++tower) {
      if (mTower->isNeighbor(**tower) && !couldBePeakTower(mTower, *tower)) {
        return true;  // Stop looping once we find any match
      }  // if
    }  // for loop over all towers in a cluster
    return false;
  }
  /**
   Attempt to add a potential associate cluster with this tower.

   Add the cluster to the list of potential associates if this tower can
   associate with it (see canAssociate()).
   If there is already one or more clusters in the list:
   - if the new cluster is closer to the tower, replace the existing cluster(s).
   - if the new cluster if further away, do not add it.
   - if the new cluster is *exactly* the same separation as the existing
     cluster(s), add it to the list but keep the existing ones.

   i.e. at any time there can only be clusters of the same (minimal) separation
   from the tower, but there can be multiple clusters of identical separation.

   See separation(const StFmsTowerCluster*, const ETowerClusterDistance)
   for the meaning of the distance argument.

   Returns true if the new cluster is added, false if not.
   */
  void add(StFmsTowerCluster* cluster, const ETowerClusterDistance distance) {
    if (canAssociate(cluster)) {
      if (mClusters.empty()) {
        associate(cluster);
      } else {
        // Cluster(s) are already present, so only add the new one if it is
        // not further away. If it is closer, remove the existing cluster.
        double distNew = separation(cluster, distance);
        double distOld = separation(mClusters.front(), distance);
        if (distNew < distOld) {
          mClusters.clear();
        }  // if
        // Add the new cluster if it is not further away than existing ones
        if (!(distNew > distOld)) {
          associate(cluster);
        }  // if
      }  // if
    }  // if
  }
  /** Add a cluster to the list and set this tower's cluster index */
  void associate(StFmsTowerCluster* cluster) {
    mClusters.push_back(cluster);
    mTower->setCluster(cluster->index());
  }
  /**
   Calculate the nearest cluster out of the list of potential associates.

   The distance is that between this tower and the cluster centre, (x0, y0),
   therefore StFmsTowerCluster::calculateClusterMoments() must have been called
   before doing this, in order to calculate x0 and y0 of the cluster.

   Returns nullptr if there are no clusters in the list.
   */
  StFmsTowerCluster* nearestCluster() {
    StFmsTowerCluster* nearest = nullptr;
    double minDist = 99999.;
    for (auto i = mClusters.begin(); i != mClusters.end(); ++i) {
      double distance = separation(*i, kClusterCenter);
      // Check if the distance to the "center" of this cluster is smaller
      if (distance < minDist) {
        minDist = distance;
        nearest = *i;
      }  // if
    }  // for
    return nearest;
  }

 private:
  StFmsTower* mTower;  ///< Reference FMS tower  
  int mDetectorId; //! working detectorId
  std::list<StFmsTowerCluster*> mClusters;   ///< Associable clusters  
};

StFmsClusterFinder::StFmsClusterFinder(double energyCutoff)
    : mEnergyCutoff(energyCutoff), mNClusts(0), mDetectorId(0) { }

StFmsClusterFinder::~StFmsClusterFinder() { }

void StFmsClusterFinder::calculateClusterMoments(
    StFmsTowerCluster* cluster) const {
  cluster->calculateClusterMoments(mEnergyCutoff);
  cluster->cluster()->setNTowers(cluster->towers().size());
}

int StFmsClusterFinder::categorise(StFmsTowerCluster* towerCluster) {
  StFmsCluster* cluster = towerCluster->cluster();
  if (cluster->nTowers() < CAT_NTOWERS_PH1) {
    cluster->setCategory(k1PhotonCluster);
  } else {  // Categorise cluster based on empirical criteria
    const double sigmaMaxE = cluster->sigmaMax() * cluster->energy();
    if (cluster->energy() < CAT_EP1_PH2 * (sigmaMaxE - CAT_EP0_PH2)) {
      if (sigmaMaxE > CAT_SIGMAMAX_MIN_PH2) {
        cluster->setCategory(k2PhotonCluster);
      } else {
        cluster->setCategory(kAmbiguousCluster);
      }  // if
    } else if (cluster->energy() > CAT_EP1_PH1 * (sigmaMaxE - CAT_EP0_PH1)) {
      if (sigmaMaxE < CAT_SIGMAMAX_MAX_PH1) {
        cluster->setCategory(k1PhotonCluster);
      } else {
        cluster->setCategory(kAmbiguousCluster);
      }  // if
    } else {
      cluster->setCategory(kAmbiguousCluster);
    }  // if
  }  // if
  return cluster->category();
}

int StFmsClusterFinder::categorise2(StFmsTowerCluster* towerCluster) {
    StFmsCluster* cluster = towerCluster->cluster();
    if (cluster->nTowers() < CAT_NTOWERS_PH1) {
	cluster->setCategory(k1PhotonCluster);
    }else{  // Categorise cluster based on empirical criteria
	//int det=towerCluster->towers().front()->hit()->detectorId();//detectorId for top cell
	const double sigma=cluster->sigmaMax();
	const double e    =cluster->energy();
	if(sigma > 1/2.5 + 0.003*e + 7.0/e){
	    cluster->setCategory(k2PhotonCluster);
	}else if(sigma < 1/2.1 -0.001*e + 2.0/e){
	    cluster->setCategory(k1PhotonCluster);
	}else{
	    cluster->setCategory(kAmbiguousCluster);
	}
	//LOG_INFO << Form("Det=%2d e=%6.2f sigma=%6.3f cat=%1d",det,e,sigma,cluster->category()) <<endm;
    } 
    return cluster->category();
}

int StFmsClusterFinder::findClusters(TowerList* towers, ClusterList* clusters, int detectorId) {
  mDetectorId=detectorId; //current working detectorId
  // Remove towers below energy threshold, but save them for later use
  TowerList belowThreshold = filterTowersBelowEnergyThreshold(towers);
  TowerList neighbors;  // List of non-peak towers in clusters
  locateClusterSeeds(towers, &neighbors, clusters);
  // We have now found all seeds. Now decide the affiliation of neighbor towers
  // i.e. which peak each neighbor is associated with in a cluster.
  neighbors.sort(std::ptr_fun(&ascendingTowerEnergySorter));
  // Associated neighbor towers grow outward from the seed tower.
  // Keep trying to make tower-cluster associations until we make an entire loop
  // through all neighbors without successfully associating anything. Then stop,
  // otherwise we end up in an infinite loop when we can't associate all the
  // neighbors with a cluster (which we usually can't).
  TObjArray valleys(16);  // Stores towers equidistant between seeds
  valleys.SetOwner(true);
  unsigned nAssociations(0);
  do {
    nAssociations = associateTowersWithClusters(&neighbors, clusters, &valleys);
  } while (nAssociations > 0);
  // Calculate the moments of clusters. We need to do this before calling
  // TowerClusterAssociation::nearestCluster, which uses the cluster moment
  // to determine tower-cluster separations for the valley towers.
  for (auto i = clusters->begin(); i != clusters->end(); ++i) {
    calculateClusterMoments(i->get());
  }  // for
  // Ambiguous "valley" towers that were equally spaced between clusters can
  // now be associated
  associateValleyTowersWithClusters(&neighbors, clusters, &valleys);
  // If there are still towers left in "neighbor", distribute them to clusters
  do {
    nAssociations = associateResidualTowersWithClusters(&neighbors, clusters);
  } while (nAssociations > 0);
  /** \todo Check that this sort action is still needed */
  sortTowersEnergyDescending(clusters);
  // Recalculate various moment of clusters
  for (auto i = clusters->begin(); i != clusters->end(); ++i) {
    calculateClusterMoments(i->get());
  }  // for
  // Finally add "zero" energy towers to the clusters
  associateSubThresholdTowersWithClusters(&belowThreshold, clusters);
  return mNClusts;
}

unsigned StFmsClusterFinder::locateClusterSeeds(TowerList* towers,
                                                TowerList* neighbors,
                                                ClusterList* clusters) const {
  // The algorithm requires we sort towers in descending order or energy
  towers->sort(std::ptr_fun(&descendingTowerEnergySorter));
  while (!towers->empty() && clusters->size() < kMaxNClusters) {
    // By design, this tower is the highest tower remaining in towers, but it
    // could be lower than a tower in neighbors
    StFmsTower* high = towers->front();
    towers->pop_front();
    // Compare this highest tower with all towers in neighbors, and if it is
    // lower than any of those, make it a neighbor. Otherwise, it is a
    // peak (seed) tower so add it to a new cluster.
    if (couldBePeakTower(high, neighbors)) {
      // Add "high" to cluster and move towers neighboring "high" to "neighbor"
      high->setCluster(clusters->size());
      typedef FMSCluster::ClusterList::value_type ClusterPtr;
      clusters->push_back(ClusterPtr(new StFmsTowerCluster(new StFmsCluster, mDetectorId)));
      clusters->back()->setIndex(high->cluster());
      clusters->back()->towers().push_back(high);
      // Add neighbors of the new peak tower to the neighbor list.
      // Partition the remaining towers so that neighbours of the high tower are
      // placed at the beginning, and non-neighbours placed at the end. Use
      // stable_partition so we don't alter the energy ordering.
      auto neighborEnd =
        std::stable_partition(towers->begin(), towers->end(),
                              std::bind2nd(std::ptr_fun(&towerIsNeighbor),
                                           high));
      // Copy neighbors to the neighbor list, erase them from the tower list
      neighbors->insert(neighbors->end(), towers->begin(), neighborEnd);
      towers->erase(towers->begin(), neighborEnd);
    } else {  // Not a peak, add it to the neighbor collection
      neighbors->push_back(high);
    }  // when "high" is a "peak"
    // A tower separated from neighbors only by towers of the same energy will
    // become a peak by the above logic. To close this loophole, loop again
    // over towers and move any with energy <= any of its neighbors to the
    // neighbor list.
    auto towerIter = towers->begin();
    while (towerIter != towers->end()) {
      // Need to remove list items whilst iterating, so be careful to increment
      // the iterator before erasing items to avoid iterator invalidation
      if (!couldBePeakTower(*towerIter, neighbors)) {
        neighbors->push_back(*towerIter);
        towers->erase(towerIter++);  // Increment will evaluate before erase()
      } else {
        ++towerIter;
      }  // if
    }  // while
  }  // End of for loop over "arrTow"
  return clusters->size();
}

unsigned StFmsClusterFinder::associateTowersWithClusters(
    TowerList* neighbors,
    ClusterList* clusters,
    TObjArray* valleys) const {
  TowerList associated;  // Store neighbors we associate
  // Towers are sorted in ascending energy, so use reverse iterator to go from
  // highest to lowest energy
  TowerConstRIter tower;
  for (tower = neighbors->rbegin(); tower != neighbors->rend(); ++tower) {
    // Populate association information of this tower with each cluster
    std::unique_ptr<TowerClusterAssociation> association(new TowerClusterAssociation(*tower,mDetectorId));
    for (auto i = clusters->begin(); i != clusters->end(); ++i) {
      association->add(i->get(), kPeakTower);
    }  // for
    // Attempt to move the tower to the appropriate cluster
    if (association->clusters()->size() == 1) {
      // Only one peak is closest to the tower; the tower belongs to this peak
      association->clusters()->front()->towers().push_back(*tower);
      associated.push_back(*tower);
    } else if (association->clusters()->size() > 1) {
      // Multiple potential clusters, need to do something more sophisticated
      // Add this association to the "valley" array so we can use it later
      valleys->Add(association.release());
      associated.push_back(*tower);
    }  // if
  }  // loop over TObjArray "neighbor"
  // Remove associated neighbors from the neighbor list.
  for (auto i = associated.begin(); i != associated.end(); ++i) {
    neighbors->remove(*i);
  }  // for
  return associated.size();
}

unsigned StFmsClusterFinder::associateValleyTowersWithClusters(
    TowerList* neighbors,
    ClusterList* clusters,
    TObjArray* valleys) const {
  unsigned size = neighbors->size();
  for (Int_t i(0); i < valleys->GetEntriesFast(); ++i) {
    TowerClusterAssociation* association = static_cast<TowerClusterAssociation*>(valleys->At(i));    
    StFmsTowerCluster* cluster = association->nearestCluster();
    if (cluster) {
      // Move the tower to the appropriate cluster
      association->tower()->setCluster(cluster->index());
      cluster->towers().push_back(association->tower());
    } else {
      LOG_INFO << "Something is wrong! The following \"Valley\" tower does "
        << "not belong to any cluster! Error!" << endm;
      association->tower()->Print();
    }  // if (cluster)
  }  // end of for loop over valley towers
  return size - neighbors->size();
}

unsigned StFmsClusterFinder::associateResidualTowersWithClusters(
    TowerList* neighbors,
    ClusterList* clusters) const {
  TowerList associated;
  TowerConstRIter tower;
  for (tower = neighbors->rbegin(); tower != neighbors->rend(); ++tower) {
    // Populate tower-cluster association information
    TowerClusterAssociation association(*tower,mDetectorId);
    for (auto i = clusters->begin(); i != clusters->end(); ++i) {
      // There are already some towers in the cluster so we can use a computed
      // cluster center to give a better estimate of tower-cluster separation
      calculateClusterMoments(i->get());
      association.add(i->get(), kClusterCenter);
    }  // loop over all clusters
    if (!association.clusters()->empty()) {
      StFmsTowerCluster* cluster = association.clusters()->front();
      (*tower)->setCluster(cluster->index());
      cluster->towers().push_back(*tower);
      associated.push_back(*tower);
    }  // if
  }  // loop over TObjArray "neighbor"
  for (auto i = associated.begin(); i != associated.end(); ++i) {
    neighbors->remove(*i);
  }  // for
  return associated.size();
}

void StFmsClusterFinder::associateSubThresholdTowersWithClusters(
    TowerList* towers,
    ClusterList* clusters) const {
  for (auto tower = towers->begin(); tower != towers->end(); ++tower) {
      TowerClusterAssociation association(*tower,mDetectorId);
    // loop over all clusters
    for (auto i = clusters->begin(); i != clusters->end(); ++i) {
      association.add(i->get(), kPeakTower);
    }  // for
    StFmsTowerCluster* cluster = association.nearestCluster();
    if (cluster && association.separation(cluster, kClusterCenter) < 0.3) {
      (*tower)->setCluster(cluster->index());
      cluster->towers().push_back(*tower);
    }  // if
  }  // for
}
}  // namespace FMSCluster
