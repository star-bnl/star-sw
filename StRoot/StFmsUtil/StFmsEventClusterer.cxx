// $Id: StFmsEventClusterer.cxx,v 1.4 2015/10/01 18:19:59 akio Exp $
//
// $Log: StFmsEventClusterer.cxx,v $
// Revision 1.4  2015/10/01 18:19:59  akio
// fixed typo
//
// Revision 1.3  2015/10/01 18:08:58  akio
// adding warning if removing cluster with too many towers
//
// Revision 1.2  2015/09/02 15:01:32  akio
// Removing StFmsGeometry class, and now it uses StFmsDbMaker to get appropriate parameters.
//
// Revision 1.1  2015/03/10 14:38:54  jeromel
// First version of FmsUtil from Yuxi Pan - reviewd 2015/02
//
/**
 \file      StFmsEventClusterer.cxx
 \brief     Implementation of StFmsEventClusterer, manager class for clustering
 \author    Steven Heppelmann <steveheppelmann@gmail.com>
 \author    Yuxi Pan <yuxipan@physics.ucla.edu>
 \author    Thomas Burton <tpb@bnl.gov>
 \date      2014
 \copyright Brookhaven National Lab
 */
#include "StFmsEventClusterer.h"
#include "StRoot/StFmsDbMaker/StFmsDbMaker.h"

#include <algorithm>
#include <array>
#include <functional>
#include <iterator>
#include <list>
#include <numeric>

#include "TF2.h"  // To use shower-shape function
#include "TMath.h"
#include "TRandom.h"  // For ROOT global random generator, gRandom

#include "StRoot/St_base/StMessMgr.h"
#include "StRoot/StEvent/StFmsCluster.h"
#include "StRoot/StEvent/StFmsHit.h"

#include "StFmsClusterFitter.h"
#include "StFmsFittedPhoton.h"
//#include "StFmsGeometry.h"
#include "StFmsTower.h"
#include "StFmsTowerCluster.h"
#include "StFmsConstant.h"

namespace {
namespace fms = FMSCluster;
// We use the tower list defined in StFmsTowerCluster throughout this file.
// Define some typedefs for convenience.
typedef fms::StFmsTowerCluster::Towers Towers;
typedef fms::ClusterList::iterator ClusterIter;

/* Helper function to add numbers of photons using std::accumulate */
int accumulatePhotons(int nPhotons,
                      const fms::ClusterList::value_type& cluster) {
  return nPhotons + cluster->photons().size();
}

/* Sum the total number of photons in a list of clusters */
template<class Iterator>
int sumPhotonsOverClusters(Iterator start, Iterator end) {
  return std::accumulate(start, end, 0, accumulatePhotons);
}

/* Sum the total number of photons in a list of clusters */
template<class Container>
int sumPhotonsOverClusters(const Container& clusters) {
  return sumPhotonsOverClusters(clusters.begin(), clusters.end());
}

/* Unary predicate for selecting bad clusters. */
struct IsBadCluster {
  // Set minimum allowed cluster energy and maximum number of towers
  IsBadCluster(double minEnergy, unsigned maxTowers)
      : energy(minEnergy), towers(maxTowers) { }
  bool operator()(const fms::ClusterList::value_type& cluster) const {
      if(cluster->cluster()->energy() <= energy){
	  LOG_DEBUG << Form("Removing cluster because E=%f <= %f (NTower=%d)",cluster->cluster()->energy(),energy,cluster->towers().size()) << endm;
      }
      if(cluster->towers().size() > towers){
	  LOG_INFO << Form("Removing cluster because NTower=%d > %d (E=%f)",cluster->towers().size(),towers,cluster->cluster()->energy()) << endm;
      }      
      return cluster->cluster()->energy() <= energy ||
	     cluster->towers().size() > towers;
  }
  double energy;
  unsigned towers;
};

/*
 Returns a pointer to the lowest energy photon in a cluster
 
 Assumes the cluster is either 1- or 2-photon
 Returns nullptr if there is no photon in the cluster
 */
const fms::StFmsFittedPhoton* findLowestEnergyPhoton(
    const fms::StFmsTowerCluster* cluster) {
  const fms::StFmsFittedPhoton* photon = nullptr;
  const auto& photons = cluster->photons();
  switch (photons.size()) {
    case 1:
      photon = &(photons.front());
      break;
    case 2:
      if (photons.front().energy < photons.back().energy) {
        photon = &(photons.front());
      } else {
        photon = &(photons.back());
      }  // if
    default:
      break;  // photon remains nullptr
  }  // switch
  return photon;
}

/* Functor returning true if a tower matches (row, column) */
struct HasRowColumn {
  int row, column;
  HasRowColumn(int r, int c) : row(r), column(c) { }
  bool operator()(const fms::StFmsTower* tower) const {
    return tower->row() == row && tower->column() == column;
  }
};

/*
 Search towers in a cluster for one matching a row and column number

 Return a pointer to the matching tower if one is found, nullptr otherwise.
 */
const fms::StFmsTower* searchClusterTowers(
    int row, int column, const fms::StFmsTowerCluster& cluster) {
  auto found = std::find_if(cluster.towers().begin(), cluster.towers().end(),
                            HasRowColumn(row, column));
  if (found != cluster.towers().end()) {
    return *found;
  }  // if
  return nullptr;
}

/*
 Gives fit parameter start points and limits for 1-photon fit.

 See SFmsClusterFitter::fitNPhoton() for parameter meanings
 */
struct OnePhotonFitParameters {
  std::vector<double> start, lower, upper;
  OnePhotonFitParameters(const std::vector<double>& xyWidth,
                         const StFmsCluster* cluster) {
    const double x = xyWidth.at(0);
    const double y = xyWidth.at(1);
    start = {
      PH1_START_NPH,
      x * cluster->x(),
      y * cluster->y(),
      cluster->energy()
    };
    const std::vector<double> delta = {
      PH1_DELTA_N,
      x * PH1_DELTA_X,
      y * PH1_DELTA_Y,
      cluster->energy() * PH1_DELTA_E
    };
    for (unsigned i(0); i < start.size(); ++i) {
      lower.push_back(start.at(i) - delta.at(i));
      upper.push_back(start.at(i) + delta.at(i));
    }  // for
  }
};

/*
 Gives fit parameter start points and limits for 2-photon fit.

 See SFmsClusterFitter::fit2Photon() for parameter meanings
 */
struct TwoPhotonFitParameters {
  std::array<double, 7> start, steps, lower, upper;
  TwoPhotonFitParameters(const std::vector<double>& xyWidth,
                         const fms::StFmsTowerCluster* towerCluster) {
    const double x = xyWidth.at(0);
    const double y = xyWidth.at(1);
    const auto cluster = towerCluster->cluster();
    start = std::array<double, 7>{ {
      PH2_START_NPH,
      x * cluster->x(),
      y * cluster->y(),
      PH2_START_FSIGMAMAX * x * cluster->sigmaMax(),
      towerCluster->thetaAxis(),
      gRandom->Uniform(PH2_RAN_LOW, PH2_RAN_HIGH),
      cluster->energy(),
    } };
    steps = std::array<double, 7>{ {PH2_STEP_0, PH2_STEP_1, PH2_STEP_2, PH2_STEP_3, PH2_STEP_4, PH2_STEP_5, PH2_STEP_6} };
    const double sigmaMaxE = cluster->sigmaMax() * cluster->energy();
    double maxTheta = cluster->sigmaMin() / cluster->sigmaMax() / PH2_MAXTHETA_F;
    maxTheta = std::min(maxTheta, TMath::PiOver2());
    lower = std::array<double, 7>{ {
      PH2_LOWER_NPH,
      start.at(1) - PH2_LOWER_XF * x,
      start.at(2) - PH2_LOWER_YF * y,
      std::max(PH2_LOWER_XMAX_F / pow(sigmaMaxE, PH2_LOWER_XMAX_POW), PH2_LOWER_XMAX_LIMIT) * x,
      start.at(4) - maxTheta,
      PH2_LOWER_5_F,
      start.at(6) * PH2_LOWER_6_F
    } };
    upper = std::array<double, 7>{ {
      PH2_UPPER_NPH,
      start.at(1) + PH2_UPPER_XF * x,
      start.at(2) + PH2_UPPER_YF * y,
      std::min(PH2_UPPER_XMIN_F * (PH2_UPPER_XMIN_P0 - sigmaMaxE), PH2_UPPER_XMIN_LIMIT) * x,
      start.at(4) + maxTheta,
      PH2_UPPER_5_F,
      start.at(6) * PH2_UPPER_6_F
    } };
    // With the above approach the limits on parameter 3 can sometimes go beyond
    // sensible values, so limit them.
    lower.at(3) = std::min(lower.at(3), start.at(3) * PH2_3_LIMIT_LOWER);
    upper.at(3) = std::max(upper.at(3), start.at(3) * PH2_3_LIMIT_UPPER);
  }
};

/* Gives fit parameters for global photon fit */
struct GlobalPhotonFitParameters {
  std::vector<double> start, lower, upper;
  GlobalPhotonFitParameters(unsigned nPhotons,
                            ClusterIter first, ClusterIter end)
    // Initialise N-photons parameters as the first element
    : start(1, nPhotons), lower(1, GL_LOWER_1),
      upper(1, fms::StFmsClusterFitter::maxNFittedPhotons() + GL_UPPER_DELTA_MAXN) {
    // Append (x, y, E) fit parameters for each photon
    for (auto cluster = first; cluster != end; ++cluster) {
      const auto& photons = (*cluster)->photons();
      for (auto p = photons.begin(); p != photons.end(); ++p) {
        start.push_back(p->x);
        lower.push_back(start.back() - GL_0_DLOWER);
        upper.push_back(start.back() + GL_0_DUPPER);
        start.push_back(p->y);
        lower.push_back(start.back() - GL_1_DLOWER);
        upper.push_back(start.back() + GL_1_DUPPER);
        start.push_back(p->energy);
        lower.push_back(start.back() * (1 - GL_2_DLOWER));  // Limit to +/- 30% energy
        upper.push_back(start.back() * (1 + GL_2_DUPPER));
      }  // for
    }  // for
  }
};
}  // unnamed namespace

namespace FMSCluster {
  StFmsEventClusterer::StFmsEventClusterer( //const StFmsGeometry* geometry,
					   StFmsDbMaker* db, Int_t detectorId)
    : mClusterFinder(0.5), /*mGeometry(geometry),*/ mDetectorId(detectorId), mFmsDbMaker(db) { }

StFmsEventClusterer::~StFmsEventClusterer() {}

Bool_t StFmsEventClusterer::cluster(std::vector<StFmsTower>* towerList) {
  mTowers = towerList;
  //mTowerWidthXY = mGeometry->towerWidths(mDetectorId);
  if(!mFmsDbMaker){
    LOG_ERROR << "StFmsEventCusterer cannot find StFmsDbMaker !!!!!!"<<endm;
    return false;
  }
  Float_t xw = mFmsDbMaker->getXWidth(mDetectorId);
  Float_t yw = mFmsDbMaker->getYWidth(mDetectorId);    
  mTowerWidthXY.clear();
  mTowerWidthXY.push_back(xw);
  mTowerWidthXY.push_back(yw);
  /** \todo Test of number of towers should be detector-dependent */
  if (mTowers->size() > TOTAL_TOWERS) {
    LOG_ERROR << "Too many towers for Fit" << endm;
    return false;
  }  // if
  mFitter.reset(new StFmsClusterFitter(/*mGeometry,*/ mDetectorId,xw,yw));
  return fitEvent();  // Return true for success
}

Int_t StFmsEventClusterer::fitEvent() {
  if (findClusters()) {
    return fitClusters() && refitClusters();
  }  // if
  return false;
}

Int_t StFmsEventClusterer::findClusters() {
  StFmsClusterFinder::TowerList towerList;
  for (auto i = mTowers->begin(); i != mTowers->end(); ++i) {
    towerList.push_back(&(*i));
  }  // for
  mClusterFinder.findClusters(&towerList, &mClusters);
  //  switch (mDetectorId) {
  switch(mFmsDbMaker->largeSmall(mDetectorId)){
    //case kFmsNorthLarge:  // Deliberate fall-through
    //case kFmsSouthLarge:
  case 0:
    mClusters.remove_if(IsBadCluster(BAD_MIN_E_LRG, BAD_MAX_TOW_LRG));
    break;
    //case kFmsNorthSmall:  // Deliberate fall-through
    //case kFmsSouthSmall:
  case 1:
    mClusters.remove_if(IsBadCluster(BAD_MIN_E_SML, BAD_MAX_TOW_SML));
    break;
  default:
    break;
  }  // switch
  // Must do moment analysis before catagorization
  for (auto i = mClusters.begin(); i != mClusters.end(); ++i) {
    (*i)->findClusterAxis(mClusterFinder.momentEnergyCutoff());
  }  // for
  return mClusters.size();
}

Bool_t StFmsEventClusterer::fitClusters() {
  // Loop over clusters, catagorize, guess the photon locations for cat 0 or 2
  // clusters then fit, compare, and choose the best fit
  bool badFit = false;
  for (auto iter = mClusters.begin(); iter != mClusters.end(); ++iter) {
    int category = mClusterFinder.categorise(iter->get());
    mFitter->setTowers(&(*iter)->towers());
    switch (category) {
      case k1PhotonCluster:
        fit1PhotonCluster(iter->get());
        break;
      case k2PhotonCluster:
        fit2PhotonCluster(iter);
        break;
      case kAmbiguousCluster:
        category = fitAmbiguousCluster(iter);
        break;
      default:
        LOG_ERROR << "The logic of cluster catagory is wrong and something "
          << "impossible has happened! This a catagory-" << category <<
          " cluster! Do not know how to fit it!" << endm;
        break;
    }  // switch
    if (category == k2PhotonCluster && (*iter)->chiSquare() > BAD_2PH_CHI2) {
      badFit = true;
    }  // if
  }  // Loop over all real clusters
  return !badFit;
}

Bool_t StFmsEventClusterer::refitClusters() {
  // Only do a global fit for 2 or more clusters (2-photon fit for one cluster
  // already performs a global fit as part of its normal procedure)
  if (mClusters.size() < 2) {
    return true;
  }  // if
  Towers towers;
  for (auto i = mClusters.begin(); i != mClusters.end(); ++i) {
    towers.insert(towers.end(), (*i)->towers().begin(), (*i)->towers().end());
  }  // for
  mFitter->setTowers(&towers);
  const int nPhotons = sumPhotonsOverClusters(mClusters);
  fitGlobalClusters(nPhotons, mClusters.size(), mClusters.begin());
  return nPhotons == sumPhotonsOverClusters(mClusters);  // Shouldn't change
}

Double_t StFmsEventClusterer::photonEnergyInCluster(
    const StFmsTowerCluster* cluster,
    const StFmsFittedPhoton* photon) const {
  double energy = 0.;
  const Towers& towers = cluster->towers();
  for (auto tower = towers.begin(); tower != towers.end(); ++tower) {
    energy += photonEnergyInTower(*tower, photon);
  }  // for
  return energy;
}

Double_t StFmsEventClusterer::photonEnergyInTower(
    const StFmsTower* tower,
    const StFmsFittedPhoton* photon) const {
  double x = (tower->column() - 0.5) * mTowerWidthXY.at(0) - photon->x;
  double y = (tower->row() - 0.5) * mTowerWidthXY.at(1) - photon->y;
  return photon->energy * mFitter->showerShapeFunction()->Eval(x, y);
}

/* 1-photon fitting function */
Double_t StFmsEventClusterer::fit1PhotonCluster(
    StFmsTowerCluster* towerCluster) {
  OnePhotonFitParameters parameters(mTowerWidthXY, towerCluster->cluster());
  PhotonList photons;
  double chiSquare = mFitter->fitNPhoton(parameters.start, parameters.lower,
                                         parameters.upper, &photons);
  if (photons.empty()) {  // check return status in case of a bad fit
    LOG_ERROR << "1-photon Minuit fit found no photons" << endm;
  } else {
    towerCluster->photons().assign(photons.begin(), photons.end());
  }  // if
  const int nDegreesOfFreedom =
    std::max(int(towerCluster->towers().size()) - 3, 1);
  towerCluster->setChiSquare(chiSquare / nDegreesOfFreedom);
  return towerCluster->chiSquare();
}

/* 2-photon fitting function */
Double_t StFmsEventClusterer::fit2PhotonCluster(ClusterIter towerCluster) {
  TwoPhotonFitParameters parameters(mTowerWidthXY, towerCluster->get());
  PhotonList photons;
  double chiSquare =
    mFitter->fit2Photon(parameters.start, parameters.steps,
                        parameters.lower, parameters.upper, &photons);
  if (photons.size() == 2) {
    (*towerCluster)->photons().assign(photons.begin(), photons.end());
  } else {
    LOG_WARN << "2-photon Minuit fit found " << photons.size() << " photons"
      << endm;
  }  // if
  chiSquare = fitGlobalClusters(2, 1, towerCluster);
  const int nDegreesOfFreedom = std::max(1,
    int((*towerCluster)->towers().size() - 6));
  (*towerCluster)->setChiSquare(chiSquare / nDegreesOfFreedom);
  return (*towerCluster)->chiSquare();
}

/* Distinguish an ambiguous cluster as either 1- or 2-photon */
Int_t StFmsEventClusterer::fitAmbiguousCluster(ClusterIter towerCluster) {
  const double chiSquare1Photon = fit1PhotonCluster(towerCluster->get());
  const StFmsFittedPhoton photon = (*towerCluster)->photons().front();  // Cache
  // Decide if this 1-photon fit is good enough, if not try 2-photon fit
  int category = k1PhotonCluster;
  if (chiSquare1Photon >= 5.) {
    if (fit2PhotonCluster(towerCluster) <= chiSquare1Photon &&
        validate2ndPhoton(towerCluster)) {
      category = k2PhotonCluster;
    }  // if
  }  // if
  // The 2-photon fit updated the cluster, so if the 1-photon fit turns out to
  // have been better, restore its properties
  if (category == k1PhotonCluster) {
    (*towerCluster)->setChiSquare(chiSquare1Photon);
    (*towerCluster)->photons().assign(1, photon);
  }  // if
  return category;
}

/* Global fitting function, fitting photons across all clusters */
Double_t StFmsEventClusterer::fitGlobalClusters(unsigned int nPhotons,
                                               const unsigned int nClusters,
                                               ClusterIter first) {
  ClusterIter end = first;
  std::advance(end, nClusters);  // Marks end point for cluster iteration
  const unsigned int totalPhotons = sumPhotonsOverClusters(first, end);
  if (totalPhotons != nPhotons) {
    LOG_WARN << "Global fit called for " << nPhotons << " but found " <<
      totalPhotons << "... will proceed with " << totalPhotons << endm;
    nPhotons = totalPhotons;
  }  // if
  if (int(nPhotons) > StFmsClusterFitter::maxNFittedPhotons() || nPhotons < 2) {
    LOG_ERROR << "Global fit cannot fit " << nPhotons << " photons" << endm;
    return -9999;
  }  // if
  GlobalPhotonFitParameters parameters(nPhotons, first, end);
  PhotonList photons;
  Double_t chiSquare = mFitter->fitNPhoton(parameters.start, parameters.lower,
                                           parameters.upper, &photons);
  if (photons.size() == nPhotons) {
    // Put the fit result back in the clusters
    auto photon = photons.begin();
    for (ClusterIter cluster = first; cluster != end; ++cluster) {
      auto& ph = (*cluster)->photons();
      for (auto p = ph.begin(); p != ph.end(); ++p, ++photon) {
        *p = *photon;
      }  // for
    }  // for loop over clusters
  } else {
    LOG_WARN << "Global Minuit fit found " << photons.size() <<
      " photons but expected " << nPhotons << endm;
  }  // if
  return chiSquare;
}

/*
 Further information:
 If one photon peak lies on top of a low (compared to photon energy) or even
 zero tower, this photon is definitely bogus. This could happen if a nearby
 cluster took away towers that might make the fit have a large Chi-square.
 So first check that the fitted photon of the lower-energy photon (we assume the
 higher energy photon should be fine) is over one of the non-zero towers of the
 cluster. First of all, this ensures that we don't have an "outside of cluster"
 bogus photon, i.e. a bogus photon that could be the result of minimizing the
 chi-square over towers that do not include the supposed peak tower. 
*/
bool StFmsEventClusterer::validate2ndPhoton(ClusterConstIter cluster) const {
  // Find the tower hit by the lowest energy photon in a cluster
  const StFmsFittedPhoton* photon = findLowestEnergyPhoton(cluster->get());
  int column = 1 + int(photon->x / mTowerWidthXY.at(0));
  int row = 1 + int(photon->y / mTowerWidthXY.at(1));
  const StFmsTower* tower = searchClusterTowers(row, column, **cluster);
  // If tower is nullptr, the photon doesn't hit in a tower in this cluster.
  if (!tower) {
    return false;
  }  // if
  // Check if the fitted energy is too large compared to the energy of the tower
  if (tower->hit()->energy() < VALID_FT * photon->energy) {
    return false;
  }  // if
  // Check if the 2nd photon's "high-hower" energy is too large compared to its
  // fitted energy. If so, it is probably splitting one photon into two
  if (tower->hit()->energy() > VALID_2ND_FT * photonEnergyInTower(tower, photon)) {
    return false;
  }  // if
  // Check that the 2nd photon is not near the edge of another cluster
  const double energyInOwnCluster =
    photonEnergyInCluster(cluster->get(), photon);
  for (ClusterConstIter i = mClusters.begin(); i != mClusters.end(); ++i) {
    if (i != cluster) {  // Skip the photon's own cluster
      if (photonEnergyInCluster(i->get(), photon) > VALID_E_OWN * energyInOwnCluster) {
        return false;  // Stop as soon as we fail for one cluster
      }  // if
    }  // if
  }  // for
  return true;  // The photon passed all tests; it's real
}
}  // namespace FMSCluster
