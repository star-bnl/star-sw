// $Id: StFmsEventClusterer.cxx,v 1.16 2019/06/26 16:49:53 akio Exp $
//
// $Log: StFmsEventClusterer.cxx,v $
// Revision 1.16  2019/06/26 16:49:53  akio
// shower shape scaling for all shapes
//
// Revision 1.15  2018/03/23 18:43:01  smirnovd
// Turn off excessive output from StFmsEventClusterer
//
// Revision 1.14  2018/03/02 20:27:29  akio
// Big update from	Zhanwen Zhu with new shower shape and six z slices
//
// Revision 1.13  2018/01/04 17:35:45  smirnovd
// [Cosmetic] Remove StRoot/ from include path
//
// $STAR/StRoot is already in the default path search
//
// Revision 1.12  2016/06/07 15:51:44  akio
// Making code better based on Coverity reports
//
// Revision 1.11  2016/01/26 14:42:48  akio
// better chi2 handling
//
// Revision 1.10  2015/12/11 18:05:08  akio
// move some LOG_INFO to LOG_DEBUG
//
// Revision 1.9  2015/11/05 17:54:57  akio
// Adding option to scale up shower shape function for large cells
//
// Revision 1.8  2015/11/02 22:44:49  akio
// Fix photonEnergyInTower()
//
// Revision 1.7  2015/10/30 21:33:56  akio
// fix parameter initialization
// adding new cluster categorization method
//
// Revision 1.6  2015/10/29 21:14:55  akio
// increase max number of clusters
// a bug fixes in valley tower association
// removing some debug comments
//
// Revision 1.5  2015/10/21 15:58:04  akio
// Code speed up (~x2) by optimizing minimization fuctions and showershape function
// Add option to merge small cells to large, so that it finds cluster at border
// Add option to perform 1photon fit when 2photon fit faield
// Add option to turn on/off global refit
// Moment analysis done without ECUTOFF when no tower in cluster exceed ECUTOFF=0.5GeV
//
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
#include "StFmsDbMaker/StFmsDbMaker.h"

#include <algorithm>
#include <array>
#include <functional>
#include <iterator>
#include <list>
#include <numeric>

#include "TF2.h"  // To use shower-shape function
#include "TMath.h"
#include "TRandom.h"  // For ROOT global random generator, gRandom

#include "St_base/StMessMgr.h"
#include "StEvent/StFmsCluster.h"
#include "StEvent/StFmsHit.h"

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
    int det, row, column;
    HasRowColumn(int d, int r, int c) : det(d), row(r), column(c) { }
    bool operator()(const fms::StFmsTower* tower) const {
	return tower->hit()->detectorId() == det && tower->row() == row && tower->column() == column ;
    }
};

/*
 Search towers in a cluster for one matching a row and column number

 Return a pointer to the matching tower if one is found, nullptr otherwise.
 */
const fms::StFmsTower* searchClusterTowers(int det,int row, int column, const fms::StFmsTowerCluster& cluster) {
  auto found = std::find_if(cluster.towers().begin(), cluster.towers().end(),
                            HasRowColumn(det, row, column));
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
  std::vector<double> start, steps, lower, upper;
  OnePhotonFitParameters(const std::vector<double>& xyWidth,
                         const StFmsCluster* cluster, const double w) {
    const double x = xyWidth.at(0); //width of cell coordinate, while w is width of top energy cell
    const double y = xyWidth.at(1);
    start = {
      PH1_START_NPH,
      x * cluster->x(),
      y * cluster->y(),
      cluster->energy()
    };
    steps = {0.0, 0.1*(x+y)/2.0/w, 0.1*(x+y)/2.0/w, 0.2};
    const std::vector<double> delta = {
      PH1_DELTA_N,
      w * PH1_DELTA_X,
      w * PH1_DELTA_Y,
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
                         const fms::StFmsTowerCluster* towerCluster, const int largesmall , double vertexZ) {
    const double x = xyWidth.at(0); //width of cell coordinate, while largesmall indicate top cells l/s
    const double y = xyWidth.at(1);
    const auto cluster = towerCluster->cluster();

    //When small merge to large , x y above from xyWidth are large cell width for coodinate calc
    //Overwrite w1 and w2 for step/limit calc
    double w1=0, w2=0;
    if (largesmall==0)  {w1=x; w2=y;}// could be no merging or large cell cluster when merging, anyway ,nothing need be done
    else                {w1=3.822; w2=3.875;} 
    LOG_DEBUG<<"x in FitP "<< x <<"    w1="<<w1<<endm;

    start = std::array<double, 7>{ {
      PH2_START_NPH,
      x * cluster->x(),// both terms are scaled, no need to use w1/w2 
      y * cluster->y(),
      //PH2_START_FSIGMAMAX * x * cluster->sigmaMax(),      // original
      PH2_START_FSIGMAMAX * x * cluster->sigmaMax() / 2.0,  // above is too big, scale down by 1/2
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
      start.at(1) - PH2_LOWER_XF * w1 ,
      start.at(2) - PH2_LOWER_YF * w2,
      //use vertex for lower limit!!!
      //if starting point is close to the thoretical limit, still allowed to go down -20% by limit below
      std::max(0.1345*2/cluster->energy()*(735.45-vertexZ) , 0.5*w1 ), 
      //theoretical lower limit assuming vertex=0,
      //std::max(0.1345*2/cluster->energy()*735.45 , 0.5*w1 ),
      //This is original limit * 0.5
      //    std::max(PH2_LOWER_XMAX_F / pow(sigmaMaxE, PH2_LOWER_XMAX_POW), PH2_LOWER_XMAX_LIMIT) * x / 2,
      //This is original limit which is too high
      //    std::max(PH2_LOWER_XMAX_F / pow(sigmaMaxE, PH2_LOWER_XMAX_POW), PH2_LOWER_XMAX_LIMIT) * x,
      start.at(4) - maxTheta,
      PH2_LOWER_5_F,
      start.at(6) * PH2_LOWER_6_F
    } };
    upper = std::array<double, 7>{ {
      PH2_UPPER_NPH,
      start.at(1) + PH2_UPPER_XF * w1,
      start.at(2) + PH2_UPPER_YF * w2,
      // original * 5, still it will be overwroitten by limit at few lines below if it goes negative
      std::min(PH2_UPPER_XMIN_F * (PH2_UPPER_XMIN_P0 - sigmaMaxE), PH2_UPPER_XMIN_LIMIT) * w1 * 5,
      //original, wrong at high energy and goes negative
      //std::min(PH2_UPPER_XMIN_F * (PH2_UPPER_XMIN_P0 - sigmaMaxE), PH2_UPPER_XMIN_LIMIT) * w1 * 5,
      start.at(4) + maxTheta,
      PH2_UPPER_5_F,
      start.at(6) * PH2_UPPER_6_F
    } };
    // With the above approach the limits on parameter 3 can sometimes go beyond
    // sensible values, so limit them.
    lower.at(3) = std::min(lower.at(3), start.at(3) * 0.8);
    upper.at(3) = std::max(upper.at(3), start.at(3) * 3.0);
    //original is too tight
    //    lower.at(3) = std::min(lower.at(3), start.at(3) * PH2_3_LIMIT_LOWER);
    //    upper.at(3) = std::max(upper.at(3), start.at(3) * PH2_3_LIMIT_UPPER);
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
					   StFmsDbMaker* db, Int_t detectorId, 
					   Int_t globalrefit, Int_t mergeSmallToLarge, 
					   Int_t try1PhotonFit, Int_t categorizationAlgo,
					   Float_t scaleShowerShapeLarge , Float_t scaleShowerShapeSmall,
					   Int_t showerShapeWithAngle ,double vertexZ)
      : mClusterFinder(0.5), /*mGeometry(geometry),*/ mDetectorId(detectorId), mTowers(0), 
	mFmsDbMaker(db), mGlobalRefit(globalrefit), mMergeSmallToLarge(mergeSmallToLarge), 
	mTry1PhotonFitWhen2PhotonFitFailed(try1PhotonFit), mCategorizationAlgo(categorizationAlgo),
        mScaleShowerShapeLarge(scaleShowerShapeLarge), mScaleShowerShapeSmall(scaleShowerShapeSmall),
        mShowerShapeWithAngle(showerShapeWithAngle), vertexz(vertexZ) { }
    
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
  mFitter.reset(new StFmsClusterFitter(/*mGeometry,*/ mDetectorId,xw,yw,
				       mScaleShowerShapeLarge,mScaleShowerShapeSmall,
				       mShowerShapeWithAngle,mMergeSmallToLarge,vertexz) );
  return fitEvent();  // Return true for success
}

Int_t StFmsEventClusterer::fitEvent() {
    if(findClusters()) {
	if(fitClusters()){
	    if(mGlobalRefit==0) return true;
	    if(refitClusters()) return true;
	    LOG_INFO << "StFmsEventClusterer::fitEvent() refitClusters failed" <<endm;
	    return false;
	}
	LOG_INFO << "StFmsEventClusterer::fitEvent() fitClusters failed" <<endm;
	return false;
    }
    //LOG_INFO << "StFmsEventClusterer::fitEvent() findClusters failed" <<endm;
    return false;
}

Int_t StFmsEventClusterer::findClusters() {
  StFmsClusterFinder::TowerList towerList;
  for (auto i = mTowers->begin(); i != mTowers->end(); ++i) {
    towerList.push_back(&(*i));
  }  // for
  mClusterFinder.findClusters(&towerList, &mClusters, mDetectorId);
  switch(mFmsDbMaker->largeSmall(mDetectorId)){
  case 0:
    mClusters.remove_if(IsBadCluster(BAD_MIN_E_LRG, BAD_MAX_TOW_LRG));
    break;
  case 1:
    mClusters.remove_if(IsBadCluster(BAD_MIN_E_SML, BAD_MAX_TOW_SML));
    break;
  default:
    break;
  }  // switch
  // Must do moment analysis before catagorization
  for (auto i = mClusters.begin(); i != mClusters.end(); ++i) {
      (*i)->findClusterAxis(mClusterFinder.momentEnergyCutoff(),mTowerWidthXY.at(0),mTowerWidthXY.at(1));
  }  // for
  return mClusters.size();
}

Bool_t StFmsEventClusterer::fitClusters() {
  // Loop over clusters, catagorize, guess the photon locations for cat 0 or 2
  // clusters then fit, compare, and choose the best fit
  bool badFit = false;
    int Clucount=0;
  for (auto iter = mClusters.begin(); iter != mClusters.end(); ++iter) {
    int category = -1;
    ++Clucount;

    if(mCategorizationAlgo==0) {category = mClusterFinder.categorise(iter->get());}
    else                       {category = mClusterFinder.categorise2(iter->get());}
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
	float chi2=(*iter)->chiSquare();
	LOG_DEBUG << Form("chi2=%f >  BAD_2PH_CHI2=%f",chi2,BAD_2PH_CHI2) << endm;
	if(mTry1PhotonFitWhen2PhotonFitFailed){
	    const std::vector<StFmsFittedPhoton> keep = (*iter)->photons(); //copy in case 2-photon fit is better
	    fit1PhotonCluster(iter->get()); //try 1-photon fit
	    float chi1=(*iter)->chiSquare();
	    LOG_DEBUG << Form("Tried 1-photon fit resulted with chi2=%f", chi1)<< endm;	
	    if(chi2<chi1){
		LOG_DEBUG << "2-photon fit was better" << endm;
		(*iter)->photons().assign(keep.begin(), keep.end());
		(*iter)->setChiSquare(chi2);
		//badFit = true;
	    }else{
		LOG_DEBUG << "Taking 1-photon fit" << endm;
	    }
	}
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
    //double x = (tower->column() - 0.5) * mTowerWidthXY.at(0) - photon->x;
    //double y = (tower->row() - 0.5) * mTowerWidthXY.at(1) - photon->y;
    //return photon->energy * mFitter->showerShapeFunction()->Eval(x, y);

 if (mShowerShapeWithAngle>0)   return photon->energy * mFitter->showerShapeFunction()->Eval(tower->x(),photon->x,tower->y(),photon->y);
 if (mShowerShapeWithAngle==0)  return photon->energy * mFitter->showerShapeFunction()->Eval(tower->x()-photon->x,tower->y()-photon->y);
}

/* 1-photon fitting function */
Double_t StFmsEventClusterer::fit1PhotonCluster(StFmsTowerCluster* towerCluster) {
  double w=towerCluster->towers().front()->w();   //get tower width from 1st/top tower
  OnePhotonFitParameters parameters(mTowerWidthXY, towerCluster->cluster(), w);
  PhotonList photons;
  double chiSquare = mFitter->fitNPhoton(parameters.start, parameters.steps,
					 parameters.lower,parameters.upper, &photons);
  if (photons.empty()) {  // check return status in case of a bad fit
    LOG_ERROR << "1-photon Minuit fit found no photons" << endm;
  } else {
    towerCluster->photons().assign(photons.begin(), photons.end());
  }  // if
  const int nDegreesOfFreedom =
    std::max(int(towerCluster->towers().size()) - 3, 1);
  towerCluster->setChiSquare(chiSquare / nDegreesOfFreedom);
  towerCluster->setChiSquare1(chiSquare / nDegreesOfFreedom);
  return towerCluster->chiSquare();
}

/* 2-photon fitting function */
Double_t StFmsEventClusterer::fit2PhotonCluster(ClusterIter towerCluster) {
  double x=towerCluster->get()->towers().front()->x();    
  double y=towerCluster->get()->towers().front()->y();   //get top energy tower position  
  double w=0;//indicator
  if(mMergeSmallToLarge>0 && x<8.0 && y>9.0 && y<25.0){
      w=1;
  }
  TwoPhotonFitParameters parameters(mTowerWidthXY, towerCluster->get(), w,vertexz);
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
  (*towerCluster)->setChiSquare2(chiSquare / nDegreesOfFreedom);
  return (*towerCluster)->chiSquare();
}

/* Distinguish an ambiguous cluster as either 1- or 2-photon */
Int_t StFmsEventClusterer::fitAmbiguousCluster(ClusterIter towerCluster) {
  const double chiSquare1Photon = fit1PhotonCluster(towerCluster->get());
  const StFmsFittedPhoton photon = (*towerCluster)->photons().front();  // Cache
  // Decide if this 1-photon fit is good enough, if not try 2-photon fit
  int category = k1PhotonCluster;
  LOG_DEBUG << "fitAmbiguousCluster chi2 for 1photon fit="<<chiSquare1Photon<<endm;
  if (chiSquare1Photon >= 5.) {
      const double chiSquare2Photon=fit2PhotonCluster(towerCluster);
      LOG_DEBUG << "fitAmbiguousCluster chi2 for 2photon fit="<<chiSquare2Photon<<endm;      
      if(chiSquare2Photon <= chiSquare1Photon ){
	  LOG_DEBUG << "fitAmbiguousCluster 2 photon fit is better, validate2ndPhoton"<<endm;
	  if(validate2ndPhoton(towerCluster)) {
	      category = k2PhotonCluster;
	  }
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
  double x=photon->x / mTowerWidthXY.at(0);
  double y=photon->y / mTowerWidthXY.at(1);
  int det=mDetectorId;
  int column = 1 + int(x);
  int row    = 1 + int(y);
  if(mMergeSmallToLarge>0 && x<8.0 && y>9.0 && y<25.0){
      column = 1 + int(x*1.5); //ZZW
      row    = 1 + int((y-9.0)*1.5);
      det+=2;
  } 
    const StFmsTower* tower = searchClusterTowers(det, row, column, **cluster);
  // If tower is nullptr, the photon doesn't hit in a tower in this cluster.
  if (!tower) {
      LOG_DEBUG << "StFmsEventClusterer::validate2ndPhoton No hit on photon" << endm;
      return false;
  }  // if
  // Check if the fitted energy is too large compared to the energy of the tower
  if (tower->hit()->energy() < VALID_FT * photon->energy) {
      LOG_DEBUG << "StFmsEventClusterer::validate2ndPhoton hit on photon too low" << endm;
      return false;
  }  // ifvalidate2ndPhoton
  // Check if the 2nd photon's "high-hower" energy is too large compared to its
  // fitted energy. If so, it is probably splitting one photon into two
  if (tower->hit()->energy() > VALID_2ND_FT * photonEnergyInTower(tower, photon)) {
      LOG_DEBUG << "StFmsEventClusterer::validate2ndPhoton photon energy too low compared to other photon" << endm;
      return false;
  }  // if
  // Check that the 2nd photon is not near the edge of another cluster
  const double energyInOwnCluster =
    photonEnergyInCluster(cluster->get(), photon);
  for (ClusterConstIter i = mClusters.begin(); i != mClusters.end(); ++i) {
    if (i != cluster) {  // Skip the photon's own cluster
      if (photonEnergyInCluster(i->get(), photon) > VALID_E_OWN * energyInOwnCluster) {
	  LOG_DEBUG << "StFmsEventClusterer::validate2ndPhoton photon is at edge of another cluster" << endm;
        return false;  // Stop as soon as we fail for one cluster
      }  // if
    }  // if
  }  // for
  LOG_DEBUG << "StFmsEventClusterer::validate2ndPhoton OK" << endm;
  return true;  // The photon passed all tests; it's real
}
}  // namespace FMSCluster
