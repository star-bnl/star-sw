/***************************************************************************
 *
 * $Id: StMuFmsUtil.cxx,v 1.8 2019/06/18 20:22:53 jdb Exp $
 *
 * Author: Jingguo Ma, Jan 2010
 ***************************************************************************
 *
 * Description: FMS Util to convert between StEvent and MuDst
 *
 ***************************************************************************
 *
 * $Log: StMuFmsUtil.cxx,v $
 * Revision 1.8  2019/06/18 20:22:53  jdb
 * Update StMuFmsUtil::recoverMuFmsCollection to solve issue with run16 AuAu data, FmsCollection not cleared
 *
 * Revision 1.7  2017/08/14 16:22:36  smirnovd
 * Recover FMS hits using StTriggerData
 *
 * commit 6d7358f4c86a15edd0671326580d291a9843aec9
 * Date:   Tue Aug 8 23:42:41 2017 -0400
 *
 *     StMuFmsUtil: Recover FMS hits using StTriggerData
 *
 * commit 556d07cb8fd87cb62e4ac674226423671c94917d
 * Date:   Tue Aug 8 23:42:34 2017 -0400
 *
 *     StMuFmsUtil: Added func to fill StMuFmsCollection with FMS hits from StTriggerData in StMuEvent
 *
 * commit c355529c1ee401849b2b81d74df8d452886593d1
 * Date:   Tue Aug 8 23:42:19 2017 -0400
 *
 *     [Cosmetic] Changes in whitespace
 *
 * commit 67fdc1b348bebbfbfb137b726ee9c455a7d8be37
 * Date:   Mon Jun 5 12:00:24 2017 -0400
 *
 *     StMuFmsCollection::addHit() Return pointer to just added default FMS hit object
 *
 * Revision 1.6  2016/06/14 17:11:34  jdb
 * Fixing Coverity Errors:
 * StMuFmsCluster.cxx : UNINIT_CTOR on member mEnergy
 * StMuFmsUtile.cxx : DEADCODE on check for null pointer
 *
 * Revision 1.5  2015/11/06 17:47:16  jdb
 * Added StMuFmsInfo.{h,cxx} as a new branch for storing event-by-event FMS paramters
 *
 * Revision 1.4  2015/10/23 19:22:49  jdb
 * akio added mFmsReconstructionFlag and related getters and setters. pushed version number of StMuFmsCollection. Corresponding changes for reconstruction flag in StMuFmsUtil.cxx
 *
 * Revision 1.3  2015/09/02 22:09:58  jdb
 * Added Akios changes to Fms
 *
 * Revision 1.2  2015/08/28 18:36:04  jdb
 * Added Akios FMS codes
 *
 * Revision 1.1  2010/01/25 03:57:39  tone421
 * Added FMS and Roman pot arrays
 *
 **************************************************************************/
#include "StEvent/StFmsCluster.h"
#include "StMuDSTMaker/COMMON/StMuFmsCluster.h"
#include "StMuDSTMaker/COMMON/StMuFmsHit.h"
#include "StMuDSTMaker/COMMON/StMuFmsPoint.h"
#include "StMuDSTMaker/COMMON/StMuFmsUtil.h"
#include "StMuDSTMaker/COMMON/StMuFmsCollection.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StEvent/StEvent.h"
#include "St_base/StMessMgr.h"
#include "StEvent/StEventTypes.h"
#include "StEvent/StTriggerData.h"
#include "StFmsDbMaker/StFmsDbMaker.h"

#include <algorithm>  // For std::find
#include <iterator>  // For std::distance

#include "TCollection.h"  // For TIter
#include "TRefArray.h"
#include "TVector3.h"

ClassImp(StMuFmsUtil)

namespace {
/*
 Return the index of an element in an StPtrVec-like array.

 The element should be of the pointer type in the StPtrVec.
 Return -1 if the element cannot be located in the array.
 See StEvent/StArray.h for the definition of St[S]PtrVec arrays.
 */
template<class StPtrVec, class Element>
int findElementIndex(const StPtrVec& array, const Element* element) {
  // This typedef is equivalent to an iterator to an Element in the StPtrVec,
  // as defined in StArray.h. The StPtrVec definition does not include a
  // template iterator typedef itself, but this the same as what
  // StPtrVec::const_iterator would do.
  typedef Element* const * ElementConstIter;
  // Find where the element is in the array
  ElementConstIter location = std::find(array.begin(), array.end(), element);
  // Ensure the desired element actually is in the array, as the behaviour
  // of std::distance is undefined otherwise.
  // Return -1 if the element isn't in the array, otherwise return its index
  if (location == array.end()) {  // Not in the array
    return -1;
  } else {
    return std::distance(array.begin(), location);
  }  // if
}
}  // namespace

StMuFmsUtil::StMuFmsUtil()
{
}
StMuFmsUtil::~StMuFmsUtil()
{
}

StMuFmsCollection* StMuFmsUtil::getMuFms(StFmsCollection *fmscol)
{
  LOG_DEBUG << "StMuFmsUtil::getMuFms" << endm;
  if(!fmscol) return NULL;
  StMuFmsCollection* muFms=new StMuFmsCollection();
  fillMuFms(muFms,fmscol);
  return muFms;
}  

StFmsCollection* StMuFmsUtil::getFms(StMuFmsCollection* muFms)
{
  if(!muFms) return NULL;
  
  StFmsCollection *fms=new StFmsCollection();
  fillFms(fms,muFms);
  return fms;
}

void StMuFmsUtil::fillMuFms(StMuFmsCollection *muFms,StFmsCollection *fmscol)
{
  if(!fmscol) return;
  if(!muFms) return;
  //fmscol->print();
  // Do hits and points before clusters, so that the hit and point lists are
  // populated before we try to set hit- and photon-in-cluster information
  // during the cluster loop

  muFms->addInfo();
  muFms->setFmsReconstructionFlag(fmscol->fmsReconstructionFlag());
  
  fillMuFmsHits(muFms, fmscol);
  fillMuFmsPoints(muFms, fmscol);
  fillMuFmsClusters(muFms, fmscol);
  // Now we need to go back and set parent cluster of each point, now that the
  // cluster list in StMuFmsCollection is populated (as these are the clusters
  // we reference).
  setMuFmsPointParentClusters(muFms, fmscol);
}

void StMuFmsUtil::fillFms(StFmsCollection* fmscol,StMuFmsCollection* muFms)
{
  if(!muFms) return;
  if(!fmscol) return;
  fmscol->setFmsReconstructionFlag(muFms->fmsReconstructionFlag());
  fillFmsHits(fmscol, muFms);
  fillFmsPoints(fmscol, muFms);
  fillFmsClusters(fmscol, muFms);
  // Now we need to go back and set parent cluster of each point, now that the
  // cluster list in StFmsCollection is populated (as these are the clusters
  // we reference).
  setFmsPointParentClusters(fmscol, muFms);
}

void StMuFmsUtil::fillMuFmsHits(StMuFmsCollection* muFms,
                                StFmsCollection* fmscol) {
  StSPtrVecFmsHit vecHit = fmscol->hits();
  for(unsigned int i=0; i<fmscol->numberOfHits(); i++){
    unsigned short detId = vecHit[i]->detectorId();
    unsigned short ch    = vecHit[i]->channel();
    unsigned short crate = vecHit[i]->qtCrate();
    unsigned short slot  = vecHit[i]->qtSlot();
    unsigned short qtch  = vecHit[i]->qtChannel();
    unsigned short adc   = vecHit[i]->adc();
    unsigned short tdc   = vecHit[i]->tdc();
    float          ene   = vecHit[i]->energy();
    muFms->addHit();
    StMuFmsHit* muFmsHit = muFms->getHit(i);
    muFmsHit->setDetectorId(detId);
    muFmsHit->setChannel(ch);
    muFmsHit->setQtCrate(crate);
    muFmsHit->setQtSlot(slot);
    muFmsHit->setQtChannel(qtch);
    muFmsHit->setAdc(adc);
    muFmsHit->setTdc(tdc);
    muFmsHit->setEnergy(ene);
  }
}



/**
 * Creates `StMuFmsHit`s from `StTriggerData` and appends them to
 * `StMuFmsCollection`. Some of this code is taken from `StFmsHitMaker::Make()`
 * that implements a similar conversion: `StTriggerData` -> `StFmsCollection`
 * "Physical" properties of StMuFmsHit such as "energy" etc. will be filled only
 * if an optional StFmsDbMaker object is provided.
 */
void StMuFmsUtil::fillMuFmsHits(StMuFmsCollection& muFmsCollection,
  const StTriggerData& triggerData, const StFmsDbMaker* fmsDbMaker)
{

  // Loop over "electronics" channels and extract raw hit data from StTriggerData
  for(unsigned short crate=1; crate<=4; crate++)
  {
    for(unsigned short slot=1; slot<=16; slot++)
    {
      for(unsigned short ch=0; ch<32; ch++)
      {
        unsigned short adc = triggerData.fmsADC(crate,slot-1,ch);
        unsigned short tdc = triggerData.fmsTDC(crate,slot-1,ch);

        if (adc <= 0 && tdc <= 0) continue;

        StMuFmsHit* muFmsHit = muFmsCollection.addHit();

        muFmsHit->setQtCrate(crate);
        muFmsHit->setQtSlot(slot);
        muFmsHit->setQtChannel(ch);
        muFmsHit->setAdc(adc);
        muFmsHit->setTdc(tdc);

        // Can proceed with "physical" quantities only if fmsDbMaker is available
        if ( !fmsDbMaker ) continue;

        int detectorId, channelId;

        fmsDbMaker->getReverseMap(crate, slot, ch, &detectorId, &channelId);

        // Cannot not proceed with invalid detector and channel IDs
        if ( detectorId <= 0 && channelId <= 0) continue;

        float g1 = fmsDbMaker->getGain(detectorId, channelId);
        float g2 = fmsDbMaker->getGainCorrection(detectorId, channelId);
        float energy  = adc*g1*g2;

        muFmsHit->setDetectorId(detectorId);
        muFmsHit->setChannel(channelId);
        muFmsHit->setEnergy(energy);
      }
    }
  }
}


/**
 * In the provided `muDst` object fills StMuFmsCollection with FMS hits
 * extracted from the StTriggerData block in the same `muDst`. The action takes
 * place only if there are no FMS hits in the muDst's StMuFmsCollection thus we
 * refer to this as "recovery". "Physical" properties of StMuFmsHit such as
 * "energy" etc. will be filled only if an optional StFmsDbMaker object is
 * provided.
 */
void StMuFmsUtil::recoverMuFmsCollection(StMuDst& muDst, const StFmsDbMaker* fmsDbMaker)
{
  StTriggerData* triggerData = (StTriggerData*) muDst.event()->triggerData();
  TClonesArray* muFmsHits = muDst.muFmsCollection() ? muDst.muFmsCollection()->getHitArray() : nullptr;

  const Int_t runNumber = muDst.event()->runId();

  if ( triggerData && muFmsHits && runNumber < 18000000 ){ // recover for run16 AuAu data
    if ( muFmsHits->GetEntriesFast()>0 ) {
      muFmsHits->Clear();
    }
    fillMuFmsHits(*muDst.muFmsCollection(), *triggerData, fmsDbMaker);
  }
}



void StMuFmsUtil::fillMuFmsClusters(StMuFmsCollection* muFms,
                                    StFmsCollection* fmscol) {
  // Fill clusters
  for (unsigned i(0); i < fmscol->numberOfClusters(); ++i) {
    const StFmsCluster* cluster = fmscol->clusters()[i];
    muFms->addCluster();  // Expand StMuFmsCollection cluster array by 1
    StMuFmsCluster* muCluster = muFms->getCluster(i);
    muCluster->setDetectorId(cluster->detectorId());
    muCluster->setCategory(cluster->category());
    muCluster->setEnergy(cluster->energy());
    muCluster->setX(cluster->x());
    muCluster->setY(cluster->y());
    muCluster->setSigmaMin(cluster->sigmaMin());
    muCluster->setSigmaMax(cluster->sigmaMax());
    muCluster->setChi2Ndf1Photon(cluster->chi2Ndf1Photon());
    muCluster->setChi2Ndf2Photon(cluster->chi2Ndf2Photon());
    muCluster->setId(cluster->id());
    
    // Propagate hits-in-cluster information
    // Remember, clusters don't *own* hits, they just reference them.
    // For each StFmsHit in the cluster, find the index of that hit in the main
    // StFmsCollection hit array. Then add the StMuFmsHit (from the main
    // StMuFmsCollection hit array) at the same index to the StMuFmsCluster.
    StPtrVecFmsHitConstIterator hit;  // Iterate over StFmsHits
    for (hit = cluster->hits().begin(); hit != cluster->hits().end(); ++hit) {
      const int index = findElementIndex(fmscol->hits(), *hit);
      if (index != -1) {
        muCluster->hits()->Add(muFms->getHit(index));
      }  // if
    }  // for
    // Do the same procedure for photon-in-cluster information
    StPtrVecFmsPointConstIterator p;
    for (p = cluster->points().begin(); p != cluster->points().end(); ++p) {
      const int index = findElementIndex(fmscol->points(), *p);
      if (index != -1) {
        muCluster->photons()->Add(muFms->getPoint(index));
      }  // if
    }  // for
  }  // for
}

void StMuFmsUtil::fillMuFmsPoints(StMuFmsCollection* muFms,
                                  StFmsCollection* fmscol) {
  for (unsigned i(0); i < fmscol->numberOfPoints(); ++i) {
    const StFmsPoint* point = fmscol->points()[i];
    StMuFmsPoint* muPoint = muFms->addPoint();
    if (point && muPoint) {
      muPoint->set(*point);
    }  // if
  }  // for
}

void StMuFmsUtil::setMuFmsPointParentClusters(StMuFmsCollection* muFms,
                                              StFmsCollection* fmscol) {
  LOG_DEBUG << "setMuFmsPointParentClusters" << endm;
  for (unsigned i(0); i < muFms->numberOfPoints(); ++i) {
    // Points and clusters in the StMuFmsCollection and StFmsCollection are in
    // the same order, so we get the corresponding objects just by index
    const StFmsPoint* point = fmscol->points().at(i);
    if (!point) {
      //LOG_WARN << Form("  No point") << endm;
      continue;
    }  // if
    // Find the index of the point's parent cluster in the main cluster list
    const int index = findElementIndex(fmscol->clusters(), point->cluster());
    // If we found it, set the StMuFmsPoint's parent cluster to be the
    // corresponding cluster in the StMuFmsCollection
    if (index != -1) {
      StMuFmsPoint* muPoint = muFms->getPoint(i);
      if (muPoint) {
        muPoint->setCluster(muFms->getCluster(index));
      }   // if
    }  // if
  }  // for
}

void StMuFmsUtil::fillFmsHits(StFmsCollection* fmscol,
                              StMuFmsCollection* muFms) {
  // Using TIter to iterate is safe in the case of hits being NULL
  TIter next(muFms->getHitArray());
  StMuFmsHit* muHit(NULL);
  while ((muHit = static_cast<StMuFmsHit*>(next()))) {
    fmscol->addHit(new StFmsHit);
    StFmsHit* hit = fmscol->hits().back();
    hit->setDetectorId(muHit->detectorId());
    hit->setChannel(muHit->channel());
    hit->setQtCrate(muHit->qtCrate());
    hit->setQtSlot(muHit->qtSlot());
    hit->setQtChannel(muHit->qtChannel());
    hit->setAdc(muHit->adc());
    hit->setTdc(muHit->tdc());
    hit->setEnergy(muHit->energy());
  }  // while
}

void StMuFmsUtil::fillFmsClusters(StFmsCollection* fmscol,
                                  StMuFmsCollection* muFms) {
  // Using TIter to iterate is safe in the case of clusters being NULL
  TIter next(muFms->getClusterArray());
  StMuFmsCluster* muCluster(NULL);
  while ((muCluster = static_cast<StMuFmsCluster*>(next()))) {
    // Create an StFmsCluster from this StMuFmsCluster
    fmscol->addCluster(new StFmsCluster);
    StFmsCluster* cluster = fmscol->clusters().back();
    cluster->setDetectorId(muCluster->detectorId());
    cluster->setCategory(muCluster->category());
    cluster->setNTowers(muCluster->hits()->GetEntries());
    cluster->setEnergy(muCluster->energy());
    cluster->setX(muCluster->x());
    cluster->setY(muCluster->y());
    cluster->setSigmaMin(muCluster->sigmaMin());
    cluster->setSigmaMax(muCluster->sigmaMax());
    cluster->setChi2Ndf1Photon(muCluster->chi2Ndf1Photon());
    cluster->setChi2Ndf2Photon(muCluster->chi2Ndf2Photon());
    cluster->setId(muCluster->id());

    //get pointers to fms points
    TIter next(muCluster->photons());
    StMuFmsPoint* muPoint(NULL);
    while ((muPoint = static_cast<StMuFmsPoint*>(next()))) {
      const int index = muFms->getPointArray()->IndexOf(muPoint);
      if(index != -1) {
	StFmsPoint* point = fmscol->points().at(index);
	cluster->points().push_back(point);
      }
    }
    /** \todo fill 4-momentum. Requires adding z field to StMuFmsPoint */
    /** \todo propagate hit pointers information */
  }  // while
}

void StMuFmsUtil::fillFmsPoints(StFmsCollection* fmscol,
                                StMuFmsCollection* muFms) {
  // Using TIter to iterate is safe in the case of points being NULL
  TIter next(muFms->getPointArray());
  StMuFmsPoint* muPoint(NULL);
  while ((muPoint = static_cast<StMuFmsPoint*>(next()))) {
    // Create an StFmsPoint from this StMuFmsPoint
    fmscol->addPoint(new StFmsPoint);
    StFmsPoint* point = fmscol->points().back();
    float energy=muPoint->energy();
    point->setDetectorId(muPoint->detectorId());
    point->setEnergy(energy);
    point->setX(muPoint->x());
    point->setY(muPoint->y());
    point->setId(muPoint->id());
    point->setXYZ(muPoint->xyz());
    //StThreeVectorF xyz(muPoint->x(),muPoint->y(),muPoint->z());
    //StThreeVectorF p = xyz.unit() * energy;
    //point->setFourMomentum(StLorentzVectorF(p, energy));
  }  // while
}

void StMuFmsUtil::setFmsPointParentClusters(StFmsCollection* fmscol,
                                            StMuFmsCollection* muFms) {
  // Points and clusters in the StMuFmsCollection and StFmsCollection are in
  // the same order, so we get the corresponding objects just by index
  for (unsigned i(0); i < muFms->numberOfClusters(); ++i) {
    const StMuFmsPoint* muPoint = muFms->getPoint(i);
    if (!muPoint) {
      continue;
    }  // if
    const int index = muFms->getClusterArray()->IndexOf(muPoint->cluster());
    if (index != -1) {
      StFmsPoint* point = fmscol->points().at(i);
      if (point) {
	StFmsCluster* cluster=fmscol->clusters().at(index);
        point->setCluster(cluster);
	point->setParentClusterId(cluster->id());
	point->setNParentClusterPhotons(cluster->nPhotons());
      }  // if
    }  // if
  }  // for
}
