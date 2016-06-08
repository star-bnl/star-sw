#include "Sti/StiToolkit.h"
#include "Sti/StiDetectorContainer.h"
#include "StiCA/StiCAKalmanTrack.h"
#include "StDetectorDbMaker/StiKalmanTrackFitterParameters.h"
#include "Sti/StiTrackNodeHelper.h"
#include "StEvent/StEnumerations.h"
#include "StiUtilities/StiDebug.h"



//_____________________________________________________________________________
int StiCAKalmanTrack::initialize(const std::vector<StiHit*> &hits)
{
  initialize0(hits);
  
  int ierr = approx(0);
  if (!ierr) return 0;
  BFactory::Free(this);
  return 1;
}


int StiCAKalmanTrack::initialize0(const std::vector<StiHit*> &hits, StiNodePars *firstPars, StiNodePars *lastPars, StiNodeErrs *firstErrs, StiNodeErrs *lastErrs)
{
  //cout << "StiCAKalmanTrack::initialize() -I- Started"<<endl;
  reset();
  //StiKalmanTrackNode * node  = 0;
  const StiDetector* detector=0;
  UInt_t nhits = hits.size();
  setSeedHitCount(nhits);
  StiDetectorContainer    *detectorContainer = StiToolkit::instance()->getDetectorContainer();
  const StiDetector* detectorOld = 0;
  StiHit *hit_Old = 0;
  for (UInt_t ihit = 0; ihit < nhits; ihit++)  {
    StiHit *hit = hits[ihit];
    detector = hit->detector();
    assert(detector);
    // look for gaps in hit list
    if (hit_Old && detector->getGroupId() == kTpcId) {
      Double_t R_hit = detector->getPlacement()->getLayerRadius();
      Double_t angle_hit = detector->getPlacement()->getNormalRefAngle();
      detectorOld = hit_Old->detector();
      Double_t R_hit_OLD = detectorOld->getPlacement()->getLayerRadius();
      if (_debug && detectorOld == detector) {
	cout << "The same detector for hit " << ihit << endl;
	cout << "hit     \t" << *hit << endl;
	if (hit_Old) 
	  cout << "hitOld\t" << *hit_Old << endl;
      }
      Double_t angle_hit_OLD = detectorOld->getPlacement()->getNormalRefAngle();
      if (TMath::Abs(angle_hit - angle_hit_OLD) < TMath::DegToRad()*5) { // the same sector
	while ((R_hit < R_hit_OLD)) {
	  detectorContainer->setToDetector( detectorOld );
	  if ( detectorContainer->moveIn()) {
	    StiDetector* d = detectorContainer->getCurrentDetector(); //**detectorContainer;
	    if (d == detector) break;
	    detectorOld = d;
	    R_hit_OLD = detectorOld->getPlacement()->getLayerRadius();
	    if (detectorOld->isActive()) {
	      StiKalmanTrackNode * nI = trackNodeFactory->getInstance();
	      nI->initialize(d);
	      add(nI,kOutsideIn);
	    }
	  }
	}
      }
    }
    StiKalmanTrackNode * n = trackNodeFactory->getInstance();
    n->initialize(hit);
    add(n,kOutsideIn);
    detectorOld = (StiDetector*) detector;
    hit_Old = hit;
  }  
  if (firstPars){
    firstNode->fitPars() = *firstPars;
  }
  if (firstErrs){ 
    firstNode->fitErrs() = *firstErrs;
      //    firstNode->resetError();
  }
  if (lastPars){
    lastNode ->fitPars() = *lastPars;
  }
  if (lastErrs){ 
    lastNode->fitErrs() = *lastErrs;
      //    firstNode->resetError();
  }
  return 0;  
}


StiKalmanTrackNode * StiCAKalmanTrack::getInnerMostTPCHitNode(int qua)   const
{
  if (firstNode==0 || lastNode==0)
 {
  //cout << "StiCAKalmanTrack::getInnOutMostNode() -E- firstNode||lastNode==0" << endl;
  throw runtime_error("StiCAKalmanTrack::getInnOutMostNode() -E- firstNode||lastNode==0");
 }

  StiKalmanTrackNode *node = 0;
  StiKalmanTrackNode* leaf = getLastNode();
  StiKTNForwardIterator it(leaf);
  StiKTNForwardIterator end = it.end();
  for (;it!=end;++it) 
  {
    StiKalmanTrackNode& node_t = *it;
    if (!node_t.isValid())		continue;
    if (node_t.getChi2()>10000.) 	continue;
    StiHit* hit = node_t.getHit();
    if (!hit) 			continue;
    if(hit->x()<58.f) continue;
    node = &node_t;
    return node;
  }
  
  cout << "StiCAKalmanTrack::getInnOutMostNode() -E- No requested nodes " << endl;
  //throw runtime_error("StiCAKalmanTrack::getInnOutMostNode() -E- No requested nodes");*/
  return 0;
}

//_____________________________________________________________________________
int StiCAKalmanTrack::refit() 
{
  int errType;
  StiKalmanTrack::refit(errType);
  return errType;
}
