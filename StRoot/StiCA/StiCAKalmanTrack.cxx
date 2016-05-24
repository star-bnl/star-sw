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
  int errType = kNoErrors;
  
  static int nCall=0; nCall++;
  StiDebug::Break(nCall);
  enum {kMaxIter=30,kPctLoss=10,kHitLoss=3};
  static double defConfidence = StiDebug::dFlag("StiConfidence",0.01);
  int nNBeg = getNNodes(3), nNEnd = nNBeg;
  if (nNBeg<=3) 	return 1;
  if (!mgMaxRefiter) 	return 0;
  StiKalmanTrackNode *inn= getInnerMostNode(3);
  int fail=0,status=0;

  StiNodePars pPrev;
  StiNodeErrs ePrev;
  int iter=0,igor=0;
  double qA;
  double errConfidence = defConfidence;
  for (int ITER=0;ITER<mgMaxRefiter;ITER++) {
    for (iter=0;iter<kMaxIter;iter++) {
      fail = 0;
      errType = kNoErrors;
      sTNH.set(StiKalmanTrackFitterParameters::instance()->getMaxChi2()*10,StiKalmanTrackFitterParameters::instance()->getMaxChi2Vtx()*100,errConfidence,iter);
      pPrev = inn->fitPars();
      ePrev = inn->fitErrs(); 
      
      status = refitL();  
      if (status) 	{fail= 1; errType = kRefitFail; break;}
      nNEnd = sTNH.getUsed();
      if ((nNEnd <=3))	{fail= 2; errType = kNotEnoughUsed; break;}
      if (!inn->isValid() || inn->getChi2()>1000) {
        inn = getInnerMostNode(3); fail=-1; errType = kInNodeNotValid; continue;}	
      qA = StiKalmanTrack::diff(pPrev,ePrev,inn->fitPars(),inn->fitErrs(),igor);
      static int oldRefit = StiDebug::iFlag("StiOldRefit");
      if (oldRefit) {
        if (qA>0.5)		{fail=-2; errType = kBadQA; continue;} 
      } else {
        if (qA <1 && errConfidence>0.1) errConfidence = 0.1;
        if (qA>0.01)		{fail=-2; errType = kBadQA; continue;} 
        if (sTNH.isCutStep())	{fail=-2; errType = kBadQA; continue;} 
      }
      double info[2][8];
      sTNH.mCurvQa.getInfo(info[0]);
      sTNH.mTanlQa.getInfo(info[1]);
      break;
    }
    if (fail>0) 						break;
      //		
    StiKalmanTrackNode *worstNode= sTNH.getWorst();
    if (worstNode && worstNode->getChi2()>StiKalmanTrackFitterParameters::instance()->getMaxChi2())     
    {//worstNode->getHit()->subTimesUsed();
      worstNode->setHit(0); worstNode->setChi2(3e33); continue;}
    if (rejectByHitSet()) { releaseHits()            ; continue;}
    
    if (!fail) 							break;
    
    StiKalmanTrackNode *flipFlopNode= sTNH.getFlipFlop();
    if (flipFlopNode && flipFlopNode->getFlipFlop()>kMaxIter/3)     
    {//flipFlopNode->getHit()->subTimesUsed();
      flipFlopNode->setHit(0); flipFlopNode->setChi2(3e33); 	continue;}
    break;
      //	The last resource
      //    errConfidence = 0.5*(errConfidence+1);
      //    if (errConfidence>0.99) 					break;
  }
  StiKalmanTrackNode *vertexNode= sTNH.getVertexNode();

    //		Test for primary 
  while (!fail && vertexNode) {
    fail = 13;			//prim node invalid
    errType = kVertexNodeInvalid;
    if (!vertexNode->isValid()) 				break;
    fail = 99;			//prim node Chi2 too big
    errType = kNodeNotValid;
    if ( vertexNode->getChi2()>StiKalmanTrackFitterParameters::instance()->getMaxChi2Vtx())	break;
    fail = 98;			//too many dropped nodes
    errType = kTooManyDroppedNodes;    
    if (nNBeg*kPctLoss/100 < nNBeg-nNEnd
        &&  nNEnd+kHitLoss < nNBeg)					break;
    fail = 0;
    errType = kNoErrors;
    break;    
  }
  if (!fail) { //Cleanup. Hits of bad nodes set to zero
    StiKalmanTrackNode *node;
    StiKTNIterator it = begin();
    for (;(node=it());it++){
      if (node == vertexNode)				continue;
      StiHit *hit = node->getHit();
      if(!hit) 						continue;
      if (node->isValid() && node->getChi2()<10000. ) 	continue;
      node->setHit(0);
    }
  }

  if (fail) setFlag(-1);
  return errType;
}
