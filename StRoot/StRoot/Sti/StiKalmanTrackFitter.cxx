#include <stdexcept>
#include "StiKalmanTrackFitter.h"
#include "StiKalmanTrack.h"
#include "StiKTNIterator.h"


Int_t StiKalmanTrackFitter::_debug = 0;

/*! Fit given track with helicoical track model.
  <h3>Notes</h3>
	<ol>
  <li>The fit is performed along a direction (inside-out||outside-in) prescribed by 
      the value of the track flag "FittingDirection".</li>
  <li>In practice, the iteration through track nodes proceeds from first-to-last
      or last-to-first whether "trackingDirection==fitDirection"
  <li>Use track node methods to do the actual propagation. 
  <li>Node with no hits are allowed as the evaluation of the track chi2 
      and updates are performed only if nodes hold a hit.
	</ol>
*/
Int_t StiKalmanTrackFitter::fit(StiTrack * stiTrack, Int_t fitDirection)  
{
  enum {kMaxNErr=333};
  static Int_t nCall=0; nCall++;

  if (debug() > 2) cout << "SKTFitter::fit() -I- Started:"<<endl;
  StiKalmanTrack * track = dynamic_cast<StiKalmanTrack * >(stiTrack);
  assert(track); 
  StiHit * targetHit;
  StiKalmanTrackNode * targetNode; // parent node
  const StiDetector * targetDet;  // parent detector
  
  StiKTNBidirectionalIterator first;
  StiKTNBidirectionalIterator last;
  StiKTNBidirectionalIterator source;
  Double_t chi2;
  Int_t status = 0,nerr =0;
  if (!fitDirection) {
    first = track->begin();
    last  = track->end();
  } else {
    last  = track->rend();
    first = track->rbegin();
  }
  if (debug()) cout << "StiKalmanTrackFitter::fit direction = "  << fitDirection << endl;
// 1st count number of accepted already good nodes
  Int_t nGoodNodes = track->getNNodes(3);
  if (nGoodNodes<3) 			return kShortTrackBeforeFit;


  StiKalmanTrackNode *pNode = 0;
  Int_t iNode=0; status = 0;
  for (source=first;source!=last;source++) {
    if (nerr>kMaxNErr) return nerr;
    do { //do refit block
      iNode++;
      targetNode = &(*source);
      targetDet = targetNode->getDetector();
      targetHit = targetNode->getHit();
      Double_t oldChi2 = targetNode->getChi2(); if(oldChi2){/*debugonly*/};
      static Int_t myKount=0;myKount++;
      if (!pNode && !targetNode->isValid()) continue;
      //begin refit at first hit
      status = 0;
      if (pNode) {
        targetNode->setChi2(1e51);
	if (targetDet)
	  status = targetNode->propagate(pNode,targetDet,fitDirection);	// hit
	else if (targetHit)
	  status = targetNode->propagate(pNode,targetHit,fitDirection);  // vertex
	if (status)			{nerr++; continue;}
      }
      else  {
	if (debug()) {
	  targetNode->ResetComment(::Form("%30s start refit",targetDet->getName().c_str()));
	  targetNode->PrintpT("S");}
//        pNode = targetNode;		continue;
        pNode = targetNode;		
      }
// target node has parameters now but not fitted
// if targetNode has hit, get chi2 and update track parameters accordingly
      do {// Fit
        targetNode->setChi2(0.);
        if (!targetHit) 		break; //There is no hit.
        assert(targetNode->getHit()==targetHit);
        StiKalmanTrackNode tryNode = *targetNode;
        targetNode->setChi2(1e52);
        if (tryNode.nudge(targetHit))	{nerr++; break;}
	chi2 = tryNode.evaluateChi2(targetHit);
        if ((chi2>StiKalmanTrackFitterParameters::instance()->getMaxChi2()))	{nerr++; break;}	//Chi2 is bad
        status = tryNode.updateNode();
        if (status) 			{nerr++; break;}
        tryNode.setChi2(chi2);
	{ //continue block
	  if (debug()) {cout << Form("%5d ",status); StiKalmanTrackNode::PrintStep();}
	}//end continue block

        *targetNode=tryNode;
      }while(0);//end fit block
      pNode = targetNode;
    } while(0);//end refit block
  }//end for of nodes
  nGoodNodes = track->getNNodes(3);
  if (nGoodNodes<3) return kShortTrackAfterFit;
  return (nerr>kMaxNErr)? kManyErrors:0;
}

