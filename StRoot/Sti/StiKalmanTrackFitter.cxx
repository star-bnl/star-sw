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
//______________________________________________________________________________
int StiKalmanTrackFitter::fit(StiTrack * stiTrack, int fitDirection) //throw (Exception)
{
  enum {kMaxNErr=333};
static int nCall=0; nCall++;
StiKalmanTrackNode::Break(nCall);

  if (debug() > 2) cout << "SKTFitter::fit() -I- Started:"<<endl;
  StiKalmanTrack * track = (StiKalmanTrack*)(stiTrack);
  StiHit *targetHit=0;
  StiKalmanTrackNode *targetNode=0; // parent node
  const StiDetector * targetDet=0;  // parent detector
  
  StiKTNBidirectionalIterator source;
  double chi2;
  int status = 0,nerr =0;
  if (!fitDirection) {source = track->begin() ;}
  else               {source = track->rbegin();}
  if (debug()) cout << "StiKalmanTrackFitter::fit direction = "  << fitDirection << endl;
// 1st count number of accepted already good nodes
  int nGoodNodes = track->getNNodes(3);
  if (nGoodNodes<3) 			return 1;


  StiKalmanTrackNode *pNode = 0;		//parent node
  int iNode=0; status = 0;
 for (;(targetNode=source());source++) {
    if (nerr>kMaxNErr) return nerr;
    do { //do refit block
      iNode++;
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
	  {status = targetNode->propagate(pNode,         targetDet,fitDirection);}// hit
	else 
	  {status = targetNode->propagate(pNode,(StiHit*)targetHit,fitDirection);}// vertex
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
        targetNode->setChi2(0.2);
        if (!targetHit) 			 break; //There is no hit.
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
  if (nGoodNodes<3) return 1;
  return (nerr>kMaxNErr)? nerr:0;
}

