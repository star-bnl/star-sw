#include <stdexcept>
#include "StiKalmanTrackFitter.h"
#include "StiKalmanTrack.h"
#include "StiKTNIterator.h"
int StiKalmanTrackFitter::_debug = 0;

StiKalmanTrackFitter::StiKalmanTrackFitter()
{
  StiKalmanTrack::setFitParameters(&_pars);
  _pars.setName("KalmanTrackFitterParameters");
}

StiKalmanTrackFitter::~StiKalmanTrackFitter()
{}

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
void StiKalmanTrackFitter::fit(StiTrack * stiTrack, int fitDirection) //throw (Exception)
{
static int nCall=0; nCall++;
StiKalmanTrackNode::Break(nCall);

  if (debug() > 2) cout << "SKTFitter::fit() -I- Started:"<<endl;
  StiKalmanTrack * track = dynamic_cast<StiKalmanTrack * >(stiTrack);
  if (track==0) 
    throw runtime_error("StiKalmanTrack::fit() - ERROR:\t Dynamic cast to StiKalmanTrack failed");
  StiHit * targetHit;
  StiKalmanTrackNode * targetNode; // parent node
  const StiDetector * targetDet;  // parent detector
  StiDirection trackingDirection =  track->getTrackingDirection(); 
  
  StiKTNBidirectionalIterator first;
  StiKTNBidirectionalIterator last;
  StiKTNBidirectionalIterator source;
  bool direction = (trackingDirection==fitDirection);
  double chi2;
  int status = 0;
  if (direction) {
    first = track->begin();
    last  = track->end();
  } else {
    last  = track->rend();
    first = track->rbegin();
  }
  if (debug()) cout << "StiKalmanTrackFitter::fit set direction T/F= " << trackingDirection << "\t" << fitDirection << endl;
// 1st count number of accepted already good nodes
  int nGoodNodes = track->getNNodes(3);
  if (!nGoodNodes) 			return;
  double errFactor = double(nGoodNodes+1)/nGoodNodes;


  StiKalmanTrackNode *pNode = 0;
  for (source=first;source!=last;source++) {
    do { //do refit block
      targetNode = &(*source);
      targetDet = targetNode->getDetector();
      targetHit = targetNode->getHit();
      double oldChi2 = targetNode->getChi2(); if(oldChi2){/*debugonly*/};
static int myKount=0;myKount++;
      if (!pNode && ((!targetHit)||!targetNode->isValid())) continue;
      //begin refit at first hit
      status = 0;
      if (pNode) {
        targetNode->setChi2(1e51);
	if (targetDet)
	  status = targetNode->propagate(pNode,targetDet,fitDirection);	// hit
	else if (targetHit)
	  status = targetNode->propagate(pNode,targetHit,fitDirection);  // vertex
	if (status) 			continue;
      }
      else  {
	if (debug()) {
	  targetNode->ResetComment(::Form("%30s start refit",targetDet->getName().c_str()));
	  targetNode->PrintpT("S");}
        pNode = targetNode;		continue;
      }
// target node has parameters now but not fitted
// if targetNode has hit, get chi2 and update track parameters accordingly
      do {// Fit
        targetNode->setChi2(0.);
        if (!targetHit) 		break; //There is no hit.
        assert(targetNode->getHit()==targetHit);
        StiKalmanTrackNode tryNode = *targetNode;
        targetNode->setChi2(1e52);
        if (tryNode.nudge(targetHit))	break;
        chi2 = tryNode.evaluateChi2(targetHit);
        if ((chi2>_pars.getMaxChi2()))	break;	//Chi2 is bad
        status = tryNode.updateNode();
        if (status) 			break;
        tryNode.setChi2(chi2);
        tryNode.resetError(errFactor);
        *targetNode=tryNode;
      }while(0);//end fit block
      pNode = targetNode;
    } while(0);//end refit block
    { //continue block
      if (debug()) cout << Form("%5d ",status) << StiKalmanTrackNode::Comment() << endl;
    }//end continue block
  }//end for of nodes
}

void StiKalmanTrackFitter::setParameters(const StiKalmanTrackFitterParameters & pars)
{
  _pars = pars;
}

EditableParameters & StiKalmanTrackFitter::getParameters()
{
  return _pars;
}

void StiKalmanTrackFitter::loadDS(TDataSet&ds)
{
  cout << "StiKalmanTrackFitter::load(TDataSet*ds) -I- Starting" << endl;
  _pars.loadDS(ds); 
  cout << "StiKalmanTrackFitter::load(TDataSet*ds) -I- Done" << endl;
}

void StiKalmanTrackFitter::loadFS(ifstream& inFile)
{
  cout << "StiKalmanTrackFitter::load(ifstream& inFile) -I- Starting" << endl;
  _pars.loadFS(inFile); 
  cout << "StiKalmanTrackFitter::load(ifstream& inFile) -I- Done" << endl;
}

void  StiKalmanTrackFitter::setDefaults()
{
  cout << "StiKalmanTrackFitter::setDefaults() -I- Starting" << endl;
  _pars.setDefaults();
  cout << "StiKalmanTrackFitter::setDefaults() -I- Done" << endl;
}


/*
	useful diagnostic tool - do not delete.

	if (!targetNode) continue;
	    {
	      cout << " StiKalmanTrackFitter::fit(StiTrack * stiTrack, int fitDirection) -E- [1] targetNode==0" << endl;
	      cout << " FIT != TRACKING --- Original Track" << endl;
	      StiKTNForwardIterator it2(track->getLastNode());
	      StiKTNForwardIterator end2 = it2.end();
	      while (it2!=end2) 
		{
		  const StiKalmanTrackNode& node2 = *it2;
		  double x_g = node2.x_g();
		  double y_g = node2.y_g();
		  double rt_g2 = sqrt(x_g*x_g+y_g*y_g);
		  cout << "rt=" << rt_g2 << " " << node2 << endl;
		  ++it2;
		}
	      cout << " report done"<<endl;
	      throw runtime_error("StiKalmanTrack::fit() -E- [1] targetNode==0"); 
	    }
*/
