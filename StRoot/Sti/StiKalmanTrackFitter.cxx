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
  int status;
  bool started = false;
#if 1
  if (direction) {
    first = track->begin();
    last  = track->end();
  } else {
    last  = track->rend();
    first = track->rbegin();
  }
  if (debug()) cout << "StiKalmanTrackFitter::fit set direction T/F= " << trackingDirection << "\t" << fitDirection << endl;
  for (source=first;source!=last;source++) {
    if (direction) {
      if ((*source).getChildCount()<=0) break;
      targetNode= static_cast<StiKalmanTrackNode*>((*source).getFirstChild());
    }
    else 
      targetNode= static_cast<StiKalmanTrackNode*>((*source).getParent());
    if (! targetNode) break;
    targetDet = targetNode->getDetector();
    targetHit = targetNode->getHit();
    //begin refit at first hit
    if (!targetHit && !started)	goto ENDLB;
    started = true;
    // evolve state from that of source using dets source to target
    if (targetDet)
      status = targetNode->propagate(&(*source),targetDet,fitDirection);	// hit
    else if (targetHit)
      status = targetNode->propagate(&(*source),targetHit,fitDirection);  // vertex
    if (status) 		goto ENDLB;
    // if targetNode has hit, get chi2 and update track parameters accordingly
    if (!targetHit) 		goto ENDLB;
    if (targetNode->nudge())	goto ENDLB;
    chi2 = targetNode->evaluateChi2(targetHit);
    targetNode->setChi2(1e52);
    if (!(chi2<_pars.getMaxChi2()))goto ENDLB;
    status = targetNode->updateNode();
    if (status) 			goto ENDLB;
    targetNode->setChi2(chi2);
    //cout<<"=="<<endl;
  ENDLB:
    if (debug()) {
      if (status) cout << "failed";
      else        cout << "passed";
      cout <<"\t" << StiKalmanTrackNode::Comment() << endl;
    }
  }
#else
  if (direction) {
    { 
      if (debug()) cout << "StiKalmanTrackFitter::fit set =="<<endl;
      first = track->begin();
      last  = track->end();
      //cout << " ||||||" << endl;
      for (source=first;source!=last;source++)
	{
	  if ((*source).getChildCount()<=0) break;
	  targetNode= static_cast<StiKalmanTrackNode*>((*source).getFirstChild());
	  targetNode->setChi2(1e52);
	  targetDet = targetNode->getDetector();
	  targetHit = targetNode->getHit();
	  //begin refit at first hit
	  if (!targetHit && !started)	goto ENDLB;
	  if (!started) {started = true; targetNode->resetError();}
	  // evolve state from that of source using dets source to target
	  if (targetDet)
	    status = targetNode->propagate(&(*source),targetDet,fitDirection);	// hit
	  else if (targetHit)
	    status = targetNode->propagate(&(*source),targetHit,fitDirection);  // vertex
            if (status) 		goto ENDLB;
	  // if targetNode has hit, get chi2 and update track parameters accordingly
	  if (!targetHit) 		goto ENDLB;
	  if (targetNode->nudge())	goto ENDLB;
	  chi2 = targetNode->evaluateChi2(targetHit);
	  targetNode->setChi2(1e52);
	  if (!(chi2<_pars.getMaxChi2()))goto ENDLB;
	  status = targetNode->updateNode();
	  if (status) 			goto ENDLB;
	  targetNode->setChi2(chi2);
	  //cout<<"=="<<endl;
	ENDLB:
	  if (debug()) cout << status << "\t" << StiKalmanTrackNode::Comment() << endl;
	}
    }
  else
    {
      //cout << " set ++" << endl;
      last  = track->rend();
      first = track->rbegin();
      if (debug()) cout << "StiKalmanTrackFitter::fit <<<<<refit<<<<<"<<endl;
      for (source=first;source!=last;source++)
	{
	  targetNode= static_cast<StiKalmanTrackNode*>((*source).getParent());
	  if (!targetNode) break;
	  targetNode->setChi2(1e51);
	  targetDet = targetNode->getDetector();
	  targetHit = targetNode->getHit();
	  //begin refit at first hit
	  if (!targetHit && !started) 	goto ENDLF;
	  if (!started) {started = true; targetNode->resetError();}
	  //cout << "  ==== " << *source << endl;
	  // evolve state from that of source using dets source to target
	  if (targetDet)
	    status = targetNode->propagate(&(*source),targetDet,fitDirection);	// hit
	  else if (targetHit)
	    status = targetNode->propagate(&(*source),targetHit,fitDirection);  // vertex
          if (status) 			goto ENDLF;
	  // if targetNode has hit, get chi2 and update track parameters accordingly
	  if (!targetHit) 		goto ENDLF;
	  targetNode->nudge();
	  chi2 = targetNode->evaluateChi2(targetHit);
	  targetNode->setChi2(1e51);
	  if (!(chi2 <_pars.getMaxChi2()))goto ENDLF;
	  status = targetNode->updateNode();
          if (status) 			goto ENDLF;
          targetNode->setChi2(chi2);
	  //cout<<"!="<<endl;
	ENDLF:
	  if (debug()) {
	    cout << status << "\t" << StiKalmanTrackNode::Comment() << endl;
	  }
	}
      if (debug()) cout << "StiKalmanTrackFitter::fit   <<<<<<<<<<<Done Refit" << endl; 
    }
  //cout << "SKTFitter::fit() -I- Done:"<<endl;
#endif
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
