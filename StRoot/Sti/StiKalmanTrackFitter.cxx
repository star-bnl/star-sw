#include "StiKalmanTrackFitter.h"
#include "StiKalmanTrack.h"


StiKalmanTrackFitter::StiKalmanTrackFitter()
{
  fitMethod = Inward;
}

void StiKalmanTrackFitter::fit(StiTrack * stiTrack) throw (Exception)
{
  /*
    Fit this track using the currently selected fit method.
  */
  cout << "StiKalmanTrackFitter::fit() - ";

  StiKalmanTrack * track = dynamic_cast<StiKalmanTrack * >(stiTrack);

  StiKalmanTrackNode * first = track->getFirstNode();
  StiKalmanTrackNode * last  = track->getLastNode();  
  if (first==0)
    throw new Exception("StiKalmanTrackFitter::fit(): track->getFirstChild() returned null");
  if (last==0)
    throw new Exception("StiKalmanTrackFitter::fit(): track->getLastChild() returned null");
  
  switch (fitMethod)
    {
    case Inward:
      cout << "Inward" << endl;
      fitInward(first);
      track->setChi2(last->fChi2);
			if (last->fP3>0)
				track->setCharge(unitCharge);
			else
				track->setCharge(-unitCharge);
			break;
    case Outward:
      cout << "Outward" << endl;
      fitOutward(last);
      track->setChi2(first->fChi2);
			if (last->fP3>0)
				track->setCharge(unitCharge);
			else
				track->setCharge(-unitCharge);
			break;
    }
}

void StiKalmanTrackFitter::fitInward(StiKalmanTrackNode * node) throw (Exception)
{
  /**
     Perform a track fit from the outside-in.
     The track is assumed to consist of a simple node a sequence. 
     i.e. It is assumed to be pruned. Additionally, since the 
     fit is done from the outside-in the MCS and E-loss are 
     turned off since one would be getting wrong values in 
     this case.
     
     The fit starts from the given node which may (or may not)
     be the first node of the track, the outer most point of the
     track.
  */
  StiKalmanTrackNode * pNode; // parent node
  StiKalmanTrackNode * cNode; // child node
  StiDetector * pDet;  // parent detector
  StiDetector * cDet;  // child detector
  //double chi2;
  
  pNode = node;
  pDet  = pNode->getDetector();
  cout << "StiKalmanTrackFitter::fitInward()" << endl
       << "FIRST NODE::" << endl
       << *pNode << endl;
  int pos;
  while (pNode->getChildCount()>0)      {
    cNode = dynamic_cast<StiKalmanTrackNode *>(pNode->getFirstChild());
    cout << "Child: " <<  *cNode;
    cDet  = cNode->getHit()->detector();
    
    pos = cNode->propagate(pNode,cDet);	  // evolve state from pDet to cDet
    if (pos<0)	      {
      cout << "POSITION < 0 =======================" << endl;
      break;
    }
    cNode->evaluateChi2();
    cNode->updateNode();
    cout << "UPDATED: " <<  *cNode;
    pNode = cNode;
    pDet  = cDet;
  }
}

void StiKalmanTrackFitter::fitOutward(StiKalmanTrackNode * node) throw (Exception) 
{
  /**
     Perform a track fit from the outside-in.
     The track is assumed to consist of a simple node a sequence. 
     i.e. It is assumed to be pruned. 
     
     The fit starts from the given node which may (or may not)
     be the last node of the track, the inner most point of the
     track.
     
     If MCS and ELOSS calculations are required, they must be
     set externally to this call.
     
     Likewise, the mass hypothesis must also be made externally.
  */
  StiKalmanTrackNode * pNode; // parent node
  StiKalmanTrackNode * cNode; // child node
  StiDetector * pDet;  // parent detector
  StiDetector * cDet;  // child detector
  //double chi2;
  
  cNode = node;
  cDet  = cNode->getHit()->detector();
  pNode = dynamic_cast<StiKalmanTrackNode *>(cNode->getParent());
  while (pNode)
    {
      pDet  = pNode->getHit()->detector();
      pNode->propagate(cNode,pDet);	// evolve state from cDet to pDet
      pNode->evaluateChi2();
      pNode->updateNode();
      cNode = pNode;
      cDet  = pDet;
      pNode = dynamic_cast<StiKalmanTrackNode *>(cNode->getParent());
    }
}


//_set/get_ methods

void StiKalmanTrackFitter::setFitMethod(StiFitMethod method)
{
  fitMethod = method;
}

StiFitMethod StiKalmanTrackFitter::getFitMethod() const
{
  return fitMethod;
}

