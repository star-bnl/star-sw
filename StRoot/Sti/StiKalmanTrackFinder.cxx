/*
  Copyright(c) 2001, STAR  Experiment at BNL, All rights reserved.      
  
  Author: STAR Integrated Track Task Force                              
  Contributors are mentioned in the code where appropriate.
  
  Permission to use, copy, modify and distribute this software and its
  documentation strictly for non-commercial purposes is hereby granted 
  without fee, provided that the above copyright notice appears in all
  copies and that both the copyright notice and this permission notice
  appear in the supporting documentation. The authors make no claims 
  about the suitability of this software for any purpose. It is     
  provided "as is" without express or implied warranty.             
  
*/

/*
  \class StiKalmanTrackFinder  
  
  \author  Claude Pruneau, Wayne State University                        
  \date March 2001                                                    
  
  \note The Kalman Filter Code imbedded in this class was given
  to us gracioulsy by Jouri Belikov from the ALICE       
  collaboration. i.e. code reproduced with autorization. 
*/

//Sti
#include "StiDebug.h"
#include "StiHit.h"
#include "StiDetector.h"
#include "StiPlacement.h"
#include "StiShape.h"
#include "StiPlanarShape.h"
#include "StiDetectorContainer.h"
#include "StiTrackContainer.h"
#include "StiTrack.h"
#include "StiTrackFinder.h"
#include "StiKalmanTrack.h"
#include "StiTrackSeedFinder.h"
#include "StiEvaluableTrackSeedFinder.h"
#include "StiCompositeSeedFinder.h"
#include "StiTrackFilter.h"
#include "StiTrack.h"
#include "StiKalmanTrackFinder.h"
#include "StiKalmanTrackFitter.h"
#include "StiMaterialInteraction.h"

ostream& operator<<(ostream&, const StiTrack&);

StiKalmanTrackFinder::StiKalmanTrackFinder()
{
    if (StiDebug::isReq(StiDebug::Flow)) cout <<"StiKalmanTrackFinder::StiKalmanTrackFinder()"<<endl; 
    StiTrack::setTrackFitter(new StiKalmanTrackFitter());
    reset();
    StiDebug::debug=StiDebug::All;
}

StiKalmanTrackFinder::~StiKalmanTrackFinder()
{
    if (StiDebug::isReq(StiDebug::Flow)) cout <<"StiKalmanTrackFinder::~StiKalmanTrackFinder()"<<endl;
}

void StiKalmanTrackFinder::reset()
{
    if (StiDebug::isReq(StiDebug::Flow)) cout <<"StiKalmanTrackFinder::reset()"<<endl;
    singleNodeDescent    = true;
    singleNodeFrom       = 20;
    mcsCalculated        = false;
    elossCalculated      = false;
    maxChi2ForSelection  = 5.;
    minContiguousHitCountForNullReset = 2;
    maxNullCount = 40;  
    maxContiguousNullCount = 10;
}

bool StiKalmanTrackFinder::isValid(bool debug) const
{
    return StiTrackFinder::isValid(debug);
}

//Temporary patch, to test seed finder (MLM, 8/20/01)

bool StiKalmanTrackFinder::hasMore()
{
    return trackSeedFinder->hasMore();
}

void StiKalmanTrackFinder::doTrackFit()
{
    if (StiDebug::isReq(StiDebug::Flow)) cout <<"StiKalmanTrackFinder::doTrackFit()"<<endl;
    try 
	{
	    StiKalmanTrack* track = 0;
	    if (trackSeedFinder->hasMore())	
		{ //Redundant check, but it protectes against naive calls
		    track = trackSeedFinder->next();
		    if (!track) 	    
			{
			    cout <<"StiKalmanTrackFinder::doTrackFit()\t Track==0. Abort"
				 <<endl;
			    return;
			}
		    else 	    {
			if (StiDebug::isReq(StiDebug::Flow))
			    cout <<"StiKalmanTrackFinder::doTrackFit()\t Got Valid track"<<endl;
			track->fit();
			trackContainer->push_back(track);
			track->update();  //This updates the track on the display
			cout <<*track<<endl;
		    }
		}
	    else {
		if (StiDebug::isReq(StiDebug::Flow))
		    cout <<"\ttrackSeedFinder->hasMore()==false"<<endl;
	    }
	}
    catch (Exception & e) {
	cout << "StiKalmanTrackFinder::doTrackFit() - Exception: " << e << endl;
    }
}

void StiKalmanTrackFinder::doTrackFind()
{
    if (StiDebug::isReq(StiDebug::Flow)) cout <<"StiKalmanTrackFinder::doTrackFind()"<<endl;
    StiKalmanTrack* track = 0;
    if (trackSeedFinder->hasMore())	
	{ //Redundant check, but it protectes against naive calls
	    track = trackSeedFinder->next();
	    if (!track) 
		{
		    if (StiDebug::isReq(StiDebug::Flow))
			cout <<"StiKalmanTrackFinder::doTrackFind()\t Track==0. Abort"
			     <<endl;
		    return;
		}
	    else 
		{
		    if (StiDebug::isReq(StiDebug::Flow))
			cout <<"StiKalmanTrackFinder::doTrackFind()\t Got Valid track"<<endl;
		    findTrack(track);
		    if (StiDebug::isReq(StiDebug::Track))
			cout << " StiKalmanTrackFinder::doTrackFind() - Track Parameters" << endl
			     << *track;
		    trackContainer->push_back(track);
		    track->update();  //This updates the track on the display
		}
	}
    else 
	{
	    if (StiDebug::isReq(StiDebug::Flow))
		cout <<"\ttrackSeedFinder->hasMore()==false"<<endl;
	}
}

void StiKalmanTrackFinder::findTracks()
{
    //-----------------------------------------------------------------
    // Find all possible tracks in the given set of hits/points.
    // 
    // Note: The following objects must be set
    // trackSeedFinder  : a helper class object used to find track seeds
    // trackFilter      : a helper class object used to filter tracks 
    //                    before they are added to the track store.
    // trackContainer   : track container
    //-----------------------------------------------------------------
    
    StiTrack * t;
    
    while (trackSeedFinder->hasMore()){
	
	t = trackSeedFinder->next(); // obtain a pointer to the next track candidate/seed
	if (t!=0) { //check for null pointer
	    try {
		findTrack(t);
		if (trackFilter->accept(t)) 
		    trackContainer->push_back(t);
	    }
	    catch (Exception e) {
		cout << e << endl;
	    }
	} 
    }
}

void StiKalmanTrackFinder::findTrack(StiTrack * t) //throw ( Exception)
{
    //-----------------------------------------------------------------
    // Find extension (track) to the given track seed
    // Return Ok      if operation was successful
    // Return Error   if given seed "t" is invalid
    //                or if input data are invalid or if some other 
    //                internal error has occured.
    //-----------------------------------------------------------------
    if (StiDebug::isReq(StiDebug::Flow))
	cout << "StiKalmanTrackFinder::findTrack(StiTrack * t) - Beginning" << endl;
    StiKalmanTrack * tt = dynamic_cast<StiKalmanTrack *> (t);
    if (!tt) 
	{
	    cout <<"StiKalmanTrackFinder::findTrack(StiTrack * t)\t - ERROR - tt==0. Abort"
		 <<endl;
	    return;  
	}
    StiKalmanTrackNode * lastNode = tt->getLastNode();
    if (!lastNode) 
	{
	    cout <<"StiKalmanTrackFinder::findTrack(StiTrack * t)\t - ERROR - lastNode==0. Abort"
		 <<endl;
	    return;  
	}
    lastNode = followTrackAt(lastNode);
    pruneNodes(lastNode);
    tt->setLastNode(lastNode);
    tt->setChi2(lastNode->fChi2);
    if (lastNode->fP3>0)
	tt->setCharge(-StiKalmanTrackNode::unitCharge);
    else
	tt->setCharge(StiKalmanTrackNode::unitCharge);
    
    //extendToMainVertex(lastNode);
    if (StiDebug::isReq(StiDebug::Flow))
	cout <<"StiKalmanTrackFinder::findTrack(StiTrack * t)\t - Done" << endl;
}

StiKalmanTrackNode *
StiKalmanTrackFinder::followTrackAt(StiKalmanTrackNode * node)
    //throw (Exception)
{
    int       hitCount;
    int       contiguousHitCount;
    int       nullCount; 
    int       contiguousNullCount;	
    int       visitedDet ;
    
    //double    sAlpha, tAlpha;
    //double    yWindow,zWindow;
    double    chi2     = 0;
    double    bestChi2 = 0;
    StiKalmanTrackNode * sNode = node; // source node
    StiKalmanTrackNode * tNode = 0;    // target node
    StiKalmanTrackNode * bestNode = 0;  
    StiDetector * sDet=0;
    StiDetector * tDet=0;
    
    bool trackDone = false;
    bool scanningDone = false;
    hitCount = 0;
    nullCount = 0;
    contiguousNullCount = 0;
    contiguousHitCount  = 0;
    sDet  = sNode->getHit()->detector();
    if (sDet==0) {
	cout << "StiKalmanTrackFinder::followTrackAt(StiKalmanTrackNode * node) - ERROR - sDet==null" << endl;
	return 0;
    }
    if (detectorContainer==0) {
	cout << "StiKalmanTrackFinder::followTrackAt(StiKalmanTrackNode * node) - ERROR - detectorContainer==null" << endl;
	return 0;
    }
    detectorContainer->setToDetector(sDet);
    
    while (!trackDone) {// search track until done
	cout << "StiKalmanTrackFinder::followTrackAt(StiKalmanTrackNode * node) - INFO - Search Track" << endl;
	
	detectorContainer->moveIn();
	
	tDet = **detectorContainer;
	cout << *tDet;
	if (tDet==0) {
	    cout << "StiKalmanTrackFinder::followTrackAt(StiKalmanTrackNode * node) - ERROR - tDet==null" << endl;
	    return 0;
	}
	if (tDet==sDet)	{
	    cout << "StiKalmanTrackFinder::followTrackAt(StiKalmanTrackNode * node);- INFO - tDet==sDet"  << endl;
	    return sNode; // track cannot be prolongued any further
	}
	//sHit = sNode->getHit();
	//yWindow = getYWindow(sNode, sHit);
	//zWindow = getZWindow(sNode, sHit);
	
	scanningDone = false;
	bestChi2 = 1e50;
	visitedDet = 0;
	int position;
	// scan layer for connecting hits
	scanningDone = false;
	while (!scanningDone) {
	    cout << "StiKalmanTrackFinder::followTrackAt(StiKalmanTrackNode * node)";
	    cout <<"- INFO - Scanning" << endl;
	    visitedDet++;
	    if (visitedDet>4) {
		scanningDone = true;
	    }
	    else {
		tNode = trackNodeFactory->getObject();
		if (tNode==0) {
		    cout << "StiKalmanTrackFinder::followTrackAt(StiKalmanTrackNode * node)";
		    cout << "\t- ERROR - tNode==null" << endl;
		    return 0;
		}
		tNode->reset();
		cout << "sNode==========="<< *sNode;
		position = tNode->propagate(sNode, tDet); // 
		if (position==kFailed) {
		    if (StiDebug::isReq(StiDebug::Flow))
			cout << "StiKalmanTrackFinder::followTrackAt(StiKalmanTrackNode * node)"
			     << " - position==kFailed" << endl;
		    return sNode;
		}
		if (tDet->isActive()) { // active vol, look for hits
		    cout << "StiKalmanTrackFinder::followTrackAt(StiKalmanTrackNode * node)";
		    cout << "\t- tDet isActive()" << endl;
		    if (position<=kEdgeZplus) {
			cout << "StiKalmanTrackFinder::followTrackAt(StiKalmanTrackNode * node)";
			cout << "Near HIT " << endl;
			hitContainer->setDeltaD(5.); //yWindow);
			hitContainer->setDeltaZ(5.); //zWindow);
			cout << "tNode==========="<<*tNode;
			//void setRefPoint(double position, double refAngle, double y, double z);
			hitContainer->setRefPoint(tNode->fX,
						  tNode->fAlpha,
						  tNode->fP0,
						  tNode->fP1);
			while (hitContainer->hasMore())	{
			    cout << "StiKalmanTrackFinder::followTrackAt(StiKalmanTrackNode * node)"
				 << "\t- hitContainer->hasMore()" << endl;
			    tNode->setHit(hitContainer->getHit());
			    chi2 = tNode->evaluateChi2();
			    if (chi2<maxChi2ForSelection && chi2 < bestChi2) {
				cout << "StiKalmanTrackFinder::followTrackAt(StiKalmanTrackNode * node)"
				     << "\t chi2:" << chi2 << endl;
				bestChi2 = chi2;
				bestNode = tNode;
			    }
			} // searching best hit
			if (position==kHit)
			    scanningDone = true;
		    }
		    else
			{
			    cout << "StiKalmanTrackFinder::followTrackAt(StiKalmanTrackNode * node)"
				 << "MISSED DET" << endl;						
			}
		}
		else  // inactive, keep only if position==0
		    {
			cout << "StiKalmanTrackFinder::followTrackAt(StiKalmanTrackNode * node)"
			     << "\t- tDet is NOT Active()" << endl;
			if (position==kHit)
			    scanningDone = true;
		    }
	    }
	    if (!scanningDone)
		{
		    // try a different detector on the same layer
		    if (position==kEdgePhiPlus || position==kMissPhiPlus)
			detectorContainer->movePlusPhi();
		    else if (position==kEdgePhiMinus || position==kMissPhiMinus)
			detectorContainer->moveMinusPhi();
		    else
			scanningDone = true;
		}
	}
	
	if (bestChi2<maxChi2ForSelection && bestNode)
	    {
		cout << "StiKalmanTrackFinder::followTrackAt(StiKalmanTrackNode * node)"
		     << "-  Node selected for addition to track" << endl;
		// found hit(s), update track info, 
		// update counters.
		tNode->updateNode();
		sNode->add(tNode);
		sNode = tNode;  // the new source node
		hitCount++;
		contiguousHitCount++;
		if (contiguousHitCount>minContiguousHitCountForNullReset)
		    contiguousNullCount = 0;
	    }
	else  {
	    // update null counters
	    cout << "StiKalmanTrackFinder::followTrackAt(StiKalmanTrackNode * node)"
		 << "-  Node WAS NOT selected for addition to track" << endl;
	    cout << "bestChi2:" << bestChi2 << " while max for selection is ";
	    cout << maxChi2ForSelection << endl;
	    contiguousNullCount++;
	    nullCount++;
	    if (nullCount>maxNullCount ||
		contiguousNullCount>maxContiguousNullCount)
		trackDone = true;
	}
	
	
	/*      detectorContainer->moveIn();
		StiDetector * newDet = **detectorContainer;
		if (newDet==tDet)
		{
		// same det implies track is  done
		trackDone = true;
		}
	*/
	}
 return sNode;
}

//_____________________________________________________________________________
void StiKalmanTrackFinder::removeNodeFromTrack(StiKalmanTrackNode * node, StiKalmanTrack* track)
{
    // Remove given node from given track. 
    // not implemented
}

void StiKalmanTrackFinder::pruneNodes(StiKalmanTrackNode * node)
{
    // Prune unnecessary nodes on the track starting at given node. 
    // All siblings of the given node, are removed, and iteratively
    // all siblings of its parent are removed from the parent of the
    // parent, etc.
    if (StiDebug::isReq(StiDebug::Flow)) 
	cout <<"StiKalmanTrackFinder::pruneNodes(StiKalmanTrackNode * node) - Beginning"<<endl;
    
    
    
    StiKalmanTrackNode * parent = dynamic_cast<StiKalmanTrackNode *>(node->getParent());
    while (parent)
	{
	    if (StiDebug::isReq(StiDebug::Finding)) 
		cout << "StiKalmanTrackFinder::pruneNodes(StiKalmanTrackNode * node) -"
		     << "node has childCount:" << parent->getChildCount() << endl;
	    parent->removeAllChildrenBut(node);
	    node = parent;
	    parent = dynamic_cast<StiKalmanTrackNode *>(node->getParent());
	}
}


/*
  
double StiKalmanTrackFinder::getYWindow(StiKalmanTrackNode * n, StiHit * h) const 
{
double rv, sy2a, sy2b;
sy2a = n->fC00;  // syy of the track at this node
sy2b = h->syy(); // measured error of the hit at this node
rv = 4*sqrt(sy2a+sy2b);
if (rv<0.2)
rv = 0.2;
else if (rv>5.)
rv = 5.;
return rv;
}

double StiKalmanTrackFinder::getZWindow(StiKalmanTrackNode * n, StiHit * h) const 
{
  double rv, sz2a, sz2b;
  sz2a = n->fC11;  // szz of the track at this node
  sz2b = h->szz(); // measured error of the hit at this node
  rv = 4*sqrt(sz2a+sz2b);
  if (rv<0.2)
    rv = 0.2;
  else if (rv>5.)
    rv = 5.;
  return rv;
}
*/

void StiKalmanTrackFinder::setElossCalculated(bool option)
{
    elossCalculated = option;
    StiKalmanTrackNode::setElossCalculated(option);
}

void StiKalmanTrackFinder::setMCSCalculated(bool option)
{
    mcsCalculated = option;
    StiKalmanTrackNode::setMCSCalculated(option);
}

void   StiKalmanTrackFinder::setMassHypothesis(double m) 
{
    StiKalmanTrackNode::setMassHypothesis(m);
};

double StiKalmanTrackFinder::getMassHypothesis() 
{ 
    return StiKalmanTrackNode::getMassHypothesis();
};
