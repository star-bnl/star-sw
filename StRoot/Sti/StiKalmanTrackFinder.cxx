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
    : StiTrackFinder(), mCurrentTrack(0), mCurrentNode(0)
{
    if (StiDebug::isReq(StiDebug::Flow))
	cout <<"StiKalmanTrackFinder::StiKalmanTrackFinder()"<<endl; 
    StiTrack::setTrackFitter(new StiKalmanTrackFitter());
    reset();
    StiDebug::debug=0;
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
    maxChi2ForSelection  = 15.;
    minContiguousHitCountForNullReset = 2;
    maxNullCount = 40;  
    maxContiguousNullCount = 15;
    mCurrentTrack=0;
    mCurrentNode=0;
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

void StiKalmanTrackFinder::doNextTrackStep()
{
    if ( mCurrentTrack==0 || mCurrentNode==0) {
	cout <<"StiKalmanTrackFinder::doNextTrackStep(). ERROR:\t";
	cout <<"mCurrentTrack==0 || mCurrentNode==0.  return."<<endl;
	return;
    }
    
    //Swap the following line for a method that actually exists
    if (false) {
	//if (mCurrentTrack->complete()==false) {
	
	//Call some function to do the next step with the current track
	// and the current node.
	
	mCurrentTrack->update();
    }
    
    else if (trackSeedFinder->hasMore()) {
	
	//Serve a new track
	mCurrentTrack = trackSeedFinder->next();
	
	if (!mCurrentTrack) {
	    cout <<"StiKalmanTrackFinder::doNextTrackStep()\t Track==0. Abort"<<endl;
	    return;
	}
	
	cout <<"StiKalmanTrackFinder::doTrackFit()\t Got Valid track"<<endl;
	
	//Call some function to do the next step with the current track
	//and the current node.
	
	mCurrentTrack->update();
    }
    
    else {
	cout <<"StiKalmanTrackFinder::doNextAction(). Event is finished. return"<<endl;
    }
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
    reserveHits(tt->getFirstNode());
    tt->setLastNode(lastNode);
    tt->setChi2(lastNode->fChi2);
    if (lastNode->fP3*StiKalmanTrackNode::getFieldConstant()>0)
	tt->setCharge(-1);
    else
	tt->setCharge(1);
    //extendToMainVertex(lastNode);
    if (StiDebug::isReq(StiDebug::Flow))
	cout <<"StiKalmanTrackFinder::findTrack(StiTrack * t)\t - Done" << endl;
}

StiKalmanTrackNode * StiKalmanTrackFinder::followTrackAt(StiKalmanTrackNode * node)
    //throw (Exception)
{
    if (0!=initSearch(node)) return sNode;
    search();
    return sNode;
}

int StiKalmanTrackFinder::initSearch(StiKalmanTrackNode * node)
{
    if (detectorContainer==0) 
	{
	    cout << "SKTF::followTrackAt(StiKalmanTrackNode * node) - ERROR - detectorContainer==null" << endl;
	    return -1;
	}
    bestNode = 0;  
    bestChi2 = 0;
    sNode = node; // source node
    tNode  = 0;    // target node
    leadDet = 0;
    trackDone = false;
    hitCount = 0;
    nullCount = 0;
    contiguousNullCount = 0;
    contiguousHitCount  = 0;
    sDet  = sNode->getHit()->detector();
    if (sDet==0) {
	cout << "StiKalmanTrackFinder::followTrackAt(StiKalmanTrackNode * node) - ERROR - sDet==null" << endl;
	return 0;
    }
    tDet=0;
    leadDet = sDet;
    return 0;
}

void StiKalmanTrackFinder::search()
{
    while (!trackDone) 
	{
	    initLayer(); 
	    scanLayer();
	    finishLayer();
	}
}

void StiKalmanTrackFinder::initLayer()
{
    cout << "InitLayer" << endl;
    if (trackDone) return;
    detectorContainer->setToDetector(leadDet);
    detectorContainer->moveIn();
    tDet = **detectorContainer;
    leadDet = tDet;
    cout << "TDET:" << *tDet;
    if (tDet==0) 
	{
	    cout << "StiKalmanTrackFinder::followTrackAt(StiKalmanTrackNode * node) - ERROR - tDet==null" << endl;
	    trackDone = true;
	    return;
	}
    else if (tDet==sDet)	
	{
	    cout << "StiKalmanTrackFinder::followTrackAt(StiKalmanTrackNode * node) - INFO - moveIn >> tDet==sDet"  << endl;
	    trackDone = true;
	    return;
	}
    //sHit = sNode->getHit();	//yWindow = getYWindow(sNode, sHit);	//zWindow = getZWindow(sNode, sHit);
    scanningDone = false;
    bestChi2  = 1e50;
    position    = 0;
    lastMove  = 0;
}

void StiKalmanTrackFinder::scanLayer()
{
    if (trackDone) return;
    hasDet = false;
    hasHit = false;
    bestNode = 0;
    while (!scanningDone)
	{
	    cout << "SKTF::followTrackAt()\t- INFO - Scanning" << endl;
	    tNode = trackNodeFactory->getObject();
	    if (tNode==0) {cout << "SKTF::followTrackAt()\t- ERROR - tNode==null" << endl;return;}
	    tNode->reset();			
	    cout << *sNode;
	    position = tNode->propagate(sNode, tDet); 
	    if (position==kFailed) {cout << "SKTF::followTrackAt()\t - position==kFailed" << endl;return;}
	    if (tDet->isActive()) 
		{ // active vol, look for hits
		    cout << "SKTF::followTrackAt()\t- tDet isActive() - Position:" << position << endl;
		    if (position<=kEdgeZplus) 
			{
			    hasDet = true;
			    leadNode = tNode;
			    leadNode->setDetector(tDet);
			    //if (position==kHit)
			    scanningDone = true;
			    
			    hitContainer->setDeltaD(5.); //yWindow);
			    hitContainer->setDeltaZ(5.); //zWindow);
			    //void setRefPoint(double position, double refAngle, double y, double z);
			    hitContainer->setRefPoint(tNode->fX,tNode->fAlpha,tNode->fP0,tNode->fP1);
			    if (hitContainer->hasMore())
				cout << "SKTF::followTrackAt()\t- Detector has hits" << endl;
			    else
				cout << "SKTF::followTrackAt()\t- Detector has _NO_ hits" << endl;
			    while (hitContainer->hasMore())	
				{
				    cout << "SKTF::followTrackAt()\t- hitContainer->hasMore()" << endl;
				    tNode->setHit(hitContainer->getHit());
				    chi2 = tNode->evaluateChi2();
				    cout << "SKTF::followTrackAt()\t chi2:" << chi2 << endl;
				    if (chi2<maxChi2ForSelection && chi2 < bestChi2) 
					{
					    
					    cout << "SKTF::followTrackAt()\t chi2:" << chi2 << endl;
					    hasHit = true;
					    bestChi2 = chi2;
					    bestNode = tNode;
					}
				} // searching best hit
			}
		    else
			{
			    cout << "SKTF::followTrackAt()\t- MISSED DET" << endl;						
			}
		}
	    else  // inactive, keep only if position==0
		{
		    cout << "SKTF::followTrackAt()\t- tDet is NOT Active()";
		    if (position==kEdgeZplus)
			{
			    hasDet = true;
			    cout << " but was a hit" << endl;
			    scanningDone = true;	
			    leadNode = tNode;
			    leadNode->setDetector(tDet);
			}
		    else
			{
			    cout << " and was a miss" << endl;
			}
		}
	    if (!scanningDone)
		{
		    StiDetector * nextDet;
		    // try a different detector on the same layer
		    if (position==kEdgePhiPlus || position==kMissPhiPlus)
			{
			    if (lastMove>=0 )
				{
				    cout << "SKTF::followTrackAt()\t- movePlusPhi()" << endl;
				    detectorContainer->movePlusPhi();			
				    nextDet = **detectorContainer;
				    if (tDet==nextDet)	
					{
					    cout << "StiKalmanTrackFinder::followTrackAt(StiKalmanTrackNode * node) - ERROR - movePlusPhi() >> tDet==sDet"  << endl;
					    scanningDone = true;
					}
				    tDet = nextDet;
				    lastMove++;
				}
			    else
				{
				    scanningDone = true;
				    cout << "SKTF::followTrackAt()\t-position==kEdgePhiPlus||kMissPhiPlus - but no PlusPhi done" << endl;
				}
			}
		    else if (position==kEdgePhiMinus || position==kMissPhiMinus)
			{
			    if (lastMove<=0)
				{
				    cout << "SKTF::followTrackAt()\t- moveMinusPhi()" << endl;
				    detectorContainer->moveMinusPhi();
				    nextDet = **detectorContainer;
				    if (tDet==nextDet)	
					{
					    cout << "StiKalmanTrackFinder::followTrackAt(StiKalmanTrackNode * node) - ERROR  -  moveMinusPhi() >> tDet==sDet"  << endl;
					    scanningDone = true;
					}
				    tDet = nextDet;
				    lastMove--;
				}
			    else
				{
				    scanningDone = true;
				    cout << "SKTF::followTrackAt()\t-position==kEdgePhiMinus||kMissPhiMinus - but no MinusPhi done" << endl;
				}
			}
		    else
			{
			    cout <<  "SKTF::followTrackAt()\t- Scanning set to done" << endl;
			    scanningDone = true;
			}
		}
	    if (abs(lastMove)>4) scanningDone = true;
	}
}

void StiKalmanTrackFinder::finishLayer()
{
    if (trackDone) return;
    if (hasDet)
	{
	    if (hasHit)
		{	
		    bestNode->updateNode();
		    sNode->add(bestNode);
		    sNode = bestNode;  
		    leadDet = bestNode->getDetector();
		    hitCount++; contiguousHitCount++;
		    if (contiguousHitCount>minContiguousHitCountForNullReset)
			contiguousNullCount = 0;
		}
	    else // no hit found
		{
		    contiguousNullCount++; nullCount++;					
		    sNode->add(leadNode);
		    sNode = leadNode;  
		    leadDet = leadNode->getDetector();
		    if (nullCount>maxNullCount ||contiguousNullCount>maxContiguousNullCount)
			trackDone = true;				
		}
	}
    else // no det crossing found
	{	
	    contiguousNullCount++;			nullCount++;
	    if (nullCount>maxNullCount ||contiguousNullCount>maxContiguousNullCount)
		trackDone = true;
	}
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
	    //if (StiDebug::isReq(StiDebug::Finding)) 
	    //cout << "StiKalmanTrackFinder::pruneNodes(StiKalmanTrackNode * node) -"
	    //			 << "node has childCount:" << parent->getChildCount() << endl;
	    parent->removeAllChildrenBut(node);
	    node = parent;
	    parent = dynamic_cast<StiKalmanTrackNode *>(node->getParent());
	}
}

void StiKalmanTrackFinder::reserveHits(StiKalmanTrackNode * node)
{
    // Declare hits on the track ending at "node"
    // as used. This method starts with the last node and seeks the
    // parent of each node recursively. The hit associated with each
    // (when there is a hit) is set to "used".
    
    if (StiDebug::isReq(StiDebug::Flow)) 
	cout <<"StiKalmanTrackFinder::reserveHits(StiKalmanTrackNode * node) - Beginning"<<endl;
    
    StiHit * hit;
    StiKalmanTrackNode * parent = dynamic_cast<StiKalmanTrackNode *>(node->getParent());
    while (parent)
	{
	    hit = parent->getHit();
	    if (hit!=0)
		hit->setUsed(true);
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
