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

#include <stdexcept>
//Sti
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
    : StiTrackFinder()
{
    //progFlowMes = Messager::instance(kProgFlowMessage);
    
    cout << "StiKalmanTrackFinder::StiKalmanTrackFinder() - Begins"<<endl;
    
    Messenger& mes = *Messenger::instance(kTrackMessage);
    mes << "this is a dum test" <<endl;
    cout << "StiKalmanTrackFinder::StiKalmanTrackFinder() - Begins"<<endl;
    
    //progFlowMes <<"StiKalmanTrackFinder::StiKalmanTrackFinder() - Begin"<<endl; 
    StiTrack::setTrackFitter(new StiKalmanTrackFitter());
    reset();
    cout << "StiKalmanTrackFinder::StiKalmanTrackFinder() - Done"<<endl;
}

StiKalmanTrackFinder::~StiKalmanTrackFinder()
{
    //progFlowMes <<"StiKalmanTrackFinder::~StiKalmanTrackFinder() - Begin/End"<<endl;
}

void StiKalmanTrackFinder::reset()
{
    //progFlowMes <<"StiKalmanTrackFinder::reset()"<<endl;
    singleNodeDescent    = true;
    singleNodeFrom       = 20;
    mcsCalculated        = false;
    elossCalculated      = false;
    maxChi2ForSelection  = 15.;
    minContiguousHitCountForNullReset = 2;
    maxNullCount = 40;  
    maxContiguousNullCount = 15;
    track = 0;
    trackDone = true;
    scanningDone = true;
    state = 0;
    mode=StepByLayer;
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
    if (mode==StepByLayer)
	{
	    cout << "\nStepByLayer" << endl;
	    switch (state)
		{
		case 0: cout<< "InitTrackSearch()"<< endl;
		    doInitTrackSearch(); 
		    state++; 
		    break;
		case 1: 
		    doInitLayer();
		    doScanLayer();
		    doFinishLayer();
		    if (trackDone)
			{
			    doFinishTrackSearch(); 
			    state=0;
			}
		    break;
		}
	}
    else if (mode==StepByDetector)
	{
	    cout << "\nStepByDetector" << endl;
	    switch (state)
		{
		case 0: cout<< "InitTrackSearch()"<<endl;
		    doInitTrackSearch(); 
		    state++; 
		    break;
		case 1: cout<< "InitLayer()" << endl;
		    doInitLayer();
		    state++;
		    break;
		case 2: cout<< "doNextDetector()" << endl;
		    doNextDetector();
		    if (scanningDone)
			{
			    doFinishLayer();
			    state = 1;
			    if(trackDone)
				{
				    doFinishTrackSearch(); 
				    state=0;
				}
			    break;
			}
		}
	}
    cout << "STATE:" << state << endl;
    track->update();
}

void StiKalmanTrackFinder::doTrackFit()
{
    //progFlowMess <<"StiKalmanTrackFinder::doTrackFit()"<<endl;
    try 
	{
	    track = 0;
	    if (trackSeedFinder->hasMore())	
		{ //Redundant check, but it protectes against naive calls
		    track = trackSeedFinder->next();
		    if (!track) 	    
			{
			    cout <<"StiKalmanTrackFinder::doTrackFit()\t Track==0. Abort" <<endl;
			    return;
			}
		    else 	    
			{
			    cout <<"StiKalmanTrackFinder::doTrackFit()\t Got Valid track"<<endl;
			    track->fit();
			    trackContainer->push_back(track);
			    track->update();  //This updates the track on the display
			    cout << "track parameters:";
			    cout << *track<<endl;
			}
		}
	    else 
		{
		    cout <<"\ttrackSeedFinder->hasMore()==false"<<endl;
		}
	}
    catch (exception & e) {
	cout << "StiKalmanTrackFinder::doTrackFit() - Internal Error :" << e.what() << endl;
    }
}

void StiKalmanTrackFinder::doTrackFind()
{
    //progFlowMes <<"StiKalmanTrackFinder::doTrackFind()"<<endl;
    trackDone = true;
    scanningDone = true;
    state = 0;
    track = 0;
    if (trackSeedFinder->hasMore())	
	{ //Redundant check, but it protectes against naive calls
	    track = trackSeedFinder->next();
	    if (!track) 
		{
		    cout << "NO MORE TRACK SEEDS - EVENT COMPLETED" << endl;
		    return;
		}
	    //				throw logic_error("StiKalmanTrackFinder::doTrackFind()\t Track==0. No more track seeds available");
	    cout <<"StiKalmanTrackFinder::doTrackFind()\t Got Valid track"<<endl;
	    findTrack(track);
	    //cout << " StiKalmanTrackFinder::doTrackFind() - Track Parameters" << endl << *track;
	    trackContainer->push_back(track);
	    track->update();  //This updates the track on the display
	    trackDone = false;  // ready for a new track
	}
    else 
	{
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
    while (trackSeedFinder->hasMore())
	{	
	    t = trackSeedFinder->next(); // obtain a pointer to the next track candidate/seed
	    if (!t) 
		{
		    cout << "NO MORE TRACK SEEDS - EVENT COMPLETED" << endl;
		    return;
		}
	    //throw logic_error("StiKalmanTrackFinder::findTracks() - Error - trackSeedFinder->next() returned 0 while trackSeedFinder->hasMore() returned thrue");
	    findTrack(t);
	    if (trackFilter->accept(t)) 
		trackContainer->push_back(t);
	}
}

void StiKalmanTrackFinder::findTrack(StiTrack * t) 
{
    //-----------------------------------------------------------------
    // Find extension (track) to the given track seed
    // Return Ok      if operation was successful
    // Return Error   if given seed "t" is invalid
    //                or if input data are invalid or if some other 
    //                internal error has occured.
    //-----------------------------------------------------------------
    cout << "StiKalmanTrackFinder::findTrack(StiTrack * t) - Beginning" << endl;
    track = dynamic_cast<StiKalmanTrack *> (t);
    if (!track) 
	throw logic_error("StiKalmanTrackFinder::findTrack()\t - ERROR - dynamic_cast<StiKalmanTrack *>  returned 0");
    StiKalmanTrackNode * lastNode = track->getLastNode();
    lastNode = followTrackAt(lastNode);
    pruneNodes(lastNode);
    reserveHits(track->getFirstNode());
    track->setLastNode(lastNode);
    track->setChi2(lastNode->fChi2);
    if (lastNode->fP3*StiKalmanTrackNode::getFieldConstant()>0)
	track->setCharge(-1);
    else
	track->setCharge(1);
    //extendToMainVertex(lastNode);
    //progFlowkMes <<"StiKalmanTrackFinder::findTrack(StiTrack * t)\t - Done" << endl;
}

void StiKalmanTrackFinder::doInitTrackSearch()
{
    cout<<"StiKalmanTrackFinder::doInitTrackSearch() - called"<<endl;
    if (!trackDone) return;   // must finish 
    if (!scanningDone) return;
    cout<<"StiKalmanTrackFinder::doInitTrackSearch() - begins"<<endl;
    track = 0;
    if (trackSeedFinder->hasMore())	
	{ 
	    //Redundant check, but it protectes against naive calls
	    track = trackSeedFinder->next();
	    if (!track) 
		{
		    cout << "NO MORE TRACK SEEDS - EVENT COMPLETED" << endl;
		    return;
		}
	    //throw logic_error("StiKalmanTrackFinder::doInitTrackSearch() - Error - trackSeedFinder->next() returned 0 while trackSeedFinder->hasMore() returned thrue");
	    cout <<"StiKalmanTrackFinder::doTrackFind()\t Got Valid track"<<endl;
	    StiKalmanTrackNode * lastNode = track->getLastNode();
	    if (!lastNode) 
		throw logic_error("StiKalmanTrackFinder::findTrack()\t - ERROR - track->getLastNode() returned 0");
	    initSearch(lastNode);
	}
    else 
	{
	    cout <<"\ttrackSeedFinder->hasMore()==false"<<endl;
	}
}

void StiKalmanTrackFinder::doFinishTrackSearch()
{	
    cout<<"StiKalmanTrackFinder::doFinishTrackSearch() - called"<<endl;
    if (!trackDone) return;   // must finish 
    if (!scanningDone) return;
    cout<<"StiKalmanTrackFinder::doFinishTrackSearch() - begins"<<endl;
    trackContainer->push_back(track);
    track->update();  //This updates the track on the display
}

StiKalmanTrackNode * StiKalmanTrackFinder::followTrackAt(StiKalmanTrackNode * node)
    //throw (Exception)
{
    initSearch(node);
    search();
    return sNode;
}


void StiKalmanTrackFinder::initSearch(StiKalmanTrackNode * node)
{
    if (!trackDone) return;
    if (detectorContainer==0) 
	throw logic_error("StiKalmanTrackFinder::initSearch() - Error - detectorContainer==0");
    sNode = node; // source node
    tNode  = 0;    // target node
    leadDet = 0;
    trackDone = false;
    scanningDone = true;
    hitCount = 0;
    nullCount = 0;
    contiguousNullCount = 0;
    contiguousHitCount  = 0;
    sDet  = sNode->getHit()->detector();
    if (sDet==0) 
	throw logic_error("StiKalmanTrackFinder::initSearch() - Error - sDet==0");
    tDet=0;
    leadDet = sDet;
    printState();
}

void StiKalmanTrackFinder::search()
{
    while (!trackDone) 
	{
	    doInitLayer(); 
	    doScanLayer();
	    doFinishLayer();
	}
    printState();
}

void StiKalmanTrackFinder::doInitLayer()
{
    cout << "StiKalmanTrackFinder::doInitLayer - called" << endl;
    if (trackDone) return;    // nothing to do
    if (!scanningDone) return; // nothing to do
    cout << "StiKalmanTrackFinder::doInitLayer - begins" << endl;
    detectorContainer->setToDetector(leadDet);
    StiDetector * currentDet = **detectorContainer;
    detectorContainer->moveIn();
    tDet = **detectorContainer;
    leadDet = tDet;
    cout << "TDET:" << *tDet<<endl;
    if (tDet==0) {
	throw logic_error("StiKalmanTrackFinder::doInitLayer() ERROR - tDet==0");
    }
    else if (tDet==currentDet)	
	{
	    cout << "StiKalmanTrackFinder::doInitLayer()";
	    cout <<"- TrackDone==true - moveIn >> tDet==sDet"  << endl;
	    trackDone = true;
	    return;
	}
    //sHit = sNode->getHit();	//yWindow = getYWindow(sNode, sHit);	//zWindow = getZWindow(sNode, sHit);
    position    = 0;
    lastMove  = 0;
    hasDet = false;
    hasHit = false;
    scanningDone = false;
    bestChi2  = 1e50;
    bestNode = 0;  
    printState();
}

void StiKalmanTrackFinder::doScanLayer()
{
    cout << " StiKalmanTrackFinder::doScanLayer() called" << endl;
    if (trackDone) return;
    cout << " StiKalmanTrackFinder::doScanLayer() begins" << endl;
    while (!scanningDone)
	{
	    doNextDetector();
	}
    printState();
}

void StiKalmanTrackFinder::doNextDetector()
{
    cout << "SKTF::doNextDetector()\t- Called" << endl;
    if (trackDone) return;
    if (scanningDone) return;
    cout << "SKTF::doNextDetector()\t- Begins" << endl;
    tNode = trackNodeFactory->getObject();
    if (tNode==0) 
	throw logic_error("SKTF::followTrackAt()\t- ERROR - tNode==null");
    tNode->reset();			
    cout << "ParentNode:"<<*sNode<<endl;
    position = tNode->propagate(sNode, tDet); 
    cout << "TargetNode:"<<*tNode<<endl;
    cout << "TargetDet :"<<*tDet<<endl;
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
			cout<< "SKTF::followTrackAt()\t- Detector has _NO_ hits" << endl;
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
	    cout << "SKTF::followTrackAt()\t- tDet is NOT Active() - Position:" << position;
	    if (position<=kEdgeZplus)
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
    printState();
}

void StiKalmanTrackFinder::printState()
{
    cout << "State:"<<state;
    if (scanningDone) 
	cout << "/Scanning Done";
    else
	cout << "/Scanning NOT Done";
    if (trackDone) 
	cout << "/Track Done"<<endl;
    else
	cout << "/Track NOT Done" << endl;
}

//_____________________________________________________________________________
void StiKalmanTrackFinder::doFinishLayer()
{
    if (trackDone) return;         // don't do this when the track is done
    if (!scanningDone) return;  // don't do this until scanning of layer is completed. 
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
    printState();
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
