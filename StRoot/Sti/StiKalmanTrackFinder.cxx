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

//Std
#include <iostream.h>
#include <stdexcept>
#include <math.h>
using namespace std;

//Sti
#include "StiKTNVertIterator.h"
#include "StiIOBroker.h"
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
    : StiTrackFinder(),trackMes(*Messenger::instance(MessageType::kTrackMessage)),
      mSubject(StiIOBroker::instance())
    
{
    //Turn off by default
    Messenger::instance()->clearRoutingBits(MessageType::kTrackMessage);
    
    cout << "StiKalmanTrackFinder::StiKalmanTrackFinder() - Begins"<<endl;
    StiTrack::setTrackFitter(new StiKalmanTrackFitter());
    reset();
    
    mSubject->attach(this);
    getNewState();
    
    cout << "StiKalmanTrackFinder::StiKalmanTrackFinder() - Done"<<endl;
}

StiKalmanTrackFinder::~StiKalmanTrackFinder()
{
    //progFlowMes <<"StiKalmanTrackFinder::~StiKalmanTrackFinder() - Begin/End"<<endl;
    if (mSubject) {
	mSubject->detach(this);
    }
}

void StiKalmanTrackFinder::getNewState()
{
    StiIOBroker *  broker = StiIOBroker::instance();
    //cout <<"StiKalmanTrackFinder::getNewState()"<<endl;
    setMCSCalculated(broker->ktfMcsCalculated());
    setElossCalculated(broker->ktfElossCalculated());
    setMaxChi2ForSelection(broker->ktfMaxChi2ForSelection());
    setField(broker->ktfBField());
    setMassHypothesis(broker->ktfMassHypothesis());
    setMinContiguousHitCount(broker->ktfMinContiguousHitCount());
    setMaxNullCount(broker->ktfMaxNullCount());
    setMaxContiguousNullCount(broker->ktfMaxContiguousNullCount());
    //setMinSearchRadius(broker->ktfMinSearchRadius());
    //setMaxSearchRadius(broker->ktfMaxSearchRadius());
    if (broker->ktfUseHelixExtrapolation()==true) 
	{
	    //cout <<"Using helix extrapolation"<<endl;
	    StiMaterialInteraction::setExtrapolationType(kHelixExtrapolation);
	}
    else 
	{
	    StiMaterialInteraction::setExtrapolationType(kLinearExtrapolation);
	}
}

void StiKalmanTrackFinder::reset()
{
    //progFlowMes <<"StiKalmanTrackFinder::reset()"<<endl;
    singleNodeDescent    = true;
    singleNodeFrom       = 20;
    mcsCalculated        = false;
    elossCalculated      = false;
    maxChi2ForSelection  = 50.;
    
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
    try
	{
	    if (mode==StepByLayer)
		{
#ifdef DEBUG
		    trackMes << "StepByLayer" << endl;
#endif
		    switch (state)
			{
			case 0:
#ifdef DEBUG
			    trackMes<< "InitTrackSearch()"<< endl;
#endif
			    
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
		    
#ifdef DEBUG
		    trackMes << "StepByDetector" << endl;
#endif
		    
		    switch (state)
			{
			case 0:
#ifdef DEBUG
			    trackMes<< "InitTrackSearch()"<<endl;
#endif
			    doInitTrackSearch(); 
			    state++; 
			    break;
			case 1:
#ifdef DEBUG
			    trackMes<< "InitLayer()" << endl;
#endif
			    doInitLayer();
			    state++;
			    break;
			case 2:
#ifdef DEBUG
			    trackMes<< "doNextDetector()" << endl;
#endif
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
#ifdef DEBUG
	    trackMes << "STATE:" << state << endl;
#endif
	    track->update();
	}
    catch (runtime_error & rte)
	{
#ifdef DEBUG
	    cout << "RunTime Error Exception: " << rte.what();
#endif
	}
    catch (exception & e)
	{
#ifdef DEBUG
	    cout << "Exception: " << e.what();
#endif
	}
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
#ifdef DEBUG
			    trackMes <<"StiKalmanTrackFinder::doTrackFit()\t Track==0. Abort" <<endl;
#endif
			    return;
			}
		    else 	    
			{
#ifdef DEBUG
			    trackMes <<"StiKalmanTrackFinder::doTrackFit()\t Got Valid track"<<endl;
#endif
			    track->fit();
			    trackContainer->push_back(track);
			    track->update();  //This updates the track on the display
#ifdef DEBUG
			    trackMes << "track parameters:";
			    trackMes << *track<<endl;
#endif
			}
		}
	    else 
		{
#ifdef DEBUG
		    trackMes <<"\ttrackSeedFinder->hasMore()==false"<<endl;
#endif
		}
	}
    catch (exception & e) {
#ifdef DEBUG
	trackMes << "StiKalmanTrackFinder::doTrackFit() - Internal Error :" << e.what() << endl;
#endif
    }
}

void StiKalmanTrackFinder::doTrackFind()
{
    trackDone = true;
    scanningDone = true;
    state = 0;
    track = 0;
    if (trackSeedFinder->hasMore())	
	{ //Redundant check, but it protectes against naive calls
	    track = trackSeedFinder->next();
	    if (!track) 
		{
#ifdef DEBUG
		    trackMes << "\tNO MORE TRACK SEEDS - EVENT COMPLETED" << endl;
#endif
		    return;
		}
#ifdef DEBUG
	    trackMes <<"StiKalmanTrackFinder::doTrackFind()\t Got Valid track"<<endl;
#endif

	    findTrack(track);
#ifdef DEBUG
	    trackMes << " SKTFinder::doTrackFind() - Track Parameters" << endl << *track;
#endif
	    reserveHits(track->getLastNode());
            trackContainer->push_back(track);
            track->update();  //This updates the track on the display
	    trackDone = false;  // ready for a new track
	}
    else 
	{
#ifdef DEBUG
	    trackMes << "\tNO MORE TRACK SEEDS - EVENT COMPLETED" << endl;
#endif
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
#ifdef DEBUG
		    trackMes << "NO MORE TRACK SEEDS - EVENT COMPLETED" << endl;
#endif
		    return;
		}
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
    //trackMes << "SKTF::findTrack(StiTrack * t) - Beginning" << endl;
    track = dynamic_cast<StiKalmanTrack *> (t);
    if (!track) 
	{
	    throw logic_error("SKTF::findTrack()\t - ERROR - dynamic_cast<StiKalmanTrack *>  returned 0");
	}
    StiKalmanTrackNode * lastNode = track->getLastNode();
    try
	{
	    lastNode = followTrackAt(lastNode);
	}
    catch (exception & e)
	{
#ifdef DEBUG
	    cout << "SKTF::findTrack() exception: " << e.what();
#endif
	}
    pruneNodes(lastNode);
    reserveHits(track->getLastNode());
    track->setLastNode(lastNode);
    track->setChi2(lastNode->fChi2);
    using namespace std;
    if (lastNode->fP3*StiKalmanTrackNode::getFieldConstant()>0)
	track->setCharge(-1);
    else
	track->setCharge(1);
    //extendToMainVertex(lastNode);
}

void StiKalmanTrackFinder::doInitTrackSearch()
{
    //    trackMes<<"SKTF::doInitTrackSearch() - called"<<endl;
    if (!trackDone) return;   // must finish 
    if (!scanningDone) return;
    //    trackMes<<"SKTF::doInitTrackSearch() - begins"<<endl;
    track = 0;
    if (trackSeedFinder->hasMore())	
	{ 
	    //Redundant check, but it protectes against naive calls
	    track = trackSeedFinder->next();
	    if (!track) 
		{
#ifdef DEBUG
		    trackMes << "NO MORE TRACK SEEDS - EVENT COMPLETED" << endl;
#endif
		    return;
		}
	    //throw logic_error("SKTF::doInitTrackSearch() - Error - trackSeedFinder->next() returned 0 while trackSeedFinder->hasMore() returned thrue");
	    //trackMes <<"SKTF::doTrackFind()\t Got Valid track"<<endl;
	    StiKalmanTrackNode * lastNode = track->getLastNode();
	    if (!lastNode) 
		throw logic_error("SKTF::findTrack()\t - ERROR - track->getLastNode() returned 0");
	    initSearch(lastNode);
	}
    else 
	{
#ifdef DEBUG
	    trackMes <<"\ttrackSeedFinder->hasMore()==false"<<endl;
#endif
	}
}

void StiKalmanTrackFinder::doFinishTrackSearch()
{	
    //trackMes<<"SKTF::doFinishTrackSearch() - called"<<endl;
    if (!trackDone) return;   // must finish 
    if (!scanningDone) return;
    //trackMes<<"SKTF::doFinishTrackSearch() - begins"<<endl;
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
	throw logic_error("SKTF::initSearch() - Error - detectorContainer==0");
    sNode = node; // source node
    tNode  = 0;    // target node
    leadDet = 0;
    trackDone = false;
    scanningDone = true;
    sDet  = sNode->getHit()->detector();
    if (sDet==0) 
	throw logic_error("SKTF::initSearch() - Error - sDet==0");
    tDet=0;
    leadDet = sDet;
    //    printState();
}

void StiKalmanTrackFinder::search()
{
    while (!trackDone) 
	{
	    doInitLayer(); 
	    doScanLayer();
	    doFinishLayer();
	}
}

void StiKalmanTrackFinder::doInitLayer()
{
    //trackMes << "SKTF::doInitLayer - called" << endl;
    if (trackDone) return;    // nothing to do
    if (!scanningDone) return; // nothing to do
    //trackMes << "SKTF::doInitLayer - begins" << endl;
    detectorContainer->setToDetector(leadDet);
    StiDetector * currentDet = **detectorContainer;
    detectorContainer->moveIn();
    tDet = **detectorContainer;
    leadDet = tDet;
    //trackMes << "TDET:" << *tDet<<endl;
    if (tDet==0) 
	throw logic_error("SKTF::doInitLayer() ERROR - tDet==0");
    else if (tDet==currentDet)	
	{
	    //trackMes << "SKTF::doInitLayer() - TrackDone==true - moveIn >> tDet==sDet"  << endl;
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
    //printState();
}

void StiKalmanTrackFinder::doScanLayer()
{
    //trackMes << " StiKalmanTrackFinder::doScanLayer() called" << endl;
    if (trackDone) return;
    //trackMes << " StiKalmanTrackFinder::doScanLayer() begins" << endl;
    while (!scanningDone)
	{
	    doNextDetector();
	}
}

void StiKalmanTrackFinder::doNextDetector()
{
    //trackMes << "SKTF::doNextDetector()\t- Called" << endl;
    if (trackDone) return;
    if (scanningDone) return;
    //trackMes << "SKTF::doNextDetector()\t- Begins" << endl;
    tNode = trackNodeFactory->getObject();
    if (tNode==0) 
	throw logic_error("SKTF::doNextDetector()\t- ERROR - tNode==null");
    tNode->reset();
    try
	{
	    position = tNode->propagate(sNode, tDet); 
	}
    catch (runtime_error & rte)
	{
#ifdef DEBUG
	    trackMes << "SKTF::doNextDetector() - RunTimeError: " << rte.what();
#endif
	    scanningDone = true;
	    trackDone = true;
	    return;
	}
#ifdef DEBUG
    trackMes << "TargetDet :"<<*tDet<<endl;
    trackMes << "TargetNode:"<<*tNode<<endl;
#endif
    
    if (position==kFailed) 
	{
#ifdef DEBUG
	    trackMes << "SKTF::doNextDetector()\t - position==kFailed" << endl;
#endif
	    scanningDone = true;
	    trackDone = true;
			return;
	    //throw runtime_error("SKTF::doNextDetector()\t- RunTimeError - newNode==null");
		}
	if (tDet->isActive()) 
		{ // active vol, look for hits
	    //trackMes << "SKTF::followTrackAt()\t- tDet isActive() - Position:" << position << endl;
	    if (position<=kEdgeZplus) 
		{
		    hasDet = true;
		    leadNode = tNode;
		    //trackMes << "===============================leadNode   is at:" << leadNode << endl;
		    //leadNode->setHit(0); // insures no hit is here...
		    leadNode->setDetector(tDet);
		    if (position==kHit)
			scanningDone = true;
		    tNode->propagateError();
		    hitContainer->setDeltaD(10.); //yWindow);
		    hitContainer->setDeltaZ(10.); //zWindow);
		    //void setRefPoint(double position, double refAngle, double y, double z);
		    //double a = tNode->fAlpha;
		    double a = tDet->getPlacement()->getCenterRefAngle();
		    //trackMes << "Ref Angle:" << (a*180./M_PI) << endl;
		    
		    //if (a<0) a+=2.*M_PI; 
		    
		    //trackMes << "Corrected Ref Angle:" << (a*180./M_PI) << endl;
		    
		    hitContainer->setRefPoint(tNode->fX,a,tNode->fP0,tNode->fP1);
		    while (hitContainer->hasMore())	
			{
#ifdef DEBUG
			    trackMes << "SKTF::followTrackAt()\t- hitContainer->hasMore()" << endl;
#endif
			    tNode->setHit(hitContainer->getHit());
			    chi2 = tNode->evaluateChi2();
#ifdef DEBUG
			    trackMes << "SKTF::followTrackAt()\t chi2:" << chi2  << endl;
#endif
			    if (chi2<maxChi2ForSelection && chi2 < bestChi2)
				{
#ifdef DEBUG
				    trackMes << "SKTF::followTrackAt()\t selected hit - chi2:" << chi2 << endl;
#endif
				    hasHit = true;
				    bestChi2 = chi2;
				    bestNode = tNode;
				}
			    if (hitContainer->hasMore()) // prepare new node
				{
				    StiKalmanTrackNode * newNode = trackNodeFactory->getObject();
				    if (newNode==0) 
					throw logic_error("SKTF::followTrackAt()\t- ERROR - newNode==null");
				    newNode->reset();			
				    newNode->setState(tNode); // get everything from tNode
				    newNode->setDetector(tDet); // set the local pointer to tDet
				    tNode = newNode;  // not a memory leak because the factory handles the objects.... ;-)
				}
			} // searching best hit
		}
	    //else
	    //{
	    //trackMes << "SKTF::followTrackAt()\t- MISSED DET" << endl;						
	    //}
	}
    else  // inactive, keep only if position==0
	{
#ifdef DEBUG
	    trackMes << "SKTF::followTrackAt()\t- tDet is NOT Active() - Position:" << position;
#endif
	    if (position<=kEdgeZplus)
		{
		    hasDet = true;
#ifdef DEBUG
		    trackMes << " but was a hit" << endl;
#endif
		    scanningDone = true;	
		    tNode->propagateError();
		    leadNode = tNode;
		    leadNode->setDetector(tDet);
		}
	    //else
	    //	{
	    //		trackMes << " and was a miss" << endl;
	    //	}
	}
    if (!scanningDone)
	{
	    StiDetector * nextDet;
	    if (position==kEdgePhiPlus || position==kMissPhiPlus)
		{
		    if (lastMove>=0 )
			{
#ifdef DEBUG
			    trackMes << "SKTF::followTrackAt()\t- movePlusPhi()" << endl;
#endif
			    detectorContainer->movePlusPhi();			
			    nextDet = **detectorContainer;
			    if (nextDet==0 || tDet==nextDet)	
				{
				    //trackMes << "SKTF::followTrackAt) - ERROR - movePlusPhi() >> tDet==sDet"  << endl;
				    scanningDone = true;
				}
			    tDet = nextDet;
			    lastMove++;
			}
		    else
			{
			    scanningDone = true;
#ifdef DEBUG
			    trackMes << "SKTF::followTrackAt()\t-position==kEdgePhiPlus||kMissPhiPlus - but no PlusPhi done" << endl;
#endif
			}
		    
		}
	    else if (position==kEdgePhiMinus || position==kMissPhiMinus)
		{
		    if (lastMove<=0)
			{
#ifdef DEBUG
			    trackMes << "SKTF::followTrackAt()\t- moveMinusPhi()" << endl;
#endif
			    detectorContainer->moveMinusPhi();
			    nextDet = **detectorContainer;
			    if (nextDet==0 || tDet==nextDet)	
				{
				    //trackMes << "SKTF::followTrackAt() - ERROR  -  moveMinusPhi() >> tDet==sDet"  << endl;
				    scanningDone = true;
				}
			    tDet = nextDet;
			    lastMove--;
			}
		    else
			{
			    scanningDone = true;
#ifdef DEBUG
			    trackMes << "SKTF::followTrackAt()\t-position==kEdgePhiMinus||kMissPhiMinus - but no MinusPhi done" << endl;
#endif
			}
		}
	    else
		{
#ifdef DEBUG
		    trackMes <<  "SKTF::followTrackAt()\t- Scanning set to done" << endl;
#endif
		    scanningDone = true;
		}
	}
    if (abs(lastMove)>2) scanningDone = true;
    //printState();
}

void StiKalmanTrackFinder::printState()
{
#ifdef DEBUG
    trackMes << "State:"<<state;
    if (scanningDone) 
	trackMes << "/Scanning Done";
    else
	trackMes << "/Scanning NOT Done";
    if (trackDone) 
	trackMes << "/Track Done"<<endl;
    else
	trackMes << "/Track NOT Done" << endl;
#endif
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
		    //trackMes << "SKTF::doFinishLayer() - Adding node with hit in det:" 
		    //<< *leadDet << endl;
		}
	    else // no hit found
		{
		    //trackMes << "===============================leadNode   is at:" 
		    //<< leadNode << endl;
		    leadDet = leadNode->getDetector();
		    if (leadDet==0)
			trackMes << "SKTF::doFinishLayer() - Fatal Error - leadDet==0" << endl;
		    //else
		    //trackMes << "SKTF::doFinishLayer() - Adding node WITHOUT hit in det:"  
		    //<< *leadDet << endl;
		    sNode->add(leadNode);
		    sNode = leadNode;
#ifdef DEBUG
		    trackMes << "SKTF::doFinishLayer() "
			     << "               nullCount:" << leadNode->nullCount << endl
			     << "    contiguousNullCount :" << leadNode->contiguousNullCount << endl;
#endif
		    if (leadNode->nullCount>StiKalmanTrackNode::maxNullCount ||
			leadNode->contiguousNullCount>StiKalmanTrackNode::maxContiguousNullCount)
			trackDone = true;				
		}
	}
    else // no det crossing found
	{
#ifdef DEBUG
	    trackMes << "SKTF::doFinishLayer() - Layer Completed with no node added" << endl;
#endif
	    //if (nullCount>maxNullCount ||contiguousNullCount>maxContiguousNullCount)
	    //	trackDone = true;
	}
    //printState();
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
    //trackMes <<"SKTF::pruneNodes(StiKalmanTrackNode * node) - Beginning"<<endl;
    StiKalmanTrackNode * parent = static_cast<StiKalmanTrackNode *>(node->getParent());
    while (parent)
	{
	    //if (StiDebug::isReq(StiDebug::Finding)) 
	    //trackMes << "SKTF::pruneNodes(StiKalmanTrackNode * node) -"
	    //			 << "node has childCount:" << parent->getChildCount() << endl;
	    parent->removeAllChildrenBut(node);
	    node = parent;
	    parent = static_cast<StiKalmanTrackNode *>(node->getParent());
	}
}

void StiKalmanTrackFinder::reserveHits(StiKalmanTrackNode * node)
{
    // Declare hits on the track ending at "node"
    // as used. This method starts with the last node and seeks the
    // parent of each node recursively. The hit associated with each
    // (when there is a hit) is set to "used".
    
    //trackMes <<"SKTF::reserveHits(StiKalmanTrackNode * node) - Beginning"<<endl;
    StiKTNVertIterator it(node);
    for_each( it, it.end(), SetHitUsed() );
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


void   StiKalmanTrackFinder::setField(double f)
{
    //cout << "Field set to : " << f << endl;
    StiKalmanTrackNode::setFieldConstant(f);
}

void   StiKalmanTrackFinder::setMinContiguousHitCount(int count)
{
    //cout << "minContiguousHitCount set to : " << count << endl;
    StiKalmanTrackNode::minContiguousHitCountForNullReset = count;
}

void   StiKalmanTrackFinder::setMaxNullCount(int count)
{
    //cout << "maxNullCount set to : " << count << endl;
    StiKalmanTrackNode::maxNullCount = count;
}

void   StiKalmanTrackFinder::setMaxContiguousNullCount(int count)
{
    //cout << "maxContiguousNullCount set to : " << count << endl;
    StiKalmanTrackNode::maxContiguousNullCount = count;
}

void   StiKalmanTrackFinder::setMaxChi2ForSelection(double chi)
{
    //cout << "	maxChi2ForSelection set to :" << chi << endl;
    maxChi2ForSelection = chi;
}
