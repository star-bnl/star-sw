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
#include "StiToolkit.h"
#include "StiKTNIterator.h"
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
#include "StiDynamicTrackFilter.h"
#include "StiTrackContainer.h"

ostream& operator<<(ostream&, const StiTrack&);

StiKalmanTrackFinder::StiKalmanTrackFinder(StiToolkit * userToolkit)
    : StiTrackFinder(userToolkit),trackMes(*Messenger::instance(MessageType::kTrackMessage)),
      mSubject(StiIOBroker::instance())
{
    //Turn off by default
    Messenger::instance()->clearRoutingBits(MessageType::kTrackMessage);
    reset();
    // set parameters used by this finder.
    setParameters(new StiKalmanTrackFinderParameters());
    mSubject->attach(this);
    getNewState();
    //cout << "StiKalmanTrackFinder::StiKalmanTrackFinder() - Done"<<endl;
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
    StiIOBroker *  broker = toolkit->getIOBroker();
    //cout <<"StiKalmanTrackFinder::getNewState()"<<endl;
    pars->setMCSCalculated(broker->ktfMcsCalculated()); //check
    pars->setElossCalculated(broker->ktfElossCalculated()); //check
    pars->setMaxChi2ForSelection(broker->ktfMaxChi2ForSelection());//check
    pars->setField(broker->ktfBField()); //check
    pars->setMassHypothesis(broker->ktfMassHypothesis()); //check
  
    pars->setMinContiguousHitCount(broker->ktfMinContiguousHitCount());  //check
    pars->setMaxNullCount(broker->ktfMaxNullCount()); //check
    pars->setMaxContiguousNullCount(broker->ktfMaxContiguousNullCount()); //check
  
    pars->setMinSearchWindow(broker->ktfMinSearchRadius()); //check
    pars->setMaxSearchWindow(broker->ktfMaxSearchRadius()); //check
    pars->setSearchWindowScale(broker->ktfSearchWindowScale()); //check

    if (broker->ktfUseHelixExtrapolation()==true)  {
	StiMaterialInteraction::setExtrapolationType(kHelixExtrapolation);
    }
    else {
	StiMaterialInteraction::setExtrapolationType(kLinearExtrapolation);
    }
}

void StiKalmanTrackFinder::reset()
{
    //progFlowMes <<"StiKalmanTrackFinder::reset()"<<endl;
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
		    trackMes << "StepByLayer" << endl;
		    switch (state)
			{
			case 0:
			    trackMes<< "InitTrackSearch()"<< endl;
       
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
      
		    trackMes << "StepByDetector" << endl;
      
		    switch (state)
			{
			case 0:
			    trackMes<< "InitTrackSearch()"<<endl;
			    doInitTrackSearch(); 
			    state++; 
			    break;
			case 1:
			    trackMes<< "InitLayer()" << endl;
			    doInitLayer();
			    state++;
			    break;
			case 2:
			    trackMes<< "doNextDetector()" << endl;
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
	    trackMes << "STATE:" << state << endl;
	    track->update();
	}
    catch (runtime_error & rte)
	{
	    cout << "RunTime Error Exception: " << rte.what();
	}
    catch (exception & e)
	{
	    cout << "Exception: " << e.what();
	}
}

void StiKalmanTrackFinder::findTracks()
{
    //-----------------------------------------------------------------
    // Find all possible tracks in the given set of hits/points.
    //-----------------------------------------------------------------
    try 
	{
	    StiKalmanTrack * track;
	    while (trackSeedFinder->hasMore())
		{ 
		    // obtain track seed from seed finder
		    track = trackSeedFinder->next();
		    if (!track) 
			throw runtime_error("StiKalmanTrackFinder::fitTracks() - trackSeedFinder returned track==0");
		    // find extension of this seed
		    findTrack(track);
		    // update on display
		    //track->update();
		}
	}
    catch (runtime_error & rte) 
	{
	    trackMes << "StiKalmanTrackFinder::findTracks() - Run Time Error :" << rte.what() << endl;
	}
    catch (exception & e) 
	{
	    cout << "StiKalmanTrackFinder::findTracks() - Internal Error :" << e.what() << endl;
	}
}

/// Fit all tracks produced by the track seed finder
/*! Fit all track produced by the track seed finder. 
 * This method is useful when the seed finder returns full tracks return
 * by a 3rd party track finder e.g. the tpt package.
 */
void StiKalmanTrackFinder::fitTracks()
{
    try 
	{
	    track = 0;
	    if (trackSeedFinder->hasMore()) 
		{ //Redundant check, but it protectes against naive calls
		    track = trackSeedFinder->next();
		    if (!track) 
			throw runtime_error("StiKalmanTrackFinder::fitTracks() - trackSeedFinder returned track==0");
		    trackMes <<"\nStiKalmanTrackFinder::fitTracks()\t Got Valid track"<<endl;
					
		    track->fit(); track->setFlag(0);
		    if (pars->useTrackFilter && trackFilter->filter(track)) 
			trackContainer->push_back(track);
		    else
			trackContainer->push_back(track);
		    track->update();  //This updates the track on the display
		    trackMes << "track parameters:";
		    trackMes << *track<<endl;
		}
	    else 
		trackMes <<"\ttrackSeedFinder->hasMore()==false"<<endl;
	}
    catch (runtime_error & rte) 
	{
	    trackMes << "StiKalmanTrackFinder::fitTracks() - Run Time Error :" << rte.what() << endl;
	}
    catch (exception & e) 
	{
	    cout << "StiKalmanTrackFinder::fitTracks() - Internal Error :" << e.what() << endl;
	}
}

/*! Extend all known tracks to primary vertex
  <p>
  Attempt an extension of all known tracks to the given primary vertex. If the extension is successfull, 
  the vertex is added to the track as a node. Node that in this implementation, it is assumed the
  track has been pruned and thus consists of a single node sequence (as opposed to a tree).
  <p>
  <ol>
  <li>Loop on all tracks currently stored in track container.</li>
  <li>It is assumed that the track does not already have a main vertex associated with it.</li>
  <li>Attempt extension to the given vertex by a call to "extendToMainVertex". 
  <li>If extension is successfull, the given vertex is added as node to the track.
  </ol>
  <p>
  <h3>Note</h3>
  Any exception thrown by "getInnerMostNode()" or "extendTrackToVertex()" are
  caught here and reported with "cout".
*/
void StiKalmanTrackFinder::extendTracksToVertex(StiHit* vertex)
{
    try 
	{
	    track = 0;
	    for (KalmanTrackMap::const_iterator it=trackContainer->begin(); it!=trackContainer->end(); ++it) 
		{
		    track = (*it).second;
		    extendTrackToVertex( track->getInnerMostNode(),vertex);
		}
	}
    catch (runtime_error & rte) 
	{
	    cout << "StiKalmanTrackFinder::extendTracksToVertex() - Run Time Error :\n" << rte.what() << endl;
	}
    catch (logic_error & le) 
	{
	    cout << "StiKalmanTrackFinder::extendTracksToVertex() - Logic Error :\n" << le.what() << endl;
	}
    catch (exception & e) 
	{
	    cout << "StiKalmanTrackFinder::extendTracksToVertex() - Internal Error :\n" << e.what() << endl;
	}
}


void StiKalmanTrackFinder::findNextTrack()
{
    try 
	{
	    track = 0;
	    if (!trackSeedFinder)
		throw runtime_error("No Track seed finder instance available");
	    if (trackSeedFinder->hasMore()) 
		{ 
		    track = 0;
		    track = trackSeedFinder->next();
		    //Redundant check, but it protectes against naive calls
		    if (!track)
			throw runtime_error("TrackSeedFinder->next() returned 0");
		    findTrack(track);
		} 
	    else 
		cout <<"StiKalmanTrackFinder::findNextTrack() - INFO - trackSeedFinder->hasMore()==false"<<endl;
	}
    catch (runtime_error & rte) 
	{
	    cout << "StiKalmanTrackFinder::findNextTrack() - Run Time Error :\n" << rte.what() << endl;
	}
    catch (logic_error & le) 
	{
	    cout << "StiKalmanTrackFinder::findNextTrack() - Logic Error :\n" << le.what() << endl;
	}
    catch (exception & e) 
	{
	    cout << "StiKalmanTrackFinder::findNextTrack() - Internal Error :\n" << e.what() << endl;
	}
}

void StiKalmanTrackFinder::fitNextTrack()
{
    try 
	{
	    track = 0;
	    if (trackSeedFinder->hasMore()) 
		{ //Redundant check, but it protectes against naive calls
		    track = trackSeedFinder->next();
		    if (!track) 
			throw runtime_error("\nStiKalmanTrackFinder::fitNextTrack() - trackSeedFinder->next() returned 0");
		    track->fit();
		    if (pars->useTrackFilter && trackFilter->filter(track)) 
			trackContainer->push_back(track);
		    else
			trackContainer->push_back(track);
		    track->update();  //This updates the track on the display
		    trackMes << "track parameters:" << trackMes << *track<<endl;
		}
	    else 
		trackMes <<"\ttrackSeedFinder->hasMore()==false"<<endl;
	}
    catch (runtime_error & rte) 
	{
	    trackMes << "StiKalmanTrackFinder::fitNextTrack() - Run Time Error :" << rte.what() << endl;
	}
    catch (exception & e) 
	{
	    cout << "StiKalmanTrackFinder::fitNextTrack() - Internal Error :" << e.what() << endl;
	}
}

void StiKalmanTrackFinder::findNextTrackSegment(){}

void StiKalmanTrackFinder::findTrack(StiTrack * t) // throws runtime_error, logic_error
{
    //-----------------------------------------------------------------
    // Find extension (track) to the given track seed
    // Return Ok      if operation was successful
    // Return Error   if given seed "t" is invalid
    //                or if input data are invalid or if some other 
    //                internal error has occured.
    //-----------------------------------------------------------------
    trackDone = true;
    scanningDone = true;
    state = 0;
    trackMes << "SKTF::findTrack(StiTrack * t) - Beginning" << endl;
    track = dynamic_cast<StiKalmanTrack *> (t);
    StiKalmanTrackNode * lastNode;
    if (!track) 
	{
	    throw logic_error("SKTF::findTrack()\t - ERROR - dynamic_cast<StiKalmanTrack *>  returned 0");
	}
    track->setFlag(0);
    initSearch(track->getLastNode());
    while (!trackDone) 
	{
	    doInitLayer(); //cout<<"init layer done"<<endl;
	    doScanLayer(); //cout <<"scan layer done"<<endl;
	    doFinishLayer(); //cout <<"finished layer"<<endl;
	}
    lastNode = sNode;
    pruneNodes(lastNode);
    reserveHits(lastNode);
    using namespace std;
    //track->setFittingDirection(kInsideOut);
    //track->fit();
    track->setFlag(1);
    if (pars->useTrackFilter && trackFilter->filter(track)) 
	trackContainer->push_back(track);
    else
	trackContainer->push_back(track);
    //cout<<"swap"<<endl;
    //track->swap();
    //track->setFittingDirection(kOutsideIn);
    //track->fit();
    //cout << *track<<endl;
    track->update();  //This updates the track on the display
    trackDone = false;  // ready for a new track
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
		    trackMes << "NO MORE TRACK SEEDS - EVENT COMPLETED" << endl;
		    return;
		}
	    //throw logic_error("SKTF::doInitTrackSearch() - Error - trackSeedFinder->next() returned 0 while trackSeedFinder->hasMore() returned true");
	    StiKalmanTrackNode * lastNode = track->getLastNode();
	    if (!lastNode) 
		throw logic_error("SKTF::findTrack()\t - ERROR - track->getLastNode() returned 0");
	    initSearch(lastNode);
	}
    else 
	{
	    trackMes <<"\ttrackSeedFinder->hasMore()==false"<<endl;
	}
}

void StiKalmanTrackFinder::doFinishTrackSearch()
{ 
    //trackMes<<"SKTF::doFinishTrackSearch() - called"<<endl;
    if (!trackDone) return;   // must finish 
    if (!scanningDone) return;
    //trackMes<<"SKTF::doFinishTrackSearch() - begins"<<endl;
    if (pars->useTrackFilter)
	{
	    //if (trackFilter->filter(track)) 
	    if (trackFilter->accept(track)) 
		trackContainer->push_back(track);
	}
    else
	{
	    trackContainer->push_back(track);
	}
    //cout << " doFinishTrackSearch() - track done - now update display" << endl;
    track->update();  //This updates the track on the display
}

//remove
//StiKalmanTrackNode * StiKalmanTrackFinder::followTrackAt(StiKalmanTrackNode * node)
//  //throw (Exception)
//{
//  initSearch(node);
//  search();
//  return sNode;
//}


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

//void StiKalmanTrackFinder::search()
//{
//while (!trackDone) 
// {
//      doInitLayer(); 
//      doScanLayer();
//      doFinishLayer();
//    }
//	//cout << " StiKalmanTrackFinder::search() done" << endl;
//}

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
	{
	    //cout << "StiKalmanTrackFinder::doInitLayer() - LOGIC ERROR - tDet==0" << endl;
	    throw logic_error("SKTF::doInitLayer() ERROR - tDet==0");
	}
    else if (tDet==currentDet) 
	{
	    //trackMes << "SKTF::doInitLayer() - TrackDone==true - moveIn >> tDet==sDet"  << endl;
	    trackDone = true;
	    return;
	}
    //sHit = sNode->getHit(); //yWindow = getYWindow(sNode, sHit); //zWindow = getZWindow(sNode, sHit);
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
	    trackMes << "SKTF::doNextDetector() - RunTimeError: " << rte.what();
	    scanningDone = true;
	    trackDone = true;
	    return;
	}
    trackMes << "TargetDet :"<<*tDet<<endl;
    trackMes << "TargetNode:"<<*tNode<<endl;
    
    if (position<=kEdgeZplus) 
	{
	    hasDet = true;
	    leadNode = tNode;
	    leadNode->setDetector(tDet);
	    if (tDet->isActive()) 
		{ // active vol, look for hits
		    if (position==kHit)
			scanningDone = true;
		    //cout <<"dy,dz:"<< 3.*tNode->fC00 << "\t" << tNode->fC11 << endl;
		    hitContainer->setDeltaD(tNode->getWindowY());
		    hitContainer->setDeltaZ(tNode->getWindowZ());
		    hitContainer->setRefPoint(tNode->fX,tDet->getPlacement()->getCenterRefAngle(),tNode->fP0,tNode->fP1);
		    bool hasMore = hitContainer->hasMore();
		    while (hasMore) 
			{
			    tNode->setHit(hitContainer->getHit());
			    chi2 = tNode->evaluateChi2();
			    trackMes << "SKTF::followTrackAt()\t chi2:" << chi2  << endl;
			    if (chi2<pars->maxChi2ForSelection && chi2 < bestChi2)
				{
				    trackMes << "SKTF::followTrackAt()\t selected hit - chi2:" << chi2 << endl;
				    hasHit = true;
				    bestChi2 = chi2;
				    bestNode = tNode;
				}
			    hasMore = hitContainer->hasMore();
			    if (hasMore) // prepare new node
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
	    else // projection in inactive volume, scanning is done
		scanningDone = true;
	}
    else if (position==kFailed) 
	{
	    trackMes << "SKTF::doNextDetector()\t - position==kFailed" << endl;
	    scanningDone = true;
	    trackDone = true;
	    return;
	    //throw runtime_error("SKTF::doNextDetector()\t- RunTimeError - newNode==null");
	}


    if (!scanningDone)
	{
	    StiDetector * nextDet;
	    if (position==kEdgePhiPlus || position==kMissPhiPlus)
		{
		    if (lastMove>=0 )
			{
			    trackMes << "SKTF::followTrackAt()\t- movePlusPhi()" << endl;
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
			    trackMes << "SKTF::followTrackAt()\t-position==kEdgePhiPlus||kMissPhiPlus - but no PlusPhi done" << endl;
			}
      
		}
	    else if (position==kEdgePhiMinus || position==kMissPhiMinus)
		{
		    if (lastMove<=0)
			{
			    trackMes << "SKTF::followTrackAt()\t- moveMinusPhi()" << endl;
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
			    trackMes << "SKTF::followTrackAt()\t-position==kEdgePhiMinus||kMissPhiMinus - but no MinusPhi done" << endl;
			}
		}
	    else
		{
		    trackMes <<  "SKTF::followTrackAt()\t- Scanning set to done" << endl;
		    scanningDone = true;
		}
	}
    if (abs(lastMove)>2) scanningDone = true;
    //printState();
}

void StiKalmanTrackFinder::printState()
{
    trackMes << "State:"<<state;
    if (scanningDone) 
	trackMes << "/Scanning Done";
    else
	trackMes << "/Scanning NOT Done";
    if (trackDone) 
	trackMes << "/Track Done"<<endl;
    else
	trackMes << "/Track NOT Done" << endl;
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
		    //StiHit * myHit = bestNode->getHit();
		    //cout << "HIT:" << myHit->x() << "\t" << myHit->y() << "\t" << myHit->z();
		    //cout << "CENTER:"<<  bestNode->getHelixCenter() << endl;
		    //cout << "EXTRA: "<<  bestNode->getPointAt(myHit->x()+1.) << endl;
		    sNode->add(bestNode);
		    sNode = bestNode;
		    track->setLastNode(bestNode);
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
		    track->setLastNode(leadNode);
		    trackMes << "SKTF::doFinishLayer() "
			     << "               nullCount:" << leadNode->nullCount << endl
			     << "    contiguousNullCount :" << leadNode->contiguousNullCount << endl;
		    if (leadNode->nullCount>pars->maxNullCount ||
			leadNode->contiguousNullCount>pars->maxContiguousNullCount)
			trackDone = true;    
		}
	}
    else // no det crossing found
	{
	    trackMes << "SKTF::doFinishLayer() - Layer Completed with no node added" << endl;
	    //if (nullCount>maxNullCount ||contiguousNullCount>maxContiguousNullCount)
	    // trackDone = true;
	}
    //printState();
}

/*! Remove given node from given track. 
  <p>
  Not implemented
*/
void StiKalmanTrackFinder::removeNodeFromTrack(StiKalmanTrackNode * node, StiKalmanTrack* track)
{
}

/*! Prune the track to select the best branch of the tree identified by given leaf node.
  <p>
  The best brach is assumed to be the one given by the leaf "node".
  All siblings of the given node, are removed, and iteratively
  all siblings of its parent are removed from the parent of the
  parent, etc.
*/
void StiKalmanTrackFinder::pruneNodes(StiKalmanTrackNode * node)
{
    trackMes <<"SKTF::pruneNodes() - Beginning"<<endl;
    StiKalmanTrackNode * parent = static_cast<StiKalmanTrackNode *>(node->getParent());
    while (parent)
	{
	    parent->removeAllChildrenBut(node);
	    node = parent;
	    parent = static_cast<StiKalmanTrackNode *>(node->getParent());
	}
}

/*! Declare hits associated with given track as used.
  <p>
  Declare hits on the track ending at "node" as used. 
  This method starts with the last node and seeks the
  parent of each node recursively. The hit associated with each
  node (when there is a hit) is set to "used".
*/	
void StiKalmanTrackFinder::reserveHits(StiKalmanTrackNode * node)
{
    //trackMes <<"SKTF::reserveHits(StiKalmanTrackNode * node) - Beginning"<<endl;
    StiKTNForwardIterator it(node);
    for_each( it, it.end(), SetHitUsed() );
}

/*! Extend track encapsulated by "sNode" to the given vertex.
  <p>
  Attempt an extension of the track encapsulated by sNode the given vertex. 
  <p>
  <ol>
  <li>Get node from node factory.</li>
  <li>Reset node.</li>
  <li>Propagate the node from given parent node "sNode", to the given vertex using a 
  call to "propagate".</li>
  <li>Evaluate the chi2 of the extrapolated if the vertex is added to the track. Done
  using a call to "evaluateChi2".</li>
  <li>If chi2 is less than max allowed "maxChi2ForSelection", update track parameters
  using the vertex as a measurement and add the vertex to the track as the last node.</li>
  </ol>
  <h3>Notes</h3>
  <ul>
  <li>Throws logic_error if no node can be obtained from the node factory.</li>
  <li>The methods "propagate", "evaluateChi2", and "updateNode" may throw 
  runtime_error exceptions which are NOT caught here...</li>
  </ul>
*/
void StiKalmanTrackFinder::extendTrackToVertex(StiKalmanTrackNode * sNode, StiHit* vertex)
{
    tNode = trackNodeFactory->getObject();
    if (tNode==0) 
	throw logic_error("SKTF::extendTrackToVertex()\t- ERROR - tNode==null");
    tNode->reset();
    position = tNode->propagate(sNode, vertex);
    tNode->setHit(vertex);
    chi2 = tNode->evaluateChi2(); 
    if (chi2<pars->maxChi2ForSelection)
	{
	    tNode->updateNode();
	    sNode->add(tNode);	
	    track->setLastNode(tNode);
	}
    cout << *track << endl;
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

*/

