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
  StiKalmanTrackFinder  
  
  Author:  Claude Pruneau, Wayne State University                        
  Created: March 2001                                                    
  
  Important Note: The Kalman Filter Code imbedded in this class was given
  to us gracioulsy by Jouri Belikov from the ALICE       
  collaboration. i.e. code reproduced with autorization. 
*/

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
{
    cout <<"StiKalmanTrackFinder::StiKalmanTrackFinder()"<<endl; 
    StiTrack::setTrackFitter(new StiKalmanTrackFitter());
    reset();
}

StiKalmanTrackFinder::~StiKalmanTrackFinder()
{
	cout <<"StiKalmanTrackFinder::~StiKalmanTrackFinder()"<<endl;
}

void StiKalmanTrackFinder::reset()
{
	singleNodeDescent    = true;
	singleNodeFrom       = 20;
	mcsCalculated        = false;
	elossCalculated      = false;
	maxChi2ForSelection  = 5.;
	minContiguousHitCountForNullReset = 2;
	maxNullCount = 40;  
	maxContiguousNullCount = 5;
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
    try {
	StiKalmanTrack* track = 0;
	if (trackSeedFinder->hasMore())	{ //Redundant check, but it protectes against naive calls
	    track = trackSeedFinder->next();
	    if (!track) 
	      {
		cout <<"StiKalmanTrackFinder::doTrackFit()\t Track==0. Abort"
		     <<endl;
		return;
	      }
	    else 
	      {
		cout <<"StiKalmanTrackFinder::doTrackFit()\t Got Valid track"<<endl;
		track->fit();
		/*
		StiKalmanTrackNode * f = track->getFirstNode();
		StiKalmanTrackNode * ccc = dynamic_cast<StiKalmanTrackNode *>(f->getFirstChild());
		if (ccc==0) {
		    cout <<"StiKalmanTrackFinder::doTrackFit(): ERROR!\ttrack->getFirstChild() Failed! Abort"<<endl;
		    return;
		}
		fitInward(ccc);
		//if (trackFilter->accept(track))
		**/
		trackContainer->push_back(track);
		track->update();  //This updates the track on the display
		cout <<*track<<endl;
	    }
	}
	else {
	    cout <<"\ttrackSeedFinder->hasMore()==false"<<endl;
	}
    }
    
    catch (Exception & e) {
	cout << "StiKalmanTrackFinder::doTrackFit() - Exception: " << e << endl;
    }
}

void StiKalmanTrackFinder::doTrackFind()
{
    try {
	StiKalmanTrack* track = 0;
	if (trackSeedFinder->hasMore())	{ //Redundant check, but it protectes against naive calls
	    track = trackSeedFinder->next();
	    if (!track) {
		cout <<"StiKalmanTrackFinder::doTrackFind()\t Track==0. Abort"
		     <<endl;
		return;
	    }
	    else {
		cout <<"StiKalmanTrackFinder::doTrackFind()\t Got Valid track"<<endl;
		findTrack(track);
		//if (trackFilter->accept(track)) 
		trackContainer->push_back(track);
		
		track->update();  //This updates the track on the display
	    }
	}
	
	else {
	    cout <<"\ttrackSeedFinder->hasMore()==false"<<endl;
	}
    }
    
    catch (Exception & e) {
	cout << "StiKalmanTrackFinder::doTrackFind() - Exception: " << e << endl;
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
      try
	{
	  findTrack(t);
	  if (trackFilter->accept(t)) 
	    trackContainer->push_back(t);
	}
      catch (Exception e)
	{
	  cout << e << endl;
	}
    } 
  }
}

void StiKalmanTrackFinder::findTrack(StiTrack * t) throw ( Exception)
{
  //-----------------------------------------------------------------
  // Find extension (track) to the given track seed
  // Return Ok      if operation was successful
  // Return Error   if given seed "t" is invalid
  //                or if input data are invalid or if some other 
  //                internal error has occured.
  //-----------------------------------------------------------------
  StiKalmanTrack * tt = dynamic_cast<StiKalmanTrack *> (t);
  StiKalmanTrackNode * lastNode = tt->getLastNode();
  followTrackAt(lastNode);
  lastNode = followTrackAt(lastNode);
  pruneNodes(lastNode);
  tt->setLastNode(lastNode);
  //extendToMainVertex(lastNode);
}

StiKalmanTrackNode *
StiKalmanTrackFinder::followTrackAt(StiKalmanTrackNode * node) throw (Exception)
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
  StiKalmanTrackNode * tNode;        // target node
  StiKalmanTrackNode * bestNode = 0;  
  StiDetector * sDet;
  StiDetector * tDet;

  bool trackDone = false;
  bool scanningDone = false;
  hitCount = 0;
  nullCount = 0;
  contiguousNullCount = 0;
  contiguousHitCount  = 0;
  sDet  = sNode->getHit()->detector();
  detectorContainer->setToDetector(sDet);
  detectorContainer->moveIn();
  if (tDet==sDet)
    return sNode; // track cannot be prolongued any further

  while (!trackDone)   // search track until done
    {
      //sHit = sNode->getHit();
      //yWindow = getYWindow(sNode, sHit);
      //zWindow = getZWindow(sNode, sHit);
      
      scanningDone = false;
      bestChi2 = 1e50;
      visitedDet = 0;
      int position;
      // scan layer for connecting hits
      scanningDone = false;
      while (!scanningDone)
				{
					visitedDet++;
					tNode = trackNodeFactory->getObject();
					tNode->reset();
					position = tNode->propagate(sNode, tDet); // 
					if (position==kFailed) return sNode;
					if (tDet->isActive())  // active vol, look for hits
						{
							if (position<=kEdgeZplus)
								{
									hitContainer->setDeltaD(5.); //yWindow);
									hitContainer->setDeltaZ(5.); //zWindow);
									hitContainer->setRefPoint(tNode->fAlpha,
																						tNode->fX,
																						tNode->fP0,
																						tNode->fP1);
									while (hitContainer->hasMore())
										{
											tNode->setHit(hitContainer->getHit());
											chi2 = tNode->evaluateChi2();
											if (chi2<maxChi2ForSelection && chi2 < bestChi2)
												{
													bestChi2 = chi2;
													bestNode = tNode;
												}
										} // searching best hit
									if (position==kHit)
										scanningDone = true;
								}
						}
					else  // inactive, keep only if position==0
						{
							if (position<=kEdgeZplus)
								scanningDone = true;
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
      else
				{
					// update null counters
					contiguousNullCount++;
					nullCount++;
					if (nullCount>maxNullCount ||
							contiguousNullCount>maxContiguousNullCount)
						trackDone = true;
				}
			
      
      detectorContainer->moveIn();
      StiDetector * newDet = **detectorContainer;
      if (newDet==tDet)
	{
	  // same det implies track is  done
	  trackDone = true;
	}
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

  StiKalmanTrackNode * parent = dynamic_cast<StiKalmanTrackNode *>(node->getParent());
  while (parent)
    {
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
