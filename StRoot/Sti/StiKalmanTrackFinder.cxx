/// \File StiKalmanTrackFinder.cxx
/// \Author Claude A Pruneau, 2001-2003
/// \Copyright(c) 2001, STAR  Experiment at BNL, All rights reserved.      
/// \Note  
/// Permission to use, copy, modify and distribute this software and its
/// documentation strictly for non-commercial purposes is hereby granted 
/// without fee, provided that the above copyright notice appears in all
/// copies and that both the copyright notice and this permission notice
/// appear in the supporting documentation. The authors make no claims 
/// about the suitability of this software for any purpose. It is     
/// provided "as is" without express or implied warranty.             
#include <iostream>
#include <stdexcept>
#include <math.h>
using namespace std;
#include "Sti/Base/Messenger.h"
#include "Sti/Base/Parameter.h"
#include "Sti/Base/EditableFilter.h"
#include "StiHitLoader.h"
#include "StiToolkit.h"
#include "StiKTNIterator.h"
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
#include "StiTrack.h"
#include "StiMcTrack.h"
#include "StiGui/StiRootDrawableMcTrack.h"
#include "StiKalmanTrackFinder.h"
#include "StiTrackContainer.h"
#include "StiDrawableTrack.h"
#include "StEvent.h"
#include "StPrimaryVertex.h"
#include "StMcEvent.hh"
#include "StMcTrack.hh"
#include "StMcContainers.hh"
#include "StiMaker/StiStEventFiller.h"
#include "StiKalmanTrack.h"
#include "StiKalmanTrackNode.h"
#include "StiKalmanTrackFinderParameters.h"
#include "Sti/Base/Messenger.h"
#include "StThreeVector.hh"
#include "StThreeVectorF.hh"
#include "StThreeVectorD.hh"
#include "StiVertexFinder.h"

ostream& operator<<(ostream&, const StiTrack&);
void StiKalmanTrackFinder::initialize()
{
  //set the cached variables
  cout << "StiKalmanTrackFinder::initialize() -I- Started"<<endl;
  _toolkit = StiToolkit::instance();
  _trackSeedFinder   = _toolkit->getTrackSeedFinder();
  _trackNodeFactory  = _toolkit->getTrackNodeFactory(); 
  _trackFactory      = _toolkit->getTrackFactory();
  _mcTrackFactory    = _toolkit->getMcTrackFactory(); 
  _hitFactory        = _toolkit->getHitFactory(); 
  _detectorContainer = _toolkit->getDetectorContainer();
  _hitLoader         = _toolkit->getHitLoader();
  _hitContainer      = _toolkit->getHitContainer();
  _trackContainer    = _toolkit->getTrackContainer();
  _mcTrackContainer  = _toolkit->getMcTrackContainer();
  _vertexFinder      = _toolkit->getVertexFinder();
  _eventFiller       = new StiStEventFiller();
  cout << "StiKalmanTrackFinder::initialize() -I- Done"<<endl;
}

StiKalmanTrackFinder::StiKalmanTrackFinder(StiToolkit*toolkit)
  : _toolkit(toolkit),
    _trackFilter(0), 
    _guiTrackFilter(0), 
    _guiMcTrackFilter(0), 
    _trackSeedFinder(0),
    _trackNodeFactory(0),
    _trackFactory(0),
    _mcTrackFactory(0),
    _hitFactory(0),
    _detectorContainer(0),
    _hitLoader(0),
    _hitContainer(0),
    _trackContainer(0),
    _mcTrackContainer(0),
    _vertexFinder(0),
    _eventFiller(0),
    _event(0),
    _mcEvent(0),
    _pars(0),
    _messenger(*Messenger::instance(MessageType::kTrackMessage))
{
  _messenger << "StiKalmanTrackFinder::StiKalmanTrackFinder() - Started"<<endl;
  if (!_toolkit)
    throw runtime_error("StiKalmanTrackFinder::StiKalmanTrackFinder(...) - FATAL - toolkit==0");
  setParameters(new StiKalmanTrackFinderParameters());
  _messenger << "StiKalmanTrackFinder::StiKalmanTrackFinder() - Done"<<endl;
}

StiKalmanTrackFinder::~StiKalmanTrackFinder()
{ }

/*!
 Reset the state of the finder  to "event not tracked"
 <p>
 The track factory, the track container are reset. This
 method is distinct from the "clear" method which reset 
 the state to "event not loaded".
 */
void StiKalmanTrackFinder::reset()
{
  _messenger << "StiKalmanTrackFinder::reset() - INFO - Starting" <<endl;
  _detectorContainer->reset();
  _trackContainer->clear();
  _trackFactory->reset();
  _trackNodeFactory->reset();
  _trackSeedFinder->reset();
  _messenger << "StiKalmanTrackFinder::reset() - INFO - Done" <<endl;
}

/*!
 Reset the state of the finder  to "no event loaded"
 <p>
 A reset or clear command is used to all components this tracker 
 depends on. This include the hitContainer, the detector container,
 the hit, track, track node, mc track factories, the track containers,
 and the seed finder.
 */
void StiKalmanTrackFinder::clear()
{
  _messenger << "StiKalmanTrackFinder::clear() - INFO - Starting" <<endl;
  _detectorContainer->reset();
  _hitContainer->clear();
  _hitFactory->reset();
  _trackFactory->reset();
  _mcTrackFactory->reset();
  _trackNodeFactory->reset();
  _trackContainer->clear();
  _mcTrackContainer->clear();
  _messenger << "StiKalmanTrackFinder::clear() - INFO - Done" <<endl;
}

/*! Find all tracks associated with the current event.
 <p>
 Algorithm: In a <b>while loop</b>, obtain track seeds from 
 current track seed finder and proceed to extend it through the
 detector. 
 <p>Found tracks are added to the track container if no track 
 filter is set or if they satisfy the track filter requirements. 
 */
void StiKalmanTrackFinder::findTracks()
{
  if (!_trackContainer)
    throw runtime_error("StiKalmanTrackFinder::findTracks() -F- _trackContainer==0");
  if (!_trackSeedFinder)
    throw runtime_error("StiKalmanTrackFinder::findTracks() -F- _trackSeedFinder==0");
  StiKalmanTrack * track;
  // find global tracks
  _messenger<<"StiKalmanTrackFinder::findTracks() -I- Begin tracking loop"<<endl;
  try
    {
      while (_trackSeedFinder->hasMore())
	{ 
	  try
	    {
	      // obtain track seed from seed finder
	      track = _trackSeedFinder->next();
	      if (!track) break;
	      track->find();
	      StiDrawableTrack * t = dynamic_cast<StiDrawableTrack *>(track);
	      if (t) t->update();
	      if (_trackFilter)
		{
		  if (_trackFilter->filter(track)) 
		    _trackContainer->push_back(track);
		}
	      else
		_trackContainer->push_back(track);
	    }
	  catch (runtime_error & rte)
	    {
	      _messenger<< "StiKalmanTrackFinder::findTracks() - Run Time Error :" << rte.what() << endl;
	    }
	}
      if (_eventFiller)
	_eventFiller->fillEvent(_event, _trackContainer);
      if (_vertexFinder)
	{
	  StiHit *vertex=0;
	  vertex = _vertexFinder->findVertex(_event);
	  if (vertex)
	    {
	      extendTracksToVertex(vertex);
	      if (_eventFiller)
		_eventFiller->fillEventPrimaries(_event, _trackContainer);
	    }						
	}
    }
  catch (runtime_error & rte)
    {
      _messenger<< "StiKalmanTrackFinder::findTracks() - Run Time Error :" << rte.what() << endl;
    }
}
  
/*! Fit all track produced by the track seed finder. 
  <p>
 This method is useful when the seed finder returns full tracks produced
 by a 3rd party track finder e.g. the tpt package.
 <p>Fitted tracks are added to the track container if no track 
 filter is set or if they satisfy the track filter requirements. 
 */
void StiKalmanTrackFinder::fitTracks()
{
  try 
    {
      track = 0;
      if (_trackSeedFinder->hasMore()) 
	{ //Redundant check, but it protectes against naive calls
	  track = _trackSeedFinder->next();
	  if (!track) 
	    throw runtime_error("StiKalmanTrackFinder::fitTracks() - trackSeedFinder returned track==0");
	  track->fit(kOutsideIn); track->setFlag(0);
	  // apply filter if one is defined 
	  // always add track if not
	  if (_trackFilter)
	    {
	      if (_trackFilter->filter(track))
		_trackContainer->push_back(track);
	    }
	  else
	    _trackContainer->push_back(track);
	  StiDrawableTrack * t = dynamic_cast<StiDrawableTrack *>(track);
	  t->update();
	  _messenger << "track parameters:";
	  _messenger << *track<<endl;
	}
      else 
	_messenger <<"\ttrackSeedFinder->hasMore()==false"<<endl;
    }
  catch (runtime_error & rte) 
    {
      _messenger << "StiKalmanTrackFinder::fitTracks() - Run Time Error :" << rte.what() << endl;
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
  caught here and reported with "_messenger".
*/
void StiKalmanTrackFinder::extendTracksToVertex(StiHit* vertex)
{
  int rawCount = 0;
  int goodCount= 0;
  int plus=0;
  int minus=0;
  int helPlus = 0;
  int helMinus = 0;
  for (TrackMap::const_iterator it=_trackContainer->begin(); 
       it!=_trackContainer->end(); 
       ++it) 
    {
      try
	{
	  rawCount++;
	  if ((*it).second->extendToVertex(vertex))
	    goodCount++;
	  if ((*it).second->getCharge()>0)
	    plus++;
	  else
	    minus++;
	  const StiKalmanTrack* tempo = dynamic_cast<const StiKalmanTrack*>((*it).second);
	  if (tempo->getInnerMostHitNode()->getHelicity()>0)
	    helPlus++;
	  else
	    helMinus++;
	   
	}
      catch (runtime_error & rte) 
	{
	  _messenger << "SKTF::extendTracksToVertex()"
		     << "- WARNING - Run Time Error while extending a track to main vertex."<<endl
		     << "Error Message:" << endl
		     << rte.what() << endl;
	}
    }
  cout << "SKTF::extendTracksToVertex(StiHit* vertex) -I- rawCount:"<<rawCount<<endl
       << "                                          extendedCount:"<<goodCount<<endl
       << "                                                   plus:"<<plus<<endl
       << "                                                  minus:"<<minus<<endl
       << "                                                helPlus:"<<helPlus<<endl
       << "                                               helMinus:"<<helMinus<<endl;
}

/// Find extension (track) to the given track seed
/// Return Ok      if operation was successful
/// Return Error   if given seed "t" is invalid
///                or if input data are invalid or if some other 
///                internal error has occured.
bool StiKalmanTrackFinder::find(StiTrack * t, int direction) // throws runtime_error, logic_error
{
  _messenger << "SKTF::find(StiTrack * t) - INFO - Started" << endl;
  int position;
  StiDetector * nextDet;
  StiHit * stiHit;
  track = dynamic_cast<StiKalmanTrack *> (t);
  if (!track) 
		{
			cout <<"StiKalmanTrackFinder::find(StiTrack * t, int direction) -F- track cast failed"<<endl;
			throw logic_error("SKTF::find()\t - ERROR - dynamic_cast<StiKalmanTrack *>  returned 0");
		}
  int  nAdded       = 0;
  bool trackDone    = false;
  bool scanningDone = true;
  sNode   = track->getLastNode(); 
	if (!sNode)
		{
			cout <<"                     sNode is null"<<endl;
			return false;
		}
	StiHit* hhh = sNode->getHit();
	if (!hhh)
		{
			cout << "           hhh==0" <<endl;
			return false;
		}
  sDet    = hhh->detector();
  if (sDet==0) 
		{
			cout << "StiKalmanTrackFinder::find(StiTrack * t, int direction) -I- sDet==0"<<endl;
			throw logic_error("SKTF::find(StiTrack*) - FATAL - sDet==0");
		}
  tNode   = 0;
  tDet    = 0;
  leadDet = sDet;
  StiKalmanTrackNode testNode;
  StiDetector * currentDet;
	_messenger << "SKTF::find(StiTrack * t) -I- searching track"<< endl;
  while (!trackDone) 
    {
      _detectorContainer->setToDetector(leadDet);
      currentDet = **_detectorContainer;
      if (direction==kOutsideIn)
				_detectorContainer->moveIn();
      else
				_detectorContainer->moveOut();
      tDet = **_detectorContainer;
      leadDet = tDet;
      // tDet==0 implies a severe detector container error - exit
      if (tDet==0) throw logic_error("SKTF::find() - ERROR -  **_detectorContainer ==0");
      // tDet=currentDet implies there are no more new volumes to go to - exit loop
      if (tDet==currentDet) break;
      lastMove     = 0;
      scanningDone = false;
      // loop over possible volumes
			_messenger << "SKTF::find(StiTrack * t) -I- scanning detors"<< endl;
      while (!scanningDone)
				{
					//_messenger <<"SCANNING LAYER"<<endl;
					testNode.reset();
					testNode.setChi2(1e50);
					position = testNode.propagate(sNode,tDet);
					if (position<0)
						{ // not reaching this detector layer - stop track
							_messenger << "TRACK DOES NOT REACH CURRENT LAYER"<<endl;
							trackDone = true; break;
						}
					else if (position<=kEdgeZplus) 
						{ // within detector on reached layer
							_messenger <<"WITHIN VOLUME"<<endl;
							testNode.setDetector(tDet);
							if (tDet->isActive()) 
								{ // active detector may have a hit
									_hitContainer->setRefPoint(testNode);
									while (_hitContainer->hasMore()) 
										{  
											stiHit = _hitContainer->getHit();
											if (!stiHit) throw logic_error("StiKalmanTrackFinder::doNextDetector() - FATAL - StiHit*hit==0");
											_messenger << "      chi2:"<<(chi2 = testNode.evaluateChi2(stiHit));
											_messenger << "  max chi2:"<< _pars->maxChi2ForSelection<<endl;
											_messenger << " best chi2:"<< testNode.getChi2()<<endl;
											if (chi2<_pars->maxChi2ForSelection && chi2<testNode.getChi2())
												{
													testNode.setHit(stiHit);
													testNode.setChi2(chi2);
												}
										} 
								}
							// add best node to track if it has a hit or
							// if the maximum of node with null hit has NOT been exceeded
							//cout << "CURRENT NODE:" << testNode<<endl;
							if (testNode.getHit() ||
									(testNode.nullCount  <  _pars->maxNullCount &&
									 testNode.contiguousNullCount  <  _pars->maxContiguousNullCount) )
								{
									StiKalmanTrackNode * node = _trackNodeFactory->getInstance();
									if (node==0) throw logic_error("SKTF::find() - ERROR - node==null");
									node->reset();
									*node = testNode;
									sNode = track->add(node);
									leadDet = sNode->getDetector();
									break;
								}
							else
								{
									//_messenger <<"TRACK IS DONE"<<endl;
									trackDone = true;  break;
								}
						}
					else if ( (position==kEdgePhiPlus || position==kMissPhiPlus)  && lastMove>=0 )
						{
							//_messenger << " MOVE PLUS PHI"<<endl;
							_detectorContainer->movePlusPhi(); lastMove++;
						}
					else if ( (position==kEdgePhiMinus || position==kMissPhiMinus) && lastMove<=0)
						{
							//_messenger << " MOVE MINUUS PHI"<<endl;
							_detectorContainer->moveMinusPhi(); lastMove--;
						}
					if (abs(lastMove)>2) break;
					nextDet = **_detectorContainer;
					if (nextDet==0 || tDet==nextDet) 
						{
							//_messenger <<" SCANNING DONE"<<endl;
							scanningDone = true;
						}
					else
						tDet = nextDet;
				}
    }
  _messenger << "StiKalmanTrackFinder::find(StiTrack* ) -I- TRACK IS DONE"<<endl;
  return nAdded>0;
}

/*! Get the number of tracks found by this finder for the current event.
  <p>
  This convenience method returns the number of tracks found by this finder
  for the current. The number of tracks is simply determined based on the size
  of the track container used by this finder. All tracks inserted in the container
  are counted, no quality cut is used. 
*/
int StiKalmanTrackFinder::getTrackFoundCount() const
{
  return _trackContainer->size();
}

/*! Get the number of tracks satisfying the given track filter.
  <p>
  This convenience method returns the number of tracks found by this finder
  for the current that satisfy the give track filter. 
*/
int StiKalmanTrackFinder::getTrackFoundCount(Filter<StiTrack> * filter) const
{
  // reset filter counter to zero.
  filter->reset();
  // loop over all tracks and filter
  TrackMap::const_iterator it;
  for (it=_trackContainer->begin(); 
       it!=_trackContainer->end(); 
       ++it) 
    filter->filter((*it).second);
  return filter->getAcceptedCount();
}

/*! Get the number of track seeds found by the seed finder used by this finder for the current event.
  <p>
  This convenience method returns the number of track seeds found by the 
  seed finder used by this finder for the current. 
  Note:needs to be fixed.
*/
int StiKalmanTrackFinder::getTrackSeedFoundCount() const
{
  return _trackContainer->size();
}

void StiKalmanTrackFinder::findNextTrack()
{
  try 
    {
      track = 0;
      if (!_trackSeedFinder)
				throw runtime_error("No Track seed finder instance available");
      if (_trackSeedFinder->hasMore()) 
				{ 
					track = 0;
					track = _trackSeedFinder->next();
					//Redundant check, but it protectes against naive calls
					if (!track) throw runtime_error("TrackSeedFinder->next() returned 0");
					track->find();
					StiDrawableTrack * t = dynamic_cast<StiDrawableTrack *>(track);
					if (t) t->update();
					if (_trackFilter)
					  {
					    if (_trackFilter->filter(track)) 
					      _trackContainer->push_back(track);
					  }
					else 
					  _trackContainer->push_back(track);
				} 
      else 
				_messenger <<"StiKalmanTrackFinder::findNextTrack() - I - trackSeedFinder->hasMore()==false"<<endl;
    }
  catch (runtime_error & rte) 
    {
      _messenger << "StiKalmanTrackFinder::findNextTrack() - Run Time Error :\n" << rte.what() << endl;
      //StiDrawableTrack * t = dynamic_cast<StiDrawableTrack *>(track);
      //if (t) t->update();
    }
}

void StiKalmanTrackFinder::fitNextTrack()
{
  try 
    {
      track = 0;
      if (_trackSeedFinder->hasMore()) 
				{ //Redundant check, but it protectes against naive calls
					track = _trackSeedFinder->next();
					if (!track) 
						throw runtime_error("\nStiKalmanTrackFinder::fitNextTrack() - trackSeedFinder->next() returned 0");
					track->fit(kOutsideIn);
					if (_trackFilter)
					  {
					    if(_trackFilter->filter(track)) 
					      _trackContainer->push_back(track);
					  }
					else
					  _trackContainer->push_back(track);
					StiDrawableTrack * t = dynamic_cast<StiDrawableTrack *>(track);
					t->update();
					_messenger << "track parameters:" << _messenger << *track<<endl;
				}
      else 
				_messenger <<"\ttrackSeedFinder->hasMore()==false"<<endl;
    }
  catch (runtime_error & rte) 
    {
      _messenger << "StiKalmanTrackFinder::fitNextTrack() - Run Time Error :" << rte.what() << endl;
    }
}

void StiKalmanTrackFinder::findNextTrackSegment()
{
  // depracated
}

void StiKalmanTrackFinder::update()
{
  TrackMap::const_iterator iter;
  StiDrawableTrack * t;

  StiToolkit::instance()->getDisplayManager()->reset();
  StiToolkit::instance()->getDisplayManager()->draw();
  StiToolkit::instance()->getDisplayManager()->update();
  // Monte Carlo Tracks
  if (_mcTrackContainer)
    {
      //_messenger << "StiKalmanTrackFinder::update() - INFO - Looping on MC tracks."<<endl;
      for (iter=_mcTrackContainer->begin();iter!=_mcTrackContainer->end(); ++iter) 
				{
					if (_guiMcTrackFilter)
						{
							//_messenger << "StiKalmanTrackFinder::update() - INFO - Filter OK/";
							if (_guiMcTrackFilter->filter((*iter).second))
								{
									//_messenger <<"Passed"<<endl;
									t = dynamic_cast<StiDrawableTrack *>((*iter).second);
									if (t)
										t->update();
									else
										_messenger << "StiKalmanTrackFinder::update() - WARNING - MC dynamic_cast failed." << endl;
								}
						}	
					else
						_messenger << "StiKalmanTrackFinder::update() - INFO - Filter NOK"<<endl;
				}
    }
	
  // Reconstructed tracks
  for (iter=_trackContainer->begin();iter!=_trackContainer->end();++iter) 
    {
      if (_guiTrackFilter && _guiTrackFilter->filter((*iter).second))
	{
	  t = dynamic_cast<StiDrawableTrack *>((*iter).second);
	  if (t)
	    t->update();
	  else
	    _messenger << "StiKalmanTrackFinder::update() - WARNING - Kalman dynamic_cast failed." << endl;
	}
    }
  StiToolkit::instance()->getDisplayManager()->draw();
  StiToolkit::instance()->getDisplayManager()->update();
  //_messenger << "StiKalmanTrackFinder::update() - INFO - Done."<<endl;
}

void StiKalmanTrackFinder::reloadEvent()
{
	clear();
	loadEvent(_event,_mcEvent);
}

void StiKalmanTrackFinder::loadEvent(StEvent * event, StMcEvent * mcEvent)
{
  if (!event)
    throw runtime_error("StiKalmanTrackFinder::loadEvent(...) - FATAL - given event==0");
  _event = event;
  _mcEvent = mcEvent;
  loadHits(event);

  if (mcEvent)
    { 
      StiEvaluableTrackSeedFinder* temp;
      temp = dynamic_cast<StiEvaluableTrackSeedFinder*>(_trackSeedFinder);
      if (temp!=0) 
				temp->setEvent(mcEvent);
      loadMcTracks(mcEvent);
    }
  _trackSeedFinder->reset();

}

void StiKalmanTrackFinder::loadHits(StEvent * event)
{
  if(!_hitLoader)
    throw runtime_error("StiKalmanTrackFinder::loadHits(...) - FATAL - _hitLoader==0");
  if(!_hitContainer)
    throw runtime_error("StiKalmanTrackFinder::loadHits(...) - FATAL - _hitContainer==0");
  _hitContainer->clear();
  _hitLoader->loadHits(event);
  _hitContainer->sortHits();
  _hitContainer->update(); //uncommented, MLM, 9/27
}

void StiKalmanTrackFinder::loadMcTracks(StMcEvent * mcEvent)
{
  // Store the pointers to StMcTrack in StiMcTrack
  StSPtrVecMcTrack & mcTracks = mcEvent->tracks();
  StiMcTrack * mcTrack;
  StMcTrackConstIterator iter;
  if (!_mcTrackFactory)
    throw runtime_error("StiKalmanTrackFinder::loadMcTracks() - ERROR - _mcTrackFactory==0");
  if (!_mcTrackContainer)
    throw runtime_error("StiKalmanTrackFinder::loadMcTracks() - ERROR - _mcTrackContainer==0");
  for (iter=mcTracks.begin();iter!=mcTracks.end();iter++)
    {
      //_messenger << "Loading StMcTrack into _mcTrackContainer" << endl;
      mcTrack = _mcTrackFactory->getInstance();
      mcTrack->setStMcTrack( (*iter) );
      _mcTrackContainer->add(mcTrack);
    }
}


/*! Find all tracks in the given event
<p>
<ol>
<li>First reset/clear the finder</li>
<li>Load the hits of the given event, and optionally the tracks of the given McEvent</li>
<li>Proceed to find all tracks of the given event</li>
</ol>
*/
void StiKalmanTrackFinder::findTracks(StEvent * event, StMcEvent * mcEvent)
{
  _messenger << "StiKalmanTrackFinder::findTracks(StEvent * event, StMcEvent * mcEvent) - INFO - Starting"  << endl;
  // Reset/Clear all data from previous events
  clear();
  // Load data from given event and mcEvent
  loadEvent(event,mcEvent);
  // Proceed to find all track 
  findTracks();
  _messenger << "StiKalmanTrackFinder::findTracks(StEvent*) - INFO - Done"  << endl;
}

void StiKalmanTrackFinder::setParameters(StiKalmanTrackFinderParameters *par)
{
  _pars = par;
  StiKalmanTrack::setParameters(par);
  StiKalmanTrackNode::setParameters(par);
}

EditableParameters * StiKalmanTrackFinder::getParameters() 
{
  return _pars;
}

/// Get the vertex finder used by this track finder
StiVertexFinder * StiKalmanTrackFinder::getVertexFinder()
{
  return _vertexFinder;
}

/// Set the vertex finder used by this tracker
void StiKalmanTrackFinder::setVertexFinder(StiVertexFinder *vertexFinder)
{
  _vertexFinder = vertexFinder;
}

void StiKalmanTrackFinder::loadTracks(StEvent * event)
{
  /* not implemented yet */
}



/*
  _messenger << "StiKalmanTrackFinder() - INFO - Instantiating gui filters" << endl;
  EditableFilter<StiTrack> * guiFilter;
  _guiTrackFilter = _trackFilterFactory->getInstance();
  guiFilter = static_cast< EditableFilter<StiTrack> * >(_guiTrackFilter);
  guiFilter->setName("RecTrackFilter");
  guiFilter->add("Chi2Used", "Use Chi2",     false, false, 0,1,1,Parameter::Boolean, StiTrack::kChi2);
  guiFilter->add("Chi2Min",  "Minimum Chi2", 0., 0., 0., 100.,1,Parameter::Double, StiTrack::kChi2);
  guiFilter->add("Chi2Max",  "Maximum Chi2", 20., 20., 0., 100.,1,Parameter::Double, StiTrack::kChi2);
  
  guiFilter->add("PhiUsed",  "Use Phi",     false, false, 0,1,1,Parameter::Boolean, StiTrack::kPhi);
  guiFilter->add("PhiMin",   "Minimum Phi", 0.,   0.,  0., 6.3,2,Parameter::Double, StiTrack::kPhi);
  guiFilter->add("PhiMax",   "Maximum Phi", 6.3, 6.3, 0., 6.3,2,Parameter::Double, StiTrack::kPhi);
  
  guiFilter->add("PtUsed",   "Use Pt",     false, false, 0,1,1,Parameter::Boolean, StiTrack::kPt);
  guiFilter->add("PtMin",    "Minimum Pt", 0., 0., 0., 100.,2,Parameter::Double, StiTrack::kPt);
  guiFilter->add("PtMax",    "Maximum Pt", 10., 10., 0., 100.,2,Parameter::Double, StiTrack::kPt);
  
  guiFilter->add("PUsed",    "Use P",     false, false, 0,1,1,Parameter::Boolean, StiTrack::kP);
  guiFilter->add("PMin",     "Minimum P", 0., 0., 0., 100.,2,Parameter::Double, StiTrack::kP);
  guiFilter->add("PMax",     "Maximum P", 10., 10., 0., 100.,2,Parameter::Double, StiTrack::kP);
  
  guiFilter->add("EtaUsed",  "Use Eta",     false, false, 0,1,1,Parameter::Boolean, StiTrack::kPseudoRapidity);
  guiFilter->add("EtaMin",   "Minimum Eta", -1.5, -1.5, -10., 10.,2,Parameter::Double, StiTrack::kPseudoRapidity);
  guiFilter->add("EtaMax",   "Maximum Eta",  1.5,  1.5, -10., 10.,2,Parameter::Double, StiTrack::kPseudoRapidity);
  
  guiFilter->add("nPtsUsed", "Use nPts",     false, false, 0,1,1,Parameter::Boolean, StiTrack::kPointCount);
  guiFilter->add("nPtsMin",  "Minimum nPts", 0., 0., 0., 100.,1,Parameter::Integer, StiTrack::kPointCount);
  guiFilter->add("nPtsMax",  "Maximum nPts", 60., 60., 0., 100.,1,Parameter::Integer, StiTrack::kPointCount);
  
  guiFilter->add("nGapsUsed","Use nGaps",     false, false, 0,1,1,Parameter::Boolean, StiTrack::kGapCount);
  guiFilter->add("nGapsMin", "Minimum nGaps", 0., 0., 0., 100.,1,Parameter::Integer, StiTrack::kGapCount);
  guiFilter->add("nGapsMax", "Maximum nGaps", 60., 60., 0., 100.,1,Parameter::Integer, StiTrack::kGapCount);
  
  guiFilter->add("chargeUsed","Use Charge",     false, false, 0,1,1,Parameter::Boolean, StiTrack::kCharge);
  guiFilter->add("chargeMin", "Minimum Charge", -1., -1., -100.,   100.,1,Parameter::Integer, StiTrack::kCharge);
  guiFilter->add("chargeMax", "Maximum Charge",  1.,  1., -100.,   100.,1,Parameter::Integer, StiTrack::kCharge);
  Observer * obs = dynamic_cast<Observer *>(this);
  dynamic_cast<Subject*>(_guiTrackFilter)->attach(obs);
  
  _guiMcTrackFilter = _trackFilterFactory->getInstance();
  guiFilter = static_cast<EditableFilter<StiTrack> * >(_guiMcTrackFilter);
  guiFilter->setName("McTrackFilter");
  guiFilter->add("PhiUsed",  "Use Phi",     false, false, 0,1,1,Parameter::Boolean, StiTrack::kPhi);
  guiFilter->add("PhiMin",   "Minimum Phi", 0.,   0.,  0., 6.3,2,Parameter::Double, StiTrack::kPhi);
  guiFilter->add("PhiMax",   "Maximum Phi", 6.3, 6.3, 0., 6.3,2,Parameter::Double, StiTrack::kPhi);
  guiFilter->add("PtUsed",   "Use Pt",     false, false, 0,1,1,Parameter::Boolean, StiTrack::kPt);
  guiFilter->add("PtMin",    "Minimum Pt", 0., 0., 0., 100.,2,Parameter::Double, StiTrack::kPt);
  guiFilter->add("PtMax",    "Maximum Pt", 10., 10., 0., 100.,2,Parameter::Double, StiTrack::kPt);
  guiFilter->add("PUsed",    "Use P",     false, false, 0,1,1,Parameter::Boolean, StiTrack::kP);
  guiFilter->add("PMin",     "Minimum P", 0., 0., 0., 100.,2,Parameter::Double, StiTrack::kP);
  guiFilter->add("PMax",     "Maximum P", 10., 10., 0., 100.,2,Parameter::Double, StiTrack::kP);
  guiFilter->add("EtaUsed",  "Use Eta",     false, false, 0,1,1,Parameter::Boolean, StiTrack::kPseudoRapidity);
  guiFilter->add("EtaMin",   "Minimum Eta", -1.5, -1.5, -10., 10.,2,Parameter::Double, StiTrack::kPseudoRapidity);
  guiFilter->add("EtaMax",   "Maximum Eta",  1.5,  1.5, -10., 10.,2,Parameter::Double, StiTrack::kPseudoRapidity);
  guiFilter->add("nPtsUsed", "Use nPts",     false, false, 0,1,1,Parameter::Boolean, StiTrack::kPointCount);
  guiFilter->add("nPtsMin",  "Minimum nPts", 0., 0., 0., 100.,1,Parameter::Integer, StiTrack::kPointCount);
  guiFilter->add("nPtsMax",  "Maximum nPts", 60., 60., 0., 100.,1,Parameter::Integer, StiTrack::kPointCount);
  guiFilter->add("chargeUsed","Use Charge",     false, false, 0,1,1,Parameter::Boolean, StiTrack::kCharge);
  guiFilter->add("chargeMin", "Minimum Charge", -1., -1., -100.,   100.,1,Parameter::Integer, StiTrack::kCharge);
  guiFilter->add("chargeMax", "Maximum Charge",  1.,  1., -100.,   100.,1,Parameter::Integer, StiTrack::kCharge);
  dynamic_cast<Subject*>(_guiMcTrackFilter)->attach(obs);
*/
