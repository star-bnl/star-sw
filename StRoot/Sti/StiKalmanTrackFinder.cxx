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
#include "Stiostream.h"
#include <stdexcept>
#include <math.h>
using namespace std;
#include "Sti/Base/Messenger.h"
#include "Sti/Base/Parameter.h"
#include "Sti/Base/EditableParameter.h"
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
#include "StiDefaultTrackFilter.h"
#include "StiMasterDetectorBuilder.h"

ostream& operator<<(ostream&, const StiTrack&);
void StiKalmanTrackFinder::initialize()
{
  cout << "StiKalmanTrackFinder::initialize() -I- Started"<<endl;
  _toolkit = StiToolkit::instance();
  _trackNodeFactory  = _toolkit->getTrackNodeFactory(); 
  _trackFactory      = _toolkit->getTrackFactory();
  _mcTrackFactory    = _toolkit->getMcTrackFactory(); 
  _hitFactory        = _toolkit->getHitFactory(); 
  _detectorContainer = _toolkit->getDetectorContainer(); 
  _detectorContainer->reset();
  _trackSeedFinder   = _toolkit->getTrackSeedFinder();
  _hitLoader         = _toolkit->getHitLoader();
  _hitContainer      = _toolkit->getHitContainer();
  _trackContainer    = _toolkit->getTrackContainer();
  _mcTrackContainer  = _toolkit->getMcTrackContainer();
  _vertexFinder      = _toolkit->getVertexFinder();
  StiDefaultTrackFilter * trackFilter = new StiDefaultTrackFilter("FinderTrackFilter","Reconstructed Track Filter");
  trackFilter->add( new EditableParameter("nPtsUsed","Use nPts", 1., 1., 0., 1., 1.,
					  Parameter::Boolean, StiTrack::kPointCount) );
  trackFilter->add( new EditableParameter("nPtsMin", "Minimum nPts", 10., 10., 0., 100.,1., 
					  Parameter::Integer,StiTrack::kPointCount) );
  trackFilter->add( new EditableParameter("nPtsMax", "Maximum nPts", 60., 60., 0., 100.,1., 
					  Parameter::Integer,StiTrack::kPointCount) );
  _trackFilter = trackFilter;
  _toolkit->setFinderTrackFilter(trackFilter);
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
  _messenger << "StiKalmanTrackFinder::reset() -I- Starting" <<endl;
  _detectorContainer->reset();
  _trackContainer->clear();
  _trackFactory->reset();
  _trackNodeFactory->reset();
  _hitContainer->reset();
  _trackSeedFinder->reset();
  _messenger << "StiKalmanTrackFinder::reset() -I- Done" <<endl;
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
  _messenger << "StiKalmanTrackFinder::clear() -I- Starting" <<endl;
    _mcTrackContainer->clear();
  _hitContainer->clear();
  _hitFactory->reset();
  _mcTrackFactory->reset();
  reset();  
  _messenger << "StiKalmanTrackFinder::clear() -I- Done" <<endl;
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
  _trackSeedFinder->reset();
  _trackContainer->clear();
  if (_trackFilter) _trackFilter->reset();
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
	      if (!_trackFilter || _trackFilter->filter(track)) 
		{
		  _trackContainer->push_back(track);
		  track->reserveHits();
		}
	    }
	  catch (runtime_error & rte)
	    {
	      cout<< "StiKalmanTrackFinder::findTracks() - Run Time Error :" << rte.what() << endl;
	    }
	}
    }
  catch (runtime_error & rte)
    {
      cout << "StiKalmanTrackFinder::findTracks() - Run Time Error (2) :" << rte.what() << endl;
    }  
  if (_trackFilter)
    cout << "  tracks Analyzed:"<< _trackFilter->getAnalyzedCount() << endl
	 << "  tracks Accepted:"<< _trackFilter->getAcceptedCount() << endl;
  else
    cout << "SKTF::findTracks() -I- Done"<<endl;
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
	  if (!_trackFilter || _trackFilter->filter(track))
	    _trackContainer->push_back(track);
	}
    }
  catch (runtime_error & rte) 
    {
      cout << "StiKalmanTrackFinder::fitTracks() - Run Time Error :" << rte.what() << endl;
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
  for (TrackToTrackMap::const_iterator it=_trackContainer->begin(); 
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
	  cout << "SKTF::extendTracksToVertex()"
	       << "-W- Run Time Error while extending a track to main vertex."<<endl
	       << "Error Message:" << endl
	       << rte.what() << endl;
	}
    }
  cout << "SKTF::extendTracksToVertex(StiHit* vertex) -I- rawCount:"<<rawCount<<endl
       << "                                          extendedCount:"<<goodCount<<endl
       << "                                                   plus:"<<plus<<endl
       << "                                                  minus:"<<minus<<endl
       << "                                                helPlus:"<<helPlus<<endl
       << "                                               helMinus:"<<helMinus<<endl<<endl<<endl<<endl<<endl<<endl<<endl;
}

/// Find extension (track) to the given track seed
/// Return Ok      if operation was successful
/// Return Error   if given seed "t" is invalid
///                or if input data are invalid or if some other 
///                internal error has occured.
bool StiKalmanTrackFinder::find(StiTrack * t, int direction) // throws runtime_error, logic_error
{
  _messenger << "SKTF::find(StiTrack * t) -I- Started" << endl;
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
  bool debug=false;
  if (!sNode) throw logic_error("SKTF::find()\t - ERROR - track last node ==0");
  StiHit* hhh = sNode->getHit();
  sDet = sNode->getDetector();
  if (sDet==0) throw logic_error("SKTF::find(StiTrack*) - FATAL - sDet==0");
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
      //_messenger << tDet->getName()<<" ACTIVE:" << tDet->isActive() << endl;
	  
      leadDet = tDet;
      // tDet==0 implies a severe detector container error - exit
      // tDet=currentDet implies there are no more new volumes to go to - exit loop
      if (tDet==0) throw logic_error("SKTF::find() - ERROR -  **_detectorContainer ==0");
      if (tDet==currentDet) break;
      double maxChi2 = tDet->getTrackingParameters()->getMaxChi2ForSelection();
      lastMove     = 0;
      scanningDone = false;
      // loop over possible volumes
      while (!scanningDone)
	{
	  testNode.reset();
	  testNode.setChi2(1e50);
	  position = testNode.propagate(sNode,tDet);
	  if(debug)cout << "tDet:" << *tDet << "  POSITION:"<<position<<endl;
	  if (position<0)
	    { // not reaching this detector layer - stop track
	      if(debug)cout << "TRACK DOES NOT REACH CURRENT LAYER"<<endl;
	      trackDone = true; break;
	    }
	  else if (position<=kEdgeZplus) 
	    { 
	      testNode.setDetector(tDet);
	      bool active = tDet->isActive();
	      if (active&&(testNode.nullCount<_pars->maxNullCount&&testNode.contiguousNullCount<_pars->maxContiguousNullCount)) 
		{ // active detector may have a hit
		  _hitContainer->setRefPoint(testNode);
		  while (_hitContainer->hasMore()) 
		    {  
		      stiHit = _hitContainer->getHit();if (!stiHit) throw logic_error("StiKalmanTrackFinder::doNextDetector() - FATAL - StiHit*hit==0");
		      chi2 = testNode.evaluateChi2(stiHit);
		      //if (chi2<_pars->maxChi2ForSelection && chi2<testNode.getChi2())
		      if (chi2<maxChi2 && chi2<testNode.getChi2())
			{
			  testNode.setHit(stiHit); testNode.setChi2(chi2);
			}
		    } 
		}
	      StiKalmanTrackNode * node = _trackNodeFactory->getInstance();if (node==0) throw logic_error("SKTF::find() - ERROR - node==null");
	      node->reset();
	      *node = testNode;
	      sNode = track->add(node);
	      if (node->getHit())
		{
		  nAdded++;
		  node->hitCount = sNode->hitCount+1;
		  node->contiguousHitCount = sNode->contiguousHitCount+1; 
		  if (node->contiguousHitCount>_pars->minContiguousHitCountForNullReset)
		    node->contiguousNullCount = 0;
		  else
		    node->contiguousNullCount = sNode->contiguousNullCount;
		  node->nullCount = sNode->nullCount;
		}
	      else if (position>0 || !active) // detectors edge - don't really expect a hit here
		{
		  node->nullCount           = sNode->nullCount;
		  node->contiguousNullCount = sNode->contiguousNullCount;
		  node->hitCount            = sNode->hitCount;
		  node->contiguousHitCount  = 0;
		} 
	      else // there should have been a hit but we found none
		{
		  node->nullCount           = sNode->nullCount+1;
		  node->contiguousNullCount = sNode->contiguousNullCount+1;
		  node->hitCount            = sNode->hitCount;
		  node->contiguousHitCount  = 0;
		} 
	      leadDet = sNode->getDetector(); break;
	    }
	  else if ( (position==kEdgePhiPlus || position==kMissPhiPlus)  && lastMove>=0 )
	    {
	      //cout << " MOVE PLUS PHI"<<endl;
	      _detectorContainer->movePlusPhi(); lastMove++;
	    }
	  else if ( (position==kEdgePhiMinus || position==kMissPhiMinus) && lastMove<=0)
	    {
	      //cout << " MOVE MINUUS PHI"<<endl;
	      _detectorContainer->moveMinusPhi(); lastMove--;
	    }
	  if (abs(lastMove)>2) break;
	  nextDet = **_detectorContainer;
	  if (nextDet==0 || tDet==nextDet) 
	    { //cout <<" SCANNING DONE"<<endl;
	      scanningDone = true;
	    }
	  else
	    tDet = nextDet;
	} // scanningDone
    } // trackDone
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
  TrackToTrackMap::const_iterator it;
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

