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
#include "Sti/Base/Parameter.h"
#include "Sti/Base/EditableParameter.h"
#include "Sti/Base/EditableFilter.h"
#include "StiToolkit.h"
#include "StiHit.h"
#include "StiHitContainer.h"
#include "StiDetector.h"
#include "StiDetectorContainer.h"
#include "StiTrackContainer.h"
#include "StiTrack.h"
#include "StiTrackFinder.h"
//#include "StiTrackSeedFinder.h"
#include "StiTrack.h"
#include "StiKalmanTrackFinder.h"
#include "StiTrackContainer.h"
#include "StiKalmanTrack.h"
#include "StiKalmanTrackNode.h"
#include "StiVertexFinder.h"
#include "StiDefaultTrackFilter.h"

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
  trackFilter->add(new EditableParameter("lengthUsed","Use Length", 1., 1., 0.,1.,1.,Parameter::Boolean, StiTrack::kTrackLength));
  trackFilter->add(new EditableParameter("lengthMin", "Min Length", 0., 0., -300.,   300.,2,Parameter::Double, StiTrack::kTrackLength));
  trackFilter->add(new EditableParameter("lengthMax", "Max Length", 300.,  300., -300.,   300.,2,Parameter::Double, StiTrack::kTrackLength));
  _trackFilter = trackFilter;
  _toolkit->setFinderTrackFilter(trackFilter);
  cout << "StiKalmanTrackFinder::initialize() -I- Done"<<endl;
}

StiKalmanTrackFinder::StiKalmanTrackFinder(StiToolkit*toolkit)
  : _toolkit(toolkit),
    _trackFilter(0), 
    _trackSeedFinder(0),
    _trackNodeFactory(0),
    _trackFactory(0),
    _mcTrackFactory(0),
    _hitFactory(0),
    _detectorContainer(0),
    _hitContainer(0),
    _trackContainer(0),
    _mcTrackContainer(0),
    _vertexFinder(0),
    _eventFiller(0),
    _event(0),
    _mcEvent(0)
{
  cout << "StiKalmanTrackFinder::StiKalmanTrackFinder() - Started"<<endl;
  StiKalmanTrack::setParameters(&_pars);
  StiKalmanTrackNode::setParameters(&_pars);
  _pars.setName("KalmanTrackFinderParameters");
  if (!_toolkit)
    throw runtime_error("StiKalmanTrackFinder::StiKalmanTrackFinder(...) - FATAL - toolkit==0");
  cout << "StiKalmanTrackFinder::StiKalmanTrackFinder() - Done"<<endl;
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
  //cout << "StiKalmanTrackFinder::reset() -I- Starting" <<endl;
  _detectorContainer->reset();
  _trackContainer->clear();
  _trackFactory->reset();
  _trackNodeFactory->reset();
  _hitContainer->reset();
  _trackSeedFinder->reset();
  //cout << "StiKalmanTrackFinder::reset() -I- Done" <<endl;
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
  //cout << "StiKalmanTrackFinder::clear() -I- Starting" <<endl;
    _mcTrackContainer->clear();
  _hitContainer->clear();
  _hitFactory->reset();
  _mcTrackFactory->reset();
  reset();  
  //cout << "StiKalmanTrackFinder::clear() -I- Done" <<endl;
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
  if (!_trackContainer)   throw runtime_error("StiKalmanTrackFinder::findTracks() -F- _trackContainer==0");
  if (!_trackSeedFinder)  throw runtime_error("StiKalmanTrackFinder::findTracks() -F- _trackSeedFinder==0");
  StiTrack * track;
  _trackSeedFinder->reset();
  _trackContainer->clear();
  if (_trackFilter) _trackFilter->reset();
  try
    {
      while (true)
	{ 
	  try
	    {
	      // obtain track seed from seed finder
	      track = _trackSeedFinder->findTrack();
	      if (!track) break; // no more seeds
	      track->find();
	      if (!_trackFilter || _trackFilter->filter(track)) 
		{
		  _trackContainer->push_back(track);
		  static_cast<StiKalmanTrack*>(track)->reserveHits();
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
  //if (_trackFilter)
  //cout << "  Tracks Analyzed:"<< _trackFilter->getAnalyzedCount() << endl
  //			 << "         Accepted:"<< _trackFilter->getAcceptedCount() << endl;
  //else
  //  cout << "SKTF::findTracks() -I- Done"<<endl;
}
  
/* Fit all track produced by the track seed finder. 
  <p>
 This method is useful when the seed finder returns full tracks produced
 by a 3rd party track finder e.g. the tpt package.
 <p>Fitted tracks are added to the track container if no track 
 filter is set or if they satisfy the track filter requirements. 
void StiKalmanTrackFinder::fitTracks()
{
  StiTrack * track = 0;
  try 
    {
      track = _trackSeedFinder->findTrack();
      if (track) 
	{ 
	  track->fit(kOutsideIn); track->setFlag(0);
	  if (!_trackFilter || _trackFilter->filter(track))  _trackContainer->push_back(track);
	}
    }
  catch (runtime_error & rte) 
    {
      cout << "StiKalmanTrackFinder::fitTracks() - Run Time Error :" << rte.what() << endl;
    }
}
*/

/*
 Extend all known tracks to primary vertex
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
    cout << "SKTF::extendTracksToVertex() - vertex position " << vertex->x_g() << ", " << vertex->y_g() << ", " << vertex->z_g() << endl;

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
  //cout << "SKTF::find(StiTrack * t) -I- Started" << endl;
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
  StiHit* hhh = sNode->getHit(); if (hhh){}
  sDet = sNode->getDetector();
  if (sDet==0) throw logic_error("SKTF::find(StiTrack*) - FATAL - sDet==0");
  tNode   = 0;
  tDet    = 0;
  leadDet = sDet;

	////

  StiKalmanTrackNode testNode;
  StiDetector * currentDet;
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
		debug = false;
	  if (position<0)
	    { // not reaching this detector layer - stop track
	      if(debug)cout << "TRACK DOES NOT REACH CURRENT LAYER"<<endl;
	      trackDone = true; break;
	    }
	  else if (position<=kEdgeZplus) 
	    { 
				//cout << "position<=kEdgeZplus" << endl;
	      testNode.setDetector(tDet);
	      bool active = tDet->isActive();
	      if (active&&(testNode.nullCount<_pars.maxNullCount&&testNode.contiguousNullCount<_pars.maxContiguousNullCount)) 
					{ 
						// active detector may have a hit
						vector<StiHit*> & candidateHits = _hitContainer->getHits(testNode,true);
						//vector<StiHit*> & candidateHits = _hitContainer->getHits(tDet);
						vector<StiHit*>::iterator hitIter;
						//cout << "---------  candidates:"<< candidateHits.size() << endl;
						for (hitIter=candidateHits.begin();hitIter!=candidateHits.end();++hitIter)
							{
								stiHit = *hitIter;
								chi2 = testNode.evaluateChi2(stiHit);
								if (chi2<maxChi2 && chi2<testNode.getChi2())
									{
										testNode.setHit(stiHit); testNode.setChi2(chi2);
									}
							} 
					}
	      StiKalmanTrackNode * node = _trackNodeFactory->getInstance();
				if (node==0) throw logic_error("SKTF::find() - ERROR - node==null");
	      node->reset();
	      *node = testNode;
	      sNode = track->add(node);
				bool gotHit=false;
	      if (node->getHit())
					{
						nAdded++;
						node->hitCount = sNode->hitCount+1;
						node->contiguousHitCount = sNode->contiguousHitCount+1; 
						if (node->contiguousHitCount>_pars.minContiguousHitCountForNullReset)
							node->contiguousNullCount = 0;
						else
							node->contiguousNullCount = sNode->contiguousNullCount;
						node->nullCount = sNode->nullCount;
						gotHit = true;
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
				leadDet = sNode->getDetector();
				//break;
				
				double cut = 3.;
					if (gotHit || testNode._refX>cut)
					{
						//cout << "gotHit || testNode._refX>"<< cut << endl;
						break;//scanning is done
					}
					else
						{
							//cout << "no HIT && _refX<="<<cut << endl;
							
							if (lastMove==0)
								{
									_detectorContainer->movePlusPhi();//cout << " MOVE PLUS PHI for r<="<<cut<<endl;
									lastMove=1;
								}
							else if (lastMove==1)
								{
									_detectorContainer->moveMinusPhi();// cout << " MOVE MINUS PHI for r<="<<cut<<endl;
									lastMove=-1;
								}
							else
								{
									//cout << " NO MORE MOVES LEFT" << endl;
									break;
								}
							
							//break;
					}
	    }
	  else if ( (position==kEdgePhiPlus || position==kMissPhiPlus)  && lastMove>=0 )
	    {
				//cout << " testNode._refX:" << testNode._refX << endl;
	      //cout << " MOVE PLUS PHI"<<endl;
	      _detectorContainer->movePlusPhi(); lastMove++;
	    }
	  else if ( (position==kEdgePhiMinus || position==kMissPhiMinus) && lastMove<=0)
	    {
				//cout << " testNode._refX:" << testNode._refX << endl;
	      //cout << " MOVE MINUS PHI"<<endl;
	      _detectorContainer->moveMinusPhi(); lastMove--;
	    }
	  if (abs(lastMove)>4) break; // xxxxxxx 
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


StiTrack * StiKalmanTrackFinder::findTrack()
{
  StiTrack * track = 0;
  try 
    {
      if (!_trackSeedFinder) throw runtime_error("StiKalmanTrackFinder::findTrack() -E- No Track seed finder instance available");
      track = _trackSeedFinder->findTrack();
      if (track)
	{ 
	  track->find();
	  if (!_trackFilter || _trackFilter->filter(track) ) _trackContainer->push_back(track);	
	} 
    }
  catch (runtime_error & rte) 
    {
      cout << "StiKalmanTrackFinder::findTrack() - Run Time Error :\n" << rte.what() << endl;
    }
  return track;
}

/*
void StiKalmanTrackFinder::fitNextTrack()
{
  StiTrack * track = 0;
  try 
    {
      track = _trackSeedFinder->findTrack();
      if (track)
	{ 
	  track->fit(kOutsideIn);
	  if (!_trackFilter || _trackFilter->filter(track) ) _trackContainer->push_back(track);
	}
    }
  catch (runtime_error & rte) 
    {
      cout << "StiKalmanTrackFinder::fitNextTrack() - Run Time Error :" << rte.what() << endl;
    }
}
*/

void StiKalmanTrackFinder::setParameters(const StiKalmanTrackFinderParameters & par)
{
  _pars = par;
}

EditableParameters & StiKalmanTrackFinder::getParameters() 
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


void StiKalmanTrackFinder::loadDS(TDataSet&ds)
{
  cout << "StiKalmanTrackFinder::loadDS(TDataSet*ds) -I- Starting" << endl;
  _pars.loadDS(ds); 
  cout << "StiKalmanTrackFinder::loadDS(TDataSet*ds) -I- Done" << endl;
}

void StiKalmanTrackFinder::loadFS(ifstream & iFile)
{
  cout << "StiKalmanTrackFinder::loadFS(ifstream&) -I- Starting" << endl;
  _pars.loadFS(iFile); 
  cout << "StiKalmanTrackFinder::loadFS(ifstream&) -I- Done" << endl;
}


void StiKalmanTrackFinder::setDefaults()
{
  cout << "StiKalmanTrackFinder::setDefaults() -I- Starting" << endl;
  _pars.setDefaults(); 
  cout << "StiKalmanTrackFinder::setDefaults() -I- Done" << endl;
}

