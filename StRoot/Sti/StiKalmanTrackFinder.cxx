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
#include <iostream>
#include <stdexcept>
#include <math.h>
using namespace std;
using std::cout;
using std::endl;

//Sti
#include "Sti/Base/Parameter.h"
#include "Sti/EditableFilter.h"
#include "StiHitLoader.h"
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
  //_vertexFinder     = _toolkit->getVertexFinder();
  _eventFiller       = new StiStEventFiller();
}

StiKalmanTrackFinder::StiKalmanTrackFinder(StiToolkit*toolkit)
  : Observer(StiIOBroker::instance()),
    _toolkit(toolkit),
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
    _messenger(*Messenger::instance(MessageType::kTrackMessage)),
    mode(StepByLayer),
    state(0),
    visitedDet(0),
    position(0),
    lastMove(0),
    chi2(0.),
    bestChi2(0.),
    track(0),
    sNode(0),
    tNode(0),
    bestNode(0),
    leadNode(0),
    sDet(0),
    tDet(0),
    leadDet(0),
    trackDone(true),
    scanningDone(true),
    hasHit(false),
    hasDet(false)
{
  if (!_toolkit)
    throw runtime_error("StiKalmanTrackFinder::StiKalmanTrackFinder(...) - FATAL - toolkit==0");

  StiIOBroker *  broker = _toolkit->getIOBroker();
  Factory< Filter<StiTrack>  >* _trackFilterFactory = _toolkit->getTrackFilterFactory();
  _trackFilter      = _trackFilterFactory->getInstance();
  if (broker->useGui())
    {
      cout << "StiKalmanTrackFinder() - INFO - Instantiating gui filters" << endl;
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
    }
  else
    cout << "StiKalmanTrackFinder() - INFO - No GUI filters selected" << endl;

  //Messenger::instance()->clearRoutingBits(MessageType::kTrackMessage);
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
  StiIOBroker *  broker = _toolkit->getIOBroker();
  //cout <<"StiKalmanTrackFinder::getNewState()"<<endl;
  _pars->setMCSCalculated(broker->ktfMcsCalculated()); //check
  _pars->setElossCalculated(broker->ktfElossCalculated()); //check
  _pars->setMaxChi2ForSelection(broker->ktfMaxChi2ForSelection());//check
  _pars->setField(broker->ktfBField()); //check
  _pars->setMassHypothesis(broker->ktfMassHypothesis()); //check
  _pars->setMinContiguousHitCount(broker->ktfMinContiguousHitCount());  //check
  _pars->setMaxNullCount(broker->ktfMaxNullCount()); //check
  _pars->setMaxContiguousNullCount(broker->ktfMaxContiguousNullCount()); //check
  _pars->setMinSearchWindow(broker->ktfMinSearchRadius()); //check
  _pars->setMaxSearchWindow(broker->ktfMaxSearchRadius()); //check
  _pars->setSearchWindowScale(broker->ktfSearchWindowScale()); //check
}

/*!
 Reset the state of the finder  to "event not tracked"
 <p>
 The track factory, the track container are reset. This
 method is distinct from the "clear" method which reset 
 the state to "event not loaded".
 */
void StiKalmanTrackFinder::reset()
{
  cout << "StiKalmanTrackFinder::reset() - INFO - Starting" <<endl;
  _detectorContainer->reset();
  _trackFactory->reset();
  _trackNodeFactory->reset();
  _trackContainer->clear();
  cout << "StiKalmanTrackFinder::reset() - INFO - Done" <<endl;
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
  cout << "StiKalmanTrackFinder::clear() - INFO - Starting" <<endl;
  _hitContainer->clear();
  _detectorContainer->reset();
  _hitFactory->reset();
  _trackFactory->reset();
  _mcTrackFactory->reset();
  _trackNodeFactory->reset();
  _trackContainer->clear();
  _mcTrackContainer->clear();
  cout << "StiKalmanTrackFinder::clear() - INFO - Done" <<endl;
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
  StiKalmanTrack * track;
  // find global tracks
  while (_trackSeedFinder->hasMore())
    { 
      // obtain track seed from seed finder
      track = _trackSeedFinder->next();
      try
	{
	  track->find();
	  StiDrawableTrack * t = dynamic_cast<StiDrawableTrack *>(track);
	  if (t) t->update();
	  if (_pars->useTrackFilter && _trackFilter->filter(track)) 
	    _trackContainer->push_back(track);
	  else
	    _trackContainer->push_back(track);
	}
      catch (runtime_error & rte)
	{
	  cout<< "StiKalmanTrackFinder::findTracks() - Run Time Error :" << rte.what() << endl;
	}
    }
  try
    {
      // Fill tracks into StEvent
      if (_eventFiller)
	_eventFiller->fillEvent(_event, _trackContainer);
      StiHit *vertex;
      if (_vertexFinder)
	vertex = _vertexFinder->findVertex(_event);
      if (vertex)
	{
	  extendTracksToVertex(vertex);
	  if (_eventFiller)
	    _eventFiller->fillEventPrimaries(_event, _trackContainer);
	}
    }
  catch (runtime_error & rte2)
    {
      cout<< "StiKalmanTrackFinder::findTracks() - Run Time Error :" << rte2.what() << endl;
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
	  if (_pars->useTrackFilter && _trackFilter->filter(track)) 
	    _trackContainer->push_back(track);
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
  caught here and reported with "cout".
*/
void StiKalmanTrackFinder::extendTracksToVertex(StiHit* vertex)
{
  track = 0;
  for (TrackMap::const_iterator it=_trackContainer->begin(); 
       it!=_trackContainer->end(); 
       ++it) 
    {
      track = static_cast<StiKalmanTrack*>((*it).second);
      try
	{
	  track->extendToVertex(vertex);
	}
      catch (runtime_error & rte) 
	{
	  _messenger << "SKTF::extendTracksToVertex()"
		     << "- WARNING - Run Time Error while extending a track to main vertex."<<endl
		     << "Error Message:" << endl
		     << rte.what() << endl;
	}
    }
}


/// Find extension (track) to the given track seed
/// Return Ok      if operation was successful
/// Return Error   if given seed "t" is invalid
///                or if input data are invalid or if some other 
///                internal error has occured.
bool StiKalmanTrackFinder::find(StiTrack * t, int direction) // throws runtime_error, logic_error
{
  _messenger << "SKTF::find(StiTrack * t) - Beginning" << endl;
  track = dynamic_cast<StiKalmanTrack *> (t);
  if (!track) 
      throw logic_error("SKTF::find()\t - ERROR - dynamic_cast<StiKalmanTrack *>  returned 0");
  nAdded       = 0;
  state        = 0;
  trackDone    = false;
  scanningDone = true;
  sNode   = track->getLastNode(); 
  sDet    = sNode->getHit()->detector();
  tNode   = 0;    // target node
  tDet    = 0;
  leadDet = 0;
  if (sDet==0) throw logic_error("SKTF::find(StiTrack*) - Error - sDet==0");
  leadDet = sDet;
  while (!trackDone) 
    {
      doInitLayer(direction); //cout<<"init layer done"<<endl;
      while (!scanningDone)
	{
	  doNextDetector();
	  if (nAdded>50) throw runtime_error("StiKalmanTrackFinder::find() - ERROR - nAdded>50");
	}
      doFinishLayer(); 
      
    }
  trackDone = true;
  return nAdded>0;
}

void StiKalmanTrackFinder::doInitLayer(int trackingDirection)
{
  if (trackDone || !scanningDone) return; // nothing to do
  _detectorContainer->setToDetector(leadDet);
  StiDetector * currentDet = **_detectorContainer;
  if (trackingDirection==kOutsideIn)
    _detectorContainer->moveIn();
  else
    _detectorContainer->moveOut();
  tDet = **_detectorContainer;
  leadDet = tDet;
  if (tDet==0) 
    throw logic_error("SKTF::doInitLayer() ERROR - tDet==0");
  else if (tDet==currentDet) 
    trackDone = true;
  else
    {
      position     = 0;
      lastMove     = 0;
      hasDet = false;
      hasHit = false;
      scanningDone = false;
      bestChi2     = 1e50;
      bestNode     = 0;  
    }
}


void StiKalmanTrackFinder::doNextDetector()
{
  if (trackDone || scanningDone) return;
  tNode = _trackNodeFactory->getInstance();
  if (tNode==0) throw logic_error("SKTF::doNextDetector()\t- ERROR - tNode==null");
  tNode->reset();
  try
    {
      position = tNode->propagate(sNode, tDet); 
    }
  catch (runtime_error & rte)
    {
      _messenger << "SKTF::doNextDetector() - RunTimeError: " << rte.what();
      scanningDone = true;
      trackDone = true;
      return;
    }
  _messenger << "TargetDet :"<<*tDet<<endl
	     << "TargetNode:"<<*tNode<<endl;
  if (position<0)
    {
      scanningDone = true;
      trackDone = true;
    }
  cout <<" ---------------------------------------------------------->>>>>"<<endl;
  
  if (position<=kEdgeZplus) 
    {
      cout <<" let's find hits ----------------------"<<endl;
      hasDet = true;
      leadNode = tNode;
      leadNode->setDetector(tDet);
      if (tDet->isActive()) 
	{ // active vol, look for hits
	  cout <<"active volume - OK"<<endl;
	  if (position==kHit) scanningDone = true;
	  _hitContainer->setDeltaD(tNode->getWindowY());
	  _hitContainer->setDeltaZ(tNode->getWindowZ());
	  cout << "NODE:"<<*tNode;
	  cout << " ============= COntainer data ============" << endl;
	  
	  hitvector hv = _hitContainer->hits(tDet);
	  hitvector::const_iterator hititer;
	  for (hititer=hv.begin();hititer!=hv.end();hititer++)
	    {
	      cout << *(*hititer) << endl;
	    }
	  _hitContainer->setRefPoint(tNode->_refX,tNode->_alpha,tNode->_p0,tNode->_p1);
	  bool hasMore = _hitContainer->hasMore();
	  cout <<"\nCONTAINER HAS HITS:"<<hasMore<<endl;
	  while (hasMore) 
	    {
	      cout <<"trying hits"<<endl;
	      StiHit * h = _hitContainer->getHit();
	      if (!h) throw logic_error("StiKalmanTrackFinder::doNextDetector() - FATAL - StiHit*hit==0");
	      tNode->setHit(h);
	      chi2 = tNode->evaluateChi2();
	      _messenger << "SKTF::followTrackAt()\t chi2:" << chi2  << endl;
	      if (chi2<_pars->maxChi2ForSelection && chi2 < bestChi2)
		{
		  _messenger << "SKTF::followTrackAt()\t selected hit - chi2:" << chi2 << endl;
		  hasHit = true;
		  bestChi2 = chi2;
		  bestNode = tNode;
		}
	      hasMore = _hitContainer->hasMore();
	      if (hasMore) // prepare new node
		{
		  StiKalmanTrackNode * newNode = _trackNodeFactory->getInstance();
		  if (newNode==0) 
		    throw logic_error("SKTF::followTrackAt()\t- ERROR - newNode==null");
		  newNode->reset();   
		  newNode->setState(tNode); // get everything from tNode
		  newNode->setDetector(tDet); // set the local pointer to tDet
		  tNode = newNode;  // not a memory leak because the factory handles the objects.... ;-)
		}
	    } // searching best hit
	}
      else
	{
	  cout <<"projection in inactive volume, scanning is done"<<endl;
	  scanningDone = true;
	}
    }
  else if (position==kFailed) 
    {
      _messenger << "SKTF::doNextDetector()\t - position==kFailed" << endl;
      scanningDone = true;
      trackDone = true;
      return;
    }
  if (!scanningDone)
    {
      StiDetector * nextDet;
      if (position==kEdgePhiPlus || position==kMissPhiPlus)
	{
	  if (lastMove>=0 )
	    {
	      _messenger << "SKTF::followTrackAt()\t- movePlusPhi()" << endl;
	      _detectorContainer->movePlusPhi();   
	      nextDet = **_detectorContainer;
	      if (nextDet==0 || tDet==nextDet) 
		scanningDone = true;
	      tDet = nextDet;
	      lastMove++;
	    }
	  else
	    {
	      scanningDone = true;
	      _messenger << "SKTF::followTrackAt()\t-position==kEdgePhiPlus||kMissPhiPlus - but no PlusPhi done" << endl;
	    }
	  
	}
      else if (position==kEdgePhiMinus || position==kMissPhiMinus)
	{
	  if (lastMove<=0)
	    {
	      _messenger << "SKTF::followTrackAt()\t- moveMinusPhi()" << endl;
	      _detectorContainer->moveMinusPhi();
	      nextDet = **_detectorContainer;
	      if (nextDet==0 || tDet==nextDet) 
		{
		  //_messenger << "SKTF::followTrackAt() - ERROR  -  moveMinusPhi() >> tDet==sDet"  << endl;
		  scanningDone = true;
		}
	      tDet = nextDet;
	      lastMove--;
	    }
	  else
	    {
	      scanningDone = true;
	      _messenger << "SKTF::followTrackAt()\t-position==kEdgePhiMinus||kMissPhiMinus - but no MinusPhi done" << endl;
	    }
	}
      else
	{
	  _messenger <<  "SKTF::followTrackAt()\t- Scanning set to done" << endl;
	  scanningDone = true;
	}
    }
  if (abs(lastMove)>2) scanningDone = true;
}

void StiKalmanTrackFinder::printState()
{
  _messenger << "State:"<<state;
  if (scanningDone) 
    _messenger << "/Scanning Done";
  else
    _messenger << "/Scanning NOT Done";
  if (trackDone) 
    _messenger << "/Track Done"<<endl;
  else
    _messenger << "/Track NOT Done" << endl;
}

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
	  track->setLastNode(bestNode);
	  leadDet = bestNode->getDetector();
	  _messenger << "SKTF::doFinishLayer() - Adding node with hit in det:" << *leadDet << endl;
	  nAdded++;
	}
      else // no hit found
	{
	  leadDet = leadNode->getDetector();
	  if (leadDet==0)
	    _messenger << "SKTF::doFinishLayer() - Fatal Error - leadDet==0" << endl;
	  else
	    _messenger << "SKTF::doFinishLayer() - Adding node WITHOUT hit in det:"  
		     << *leadDet << endl;
	  sNode->add(leadNode);
	  sNode = leadNode;
	  track->setLastNode(leadNode);
	  _messenger << "SKTF::doFinishLayer() "
		     << "               nullCount:" << leadNode->nullCount << endl
		     << "    contiguousNullCount :" << leadNode->contiguousNullCount << endl;
	  if (leadNode->nullCount>_pars->maxNullCount ||
	      leadNode->contiguousNullCount>_pars->maxContiguousNullCount)
	    trackDone = true;    
	}
    }
  else // no det crossing found
    {
      _messenger << "SKTF::doFinishLayer() - Layer Completed with no node added" << endl;
    }
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

void StiKalmanTrackFinder::doNextTrackStep()
{
  try
    {
      switch (state)
	{
	case 0: _messenger<< "InitTrackSearch()"<< endl;
	  track = 0;
	  if (_trackSeedFinder->hasMore()) 
	    { 
	      //Redundant check, but it protectes against naive calls
	      track = _trackSeedFinder->next();
	      if (!track) 
		throw logic_error("StiKalmanTrackFinder::doNextTrackStep() - FATAL - _trackSeedFinder->next()==0");
	      sNode = track->getLastNode();
	      if (!sNode) 
		throw logic_error("SKTF::doNextTrackStep()\t - ERROR - track->getLastNode() returned 0");
	      if (!trackDone) return;
	      if (_detectorContainer==0) 
		throw logic_error("SKTF::doNextTrackStep() - Error - _detectorContainer==0");
	      tNode   = 0;    // target node
	      leadDet = 0;
	      trackDone = false;
	      scanningDone = true;
	      sDet  = sNode->getHit()->detector();
	      if (sDet==0) 
		throw logic_error("SKTF::doNextTrackStep() - Error - sDet==0");
	      tDet=0;
	      leadDet = sDet;		 
	    }
	  else 
	    {
	      _messenger <<"\ttrackSeedFinder->hasMore()==false"<<endl;
	    }
	  state++; break;
	case 1: doInitLayer(track->getTrackingDirection());
	  while (!scanningDone)
	    {
	      doNextDetector();
	    }
	  doFinishLayer();
	  if (trackDone)
	    state=0;
	  break;
	}
      _messenger << "Track state:" << state << endl;
       StiDrawableTrack * t = dynamic_cast<StiDrawableTrack *>(track);
       t->update();
    }
  catch (runtime_error & rte)
    {
      _messenger << "StiKalmanTrackFinder::doNextTrackStep() - WARNING - RunTime Error Exception: "
		 << rte.what();
    }
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
	  if (!track)
	    throw runtime_error("TrackSeedFinder->next() returned 0");
	  track->find();
	  if (_pars->useTrackFilter && _trackFilter->filter(track)) {
	      _trackContainer->push_back(track);
	      //this next bit is bad design.  casting doesn't count as polymorphism.  downcast == bad!
	      StiDrawableTrack * t = dynamic_cast<StiDrawableTrack *>(track);
	      if (t) {
		  t->update();
	      }
	  }
	  else {
	      _trackContainer->push_back(track);
	      StiDrawableTrack * t = dynamic_cast<StiDrawableTrack *>(track);
	      if (t) {
		  t->update();
	      }
	  }
	} 
      else {
	  _messenger <<"StiKalmanTrackFinder::findNextTrack() - INFO - trackSeedFinder->hasMore()==false"<<endl;
      }
    }
  catch (runtime_error & rte) 
      {
	  _messenger << "StiKalmanTrackFinder::findNextTrack() - Run Time Error :\n" << rte.what() << endl;
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
	  if (_pars->useTrackFilter && _trackFilter->filter(track)) 
	    _trackContainer->push_back(track);
	  else
	    _trackContainer->push_back(track);
       StiDrawableTrack * t = dynamic_cast<StiDrawableTrack *>(track);
       t->update();
       //	  track->update();  //This updates the track on the display
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
  //cout << "StiKalmanTrackFinder::update() - INFO - Starting."<<endl;
  TrackMap::const_iterator iter;
  StiDrawableTrack * t;

  StiToolkit::instance()->getDisplayManager()->reset();
  StiToolkit::instance()->getDisplayManager()->draw();
  StiToolkit::instance()->getDisplayManager()->update();
  // Monte Carlo Tracks
  if (_mcTrackContainer)
    {
      //cout << "StiKalmanTrackFinder::update() - INFO - Looping on MC tracks."<<endl;
      for (iter=_mcTrackContainer->begin();iter!=_mcTrackContainer->end(); ++iter) 
	{
	   if (_guiMcTrackFilter)
	     {
	       //cout << "StiKalmanTrackFinder::update() - INFO - Filter OK/";
	       if (_guiMcTrackFilter->filter((*iter).second))
		 {
		   //cout <<"Passed"<<endl;
		   t = dynamic_cast<StiDrawableTrack *>((*iter).second);
		   if (t)
		     t->update();
		   else
		     cout << "StiKalmanTrackFinder::update() - WARNING - MC dynamic_cast failed." << endl;
		 }
	     }	
	   else
	     cout << "StiKalmanTrackFinder::update() - INFO - Filter NOK"<<endl;
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
	    cout << "StiKalmanTrackFinder::update() - WARNING - Kalman dynamic_cast failed." << endl;
	}
    }
  StiToolkit::instance()->getDisplayManager()->draw();
  StiToolkit::instance()->getDisplayManager()->update();
  //cout << "StiKalmanTrackFinder::update() - INFO - Done."<<endl;
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
      //cout << "Loading StMcTrack into _mcTrackContainer" << endl;
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
  cout << "StiKalmanTrackFinder::findTracks() - INFO - Starting"  << endl;
  // Reset/Clear all data from previous events
  reset();
  // Load data from given event and mcEvent
  loadEvent(event,mcEvent);
  // Proceed to find all track 
  findTracks();
  cout << "StiKalmanTrackFinder::findTracks(StEvent*) - INFO - Done"  << endl;
}

void StiKalmanTrackFinder::changed(Subject* changedSubject)
{
  cout << "StiKalmanTrackFinder::changed(Subject* changedSubject)" << endl;
  getNewState();
  update();
}

void StiKalmanTrackFinder::setParameters(StiKalmanTrackFinderParameters *par)
{
  _pars = par;
  StiKalmanTrack::setParameters(par);
  StiKalmanTrackNode::setParameters(par);
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
