// $Id $
/// \File StiMaker.cxx
/// \author M.L. Miller 5/00
/// \author C Pruneau 3/02
// $Log: StiMaker.cxx,v $
// Revision 1.119  2003/04/10 14:53:06  pruneau
// removing obsolete files and classes
//
// Revision 1.118  2003/04/10 12:10:09  pruneau
// Changed StiMaker and Default Toolkit to accomodate the new Event Display
//
// Revision 1.117  2003/03/31 17:19:27  pruneau
// various
//
// Revision 1.116  2003/03/17 17:44:49  pruneau
// *** empty log message ***
//
// Revision 1.115  2003/03/13 18:59:42  pruneau
// various updates
//
// Revision 1.114  2003/03/13 16:30:59  andrewar
// Added plotting package
//
// Revision 1.113  2003/03/13 15:15:51  pruneau
// various
//
// Revision 1.112  2003/03/12 17:58:04  pruneau
// fixing stuff
//
// Revision 1.111  2003/02/25 14:21:06  pruneau
// *** empty log message ***
//
// Revision 1.110  2003/01/24 06:12:28  pruneau
// removing centralized io
//
// Revision 1.109  2003/01/22 20:06:26  andrewar
// Changed includes to point to new libraries (StiTpc, StiSvt, etc)
//
// Revision 1.108  2002/12/19 19:29:42  pruneau
// *** empty log message ***
//
// Revision 1.106  2002/10/04 01:54:48  pruneau
// DefaultToolkit now uses the StiHitLoader scheme rahter than the StiHitFiller.
//
// Revision 1.105  2002/09/27 19:19:01  mmiller
// Changed program flow to once again allow for track by track gui.
//
// Revision 1.104  2002/09/10 18:42:40  pruneau
// Fixed bug in the call sequence of the association maker
// introduced in the previous release.
//
// Revision 1.103  2002/09/05 21:27:10  pruneau
// Fixed problem with StiRootSimpleTrackFilter::makeNewObject
//
// Revision 1.102  2002/09/05 05:47:30  pruneau
// Adding Editable Parameters and dynamic StiOptionFrame
//
// Revision 1.101  2002/08/28 17:14:18  pruneau
// Simplified the interface of StiKalmanTrackFinder and the calls
// required in StiMaker.
//
// Revision 1.100  2002/08/23 18:16:50  pruneau
// Added StiSimpleTrackFilter to StiMaker to enable simple and
// fast track finding diagnostics.
//
// Revision 1.99  2002/08/19 19:32:59  pruneau
// eliminated cout when unnecessary, made helix member of the EventFiller
//
// Revision 1.98  2002/06/26 23:05:31  pruneau
// changed macro
//
// Revision 1.97  2002/06/18 18:08:34  pruneau
// some cout statements removed/added
//
// Revision 1.96  2002/06/04 19:45:31  pruneau
// including changes for inside out tracking
//
#include <iostream.h>
#include <math.h>
#include <string>
#include "StChain.h"
#include "St_DataSet.h"
#include "St_DataSetIter.h"
#include "StMessMgr.h"
#include "SystemOfUnits.h"
#include "PhysicalConstants.h"
#include "StDetectorId.h"
#include "StEventTypes.h"
#include "StMcEventMaker/StMcEventMaker.h"
#include "Sti/StiKalmanTrackFinder.h"
#include "Sti/StiTrackContainer.h"
#include "Sti/StiDefaultTrackFilter.h"
#include "Sti/Star/StiStarDetectorGroup.h"
#include "StiTpc/StiTpcDetectorGroup.h"
#include "StiSvt/StiSvtDetectorGroup.h"
#include "StiEmc/StiEmcDetectorGroup.h"
#include "Sti/StiKalmanTrackNode.h"
#include "Sti/StiTrackingPlots.h"
#include "Sti/StiKalmanTrack.h"
#include "Sti/StiHitLoader.h"
#include "Sti/StiTrackSeedFinder.h"
#include "Sti/StiVertexFinder.h"
#include "StiMaker/StiMakerParameters.h"
#include "StiMaker/StiStEventFiller.h"
#include "StiGui/EventDisplay.h"
#include "StiDefaultToolkit.h"
#include "StiMaker.h"
#include "TFile.h"

ClassImp(StiMaker)
  
  StiMaker::StiMaker(const Char_t *name) : 
    StMaker(name),
    _pars(0),
    _initialized(false),
    _toolkit(StiToolkit::instance() ),
    _hitLoader(0),
    _seedFinder(0),
    _tracker(0),
    _eventFiller(0),
    _trackContainer(0),
    _vertexFinder(0),
    mMcEventMaker(0),
    mAssociationMaker(0)
{
  cout <<"StiMaker::StiMaker() -I- Starting"<<endl;
}

StiMaker::~StiMaker() 
{
  cout <<"StiMaker::~StiMaker() -I- Started/Done"<<endl;
}

void StiMaker::Clear(const char*)
{
  if (_initialized) 
      _tracker->clear();
  StMaker::Clear();
}

Int_t StiMaker::Finish()
{
  if (_pars->doPlots)
    {
      if (_recPlotter) _recPlotter->write("StiMakerHistograms.root");
      if (_mcPlotter)  _mcPlotter->write("StiMakerHistograms.root","UPDATE");
    }
  return StMaker::Finish();
}

Int_t StiMaker::Init()
{  
  return kStOk;
}

Int_t StiMaker::InitDetectors()
{
  StiDetectorGroup<StEvent,StMcEvent> * tpc;
  StiDetectorGroup<StEvent,StMcEvent> * svt;
  StiDetectorGroup<StEvent,StMcEvent> * emc;
  cout<<"StiMaker::InitDetectors() -I- Adding detector group:Star"<<endl;
  _toolkit->add(new StiStarDetectorGroup());
  if (_pars->useTpc)
    {
      cout<<"StiMaker::InitDetectors() -I- Adding detector group:TPC"<<endl;
      _toolkit->add(tpc = new StiTpcDetectorGroup(true));
      tpc->setGroupId(kTpcId);
    }
  if (_pars->useSvt)
    {
      cout<<"StiMaker::Init() -I- Adding detector group:SVT"<<endl;
      _toolkit->add(svt = new StiSvtDetectorGroup(true));
      svt->setGroupId(kSvtId);
    }
  return kStOk;
}

Int_t StiMaker::InitRun(int run)
{
  return StMaker::InitRun(run);
}

Int_t StiMaker::Make()
{
  cout <<"StiMaker::Make() -I- Starting"<<endl;

  // a  kludge because some guys don't initialize their detectors
  // in Init but in Make - this is BAD!
  if (!_initialized)
    {
      cout <<"StiMaker::Make() -I- Initialization Segment Started"<<endl;
      _initialized=true;
      InitDetectors();
      _hitLoader = _toolkit->getHitLoader();
      _hitLoader->setUseMcAsRec(_pars->useMcAsRec);
      _seedFinder = _toolkit->getTrackSeedFinder();
      cout << "StiMaker::Make() -I- Instantiate Tracker" <<  endl;
      _tracker = dynamic_cast<StiKalmanTrackFinder *>(_toolkit->getTrackFinder());
      _eventFiller =  new StiStEventFiller();
      _trackContainer = _toolkit->getTrackContainer();
      _vertexFinder   = _toolkit->getVertexFinder();
      if (!_tracker)
	throw runtime_error("StiMaker::Make() -F- tracker is not a StiKalmanTrackFinder");
      _tracker->initialize();
      _tracker->clear();
      if (_toolkit->isGuiEnabled())
	{
	  _eventDisplay->draw();
	}
      if (_pars->doPlots)
	{
	  _recPlotter = new StiTrackingPlots("R","Reconstructed");
	  if (_pars->doSimulation) _mcPlotter = new StiTrackingPlots("MC","MC");
	}
      cout <<"StiMaker::Make() -I- Initialization Segment Completed"<<endl;
    }
  eventIsFinished = false;
  StMcEvent * mcEvent;
  StEvent   * event = dynamic_cast<StEvent*>( GetInputDS("StEvent") );
  if (!event)
    throw runtime_error("StiMaker::Make() - ERROR - event == 0");
  if (_toolkit->isMcEnabled())
    {
      mcEvent= mMcEventMaker->currentMcEvent();
      if (!mcEvent)
	throw runtime_error("StiMaker::Make() - ERROR - mcEvent == 0");
    }
  else 
    mcEvent = 0;
  if (_toolkit->isGuiEnabled())
    {
      cout << "StiMaker::Make() -I- Loading EVENT"<<endl;
      _tracker->clear();
      _hitLoader->loadEvent(event,mcEvent);
      _seedFinder->reset();
      _eventDisplay->draw();
    }
  else
    {
      _tracker->clear();
      _hitLoader->loadEvent(event,mcEvent);
      _seedFinder->reset();
      _tracker->findTracks();
      try
	{
	  if (_eventFiller && !_pars->useMcAsRec)
	    _eventFiller->fillEvent(event, _trackContainer);
	  cout << "SKTF::findTracks() -I- Global Track StEvent Fill Completed"<<endl;
	}
      catch (runtime_error & rte)
	{
	  cout << "StiMaker::Make() - Run Time Error :" << rte.what() << endl;
	}
      if (_vertexFinder)
	{
	  StiHit *vertex=0;
	  cout << "StiMaker::Maker() -I- Will Find Vertex"<<endl;
	  vertex = _vertexFinder->findVertex(event);
	  if (vertex)
	    {
	      cout << "StiMaker::Make() -I- Got Vertex; extend Tracks"<<endl;
	      _tracker->extendTracksToVertex(vertex);
	      cout << "StiMaker::Make() -I- Primary Filling"<<endl; 
	      try
		{
		  if (_eventFiller && !_pars->useMcAsRec)
		    _eventFiller->fillEventPrimaries(event, _trackContainer);
		}  
	      catch (runtime_error & rte)
		{
		  cout<< "StiMaker::Make() - Run Time Error :" << rte.what() << endl;
		}						
	    }
	}
      cout<< "StiMaker::Make() -I- Will Fill Plots As Needed"<<endl;
      if (_recPlotter) _recPlotter->fill(_toolkit->getTrackContainer());
      if (_mcPlotter ) _mcPlotter->fill(_toolkit->getMcTrackContainer());  
    }
  cout<< "StiMaker::Make() -I- Done"<<endl;
  return kStOK;
}

 void StiMaker::setParameters(StiMakerParameters * pars)
{
  _pars = pars;
}

StiMakerParameters * StiMaker::getParameters()
{
  return _pars;
}
