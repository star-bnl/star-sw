// $Id $
/// \File StiMaker.cxx
/// \author M.L. Miller 5/00
/// \author C Pruneau 3/02
// $Log: StiMaker.cxx,v $
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
// Revision 1.95  2002/05/29 19:13:50  calderon
// Added
//
//   mevent = mStEventFiller->fillEvent(mevent, _toolkit->getTrackContainer());
//
// for globals and
//
//   mevent = mStEventFiller->fillEventPrimaries(mevent, _toolkit->getTrackContainer());
//
// for primaries.
//
//
#include <iostream.h>
#include <math.h>
#include <string>
#include "StChain.h"
#include "St_DataSet.h"
#include "St_DataSetIter.h"
#include "StMessMgr.h"
// SCL
#include "SystemOfUnits.h"
#include "PhysicalConstants.h"
// StEvent
#include "StEventTypes.h"
//StMcEventMaker
#include "StMcEventMaker/StMcEventMaker.h"
#include "Sti/StiKalmanTrackFinder.h"
#include "Sti/StiDefaultTrackFilter.h"
#include "Sti/Star/StiStarDetectorGroup.h"
#include "StiTpc/StiTpcDetectorGroup.h"
#include "StiSvt/StiSvtDetectorGroup.h"
#include "StiEmc/StiEmcDetectorGroup.h"

#include "StiGui/StiRootDisplayManager.h"

#include "StiDefaultToolkit.h"
#include "StiMaker.h"

StiMaker* StiMaker::sinstance = 0;

ClassImp(StiMaker)
  
  StiMaker::StiMaker(const Char_t *name) : 
    StMaker(name),
    initialized(false),
    _toolkit(StiToolkit::instance() ),
    tracker(0),
    mMcEventMaker(0),
    mAssociationMaker(0)
{
  cout <<"StiMaker::StiMaker() -I- Starting"<<endl;
  sinstance = this;
}

StiMaker* StiMaker::instance()
{
  return (sinstance) ? sinstance : new StiMaker();
}

void StiMaker::kill()
{
  if (sinstance) {
    delete sinstance;
    sinstance = 0;
  }
  return;
}

StiMaker::~StiMaker() 
{
  cout <<"StiMaker::~StiMaker()"<<endl;
 
}

void StiMaker::Clear(const char*)
{
  cout <<"StiMaker::Clear( ) -I- Started"<<endl;
  if (initialized) 
    {
      cout <<"StiMaker::Clear( ) -I- Initialized - call tracker reset"<<endl;
      tracker->clear();
      if (_toolkit->isGuiEnabled() )
				_toolkit->getDisplayManager()->reset();
    }
  cout <<"StiMaker::Clear( ) -I- Call base class clear"<<endl;
  StMaker::Clear();
  cout <<"StiMaker::Clear( ) -I- Done"<<endl;
}

Int_t StiMaker::Finish()
{
  return StMaker::Finish();
}

Int_t StiMaker::Init()
{  
  return kStOk;
}

Int_t StiMaker::InitDetectors()
{
  cout<<"StiMaker::InitDetectors() -I- Adding detector group:Star"<<endl;
  _toolkit->add(new StiStarDetectorGroup());
  cout<<"StiMaker::InitDetectors() -I- Adding detector group:TPC"<<endl;
  _toolkit->add(new StiTpcDetectorGroup(true));
  cout<<"StiMaker::Init() -I- Adding detector group:SVT"<<endl;
  _toolkit->add(new StiSvtDetectorGroup(true));
  //cout<<"StiMaker::Init() -I- Adding detector group:EMC"<<endl;
  //toolkit->add(new StiEmcDetectorGroup(true));
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
  if (!initialized)
    {
      cout <<"\n --- StiMaker::InitRun() -I- Building --- \n"<<endl;
      initialized=true;
      InitDetectors();
      cout << "StiMaker::Make() -I- Instantiate Tracker" <<  endl;
      tracker = dynamic_cast<StiKalmanTrackFinder *>(_toolkit->getTrackFinder());
      if (!tracker)
	throw runtime_error("StiMaker::Make() - FATAL - tracker is not a StiKalmanTrackFinder");
      tracker->initialize();
      tracker->clear();
      if (_toolkit->isGuiEnabled())
				{
					cout << "StiMaker::Make() -I- Instantiate/Setup DisplayManager" <<  endl;
					_toolkit->getDisplayManager()->cd();
					_toolkit->getDisplayManager()->setView(0);
					_toolkit->getDisplayManager()->draw();
					_toolkit->getDisplayManager()->update();
				}
      cout <<"\n --- StiMaker::InitRun(): Done building --- \n"<<endl;
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
      tracker->loadEvent(event,mcEvent);
      _toolkit->getDisplayManager()->draw();
      _toolkit->getDisplayManager()->update();
    }
  else
    tracker->findTracks(event,mcEvent);
  return kStOK;
}

/*
void StiMaker::finishEvent()
{
  StTimer clockGlobalFinder;
  StTimer clockAssociator;
  if (eventIsFinished)
	{
	cout << "StiMaker::finishEvent() - Event reconstruction is finished." <<endl;
      return;				
    }
		cout <<"StiMaker::finishEvent() - Event reconstruction begins."<<endl;
  clockGlobalFinder.start();   
  tracker->findTracks();  
  clockGlobalFinder.stop();
  if (_toolkit->isGuiEnabled())
	{
	tracker->update();
	StiDefaultTrackFilter * f = 0;
	if (f)
	{
	  f->getParameter("Chi2Used")->setValue(true);
	  f->getParameter("Chi2Min")->setValue(0.);
	  f->getParameter("Chi2Max")->setValue(20.);
	  f->getParameter("PtUsed")->setValue(true);
	  f->getParameter("PtMin")->setValue(0.1);
	  f->getParameter("PtMax")->setValue(10.);
	  f->getParameter("nPtsUsed")->setValue(true);
	  f->getParameter("nPtsMin")->setValue(0);
	  f->getParameter("nPtsMax")->setValue(60);
	  f->getParameter("nGapsUsed")->setValue(true);
	  f->getParameter("nGapsMin")->setValue(0);
	  f->getParameter("nGapsMax")->setValue(60);
	  cout << "      Total Found:" << tracker->getTrackFoundCount()<<endl;
	  f->getParameter("PtMin")->setValue(0.1);
	  f->getParameter("PtMax")->setValue(0.2);
	  cout << "       0.1<pt<0.2:" << tracker->getTrackFoundCount(f) << endl;
	  f->getParameter("PtMin")->setValue(0.2);
	  f->getParameter("PtMax")->setValue(0.5);
	  cout << "       0.2<pt<0.5:" << tracker->getTrackFoundCount(f) << endl;
	  f->getParameter("PtMin")->setValue(0.5);
	  f->getParameter("PtMax")->setValue(1.0);
	  cout << "       0.5<pt<1.0:" << tracker->getTrackFoundCount(f) << endl;
	}
    }
  eventIsFinished = true;
  cout <<"StiMaker::finishEvent()"<<endl
       <<"        Activity :   Time Elapsed(cpu-s)"<<endl
       <<"====================================================="<<endl
       <<" Global  Finding :"<<clockGlobalFinder.elapsedTime()<<endl
       <<" Primary Finding :"<<clockPrimaryFinder.elapsedTime()<<endl
       <<"     Association :"<<clockAssociator.elapsedTime()<<endl
  cout <<"StiMaker::finishEvent() -I- Done"<<endl;
}
*/


