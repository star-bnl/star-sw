//
// $Id $
//StiMaker.cxx
// M.L. Miller
// 5/00
// Modified Pruneau 3/02
//
//
// $Log: StiMaker.cxx,v $
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
//   mevent = mStEventFiller->fillEvent(mevent, toolkit->getTrackContainer());
//
// for globals and
//
//   mevent = mStEventFiller->fillEventPrimaries(mevent, toolkit->getTrackContainer());
//
// for primaries.
//
//
#include <iostream.h>
#include <math.h>
#include <string>

// StRoot
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

// Sti
#include "Sti/Messenger.h"
#include "Sti/StiIOBroker.h"
#include "Sti/StiHitContainer.h"
#include "Sti/StiHit.h"
#include "Sti/StiHitFiller.h"
//#include "Sti/StiDetector.h"
//#include "Sti/StiDetectorContainer.h"
//#include "Sti/StiDetectorFinder.h"
#include "Sti/StiTrackContainer.h"
#include "Sti/StiCompositeSeedFinder.h"
#include "Sti/StiSeedFinder.h"
#include "Sti/StiLocalTrackSeedFinder.h"
#include "Sti/StiTrackSeedFinder.h"
#include "Sti/StiEvaluableTrackSeedFinder.h"
#include "Sti/StiKalmanTrack.h"
#include "Sti/StiKalmanTrackFinder.h"
//#include "Sti/StiTrackMerger.h"
//#include "Sti/StiLocalTrackMerger.h"
#include "Sti/StiDefaultTrackFilter.h"

//StiGui
//#include "StiGui/StiDrawableHits.h"
//#include "StiGui/StiRDLocalTrackSeedFinder.h"
//#include "StiGui/StiRootDrawableHits.h"
//#include "StiGui/StiRootDrawableLine.h"
//#include "StiGui/StiRootDrawableHitContainer.h"
#include "StiGui/StiRootDisplayManager.h"

//StiEvaluator
#include "StiEvaluator/StiEvaluator.h"
#include "StiEvaluator/StiEventAssociator.h"

// StiMaker
#include "StiDefaultToolkit.h"
#include "StiStEventFiller.h"
#include "StiMaker.h"

StiMaker* StiMaker::sinstance = 0;

ClassImp(StiMaker)
  
  StiMaker::StiMaker(const Char_t *name) : 
    StMaker(name),
    initialized(0),
    mEvalFileName("empty"),
    ioBroker(0),
    toolkit(0),
    tracker(0),
    mStEventFiller(0),
    mevent(0), 
    mMcEvent(0), 
    mMcEventMaker(0),
    mAssociationMaker(0)
{
  cout <<"StiMaker::StiMaker()"<<endl;
  sinstance = this;
  StiToolkit::setToolkit(new StiDefaultToolkit());
  // local cache
  toolkit = StiToolkit::instance();
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
  Messenger::kill();
}

void StiMaker::Clear(const char*)
{
  if (initialized) 
    {
      initialized = true;
      tracker->reset();
      if (ioBroker->useGui()) 
	toolkit->getDisplayManager()->reset();
    }
  StMaker::Clear();
}

Int_t StiMaker::Finish()
{
  return StMaker::Finish();
}

Int_t StiMaker::Init()
{
  return kStOk;
}

Int_t StiMaker::InitRun(int run)
{
  if (!initialized)
    {
      cout <<"\n --- StiMaker::InitRun(): Building --- \n"<<endl;
      initialized=true;
      
      Messenger::init();
      Messenger::setRoutingMask(0);
      
      ioBroker = toolkit->getIOBroker();
      cout <<"\n\n ------------------- StiIOBroker ----------------------- \n\n"<<*ioBroker<<endl;
      if (ioBroker->useGui()) 
	{
	  cout <<"--- Display Manager will be set" << endl;
	  toolkit->getDisplayManager()->cd();
	  cout <<"--- Display Manager Ready" << endl;
	}
      else
	cout <<"--- Display Manager will not be used" << endl;
      if (ioBroker->simulated()==true)
	{
	  if (mAssociationMaker)
	    {
	      mAssociationMaker->SetDebug(true);
	      cout << "AssociationMaker Defined" << endl;
	    }
	  else
	    cout << "---- AssociationMaker NOT Defined" << endl;
	  
	  StiEventAssociator::instance(mAssociationMaker);
	  StiEvaluator::instance(mEvalFileName);
	  cout <<"---- Evaluator Ready" << endl;
	}
      else
	cout <<"--- Evaluator will not be used" << endl;
      tracker = dynamic_cast<StiKalmanTrackFinder *>(toolkit->getTrackFinder());
      //StiStEventFiller
      mStEventFiller = new StiStEventFiller();
      
      cout <<"--- Tracker Ready" << endl;
      if (ioBroker->useGui()) 
	{
	  toolkit->getDisplayManager()->setView(0);
	  toolkit->getDisplayManager()->draw();
	  toolkit->getDisplayManager()->update();
	}
      cout <<"\n --- StiMaker::InitRun(): Done building --- \n"<<endl;
    }
  return StMaker::InitRun(run);
}

Int_t StiMaker::Make()
{
  cout <<"StiMaker::Make() - INFO - Starting"<<endl;
  eventIsFinished = false;
  mevent = dynamic_cast<StEvent*>( GetInputDS("StEvent") );
  if (!mevent)
    {
      cout <<"StiMaker::Make() - ERROR - rEvent == 0" << endl;
      return 0;
    }
  
  if (ioBroker->simulated()) 
    {
      mMcEvent= mMcEventMaker->currentMcEvent();
      if (!mMcEvent)
	{
	  cout <<"StiMaker::Make() - ERROR - mcEvent == 0" << endl;
	  return 0;
	}
    }
  else {
      mMcEvent = 0;
  }
  tracker->setEvent(mevent,mMcEvent);
  if (ioBroker->useGui()==false) {
      finishEvent();
  }
  else {
      //toolkit->getHitContainer()->update();
      toolkit->getDisplayManager()->draw();
      toolkit->getDisplayManager()->update();
  }
  return kStOK;
}

void StiMaker::printStatistics() const
{
  //cout <<"HitFactory Size:\t"<<toolkit->getHitFactory()->getCurrentSize()<<endl;
  cout <<"HitContainer size:\t"<<toolkit->getHitContainer()->size()<<endl;
  cout <<"Number of Primary Vertices:\t"<<toolkit->getHitContainer()->numberOfVertices()<<endl;
}

void StiMaker::finishEvent()
{
  StTimer clockGlobalFinder;
  StTimer clockGlobalFiller;
  StTimer clockPrimaryFiller;
  StTimer clockPrimaryFinder;
  StTimer clockAssociator;
  StTimer clockEvaluator;
  
  if (eventIsFinished)
    {
      cout << "StiMaker::finishEvent() - Event reconstruction is finished." <<endl;
      return;				
    }
  cout <<"StiMaker::finishEvent() - Event reconstruction begins."<<endl;
  

  clockGlobalFinder.start();   
  tracker->findTracks();  
  clockGlobalFinder.stop();

  clockGlobalFiller.start();
  mevent = mStEventFiller->fillEvent(mevent, toolkit->getTrackContainer());
  clockGlobalFiller.stop();

  if (mevent->primaryVertex()) 
    {
      clockPrimaryFinder.start();
      StiHit * vertex = toolkit->getHitFactory()->getInstance();
      const StThreeVectorF& vp = mevent->primaryVertex()->position();
      const StThreeVectorF& ve = mevent->primaryVertex()->positionError();
      vertex->set(0.,0.,vp.x(),vp.y(),vp.z(),ve.x(),0.,0.,ve.y(),0.,ve.z());
      vertex->setStHit(mevent->primaryVertex());
      tracker->extendTracksToVertex(vertex);
      clockPrimaryFinder.stop();
      clockPrimaryFiller.start();
      mevent = mStEventFiller->fillEventPrimaries(mevent, toolkit->getTrackContainer());
      clockPrimaryFiller.stop();
    }
  else 
    cout <<"StiMaker::finishEvent() - INFO - Event has no vertex" << endl;

  if (ioBroker->simulated())
    {
      clockAssociator.start();
      //StiEventAssociator::instance()->associate(mMcEvent);
      clockAssociator.stop();

      //clockEvaluator.start();
      //StiEvaluator::instance()->evaluate(toolkit->getTrackContainer());
      //clockEvaluator.stop();
    }
  if (ioBroker->useGui()==true) 
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
      if (mAssociationMaker)
	{
	  rcTrackMapType * map = mAssociationMaker->rcTrackMap();
	  rcTrackMapIter iter;
	  for (iter=map->begin();iter!=map->end();iter++)
	    {
	      StGlobalTrack* g = (*iter).first;
	      StTrackPairInfo* pairInfo = (*iter).second;
	      if (g && pairInfo)
		{
		  
		}
	    }
	}
    }
  eventIsFinished = true;
  cout <<"StiMaker::finishEvent()"<<endl
       <<"        Activity :   Time Elapsed(cpu-s)"<<endl
       <<"====================================================="<<endl
       <<" Global  Finding :"<<clockGlobalFinder.elapsedTime()<<endl
       <<"         Filling :"<<clockGlobalFiller.elapsedTime()<<endl
       <<" Primary Finding :"<<clockPrimaryFinder.elapsedTime()<<endl
       <<"         Filling :"<<clockPrimaryFiller.elapsedTime()<<endl
       <<"     Association :"<<clockAssociator.elapsedTime()<<endl
       <<"      Evaluation :"<<clockEvaluator.elapsedTime()<<endl;


  cout <<"StiMaker::finishEvent() - INFO - Done"<<endl;
}

void StiMaker::finishTrack()
{
  //Add call to next tracker action here
  if (ioBroker->doTrackFit()==true) {
    tracker->fitNextTrack();
  }
  else {
    tracker->findNextTrack();
  }
  return;
}

void StiMaker::doNextTrackStep()
{
  tracker->doNextTrackStep();
}

void StiMaker::defineNextTrackStep(StiFindStep val)
{
  cout <<"StiMaker::defineNextTrackStep(). Set to: ";
  cout <<static_cast<int>(val)<<endl;
  tracker->setStepMode(val);
}

StiIOBroker* StiMaker::getIOBroker()
{
  return toolkit->getIOBroker();
}

