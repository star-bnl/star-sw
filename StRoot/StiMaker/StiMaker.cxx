//
// $Id $
//StiMaker.cxx
// M.L. Miller
// 5/00
// Modified Pruneau 3/02
//
//
// $Log: StiMaker.cxx,v $
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
#include "Sti/StiIOBroker.h"
#include "Sti/StiFactoryTypes.h"
#include "Sti/StiHitContainer.h"
#include "Sti/StiHit.h"
#include "Sti/StiDetector.h"
#include "Sti/StiPlacement.h"
#include "Sti/StiHitFiller.h"
#include "Sti/StiDetectorContainer.h"
#include "Sti/StiTrackContainer.h"
#include "Sti/StiCompositeSeedFinder.h"
#include "Sti/StiSeedFinder.h"
#include "Sti/StiLocalTrackSeedFinder.h"
#include "Sti/StiTrackSeedFinder.h"
#include "Sti/StiEvaluableTrackSeedFinder.h"
#include "StiGui/StiRDLocalTrackSeedFinder.h"
#include "Sti/StiDetectorFinder.h"
#include "Sti/StiKalmanTrack.h"
#include "Sti/StiKalmanTrackFinder.h"
#include "Sti/StiTrackMerger.h"
#include "Sti/StiLocalTrackMerger.h"
#include "Sti/Messenger.h"
#include "Sti/StiDynamicTrackFilter.h"
#include "Sti/StiSimpleTrackFilter.h"

//StiGui
#include "StiGui/StiGuiFactoryTypes.h"
#include "StiGui/StiDrawableHits.h"
#include "StiGui/StiRootDrawableHits.h"
#include "StiGui/StiRootDrawableLine.h"
#include "StiGui/StiRootDrawableHitContainer.h"
#include "StiGui/StiRootDisplayManager.h"

//StiEvaluator
#include "StiEvaluator/StiEvaluator.h"
#include "StiEvaluator/StiEventAssociator.h"

// StiMaker
#include "StiDefaultToolkit.h"
#include "StiStEventFiller.h"
#include "StiMaker.h"
#include "Sti/StiSimpleTrackFilter.h"

StiMaker* StiMaker::sinstance = 0;

ostream& operator<<(ostream&, const StiHit&);

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
  trackFilter = new StiSimpleTrackFilter();

  toolkit = StiDefaultToolkit::instance();
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
	
  delete trackFilter;

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
	    cout << "AssociationMaker Defined" << endl;
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
  else
    mMcEvent = 0;
  tracker->setEvent(mevent,mMcEvent);
  if (ioBroker->useGui()) 
    {
      toolkit->getDisplayManager()->draw();
      toolkit->getDisplayManager()->update();
    }
  else
    finishEvent();

  return kStOK;
}

void StiMaker::printStatistics() const
{
  cout <<"HitFactory Size:\t"<<toolkit->getHitFactory()->getCurrentSize()<<endl;
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
  StTimer clockPlot;
  
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
      StiHit * vertex = toolkit->getHitFactory()->getObject();
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
      StiEventAssociator::instance()->associate(mMcEvent);
      clockAssociator.stop();
      clockEvaluator.start();
      StiEvaluator::instance()->evaluate(toolkit->getTrackContainer());
      clockEvaluator.stop();
    }
  if (ioBroker->useGui()==true)    tracker->update();

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
    
  // filter our baby...
  trackFilter->set(StiSimpleTrackFilter::kChi2,       0., 100.);
  trackFilter->set(StiSimpleTrackFilter::kPt,         0., 10. );
  //trackFilter->set(StiSimpleTrackFilter::kPseudoRap, -1.5, 1.5);
  trackFilter->set(StiSimpleTrackFilter::kNPts,       5., 50.);
  trackFilter->set(StiSimpleTrackFilter::kNGaps,      0., 50.);
  cout << "      Total Found:" << tracker->getTrackFoundCount()<<endl;
  trackFilter->set(StiSimpleTrackFilter::kPt,         0.1, 0.2 );
  cout << "       0.1<pt<0.2:" << tracker->getTrackFoundCount(trackFilter) << endl;
  trackFilter->set(StiSimpleTrackFilter::kPt,         0.2, 0.5 );
  cout << "       0.2<pt<0.5:" << tracker->getTrackFoundCount(trackFilter) << endl;
  trackFilter->set(StiSimpleTrackFilter::kPt,         0.5, 1.0 );
  cout << "       0.5<pt<1.0:" << tracker->getTrackFoundCount(trackFilter) << endl;
  trackFilter->set(StiSimpleTrackFilter::kPt,         1.0,10.0 );
  cout << "       1.0<pt<10.:" << tracker->getTrackFoundCount(trackFilter) << endl;
  trackFilter->set(StiSimpleTrackFilter::kPt,         0.1, 0.5 );
  trackFilter->set(StiSimpleTrackFilter::kNPts,       5., 20.);
  cout << "NPts<20 && 0.1<pt<0.5 :" << tracker->getTrackFoundCount(trackFilter) << endl;
  trackFilter->set(StiSimpleTrackFilter::kNPts,       20., 100.);
  cout << "NPts>20 && 0.1<pt<0.5 :" << tracker->getTrackFoundCount(trackFilter) << endl;
  trackFilter->set(StiSimpleTrackFilter::kPt,         0.5, 10. );
  trackFilter->set(StiSimpleTrackFilter::kNPts,       5., 20.);
  cout << "NPts<20 && 0.5<pt<10. :" << tracker->getTrackFoundCount(trackFilter) << endl;
  trackFilter->set(StiSimpleTrackFilter::kNPts,       20., 100.);
  cout << "NPts>20 && 0.5<pt<10. :" << tracker->getTrackFoundCount(trackFilter) << endl;
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

