//StiMaker.cxx
// M.L. Miller
// 5/00

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
#include "Sti/StiGeometryTransform.h"
#include "Sti/StiTrackSeedFinder.h"
#include "Sti/StiEvaluableTrackSeedFinder.h"
#include "Sti/StiDetectorFinder.h"
//#include "Sti/TrackNodeTest.h"
#include "Sti/StiCompositeSeedFinder.h"
#include "Sti/StiKalmanTrack.h"
#include "Sti/StiKalmanTrackFinder.h"
#include "Sti/StiTrackMerger.h"
#include "Sti/StiLocalTrackMerger.h"
#include "Sti/Messenger.h"
#include "Sti/StiDynamicTrackFilter.h"

//StiGui
#include "StiGui/StiGuiFactoryTypes.h"
#include "StiGui/StiDrawableHits.h"
#include "StiGui/StiRootDrawableHits.h"
#include "StiGui/StiRootDrawableLine.h"
#include "StiGui/StiRootDrawableHitContainer.h"
#include "StiGui/StiDisplayManager.h"

//StiEvaluator
#include "StiEvaluator/StiEvaluator.h"
#include "StiEvaluator/StiEventAssociator.h"

// StiMaker
#include "StiStEventFiller.h"
#include "StiMaker.h"

StiMaker* StiMaker::sinstance = 0;

ostream& operator<<(ostream&, const StiHit&);


ClassImp(StiMaker)
  
StiMaker::StiMaker(const Char_t *name) : StMaker(name),
					 //names
					 mEvalFileName("empty"),
					 //Containers
					 mhitstore(0), mdetector(0), mtrackstore(0),
					 //Factories
					 mhitfactory(0), mtrackfactory(0),
					 mktracknodefactory(0), mdetectorfactory(0),
					 mdatanodefactory(0),
					 //Display
					 mdisplay(0),
					 //Utilities
					 mhitfiller(0), mTrackMerger(0),
					 //SeedFinders
					 mSeedFinder(0),
					 //Tracker
					 mtracker(0),
					 //Filter
					 mFilter(0),
					 //Filler
					 mStEventFiller(0),
					 //flags
					 mBuilt(false),
					 //Members
					 mevent(0), mMcEvent(0), mMcEventMaker(0),
					 mAssociationMaker(0)
{
    cout <<"StiMaker::StiMaker()"<<endl;
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

    delete mhitstore;
    mhitstore = 0;
    
    delete mhitfactory;
    mhitfactory = 0;
    
    delete mhitfiller;
    mhitfiller = 0;

    delete mTrackMerger;
    mTrackMerger = 0;

    delete mFilter;
    mFilter=0;

    if (StiIOBroker::instance()->useGui()) {
	StiDisplayManager::kill();
	mdisplay = 0;
    }

    delete mtrackfactory;
    mtrackfactory = 0;

    delete mSeedFinder;
    mSeedFinder = 0;
        
    StiDetectorContainer::kill();
    mdetector = 0;
    
    StiTrackContainer::kill();
    mtrackstore = 0;

    delete mdetectorfactory;
    mdetectorfactory = 0;

    delete mdatanodefactory;
    mdatanodefactory = 0;

    delete mktracknodefactory;
    mktracknodefactory = 0;

    delete mtracker;
    mtracker = 0;

    delete mStEventFiller;
    mStEventFiller = 0;
    
    StiGeometryTransform::kill();

    StiDetectorFinder::kill();

    StiEvaluator::kill();

    Messenger::kill();
}

void StiMaker::Clear(const char*)
{
    if (mBuilt) {
	
	//Clear HitContainer
	mhitstore->clear();
	
	//Reset DetectorContainer
	StiDetectorContainer::instance()->reset();
	
	//Reset HitFactory
	mhitfactory->reset();
	
	//Reset EvaluableTrackFactory
	mtrackfactory->reset();
	mktracknodefactory->reset();
	
	if (StiIOBroker::instance()->useGui()) {
	    //Reset DisplayManager
	    mdisplay->reset();
	}
	
	//Clear the track store
	mtrackstore->clear();
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
    if (!mBuilt) {

	cout <<"\n --- StiMaker::InitRun(): Building --- \n"<<endl;
	mBuilt=true;

	Messenger::init();
	Messenger::setRoutingMask(0);
	//Messenger::instance()->setRoutingMask(0); //turn off all streams
	//Messenger::instance()->setRoutingBits(MessageType::kHitMessage);

	//The IOBroker
	StiIOBroker* stiIO = StiIOBroker::instance();
	cout <<"\n\n ------------------- StiIOBroker ----------------------- \n\n"<<*stiIO<<endl;

	//The Display
	if (StiIOBroker::instance()->useGui()) {
	    mdisplay = StiDisplayManager::instance();
	    //Must come before anything that you want to be drawn
	    mdisplay->cd();
	}

	if (stiIO->simulated()==true) {
	    StiEventAssociator::instance(mAssociationMaker);
	}
    
	//The hit container
	if (StiIOBroker::instance()->useGui()) {
	    mhitstore = new StiRootDrawableHitContainer();
	}
	else {
	    mhitstore = new StiHitContainer();
	}

	//The track store
	mtrackstore = StiTrackContainer::instance();

	//The track merger
	mTrackMerger = new StiLocalTrackMerger(mtrackstore);

	//The Hit Factory
	mhitfactory = new StiHitFactory("HitFactory");
	mhitfactory->setIncrementalSize(50000); //Allocate in chunks of 50k hits
	mhitfactory->setMaxIncrementCount(10);
	//So, we can have 10 allocations at 50k a pop -> 500k hits max.

	//The Track node factory
	mktracknodefactory =
	    new StiKalmanTrackNodeFactory("StiKalmanTrackNodeFactory");
	mktracknodefactory->setIncrementalSize(10000);
	mktracknodefactory->setMaxIncrementCount(200);
	//So, we can have 100 allocations at 10000 a pop ->1M nodes max
    
	StiKalmanTrack::setKalmanTrackNodeFactory( mktracknodefactory );    

	//The StiDetector factory
	if (StiIOBroker::instance()->useGui()==true) {
	    mdetectorfactory = new StiRDDetectorFactory("RDDetectorFactory");
	}
	else {
	    mdetectorfactory = new StiDetectorFactory("DetectorFactory");
	}
	mdetectorfactory->setIncrementalSize(1000);
	mdetectorfactory->setMaxIncrementCount(10);
	mdetectorfactory->reset();

	//The DetectorNodeFactory
	mdatanodefactory = new StiDetectorNodeFactory("DetectorNodeFactory");
	mdatanodefactory->setIncrementalSize(1000);
	mdatanodefactory->setMaxIncrementCount(10);
	mdatanodefactory->reset();
    
	//The Detector Tree
	mdetector = StiDetectorContainer::instance();
	mdetector->buildDetectors(mdatanodefactory, mdetectorfactory);
	mdetector->reset();
      
	//The Hit Filler
	mhitfiller = new StiHitFiller();
	mhitfiller->addDetector(kTpcId);
	mhitfiller->addDetector(kSvtId);
	cout <<"Hits used from detectors:\t"<<*mhitfiller<<endl;

	//The seed finder (must be built after detector-tree)
	if (StiIOBroker::instance()->seedFinderType()==StiIOBroker::kEvaluable) {
	    //Make an evaluable track factory
	    if (StiIOBroker::instance()->useGui()==true) {
		mtrackfactory = new StiRDEvaluableTrackFactory("StiRDEvaluableTrackFactory",50);
		mtrackfactory->setIncrementalSize(1000);
		mtrackfactory->setMaxIncrementCount(200);
	    }
	    else {
		mtrackfactory = new StiEvaluableTrackFactory("StiEvaluableTrackFactory");
		mtrackfactory->setIncrementalSize(1000);
		mtrackfactory->setMaxIncrementCount(200);
	    }
	    
	    cout <<"StiMaker::init(). Set tracker seed finder to StiIOBroker::kEvaluable"<<endl;
	    StiEvaluableTrackSeedFinder* temp =
		new StiEvaluableTrackSeedFinder(mAssociationMaker, mhitstore);
	    temp->setFactory(mtrackfactory);
	    mSeedFinder=temp;
	}
    
	else if (StiIOBroker::instance()->seedFinderType()==StiIOBroker::kComposite) {
	    //Make a kalman track factory
	    if (StiIOBroker::instance()->useGui()==true) {
		mtrackfactory = new StiRDKalmanTrackFactory("StiRDKalmanTrackFactory",50);
	    }
	    else {
		mtrackfactory = new StiKalmanTrackFactory("StiKalmanTrackFactory");
	    }
	    mtrackfactory->setIncrementalSize(1000);
	    mtrackfactory->setMaxIncrementCount(200);
	
	    cout <<"StiMaker::init(). Set tracker seed finder to StiIOBroker::kComposite"<<endl;
	    StiCompositeSeedFinder* temp = new StiCompositeSeedFinder(mtrackfactory, mhitstore);
	    mSeedFinder=temp;
	}
	else if (StiIOBroker::instance()->seedFinderType()==StiIOBroker::kUndefined) { //not initialized
	    cout <<"StiMaker::init(). ERROR:\t SeedFinderType==StiIOBroker::kUndefined"<<endl;
	}
	else { //catch all
	    cout <<"StiMaker::init(). ERROR:\t unkown SeedFinderType"<<endl;
	}

	//Track filter
	mFilter = new StiDynamicTrackFilter(StiIOBroker::instance());
	cout <<"\n--- HitFilter ---\n"<<endl;
	mFilter->print();

	//StiStEventFiller
	mStEventFiller = new StiStEventFiller();
	
	//The Tracker
	mtracker = new StiKalmanTrackFinder();
	mtracker->setTrackNodeFactory(mktracknodefactory);
	mtracker->setTrackSeedFinder(mSeedFinder);
	mtracker->setHitContainer(mhitstore);
	mtracker->isValid(true);
	
	if (StiIOBroker::instance()->useGui()) {
	    mdisplay->setSkeletonView();
	    mdisplay->draw();
	    mdisplay->update();
	    //mdisplay->print();
	}
	
	//The Evaluator
	//First call to instance must specify then output file name
	if (StiIOBroker::instance()->simulated()==true) {
	    StiEvaluator::instance(mEvalFileName);
	}
	
	cout <<"\n --- StiMaker::InitRun(): Done building --- \n"<<endl;
    }
    return StMaker::Init();
}

Int_t StiMaker::Make()
{
    cout <<" \n\n ------------ You have entered StiMaker::Make() ----------- \n\n"<<endl;
    //cout <<"\n--- HitFilter ---\n";
    //mFilter->print();
    
    StEvent* rEvent = 0;
    rEvent = (StEvent*) GetInputDS("StEvent");
    
    if (StiIOBroker::instance()->simulated() && !mMcEventMaker) {
	cout <<"StiMaker::Make(). ERROR!\tmMcEventMaker==0"<<endl;
	return 0;
    }
    
    StMcEvent* mc = 0;
    if (StiIOBroker::instance()->simulated()) {
	mc = mMcEventMaker->currentMcEvent();
	mMcEvent = mc;
    }
    
    if (StiIOBroker::instance()->simulated()==true && mc==0) {
	cout <<"StiMaker::Make(). ERROR!\tMcEvent==0"<<endl;
	return 0;
    }
    
    if (rEvent) {
	mevent = rEvent;
	
	cout <<"Number of Primary Vertices:\t"<<mevent->numberOfPrimaryVertices()<<endl;

	//Fill hits, organize the container
	mhitfiller->setEvent(mevent);
	mhitfiller->fillHits(mhitstore, mhitfactory);

	cout <<"StiMaker::Make()\tsortHits"<<endl;
	mhitstore->sortHits();
	cout <<"\tdone"<<endl;

	cout <<"StiMaker::Make()\tCall StiHitContainer::update()"<<endl;
	mhitstore->update();
	cout <<"\tdone"<<endl;
	    
	//Init seed finder for start
	mSeedFinder->reset();

	//Pass mc event if simulated
	StiEvaluableTrackSeedFinder* temp = dynamic_cast<StiEvaluableTrackSeedFinder*>(mSeedFinder);
	if (StiIOBroker::instance()->simulated() && temp!=0) {
	    temp->setEvent(mc);
	}

	//Now we can loop, if we're not using the gui
	if (StiIOBroker::instance()->useGui()==false) {
	    finishEvent();
	}
	
    }
    
    if (StiIOBroker::instance()->useGui()==true) {
	mdisplay->draw();
	mdisplay->update();
    }
    return kStOK;
}

void StiMaker::printStatistics() const
{
    cout <<"HitFactory Size:\t"<<mhitfactory->getCurrentSize()<<endl;
    cout <<"HitContainer size:\t"<<mhitstore->size()<<endl;
    cout <<"Number of Primary Vertices:\t"<<mhitstore->numberOfVertices()<<endl;
}

void StiMaker::finishEvent()
{
    cout <<"StiMaker::finishEvent()"<<endl;
    StTimer clock;
    clock.start();
    double n=0.;
    if (mtracker->hasMore()) {
	cout <<"Initial pass to track event:"<<endl;
	while (mtracker->hasMore()) {
	    ++n;
	    finishTrack();
	    if (fmod(n, 100.)==0.) {
		cout <<"Chugging on track: "<<n<<endl;
	    }
	}
	clock.stop();
	cout <<"Time to find "<<n<<" tracks: "<<clock.elapsedTime()<<" cpu seconds"<<endl;
	cout <<"Merge Tracks"<<endl;
	clock.reset();
	//clock.start();
	//mTrackMerger->mergeTracks();
	//clock.stop();
	//cout <<"Time to merge tracks: "<<clock.elapsedTime()<<" cpu seconds"<<endl;

	//Write into StEvent
	cout <<"Fill StEvent:\t"<<endl;
	clock.reset();
	clock.start();
	mevent = mStEventFiller->fillEvent(mevent, mtrackstore);
	clock.stop();
	cout <<"Time to fill StEvent: "<<clock.elapsedTime()<<" cpu seconds"<<endl;
	
	if (StiIOBroker::instance()->simulated()==true) {
	    cout <<"Associate for event\t";
	    StiEventAssociator::instance()->associate(mMcEvent);
	    cout <<"done"<<endl;
	    StiEvaluator::instance()->evaluateForEvent(mtrackstore);
	}
    }
    else {
	cout <<"Test cleanup algorithm:"<<endl;
	cout <<"Parition Hits:\t";
	mhitstore->partitionUsedHits();
	cout <<"done"<<endl;
	if (StiIOBroker::instance()->useGui()==true) {
	    cout <<"Update gui hits:\t";
	    mdisplay->reset();
	    mhitstore->update();
	    cout <<"done"<<endl;
	    cout <<"Update display:\t";
	    mdisplay->draw();
	    mdisplay->update();
	    cout <<"done"<<endl;
	}
	clock.stop();
    }

    cout <<"\tStiMaker::finishEvent(). done"<<endl;
}

void StiMaker::finishTrack()
{
    //Add call to next tracker action here
    if (StiIOBroker::instance()->doTrackFit()==true) {
	mtracker->doTrackFit();
    }
    else {
	mtracker->doTrackFind();
    }
    return;
}

void StiMaker::doNextTrackStep()
{
    mtracker->doNextTrackStep();
}

void StiMaker::defineNextTrackStep(StiFindStep val)
{
    cout <<"StiMaker::defineNextTrackStep(). Set to: ";
    cout <<static_cast<int>(val)<<endl;
    mtracker->setStepMode(val);
}

