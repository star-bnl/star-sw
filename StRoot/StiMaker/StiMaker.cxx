//  StiMaker.cxx
// M.L. Miller
// 5/00

#include <iostream.h>
#include <math.h>
#include <string>

//Root (Temp)
#include "TCanvas.h"
#include "TPolyMarker3D.h"
#include "TNode.h"
#include "TTUBE.h"

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

// Sti
#include "Sti/StiHitContainer.h"
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
#include "Sti/StiKalmanTrackFinder.h"

//StiGui
#include "StiGui/StiDrawableHits.h"
#include "StiGui/StiRootDrawableHits.h"
#include "StiGui/StiRootDrawableLine.h"
#include "StiGui/StiDisplayManager.h"

// StiMaker
#include "StiEvaluator.h"
#include "StiMaker.h"

StiMaker* StiMaker::sinstance = 0;

ClassImp(StiMaker)
  
StiMaker::StiMaker(const Char_t *name) : StMaker(name)
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
    StiHitContainer::kill();
    mhitstore = 0;
    
    delete mhitfactory;
    mhitfactory = 0;
    
    delete mhitfiller;
    mhitfiller = 0;
    
    StiDisplayManager::kill();
    mdisplay = 0;

    delete mtrackfactory;
    mtrackfactory = 0;

    delete mtrackseedfinder;
    mtrackseedfinder = 0;
    
    StiDetectorContainer::kill();
    mdetector = 0;
    
    StiTrackContainer::kill();
    mtrackstore = 0;

    delete mdetectorfactory;
    mdetectorfactory = 0;

    delete mdatanodefactory;
    mdatanodefactory = 0;

    delete mtracknodefactory;
    mtracknodefactory = 0;

    delete mkalmanseedfinder;
    mkalmanseedfinder = 0;

    delete mkalmantrackfactory;
    mkalmantrackfactory=0;

    //TEST!!!!!
    delete mtempseedfinder;
    mtempseedfinder=0;

    delete mcompseedfinder;
    mcompseedfinder=0;

    delete mtracker;
    mtracker = 0;
    
    StiGeometryTransform::kill();

    StiDetectorFinder::kill();

    StiEvaluator::kill();
}

void StiMaker::Clear(const char*)
{
    //Clear HitContainer
    mhitstore->clear();

    //Clear Drawable hits
    mdrawablehits->clear();
    
    //Reset DetectorContainer
    StiDetectorContainer::instance()->reset();
    
    //Reset HitFactory
    mhitfactory->reset();

    //Reset EvaluableTrackFactory
    mtrackfactory->reset();
    mtracknodefactory->reset();

    //Reset KalmanTrackFactory
    mkalmantrackfactory->reset();
    
    //Reset DisplayManager
    mdisplay->reset();

    //Reset Kalman Track Seed Finder
    mkalmanseedfinder->clear();

    //Clear the track store
    mtrackstore->clear();
    
    StMaker::Clear();
}

Int_t StiMaker::Finish()
{
    return StMaker::Finish();
}

Int_t StiMaker::Init()
{
    //The track store
    mtrackstore = StiTrackContainer::instance();

    //The hit container
    mhitstore = StiHitContainer::instance();

    //The Hit Factory
    mhitfactory = new StiHitFactory("HitFactory");
    mhitfactory->setIncrementalSize(50000); //Allocate in chunks of 50k hits
    mhitfactory->setMaxIncrementCount(10);  //So, we can have 10 allocations at 50k a pop -> 500k hits max.

    //The Evalualbe Track Factory
    mtrackfactory = new StiEvaluableTrackFactory("EvaluableTrackFactory");
    mtrackfactory->setIncrementalSize(1000);
    mtrackfactory->setMaxIncrementCount(10);

    //The Track node factory
    mtracknodefactory = new StiTrackNodeFactory("StiTrackNodeFactory");
    mtracknodefactory->setIncrementalSize(1000);
    mtracknodefactory->setMaxIncrementCount(100);
    StiKalmanTrack::trackNodeFactory = mtracknodefactory;
    
    //The Kalman Track Factory
    mkalmantrackfactory = new StiKalmanTrackFactory("KalmanTrackFactory");
    mkalmantrackfactory->setIncrementalSize(1000);
    mkalmantrackfactory->setMaxIncrementCount(10);

    //EvaluableTrack SeedFinder
    mtrackseedfinder = new StiEvaluableTrackSeedFinder();
    mtrackseedfinder->setFactory(mtrackfactory, mhitfactory);
    mtrackseedfinder->setStTrackType(mStTrackType);

    //KalmanTrackSeedFinder
    mkalmanseedfinder = new StiTrackSeedFinder(mhitstore);
    mkalmanseedfinder->setFactory(mkalmantrackfactory);

    //The StiDetector factory
    mdetectorfactory = new detector_factory("DrawableDetectorFactory");
    mdetectorfactory->setIncrementalSize(1000);
    mdetectorfactory->setMaxIncrementCount(10);
    mdetectorfactory->reset();

    //The DetectorNodeFactory
    mdatanodefactory = new data_node_factory("DataNodeFactory");
    mdatanodefactory->setIncrementalSize(1000);
    mdatanodefactory->setMaxIncrementCount(10);
    mdatanodefactory->reset();
    
    //The Display
    mdisplay = StiDisplayManager::instance(); //Must come before anything that you want to be drawn
    mdisplay->cd();
    mdisplay->draw();
    mdisplay->update();
    
    //Drawable hits
    mdrawablehits = new StiRootDrawableHits();
    mdrawablehits->clear();
    mdrawablehits->setMarkerSize(.2);
    mdisplay->addDrawable(mdrawablehits);

    //The Detector Tree
    mdetector = StiDetectorContainer::instance();
    mdetector->buildDetectors(mdetectorbuildpath, mdatanodefactory, mdetectorfactory);
    mdetector->reset();
    //mdetector->print();
      
    mdisplay->draw();
    mdisplay->update();

    //The Hit Filler
    mhitfiller = new StiHitFiller();
    mhitfiller->addDetector(kTpcId);
    mhitfiller->addDetector(kSvtId);
    cout <<"Hits used from detectors:\t"<<*mhitfiller<<endl;
    
//    TrackNodeTest *pTest = new TrackNodeTest();
//    pTest->doTest();

    //Test another KalmanTrackSeedFinder via StiCompositeSeedFinder
    mtempseedfinder = new StiTrackSeedFinder(mhitstore);
    mtempseedfinder->setFactory(mkalmantrackfactory);
    mcompseedfinder =new StiCompositeSeedFinder();
    mcompseedfinder->buildOuterSeedFinder(mtempseedfinder);
    mcompseedfinder->buildInnerSeedFinder(mtempseedfinder);
    
    //The Tracker
    mtracker = new StiKalmanTrackFinder();
    mtracker->setTrackNodeFactory(mtracknodefactory);
    //mtracker->setTrackSeedFinder(mkalmanseedfinder);
    mtracker->setTrackSeedFinder(mcompseedfinder);
    mtracker->isValid(true);
    
    return StMaker::Init();
}

Int_t StiMaker::Make()
{
    StEvent* rEvent = 0;
    rEvent = (StEvent*) GetInputDS("StEvent");
    if (rEvent) {
	mevent = rEvent;
	
	cout <<"\n---------- StiMaker::Make() ------------\n"<<endl;
	cout <<"Number of Primary Vertices:\t"<<mevent->numberOfPrimaryVertices()<<endl;

	//Fill hits, organize the container
	mhitfiller->setEvent(mevent);
	mhitfiller->fillHits(mhitstore, mhitfactory);

	cout <<"StiMaker::Make()\tsortHits"<<endl;
	mhitstore->sortHits();
	cout <<"\tdone"<<endl;
	
	//Init seed finder for start
	mcompseedfinder->reset();

	//Temp patch to draw hits
	cout <<"StiMaker::Make()\tFill Drawable hits"<<endl;
	const hitmap& hits = mhitstore->hits();
	for (hitmap::const_iterator it=hits.begin(); it!=hits.end(); it++) {
	    const hitvector& tempvec = (*it).second;
	    for (hitvector::const_iterator vit=tempvec.begin(); vit!=tempvec.end(); vit++) {
		
		mdrawablehits->push_back( (*vit) );
	    }
	}
	cout <<"\tdone"<<endl;
	cout <<"StiMaker::Make()\tfill hits for drawing"<<endl;
	mdrawablehits->fillHitsForDrawing();
	cout <<"\tdone"<<endl;

	//Initialize the SeedFinder, loop on tracks
	cout <<"StiMaker::Make()\tFill drawable tracks"<<endl;
	mtrackseedfinder->setEvent(mevent);
	while (mtrackseedfinder->hasMore()) {
	    StiRootDrawableStiEvaluableTrack* thetrack =
		dynamic_cast<StiRootDrawableStiEvaluableTrack*>(mtrackseedfinder->next());
	    if (thetrack) {
		thetrack->fillHitsForDrawing();
	    }
	}
	cout <<"\tdone"<<endl;
    }

    //TEMP !!!!!
    //mcompseedfinder->test();

    StiEvaluator::instance()->evaluateForEvent(mtrackstore);
    
    mdisplay->draw();
    mdisplay->update();
    return kStOK;
}

void StiMaker::printStatistics() const
{
    cout <<"HitFactory Size:\t"<<mhitfactory->getCurrentSize()<<endl;
    cout <<"HitContainer size:\t"<<mhitstore->size()<<endl;
    cout <<"Number of Primary Vertices:\t"<<mhitstore->numberOfVertices()<<endl;
}

void StiMaker::setMaterialBuildPath(char* val)
{
    mmaterialbuildpath = val;
}

void StiMaker::setDetectorBuildPath(char* val)
{
    mdetectorbuildpath = val;
}

void StiMaker::doNextAction()
{
    //Add call to next tracker action here
    mtracker->doNextAction();
    return;
}

